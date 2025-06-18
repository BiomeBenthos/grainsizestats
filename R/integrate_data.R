#' Intègre les données granulométriques tamis et laser
#'
#' Cette fonction prend les données granulométriques issues des méthodes tamis et laser, 
#' identifie les échantillons disponibles pour chaque méthode, et les fusionne selon les plages de diamètres définies.
#'
#' @param sieves_data Un data.frame des données obtenues par tamis.
#' @param laser_data Un data.frame des données obtenues par analyse laser.
#' @param granulo_ids Un vecteur d'identifiants uniques pour chaque échantillon granulométrique.
#' @param sieves_ids Un vecteur des identifiants spécifiques à l'analyse tamis pour chaque échantillon.
#' @param laser_ids Un vecteur des identifiants spécifiques à l'analyse laser pour chaque échantillon.
#' @param upper_limits Limites supérieures de la portion analysée au laser dans les tamis.
#' @param lower_limits Limites inférieures correspondantes.
#' @param merge_mud Booléen indiquant s'il faut fusionner les classes fines en un seul groupe.
#'
#' @return Un data.frame combiné des résultats granulométriques pour tous les échantillons.
#' @export
integrate_data <- function(sieves_data, 
                           laser_data, 
                           granulo_ids, 
                           sieves_ids, 
                           laser_ids,
                           upper_limits,
                           lower_limits,
                           merge_mud = TRUE) {

  # All samples
  samples <- granulo_ids |> unique()

  integrated <- lapply(samples, function(x) {
    # Check method for this sample
    subsamples_index <- granulo_ids == x
    subsamples_sieves <- sieves_ids[subsamples_index]
    subsamples_laser <- laser_ids[subsamples_index]

    if(all(is.na(subsamples_laser)) | all(subsamples_laser %in% "")) {
      granulo_sieves <- sieves_data |>
        dplyr::filter(id %in% subsamples_sieves |> unique())
      return(granulo_sieves)
    }

    if(all(is.na(subsamples_sieves)) | all(subsamples_sieves %in% "")) {
      granulo_laser <- laser_data |>
        dplyr::filter(id %in% subsamples_laser |> unique())
      return(process_laser(granulo_laser))
    }

    upper_limit <- upper_limits[subsamples_index]
    lower_limit <- lower_limits[subsamples_index]

    laser_processed <- lapply(1:length(subsamples_index), function(i) {
      granulo_sieves <- sieves_data |> 
        dplyr::filter(id %in% subsamples_sieves[i])
      granulo_laser <- laser_data |>
        dplyr::filter(id %in% subsamples_laser[i])
      
      laser_subsieves <- process_laser_subsieves(
        granulo_sieves,
        granulo_laser,
        upper_limit[i],
        lower_limit[i]
      )
      return(laser_subsieves)
    })

    # Merge si plusieurs laser pour une station
    if(length(laser_processed) > 1) {
      laser_processed_combined <- dplyr::bind_rows(laser_processed) %>%
        dplyr::mutate(
          id = sub("_[0-9]+$", "", id)  # Enlève le suffixe _1, _2, etc.
        ) %>%
        dplyr::group_by(id, diam, measurement_type) %>%
        dplyr::summarise(
          measurement_value = sum(measurement_value),
          .groups = "drop"
        ) |>
        dplyr::arrange(dplyr::desc(diam)) |>
        as.data.frame()
    } else {
      laser_processed_combined <- laser_processed[[1]]
    }

    # Keep mass in the sieve data for size that has not been added to laser analysis
    granulo_sieves <- sieves_data |>
      dplyr::filter(id %in% subsamples_sieves |> unique())
    for(i in 1:length(subsamples_index)) {
      granulo_sieves <- partially_reset_sieves_mass(
        granulo_sieves,
        upper_limit[i],
        lower_limit[i]
      )
    }

    granulo <- merge_sieves_laser(
      id = x, 
      granulo_sieves, 
      laser_processed_combined
    )

    return(granulo)
  }) |> do.call(what = rbind, args = _)
}


#' Title
#' 
#' Description
#'
#' @param
#'
#' @return
merge_sieves_laser <- function(id, sieves, laser) {
  tmp <- granulo_scale
  tmp[,"id"] <- id
  tmp <- tmp[,c("id", "diam")]

  # Somme pour tous les diamètres
  for(i in 1:nrow(tmp)) {
    if(i == 1) {
      sieves_portion <- sieves |>
        dplyr::filter(diam >= tmp[i,"diam"])
      laser_portion <- laser |>
        dplyr::filter(diam >= tmp[i,"diam"])
    } else {
      sieves_portion <- sieves |>
        dplyr::filter(
          diam > tmp[i,"diam"] &
          diam <= tmp[i-1, "diam"]
        )
      laser_portion <- laser |>
        dplyr::filter(
          diam > tmp[i,"diam"] &
          diam <= tmp[i-1, "diam"]
        )
    }
    
    tmp[i, "measurement_value"] <- rbind(
      laser_portion,
      sieves_portion
    ) |>
      dplyr::select(measurement_value) |>
      dplyr::pull() |> sum()
  }

  tmp$measurement_value <- tmp$measurement_value*100/(sum(tmp$measurement_value))

  tmp[,"measurement_type"] <- "mass_(pct)"

  return(tmp)
}


#' Title
#' 
#' Description
#'
#' @param
#'
#' @return
process_laser <- function(data) {
  tmp <- granulo_scale
  tmp[,"id"] <- unique(data$id)
  tmp <- tmp[,c("id", "diam")]

  # Somme pour tous les diamètres
  for(i in 1:nrow(tmp)) {
    if(i == 1) {
      laser_portion <- data |>
        dplyr::filter(diam >= tmp[i,"diam"])
    } else {
      laser_portion <- data |>
        dplyr::filter(
          diam > tmp[i,"diam"] &
          diam <= tmp[i-1, "diam"]
        )
    }
    
    tmp[i, "measurement_value"] <- laser_portion |>
      dplyr::select(measurement_value) |>
      dplyr::pull() |> sum()
  }

  tmp[,"measurement_type"] <- "volume_(pct)"

  return(tmp)
}


#' Title
#' 
#' Description
#'
#' @param
#'
#' @return 
process_laser_subsieves <- function(sieves, laser, upper_limit, lower_limit) {
  # Compute mass of sieves tat has been analyzed with laser
  mass_laser <- calc_mass_laser(
    sieves, 
    laser, 
    upper_limit,
    lower_limit
  )

  # Merge laser into class
  laser_into_classes <- process_laser(laser)

  # Multiply pct by mass_laser
  laser_into_classes$measurement_value <- (laser_into_classes$measurement_value/100)*mass_laser
  laser_into_classes$measurement_type <- "mass_(g)"

  return(laser_into_classes)
}

#' Title
#' 
#' Description
#'
#' @param
#'
#' @return
calc_mass_laser <- function(sieves, laser, upper_limit, lower_limit = 0) {

  # Replace <63 with 0
  sieves$diam[length(sieves$diam)] <- 0
  sieves$diam <- as.numeric(sieves$diam)

  # Poids en numeric
  sieves$measurement_value <- as.numeric(sieves$measurement_value)

  # Extract portion of sieves run with laser
  sieves_laser <- extract_portion_of_sieves(sieves, upper_limit, lower_limit)

  # Extract mass of portion of sieves run with laser
  mass_sieves_laser <- sum(sieves_laser$measurement_value |> as.numeric())

  return(mass_sieves_laser)
} 


#' Title
#' 
#' Description
#'
#' @param
#'
#' @return 
partially_reset_sieves_mass <- function(sieves, upper_limit, lower_limit) {
  sieves_portion <- get_index_portion(sieves, upper_limit, lower_limit)
  sieves[sieves_portion, "measurement_value"] <- 0
  return(sieves)
}
 

#' Title
#' 
#' Description
#'
#' @param
#'
#' @return
extract_portion_of_sieves <- function(sieves, upper_limit, lower_limit) {
  sieves_portion <- get_index_portion(sieves, upper_limit, lower_limit)
  return(sieves[sieves_portion,])
}

#' Title
#' 
#' Description
#'
#' @param
#'
#' @return 
get_index_portion <- function(sieves, upper_limit, lower_limit) {
  lower <- ifelse(
    is.null(lower_limit),
    nrow(sieves),
    grep(paste0("^",lower_limit), sieves$diam)
  )
  upper <- grep(paste0("^",upper_limit), sieves$diam) + 1
  return(upper:lower)
}
