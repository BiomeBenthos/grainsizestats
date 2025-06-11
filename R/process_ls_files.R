#' Title
#' 
#' Description
#'
#' @param
#'
#' @return
#' @export
process_laser_file <- function(file) {

  # Lire le fichier
  dat <- read_ls_file(file)

  # Extraire le volume
  volume_distribution <- get_volume_distribution(
    dat, 
    get_bracket(dat)
  )

  # Extraire diamètre du sédiment
  diam <- get_diam(
    dat, 
    get_bracket(dat)
  )

  # Bind both together and  return
  granulo <- cbind(diam, volume_distribution) |>
    as.data.frame()
  colnames(granulo) <- c("diameter (µm)", "volume_diff (%)")
  
  return(granulo)

}


#' Read raw files from laser granulometry analysis
#'
#' @param
#'
#' @return
read_ls_file <- function(file) {

  # Open connection to file
  con <- file(file, open = "r")
  # Read file's lines
  dat <- readLines(con)
  # Close connection
  close(con)

  return(dat)
}


#' Title
#' 
#' Description
#'
#' @param
#'
#' @return
get_bracket <- function(x) {
  grep("^\\[.*\\]", x)
}


#' Title
#' 
#' Description
#'
#' @param
#'
#' @return
get_volume_distribution <- function(dat, bracket) {
  binheight <- which(dat[bracket] == "[#Binheight]")
  height_index <- (bracket[binheight]+1):(bracket[binheight+1]-1)
  volume_distribution <- (as.numeric(dat[height_index])/ 
                          sum(as.numeric(dat[height_index]))) * 100
  return(volume_distribution)
}

#' Title
#' 
#' Description
#'
#' @param
#'
#' @return
get_diam <- function(dat, bracket) {
  bindiam <- which(dat[bracket] == "[#Bindiam]")
  diam_index <- (bracket[bindiam]+1):(bracket[bindiam+1]-1)
  diam <- as.numeric(dat[diam_index])
  return(diam)
}
