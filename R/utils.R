# Transform 3D coordinates in 2D coordinates
ternary_y  <- function(x, gsm = FALSE) {
  if(gsm) x <- gsm_transform(x)
  return( sqrt(0.75) *  x[3] / sum(x) )
}

ternary_x  <- function(x, gsm = FALSE) {
  if(gsm) x <- gsm_transform(x)
  return((x[2] + 0.5 * x[3]) / sum(x))
}


# Vertices of the triangle
vertices <- function() {
  data.frame(
    clay_mud = c(1, 0, 0),
    silt_sand = c(0, 0, 1),
    sand_gravel = c(0, 1, 0)
  )
}


# 3D Coordinates of every segments that specifify limits between two classes of soil
## gravel-sand-mud
gsm_limits <- function() {
  list(
    top_horiz = data.frame(
      mud = c(0.2, 0),
      sand = c(0, 0.2),
      gravel = c(0.8, 0.8)
    ),
    mid_horiz = data.frame(
      mud = c(0.7, 0),
      sand = c(0, 0.7),
      gravel = c(0.3, 0.3)
    ),
    low_horiz = data.frame(
      mud = c(0.85, 0),
      sand = c(0, 0.85),
      gravel = c(0.15, 0.15)
    ),
    trace_horiz = data.frame(
      mud = c(0.95, 0),
      sand = c(0, 0.95),
      gravel = c(0.05, 0.05)
    ),
    left_vert = data.frame(
      mud = c(9/10, 0.765),
      sand = c(1/10, 0.085),
      gravel = c(0, 0.15)
    ),
    mid_vert = data.frame(
      mud = c(0.5, 0.1),
      sand = c(0.5, 0.1),
      gravel = c(0, 0.8)
    ),
    right_vert = data.frame(
      mud = c(1/10, 0.02),
      sand = c(9/10, 0.18),
      gravel = c(0, 0.8)
    )
  )
}

## sand-silt-clay
ssc_limits <- function() {
  list(
    top_horiz = data.frame(
      clay = c(0.1, 0),
      silt = c(0, 0.1),
      sand = c(0.9, 0.9)
    ),
    mid_horiz = data.frame(
      clay = c(0.5, 0),
      silt = c(0, 0.5),
      sand = c(0.5, 0.5)
    ),
    low_horiz = data.frame(
      clay = c(0.9, 0),
      silt = c(0, 0.9),
      sand = c(0.1, 0.1)
    ),
    left_vert = data.frame(
      clay = c(2/3, 2/30),
      silt = c(1/3, 1/30),
      sand = c(0, 0.9)
    ),
    right_vert = data.frame(
      clay = c(1/3, 1/30),
      silt = c(2/3, 2/30),
      sand = c(0, 0.9)
    )
  )
}


# 3D coordinates of every ticks
## gravel-sand-mud
gsm_ticks <- function() {
  list(
    bottom = data.frame(
      text = c("1:9", "1:1", "9:1"),
      mud = c(9/10, 0.5, 1/10),
      sand = c(1/10, 0.5, 9/10),
      gravel = c(0, 0, 0)
    ),
    left = data.frame(
      text = c("Trace", "0.05", "0.3", "0.8"),
      mud = c(0.95, 0.85, 0.7, 0.2),
      sand = c(0, 0, 0, 0),
      gravel = c(0.05, 0.15, 0.3, 0.8)
    )
  )
}

## sand-silt-clay
ssc_ticks <- function() {
  list(
    bottom = data.frame(
      text = c("1:2", "2:1"),
      clay = c(2/3, 1/3),
      silt = c(1/3, 2/3),
      sand = c(0, 0)
    ),
    left = data.frame(
      text = c("0.1", "0.5", "0.9"),
      clay = c(0.9, 0.5, 0.1),
      silt = c(0, 0, 0),
      sand = c(0.1, 0.5, 0.9)
    )
  )
}


# 3D coordinates of text specifying classe of soil
## gravel-sand-mud
gsm_text <- function(lan) {
  gravel <- soil_classes_gsm(lan)$gravel
  sand <- soil_classes_gsm(lan)$sand
  data.frame(
    text = soil_classes_gsm(lan)$text,
    mud = (1-gravel-((1-gravel)*sand)),
    sand = (1-gravel)*sand,
    gravel = gravel
  )
}

## sand-silt-clay
ssc_text <- function(lan) {
  
  # Sand coordinates (calculate silt and clay coordinates from sand)
  sand <- soil_classes_ssc(lan)$sand
  
  data.frame(
    text = soil_classes_ssc(lan)$text,
    clay = (1-sand)*c(5/6, 1/2, 1/6, 5/6, 1/2, 1/6, 5/6, 1/2, 1/6, 1/2),
    silt = (1-sand)*c(1/6, 1/2, 5/6, 1/6, 1/2, 5/6, 1/6, 1/2, 5/6, 1/2),
    sand = sand
  )

}

# Length of ticks and distance of ticks labels and axes labels from plot limits
meas <- function() {
  list(
    ticks = 0.04,
    ticks_lab = 0.06,
    axes_lab = 0.12
  )
}

gsm_transform  <- function(x) {
  if(x[3] <= 0.01) {
    x[3] <- (x[3]/0.01)*0.05
    x1x2 <- sum(x[1:2])
    x[2] <- (1-x[3]) * (x[2]/x1x2)
    x[1] <- (1-x[3]) * (x[1]/x1x2)
  } else if(x[3] <= 0.05) {
    x[3] <- (x[3]/0.05)*0.15
    x1x2 <- sum(x[1:2])
    x[2] <- (1-x[3]) * (x[2]/x1x2)
    x[1] <- (1-x[3]) * (x[1]/x1x2)
  }
  return(x)
}

soil_classes_gsm <- function(lan) {
  switch(
    lan,
    "en" = {
      list(
        # Gravel and sand coordinates (calculate mud coordinates from them)
        gravel = c(0.025, 0.025, 0.025, 0.025, 0.1, 0.1, 0.1, 0.1, 0.20, 0.20, 0.20, 0.5, 0.5, 0.34, 0.85), 
        sand = c(1/20, 0.3, 0.7, 19/20, 1/30, 0.3, 0.7, 14/15, 1/4, 0.7, 14/15, 1/4, 0.7, 14/15, 0.5),
        text = c("Mud", "Sandy Mud", "Muddy Sand", "Sand", "         Slightly\n      Gravelly\n  Mud", "Slightly Gravelly\nSandy Mud", "Slightly Gravelly\nMuddy Sand", "Slightly\n     Gravelly\n        Sand", "Gravelly Mud", "Gravelly Muddy Sand", "   Gravelly\n    Sand", "Muddy Gravel", "Muddy\nSandy\nGravel", "  Sandy\n     Gravel", "Gravel")
      )
    },
    "fr" = {
      list(
        # Gravel and sand coordinates (calculate mud coordinates from them)
        gravel = c(0.025, 0.025, 0.025, 0.025, 0.1, 0.1, 0.1, 0.1, 0.20, 0.20, 0.20, 0.5, 0.5, 0.34, 0.85), 
        sand = c(1/20, 0.3, 0.7, 19/20, 1/30, 0.3, 0.7, 14/15, 1/4, 0.7, 14/15, 1/4, 0.7, 14/15, 0.5),
        text = c("Limon et\nargile",
                 "Limon et argile avec sable",
                 "Sable avec limon et argile",
                 "Sable",
                 "           Limon et\n      argile avec\n   un peu \nde gravier",
                 "Limon et argile avec sable\n et un peu de gravier",
                 "Sable avec limon et argile\net un peu de gravier",
                 " Sable avec\n   un peu\n          de gravier",
                 "Limon et argile avec gravier",
                 "Sable avec gravier, limon et argile",
                 "Sable\n   avec\n        gravier",
                 "Gravier avec\nlimon et argile",
                 "Gravier avec\nsable, limon et argile",
                 "Gravier\n   avec\n       sable",
                 "Gravier")
      )
    }
  )
}

soil_classes_ssc <- function(lan) {
  switch(
    lan,
    "en" = {
      list(
        # Sand coordinates (calculate silt and clay coordinates from sand)
        sand = c(0.05, 0.05, 0.05, 0.3, 0.3, 0.3, 0.55, 0.55, 0.55, 0.92),
        text = c("Clay", "Mud", "Silt", "Sandy Clay", "Sandy Mud", "Sandy Silt", "Clavey Sand", "Muddy Sand", "Silty Sand", "Sand")
      )
    },
    "fr" = {
      list(
        # Sand coordinates (calculate silt and clay coordinates from sand)
        sand = c(0.05, 0.05, 0.05, 0.3, 0.3, 0.3, 0.55, 0.55, 0.55, 0.92),
        text = c("Argile", "Boue", "Limon", "Argile sableuse", "Boue sableuse", "Limon sableux", "Sable argileux", "Sable boueux", "Sable limoneux", "Sable")
      )
    }
  )
}

labels_gsm <- function(lan) {
  switch(
    lan,
    "en" = {
      list(
        bottom = "Sand:Mud Ratio",
        left = "%Gravel"
      )
    },
    "fr" = {
      list(
        bottom = "ratio sable:limon et argile",
        left = "% de gravier"
      )
    }
  )
}

labels_ssc <- function(lan) {
  switch(
    lan,
    "en" = {
      list(
        bottom = "Silt:Clay Ratio",
        left = "%Sand"
      )
    },
    "fr" = {
      list(
        bottom = "Ratio limon:argile",
        left = "% de sable"
      )
    }
  )
}
