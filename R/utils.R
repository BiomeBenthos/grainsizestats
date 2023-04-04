# Transform 3D coordinates in 2D coordinates
ternary_y  <- function(x) {
  return( sqrt(0.75) *  x[3] / sum(x) )
}

ternary_x  <- function(x) {
  return( (x[2] + 0.5 * x[3]) / sum(x) )
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
      mud = c(0.5, 0),
      sand = c(0, 0.5),
      gravel = c(0.5, 0.5)
    ),
    low_horiz = data.frame(
      mud = c(0.9, 0),
      sand = c(0, 0.9),
      gravel = c(0.1, 0.1)
    ),
    left_vert = data.frame(
      mud = c(2/3, 2/30),
      sand = c(1/3, 1/30),
      gravel = c(0, 0.9)
    ),
    right_vert = data.frame(
      mud = c(1/3, 1/30),
      sand = c(2/3, 2/30),
      gravel = c(0, 0.9)
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


## sand-silt-clay
ssc_text <- function() {
  
  # Sand coordinates (calculate silt and clay coordinates from sand)
  sand <- c(0.05, 0.05, 0.05, 0.3, 0.3, 0.3, 0.55, 0.55, 0.55, 0.92) 
  
  data.frame(
    text = c("Clay", "Mud", "Silt", "Sandy Clay", "Sandy Mud", "Sandy Silt", "Clavey Sand", "Muddy Sand", "Silty Sand", "Sand"),
    clay = (1-sand)*c(5/6, 1/2, 1/6, 5/6, 1/2, 1/6, 5/6, 1/2, 1/6, 1/2),
    silt = (1-sand)*c(1/6, 1/2, 5/6, 1/6, 1/2, 5/6, 1/6, 1/2, 5/6, 1/2),
    sand = sand
  )

}

# Length of ticks and distance of ticks labels and axes labels from plot limits
meas <- function() {
  list(
    ticks = 0.02,
    ticks_lab = 0.05,
    axes_lab = 0.1
  )
}