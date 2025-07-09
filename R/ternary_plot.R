#' Title
#' 
#' Description
#'
#' @param TODO:
#'
#' @return plot
#' @export

ternary_plot <- function(df, type = NULL, lan = "en", group_by = NULL, col, pch, alpha = 1, main = NULL, ...) {
  
  switch(
    type,
    gsm = {
      
      if(any(df[,c("gravel", "sand", "mud")] > 1)) {
        df$gravel <- df$gravel/100
        df$sand <- df$sand/100
        df$mud <- df$mud/100
      }
      
      df[,"x"] <- apply(df[,c("mud", "sand", "gravel")],
                        1,
                        function(x) ternary_x(x, gsm = TRUE))
      df[,"y"] <- apply(df[,c("mud", "sand", "gravel")],
                        1,
                        function(x) ternary_y(x, gsm = TRUE))
      
      ternary_base_gsm(lan)    

      if(!is.null(group_by)) {
        names(col) <- levels(factor(df[,group_by]))
        col <- scales::alpha(col[factor(df[,group_by])], alpha)
#        names(pch) <- levels(factor(df[,group_by]))
#        pch <- pch[factor(df[,group_by])]
      }

      points(df$x, df$y,
             pch = pch,
             col = col)

      if(!is.null(group_by)) {
        legend("topleft",
               legend = levels(factor(df[,group_by])),
               pch = pch,
               col = col[levels(factor(df[,group_by]))])
      }
      title(main = main)
    },
    ssc = {
      ternary_base()
      ternary_base_ssc(lan)
    }
  )
}


#' Title
#' 
#' Description
#'
#' @param TODO: 
#'
#' @return TODO: 

ternary_base <- function() {
  
  # Find vertices points
  xy <- data.frame(x = apply(vertices(), 1, ternary_x),
                   y = apply(vertices(), 1, ternary_y),
                   pos = c("bl", "top", "br"),
                   row.names = NULL)
  # Plot extent of the vertices
  plot(xy[,c("x", "y")], 
       type = "p",
       pch = 16,
       bty = "n",
       axes = FALSE,
       ylab = "",
       xlab = "",
       col = "transparent")
  
  # Plot the border of the triangle
  segments(x0 = xy[xy$pos == "bl","x"], 
           x1 = xy[xy$pos == "top","x"],
           y0 = xy[xy$pos == "bl","y"], 
           y1 = xy[xy$pos == "top","y"])
  segments(x0 = xy[xy$pos == "top","x"], 
           x1 = xy[xy$pos == "br","x"],
           y0 = xy[xy$pos == "top","y"], 
           y1 = xy[xy$pos == "br","y"])
  segments(x0 = xy[xy$pos == "br","x"], 
           x1 = xy[xy$pos == "bl","x"],
           y0 = xy[xy$pos == "br","y"], 
           y1 = xy[xy$pos == "bl","y"])

}


ternary_base_gsm <- function(lan) {

  ternary_base()

  # Get every limits segments coordinates
  xy_seg <- lapply(gsm_limits(), function(k) {
    
    # Transform coordinates of limits into x and y coordinates
    x = apply(k, 1, ternary_x)
    y = apply(k, 1, ternary_y)

    xy <- data.frame(
      x0 = x[1],
      x1 = x[2],
      y0 = y[1],
      y1 = y[2]
    )

    return(xy)

  }) |>
    do.call(what = rbind, args = _)

  # Plot the segments
  segments(x0 = xy_seg$x0,
           x1 = xy_seg$x1,
           y0 = xy_seg$y0,
           y1 = xy_seg$y1)


  # Plot ticks
  xy_ticks <- lapply(names(gsm_ticks()), function(k) {
    
    # Transform coordinates of ticks into x and y coordinates
    x = apply(gsm_ticks()[[k]][,-1], 1, ternary_x)
    y = apply(gsm_ticks()[[k]][,-1], 1, ternary_y)

    xy <- data.frame(x = x,
                     y = y,
                     side = k,
                     text = gsm_ticks()[[k]][,"text"])
  }) |>
    do.call(what = rbind, args = _)

  # Make ticks and ticks labels
  apply(xy_ticks, 1, function(x) {
    switch(
      x["side"],
      bottom = {
        segments(x0 = as.numeric(x["x"]),
                 x1 = as.numeric(x["x"]),
                 y0 = as.numeric(x["y"]),
                 y1 = as.numeric(x["y"])-meas()$ticks)
        text(as.numeric(x["x"]),
             as.numeric(x["y"])-meas()$ticks_lab,
             x["text"],
             adj = c(0.5, 0.5),
             xpd = NA) # xpd = NA allows to put text everywhere on the plot (inside and outside the plot margin)
      },
      left = {
        segments(x0 = as.numeric(x["x"]),
                 x1 = as.numeric(x["x"])-meas()$ticks,
                 y0 = as.numeric(x["y"]),
                 y1 = as.numeric(x["y"]))
        text(as.numeric(x["x"])-meas()$ticks_lab,
             as.numeric(x["y"]),
             adj = c(1, 0.5),
             x["text"],
             xpd = NA) # xpd = NA allows to put text everywhere on the plot (inside and outside the plot margin)
      }
    )
  })

  # Axis labels
  ## Left margin
  text(x = 0.25 - meas()$axes_lab,
       y = 0.5,
       labels_gsm(lan)$left,
       adj = c(1, 1),
       xpd = NA,
       srt = 60) # Rotation of text

  ## Bottom margin
  text(x = 0.5,
       y = 0 - meas()$axes_lab,
       labels_gsm(lan)$bottom,
       xpd = NA)

  # Class labels
  xy_class <- data.frame(x = apply(gsm_text(lan)[,-1], 1, ternary_x),
                         y = apply(gsm_text(lan)[,-1], 1, ternary_y),
                         text = gsm_text(lan)$text)
  if(lan == "fr") text_size <- 0.6 else text_size <- 0.7
  text(xy_class$x,
       xy_class$y,
       xy_class$text,
       cex = text_size)

}

ternary_base_ssc <- function(lan) {

  ternary_base()

  # Get every limits segments coordinates
  xy_seg <- lapply(ssc_limits(), function(k) {
    
    # Transform coordinates of limits into x and y coordinates
    x = apply(k, 1, ternary_x)
    y = apply(k, 1, ternary_y)

    xy <- data.frame(
      x0 = x[1],
      x1 = x[2],
      y0 = y[1],
      y1 = y[2]
    )

    return(xy)

  }) |>
    do.call(what = rbind, args = _)

  # Plot the segments
  segments(x0 = xy_seg$x0,
           x1 = xy_seg$x1,
           y0 = xy_seg$y0,
           y1 = xy_seg$y1)


  # Plot ticks
  xy_ticks <- lapply(names(ssc_ticks()), function(k) {
    
    # Transform coordinates of ticks into x and y coordinates
    x = apply(ssc_ticks()[[k]][,-1], 1, ternary_x)
    y = apply(ssc_ticks()[[k]][,-1], 1, ternary_y)

    xy <- data.frame(x = x,
                     y = y,
                     side = k,
                     text = ssc_ticks()[[k]][,"text"])
  }) |>
    do.call(what = rbind, args = _)

  # Make ticks and ticks labels
  apply(xy_ticks, 1, function(x) {
    switch(
      x["side"],
      bottom = {
        segments(x0 = as.numeric(x["x"]),
                 x1 = as.numeric(x["x"]),
                 y0 = as.numeric(x["y"]),
                 y1 = as.numeric(x["y"])-meas()$ticks)
        text(as.numeric(x["x"]),
             as.numeric(x["y"])-meas()$ticks_lab,
             adj = c(0.5, 0.5),
             x["text"],
             xpd = NA) # xpd = NA allows to put text everywhere on the plot (inside and outside the plot margin)
      },
      left = {
        segments(x0 = as.numeric(x["x"]),
                 x1 = as.numeric(x["x"])-meas()$ticks,
                 y0 = as.numeric(x["y"]),
                 y1 = as.numeric(x["y"]))
        text(as.numeric(x["x"])-meas()$ticks_lab,
             as.numeric(x["y"]),
             x["text"],
             adj = c(1, 0.5),
             xpd = NA) # xpd = NA allows to put text everywhere on the plot (inside and outside the plot margin)
      }
    )
  })

  # Axis labels
  ## Left margin
  text(x = 0.25 - meas()$axes_lab,
       y = 0.5,
       labels_ssc(lan)$left,
       adj = c(1, 1),
       xpd = NA,
       srt = 60) # Rotation of text

  ## Bottom margin
  text(x = 0.5,
       y = 0 - meas()$axes_lab,
       labels_ssc(lan)$bottom,
       xpd = NA)

  # Class labels
  xy_class <- data.frame(x = apply(ssc_text(lan)[,-1], 1, ternary_x),
                         y = apply(ssc_text(lan)[,-1], 1, ternary_y),
                         text = ssc_text(lan)$text)
  if(lan == "fr") text_size <- 0.65 else text_size <- 0.7
  text(xy_class$x,
       xy_class$y,
       xy_class$text,
       cex = text_size)

}
