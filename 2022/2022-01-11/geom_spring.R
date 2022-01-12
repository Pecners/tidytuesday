create_spring <- function(x, y, xend, yend, diameter = 1, tension = 0.75, n = 50) {
  if (tension <= 0) {
    rlang::abort("`tension` must be larger than zero.")
  }
  if (diameter == 0) {
    rlang::abort("`diameter` can not be zero.")
  }
  if (n == 0) {
    rlang::abort("`n` must be greater than zero.")
  }
  # Calculate direct length of segment
  length <- sqrt((x - xend)^2 + (y - yend)^2)
  
  # Figure out how many revolutions and points we need
  n_revolutions <- length / (diameter * tension)
  n_points <- n * n_revolutions
  
  # Calculate sequence of radians and x and y offset
  radians <- seq(0, n_revolutions * 2 * pi, length.out = n_points)
  x <- seq(x, xend, length.out = n_points)
  y <- seq(y, yend, length.out = n_points)
  
  # Create the new data
  data.frame(
    x = cos(radians) * diameter/2 + x,
    y = sin(radians) * diameter/2 + y
  )
}

GeomSpring <- ggproto("GeomSpring", Geom,
                      setup_data = function(data, params) {
                        if (is.null(data$group)) {
                          data$group <- seq_len(nrow(data))
                        }
                        if (anyDuplicated(data$group)) {
                          data$group <- paste(data$group, seq_len(nrow(data)), sep = "-")
                        }
                        data
                      },
                      draw_panel = function(data, panel_params, coord, n = 50, arrow = NULL,
                                            lineend = "butt", linejoin = "round", linemitre = 10,
                                            na.rm = FALSE) {
                        cols_to_keep <- setdiff(names(data), c("x", "y", "xend", "yend"))
                        springs <- lapply(seq_len(nrow(data)), function(i) {
                          spring_path <- create_spring(data$x[i], data$y[i], data$xend[i], 
                                                       data$yend[i], data$diameter[i],
                                                       data$tension[i], n)
                          cbind(spring_path, unclass(data[i, cols_to_keep]))
                        })
                        springs <- do.call(rbind, springs)
                        GeomPath$draw_panel(
                          data = springs, 
                          panel_params = panel_params, 
                          coord = coord, 
                          arrow = arrow, 
                          lineend = lineend, 
                          linejoin = linejoin, 
                          linemitre = linemitre, 
                          na.rm = na.rm
                        )
                      },
                      required_aes = c("x", "y", "xend", "yend"),
                      default_aes = aes(
                        colour = "black", 
                        size = 0.5, 
                        linetype = 1L, 
                        alpha = NA, 
                        diameter = 1, 
                        tension = 0.75
                      )
)

geom_spring <- function(mapping = NULL, data = NULL, stat = "identity", 
                        position = "identity", ..., n = 50, arrow = NULL, 
                        lineend = "butt", linejoin = "round", na.rm = FALSE,
                        show.legend = NA, inherit.aes = TRUE) {
  layer(
    data = data, 
    mapping = mapping, 
    stat = stat, 
    geom = GeomSpring, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(
      n = n, 
      arrow = arrow, 
      lineend = lineend, 
      linejoin = linejoin, 
      na.rm = na.rm, 
      ...
    )
  )
}
