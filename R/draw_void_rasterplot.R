#' Draw void rasterplot
#' @param fname character
#' @param start numeric
#' @noRd
draw_void_rasterplot <- function(fname, start, end) {

  time.range <- end - start
  step <- 100
  if (time.range <= 50) step <- 5
  if (time.range > 50 & time.range <= 100) step <- 10
  if (time.range > 100 & time.range <= 200) step <- 25
  if (time.range > 200 & time.range <= 600) step <- 50

  ggplot2::ggplot() + ggplot2::theme_void() +
    ggplot2::labs(x = "Time [s]", y = "# Electrode", title = fname) +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous(breaks = seq(start, end, step), expand = c(.01, 0), limits = c(start, end)) +
    ggplot2::scale_y_continuous(
      breaks = seq(1, 64, 1), expand = c(0, 0), limits = c(64.5, 0.5) ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 10),
      axis.text.x = ggplot2::element_text(size = 9, angle = 0, vjust = 0.3, color = "gray5"),
      axis.text.y = ggplot2::element_text(size = 3, color = "gray5"),
      axis.title = ggplot2::element_text(size = 10, angle = 0, vjust = 0.3, color = "black"),
      legend.text = ggplot2::element_text(size = 7),
      panel.grid.major  = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(angle = 90, hjust = .5, size = 9),
      axis.line.x = ggplot2::element_line(colour = "black", size = .2),
      axis.ticks.x =  ggplot2::element_line(colour = "gray5", size = .2)
    )

}
