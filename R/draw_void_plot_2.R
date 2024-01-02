#' Draw void plot 2
#' @param channel_number character
#' @param  interval.max numeric
#' @param type integer
#' @noRd
draw_void_plot_2 <- function(channel_number, interval.max, type = 1) {

  g <- ggplot2::ggplot() + ggplot2::theme_void() +
    ggplot2::scale_x_continuous(breaks = seq(0, 500, 50), minor_breaks = seq(0, 500, 10),
                       limits = c(-1, interval.max+1), expand = c(0,0)) +
    ggplot2::scale_y_continuous(
      breaks = NULL,
      labels = NULL, limits = c(0, 1), expand = c(0,0)) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 7, hjust = 0.5),
      axis.text = ggplot2::element_text(size = 7, color = "black"),
      axis.title = ggplot2::element_blank(),
      panel.grid.major  = ggplot2::element_line(color = "gray92", size = .1),
      panel.grid.minor = ggplot2::element_line(color = "gray95", size = .1),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = .4),
      axis.ticks = ggplot2::element_line(colour = "gray10", size = .3),
      axis.ticks.length = ggplot2::unit(.15, "cm"),
      axis.line = ggplot2::element_line(colour = "black", size = .4)
    )

  if (type == 1) g <- g + ggplot2::ggtitle(paste0("channel ", channel_number, ", n=0"))+
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()
      )

  if (type == 2) g <- g + ggplot2::geom_segment(ggplot2::aes(x = -1, y = 0, xend = interval.max+1, yend = 1), size = 1) +
      ggplot2::geom_segment(ggplot2::aes(x = -1, y = 1, xend = interval.max+1, yend = 0), size = 1) +
      ggplot2::ggtitle(paste0("channel ", channel_number, ", excluded"))+
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()
      )

  return(g)

}
