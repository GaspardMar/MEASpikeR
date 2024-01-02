#' Draw void plot
#' @param channel_number character
#' @param type integer
#' @noRd
draw_void_plot <- function(channel_number, type = 1) {

  g <- ggplot2::ggplot() + ggplot2::theme_void() +
    ggplot2::scale_x_continuous(breaks = seq(-1, 2.5, .5), limits = c(-1, 2.5)) +
    ggplot2::scale_y_continuous(labels = NULL, limits = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(y = paste0("Amplitude (", paste0("\u00b5","V"),")"),
                  x = "Time (ms)")+
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 7, hjust = 0.5),
      axis.text = ggplot2::element_text(size = 7, color = "black"),
      # axis.title = ggplot2::element_blank(),
      panel.grid.major  = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = .4),
      axis.ticks.x = ggplot2::element_line(colour = "gray10", size = .3),
      axis.ticks.length.x = ggplot2::unit(.15, "cm"),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black", size = .4)
    )

  if (type == 1) g <- g + ggplot2::ggtitle(paste0("channel ", channel_number, ", n=0"))+
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      )

  if (type == 2) g <- g + ggplot2::geom_segment(ggplot2::aes(x = -1, y = 0, xend = 2.5, yend = 1), size = 1) +
      ggplot2::geom_segment(ggplot2::aes(x = -1, y = 1, xend = 2.5, yend = 0), size = .8) +
      ggplot2::ggtitle(paste0("channel ", channel_number, ", excluded")) +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      )

  if (type == 3) g <- g + ggplot2::ggtitle(paste0("channel ", channel_number, ", n=", nrow(dat[dat$channel == j,])))

  return(g)

}
