#' Draw void heatmap
#' @param fname character
#' @param mypal numeric
#' @param col.cell.border character
#' @noRd
draw_void_heatmap <- function(fname, mypal, col.cell.border) {

  mM <- matrix(rep(0,64), byrow = TRUE, nrow = 8, ncol = 8)
  colnames(mM) <- paste0("col", 1:8)
  mM <- reshape2::melt(mM)
  colnames(mM) <- c("row", "col", "value")
  mM$row <- factor(mM$row, levels = 8:1)

  ggplot2::ggplot(mM, ggplot2::aes(x = col, y = row)) +
    ggplot2::geom_tile(ggplot2::aes(fill = value), color = col.cell.border) +
    ggplot2::labs(y = "row #", title = fname) +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_discrete( expand = c(0, 0) ) +
    ggplot2::scale_y_discrete( expand = c(0, 0) ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 10),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 9, color = "gray10", margin = ggplot2::margin(0,5,0,0)),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(size = 10, angle = 0, vjust = 1, color = "black"),
      axis.ticks.length = ggplot2::unit(0, "cm"),
      legend.text = ggplot2::element_text(size = 7),
      legend.title = ggplot2::element_text(angle = 90, hjust = .5, size = 9)
    ) +
    ggplot2::labs(fill = "# spikes") +
    ggplot2::scale_fill_gradient2( low = mypal[1], mid = mypal[1], high = mypal[1] ) +
    ggplot2::guides(fill = ggplot2::guide_colourbar(title.position = "right"))

}
