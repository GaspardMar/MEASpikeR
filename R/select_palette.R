#' Select Palette
#' @param name character
#' @noRd
select_palette <- function(name = "viridis2") {

  name <- tolower(name)

  ls.palette.rcolorbrewer <- c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys",
                               "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds",
                               "YlGn", "YlGnBu", "YlOrBr", "YlOrRd", "BrBG", "PiYG", "PRGn",
                               "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral")

  if (name %in% tolower(ls.palette.rcolorbrewer))
    name <- ls.palette.rcolorbrewer[tolower(ls.palette.rcolorbrewer) == name]

  if (name == "jet")
    outpal <- matlab::jet.colors(16)
  if (name == "magma")
    outpal <- viridis::viridis_pal(alpha = 1, begin = 0, end = 1, direction = 1, option = "A")(16)
  # "magma" ("A"), "inferno" ("B"), "plasma" ("C"), "viridis" ("D"), "cividis" ("E")
  if (name == "magma")
    outpal <- viridis::viridis_pal(alpha = 1, begin = 0, end = 1, direction = 1, option = "A")(16)
  if (name == "inferno")
    outpal <- viridis::viridis_pal(alpha = 1, begin = 0, end = 1, direction = 1, option = "B")(16)
  if (name == "plasma")
    outpal <- viridis::viridis_pal(alpha = 1, begin = 0, end = 1, direction = 1, option = "C")(16)
  if (name == "viridis")
    outpal <- viridis::viridis_pal(alpha = 1, begin = 0, end = 1, direction = 1, option = "D")(16)
  if (name == "cividis")
    outpal <- viridis::viridis_pal(alpha = 1, begin = 0, end = 1, direction = 1, option = "E")(16)
  # "Viridis improved" = viridis starting on the turquoise tones...
  if (name == "viridis2")
    outpal <- grDevices::colorRampPalette(viridis::viridis_pal(alpha = 1, begin = 0, end = 1, direction = 1, option = "D")(16)[9:16])(16)
  if (name %in% ls.palette.rcolorbrewer)
    outpal <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 9, name))(16)[16:1]
  if (name == "zissou1")
    outpal <- grDevices::colorRampPalette(wesanderson::wes_palette(name = "Zissou1", n = 10, type = "continuous"))(16)[16:1]

  return(outpal)

}
