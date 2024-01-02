#' Remove Overlapping intervals
#' @param x numeric vector
#' @param start.t numeric
#' @param end.t numeric
#' @noRd
rm_overlapping_int <- function(x, start.t, end.t) {

  ls.int <- x
  nb.int <- length(ls.int)
  j <- 1

  int <- data.frame(
    start = as.numeric(sapply(ls.int, FUN = function(i) unlist(strsplit(i, split = "-"))[1])),
    end = as.numeric(sapply(ls.int, FUN = function(i) unlist(strsplit(i, split = "-"))[2]))
  )

  int[ int$start < start.t, 1] <- start.t
  int[ int$start > end.t, 1] <- end.t
  int[ int$end > end.t, 2] <- end.t

  while (j < nb.int) {

    test <- (int[j,1] <= int[ (j+1):nb.int, 2] & int[j,2] >= int[  (j+1):nb.int, 2]) |
      (int[j,1] <= int[ (j+1):nb.int, 1] & int[j,2] >= int[  (j+1):nb.int, 1])

    if (sum(test) > 0) {

      add <- which(test == 1)[1] + j
      ls.int[j] <- paste(min(int[j,1], int[add,1]), max(int[j,2], int[add,2]), sep = "-")
      ls.int <- ls.int[-add]
      nb.int <- nb.int - 1

      int <- data.frame(
        start = as.numeric(sapply(ls.int, FUN = function(i) unlist(strsplit(i, split = "-"))[1])),
        end = as.numeric(sapply(ls.int, FUN = function(i) unlist(strsplit(i, split = "-"))[2]))
      )

    } else { j <- j + 1 }

  }

  ll <- list(ls.int = ls.int, TeX = sum(int[,2]-int[,1]), nb.int = length(ls.int))
  ll

}
