#' detecting overlap time window
#' @param x numeric vector
#' @param start.t numeric
#' @param end.t numeric
#' @noRd
is_overlap <- function(x, start.t, end.t) {

  check.overlap <- FALSE

  ls.int <- x
  nb.int <- length(ls.int)

  int <- data.frame(
    start = as.numeric(sapply(ls.int, FUN = function(i) unlist(strsplit(i, split = "-"))[1])),
    end = as.numeric(sapply(ls.int, FUN = function(i) unlist(strsplit(i, split = "-"))[2]))
  )

  # STOP If Start > End

  if (sum(int$start > int$end) > 0) check.overlap <- TRUE

  # STOP If "Start Exclusion" before "Start Time Window"

  if (sum(int$start < start.t) > 0) check.overlap <- TRUE

  # STOP If "Start Exclusion" after "End Time Window"

  if (sum(int$start > end.t) > 0) check.overlap <- TRUE

  # STOP If "End Exclusion" after "End Time Window"

  if (sum(int$end > end.t) > 0) check.overlap <- TRUE

  # STOP If "OVERLAP"

  j <- 1

  while (j < nb.int & check.overlap == FALSE) {

    test <- (int[j,1] <= int[ (j+1):nb.int, 2] & int[j,2] >= int[  (j+1):nb.int, 2]) |
      (int[j,1] <= int[ (j+1):nb.int, 1] & int[j,2] >= int[  (j+1):nb.int, 1])

    if (sum(test) > 0) check.overlap <- TRUE

    j <- j + 1

  }

  return(check.overlap)

}
