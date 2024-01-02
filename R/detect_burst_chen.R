#' Detect burst with chen method
#' @param v.isi an integer vector
#' @param min.spike.within.burst integer
#' @noRd
detect_burst_chen <- function(v.isi, min.spike.within.burst = 3) {

  if (length(v.isi) == 0) {

    burst <- NULL

  } else {

    if (length(v.isi) %in% c(1,2)) { burst <- rep(NA, length(v.isi)) } else {

      chen.ml <- function(x) {
        x <- stats::na.omit(x)
        mean(x[ x < mean(x) ])
      }

      v.ml <- chen.ml(v.isi)

      ix <- v.isi < v.ml
      burst <- c()
      num <- 1

      for (k in 2:length(ix)) {

        if(!is.na(ix[k])){

        if (ix[k] == TRUE) burst[c(k-1,k)] <- paste0("burst", num)

        if (ix[k] == FALSE & is.na(ix[k-1])) burst[c(k-1,k)] <- NA

        if (ix[k] == FALSE & ix[k-1] == FALSE & !(is.na(ix[k-1]))) burst[c(k-1,k)] <- NA

        # end of the burst #
        if (ix[k] == FALSE & ix[k-1] == TRUE & !(is.na(ix[k-1])))
        {
          burst[k] <- NA
          if (sum(burst == paste0("burst", num), na.rm =TRUE) < min.spike.within.burst) {
            burst[ burst == paste0("burst", num) & !(is.na(burst)) ] <- NA } else {
              num <- num+1 }
        }

        }

      }

      if (sum(burst == paste0("burst", num), na.rm =TRUE) < min.spike.within.burst) {
        burst[ burst == paste0("burst", num) & !(is.na(burst)) ] <- NA }

    } }

  return(burst)

}
