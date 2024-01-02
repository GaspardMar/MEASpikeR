#' Detect burst with mi method
#' @param v.isi integer
#' @param min.spike.within.burst integer
#' @param MaxStartInt integer
#' @param MaxEndInt integer
#' @param MinInterburstInt integer
#' @param min.burst.duration integer
#' @noRd
detect_burst_mi <- function( v.isi, min.spike.within.burst = 3,
                             MaxStartInt = 170, MaxEndInt = 300, MinInterburstInt = 200,
                             min.burst.duration = 50 )

{
  #print(v.isi)

  burst <- rep(NA, length(v.isi))

  #---------------------------#

  #-- STEP 1: Detect bursts --#

  #---------------------------#

  pos <- 1

  n.spikes <- length(v.isi)

  k <- 1

  ls.bursts <- list()

  while( pos < n.spikes ) {

    find.start <- which(v.isi[pos:n.spikes] <= MaxStartInt)

    if (length(find.start) == 0) {

      pos <- n.spikes

    } else {

      pos <- pos + min(find.start) - 1

      beg.burst <- pos-1

      find.end <- which(v.isi[(pos+1):n.spikes] > MaxEndInt)

      find.end <- ifelse( length(find.end) == 0,
                          n.spikes - pos,
                          min(which(v.isi[(pos+1):n.spikes] > MaxEndInt)) )

      end.burst <- beg.burst+find.end

      ls.bursts[[k]] <- c(beg.burst, end.burst)

      pos <- end.burst+1
      k <- k+1

    }

  }  # end_while

  # ls.bursts

  #--------------------------#

  #-- STEP 2: Merge bursts --#

  #--------------------------#

  if ( length(ls.bursts) > 0 ) {

    j <- 1

    merged.ls.bursts <- ls.bursts

    while ( j < length(merged.ls.bursts) ) {

      if( merged.ls.bursts[[j+1]][1] - merged.ls.bursts[[j]][2] > 1 ) {

        j <- j + 1

      } else {

        if ( v.isi[ merged.ls.bursts[[j+1]][1] ] > MinInterburstInt ) {

          j <- j + 1

        } else {

          if (j == 1) {

            merged.ls.bursts <- c(
              list(c(merged.ls.bursts[[1]][1], merged.ls.bursts[[2]][2])),
              merged.ls.bursts[-c(1:2)] )

          } else {

            merged.ls.bursts <- c(
              merged.ls.bursts[1:(j-1)],
              list(c(merged.ls.bursts[[j]][1], merged.ls.bursts[[j+1]][2])),
              merged.ls.bursts[(j+2):length(merged.ls.bursts)] )

          }

        }

      }

    }

    # merged.ls.bursts

    #-----------------------#

    #-- STEP 3: Exclusion --#

    #-----------------------#

    ix.beg <- sapply(merged.ls.bursts, function(i) unlist(i)[1])
    ix.end <- sapply(merged.ls.bursts, function(i) unlist(i)[2])

    nspike.burst <- ix.end - ix.beg+1

    duration.burst <- sapply( 1:length(merged.ls.bursts), function(i) {
      #print(v.isi[ seq(ix.beg[i]+1, ix.end[i])])
      sum(v.isi[ seq(ix.beg[i]+1, ix.end[i])], na.rm = TRUE)})

    # print(nspike.burst)
    # print(min.spike.within.burst)
    # print(duration.burst)
    # print(min.burst.duration)

    keep.ix <- c( nspike.burst >= min.spike.within.burst & duration.burst >= min.burst.duration )
    #print(keep.ix)

    if ( sum(keep.ix) > 0 ) {

      merged.ls.bursts <- merged.ls.bursts[ keep.ix ]
      ix.beg <- sapply(merged.ls.bursts, function(i) unlist(i)[1])
      ix.end <- sapply(merged.ls.bursts, function(i) unlist(i)[2])

      for (i in 1:length(merged.ls.bursts)) {
        burst[ seq(ix.beg[i], ix.end[i]) ] <- paste0("burst", i)
      }

    }

  }

  return(burst)

} # end_detect.burst
