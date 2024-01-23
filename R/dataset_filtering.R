#' Data Filtering
#'
#' Filters the data according to the conditions of the guideline
#'
#' @param data.path The path directory to the folder containing the data and the guideline.
#' @param guideline_file_name The name of the excel file containing the guideline (by default "MEA_Spikes_ANA_R_guideline")
#' @param sheet_used Number indicating the excel sheet where the guideline is located (by default 1)
#' @param MinFR Minimum number of spike considered in the guideline time window.
#' @param spike.sorting Boolean, if TRUE a spike sorting is performed (by default FALSE)
#' @param min.spike Minimum number of spikes in the considered time window to be used during spike sorting (by default 10)
#' @param validation.index The method used to determine the optimal number of clusters (by default "silhouette")
#' @param cutoff.index The minimum threshold for the selection method in
#' \code{validation.index} to determine the optimal spike sorting cluster number (by default 0.5)
#' @param nb.electrode Number which specifies the number of electrodes located on the MEA chip (by default 64)
#' @param save.rDATA Boolean, if TRUE (by default) the filtered data are saved in the directory specified in \code{data.path}
#' @param filename Character, the name of the .Rdat file in which filtered data will
#' be saved if \code{save.rDATA = TRUE} (by default "MEA_DATA2ANALYZE")
#'
#' @return An object of class 'MEASpikeR' which will be the input of all other functions of the MEASpikeR package.
#' if \code{save.rDATA = TRUE}, a ".Rdat" file called by default "MEA_DATA2ANALYZE" is saved
#' in the \code{data.path} directory
#' @examples \dontrun{
#' dataset_filtering(data.path = "C:/Users/prenom.nom/Documents/data",
#' guideline_file_name = "MEA_Spikes_ANA_R_guideline")
#' }
#'
#' @author François-Xavier Lejeune, Gaspard Martet, Carine Dalle
#' @references Dalle C., Martet G., Kaddouri Y., Rebola N., Whitmarsh S., Lejeune FX.(2023). **_MEASpikeR_: a new package
#' for spike analysis and visualization of _in vitro_ MEA data**.
#' @export
dataset_filtering <- function(
    data.path,
    guideline_file_name = "MEA_Spikes_ANA_R_guideline",
    sheet_used = 1,
    MinFR = 0,
    spike.sorting = FALSE,
    min.spike = 10,
    validation.index = "silhouette",
    cutoff.index = 0.5,
    nb.electrode = 64,
    save.rDATA = TRUE,
    filename = "MEA_DATA2ANALYZE"
) {

  if (file.exists(paste0(data.path, "/",guideline_file_name, ".xlsx")) == FALSE){
    stop("Path to the guideline file doesn't exist. Please check the name of your guideline file
         and specify it in guideline_file_name argument", call. = FALSE)
  }

  guideline <- openxlsx::read.xlsx(paste0(data.path, "/",guideline_file_name, ".xlsx"),
                                   sheet = sheet_used,
                                   rowNames = FALSE, colNames = FALSE, startRow = 2,
                                   skipEmptyCols = FALSE,
                                   cols = 1:12)


  # read.xlsx does not read the last empty columns of a data.frame
  # Need in this case to add NA columns to have exactly 11 columns

  if (ncol(guideline) < 12) guideline[,(ncol(guideline)+1):12] <- NA

  colnames(guideline) <- c("ConditionNumber", "Filename_spike", "Filename_freq",
                           "removeChannel",
                           "TimeLimitStart", "TimeLimitEnd", "TimeToExclude",
                           "ExperimentalGroup","ExperimentalCondition",
                           "ExperimentalComment", "InformationFolderName", "AnalysisComment")

  guideline$removeChannel <- as.character(guideline$removeChannel)
  guideline$removeChannel <- gsub("\\.", ",", guideline$removeChannel)

  #--------------------------------------------#

  #-- Check conditions in the guideline file --#

  #--------------------------------------------#

  # STOP IF guideline$ConditionNumber has empty (NA) or duplicated values

  if (sum(is.na(guideline$ConditionNumber)) > 0)
    stop("Missing ConditionNumber values in the guideline file",
         call. = FALSE)

  if (sum(as.numeric(table(guideline$ConditionNumber) > 1)) > 0)
    stop("Duplicated ConditionNumber values found in the guideline file",
         call. = FALSE)

  # STOP IF guideline$TimeLimitStart or guideline$TimeLimitEnd has empty (NA) values

  if (sum(is.na(guideline$TimeLimitStart) | is.na(guideline$TimeLimitEnd)) > 0)
    stop("Missing values of the time window in the guideline file",
         call. = FALSE)

  # STOP IF guideline$removeChannel has an input channel number out of 1:nb.electrode

  guideline$removeChannel <- gsub(" ", "", guideline$removeChannel)
  guideline$removeChannel[guideline$removeChannel == ""] <- NA

  if (length(stats::na.omit(guideline$removeChannel)) > 0) {

    ls.rm.ch <- strsplit(stats::na.omit(guideline$removeChannel), split = ",")

    if ( !(all(sapply(1:length(ls.rm.ch),
                      function(i) all(ls.rm.ch[[i]] %in% as.character(1:nb.electrode))))) )
      stop(paste0("Input channel to remove out of 1:", nb.electrode, " in the guideline file"), call. = FALSE)

    if ( !(all(sapply(1:length(ls.rm.ch),
                      function(i) length(sort(unique(as.numeric(ls.rm.ch[[i]])))) != nb.electrode ))))
      stop("Number of channels to be removed must be less than the number of electrodes in all guideline conditions",
           call. = FALSE)

  }

  # STOP IF one spike file or one freq file is not found in the data directory

  ls.f_spike <- guideline$Filename_spike # list of files
  ls.f_freq <- guideline$Filename_freq

  test.f_spike <- !(ls.f_spike %in% list.files(data.path))
  if (sum(test.f_spike) > 0)
    stop(paste("Condition(s)#", paste(which(test.f_spike), collapse= "|"), "not found"),
         call. = FALSE)

  test.f_freq <- !(ls.f_freq %in% list.files(data.path))
  if (sum(test.f_freq) > 0)
    stop(paste("Condition(s)#", paste(which(test.f_freq), collapse= "|"), "not found"),
         call. = FALSE)

  ls.f_spike <- ls.f_spike[ ls.f_spike %in% list.files(data.path) ]
  ls.f_freq <- ls.f_freq[ ls.f_freq %in% list.files(data.path) ]

  # STOP if guideline$TimeToExclude with overlapping intervals or intervals outside the Time Window

  check.int <- function(i) {
    ix.ti <- unlist(strsplit( guideline[i, "TimeToExclude"], split = ","))
    MEASpikeR:::is_overlap(ix.ti, guideline$TimeLimitStart[i], guideline$TimeLimitEnd[i])
  }

  if (sum(!(is.na(guideline$TimeToExclude))) > 0) {

    ix <- which(!(is.na(guideline$TimeToExclude)))

    test.int.excluded.time <- sapply(ix, FUN = check.int)

    if (sum(test.int.excluded.time) > 0)
      stop(paste("Invalid TimeToExclude in condition(s)#",
                 paste(guideline$ConditionNumber[ which(test.int.excluded.time) ], collapse= "|")),
           call. = FALSE)

  }


  #----------#

  #-- Main --#

  #----------#

  nb.condition <- length(ls.f_spike) # number of files

  info.cond <- NULL

  for (i in 1:nb.condition) { # Begin_For_Condition

    cat("### condition =", i, "\n")

    #-- Import data from "Spike" csv file --#

    filename_spike <- ls.f_spike[i]

    shortname <- paste(unlist(strsplit(filename_spike, split = ".modat"))[1],
                       unlist(strsplit(filename_spike, split = "Spikes1\\+"))[2], sep = "_")

    shortname <- gsub(".csv", "", shortname)

    # If empty spike file, return a void filtered spike dataset with the name of the condition

    dat <- tryCatch(
      data.table::fread(paste(data.path, filename_spike, sep = "/"), skip = 5, header = FALSE),
      error = function(e) data.frame(matrix(NA, nrow = 0, ncol = 80)) )

    colnames(dat) <- c("channel", "time_of_day", "within_session_time", "within_session_time_ms",
                       "within_trace_time_ms", "cluster_id", "trace_num", "pre_ms", "post_ms",
                       paste0("y", 0:70, "_mV"))

    empty.file <- nrow(dat) == 0

    #---------------------------------------------------#

    #-- Import data from "Freq" csv file              --#

    #-- Get recording duration using the first column --#

    #---------------------------------------------------#

    filename_freq <- ls.f_freq[i]

    record_duration <- data.table::fread(paste(data.path, filename_freq, sep = "/"), skip = 5, header = FALSE)[,1]
    record_duration <- as.integer(utils::tail(record_duration, 1))

    #-------------------------------------#

    #-- Start DATA Filtering Processing --#

    #-------------------------------------#

    #-----------------------------------------#

    #-- STEP 1: Channel inclusion/exclusion --#

    #-----------------------------------------#

    if (length(stats::na.omit(guideline[ i, "removeChannel" ])) == 0) {

      ix.ch <- 1:nb.electrode

    } else {

      ix.rm.ch <- unlist(strsplit( guideline[ i, "removeChannel" ], split = ","))
      ix.rm.ch <- as.numeric(ix.rm.ch)
      ix.ch <- setdiff(1:nb.electrode, ix.rm.ch)
      ix.ch <- sort(unique(ix.ch))

    }

    nb.excluded <- nb.electrode-length(ix.ch)

    if (nrow(dat) > 0) {

      dat <- dat[ dat$channel %in% ix.ch, ]

    }

    #--------------------------------------------------------#

    #-- STEP 2: Remove all spikes outside the Time_Window  --#

    #--------------------------------------------------------#

    start_TW <- guideline$TimeLimitStart[i]
    end_TW <- guideline$TimeLimitEnd[i]
    true_end <- min(end_TW, record_duration)

    if (nrow(dat) > 0) {

      dat <- dat[ dat$within_session_time_ms/1000 >= start_TW & dat$within_session_time_ms/1000 <= true_end, ]

    }

    #---------------------------------------------------------------#

    #-- STEP 3: Remove all spikes within time exclusion intervals --#

    #---------------------------------------------------------------#

    TeX <- 0 # By default if not exclusion time
    ix.ti <- NA

    if (!(is.na(guideline[i, "TimeToExclude"]))) {

      ix.ti <- unlist(strsplit( guideline[i, "TimeToExclude"], split = ","))
      ix.ti <- sapply(1:length(ix.ti), function(j) gsub(" ", "", ix.ti[j]))

      ix.ti <- MEASpikeR:::rm_overlapping_int(ix.ti, start_TW, true_end)$ls.int
      TeX <- MEASpikeR:::rm_overlapping_int(ix.ti, start_TW, true_end)$TeX

      for (k in 1:length(ix.ti)) {
        ix <- as.numeric(unlist(strsplit(ix.ti[k], split = "-")))
        if (nrow(dat) > 0) {
          dat <- dat[ dat$within_session_time_ms/1000 < ix[1] | dat$within_session_time_ms/1000 > ix[2], ]
        }
      }
    }

    #--------------------------------------------------------------------------------------#

    #-- STEP 4: Remove channels with a firing rate below the Minimum Firing Rate (minFR) --#

    #--------------------------------------------------------------------------------------#

    time.analyzed <- true_end - start_TW - TeX

    if (nrow(dat) > 0) {

      rm.ch.MinFR <- names(table(dat$channel))[ table(dat$channel)/time.analyzed*60 < MinFR ]
      dat <- dat[ !(dat$channel %in% rm.ch.MinFR), ]

    }

    #-----------------------------------#

    #-- STEP 5: Spike sorting if TRUE --#

    #-----------------------------------#

    if (nrow(dat) > 0 & spike.sorting == TRUE) { # BEGIN_IF_SPIKE_SORTING

      ix <- names(table(dat$channel))[ table(dat$channel) > min.spike ]
      ix <- sort(as.numeric(ix))

      #-- To be repeated for each active electrode of the current condition i --#

      if (length(ix) > 0) {

        for (j in ix) { # begin_for_channel

          cat(j, "\n")

          #-- 1/ Extract 4 principal components from PCA --#

          tmp <- dat[ dat$channel == j, 10:80]
          outpca <- stats::prcomp(tmp, scale = TRUE)
          pc <- outpca$x[,1:4]

          #-- 2/ Find optimal number of clusters --#

          set.seed(1234)
          nc <- NbClust::NbClust(pc, min.nc = 2, max.nc = 5, method = "kmeans",
                        index = tolower(validation.index))
          nc <- nc$Best.nc
          nc <- ifelse(nc[2] > cutoff.index, nc[1], 1)

          #-- 3/ Perform KM with optimal number of clusters if nc > 1 --#

          if (nc > 1) {

            # Perform k-means with optimal number of clusters
            # and replace "none" values in cluster_id

            res.km <- stats::kmeans(pc, centers = nc, nstart = 25)

            dat[ dat$channel == j, ]$cluster_id <- factor(as.numeric(res.km$cluster))

          }

        } # end_for_channel

      } # if_nb_channel_greater_than_1

      #-- 4/ Remove clusters with firing rate below the Minimum Firing Rate (minFR) --#

      if (nrow(dat) > 0) {

        ch.cluster <- factor(paste(dat$channel, dat$cluster_id, sep = "_"))

        rm.ch.clus.MinFR <- names(table(ch.cluster))[ table(ch.cluster)/time.analyzed*60 < MinFR ]
        dat <- dat[ !(ch.cluster %in% rm.ch.clus.MinFR), ]

      }

    } # END_IF_SPIKE_SORTING

    #---------------------------------------------------------#

    #-- Output generation: "filtered.dataspike" + info.cond --#

    #-- Updated at each loop iteration (condition)          --#

    #---------------------------------------------------------#

    assign(paste0("filtered.dataspike", guideline$ConditionNumber[i]), dat)

    info.cond <- rbind( info.cond,
                        data.frame(
                          Filename = paste0("filtered.dataspike", guideline$ConditionNumber[i]),
                          ShortFilename = shortname,
                          ConditionNumber = guideline$ConditionNumber[i],
                          empty.file = empty.file,
                          record_duration = record_duration,
                          start_TW = start_TW,
                          end_TW = end_TW,
                          TW = paste(start_TW, end_TW, sep = "-"),
                          true_end = true_end,
                          intTeX = paste(ix.ti, collapse = ","),
                          TeX = TeX,
                          MinFR = MinFR,
                          time.analyzed = time.analyzed,
                          nb.ch.excluded = nb.excluded,
                          spike.sorting = spike.sorting,
                          index = validation.index,
                          cutoff = cutoff.index
                        ) )

    info.cond[ info.cond == "NA" ] <- NA

  } # End_For_Condition

  output <- list(
    "guideline" = guideline,
    "nb.condition" = nb.condition, "nb.electrode" = nb.electrode,
    "MinFR" = MinFR, "info.cond" = info.cond
  )
  fd <- lapply(ls()[ grep("filtered.dataspike", ls()) ], function(i) get(i))
  names(fd) <- ls()[ grep("filtered.dataspike", ls()) ]
  output <- c(output, fd)

  class(output) <- "MEASpikeR"

  if (save.rDATA == TRUE) save(output,
                               file = paste(data.path, paste0(filename,".Rdat"), sep = "/"))

  return(output)

  # if (save.rDATA == TRUE) save(output,
  #                               file = paste(data.path, paste0(filename,".Rdat"), sep = "/"))

} # END FUNCTION MEA_1_DATA_FILTERING


