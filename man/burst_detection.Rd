% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/burst_detection.R
\name{burst_detection}
\alias{burst_detection}
\title{Burst analysis using Chen or MI methods}
\usage{
burst_detection(
  object = NA,
  filt.data.path = NA,
  output_directory = NA,
  burst.detection.method = "MI",
  max_ISI_start_burst = 170,
  max_ISI_end_burst = 300,
  min_IBI = 50,
  min_spike_in_burst = 5,
  min_burst_duration = 50
)
}
\arguments{
\item{object}{Object of class MEASpikeR generated by the \code{dataset_filtering} function}

\item{filt.data.path}{Path to the ".Rdat" file generated
by the \code{dataset_filtering} function}

\item{output_directory}{Character specifying the output directory where we want to save the output excel file}

\item{burst.detection.method}{Character specifying the choice of the burst
detection method ("MI" or "chen") (by default "MI")}

\item{max_ISI_start_burst}{Number, parameter setting for burst detection (by default 170)}

\item{max_ISI_end_burst}{Number, parameter setting for burst detection (by default 300)}

\item{min_IBI}{Number, parameter setting for burst detection (by default 50)}

\item{min_spike_in_burst}{Number, parameter setting for burst detection (by default 5)}

\item{min_burst_duration}{Number, parameter setting for burst detection (by default 50)}
}
\value{
An excel file with burst analysis located in the output directory
specified in the \code{output_directory} argument
}
\description{
Burst analysis using Chen or MI methods
}
\examples{
\dontrun{
out.MEA <- MEASpikeR::dataset_filtering(data.path = "C:/Users/prenom.nom/Documents/data",
guideline_name_file = "MEA_Spikes_ANA_R_guideline",
save.rDATA = TRUE)

# if you want to work with the MEASpikeR object
MEASpikeR::burst_detection(
object = out.MEA,
output_directory = "C:/Users/prenom.nom/Documents"
)

# if you want to work with the saved Rdat file
MEASpikeR::burst_detection(
filt.data.path = "C:/Users/prenom.nom/Documents/data/MEA_DATAFILTERED.Rdat",
output_directory = "C:/Users/prenom.nom/Documents"
)
}
}
\references{
Dalle C., Martet G., Kaddouri Y., Rebola N., Whitmarsh S., Lejeune FX.(2023). \strong{\emph{MEASpikeR}: a new package
for spike analysis and visualization of \emph{in vitro} MEA data}.
}
\author{
François-Xavier Lejeune, Gaspard Martet, Carine Dalle
}
