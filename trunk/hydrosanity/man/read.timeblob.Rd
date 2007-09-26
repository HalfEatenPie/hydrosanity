\name{read.timeblob}
\alias{read.timeblob}
\title{Read Timeblob from a Text File}
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.timeblob(file, skip = 1, sep = ",", sitename = NULL, dataname = "Data", 
              dataCol = 2, qualCol = 3, extraCols = c(), 
              extraNames = paste("Extra", extraCols), readTimesFromFile = T, 
              timeCol = 1, timeFormat = "\%d \%b \%Y", startTime = NA, 
              tz = "GMT", timeSeqBy = "days", timeOffset = NULL, ...)
}
\arguments{
  \item{file}{ path to a file, or a connection. }
  \item{skip}{ number of lines in file before data begins. }
  \item{sep}{ column separator, see \code{\link{read.table}}. }
  \item{sitename}{ ~~Describe \code{sitename} here~~ }
  \item{dataname}{ ~~Describe \code{dataname} here~~ }
  \item{dataCol}{ file column number (starting from 1) containing data. }
  \item{qualCol}{ file column number (starting from 1) containing quality codes. }
  \item{extraCols}{ a vector of other file column numbers to read in. }
  \item{extraNames}{ names of the columns given above. }
  \item{readTimesFromFile}{ whether to read time series times from the file (column \code{timeCol}). }
  \item{timeCol}{ used when \code{readTimesFromFile = TRUE}. 
  It should give the file column number (starting from 1) containing times. }
  \item{timeFormat}{ format of times in \code{timeCol}: see \code{\link{strptime}}. }
  \item{startTime}{ used when \code{readTimesFromFile = FALSE}. 
  It should be (coercible to) a \code{\link{POSIXt}} object, or else be a list like
  \code{list(year=1,month=2,day=3)} (etc) where these give column numbers to read 
  the start time components (from the first line in file). }
  \item{tz}{ timezone, see \code{\link{POSIXt}}. }
  \item{timeSeqBy}{ time step to go with \code{startTime}, passed to \code{\link{seq.POSIXt}}. }
  \item{timeOffset}{ a \code{\link{difftime}} object to add to the times. }
  \item{\dots}{ passed to \code{\link{read.table}}. }
}
\details{
	
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
	
}
\keyword{ ts }
