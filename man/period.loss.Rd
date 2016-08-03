% --- Source file: period.loss.Rd ---
\name{period.loss}
\alias{period.loss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Aggregating losses by period }
\description{
  Function aggregating losses by a given period.
}
\usage{
period.loss(data, period = c("none", "days", "weeks", "months", "quarters"),dts = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ list with two columnes, first with dates of events and second with loss amount }
  \item{period}{ could be \code{none} (losses are not aggregated) or \code{"days"}, \code{"weeks"}, \code{"months"} or \code{"quarters"} (loss are aggregated by days, weeks, months or quarters respectively) }
  \item{dts}{ logical: whether to have period begins' dates or not (\code{FALSE} default)}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
   List of aggreagated losses.
%  ~Describe the value returned
%  If it is a LIST, use
%  \item{comp1 }{Description of 'comp1'}
%  \item{comp2 }{Description of 'comp2'}
%  ...
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
%\note{ ~~further notes~~ 

% ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{ \code{\link{key.sum}}, \code{\link{weekdays}}, \code{\link{as.Date}} }
\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%
%## The function is currently defined as
data(loss.data.object)
x<- read.loss(1,2,loss.data.object)
 t0 <- period.loss(x,"none"); t0
 t1 <- period.loss(x,"days"); t1
 t2 <- period.loss(x,"weeks"); t2
 t3 <- period.loss(x,"months"); t3
 t4 <- period.loss(x,"quarters"); t4

sum(t0);sum(t1); sum(t2); sum(t3); sum(t4) # note that all these values must be equal

# the same with dates - note that only for days and no period there are original
# dates; for weeks, months and periods there are only dates opening period in which
# loss occured

 t0 <- period.loss(x,"none",dts=T); t0
 t1 <- period.loss(x,"days",dts=T); t1
 t2 <- period.loss(x,"weeks",dts=T); t2
 t3 <- period.loss(x,"months",dts=T); t3
 t4 <- period.loss(x,"quarters",dts=T); t4

sum(t0);sum(t1); sum(t2); sum(t3); sum(t4)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
