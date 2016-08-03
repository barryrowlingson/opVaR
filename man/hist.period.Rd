% --- Source file: hist.period.Rd ---
\name{hist.period}
\alias{hist.period}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Loss frequency histograms  }
\description{
  Function plotting histograms of frequency of losses.
}
\usage{
hist.period(data, period, wknd = TRUE, crt = 0, begin=NULL,end=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ two-dimensional data object; the first column of \code{data} contains loss dates, the second - loss values }
  \item{period}{ describes how data should be agregated; possible \code{period} values are: \code{"days"},\code{"weeks"},\code{"months"} and \code{"quarters"} }
  \item{wknd}{ a logical value indicating whether weekend days (\code{wknd}) count as days being bussiness days; \code{wknd} is designed only for \code{period} = \code{"days"} }
  \item{crt}{ correction indicates how many days should be excluded from the time horizon; it is designed for holidays and only for \code{period} = \code{"days"} }
  \item{begin}{ period begin date; if not given, it would be minimum from loss dates}
  \item{end}{ period end date; if not given, it would be maximum from loss dates}
  \item{\dots}{ further arguments passed to \code{\link{barplot}} (graphical parameters) }
}
% \details{
%  ~~ If necessary, more details than the description above ~~
% }
\value{
  Table \code{y} indicating how many days/weeks/months/quarters (depending on \code{period}) there was with 0,1,2,... losses. 
%  ~Describe the value returned
%  If it is a LIST, use
%  \item{y}{table indicating how many days/weeks/months/quarters (depending on \code{period}) there was with 0,1,2,... losses }
%  \item{comp2 }{Description of 'comp2'}
%  ...
}
% \references{ ~put references to the literature/web site here ~ }
% \author{ ~~who you are~~ }
% \note{ ~~further notes~~ 
% ~Make other sections like Warning with \section{Warning }{....} ~
% }
 \seealso{  \code{\link{barplot}}, \code{\link{root.period}} }
\examples{
% ##---- Should be DIRECTLY executable !! ----
% ##-- ==>  Define data, use random,
% ##--	or do  help(data=index)  for the standard data sets.
% ## The function is currently defined as...
# data comes from risk category called "Execution, Delivery & Process Management"
# and from business line called "Payment & Settlement"

# the first example:

 data(loss.data.object)
 x<- read.loss(5,5,loss.data.object)

 hist.period(x,"weeks")

# the second example:

 data(loss.data.object)
 x<- read.loss(5,5,loss.data.object)
 z<- {}		

 par(mfrow=c(2,2))
 z$days <- hist.period(x,"days",col = "pink1")
 z$weeks <- hist.period(x,"weeks",col = "lightblue")
 z$months <- hist.period(x,"months",col = "khaki1" )
 z$quarters <- hist.period(x,"quarters",col = "lightgreen")

z

# the third example:

 data(loss.data.object)
 x<- read.loss(5,5,loss.data.object)
 y<- {}		

 # begin and end dates would be given:
 par(mfrow=c(2,2))
 y$days <- hist.period(x,"days",col = "pink1",begin="2010-01-01",end="2010-12-31")
 y$weeks <- hist.period(x,"weeks",col = "lightblue",begin="2010-01-01",end="2010-12-31")
 y$months <- hist.period(x,"months",col = "khaki1",begin="2010-01-01",end="2010-12-31")
 y$quarters <- hist.period(x,"quarters",col = "lightgreen",begin="2010-01-01",end="2010-12-31")

y	# compare with z from the previous example

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
