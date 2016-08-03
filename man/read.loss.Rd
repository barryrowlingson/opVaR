% --- Source file: read.loss.Rd ---
\name{read.loss}
\alias{read.loss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Loss reading }
\description{
  Function reading losses (dates and amounts) from object of type \code{\link{loss.data.object}} for given risk category and business lines.
}
\usage{
read.loss(b, r, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{b}{ business line number }
  \item{r}{ risk category number }
  \item{data}{ object of type \code{\link{loss.data.object}} }
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
   List with two columnes, first with dates of events and second with loss amount. 
%  ~Describe the value returned
%  If it is a LIST, use
%  \item{comp1 }{Description of 'comp1'}
%  \item{comp2 }{Description of 'comp2'}
%  ...
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{is.element}}, \code{\link{loss.data.object}} }
\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%
data(loss.data.object)

loss.data.object$blines[1] # business line "Agency Services"
loss.data.object$rcateg[2] # risk category "Clients, Products & Business Practices"
x<- read.loss(1,2,loss.data.object) # reads losses (dates and amounts)
head(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
