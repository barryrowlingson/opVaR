% --- Source file: loss.matrix.Rd ---
\name{loss.matrix}
\alias{loss.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Creates loss matrix }
\description{
  Function creating a loss matrix for a given loss data, risk categories and business lines.
}
\usage{
loss.matrix(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ argument must be of \code{\link{loss.data.object}} type }
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
Returns matrix of losses in business lines and risk categories. For returned matrix D, i being number of business line and j number of risk category respectively, D[i,1,j] is maximum loss, D[i,2,j] mean loss, D[i,3,j] minimum loss, D[i,4,j] number of losses.
%  ~Describe the value returned
%  If it is a LIST, use
%  \item{comp1 }{Description of 'comp1'}
%  \item{comp2 }{Description of 'comp2'}
%  ...
}
% \references{ ~put references to the literature/web site here ~ }
% \author{ ~~who you are~~ }
% \note{ ~~further notes~~ 

% ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{ \code{\link{read.loss}}, \code{\link{loss.data.object}}, \code{\link{loss.matrix.image}} }
\examples{
% ##---- Should be DIRECTLY executable !! ----
% ##-- ==>  Define data, use random,
% ##--	or do  help(data=index)  for the standard data sets.
%
% ## The function is currently defined as
data(loss.data.object)
D <- loss.matrix(loss.data.object)
D # see also loss.matrix.image

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
