% --- Source file: loss.matrix.image.Rd ---
\name{loss.matrix.image}
\alias{loss.matrix.image}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Draws loss matrix image }
\description{
  Function drawning table of losses for a given loss matrix, risk categories and business lines.
}
\usage{
loss.matrix.image(D, businesslines, riskcategories, data = NULL,
	col1 = c("lavender"), col2 = c("rosybrown 1"),
	col3 = c("lightpink"), col4 = c("mistyrose"), col5 = c("mintcream"),
	cex1 = 0.6, cex2 = 0.6, cex3 = 0.6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{D}{ matrix of losses, it should be of \code{\link{loss.matrix}} value type }
  \item{businesslines}{ vector of business lines names }
  \item{riskcategories}{ vector of risk categories names }
  \item{data}{object of \code{\link{loss.data.object}} type; it can be used instead of three previous arguments}
  \item{col1}{ colour of rectangles with no data }
  \item{col2}{ colour for rectangles with mean loss more than or equal to mean loss for \code{D} and less than mean loss for \code{D} + standard deviation of mean loss for \code{D}}
  \item{col3}{ colour for rectangles with mean loss more than or equal to mean loss for \code{D} + standard deviation of mean loss for \code{D}}
  \item{col4}{ colour for rectangles with mean loss more than 0 and less than mean loss for \code{D} }
  \item{col5}{ legend colour}
  \item{cex1}{ the final character size of names of risk categories and business lines}
  \item{cex2}{the final character size of number of losses, mean loss and maximum loss}
  \item{cex3}{the final character size of legend letters}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%  ~Describe the value returned
%  If it is a LIST, use
%  \item{comp1 }{Description of 'comp1'}
%  \item{comp2 }{Description of 'comp2'}
%  ...
%}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{ \code{\link{loss.matrix}}, \code{\link{loss.data.object}}, \code{\link{read.loss}} }
\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%
% ## The function is currently defined as
data(loss.data.object)
D <- loss.matrix(loss.data.object)

# first example:

loss.matrix.image(D,loss.data.object$blines,loss.data.object$rcateg)

# second example:

loss.matrix.image(D,loss.data.object$blines,loss.data.object$rcateg,col1="ghostwhite") # one colour is changed

# third example:

 # not every business and risk line is chosen
loss.matrix.image(D[c(2,8),,c(1,3,5)],loss.data.object$blines[c(2,8)],loss.data.object$rcateg[c(1,3,5)])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
