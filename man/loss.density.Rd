% --- Source file: loss.density.Rd ---
\name{loss.density}
\alias{loss.density}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plots loss density }
\description{
  A function plotting loss density for a chosen business line or risk category and for all risk categories or business lines accordingly.
  Number of losses is printed in the plot \code{main}.
}
\usage{
loss.density(a, b, data, no = FALSE, rnumb = c(), bnumb = c(),
            period = c("none", "days", "weeks", "months", "quarters"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{ If \code{a}=1 then function plots density for all business lines and chosen risk category (\code{b}), and if \code{a}=2 then function plots density for all risk categories and chosen business line (\code{b})}
  \item{b}{ chosen position from business line or risk category }
  \item{data}{ should be of \code{\link{loss.data.object}} type }
  \item{no}{ whether to draw antyhing when there is no data, or not (if \code{FALSE} empty plot is drawn when there is no data for given business line and risk category) }
  \item{rnumb}{ risk categories numbers; argument designed for chosing some of risk categories }
  \item{bnumb}{ business lines numbers; argument designed for chosing some of business lines }
  \item{period}{ could be \code{"none"} (losses are not aggregated), \code{"days"}, \code{"weeks"}, \code{"months"} or \code{"quarters"} (in last four cases loss are aggregated by days, weeks, months or quarters respectively ) }
  \item{\dots}{ Arguments to be passed to \code{\link{density}}   }
}
% \details{
%   ~~ If necessary, more details than the description above ~~
% }
% \value{
%   ~Describe the value returned
%   If it is a LIST, use
%  \item{comp1 }{Description of 'comp1'}
%  \item{comp2 }{Description of 'comp2'}
%  ...
%}
% \references{ ~put references to the literature/web site here ~ }
% \author{ ~~who you are~~ }
 \note{  \code{a} =1 (business) or \code{a} =2 (risk), so when \code{a}=1, it is useless to give any \code{rnumb} and when \code{a}=2, it makes no sense to use \code{bnumb}. 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ \code{\link{density}} }
\examples{
% ##---- Should be DIRECTLY executable !! ----
% ##-- ==>  Define data, use random,
% ##--	or do  help(data=index)  for the standard data sets.
%
% ## The function is currently defined as
%
data(loss.data.object)

# first example:
 
 loss.density(1,7,loss.data.object) # loss density from all business lines 
 #(loss.data.object$blines) and risk category Internal Fraud
 
# second example:
 
 loss.density(1,7,loss.data.object,no=T) # second example, but with no empty plots

# third example:

 loss.density(1,1,loss.data.object) # of course it is empty (no loss data)
 # see
 D <- loss.matrix(loss.data.object); par(mfrow = c(1,1))
 loss.matrix.image(D,loss.data.object$blines,loss.data.object$rcateg)

# fourth example:

 loss.density(1,7,loss.data.object,period="quarters") # loss are aggreagated by quarters

# fifth example:

 loss.density(1,7, loss.data.object,bnumb=c(1,2,3,5,6,7)) # in this example business lines are
									# loss.data.object$blines[c(1,2,3,5,6,7)]

# sixth example:

 loss.density(1,5,loss.data.object,bnumb=c(5)) # risk category is loss.data.object$rlines[5]
   # and because bnumb=c(5), there is only one business line, loss.data.object$blines[5] 

# seventh example:

 loss.density(1,5,loss.data.object,kernel="triangular") 

# eighth example:

 # (with some mistake)
 loss.density(1,5,loss.data.object,bnumb=c(5),rnumb=c(11))
# of course rnumb=c(11) makes no sense, but it does not affect this function, because there is already
# one chosen position from risk categories and it is 5, not 11 so 11 is simply ignored
# (and note that we have no 11th risk category)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
