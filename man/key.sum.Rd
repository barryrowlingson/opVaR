% --- Source file: key.sum.Rd ---
\name{key.sum}
\alias{key.sum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function summing by a given key }
\description{
  Sums elements of a given vector by a given key.
}
\usage{
key.sum(v, u)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{ vector to be summed }
  \item{u}{ vector of non-negative integers indicating how values of \code{v} should be summed }
}
% \details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
 A vector obtained by summing values of \code{v} by the key \code{u}.
%  If it is a LIST, use
%  \item{comp1 }{Description of 'comp1'}
%  \item{comp2 }{Description of 'comp2'}
%  ...
}
% \references{ ~put references to the literature/web site here ~ }
% \author{ ~~who you are~~ }
\note{ It is intended that: length(v) != sum(u) but otherwise function computes its value vector as well.

% ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ function \code{key.sum} is part of function \code{\link{period.loss}} }
\examples{
% ##---- Should be DIRECTLY executable !! ----
% ##-- ==>  Define data, use random,
% ##--	or do  help(data=index)  for the standard data sets.
%
# the first example:

v <- c(0,1,2,3.4,2)
u <- c(2,2,0,1)

key.sum(v,u)

# the second example:

v1 <- c(0,1,2,3.4,2)
u1 <- c(2,2,0)

key.sum(v1,u1) # of course there is not enough u data to have v1[5] accounted in the value vector

# the third example:

v2 <- c(0,1,2,3.4)
u2 <- c(2,2,0,1)

key.sum(v2,u2) # of course there is not enough v2 data to sum by u2

%## The function is currently defined as
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
