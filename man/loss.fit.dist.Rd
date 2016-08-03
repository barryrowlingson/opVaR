% --- Source file: loss.fit.dist.Rd ---
\name{loss.fit.dist}
\alias{loss.fit.dist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Fits density for loss density }
\description{
  Fits a chosen density to loss data and draws plot of empirical and fitted density. 
}
\usage{
loss.fit.dist(densfun, x, start = NULL, name = NULL, qq = FALSE, period, ylim = c(), xlim = c(),
	 col = "red", from = 0.1^15, to = 1 - 0.1^15, length.out = 10000, by = NULL, kernel = NULL,
	 n = NULL, draw.diff = F, draw.max = F,xlog.scale = FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{densfun}{ a character string or a function returning a density evaluated at its first argument. 
Distributions \code{"beta"}, \code{"cauchy"}, \code{"chi-squared"}, \code{"exponential"}, \code{"f"}, \code{"gamma"}, \code{"geometric"}, \code{"log-normal"} (or \code{"lognormal"}), \code{"logistic"}, \code{"normal"}, \code{"weibull"} and \code{"inverse gaussian"} are recognised, case being ignored. }
% \code{"t"} wyciety
  \item{x}{ an object with two columns (losses in second column) or vector of losses }
  \item{start}{ a named list giving the parameters to be optimized with initial values (it can be omitted for the named distributions and must be for others) }
  \item{name}{ if densfun is not a named function, \code{name} will be in the main of the plot drawn }
  \item{qq}{ if \code{TRUE}, a \code{qqplot} is drawn }
  \item{period}{ could be \code{"none"} (losses are not aggregated), \code{"days"}, \code{"weeks"}, \code{"months"} or \code{"quarters"} (in last four cases loss are aggregated by days, weeks, months or quarters respectively); if missing, \code{"none"} }
  \item{ylim}{ \code{optional} }
  \item{xlim}{ \code{optional} }
  \item{col}{ fitted density colour (default \code{red}) }
  \item{from}{start value of \code{p} values in numeric vector of probabilities given to compute empirical and theoretical quantiles ; values in \code{[0,1]}, \code{0.1^15} default; \code{p = seq(from, to, length.out, by)}}
  \item{to}{end value of \code{p} values in numeric vector of probabilities given to compute empirical and theoretical quantiles; values in \code{[0,1]}, \code{1 - 0.1^15} default; \code{p = seq(from, to, length.out, by)}}
  \item{length.out}{desired length of the sequence p, \code{10000} default; \code{p = seq(from, to, length.out, by)}}
  \item{by}{step in numeric vector of probabilities \code{p = seq(from, to, length.out, by)}; \code{NULL} default}
  \item{kernel}{smoothing kernel to be used; see \code{\link{density}}}
  \item{n}{the number of equally spaced points at which the density is to be estimated; see \code{\link{density}} (if not given, default \code{density} n is used)}
  \item{draw.diff}{logical; draw differences between empirical and estimated density values?}
  \item{draw.max}{logical; draw maximal differences between empirical and estimated density values?}
  \item{xlog.scale}{a logical value (see \code{log} in \code{\link{plot.default}}). If TRUE, a \code{x} logarithmic scale is in use}
  \item{\dots}{arguments passed to \code{\link{fitdistr}} (in fact, to \code{\link{optim}})}
%
% czy brac cos z innych metod? czy zachowac sd z fitdistr?
%
}
%\details{
%  ~~ If necessary, more details than the description above ~~
% }
\value{
%  ~Describe the value returned
%  If it is a LIST, use
  \item{loglik }{ the log-likelihood (from \code{\link{fitdistr}})}
  \item{param }{ fitted parameters (from \code{\link{fitdistr}})}
  \item{sd}{ estimated standard errors (from \code{\link{fitdistr}})}
  \item{q.e}{ vector of \code{x} emprical quantiles}
  \item{q.t}{ vector of theoretical quantiles from fitted distribution}
  \item{ad}{absolute differences between empirical and fitted density values}
  \item{teor.dens}{fitted density values}
  \item{emp.dens}{empirical density values}
  \item{maxdiff}{maximum difference between empirical and fitted density values}
  \item{meandiff}{mean difference between empirical and fitted density values}
%  ...
}
% \references{ ~put references to the literature/web site here ~ }
% \author{ ~~who you are~~ }
% \note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{ \code{\link{fitdistr}}, \code{\link{optim}}, \code{\link{qqplot}}, \code{\link{period.loss}} }
\examples{
% ##---- Should be DIRECTLY executable !! ----
% ##-- ==>  Define data, use random,
% ##--	or do  help(data=index)  for the standard data sets.
% 
data(loss.data.object)
x<- read.loss(5,5,loss.data.object)

# first example:

mx<-x[,2]

%par(mfrow=c(2,1))
par(mfrow=c(2,2))
loss.fit.dist("gamma",x)
loss.fit.dist("gamma",mx,col="blue") # no difference between mx and x
loss.fit.dist("gamma",mx,col = "darkgreen",qq=T)

# second example:

% loss.fit.dist("inverse gaussian",x)
par(mfrow=c(2,1))
a = loss.fit.dist("inverse gaussian",col = "blue",x,qq=T)# there are emprical and theoretical quantiles

# third example: 

loss.fit.dist("exponential",x)

#  fourth example:

st<- list(shape=0.7882,scale = 11533) 
par(mfrow=c(2,1))
loss.fit.dist("weibull",x,start = st) # fitting weibull distribution with given start
loss.fit.dist("weibull",x) # and without start

# fifth example:

par(mfrow=c(2,1))
sta<-list(sd = 3,mean = 0.5)
a1 = loss.fit.dist(dnorm,x,start = sta,name = "normal!") # of course "normal" is recognised distribution
# but supplying pars for the normal distribution is not supported!
# so it can be only that way
a2 = loss.fit.dist(dnorm,x,start = sta,name = "normal!",col = "blue")
# method is optim argument
# compare parameters
a1$param
a2$param 

# sixth example:

sta<-list(sd = 3,mean = 0.5)
loss.fit.dist(dnorm,x,start = sta,name = "normal!") 
a = loss.fit.dist(dnorm,x,start = sta,name = "normal!",qq = TRUE);summary(a)
head(cbind(a$q.e,a$q.t))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
