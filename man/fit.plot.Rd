% --- Source file: fit.plot.Rd ---
\name{fit.plot}
\alias{fit.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plotting fitted and empirical distribution }
\description{
  A function to plot empirical and fitted density (with given parameters) and compute differences between values of that densities.
}
\usage{
fit.plot(x, densfun, param, distname = NULL, col = c("red"),col2 = c("grey"),col3 = c("blue"),
	 ylim = c(), xlim = c(), kernel = NULL, n = NULL, draw.diff = F, draw.max = F, 
	 scaled = F,positive=T, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a data vector }
  \item{densfun}{ density function }
  \item{param}{ density function parameters}
  \item{distname}{ density function name ( \code{optional}) }
  \item{col}{ fitted density colour (default \code{red}) }
  \item{col2}{draw.diff lines colour - see draw.max option}
  \item{col3}{draw.max line colour - see draw.max option}
  \item{ylim}{ \code{optional} }
  \item{xlim}{ \code{optional} }
  \item{kernel}{smoothing kernel to be used; see \code{\link{density}} }
  \item{n}{the number of equally spaced points at which the density is to be estimated; see \code{\link{density}} (if not given, default \code{density} n is used)}
  \item{draw.diff}{logical; draw differences between empirical and estimated density values?}
  \item{draw.max}{logical; draw maximal differences between empirical and estimated density values?}
  \item{scaled}{a logical value. If TRUE, values are scaled (\code{x<- x/max(x)}), then values computed and after that scaled once more; try it for \code{dbeta} distribution}
  \item{positive}{a logival value: compute differences only for positive x density arguments?}
  \item{\dots}{arguments passed to \code{\link{plot}}}
%
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
%  ~Describe the value returned
%  If it is a LIST, use
  \item{ad}{absolute differences between empirical and fitted density values}
  \item{teor}{fitted density values}
  \item{emp}{empirical density values}
  \item{maxdiff}{maximum difference between empirical and fitted density values}
  \item{meandiff}{mean difference between empirical and fitted density values}
% \item{comp2 }{Description of 'comp2'}
%  ...
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ see also \code{\link{loss.fit.dist}}, \code{\link{fitdistr}} and \code{\link{plot.default}} }
\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%
# first example:

data(loss.data.object)	# data reading
x<- read.loss(1,2,loss.data.object)	# choice of data
z<- x[,2]	# z is loss data

fit.plot(z,dnorm, param = list(mean = mean(z),sd = sd(z)))

 # and the same with draw.diff = T
fit.plot(z,dnorm, param = list(mean = mean(z),sd = sd(z)),draw.diff=T)

 # draw.diff = T, draw.max = T (maximum difference is drawn), 
 # n = 40 (density values are computed only for 40 points)
fit1<-fit.plot(z,dnorm, param = list(mean = mean(z),sd = sd(z)),draw.diff=T,n=40)
length(fit1$teor) # absolute differences are computed only for positive density arguments
length(fit1$emp)  # number of arguments
# then ad is computed as sum of that absolute differences
# compare:
sum(abs(fit1$teor - fit1$emp))
fit1$ad

 # compare:
par(mfrow = c(2,1))
fit.plot(z,dnorm, param = list(mean = mean(z),sd = sd(z)),draw.diff=T,n=40,col2 = "darkblue")
fit.plot(z,dnorm, param = list(mean = mean(z),sd = sd(z)),draw.diff=T,n=40,positive=F,col2 = "darkblue")
 # more values thanks to positive = F

# second example:

 # beta distribution is special because it is scaled
fit.plot(z,dbeta,"beta", param = list(shape1 = 0.4,shape2=17)) 
 # ... and with x logarithmic scale
fit.plot(z,dbeta,"beta", param = list(shape1 = 0.4,shape2=17),log = "x") 

# third example:

parameters <-loss.fit.dist("lognormal",x)$param # fits lognormal distribution to
								# second column of our data (i.e. z)		
parameters <- as.numeric(parameters)
parameters # the estimated parameters of lognormal distribution

fit.plot(z,dlnorm,distname = "lognormal",
	param = list(meanlog = parameters[1],sdlog = parameters[2]))

 # ... and with x logarithmic scale
fit.plot(z,dlnorm,distname = "lognormal",
	param = list(meanlog = parameters[1],sdlog = parameters[2]),log="x")
 # lognormal seems to fit that data

 # and with differences drawn (there was ylim changed to see these lines better)
fit.plot(z,dlnorm,distname = "lognormal",
 param = list(meanlog = parameters[1],sdlog = parameters[2]),log="x",draw.diff=T,ylim = c(0,4e-05))


%
% 
%## The function is currently defined as
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
