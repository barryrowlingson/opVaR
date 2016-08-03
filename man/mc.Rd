% --- Source file: mc.Rd ---
\name{mc}
\alias{mc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Monte Carlo loss simulation }
\description{
  Performs Monte Carlo loss simulation for a given number of given periods and some fitted distributions for loss frequency and denstiy.
}
\usage{
mc(x, rfun = NULL, period, iterate, nmb = 1000, begin = NULL, end = NULL,
   wknd = TRUE, crt = 0, type = NULL, param = NULL, zero = F, distname = NULL, fit=T,
   flist = c("beta", "cauchy", "chi-squared", "exponential","f", "gamma", "geometric", 
   "log-normal", "logistic", "normal", "weibull", "inverse gaussian"), 
% width = 1, space = 0.2,
   p = c(0.95, 0.99, 0.999), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ list with two columnes, first with dates of events and second with loss amount }
  \item{rfun}{ random generation for the chosen distribution; \code{NULL} default }
  \item{period}{ could be \code{"days"}, \code{"weeks"}, \code{"months"} or \code{"quarters"} (loss are aggregated by days, weeks, months or quarters respectively) }
  \item{iterate}{ could be \code{"years"}, \code{"quarters"}, \code{"months"} or \code{"weeks"} }
  \item{nmb}{ number of period iterations }
  \item{begin}{ period begin date; if not given, it would be minimum from loss dates}
  \item{end}{ period end date; if not given, it would be maximum from loss dates}
  \item{wknd}{ whether to have 252 days a year (\code{FALSE}) or not (\code{TRUE}, then 365), designed only for \code{period} = \code{"days"} }
  \item{crt}{ correction; argument passed to \code{\link{root.period}}  }
  \item{type}{ could be \code{"poisson"}, \code{"binomial"}, \code{"nbinomial"}, if not given, it would be chosen (fitted) automatically }
  \item{param}{ start parameters (only for \code{rfun} not being character, see examples) }
  \item{zero}{ should zero losses be drawn (\code{TRUE}) or not (\code{FALSE})? }
  \item{distname}{ distribution name; if \code{rfun}= rbeta, it should be \code{distname} = "beta" (it is designed for that case) }
  \item{fit}{logical: fit \code{rfun} parameters?}
  \item{flist}{ list of distributions \code{"beta"}, \code{"cauchy"}, \code{"chi-squared"}, \code{"exponential"}, \code{"gamma"}, \code{"geometric"}, \code{"log-normal"}, \code{"logistic"}, \code{"negative binomial"}, \code{"normal"}, \code{"poisson"}, \code{"weibull"}, \code{"inverse gaussian"} }
 % \item{width}{ optional vector of bar widths; argument passed to \code{\link{barplot}} (default 1) }
 % \item{space}{ the amount of space left before each bar; argument passed to \code{\link{barplot}} (default 0.2) }
  \item{p}{ confidence level (default c(0.95, 0.99, 0.999)) }
 % \item{p1}{ text position on plot (x coordinate)}
 % \item{p2}{ text position on plot (y coordinate)}
 % \item{col}{ bars colour (default \code{grey})}
 % \item(ecdf){whether to drawn \code{\link{ecdf}} (empirical cumulative distribution function) plot or not (\code{TRUE} default)} 
 % \item{density}{whether to drawn \code{\link{denisty}} plot or not (\code{FALSE} default)}
  \item{\dots}{ arguments passed to \code{\link{root.period}} }
}
%\details{
%  ~~ If necessary, more details than the description above ~~
% }
\value{
%  ~Describe the value returned
%  If it is a LIST, use
  \item{table$losses }{generated losses}
  \item{table$q }{\code{losses} \code{p}-quantile }
  \item{ad}{absolute differences between empirical and fitted density values}
%  ...
}
% \references{ ~put references to the literature/web site here ~ }
% \author{ ~~who you are~~ }
\note{ "log-normal" and "lognormal" are not the same anymore; only "log-normal" would work
%
% ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ \code{\link{root.period}}, \code{\link{loss.fit.dist}}, \code{\link{fitdistr}} }
\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%
%## The function is currently defined as
data(loss.data.object)
%# x<- read.loss(1,2,loss.data.object) # business line Agency Services, risk category Clients, Products and Business Practices

x<- read.loss(3,2,loss.data.object)

# first example:

 l1 = mc(x,begin="2010-01-01",end="2010-12-31")$table
 l1

# second example:

 l2 = mc(x,rfun = "inverse gaussian",nmb =100)$table
 l2 # yearly losses for 100 years, 365 days per year

# third example:

 l3 = mc(x,rfun = "beta", p=c(0.95))$table

# fourth example:

 l4 = mc(x,rfun = "beta",type = "binomial")$table # type of frequency distribution is chosen

# fifth example:

 l5 = mc(x,rfun = rbeta,param = list(shape1 = 0.47,shape2 = 36.66),period = "days",distname = "beta")$table
# parameters for beta distribution are given

# sixth example:

 l6 = mc(x,rfun = "normal") # comapare loss.fit.dist("normal",x) - fit is very poor

# seventh example:

 l7 = mc(x,rfun = rnorm,type = "binomial",param = list(mean=3,sd=4))$table
# parameters for normal distribution are given (and very poor, comparing loss.fit.dist("normal",x) or sixth example )

# eighth example:

 l8 = mc(x,flist= c("normal","inverse gaussian"))$table

%# ninth example:
%
% l9 = mc(x,rfun = "beta",type = "binomial",ecdf = FALSE,density = TRUE, p = c(0.99,0.999))$table
% # compare with ecdf plot:
% losses<- l9$losses
% p<- c(0.99,0.999)
% q<- quantile(losses,p)
% plot(ecdf(losses),main=c("ecdf(losses)"))
% for(i in 1:length(q)){
% lines(c(q[i],q[i]),c(0,1.5),col="red")
% text(q[i],0.05,cex=0.9,paste(round(q[i])),pos=2)
% text(q[i],0.1,cex=0.9,paste(p[i]),pos=2)
% }
% # note that running mc(x,rfun = "beta",type = "binomial",p = c(0.99,0.999)) to compare
% # obtained density plot with ecdf plot is not a good idea because it would be drawn
% # for two different loss simulations
%
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
