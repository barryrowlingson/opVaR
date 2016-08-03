% --- Source file: opVaR-package.Rd ---
\name{opVaR-package}
\alias{opVaR-package}
\alias{opVaR}
\docType{package}
\title{
Computing operational risk - opVaR package
%~~ package title ~~
}
\description{
opVaR package computes operational risk by Monte Carlo simulation performed by \code{\link{mc}} function.
 It provides reading losses, merges them by chosen periods, fits frequency and severity distributions.
 Finally, VaR is computed basing on simulated yearly losses.   
%More about what it does (maybe more than one line)
%~~ A concise (1-5 lines) description of the package ~~
}
\details{
\tabular{ll}{
Package: \tab opVaR\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2010-05-30\cr
License: \tab GPL-3\cr
LazyLoad: \tab yes\cr
}
%~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Anna Patrycja Zalewska, Faculty of Mathematics, Informatics and Mechanics at the University of Warsaw. 

Maintainer: Anna Patrycja Zalewska <anna.patrycja.zalewska@gmail.com>
% Who to complain to <yourfault@somewhere.net>
%~~ The author and/or maintainer of the package ~~
}
%\references{
%~~ Literature or other references for background information ~~
%}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R ~~
~~ documentation directory ~~
\keyword{ package }
%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}
\examples{
%~~ simple examples of the most important functions ~~
data(loss.data.object)

D<- loss.matrix(loss.data.object);D
	# matrix of loss summaries

loss.matrix.image(data=loss.data.object)
	# table of loss summaries

loss.density(a=1,b=2,loss.data.object)
	# loss denisties for every business line
	# and second risk category
	# "Clients, Products and Business Practices"

x<- read.loss(3,2,loss.data.object)
	# read losses from third business line
	# and second risk category

hist.period(x,"days")
	# frequency histogram for days
root.period(x,"days","nbinomial")
	# fitted frequency - nbinomial

z<- x[,2] # these are losses amounts
par(mfrow = c(1,2))
fit.plot(z,dnorm, param = list(mean = mean(z),sd = sd(z)))
fit.plot(z,dnorm, param = list(mean = mean(z),sd = sd(z)),draw.diff=T)
	# empirical and fitted severity distributions

loss.fit.dist("lognormal",x)
	# fitting lognormal distribution 
	
mc(x,rfun="log-normal",nmb=200)
	# 200 yearly losses simulated for x
	# with given lognormal distribution as severity distribution
	# and chosen binomial distribution as frequency distribution

}
