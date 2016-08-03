% --- Source file: root.period.Rd ---
\name{root.period}
\alias{root.period}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Fitting loss frequency distribution }
\description{
  Function fitting frequency distribution of losses aggregated by a given period and drawning rootogram.
}
\usage{
root.period(data, period, type, begin = NULL, end = NULL, method = c("ML", "MinChisq"),
 	 scale = c("sqrt", "raw"), wknd = TRUE, crt = 0, bar_type = c("hanging", "standing", "deviation"),
       rect_gp = gpar(fill = "lightgray"), lines_gp = gpar(col = "red"), points_gp = gpar(col = "red"), 
       pch = 19, newpage = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ list with two columnes, first with dates of events and second with loss amount }
  \item{period}{ describes how data should be agregated; possible \code{period} values are: \code{"days"},\code{"weeks"},\code{"months"} and \code{"quarters"} }
  \item{type}{ distribution to be fitted; could be \code{"poisson"}, \code{"binomial"}, \code{"nbinomial"} }
  \item{begin}{ period begin date; if not given, it would be minimum from loss dates}
  \item{end}{ period end date; if not given, it would be maximum from loss dates}
  \item{method}{ method to be used; could be \code{"ML"}, \code{"MinChisq"}; see \code{\link{goodfit}} }
  \item{scale}{ a raw or square root scale; see \code{\link{rootogram}} }
  \item{wknd}{ a logical value indicating whether weekend days (\code{wknd}) count as days being bussiness days; \code{wknd} is designed only for \code{period} = \code{"days"} }
  \item{crt}{ correction indicates how many days should be excluded from the time horizon; it is designed for holidays and only for \code{period} = \code{"days"} }
  \item{bar_type}{ should the bars  be hanging or standing or indicate the deviation between observed and fitted frequencies; see \code{type} in \code{\link{rootogram}} }
  \item{rect_gp}{ graphical parameters of the rectangles; see \code{\link{rootogram}} }
  \item{lines_gp}{ graphical parameters of the lines; see \code{\link{rootogram}} }
  \item{points_gp}{ graphical parameters of the points; see \code{\link{rootogram}} }
  \item{pch}{ plotting character for the points; see \code{\link{rootogram}} }
%  \item{newpage}{ logical. Should \code{\link{grid.newpage}} be called before plotting? see \code{\link{rootogram}}}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
%  ~Describe the value returned
%  If it is a LIST, use
  \item{table }{table of observed and fitted values}
  \item{param }{fitted parameters}
  \item{p }{ value \code{P(> X^2)} from summary of \code{\link{goodfit}} }
%  ...
}
% \references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{ \code{\link{rootogram}}, \code{\link{goodfit}}, \code{\link{weekdays}}, \code{\link{loss.data.object}}, \code{\link{hist.period}} }
\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%
%## The function is currently defined as
%
data(loss.data.object)
x<- read.loss(5,5,loss.data.object)

# first example:
y<- root.period(x,"days","poisson")
	t <- y$table; t
 	par <- y$param; par
	p <- y$p; p

# second example: 

 root.period(x,"days","nbinomial") # rather good fit
 root.period(x,"weeks","nbinomial")
 root.period(x,"months","nbinomial") 
 root.period(x,"quarters","nbinomial") # that does not fit nbinomial at all
 root.period(x,"quarters","nbinomial",begin="2010-01-01",end="2010-12-31") # that at least computes something

# third example:
 root.period(x,"days","nbinomial",scale="raw") # values are plotted on the raw scale (y label is Frequency)
 root.period(x,"days","nbinomial") # values are plotted on the square root scale (y label is sqrt(Frequency))

# fourth example:
x<- read.loss(1,2,loss.data.object)
b = "2010-01-01"
e = "2010-12-31"
 root.period(x,"days","nbinomial",begin=b,end=e) # fit via ML (Maximum Likelihood) 
 root.period(x,"days","nbinomial","MinChisq",begin=b,end=e) # fit via Minimum Chi-squared.
 # there are some significant differences in function values table, param and p

# fifth example:
 # some changes of bar colours, lines and points types
 root.period(x,"days","nbinomial",rect_gp =gpar(fill = "lightblue"),
		lines_gp = gpar(col = "black"),points_gp = gpar(col = "darkblue"), pch = 20)

# sixth example:
 root.period(x,"days","nbinomial") # differences in bar types
 root.period(x,"days","nbinomial",bar_type="standing")
 root.period(x,"days","nbinomial",bar_type="deviation")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
