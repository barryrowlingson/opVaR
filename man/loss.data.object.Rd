% --- Source file: loss.data.object.Rd ---
\name{loss.data.object}
\alias{loss.data.object}
\docType{data}
\title{ Operational risk data}
\description{
  A dataset containing losses and losses' dates with given business and risk categorys.
}
\usage{data(loss.data.object)}
\format{\code{loss.data.object} is a list. It contains: names of business lines (\code{loss.data.object$blines}), names of risk categories (\code{loss.data.object$rcateg}), \code{loss.data.object$risk} which indicates how \code{Internal_RC_ID} and \code{rcateg} are connected, \code{loss.data.object$business} which indicates how \code{Internal_BL_ID} and \code{blines} are connected, \code{losses} which contains \code{Internal_RC_ID}, \code{Internal_BL_ID}, loss date and its gross amount.
}
% \details{
%  ~~ If necessary, more details than the __description__ above ~~
% }
% \source{
%   ~~ reference to a publication or URL from which the data were obtained ~~
% }
% \references{
%  ~~ possibly secondary sources and usages ~~
% }
\examples{
data(loss.data.object)
summary(loss.data.object) # a simple summary
str(loss.data.object) # compactly display the internal structure of an loss.data.object (more detailed)

head(loss.data.object$losses)
%
% ## maybe str(loss.data.object) ; plot(loss.data.object) ...
}
% \keyword{datasets}
