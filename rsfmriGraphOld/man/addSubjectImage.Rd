\name{addSubjectImage}
\alias{addSubjectImage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
addSubjectImage(imageDataList, id, filename, directory, image.type, registered = TRUE, template = NULL, type = "nifti", verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{imageDataList}{
%%     ~~Describe \code{imageDataList} here~~
}
  \item{id}{
%%     ~~Describe \code{id} here~~
}
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
  \item{directory}{
%%     ~~Describe \code{directory} here~~
}
  \item{image.type}{
%%     ~~Describe \code{image.type} here~~
}
  \item{registered}{
%%     ~~Describe \code{registered} here~~
}
  \item{template}{
%%     ~~Describe \code{template} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (imageDataList, id, filename, directory, image.type, 
    registered = TRUE, template = NULL, type = "nifti", verbose = TRUE) 
{
    if (type != "nifti") 
        stop("Only nifti supported at this time")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
