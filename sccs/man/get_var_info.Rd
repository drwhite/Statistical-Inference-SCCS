\name{get_var_info}
\alias{get_var_info}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get_var_info
}

\description{
Analyzes data ...
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get_var_info(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
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
function(data) {
  var_names<-names(data)
  metadata<-NULL

  for (i in 1:length(var_names)){
    nmiss<-length(which(is.na(data[,var_names[i]])))
    numeric<-is.numeric(data[,var_names[i]])
    numDiscrVals<-length(table(data[,var_names[i]]))
    metadata<-rbind(metadata,cbind(nmiss,data.frame(numeric),numDiscrVals))
  }
  row.names(metadata)<-var_names
  return(metadata)
}
}
