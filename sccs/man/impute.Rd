\name{impute}
\alias{impute}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
impute
%%  ~~function to do ... ~~
}
\description{
Imputes ...
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
impute(data_to_impute, aux_data, nimp = 10, maxits = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_to_impute}{
%%     ~~Describe \code{data_to_impute} here~~
}
  \item{aux_data}{
%%     ~~Describe \code{aux_data} here~~
}
  \item{nimp}{
%%     ~~Describe \code{nimp} here~~
}
  \item{maxits}{
%%     ~~Describe \code{maxit} here~~
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
function(data_to_impute,aux_data,nimp=10,maxits=100) {

  metadata = analyze_data(data_to_impute)

  #--identify variables with missing values--
  vars_missing_data<-var_names[which(metadata[,1]>0)]

  #--identify variables with non-missing values--
  vars_complete_data<-var_names[which(metadata[,1]==0)]

  #--one at a time, loop through those variables with missing values--
  for (i in 1:length(vars_missing_data)){

    #--attach the imputand to the auxiliary data--
    combined_input<-data.frame(cbind(aux_data,data_to_impute[,vars_missing_data[i]]))

    #--in the following line, the imputation is done--
    aqq<-complete(mice(combined_input,maxits,m=nimp),action="long")

    #--during first iteration of the loop, create dataframe imputed_data--
    if (i==1){
      imputed_data<-data.frame(aqq[,c(".id",".imp")])
    }

    #--the imputand is placed as a field in imputed_data and named--
    imputed_data<-cbind(imputed_data,data.frame(aqq[,NCOL(combined_input)]))

    names(imputed_data)[NCOL(imputed_data)]<-data[i]
  }

  #--now the non-missing variables are attached to impdat--
  replicated_complete_data<-NULL
  for (i in 1:nimp){
    replicated_complete_data<-rbind(replicated_complete_data,data.frame(data_to_impute[,vars_complete_data]))
  }
  impdat<-cbind(impdat,replicated_complete_data)
  }
}
