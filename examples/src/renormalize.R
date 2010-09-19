#Normalize.R
normalize_probit <- function(var) {
 min = 1
 #build frequency table sorted by value ignoring missing values
 rank_freqs = table(sort(var),useNA="no")
 #compute empirical cumulative distribtuion
 cum_prop = cumsum(rank_freqs)/sum(rank_freqs)
 #add 5% tails on each end and renormalize
 #cum_prop = (cum_prop+0.05)*0.95/1.05
 #compute centers of bins
 centers = cum_prop-diff(c(0,cum_prop))/2
 #normalize centers according to normal distribution
 norm_centers = qnorm(centers)
 #adjust range to have offset min
 norm_centers = (norm_centers + (min - min(norm_centers)))
 #transform variables using norm_centers
 norm_var<-var;
 for (i in 1:length(var)) {
  norm_var[i]<-norm_centers[as.character(var[i])]
 }
 return(norm_var)
}
is_ordinal <- function(var) {
 if (!is.numeric(var)) {
  return(FALSE)
 }
 idxs = which(!is.na(var))
 var_nonmissing = var[idxs]
 num_ints = sum(var_nonmissing == round(var_nonmissing))
 num_discrete_vals = length(table(var_nonmissing))
 size = length(var_nonmissing)
 return (num_ints == size && num_discrete_vals > 2 && num_discrete_vals <
size )
}
normalize_data <- function(data) {
 norm_data = data
 var_names<-names(norm_data)
 for (i in 1:length(var_names)){
  var = norm_data[,var_names[i]]
  if (is_ordinal(var)) {
    norm_data[,var_names[i]] = normalize_probit(var)
  }
 }
 return(norm_data)
}
