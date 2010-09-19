get_var_info <- function(data) {
  var_names<-names(data)
  var_info<-NULL;

  for (i in 1:length(var_names)){
    nmiss<-length(which(is.na(data[,var_names[i]])))
    numeric<-is.numeric(data[,var_names[i]])
    num_discrete_vals<-length(table(data[,var_names[i]]))
    var_info<-rbind(var_info,cbind(nmiss,data.frame(numeric),num_discrete_vals))
  }
  row.names(var_info)<-var_names
  return(var_info)
}

normalize_proximity_matrix <- function(W,idxs) {
  diag(W)<-0 #set diagonals to 0

  #set only obs where dep var non-missing
  W<-W[idxs,idxs]

  #row standardize (rows sum to one)
  W<-W/rowSums(W) 

  return(W)
}


#--compute instrumental vars  using  the  proximity lag  of  our  indep.  varbs.--
compute_iv <- function(Y,X,W) {
  WX<-W%*%X

  WY<-W%*%Y

  o<-lm(WY~WX)

  #--the  fitted  value  is  our  instrumental  variable--
  y_hat<-fitted(o)

  #--keep  R2  from  this  regression--
  r2<-summary(o)$r.squared

  return(list(y_hat=y_hat,r2=r2,wy=WY))
}

compute_ivs <- function(Y,X,prox_list) {
  iv_data<-NULL; iv_r2s<-NULL; iv_wys<-NULL;

  for (i in 1:length(prox_list)) {
    iv = compute_iv(Y,X,prox_list[[i]])
    iv_data<-cbind(iv_data,iv$y_hat)
    iv_r2s<-cbind(iv_r2s,iv$r2)
    iv_wys<-cbind(iv_wys,iv$wy)
  }
  iv_data<-data.frame(iv_data)
  names(iv_data)<-names(prox_list)

  iv_r2s<-data.frame(iv_r2s)
  names(iv_r2s)<-names(prox_list)

  iv_wys<-data.frame(iv_wys)
  names(iv_wys)<-names(prox_list)
  list(iv_data=iv_data,iv_r2s=iv_r2s,iv_wys=iv_wys)
}

impute <- function(data_to_impute,aux_data,nimp=10,maxits=100) {

  library(foreign)
  library(mice)

  var_info = get_var_info(data_to_impute)

  var_names<-row.names(var_info)

  #--identify variables with missing values--
  vars_missing_data<-var_names[which(var_info[,1]>0)]

  #--identify variables with non-missing values--
  vars_complete_data<-var_names[which(var_info[,1]==0)]

  #--one at a time, loop through those variables with missing values--
  for (i in 1:length(vars_missing_data)){

    #--attach the imputand to the auxiliary data--
    combined_input<-data.frame(cbind(aux_data,data_to_impute[,vars_missing_data[i]]))

    #--in the following line, the imputation is done--
    imputation<-complete(mice(combined_input,maxit=maxits,m=nimp),action="long")

    #--during first iteration of the loop, create dataframe imputed_data--
    if (i==1){
      imputed_data<-data.frame(imputation[,c(".id",".imp")])
    }

    #--the imputand is placed as a field in imputed_data and named--
    imputed_data<-cbind(imputed_data,data.frame(imputation[,NCOL(combined_input)]))

    names(imputed_data)[NCOL(imputed_data)]<-vars_missing_data[i]
  }

  #--now the non-missing variables are attached to imputed_data--
  replicated_complete_data<-NULL
  for (i in 1:nimp){
    replicated_complete_data<-rbind(replicated_complete_data,data.frame(data_to_impute[,vars_complete_data]))
  }
  imputed_data<-cbind(imputed_data,replicated_complete_data)
}

two_stage_ols <- function(dataset,y,prox_list,indep_vars,restrict_vars) {

  #--create  spatially  lagged  dep.  varbs.  in  stage  1  OLS--
  X<-as.matrix(dataset[,indep_vars])

  Y<-as.matrix(y)

  ivs = compute_ivs(Y,X,prox_list)
  iv_data<-ivs$iv_data; iv_r2s<-ivs$iv_r2s; iv_wys<-ivs$iv_wys;

  dataset<-cbind(dataset,iv_data)

  #--Stage  2  OLS  estimate  of  unrestricted  model--
  (norestrict_eq <- as.formula(paste("y ~ ", paste(c(names(iv_data),indep_vars), collapse= "+"))))

  stage2_ols_estimate_ur<-lm(norestrict_eq,data=dataset)
 
  #TODO
  #if (length(intersect(restrict_vars,y_hat)) == 0) { restrict_vars[length(restrict_vars)+1]="y_hat" }
  
  #if( length(intersect(indep_vars,restrict_vars)) != length(restrict_vars)) { 
  #   warning("all restrict_vars must be in indep_vars") 
  #}

  if( length(restrict_vars) >= length(indep_vars)) { warning("# restrict_vars >= # indep_vars") }
  
  #--Stage  2  OLS  estimate  of  restricted  model--
  (restrict_eq <- as.formula(paste("y ~ ", paste(c(names(iv_data),restrict_vars), collapse= "+"))))

  stage2_ols_estimate_r<-lm(restrict_eq,data=dataset)  
  
  #--corrected  sigma2  and  R2  for  2SLS--
  qX<-dataset

  for (i in 1:length(iv_wys)) {
    qX[,names(iv_wys)[i]]<-iv_wys[,i]
  }

  b<-coef(stage2_ols_estimate_r)

  incpt<-matrix(1,NROW(qX),1)

  x<-as.matrix(cbind(incpt,qX[,names(b)[-1]]))

  e<-Y-x%*%as.matrix(b)

  sigma_corrected<-as.numeric(t(e)%*%e/(NROW(x)-NCOL(x)))

  r2_corrected<-as.numeric(1-t(e)%*%e/sum((Y-mean(Y))^2))
  
  #--collect  some  model  diagnostics--
  
  #--Ramsey  RESET  test--
  ramsey_reset<-qchisq(resettest(stage2_ols_estimate_r,type="fitted")$"p.value",1,lower.tail=FALSE)

  drop_vars=setdiff(indep_vars,restrict_vars)

  #--Wald  test  (H0:  dropped  variables  have  coefficient  equal  zero)--
  o<-linear.hypothesis(stage2_ols_estimate_ur,drop_vars,test="Chisq")$"Pr(>Chisq)"[2]

  wald<-qchisq(o,1,lower.tail=FALSE)  #find  Chisq  with  1  d.f.  and  same  pvalue

  #--Heteroskedasticity  test  (H0:  homoskedastic  residuals)--
  heteroskedast<-ncv.test(stage2_ols_estimate_r)$ChiSquare

  #--Shapiro-Wilke  normality  test  (H0:  residuals  normal)
  shapiro_wilke_norm<-qchisq(shapiro.test(e)$p.value,1,lower.tail=FALSE)

  vif<-vif(stage2_ols_estimate_r)

  ss<-diag(summary(stage2_ols_estimate_r)$cov*sigma_corrected)

  #--collect  robust  coef.  variances  when  there  is  heteroskedasticity--
  #eb<-e^2
  #x<-as.matrix(cbind(incpt,dataset_i[,names(b)[-1]]))
  #hcm<-inv(t(x)%*%x)%*%t(x)%*%diag(eb[1:length(eb)])%*%x%*%inv(t(x)%*%x)
  #ss<-rbind(ss,diag(hcm))
  coeff<-coef(stage2_ols_estimate_r)

  r2s<-cbind(r2_corrected,iv_r2s)
  names(r2s)[1]<-"R2:final model"

  #TODO: remove space, e.g. R2:IV_ language
  for (i in 1:length(iv_r2s)) {
    names(r2s)[i+1]<-paste("R2:IV_",names(iv_r2s[i]))
  }

  diagnostics<-cbind(ramsey_reset,wald,heteroskedast,shapiro_wilke_norm)
  diagnostic_vars<-c("RESET","Wald on restrs","NCV","SW normal")

  for (i in 1:length(prox_list)) {
    W_mat<-mat2listw(as.matrix(prox_list[[i]])) 

    #--LaGrange  Multiplier  test  for  network  autocorrelation
    o<-lm.LMtests(stage2_ols_estimate_r,  W_mat,  test=c("LMlag"))

    lag<-as.numeric(o$LMlag$statistic)

    diagnostics<-cbind(diagnostics,lag)

    #TODO: fix space
    diagnostic_vars <- c(diagnostic_vars,paste("lag:",names(prox_list[i])))
  }
  diagnostics <- data.frame(diagnostics)
  names(diagnostics) <- diagnostic_vars

  ret_val = list(vif = vif, ss = ss, coeff = coeff, diagnostics = diagnostics, r2s = r2s)
}


#-----------------------------------------------------
#---Performs multiple imputation and estimate  model  on  each  imputed  dataset
#-----------------------------------------------------
two_stage_ols_mi <- function(data,aux_data,prox_list,dep_var,indep_vars,restrict_vars,sig_digits=3,nimp=10,niterations=100) {

dep_var_idx = which(names(data)==dep_var)

y=data[,dep_var_idx] 

data=data[,-1*dep_var_idx] 

imputed_data=impute(data,aux_data,nimp,niterations)

library(foreign)
library(spdep)
library(car)
library(lmtest)
library(sandwich)

#--will  append  values  to  these  empty  objects--
vif<-NULL; ss<-NULL; coeffs<-NULL; diagnostics<-NULL;  r2s<-NULL;
#imputed_data<-imputed_data[,-dep_var]

y_non_missing_idxs<-which(!is.na(y))

y=y[y_non_missing_idxs]

#iterate over proximity matrixes and normalize
for (i in 1:length(prox_list)) {
  prox_list[[i]] = normalize_proximity_matrix(prox_list[[i]],y_non_missing_idxs)
}

#--loop  through  the  imputed  datasets--
results = NULL;

for (i in 1:nimp){
  
  #--select  the  ith  imputed  dataset--
  dataset_i<-imputed_data[which(imputed_data$.imp==i),]

  #--retain  only  obs.  for  which  dep.  varb.  is  nonmissing--
  dataset_i<-dataset_i[y_non_missing_idxs,]
  
  results = two_stage_ols(dataset_i,y,prox_list,indep_vars,restrict_vars) 

  #--collect  coefficients  and  their  variances--
  vif<-rbind(vif,results$vif)

  ss<-rbind(ss,results$ss)

  coeffs<-rbind(coeffs,results$coeff)

  diagnostics<-rbind(diagnostics,results$diagnostics)

  r2s<-rbind(r2s,results$r2s)

}

#--first find final regr. coefs. and p-values--
 mean_coeffs<-apply(coeffs,2,mean)

 var_coeffs<-colSums((coeffs-t(matrix(mean_coeffs,length(mean_coeffs),nimp)))^2)/(nimp-1)

 mnv<-apply(ss,2,mean)

 vrT<-mnv+var_coeffs*(1-nimp^(-1))

 fst<-mean_coeffs^2/vrT

 r<-(1+nimp^(-1))*var_coeffs/mnv

 v<-(nimp-1)*(1+r^(-1))^2

 pval<-pf(fst,1,v,lower.tail=FALSE)

 restrict_stats<-data.frame(round(cbind(mean_coeffs,fst,v,pval),sig_digits))

 restrict_stats$VIF[2:NROW(restrict_stats)]<-round(apply(vif,2,mean),sig_digits)

 names(restrict_stats)<-c("coef","Fstat","ddf","pvalue","VIF")

 #--Then combine the diagnostics we collected--
 diagnostics<-data.frame(diagnostics)

 r2<-apply(r2s,2,mean)

 mdm<-apply(diagnostics,2,mean)

 vrd<-colSums((diagnostics-t(matrix(mdm,length(mdm),nimp)))^2)/(nimp-1)

 aa<-4*mdm^2-2*vrd

 aa[which(aa<0)]<-0

 rd<-(1+nimp^(-1))*vrd/(2*mdm+aa^.5)

 vd<-(nimp-1)*(1+rd^(-1))^2

 Dm<-(mdm-(nimp-1)/(nimp+1)*rd)/(1+rd)

 #-All chi-sq we collected have df=1-------
 pvald<-pf(Dm,1,vd,lower.tail=FALSE)

 restrict_diagnostics<-data.frame(round(cbind(Dm,vd,pvald),3))

 names(restrict_diagnostics)<-c("Fstat","df","pvalue")

 list(imputed_data = imputed_data,
      diagnostics = diagnostics, 
      coeffs = coeffs, 
      ss = ss, 
      vif = vif, 
      r2 = r2, 
      restrict_stats = restrict_stats, 
      restrict_diagnostics = restrict_diagnostics)
}

