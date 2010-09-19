 #--estimate model with network-lagged dependent variables
 ols_stats=two_stage_ols_mi(model$data,
                            model$aux_data,
                            model$prox_list,
                            model$dep_var,
                            model$indep_vars,
                            model$restrict_vars,
                            sig_digits=3,
                            nimp=5,
                            niterations=10)

 #--write results to table file for perusal in spreadsheet--
 summary_results_file=paste(model$alias,"_ols_summary_results.csv",sep="")
 title=paste("==",model$alias,"OLS model for",model$dep_var,"==")
 write.table(title,file=summary_results_file,append=F,col.names=F,row.names=F)
 write.table(ols_stats$restrict_stats,file=summary_results_file,append=T,sep=",")
 write.table(ols_stats$r2,file=summary_results_file,append=T,col.names=F,sep=",")
 write.table(ols_stats$restrict_diagnostics,file=summary_results_file,append=T,sep=",")

 full_results_file=paste(model$alias,"_ols_full_results.RData",sep="")
 save(ols_stats,file=full_results_file)
 
