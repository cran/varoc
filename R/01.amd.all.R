amd.all=function(y,x){
  
  #1. ROC
  roc.fit=pROC::roc(y~x,levels=c(0,1),direction="<")
  res=data.frame(th=roc.fit$thresholds,
                 tpf=roc.fit$sensitivities,
                 fpf=(1-roc.fit$specificities),
                 tpm=NA,fpm=NA,amd=NA)
  
  auc=as.numeric(roc.fit$auc)
  #sp.range=1-c(0,fpf)
  #pauc=pROC::roc(y~x,levels=c(0,1),direction="<",partial.auc=sp.range)$auc/diff(range(sp.range))
  
  #2. amd
  M=nrow(res)
  x1=x[y==1]
  x0=x[y==0]
  for(m in 1:M){  
    amd.res=amd.cal(x1,x0,res$th[m])
    res$amd[m]=amd.res$amd
    res$tpm[m]=amd.res$tpm
    res$fpm[m]=amd.res$fpm
  }
  
  #3.return
  return(list(res=res,auc=auc))
}
