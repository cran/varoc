amd.single=function(y,x,fpf){
  
  #1. ROC
  roc.fit=pROC::roc(y~x,levels=c(0,1),direction="<")
  res=data.frame(th=roc.fit$thresholds,
                 tpf=roc.fit$sensitivities,
                 fpf=(1-roc.fit$specificities),
                 tpm=NA,fpm=NA,amd=NA)
  res=res[tail(which(res$fpf>=fpf),1),]
  
  #2. amd
  x1=x[y==1]
  x0=x[y==0]
  
  amd.res=amd.cal(x1,x0,res$th)
    
  res$amd=amd.res$amd
  res$tpm=amd.res$tpm
  res$fpm=amd.res$fpm
  
  #3.return
  return(res)
}
