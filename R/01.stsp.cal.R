#1. ROC
stsp.cal=function(y,x){
  roc1=pROC::roc(response=y,predictor=x,levels=c(0,1),direction="<")
  res1=data.frame(th=roc1$thresholds,
                  tpf=roc1$sensitivities,
                  fpf=(1-roc1$specificities),
                  tpm=NA,fpm=NA,amd=NA)
  auc=as.numeric(roc1$auc)
  return(list(res1=res1,auc=auc))
}
