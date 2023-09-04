amd=function(y,x,fpf=0.3,pval="no",alternative="greater",B=2000,conf.level=0.95){

  #0.data frame
  df=data.frame(y=y,x=x)
  n=nrow(df)

  #1. ROC & amd at all points
  res0=amd.all(y=y,x=x)
  auc=res0$auc
  res=res0$res

  sp.range=1-c(0,fpf)
  pauc=pROC::roc(y~x,levels=c(0,1),direction="<",partial.auc=sp.range)$auc/diff(range(sp.range))

  #2. amd at fpf, iamd
  res1=amd.single(y=y,x=x,fpf=fpf) #amd
  res2=iamd.all(  y=y,x=x,fpf=1)   #iamd
  res3=iamd.all(  y=y,x=x,fpf=fpf) #piamd

  res1$lcl=res2$lcl=res3$lcl=NA
  res1$ucl=res2$ucl=res3$ucl=NA

  res1$z=res2$z=res3$z=NA
  res1$pvalue=res2$pvalue=res3$pvalue=NA

  ###
  #3. amd, bootstrap
  ###
  if(pval=="yes"){
    res1.b=res2.b=res3.b=rep(NA,B)
    for(b in 1:B){
      df.b=df[sample(1:n,replace=TRUE),]

      y.b=df.b$y
      x.b=df.b$x

      res1.b[b]=amd.single(y=y.b,x=x.b,fpf=fpf)$amd
      res2.b[b]=iamd.all(  y=y.b,x=x.b,fpf=1)$iamd
      res3.b[b]=iamd.all(  y=y.b,x=x.b,fpf=fpf)$iamd
    }

    res1.se.b=sqrt(var(res1.b,na.rm=TRUE))
    res2.se.b=sqrt(var(res2.b,na.rm=TRUE))
    res3.se.b=sqrt(var(res3.b,na.rm=TRUE))

    if(alternative=="two.sided"){
      crit=qnorm(1-(1-conf.level)/2)

      res1$lcl=res1$amd-crit*res1.se.b;  res1$ucl=res1$amd+crit*res1.se.b
      res2$lcl=res2$iamd-crit*res2.se.b; res2$ucl=res2$iamd+crit*res2.se.b
      res3$lcl=res3$iamd-crit*res3.se.b; res3$ucl=res3$iamd+crit*res3.se.b

      res1$z=res1$amd/res1.se.b
      res2$z=res2$iamd/res2.se.b
      res3$z=res3$iamd/res3.se.b

      res1$pvalue=2*pnorm(abs(res1$z),lower.tail=FALSE)
      res2$pvalue=2*pnorm(abs(res2$z),lower.tail=FALSE)
      res3$pvalue=2*pnorm(abs(res3$z),lower.tail=FALSE)
    }else if(alternative=="greater"){
      crit=qnorm(conf.level)

      res1$lcl=res1$amd-crit*res1.se.b;  res1$ucl=Inf
      res2$lcl=res2$iamd-crit*res2.se.b; res2$ucl=Inf
      res3$lcl=res3$iamd-crit*res3.se.b; res3$ucl=Inf

      res1$pvalue=pnorm(res1$z,lower.tail=FALSE)
      res2$pvalue=pnorm(res2$z,lower.tail=FALSE)
      res3$pvalue=pnorm(res3$z,lower.tail=FALSE)
    }else if(alternative=="less"){
      crit=qnorm(conf.level)

      res1$lcl=-Inf; res1$ucl=res1$amd+crit*res1.se.b
      res2$lcl=-Inf; res2$ucl=res2$iamd+crit*res2.se.b
      res3$lcl=-Inf; res3$ucl=res3$iamd+crit*res3.se.b

      res1$pvalue=pnorm(res1$z)
      res2$pvalue=pnorm(res2$z)
      res3$pvalue=pnorm(res3$z)
    }
  }

  #4. summary
  res3$th=res1$th #piamd used same
  res3$tpf=res1$tpf
  res3$fpf=res1$fpm
  res3=res3[,c("th", "tpf","fpf","itpm","ifpm","iamd","lcl","ucl","z","pvalue")] #sorting

  rownames(res1)=NULL
  rownames(res2)=NULL
  rownames(res3)=NULL

  fit=list(df=df,
           fpf=fpf,
           auc=auc,
           pauc=pauc,
           res=res,
           amd=res1,
           iamd=res2,
           piamd=res3)

  return(fit)
}
