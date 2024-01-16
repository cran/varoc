amd=function(y,x,fpf=0.3,pval="no",alternative="greater",B=2000,conf.level=0.95){

  #0.data frame
  df=data.frame(y=y,x=x)
  n=nrow(df)

  #1. ROC & amd at all points
  roc.fit=stsp.cal(y=y,x=x)
  res1=roc.fit$res1
  auc1=roc.fit$auc

  res1=amd.cal(y=y,x=x,res1=res1) #amd at all th
  res2=res1[tail(which(res1$fpf>=fpf),1),] #amd at fpf
  res3=iamd.cal(res1=res1,auc=auc1) #iamd

  ###
  #3. amd, bootstrap
  ###
  if(pval=="yes"){
    RES1=matrix(NA,B,nrow(res1))
    RES2=RES3=rep(NA,B)
    for(b in 1:B){
      df.b=df[sample(1:n,replace=TRUE),]
      y.b=df.b$y
      x.b=df.b$x

      RES1[b,]=amd.cal(y=y.b,x=x.b,res1=res1)$amd #res1, not res1.b, since we use the same cutoff

      roc.fit.b=stsp.cal(y=y.b,x=x.b)
      res1.b=roc.fit.b$res1
      auc1.b=roc.fit.b$auc

      res1.b=amd.cal(y=y.b,x=x.b,res1=res1.b) #res1, not res1.b, since we use the same cutoff
      RES2[b]=res1.b[tail(which(res1.b$fpf>=fpf),1),]$amd
      RES3[b]=iamd.cal(res1=res1.b,auc=NA)$iamd
    }
    res1.se.b=sqrt(apply(RES1,2,var,na.rm=TRUE))
    res2.se.b=sqrt(var(RES2,na.rm=TRUE))
    res3.se.b=sqrt(var(RES3,na.rm=TRUE))

    res1$lcl=NA; res1$ucl=NA; res1$z=NA; res1$pvalue=NA
    res2$lcl=NA; res2$ucl=NA; res2$z=NA; res2$pvalue=NA
    res3$lcl=NA; res3$ucl=NA; res3$z=NA; res3$pvalue=NA

    res1$z=res1$amd/res1.se.b
    res2$z=res2$amd/res2.se.b
    res3$z=res3$iamd/res3.se.b

    if(alternative=="two.sided"){
      crit=qnorm(1-(1-conf.level)/2)

      res1$lcl=res1$amd-crit*res1.se.b;  res1$ucl=res1$amd+crit*res1.se.b
      res2$lcl=res2$iamd-crit*res2.se.b; res2$ucl=res2$iamd+crit*res2.se.b
      res3$lcl=res3$iamd-crit*res3.se.b; res3$ucl=res3$iamd+crit*res3.se.b

      res1$pvalue=2*pnorm(abs(res1$z),lower.tail=FALSE)
      res2$pvalue=2*pnorm(abs(res2$z),lower.tail=FALSE)
      res3$pvalue=2*pnorm(abs(res3$z),lower.tail=FALSE)
    }else if(alternative=="greater"){
      crit=qnorm(conf.level)

      res1$lcl=res1$amd-crit*res1.se.b;  res1$ucl=Inf
      res2$lcl=res2$amd-crit*res2.se.b;  res2$ucl=Inf
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

    colnames(res1)[colnames(res1)=="z"]="zAMD"
    colnames(res2)[colnames(res2)=="z"]="zAMD"
    colnames(res3)[colnames(res3)=="z"]="zIAMD"
  }

  #4. summary
  rownames(res1)=NULL
  rownames(res2)=NULL
  rownames(res3)=NULL



  fit=list(df=df,
           fpf=fpf,
           res=res1,
           amd=res2,
           iamd=res3)

  return(fit)
}
