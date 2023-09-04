varoc=function(fit,
               amd.min=NULL,amd.max=NULL,
               main="VAROC",ylab="True positive fraction",xlab="False positive fraction",
               col=c("#9932cc","#87ceeb","#ffe135","#f56642"),
               legend="right",lwd=1,
               cex.main=1,cex.axis=1,cex.lab=1,cex.legend=1,
               digits=2){

  #1.data
  y=fit$df$yf
  x=fit$df$x
  res=fit$res
  M=nrow(res)

  auc=fit$auc
  iamd=fit$iamd$iamd

  #2. amd range
  amd.min0=min(res$amd,na.rm=TRUE)
  amd.max0=max(res$amd,na.rm=TRUE)

  if(is.null(amd.min)){
    amd.min=amd.min0
  }else{
    amd.min=min(amd.min,amd.min0)
  }
  if(is.null(amd.max)){
    amd.max=amd.max0
  }else{
    amd.max=max(amd.max,amd.max0)
  }

  #3.plot
  #3.1. range
  amd.order=round((res$amd-amd.min)/(amd.max-amd.min)*M,0) #kind of normalization, match AMD to color
  amd.order[amd.order==0]=1

  col0 = grDevices::colorRampPalette(col)(M)
  col1=col0[amd.order] #line colors

  M2=length(col)
  col2 = grDevices::colorRampPalette(col)(M2) #legend colors

  plot(NA,col=col1,type='p',pch=19,
       ylab=ylab,xlab=xlab,
       main=main,cex.main=cex.main,
       cex.axis=cex.axis,cex.lab=cex.lab,lwd=lwd,xlim=c(0,1),ylim=c(0,1))
  graphics::abline(a=0,b=1,col="darkgray",lwd=lwd)
  for(m in 2:M)
    graphics::lines(c(res$tpf[m],res$tpf[m-1])~c(res$fpf[m],res$fpf[m-1]),col=col1[m],type='S',lwd=lwd)
  for(m in 1:M)
    graphics::points(res$tpf[m]~res$fpf[m],col=col1[m],cex=1,pch=19)

  #3.2. amd legend
  if(is.null(digits)){
    digits=2
    if(max(abs(c(amd.min,amd.max)))>10)
      digits=0
  }

  amd.cut=round(seq(from=amd.min,to=amd.max,length.out=M2+1),digits)
  amd.cut=format(amd.cut)

  rt=0.05 #move to the right because of anti diagnoal line (rt=0 is no moving)
  text(0.5+rt,0.15,"AMD",cex=cex.legend)

  corrplot::colorlegend(
    colbar=col2,
    labels=amd.cut,
    at = seq(0, 1, len = M2+1),
    xlim = c(0.05, 0.90)+rt, ylim = c(0, 0.1),
    vertical = FALSE, cex=cex.legend)

  if(legend%in%c("bottomleft", "topleft", "topright", "bottomright", "left", "right", "top", "bottom", "center")){
    AUC.text=paste0("AUC=",format(round(auc,2),nsmall=2))
    IAMD.text=paste0("IAMD=",format(round(iamd,2),nsmall=2))

    graphics::legend(legend,paste0(AUC.text,"\n",IAMD.text),adj=0,cex=cex.legend,bty='n')
  }

  return(fit)
}
