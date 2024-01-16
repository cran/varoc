varoc=function(fit,
               mzr="AMD",mzr.min=NULL,mzr.max=NULL,
               main="VAROC",ylab="True positive fraction",xlab="False positive fraction",
               col=c("#9932cc","#87ceeb","#ffe135","#f56642"),
               legend="right",lwd=1,
               cex.main=1,cex.axis=1,cex.lab=1,cex.legend=1,
               digits=2){

  #1.data
  y=fit$df$yf
  x=fit$df$x
  res=fit$res
  tpf=res$tpf
  fpf=res$fpf
  M=nrow(res)

  auc=fit$iamd$auc
  MZR=toupper(mzr)
  if(is.null(res$z))
    MZR="AMD"

  if(MZR=="ZAMD"){
    mzr=res$zAMD
    mzr.title="zAMD"
    iamd=fit$iamd$zIAMD
    iamd.title="zIAMD="
  }else{
    mzr=res$amd #overlap
    mzr.title="AMD"
    iamd=fit$iamd$iamd
    iamd.title="IAMD="
  }

  #2. mzr range
  mzr.min0=min(mzr,na.rm=TRUE)
  mzr.max0=max(mzr,na.rm=TRUE)

  if(is.null(mzr.min)){
    mzr.min=mzr.min0
  }else{
    mzr.min=min(mzr.min,mzr.min0)
  }
  if(is.null(mzr.max)){
    mzr.max=mzr.max0
  }else{
    mzr.max=max(mzr.max,mzr.max0)
  }

  #3.plot
  #3.1. range
  mzr.order=round((mzr-mzr.min)/(mzr.max-mzr.min)*M,0) #kind of normalization, match AMD to color
                                                       #map from mzr to [0,1]*M
  mzr.order[mzr.order==0]=1

  col0=grDevices::colorRampPalette(col)(M)
  col1=col0[mzr.order] #line colors

  M2=length(col)
  col2 = grDevices::colorRampPalette(col)(M2) #legend colors

  plot(NA,col=col1,type='p',pch=19,
       ylab=ylab,xlab=xlab,
       main=main,cex.main=cex.main,
       cex.axis=cex.axis,cex.lab=cex.lab,lwd=lwd,xlim=c(0,1),ylim=c(0,1))
  graphics::abline(a=0,b=1,col="darkgray",lwd=lwd)
  for(m in 2:M)
    graphics::lines(c(tpf[m],tpf[m-1])~c(fpf[m],fpf[m-1]),col=col1[m],type='S',lwd=lwd)
  for(m in 1:M)
    graphics::points(tpf[m]~fpf[m],col=col1[m],cex=1,pch=19)

  #3.2. mzr legend
  mzr.cut=round(seq(from=mzr.min,to=mzr.max,length.out=M2+1),digits)
  mzr.cut=format(mzr.cut)

  rt=0.05 #move to the right because of anti diagnoal line (rt=0 is no moving)
  text(0.5+rt,0.15,mzr.title,cex=cex.legend)

  corrplot::colorlegend(
    colbar=col2,
    labels=mzr.cut,
    at = seq(0, 1, len = M2+1),
    xlim = c(0.05, 0.90)+rt, ylim = c(0, 0.1),
    vertical = FALSE, cex=cex.legend)

  if(legend%in%c("bottomleft", "topleft", "topright", "bottomright", "left", "right", "top", "bottom", "center")){
    AUC.text=paste0("AUC=",format(round(auc,2),nsmall=2))
    IAMD.text=paste0(iamd.title,format(round(iamd,2),nsmall=2))

    graphics::legend(legend,paste0(AUC.text,"\n",IAMD.text),adj=0,cex=cex.legend,bty='n')
  }
}
