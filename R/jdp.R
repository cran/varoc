jdp=function(fit,
             min=NULL,max=NULL,eps=0.2,seed=1,
             main="JDP",ylab="x",xlab=c("y=0","y=1"),
             col=c("blue","red","gray","gray"),
             legend="top",lwd=1,lty=3,
             cex.main=1,cex.pt=1.5,cex.lab=1,cex.axis=1,cex.legend=1,digits=2){

  set.seed(seed)

  fpf=fit$fpf
  pauc=fit$pauc
  piamd=fit$piamd$iamd

  #1. data
  y=fit$df$x #x is plotted as y-value
  d=fit$df$y #y is plotted as x-value
  y1=y[d==1]
  y0=y[d==0]

  #2. th at fpf
  res=fit$res
  res.fpf=res[tail(which(res$fpf>=fpf),1),]
  th=res.fpf$th
  tpf=res.fpf$tpf
  amd=res.fpf$amd
  tpm=res.fpf$tpm
  fpm=res.fpf$fpm

  #3. plot
  #3.1. base plot
  ylim=range(y)
  if(!is.null(min)) ylim[1]=min
  if(!is.null(max)) ylim[2]=max

  n=length(d)
  d.jitter=d+stats::runif(n,-eps,eps)
  plot(y~d.jitter,xlim=c(0-eps,1+eps),ylim=ylim,
       main=main,cex.main=cex.main,
       ylab=ylab,xlab="",xaxt='n',
       cex=cex.pt,cex.lab=cex.lab,cex.axis=cex.axis)
  graphics::axis(1,xlab,at=c(0,1), cex.axis=cex.lab)
  abline(h=th,lwd=lwd,lty=lty)

  #3.2. color
  #_{disease , marker}
  d11=d.jitter[d==1&y>th]; d10=d.jitter[d==1&y<=th]
  d01=d.jitter[d==0&y>th]; d00=d.jitter[d==0&y<=th]

  y11=y1[y1>th]; y10=y1[y1<=th]
  y01=y0[y0>th]; y00=y0[y0<=th]

  points(y01~d01,col=col[1],pch=19,cex=cex.pt)
  points(y00~d00,col=col[3],pch=19,cex=cex.pt)
  points(y10~d10,col=col[4],pch=19,cex=cex.pt)
  points(y11~d11,col=col[2],pch=19,cex=cex.pt)

  #3.3. Add horizontal lines for TPM, FPM
  eps2=(eps)*0.9
  graphics::arrows(-eps2,  fpm, eps2,   fpm, code=0,lwd=3)
  graphics::arrows(-eps2+1,tpm, eps2+1, tpm, code=0,lwd=3)

  #3.4. adding points again
  points(y~d.jitter,cex=cex.pt)

  #3.5.legend
  if(legend%in%c("bottomleft", "topleft", "topright", "bottomright", "left", "right", "top", "bottom", "center")){
    #tpf.text=paste0("TPF=",format(round(tpf,digits),nsmall=digits))
    #amd.text=paste0("AMD=",format(round(amd,digits),nsmall=digits))
    #graphics::legend(legend,paste0(tpf.text,"\n",amd.text),bty='n',cex=cex.legend)

    tpf.text=paste0("TPF=",format(round(tpf,2),nsmall=2))
    pauc.text=paste0("PAUC=",format(round(pauc,2),nsmall=2))
    amd.text=paste0("AMD=",format(round(amd,2),nsmall=2))
    piamd.text=paste0("PIAMD=",format(round(piamd,2),nsmall=2))
    legend(legend,paste0(tpf.text,"\n",
                         pauc.text,"\n","\n",
                         amd.text,"\n",
                         piamd.text,"\n"
    ),bty='n',cex=cex.legend)
  }

  return(fit)
}
