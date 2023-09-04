iamd.all=function(y,x,fpf){
  #1.amd all
  res=amd.all(y,x)$res
  
  #2.iamd
  M=nrow(res)
  
  K=M*10
  fpf.k=seq(0,fpf,length.out=K)
  amd.k=tpm.k=fpm.k=rep(NA,K)
  for(k in 1:K){ 
    which.k=min(which(res$fpf<=fpf.k[k]))
    if(length(which.k)){
      amd.k[k]=res$amd[which.k]

      tpm.k[k]=res$tpm[which.k]
      fpm.k[k]=res$fpm[which.k]
    }
  }
  iamd=mean(amd.k,na.rm=TRUE)

  ccm.idx=!(is.na(tpm.k)|is.na(fpm.k)) #considered when both tpm.k and fpm.k are not missing
  itpm=mean(tpm.k[ccm.idx])
  ifpm=mean(fpm.k[ccm.idx])

  #3.summary
  iamd.res=data.frame(iamd=iamd,itpm=itpm,ifpm=ifpm)
                     
  return(iamd.res)
}
