#3.iamd
iamd.cal=function(res1,auc){
  M=nrow(res1)
  K=M*10
  fpf.k=seq(0,1,length.out=K)
  amd.k=tpm.k=fpm.k=rep(NA,K)
  for(k in 1:K){
    which.k=min(which(res1$fpf<=fpf.k[k]))
    if(length(which.k)){
      amd.k[k]=res1$amd[which.k]

      tpm.k[k]=res1$tpm[which.k]
      fpm.k[k]=res1$fpm[which.k]
    }
  }
  iamd=mean(amd.k,na.rm=TRUE)
  ccm.idx=!(is.na(tpm.k)|is.na(fpm.k)) #considered when both tpm.k and fpm.k are not missing
  itpm=mean(tpm.k[ccm.idx])
  ifpm=mean(fpm.k[ccm.idx])

  res3=data.frame(auc=auc,itpm=itpm,ifpm=ifpm,iamd=iamd)

  #3.return
  return(res3)
}
