amd.cal=function(x1,x0,th){
  
  x11=x1[x1>th]
  x01=x0[x0>th]

  n.tpm=length(x11)
  n.fpm=length(x01)
  
  tpm=fpm=NA

  if(n.tpm==0){; tpm=max(x1)
  }else{;        tpm=mean(x11)
  }
  
  if(n.fpm==0){; fpm=max(x0)
  }else{;        fpm=mean(x01)
  }
  
  amd=tpm-fpm
  amd.res=data.frame(amd=amd,tpm=tpm,fpm=fpm)
  
  return(amd.res)
}
