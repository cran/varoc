#2. amd
amd.cal=function(y,x,res1){
  res1$tpm=res1$fpm=res1$amd=NA
  M=nrow(res1)
  x1=x[y==1]
  x0=x[y==0]
  for(m in 1:M){
    x11=x1[x1>res1$th[m]]
    x01=x0[x0>res1$th[m]]

    n.tpm=length(x11)
    n.fpm=length(x01)

    tpm=fpm=NA

    if(n.tpm==0){; tpm=max(x1)
    }else{;        tpm=mean(x11)
    }

    if(n.fpm==0){; fpm=max(x0)
    }else{;        fpm=mean(x01)
    }

    res1$tpm[m]=tpm
    res1$fpm[m]=fpm
  }
  res1$amd=res1$tpm-res1$fpm

  #3.return
  return(res1)
}

