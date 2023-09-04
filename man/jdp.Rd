\name{jdp}
\alias{jdp}
\title{JDP: jittered ot plot}
\description{Jittered dot plot to visualize classification and continuity performances of biomarkers.}
\usage{
jdp(fit,
min=NULL,max=NULL,eps=0.2,seed=1,
main="JDP",ylab="x",xlab=c("y=0","y=1"),
col=c("blue","red","gray","gray"),
legend="top",lwd=1,lty=3,
cex.main=1,cex.pt=1.5,cex.lab=1,cex.axis=1,cex.legend=1,digits=2)
}
\arguments{
  \item{fit}{fitted results from the amd() function in the varoc R package.}
  \item{min}{minimum value of y-axis.}
  \item{max}{maximum value of y-axis.}
  \item{eps}{jittered range of x-axis.}
  \item{seed}{seed number for jittering x-axis.}
  \item{main}{title for the plot}
  \item{ylab}{title for the y axis.}
  \item{xlab}{title for the x axis.}
  \item{col}{colors to true positive, false negative, false positive, false negative.}
  \item{legend}{legend location, "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center".}
  \item{lwd}{line width.}
  \item{lty}{line type.}
  \item{cex.main}{main size.}
  \item{cex.pt}{point size.}
  \item{cex.lab}{label size.}
  \item{cex.axis}{axis size.}
  \item{cex.legend}{legend size.}
  \item{digits}{number of decimals.}
}
\details{The jdp function plots biomarker (or x) by jittered x-axis (y=0 vs y=1) and visualizes its classification and continuity metrics. The vertical dotted line is the threshold corresponding to FPF=fpf, and the two horizontal lines at y=0 and y=1 are false positive and true positive means, respectively. See the amd function for more details.}

\value{No return value, called for side effects.}

\references{Danielle Brister and Yunro Chung, Value added receiver operating characteristics curve (in-progress)}
\author{Yunro Chung [aut, cre]}

\examples{
set.seed(3)

n1=50
n0=50

#1. marker 1 (useless biomaker)
y1=c(rep(1,n1),rep(0,n0))
x1=abs(c(rnorm(n1,0,1),rnorm(n0,0,1)))

#1.1.amd
fit1=amd(y=y1,x=x1,fpf=0.3)
print(fit1)

#1.2. varoc
varoc(fit1)

#1.3. jdp
jdp(fit1)


#2. marker 2 (useful biomarker)
y2=y1
x2=abs(c(rnorm(n1,1,1),rnorm(n0,0,1)))

#2.1. amd
fit2=amd(y=y2,x=x2,fpf=0.3)

#2.2. varoc for marker 1 vs marker 2
amd.min=min(c(fit1$res$amd,fit2$res$amd))
amd.max=max(c(fit1$res$amd,fit2$res$amd))
varoc(fit1,amd.min=amd.min,amd.max=amd.max)
varoc(fit2,amd.min=amd.min,amd.max=amd.max)

#2.3. varoc for marker 1 vs marker 2
min=min(c(x1,x2))
max=max(c(x1,x2))
jdp(fit1,min=min,max=max)
jdp(fit2,min=min,max=max)
}
