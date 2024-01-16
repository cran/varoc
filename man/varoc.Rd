\name{varoc}
\alias{varoc}
\title{VAROC: value added receiver operating characteristics (ROC) curve}
\description{ROC curve to visualize classification and continuity performances of biomarkers, diagnostic tests, or risk prediction models.}
\usage{
varoc(fit,
mzr,mzr.min=NULL,mzr.max=NULL,
main="VAROC",ylab="True positive fraction",xlab="False positive fraction",
col=c("#9932cc","#87ceeb","#ffe135","#f56642"),
legend="right",lwd=1,
cex.main=1,cex.axis=1,cex.lab=1,cex.legend=1,digits=2)
}
\arguments{
  \item{fit}{fitted results from the amd() function in the varoc R package.}
  \item{mzr}{mzr="AMD" (or "zAMD") if VAROC curve adds AMD (or zAMD, i.e. normalized AMD or test statistics). Note that mzr="zAMD" works only when pval="yes" was used for the amd() function.}
  \item{mzr.min}{minimum value of AMD (or ZAMD) that is displayed on the plot.}
  \item{mzr.max}{maximum value of AMD (or ZAMD) that is displayed on the plot.}
  \item{main}{title for the plot}
  \item{ylab}{title for the y axis.}
  \item{xlab}{title for the x axis.}
  \item{col}{color that separates AMD on the plot. Default: c("#9932cc","#87ceeb","#ffe135","#f56642")}
  \item{legend}{legend location, "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center".}
  \item{lwd}{line width}
  \item{cex.main}{main size.}
  \item{cex.axis}{axis size.}
  \item{cex.lab}{label size.}
  \item{cex.legend}{legend size.}
  \item{digits}{number of decimals.}
}
\details{The varoc function plot true positive fraction(c) (or sensitivity(c)) versus false positive fraction(c) (or one minus specificty(c)) at each threshold c colored by above mean difference(c). See the amd fuction for more details.}

\value{No return value, called for side effects.}

\references{Danielle Brister and Yunro Chung, Value added receiver operating characteristics curve (in-progress)}
\author{Yunro Chung [aut, cre]}

\examples{
set.seed(1)

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
mzr.min=min(c(fit1$res$amd,fit2$res$amd))
mzr.max=max(c(fit1$res$amd,fit2$res$amd))

varoc(fit1,mzr="AMD",mzr.min=mzr.min,mzr.max=mzr.max)
varoc(fit2,mzr="AMD",mzr.min=mzr.min,mzr.max=mzr.max)

#2.3. varoc for marker 1 vs marker 2
min=min(c(x1,x2))
max=max(c(x1,x2))
jdp(fit1,min=min,max=max)
jdp(fit2,min=min,max=max)
}
