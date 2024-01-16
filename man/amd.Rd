\name{amd}
\alias{amd}
\title{AMD: above mean difference}
\description{Summary measures to evaluate the continuity performance of biomarkers, diagnostic tests, or risk prediction models.}
\usage{
amd(y,x,fpf=0.3,pval="no",alternative="greater",B=2000,conf.level=0.95)
}
\arguments{
  \item{y}{binary output, where y=1 if disease (or case) and y=0 if non-disease (or control).}
  \item{x}{continuous score, e.g. biomarker, diagnostic test, risk score.}
  \item{fpf}{false positive fraction at which above mean difference is cacluated.}
  \item{pval}{"yes" for bootstrap p-value and bootstrap confidence interval.}
  \item{alternative}{alternative hypothesis: "greater"" (default), "less", "two.sided".}
  \item{B}{number of bootstrap samples.}
  \item{conf.level}{confidence level of bootstrap confidence interval.}
}
\details{The amd function summarizes a continuity performance of x at each cutoff c as: i) above mean difference (AMD) and ii) intergrated AMD (IAMD). For i), AMD(c) is true positive mean(TPM)(c) minus false positive mean(FPM)(c), where TPM(c) is E(x>c|y=1) and FPM(c) is E(x>c|y=0). For ii), IAMD is a global measure of evaluating continuity performance of x over all thresholds.

These measures can be viewed as continuous versions of ROC curve-based measures. Specifically, TPM(c) and FPM(c) are continuous versions of true positive fraction(TPF)(c) (or sensitivity(c)) and false positive fraction(FPF)(c) (or one minus specificity(c)), where TPF(c)=P(x>c|y=1) and FPF(c)=P(x>c|y=0). The useful (or useless) x has TPF(c)-FPF(c)>0 and AMD(c)>0 (or TPF(c)-FPF(c)=0 and AMD(c)=0). Similiarly, useful (or useless) x has area under the ROC curve(AUC)>0.5 and IAMD(c)>0 (or AUC=0.5 and IAMD(c)=0).

The bootstrap p-value and confidence interval are computed under the null hypthesis: AMD(c)\eqn{\leq}0 or IAMD(c)\eqn{\leq}0, when pval="yes" and alterantive="greater".

The threshold c is determined by setting an acceptable fpf, i.e. FPF(c)=fpf. Thus, i) is interpreted as AMD at a FPF of fpf, ii) is interprted as IAMD (or average AMD) at a FPF range of 0 and 1. The varoc and jdp functions visualize them.
}

\value{
\item{df}{data frame with y and x.}
\item{fpf}{false positive fraction at which above mean difference is cacluated.}
\item{res}{data frame with tpf, fpf, tpm, fpm, amd, lcl, ucl, zAMD, pvalue at each threshold (th)), where lcl (lower confidence limit), ucl (upper confidence limit), zAMD (test statistics) and pvalue (one-sided p-value) are for amd.}
\item{amd}{data frame with tpf, fpf,  tpm, fpf, amd, lcl, ucl, zAMD, pvalue at FPF(th)=fpf, where lcl, ucl, zAMD and pvalue are for amd. Here, empierical estimator of FPF is used, and amd$fpf could be different from the fpf value in the augument.}
\item{iamd}{data frame with auc, itpm, iftm, iamd, lcl, ucl, zIAMD, pvalue, where lcl, ucl, zIAMD and pvalue are for iamd.}
}

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
mzr.min=min(c(fit1$res$amd,fit2$res$amd))
mzr.max=max(c(fit1$res$amd,fit2$res$amd))

varoc(fit1,mzr="zAMD",mzr.min=mzr.min,mzr.max=mzr.max)
varoc(fit2,mzr="zAMD",mzr.min=mzr.min,mzr.max=mzr.max)

#2.3. varoc for marker 1 vs marker 2
min=min(c(x1,x2))
max=max(c(x1,x2))
jdp(fit1,min=min,max=max)
jdp(fit2,min=min,max=max)
}
