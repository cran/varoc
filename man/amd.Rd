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
\details{The amd function summarizes a continuity performance of x at each cutoff c as: i) above mean difference (AMD), ii) intergrated AMD (IAMD), and iii) partial IAMD (PIAMD). For i), AMD(c) is true positive mean(TPM)(c) minus false positive mean(FPM)(c), where TPM(c) is E(x>c|y=1) and FPM(c) is E(x>c|y=0). For ii), the integreation of AMD over (-\eqn{\infty},\eqn{\infty}). For iii), PIAMD is an intergreation of AMD over (-\eqn{\infty},c) divided by fpf.

These measures can be viewed as continuous versions of ROC curve-based measures. Specifically, TPM(c) and FPM(c) are continuous versions of true positive fraction(TPF)(c) (or sensitivity(c)) and false positive fraction(FPF)(c) (or one minus specificity(c)), where TPF(c)=P(x>c|y=1) and FPF(c)=P(x>c|y=0). The useful (or useless) x has TPF(c)-FPF(c)>0 and AMD(c)>0 (or TPF(c)-FPF(c)=0 and AMD(c)=0), although both can be negative. Similiarly, useful (or useless) x has area under the ROC curve(AUC)>0.5 and IAMD(c)>0 (or AUC=0.5 and IAMD(c)=0) and/or (normalized) partial AUC(PAUC)>t/2 and PIAMD(c)>0 (or PAUC=t/2 or PIAMD(c)=0). Here, partial PAUC is AUC a FPF range of 0 to fpf divided by fpf. The bootstrap p-value and confidence interval are computed under the null hypthesis: AMD(c)\eqn{\leq}0, IAMD(c)\eqn{\leq}0, or PIAMD(c)\eqn{\leq}0, when pval="yes" and alterantive="greater".

The threshold c is determined by setting an acceptable fpf, i.e. FPF(c)=fpf. Thus, i) is interpreted as AMD at a FPF of fpf, ii) is interprted as IAMD (or average AMD) at a FPF range of 0 and 1, and iii) is interpreted as PIAMD (or average AMD) at a FPF of fpf or below. Thus, similar to AUC, ii) is a global measure. Similar to TPF and PAUC, i) and iii) are used when evaluating x at a specific range of FPFs is of interest. The varoc and jdp functions visualize them.
}

\value{
\item{df}{data frame with y and x.}
\item{fpf}{false positive fraction at which above mean difference is cacluated.}
\item{auc}{area under the ROC curve.}
\item{pauc}{normalized partial auc}
\item{res}{data frame with tpf, fpf, tpm, fpm, amd at each threshold (th))}
\item{amd}{data frame with tpf, fpf,  tpm, fpf, amd, lcl, ucl, z, pvalue at FPF(th)=fpf, where lcl, ucl, z, pvalue are lower confidence limit, upper confidence limit, teststatistics, and p-value for AMD(th) based on B bootstrap samples.}
\item{iamd}{data frame with tpf, fpf,  tpm, fpf, amd, lcl, ucl, z, pvalue at FPF(th)=fpf, where lcl, ucl, z, pvalue are lower confidence limit, upper confidence limit, teststatistics, and p-value for IAMD(th) based on B bootstrap samples.}
\item{piamd}{data frame with tpf, fpf,  tpm, fpf, amd, lcl, ucl, z, pvalue at FPF(th)=fpf, where lcl, ucl, z, pvalue are lower confidence limit, upper confidence limit, teststatistics, and p-value for PIAMD(th) based on B bootstrap samples.}
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
