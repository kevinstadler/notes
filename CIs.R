#'---
#'title: Confidence intervals & credible intervals
#'author: Kevin Stadler
#'output: pdf_document
#'classoptions: a4paper
#'bibliography: ../library.bib
#'---
#' @Hoekstra2014 show how nobody (not even statistics professors) know/understand what confidence intervals (CIs) are, how to interpret them or what they are supposed to tell you. Even if you know what they are, in the heat of reading a paper/glimpsing at a graph you are much more likely to interpret them intuitively as something they are not:
#'
#' is:
#'   ~ *if we were to repeat the experiment/data collection procedure* then the CIs of 95% of the samples will contain the true mean
#'
#' is not:
#'   ~ given the sample we obtained, the CI is the region that we can be 95% sure the true mean lies in 
#'
#' Confidence intervals are a *frequentist* concept, meaning that there is the assumption of one underlying true mean. From the frequentist viewpoint any particular CI you're looking at does either contain this (unknown) true mean or not. The CI is a 'tag' indicating the quality of your experimental procedure but, importantly, for any particular sample *the CI is not intended to indicate a region we think the true mean is likely to lie in*!
#'
#' If you're interested in knowing the range of likely values of the true mean based on your sample (the intuitive reading that most people go for), you are actually thinking of the [credible interval](http://en.wikipedia.org/wiki/Credible_interval) (aka 'Bayesian confidence interval') for that parameter. The difference between those two types of intervals isn't just philosophical, because the Bayesian credible interval will generally be *wider* than the confidence interval, so people's 'intuitive' reading of CI's is an underestimate of the measure that they think they're looking at!^[http://stats.stackexchange.com/questions/5903/confidence-intervals-for-regression-parameters-bayesian-vs-classical just how much wider the credible interval is is mostly dependent on the size of your sample. See the final section of this document for a comparison.]
#'
#' One conclusion from the paper is that this is yet another reason we should all go Bayes, but the obvious next question was: how do you (easily) calculate a credible interval? Let's try:

# Take a small normally distributed sample
x <- rnorm(10)

# Use a one sample t-test to get the confidence interval
t.test(x)

#' # Determining a credible interval for the mean of a sample
#' I first tried the [Bolstad](http://cran.r-project.org/web/packages/Bolstad/Bolstad.pdf) package, which has functions for inferring the most likely mean of a sample that's assumed to be normally distributed. Not making any assumptions about our sample, we use `normgcp` to infer the mean using a flat (uninformative) prior:

# the Bayesian inference occurs over a finite number of possible means. n.mu sets
# the number of means considered, which determines the resolution of the posterior
mu.mdl <- Bolstad::normgcp(x, n.mu=100)

#' But how do we go from the posterior distribution to a credible interval? A first (naive) attempt at getting at a credible interval for the mean by cutting 2.5% off either side:

limit <- sum(mu.mdl$posterior)*0.025
lower <- which(cumsum(mu.mdl$posterior)>=limit)[1]
upper <- length(mu.mdl$posterior) - which(cumsum(rev(mu.mdl$posterior))>=limit)[1]
# naive credible interval assuming fixed (known) sigma:
mu.mdl$mu[c(lower,upper)]

#' This interval is actually *smaller* than the confidence interval determined by the t-test above. What's going on? The issue can be seen in the output of `normgcp` above: the function only draws inferences about the *mean* of the distribution without considering the *standard deviation* which is simply assumed to be identical to the standard deviation of the sample. Known standard deviations can be passed as arguments to the function, but there is normally no reason to assume that we know the standard deviation. Really we will have to make inferences about the mean and standard deviation in tandem (and thus infer a credibility *region* within the distribution's multi-dimensional parameter space).
#'
#' # Bayesian inference over a (ostensibly) normally distributed sample
#' Here comes *BEST*: *Bayesian estimation supersedes the t test* [@Kruschke2013]:

library(BEST)
best.mdl <- BESTmcmc(x, verbose=FALSE)
as.matrix(hdi(best.mdl))

#' Not only does this procedure give us a wider credible interval on $\mu$ (as we expected), it also gives estimates for standard deviation and $\nu$, a measure of the sample distribution's normality.
#'
#' The `hdi` function stands for *highest density interval* - since the posterior can have any (non-symmetric) shape, there are many 95% "credible intervals", i.e. many ways to carve out a region that covers 95% of the probability mass. Rather than simply cutting off 2.5% on either end of the posterior, you normally want to select the 95% region which has a greater probability density than any part outside the region. The highest density region of the different parameters is marked by the black bars below^[NB it is not necessarily always the case that the posterior distribution is unimodal, particularly if you're fitting a complex model. In this case the highest density interval might actually be *discontinuous*, i.e. it might actually consist of two disconnected regions at different locations in the parameter space!]: <!-- http://www.stat.cmu.edu/~rsteorts/btheory2/ch4_slides.pdf -->

plotAll(best.mdl)

#' # Relation between the confidence and credible interval
#' The relative difference between the confidence intervals' and credible intervals' width depends strongly on the sample size, primarily because we cannot make any strong inferences about the true standard deviation of the underlying distribution from a small sample. Having to assume that the standard deviation could be very high means that very different settings of the mean don't actually affect the likelihood of the sample that much, leading to a flatter posterior distribution over possible mean values and consequently a wide credible interval.
#+ cache=TRUE
get.cis <- function(x) {
  best.mdl <- BESTmcmc(x, verbose=FALSE)
  cbind(ci=t.test(x)$conf.int, hdi(best.mdl)[,c("mu", "sigma")])
}

compare.cis <- function(x) {
  cis <- get.cis(x)
  cat("Confidence interval:               ", cis[,"ci"], "\n")
  cat("Highest-density credible interval: ", cis[,"mu"], "(sd", cis[,"sigma"], ")\n")
  cat("credible:confidence interval ratio: ", diff(cis[,"mu"]) / diff(cis[,"ci"]))
}

compare.cis(rnorm(4))
compare.cis(rnorm(5))
compare.cis(rnorm(8))
compare.cis(rnorm(10))
compare.cis(rnorm(20))

#' Below is a more exhaustive investigation of the credible:confidence interval ratio for different sample sizes, confirming that for $n=4$ the credible interval is consistently about 60% wider than the CI. The boxplots summarise the credible:confidence interval width ratios for different sample sizes obtained from 10 replications each.
#+ echo=FALSE, cache=TRUE
get.widths <- function(cis)
  c(ci=diff(cis[,"ci"]), mu=diff(cis[,"mu"]), sigma=diff(cis[,"sigma"]), ratio=diff(cis[,"mu"]) / diff(cis[,"ci"]))

ns <- c(4,5,8,10,20)
nsamples <- 10
manyintervals <- do.call(rbind, replicate(nsamples, lapply(ns, function(n) c(n=n, get.widths(get.cis(rnorm(n)))))))
boxplot(ratio.upper~n, data=manyintervals, ylim=c(1,max(manyintervals[,"ratio.upper"])), xlab="Sample size", ylab="Relative width of credible vs. confidence interval")
abline(h=1, lty=2)

#boxplot(mu.upper~n, data=manyintervals, ylim=c(0,max(manyintervals[,"mu.upper"])), xlab="Sample size", ylab="sample mean credible interval width")
#abline(h=0, lty=2)
#boxplot(ci.upper~n, data=manyintervals, ylim=c(0,max(manyintervals[,"ci.upper"])), xlab="Sample size", ylab="confidence interval width")
#abline(h=0, lty=2)
#boxplot(sigma.upper~n, data=manyintervals, ylim=c(0,max(manyintervals[,"sigma.upper"])), xlab="Sample size", ylab="sigma credible interval width")
#abline(h=0, lty=2)

#+ eval=FALSE, echo=FALSE, cache=TRUE
# # Quantifying CI performance
generate.cis <- function(samplefun, n=10) {
  cis <- array(dim=c(2,n,2))
  for (i in 1:n) {
    x <- samplefun()
    cis[1,i,] <- as.vector(t.test(x)$conf.int)
    cis[2,i,] <- hdi(BESTmcmc(x, verbose=FALSE))[,"mu"]
  }
  o <- order(cis[1,,2] - cis[1,,1])
  cis[1,,] <- cis[1,o,o]
}

#' # References