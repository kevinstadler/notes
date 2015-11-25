#'---
#'title: Page's test is not a trend test
#'author: Kevin Stadler
#'date: March 2015
#'output: pdf_document
#'classoptions: a4paper
#'bibliography: ~/library.bib
#'header-includes:
#'- \usepackage[table]{xcolor}
#'---

#' Page's "test for linear ranks" tests whether there is a linear ordering between $k$ conditions of a between-subjects design with $N$ replications. The predicted ordering of the conditions (or generations) has to be specified a-priori.
#' Let $m_i$ be the median rank of a score in condition or generation $i$, then the null hypothesis of the test (which is identical to the one of Friedman's test) is
#' $$ m_1 = m_2 = ... = m_k $$
#' i.e. there is no difference between the expected ranks for the $k$ conditions. @Page1963's original formulation of the *alternative* hypothesis being tested is 
#' $$m_1 > m_2 > ... > m_k.$$
#' Later papers and textbook entries correctly point out that the alternative hypothesis considered is actually
#' $$m_1 \le m_2 \le ... \le m_k$$
#' where *at least one* of the inequalities has to be a true inequality [@Siegel1988;@Hollander1999, p.284;@VanDeWiel2001, p.143]. <!-- @Sheshkin2004, IX., one that is actually wrong using '<' is @Rayner2000 also: "The scores will be different and will increase as the treatment levels change" - but not ALL are different -->


#' What this means is that even if there is only a single step-wise change in the mean rank, e.g.
#' $$m_1 < m_2 = ... = m_k$$
#' this is sufficient evidence *against* the null hypothesis.

#+ echo=FALSE
pagesL <- function(data, ties="average") {
  ranks <- apply(data, 1, function(r)rank(r, na.last="keep", ties.method=ties))
  sum(1:ncol(data) * rowSums(ranks))
}
# more docs to look at:
# http://www.tandfonline.com/doi/abs/10.1080/03610917708812026
# http://www.ncbi.nlm.nih.gov/pubmed/10513459
# https://ideas.repec.org/a/bla/scjsta/v33y2006i2p239-246.html

library(foreach)
pagesLmcmc <- function(L, k, N, trials=1000) {
  Ls <- numeric(trials)
  foreach (i=1:trials) %do% pagesL(t(replicate(N, sample(k))))
} 

normal.approx.p <- function(L, k, N) {
  if (N < 10)
    warning("Normal approximation is only reliable for N>10")
  #(12*L-3*N*k*(k+1)^2)/(k*(k^2-1)) * sqrt((k-1)/N)
  # Hollander & Wolfe (1999) formulation
  zs <- (L-(N*k*(k+1)^2)/4) / sqrt(N*k^2*(k+1)*(k^2-1)/144)
  sapply(zs, function(z)c(p=pnorm(z, lower.tail=FALSE), z=z))
  # the crank implementation is off, it looks like there is a dim[2]*^2* missing in the denominator?
  #zL<-((12*L-3*dimx[1]*dimx[2]*(dimx[2]+1)*(dimx[2]+1))/(dimx[2]*(dimx[2]-1)))*sqrt((dimx[2]-1)/dimx[1])
}
# these should be 0.001, 0.01 and 0.05, respectively
#normal.approx.p(c(917, 893, 869), 8, 5)

normal.approx.L <- function(k,N,plvl=c(0.05, 0.01, 0.001))
  qnorm(plvl, lower.tail=FALSE)*sqrt(N*k^2*(k+1)*(k^2-1)/144) + (N*k*(k+1)^2)/4

plotnormalapprox <- function(k,N,trials=3000) {
  Ls <- pagesLmcmc(L,k,N,trials)
  zLs <- suppressWarnings(sapply(Ls, function(L)normal.approx.p(L,k,N)))
  limit <- max(abs(range(zLs)))
  hist(zLs, breaks=seq(-limit, limit, length.out=30), freq=F)
  curve(dnorm, from=-limit, to=limit, add=T)
}

mcmcapprox <- function(L,k,N, trials=1000) {
  pagesLmcmc(L,k,N,trials)
}

chi2approx <- function(L,k,N,mc=1000) {
  if ((N <= 20 || k < 9) && (N <= 12 && k < 4))
    warning("Chi2 approximation is only reliable when N>=21 or when k>=9 or when (N>=13 && k>=4)")
  # from Siegel+Castellan, apparently
  (12*L-3*N*k*(k+1)^2)^2 / (N*k^2*(k^2-1)*(k+1))
}
# the chi2 approximation is actually identical to the normal approximation:
#x2.L <- chi2approx(L,k,N)
# chisq is two sided, so divide p value by two to get one-sided p
#p.x2 <- pchisq(x2.L, 1, lower.tail=FALSE)/2
# also, because it's two sided we have to invert the p value if the data is actually closer to a negative trend
#if (L < sum(1:k)*N*(k+1)/2) # benchmark L for an even or any other symmetric rank distribution
#  p.x2 <- 1 - p.x2
# at this point it is always true that p.x2 == pnorm(normalapprox(L,k,N), lower.tail=FALSE)

# p levels/N/k
Ltable <- array(NA, c(3,20,8))
# p-levels: 0.05, 0.01, 0.001
Ltable[,2:20,3] <- c(28,NA,NA,41,42,NA,54,55,56,66,68,70,79,81,83,91,93,96,104,106,109,116,119,121,128,131,134,141,144,147,153,156,160,165,169,172,178,181,185,190,194,197,202,206,210,215,218,223,227,231,235,239,243,248,251,256,260)
Ltable[,2:12,4] <- c(58,60,NA,84,87,89,111,114,117,137,141,145,163,167,172,189,193,198,214,220,225,240,246,252,266,272,278,292,298,305,317,324,331)
Ltable[,2:12,5] <- c(103,106,109,150,155,160,197,204,210,244,251,259,291,299,307,338,346,355,384,393,403,431,441,451,477,487,499,523,534,546,570,581,593)
Ltable[,2:12,6] <- c(166,173,178,244,252,260,321,331,341,397,409,420,474,486,499,550,563,577,625,640,655,701,717,733,777,793,811,852,869,888,928,946,965)
Ltable[,2:12,7] <- c(252,261,269,370,382,394,487,501,516,603,620,637,719,737,757,835,855,876,950,972,994,1065,1088,1113,1180,1205,1230,1295,1321,1348,1410,1437,1465)
Ltable[,2:12,8] <- c(362,376,388,532,549,567,701,722,743,869,893,917,1037,1063,1090,1204,1232,1262,1371,1401,1433,1537,1569,1603,1703,1736,1773,1868,1905,1943,2035,2072,2112)

# Tests for a monotonically increasing trend in the given within-subjects data.
# If you want to test for a decreasing trend, simply negate data, i.e. call:
# pages.test(-data) - it doesn't matter if your data was already in ranks, since
# all data is always ranked internally anyway.
# pages.test(matrix(c(.797,.873,.888,.923,.942,.956,
# .794,.772,.908,.982,.946,.913,
# .838,.801,.853,.951,.883,.837,
# .815,.801,.747,.859,.887,.902),nrow=4,byrow=TRUE))
pages.test <- function(data, ...) {
  k <- ncol(data) # 'n' in Page (1963)
  N <- nrow(data) # 'm' in Page (1963)
  if (N==1)
    stop("Only one sample/block, you should be running a Mann-Kendall test instead, e.g.: Kendall::MannKendall(data)")
  if (k<3)
    stop("Need at least 3 rank levels")
  # ranking is happening in here
  L <- pagesL(data, ...)
  # 1. lookup table
  if (k <= 8 && N <= 20 && (k == 3 || N <= 12)) {
    p.table <- match(TRUE, L >= Ltable[3:1,N,k])
    if (!is.na(p.table)) {
      p.table <- c(.001, .01, .05)[p.table]
    } else {
      p.table <- "NS"
    }
  } else {
    p.table <- NA
  }
  # 2. normal approx
  # should only do this for N>=10?
  approx <- suppressWarnings(normal.approx.p(L,k,N))
  result <- list(L=L, k=k, N=N, p.table=p.table, p.approx=approx[1], z.L=approx[2])
  class(result) <- "pages.test"
  return(result)
}

print.pages.test <- function(p) {
  if (p$p.approx <= 0.0005)
    approx <- p$p.approx
  else
    approx <- round(p$p.approx, digits=4)
  if (is.na(p$p.table))
    return(paste("Page's test: L=", p$L, ", k=", p$k, ", N=", p$N, ", p (approx) = ", approx, sep=""))
  else
    return(paste("Page's test: L=", p$L, ", k=", p$k, ", N=", p$N, ", p<=", p$p.table, ", p (approx) = ", approx, sep=""))
}

pseudorandomranks <- function(...) {
  unlist(lapply(list(...), function(p)if (length(p)>1) sample(p) else p))
}

#' # Mock datasets
#' To test the sensitivity of the test to single step-wise changes we can take a typical sample set of $N=4$ replications with $k=10$ levels each and fix the very first position to always be ranked top (or bottom), with all successive ranks being randomly shuffled, e.g.:
firstthenrandom <- function() pseudorandomranks(1, 2:10)
t(replicate(4, firstthenrandom()))

#+ echo=FALSE
sampleLs <- function(datafun, trials=1000) {
#  ps <- list("<=.001"=0,"<=.01"=0,"<=.05"=0,"NS"=0)
  ps <- list("0.001"=0, "0.01"=0, "0.05"=0, "NS"=0)
  for (i in 1:trials) {
    test <- pages.test(datafun())
    if (is.na(test$p.table)) {
      if (test$p.approx <= 0.001) {
        p <- "0.001"
      } else if (test$p.approx <= 0.01) {
        p <- "0.01"
      } else if (test$p.approx <= 0.05) {
        p <- "0.05"
      } else {
        p <- "NS"
      }
    } else {
      p <- as.character(test$p.table)
    }
    ps[[p]] <- ps[[p]] + 1
  }
  return(unlist(ps)/trials)
}

sampleps <- function(testfun, N, datafun, trials=1000)
  testfun(function(){t(replicate(N, datafun()))}, trials)

#' Generating 1000 datasets like the one above which really only exhibit a single point change in the distribution of median ranks, we get a significant result about half of the time:
sampleps(sampleLs, 4, firstthenrandom)
#' Using @Caldwell2008's 10x10 design and assuming only that the first generation fares worse than all the later ones:
sampleps(sampleLs, 10, firstthenrandom)
#' The influence is even stronger when the single change point gets closer to the middle of the ordered conditions. When we generate 1000 datasets where the first two ranks are always shuffled in the first two positions, followed by ranks 3-10 also shuffled randomly, we obtain the following distribution of p values:
sampleps(sampleLs, 4, function() pseudorandomranks(1:2, 3:10))

#' The test is so sensitive to evidence for a change in the suspected direction (even if it is just a single point-wise change) that even evidence for a consistent trend in the opposite direction will not make it change its mind, as can be seen in this data set where more than half of the pairwise signs indicate downwardness:
# upwards jump from the first three observations to the remaining 7, but the
# remaining 7 exhibit a consistent downwards trend
upwardsjumpdownardstrend <- function() pseudorandomranks(1:3, 10, 9, 8, 7, 6, 5, 4)
sampleps(sampleLs, 10, upwardsjumpdownardstrend)
updownup <- function() pseudorandomranks(3:4, 5:6, 1:2, 7:8)
sampleps(sampleLs, 10, updownup)

#+ echo=FALSE
# Also, because the statistic averages across all replications before looking for a trend, clear evidence for a trend in the opposite direction can be washed out completely.
#contradictorytrends <- rbind(1:8,1:8,1:8,1:8,1:8,1:8,8:1,8:1,8:1,8:1)
#print(pages.test(contradictorytrends))
#d <- read.csv("pageC.csv")
#inc <- matrix(d$inc, nrow=4)
#flat <- matrix(d$flat, nrow=4)
#print(pages.test(inc))
#seasonal.mann.kendall.test(inc)
#print(pages.test(flat))
#seasonal.mann.kendall.test(flat)
# The impact of a single change point also become more pronounced when there are fewer conditions/generations or more replications/chains. In 1000 random datasets with N=4 and k=8 where only the first rank is fixed, the rest shuffled (the p values here are based on the exact tables rather than the approximation above):
# echo=FALSE
#sampleps(sampleLs, 4, function() pseudorandomranks(1, 2:8))

#' Based on these results it is difficult to argue that Page's test is a 'trend' test in the same sense as a linear trend test would be, since it does not "explicitly test for successive improvement" [@Caldwell2008] or for "cumulative decrease" [@Verhoef2014, p.] -- unless one wants to let a single point increase qualify as a 'trend' or 'cumulative'. It also cannot show "[an] increase to be significant" [@Mesoudi2011page, p.338] since it only considers *ranks*, not increases, and can merely check for the consistency of rank orderings across replications. <!-- "significantly more dispersed towards the end of the chains" (Verhoef, p.66), and Page's test is also *not* "a non-parametric (i.e. rank-based) repeated-measure test that specifically looks for trends over time" [@Bauer2015, p.3]. -->

#+ echo=FALSE
# data ogled off figure 10 - first the solid line, then the dashed one
#verhoef <- matrix(c(c(9.7, 1.4, 1.2, 2.3, 0.7, 1.6, 1.8, 1.2, 0.6, 2.1, 2.1), c(9.7, 3.2, 1.0, 1.1, 1.8, 1.9, 2.8, 0.3, 0.9, 1.3, 1.9)), ncol=11, byrow=T)
#pages.test(-verhoef) # negate for decreasing trend
#seasonal.mann.kendall.test(-verhoef)

# ## Looking for evidence for a single change point
# the single changepoint model is only a good model if the dashed line runs through or above most medians, not below it
# 
findmedianchangepoint <- function(data, trials=500) {
  L <- pagesL(data)
  k <- ncol(data)
  Ls <- matrix(nrow=trials, ncol=k-1)
  for (i in 1:(k-1)) {
    Ls[,i] <- replicate(trials, pagesL(t(apply(data,1,function(r)pseudorandomranks(r[1:i], r[(i+1):k])))))
  }
  #print(paste("Median/mean/sd of L randomised before and after", i, "items:", median(Ls), mean(Ls), range(Ls), collapse=" "))
  boxplot(L ~ cutoff, data=data.frame(cutoff=rep(1:(k-1), each=trials), L=as.vector(Ls)), xlab="median change point", ylab="L")
  abline(h=L, lty=2)
}
# Unsurprisingly, for this the breakoff point will be after the first index
#findmedianchangepoint(t(replicate(4, pseudorandomranks(1,2:10))))

# meaning it's really the 3-to-4 change that is driving the effect?
#findbreakoffpoint(ind)
# but you could just run it as a 4x15 instead of 4x5 design. It's true that more
# between-group combinations should be considered in this case, but as a first
# attempt:
# L=409, supersignificant according to the approximation!
#crank::page.trend.test(matrix(group$Value, ncol=4, byrow=T), rank=FALSE)
#pages.test(matrix(group$Value, ncol=4, byrow=T))$L

psplot <- function(ps) {
  barplot(unlist(ps)/sum(unlist(ps)), c(0.001, 0.0099, 0.049, 0.95), log="x")
#  plot(c(0.001, 0.01, 0.05, 1), cumsum(unlist(ps)), type="l", log=T)
}

#' ## Lack of standard implementations
#' There is no implementation of Page's L in standard software packages such as SPSS (let alone an implementation of the significance test). The R implementation in the `crank` package that's most easily accessible seems to have broken $\chi^2$ as well as normal approximations (the internal calculation is missing a $^2$ somewhere).
#+ echo=FALSE
#foo <- t(replicate(4, sample(11)))
#unlist(pages.test(foo))
#page.trend.test(foo)
# test that crank approx implementation is mental
#foo <- t(replicate(4, sample(10)))
#page.trend.test(foo, ranks=FALSE)[c("L", "p.table")]
#foo <- t(replicate(4, sample(11)))
#page.trend.test(foo)[c("L", "Z", "pZ", "x2L", "px2")]

#' This hinders the reproducibility of the results, particularly in the presence of tied ranks, in which case the standard approximation for the calculation of p values is different [@Rayner2000, p.131]. But even the calculation of $L$ itself can vary, for example @Kempe2014 report $L=123$ for the 'individual' condition ($k=4, N=5$), while three different implementations used by us produced $L=125.5$. This is because their implementation appears to be taking the minimum of the rank for both scores, which leads to inflated p values. The recommended behaviour is to use average ranks for ties [@Sheshkin2004] or resolve them randomly [@Page1963]. (A modified Page's $L$ that takes ties into account is shown in @Thas2012.)

alldata <- read.csv("page-test-KM2014.csv", header=T)
ind <- matrix(subset(alldata, Condition=="Individual")$Value, ncol=4, byrow=T)
pagesL(ind)
# use the minimum rank for two tied values rather than the recommended average:
pagesL(ind, ties="min")

#' For more information on how to interpret the Page's test alternative hypothesis vs. those of related but less directional tests (Friedman's, Anderson's and Pearson's), see @Rayner2000 or @Thas2012.
#+ echo=FALSE
# Particularly insightful is their relating of the different tests' $\chi^2$ approximations to the dimension of the parameter space specified by the alternative hypothesis (p.122). Given $k>2$ treatments (or generations), the degrees of freedom of the corresponding $\chi^2$ approximations are $s!-1$ for Pearson, $(s-1)^2)$ for Anderson, $s-1$ for Friedman and for $1$ Page's test, with the alternative hypothesis parameter spaces being roughly nested. Page's test is highly directional, Friedman's more omnibus, and so on with decreasingly directional and increasingly omnibus.

#' # Correction of the original table from [@Page1963]
#' A new implementation of Page's test (as part of the \texttt{cultevo} R package, to be submitted to CRAN and currently available from \url{https://github.com/kevinstadler/cultevo}) that provides exact p-values for a large range of $N, k$ revealed that a few of the critical values from @Page1963's original table were inaccurate. The following table shows the difference between the critical values in the original table and the re-calculated ones. 
#' \begin{center}\begin{tabular}{clcccccccc} & $k=$ & 3 & 4 & 5 & 6 & 7 & 8 & 9 & 10\\N & p level\\
#+ echo=FALSE, warning=FALSE, results="asis"
ks <- 3:10
Ns <- 2:7
quality <- array(NA, c(3, max(Ns), max(ks)))
etable <- array(NA, c(3, max(Ns), max(ks)))
Nrange <- min(max(Ns), dim(Ltable)[2])
krange <- min(max(ks), dim(Ltable)[3])
etable[, 1:Nrange, 1:krange] <- Ltable[, 1:Nrange, 1:krange]
etable[, 2:5, 9] <- c(500,520,544,736,761,790,971,999,1032,1204,1236,1273)
etable[, 2:5, 10] <- c(670,696,726,987,1019,1056,1301,1339,1382,1614,1656,1704)
for (k in ks) {
  for (N in Ns) {
    f <- paste("tab/page-N", N, "k", k, ".csv", sep="")
    if (!file.exists(f))
      next
    f <- read.csv(f)
    l5 <- f[which(f$p>0.05)-1,"X"]
    l1 <- f[which(f$p>0.01)[[1]]-1,"X"]
    l01 <- f[which(f$p>0.001)[[1]]-1,"X"]
    quality[,N,k] <- etable[,N,k]-c(l5, l1, l01)
  }
}
cellcolor <- function(err) {
  if (is.na(err))
    "white"
  else if (err <= -0.1)
    "red"
  else if (err <= -0.05)
    "pink"
  else if (err >= 0.1)
    "green"
  else if (err >= 0.05)
    "lime"
  else
    "white"
}

rows <- apply(quality[,Ns,ks], c(1,2), function(ps)paste(ifelse(is.na(ps), "", paste("\\cellcolor{", sapply(ps, cellcolor), "}", round(ps,digits=3), sep="")), collapse=" & "))
#pnorm(normalapprox(c(388, 376, 362), 8, 2), lower.tail=FALSE)
cat(paste(sapply(Ns, function(x)c(x,"","")), rep(c(.05, .01, .001), times=length(Ns)), rows, sep="&", collapse="\\\\"))

#' \end{tabular}\end{center}
#'
#' # Reliability of the normal/$\chi^2$ approximation
#' The following table shows the difference between the critical $L$ values calculated from the normal approximation to the tabulated ones from Page's original paper. Red cells highlight cells where the approximated critical L is lower than the real L, green cells indicate cells in which the approximate critical L is too strict (i.e. a conservative estimate). The approximation is said to be good for high k (particularly $k>10$.
#' \begin{center}\begin{tabular}{clcccccc} & $k=$ & 3 & 4 & 5 & 6 & 7 & 8\\N & p level\\
#+ echo=FALSE, cache=TRUE, results="asis"
#apply(Ltable, c(2,3), function(ps)suppressWarnings(pnorm(normalapprox(L,k,N), lower.tail=F)-p)/p, c(0.05, 0.01, 0.001), Ltable[,N,k])
ks <- 3:8
Ns <- 2:12
quality <- array(NA, c(3,max(Ns),max(ks)))
# deviation of the p values reported for the critical L values from the table - this calculation is OFF!
#for (k in ks) {
#  for (N in Ns) {
#    quality[,N,k] <- suppressWarnings(normal.approx.p(Ltable[,N,k],k,N)["p",]-c(0.05, 0.01, 0.001))/c(0.05, 0.01, 0.001))
#  }
#}

for (k in ks) {
  for (N in Ns) {
    quality[,N,k] <- suppressWarnings(ceiling(normal.approx.L(k,N))-Ltable[,N,k])
  }
}

rows <- apply(quality[,Ns,ks], c(1,2), function(ps)paste(ifelse(is.na(ps), "", paste("\\cellcolor{", sapply(ps, cellcolor), "}", round(ps,digits=3), sep="")), collapse=" & "))
#pnorm(normalapprox(c(388, 376, 362), 8, 2), lower.tail=FALSE)
cat(paste(sapply(Ns, function(x)c(x,"","")), rep(c(.05, .01, .001), times=length(Ns)), rows, sep="&", collapse="\\\\"))

# in terms of calculating the exact p values (or rather the critical L values) for any N,k:
# there's factorial(k) possible rank orderings (not including orderings that have ties)
# there are hence ( factorial(k) over N ) possible empirical outcomes.
# in cultevo experiments k > N, so can be rewritten as:
# k! * (k!-1) * ... * (k!-N+1) / N! - this is basically only tractable until k=6, and only for small N
# in shorther chains but with more subjects this is TODO fix this

#' \end{tabular}\end{center}

#+ echo=FALSE
# TODO is there a way to figure out which of the L values in the table is actually EXACT, which ones already <.05?
# ## $\chi^2$ approximation
# \begin{table}[h!]\centering\begin{tabular}{clcccccc} & $k=$ & 3 & 4 & 5 & 6 & 7 & 8\\N & p level\\
# echo=FALSE, cache=TRUE, results="asis"
# quality <- array(NA, c(3,12,8))
# for (k in 3:8) {
#   for (N in 2:12) {
#     quality[,N,k] <- suppressWarnings((pchisq(chi2approx(Ltable[,N,k],k,N), 1, lower.tail=FALSE)/2-c(0.05, 0.01, 0.001))/c(0.05, 0.01, 0.001))
#   }
# }
# rows <- apply(quality[,2:12,3:8], c(1,2), function(ps)paste(ifelse(is.na(ps), "", paste("\\cellcolor{", sapply(ps, cellcolor), "}", round(ps,digits=3), sep="")), collapse=" & "))
# cat(paste(sapply(2:12, function(x)c(x,"","")), rep(c(.05, .01, .001), times=11), rows, sep="&", collapse="\\\\"))
# \end{tabular}\end{table}
#'
#' # Alternatives to Page's test
#+ echo=FALSE
# http://www.r-bloggers.com/beware-the-friedman-test/ criticises Friedman's
# test for throwing within-subjects difference sizes away, suggesting the use of rank transformations instead?
# Ordinal regression does not seem to consider within-subjects designs, but maybe [@Wang2006rank]?
# it would have to be conditional regression or generalised estimating equations
# http://stats.stackexchange.com/questions/65548/which-model-should-i-use-to-fit-my-data-ordinal-and-non-ordinal-not-normal-an
# http://www.kovcomp.co.uk/support/XL-Tut/trend-test-mann-kendall.html
#' The seasonal Kendall test [@Hirsch1982;@Gilbert1987;@Gibbons2009] takes seasonal effects on environmental measurements into account by computing the Mann Kendall test on each of $k$ seasons/months separately, and then combining the individual test results. Since the order of the individual seasons is not actually taken into account (it only is in a later version of the test, @Hirsch1984), the test is essentially a within-subject version that combines the results of $k$ independent Mann-Kendall tests into one to increase the statistical power [@Gibbons2009, p.211]. The test was in fact already transferred to test for trends in different geographic sample locations rather than seasons (see @Helsel2006). The seasonal's test alternative hypothesis is "a monotone trend in one or more seasons" [@Hirsch1984, p.728].
#'
#' ## Comparison on the same datasets as above
#' First the empirical datasets from @Kempe2014, first the (non-significant) individual condition vs. the group condition:
#+ echo=FALSE
# The Mann-Kendall test also yields a slope estimate Q, the estimate of the seasonal test is simply the median of the individual seasons' slopes.
# http://www.ag.unr.edu/gf/pdf/joyce.pdf
# http://www.statsdirect.com/help/default.htm#nonparametric_methods/kendall_correlation.htm
# weird version of it that controls for autocorrelated data?? http://www.sciencedirect.com/science/article/pii/S002216949700125X
# TODO produce warning if ncol(data) not in [4,10]? (the underlying function will cry anyway if k==3)
# TODO implement test with 1-sided and 2-sided p values
seasonal.mann.kendall.test <- function(data)
  Kendall::SeasonalMannKendall(ts(as.vector(data), frequency=nrow(data)))

#n <- 100
#foo <- sample(n)
#mk.test(foo)
#Kendall::MannKendall(foo)
#pnorm(mk.test(foo), sd=sqrt(2*(2*n+5)/(9*n*(n-1))))*2

mk.test <- function(x) {
  ps <- combn(x, 2)
  2*sum(sign(ps[2,]-ps[1,]))/(length(x)*(length(x)-1))
}

# more Kempe data
group <- aggregate(Value ~ Generation + Chain, alldata, subset=Condition=="Group", mean)
groupavg <- matrix(group$Value, ncol=4, byrow=T)

pages.test(ind)
seasonal.mann.kendall.test(ind)

pages.test(groupavg)
seasonal.mann.kendall.test(groupavg)

#foo <- t(replicate(4,pseudorandomranks(1,2:10)))
#pages.test(foo)
#seasonal.mann.kendall.test(foo)
#pages.test(contradictorytrends)
#seasonal.mann.kendall.test(contradictorytrends)
sampleTaus <- function(datafun, trials=1000) {
  ps <- list("0.001"=0,"0.01"=0,"0.05"=0,"NS"=0)
  for (i in 1:trials) {
    test <- seasonal.mann.kendall.test(datafun())
    if (test$sl <= 0.001) {
      p <- "0.001"
    } else if (test$sl <= 0.01) {
      p <- "0.01"
    } else if (test$sl <= 0.05) {
      p <- "0.05"
    } else {
      p <- "NS"
    }
    ps[[p]] <- ps[[p]] + 1
  }
  return(unlist(ps)/trials)  
}

#' Next, compare Page and Seasonal Mann-Kendall on the dummy datasets we used above
#+ pagevsmannkendall, cache=TRUE
sampleps(sampleLs, 4, firstthenrandom)
sampleps(sampleTaus, 4, firstthenrandom)

# Caldwell
sampleps(sampleLs, 10, firstthenrandom)
sampleps(sampleTaus, 10, firstthenrandom)

sampleps(sampleLs, 4, function() pseudorandomranks(1:2, 3:10))
sampleps(sampleTaus, 4, function() pseudorandomranks(1:2, 3:10))

sampleps(sampleLs, 4, upwardsjumpdownardstrend)
sampleps(sampleTaus, 4, upwardsjumpdownardstrend)

# pseudorandomranks(3:4, 5:6, 1:2, 7:8)
sampleps(sampleLs, 4, updownup)
sampleps(sampleTaus, 4, updownup)

#' ## Simulation study comparison between Page's test and seasonal Mann-Kendall
#' Below are the results of a simulation study that manipulates the spacing of otherwise normally distributed samples (via the $\delta$ parameter) as done in @Rayner2000. It should be noted that the p values for the Mann-Kendall test are those for the two-sided hypothesis, so its sensitivity in the table is underestimated.
#' \begin{table}[h!]\centering\begin{tabular}{lclllll}
#+ simulationstudy, echo=FALSE, cache=TRUE, results="asis"
simulatedps <- function(means, n, trials=1000) {
  lsig <- 0
  mksig <- 0
  for (i in 1:trials) {
    data <- sapply(means, function(m)rnorm(n, m))
    p <- pages.test(data)
    if (is.na(p$p.table)) {
      if (p$p.approx <= 0.05) {
        lsig <- lsig+1
      }
    } else if (is.numeric(p$p.table)) {
      lsig <- lsig+1
    }
    if (seasonal.mann.kendall.test(data)$sl <= 0.05) {
      mksig <- mksig+1
    }
  }
  c(lsig, mksig)/trials
}

simulationstudy <- function(divisors, ns = c(5,10), deltas = c(0, 0.5, 1, 1.5, 2)) {
  cat("\\multicolumn{2}{l}{$(", paste("T", 1:length(divisors), sep="_", collapse=", "), ")=(", ifelse(divisors==0, "0, ", paste(ifelse(divisors<0, "-", ""), "\\delta", ifelse(abs(divisors)==1, ", ", paste("/", abs(divisors), ", ", sep="")), sep="")), ")$} & ", paste("$\\delta=", deltas, "$", sep="", collapse="&"), "\\\\", sep="")
  for (n in ns) {
    res <- sapply(deltas, function(delta)simulatedps(ifelse(divisors==0, 0, delta/divisors), n))
    cat("$n =", n, "$ & L & ")
    cat(res[1,], sep="&")
    cat("\\\\ & $\\tau$ & ")
    cat(res[2,], sep="&")
    cat("\\\\\\hline")
  }
  cat("\\hline")
  return(res)
}

x <- simulationstudy(c(0, 1, 1, 1))
x <- simulationstudy(c(0, 0, 1, 1))
x <- simulationstudy(c(-1, 0, 0, 0, 0))
x <- simulationstudy(c(-1, 0, 1, 1, 1))

#' \end{tabular}
#' \caption{Simulation study showing the sensitivity of Page's test vs. the seasonal Mann-Kendall test for datasets with underlying 1 and 2-point changes in their underlying ranks.}\end{table}
#' # References
