#'---
#'title: Number of distinguishable levels in a bounded channel
#'author: Kevin Stadler
#'output: pdf_document
#'classoptions: a4paper,fullpage
#'---

#' The channel capacity of a channel with input signals $X$ and resulting (noise-added) transmission $Y$ is
#' $$C=\sup_{p_X} I(X;Y)$$
#' i.e. the maximum amount of mutual information between some theoretical (idealised) signal distribution $p_X$ and its noise-overlaid empirical counterpart. First, let's define functions to compute the transmission distribution with (truncated) Gaussian noise from a theoretical signal distribution, as well as the mutual information between the two:

entropy <- function(x) {
  x <- x/sum(x)
  -sum(sapply(which(x>0), function(i)x[i]*log2(x[i])))
}

# calculate the distribution of transmitted signals, as produced by gaussian
# noise with given sigma overlaid on the signals. the width of the bounded
# channel is assumed to be 1, and the given signal probability distribution
# vector signaldist specifies the signal distribution over equally spaced
# points in the interval [0,1]. the length of signaldist determines the
# resolution with which the empirical distributions will be computed, so stuff
# plenty of 0s between non-zero values to increase the accuracy of the
# calculation. the third argument specifies the effect of the noise and can be
# one of "bunch", "drop" or "wrap" (the latter unimplemented)
bounded.channel.transmissiondist <- function(signaldist, sigma, overflow="bunch") {
  # calculate absolute 'positions' of the specified signaldist in [0,1]
  resolution <- 1/(length(signaldist)-1)
  pts <- seq(0, 1, resolution)
  bins <- c(-resolution/2, pts+resolution/2)

  signalindices <- which(signaldist>0)
  # for every signal, compute its distribution after the application of the truncated noise
  jointdist <- sapply(signalindices, function(index) diff(pnorm(bins, mean=pts[index], sd=sigma)))
  # for every signal, compute the proportion of transmissions outwith the bound
  underrun <- pnorm(bins[1], mean=pts[signalindices], sd=sigma)
  overrun <- pnorm(bins[length(bins)], mean=pts[signalindices], sd=sigma, lower.tail=FALSE)
  if (overflow == "bunch") {
    # add signals bunched up against the boundaries to the extreme ends
    jointdist[1,] <- jointdist[1,] + underrun
    jointdist[nrow(jointdist),] <- jointdist[nrow(jointdist),] + overrun
    # colSums(jointdist) should be 1 now
  }
  # weight signal distributions according to signal probability
  return(signaldist[signalindices] * jointdist)
}

# create a signal distribution with nsignals equally spaced signals
equispaced.signaldist <- function(nsignals, spaces=20)
  c(rep(c(1,rep(0, spaces)), nsignals-1), 1)

# show theoretical and empirical distribution of two signals at opposite ends
# of the spectrum overlaid with noise at sigma=0.2
par(mfrow=c(1,2))
plot(equispaced.signaldist(2)/2, yaxs="i", pch=4, main="Theoretical signal distribution", ylab="p(x)")
plot(rowSums(bounded.channel.transmissiondist(equispaced.signaldist(2), 0.2))/2, yaxs="i", pch=4, main="Signal distribution after noise", ylab="p(y)")

#+ echo=FALSE,eval=FALSE
# Placing two signals at opposing ends of the spectrum:
for (sigma in c(0.05, 0.1, 0.5))
  cat("with sigma", sigma, ", C=", sapply(0:5, function(spacing) bounded.channel.mi(sigma, c(1, rep(0, spacing), 1))), "\n")
# Placing *three* signals at opposing ends of the spectrum:
for (sigma in c(0.05, 0.1, 0.5))
  cat("with sigma", sigma, ", C=", sapply(0:5, function(spacing) bounded.channel.mi(sigma, c(1, rep(0, spacing), 1, rep(0, spacing), 1))), "\n")
# Placing *four* signals at opposing ends of the spectrum:
for (sigma in c(0.05, 0.1, 0.5))
  cat("with sigma", sigma, ", C=", sapply(0:5, function(spacing) bounded.channel.mi(sigma, c(1, rep(0, spacing), 1, rep(0, spacing), 1, rep(0, spacing), 1))), "\n")

# iteratively try to determine C = sup(I(X;Y)) over all possible choices of
# increasingly expressive signalling systems P(X)
bounded.channel.capacity <- function(sigma, resolution=20) {
  c <- -1
  nextc <- 0
  i <- 0
  while (nextc > c) {
    i <- i+1
    c <- nextc
    nextc <- bounded.channel.mi(sigma, c(rep(c(1,rep(0, resolution)), i), 1))
    cat(i+1, "signals, capacity", nextc, "\n")
  }
  return(c(nsignals=i, capacity=c))
}

#+ echo=TRUE
# calculate the mutual information between the distribution of signals and the
# distribution of actual transmissions y (as computed by the function above):
# I(x,y) = H(x) + H(y) - H(x,y)
bounded.channel.mi <- function(signaldist, ...) {
  jointdist <- bounded.channel.transmissiondist(signaldist, ...)
  return(entropy(signaldist) +
    entropy(rowSums(jointdist)) - 
    entropy(jointdist))
}

#' ## Channel capacity for different sigmas and number of signals
#' The slightly paradoxical thing about the information-theoretic definition of the channel capacity is that it depends on the signal distribution $p_X$, which already kinda specifies how many different signals we are trying to send (i.e. how many effective levels of the signal space we want to use). The graph below shows at which number of maximally spread equally spaced signals the mutual information maxes out for a given sigma -- assuming that there are no other signal distributions which could yield higher values (I can't think of one?), this maximum indicates the channel capacity. Note that the channel capacity is always significantly lower than the number of bits that could theoretically be transferred with the given number of signals (if they were reliably distinguishable), I think this shows that, generally, the relationship between the channel capacity and the number of effective levels is non-trivial, and that the channel capacity measure is only useful in the context of error-correcting codes etc.

#+ echo=FALSE
sigmas <- c(0.025, 0.05, 0.1, 0.2, 0.4)
conditions <- expand.grid(sigma=sigmas, nsignals=2:10)

conditions$mutualinformation <- apply(as.matrix(conditions), 1, function(row) bounded.channel.mi(equispaced.signaldist(row["nsignals"]), row["sigma"]))
plot(mutualinformation ~ nsignals, data=conditions, pch=4, col=rainbow(length(sigmas))[as.numeric(as.factor(conditions$sigma))])
legend("topleft", paste("sigma", sigmas), fill=rainbow(length(sigmas)))

#' The next graph shows an arguably more useful measure for the question of effective levels, namely the mutual information's sensitivity to sigma depending on the number of signals/distinct categories used -- aka at what point do I effectively stop caring about a further decrease in sigma if the number of distinct levels/signals that I want to communicate is fixed:

#+ echo=FALSE
nsignals <- 2:8
conditions2 <- expand.grid(nsignals=nsignals, sigma=seq(0.005, 0.2, 0.005))

conditions2$mutualinformation <- apply(as.matrix(conditions2), 1, function(row) bounded.channel.mi(equispaced.signaldist(row["nsignals"]), row["sigma"]))
lattice::xyplot(mutualinformation ~ sigma, xlim=c(0, max(conditions2$sigma)), data=conditions2, type="l", group=conditions2$nsignals, auto.key=list(text=paste(nsignals, "signals"), points=FALSE, columns=4, lines=TRUE))

# connecting the two graphs via Hartley's law - doesn't seem to work
#cbind(sigma=sigmas, M=sapply(sigmas, function(n) sqrt(max(subset(conditions, sigmas==n)$mutualinformation))))

#+ eval=FALSE, echo=FALSE
# Finally, plots showing the development of the transmitted signal distributions (represented by the vertical slices through the contour plots) and how they change with increasing sigma, for signalling systems with 2, 3 and 4 signals respectively. The (not perfectly aligned, my bad) line indicates the proportion of mutual information between the underlying signalling system and transmitted signal that is maintained with the given level of sigma, showing how the mutual information (and thus channel capacity) decrease as soon as the individual signals' distributions start to overlap - the shape of the lines is the same as in the graph above.

sigmas <- seq(0, 1, 0.01)
nlevels=100
jointdists <- sapply(sigmas, function(sigma) rowSums(bounded.channel.transmissiondist(equispaced.signaldist(2), sigma)))/2
filled.contour(t(jointdists), xlab="sigma", xlim=c(0, max(sigmas)), ylab="signal", levels=max(jointdists)*(1-log2(seq(2,1,-1/nlevels))), col=gray(1-log2(1+0:(nlevels-2)/(nlevels-2))))
lines(sigmas, sapply(sigmas, function(sigma) bounded.channel.mi(equispaced.signaldist(2), sigma)), lty=2)

jointdists <- sapply(sigmas, function(sigma) rowSums(bounded.channel.transmissiondist(equispaced.signaldist(3), sigma)))/3
filled.contour(t(jointdists), xlab="sigma", xlim=c(0, max(sigmas)), ylab="signal", levels=max(jointdists)*(1-log2(seq(2,1,-1/nlevels))), col=gray(1-log2(1+0:(nlevels-2)/(nlevels-2))))
lines(sigmas, sapply(sigmas, function(sigma) bounded.channel.mi(equispaced.signaldist(3), sigma))/log2(3), lty=2)

jointdists <- sapply(sigmas, function(sigma) rowSums(bounded.channel.transmissiondist(equispaced.signaldist(4), sigma)))/4
filled.contour(t(jointdists), xlab="sigma", xlim=c(0, max(sigmas)), ylab="signal", levels=max(jointdists)*(1-log2(seq(2,1,-1/nlevels))), col=gray(1-log2(1+0:(nlevels-2)/(nlevels-2))))
lines(sigmas, sapply(sigmas, function(sigma) bounded.channel.mi(equispaced.signaldist(4), sigma))/2, lty=2)

