# Analysis of Page View response times

# Look at the frequency distribution of all durations:

hist(pages$duration, breaks=500, xlim=c(0,20))

# Plot the duration density:
plot(density(pages$duration), xlim=c(0, 15))

# Compare with lognormal distribution:
log_durations <- log(pages$duration)
gm <- mean(log_durations)
gsd <- sd(log_durations)
x <- seq(0, 15, by=0.1)
y <- dlnorm(x, meanlog=gm, sdlog=gsd)
lines(x=x,y=y, col='orange', lwd=4)

# For fun, transform to log space:
plot(density(log_durations),pty=5, xlim=c(0, 4))

# Add in a normal curve fit:
yn <- dnorm(x, mean=mean(log_durations), sd=sd(log_durations))
lines(x=x, y=yn, col='blue', lwd=4)

