library(rstudioapi) # to automatically set the working directory to this file's path.
library(HDInterval) # to make HDP credible intervals
library(extraDistr) # contains beta-binomial distribution

#set the working directory to this file's path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##############################################################
################# Exercise 1 b) Plots ########################
##############################################################

# posterior = Beta(51,29)
# prior = Beta(1,1)
# likelihood = Beta(51,29) 
alpha = 51
beta  = 29

colors <- c("red", "blue")
labels = c('posterior & likelihood','prior')
# ploting the posterior distribution

x <- seq(0, 1, length=1000)
posterior <- dbeta(x,shape1=alpha,shape2=beta)
likelihood <- dbeta(x,shape1=alpha,shape2=beta)

png(filename="media/pri_lik_pos.png")
plot(x, posterior, type='n',lty=2, xlab="theta",
     ylab="Density", main="likelihood vs posterior vs prior")
lines(x,posterior,col=colors[1])

# plotting the prior

prior = dbeta(x,shape1=1,shape2=1)
lines(x,prior,col=colors[2])

# plotting the likelihood
lines(x,likelihood,col=colors[3])

legend("topleft", inset=.05,
       labels, lwd=2, col=colors)
dev.off()


##############################################################
################# Exercise 1 b) Credible interval ############
##############################################################

# CENTERED CREDIBLE INTERVAL
left_quantile  = qbeta(0.025, shape1=alpha, shape2=beta); round(left_quantile,3);
right_quantile = qbeta(0.975, shape1=alpha, shape2=beta); round(right_quantile,3);

# HPD INTERVAL
CI = hdi(qbeta, 0.95, shape1=alpha,shape2=beta); round(CI['lower'],3); round(CI['upper'],3)

##############################################################
################# Exercise 1 c) P(theta > 0.5) ############
##############################################################

proba = 1 - pbeta(0.5,shape1=alpha,shape2=beta);  proba

#####################################################################################
################# Exercise 1 d) plotting jefreeys prior vs uniform prior ############
#####################################################################################


colors <- c("red", "blue")
labels = c('Beta(1/2,1/2)','Beta(1,1)')
# ploting the posterior distribution

x <- seq(0, 1, length=1000)
uniform <- dbeta(x,shape1=1,shape2=1)
jeffreys <- dbeta(x,shape1=0.5,shape2=0.5)

png(filename="media/jeffreysVsUniform.png")
plot(x, posterior, type='n',lty=2, xlab="theta",
     ylab="Density", main="Jefreeys prior Vs Uniform prior")

# plotting the jeffreys
lines(x,jeffreys,col=colors[1])

# plotting the uniform
lines(x,uniform,col=colors[2])

legend("topleft", inset=.05,
       labels, lwd=2, col=colors)
dev.off()

#####################################################################################
################# Exercise 1 d) Credible intervals ##################################
#####################################################################################

new_alpha = 50.5
new_beta = 28.5
# CENTERED CREDIBLE INTERVAL
left_quantile  = qbeta(0.025, shape1=new_alpha, shape2=new_beta); round(left_quantile,3);
right_quantile = qbeta(0.975, shape1=new_alpha, shape2=new_beta); round(right_quantile,3);

# HPD INTERVAL
CI = hdi(qbeta, 0.95, shape1=new_alpha,shape2=new_beta); round(CI['lower'],3); round(CI['upper'],3)


##############################################################
################# Exercise 1 d) P(theta > 0.5) ############
##############################################################

proba = 1 - pbeta(0.5,shape1=new_alpha,shape2=new_beta);  proba

##############################################################
################# Exercise 1 d) Plots ########################
##############################################################

# posterior = Beta(50.5,28.5)
# prior = Beta(0.5,0.5)
# likelihood = Beta(51,29) 

colors <- c("red", "blue", 'green')
labels = c('posterior','prior','likelihood')
# ploting the posterior distribution

x <- seq(0, 1, length=1000)
posterior <- dbeta(x,shape1=50.5,shape2=28.5)
likelihood <- dbeta(x,shape1=51,shape2=29)

png(filename="media/Jeffreys_pri_lik_pos.png")
plot(x, posterior, type='n',lty=2, xlab="theta",
     ylab="Density", main="likelihood vs posterior vs prior")
lines(x,posterior,col=colors[1])

# plotting the prior

prior = dbeta(x,shape1=0.5,shape2=0.5)
lines(x,prior,col=colors[2])

# plotting the likelihood
lines(x,likelihood,col=colors[3])

legend("topleft", inset=.05,
       labels, lwd=2, col=colors)
dev.off()

##############################################################
################# Exercise 1 e) Beta-binomial ########################
##############################################################
# Calculating the probability that at least 6 pairs experience improvement out of 10
n = 10
alpha = 51
beta = 29
proba = 1 - pbbinom(5, n, alpha = alpha, beta = beta, lower.tail = TRUE,log.p = FALSE); proba