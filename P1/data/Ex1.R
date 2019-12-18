rm(list=ls())
library(rstudioapi) # to automatically set the working directory to this file's path.
library(corrplot)
library(psych)
#set the working directory to this file's path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# read in data
crabs <- data.frame(read.csv("crabs_preprocessed.csv"))


## Correlation matrix 
Correl <- cor(crabs[3:5])


col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
heat_map_correlations <- corrplot(Correl, method = "ellipse", col = col(200),
                                  type = "full", 
                                  order = "hclust", number.cex = .7,
                                  addCoef.col = "black", # Add coefficient of correlation
                                  tl.col = "black", tl.srt =45 , # Text label color and rotation
                                  # hide correlation coefficient on the principal diagonal
                                  diag = FALSE)


# Factorize color, and spine condition
crabs$color = factor(crabs$color, levels =c(1,2,3,4,5))
crabs$spine_condition = factor(crabs$spine_condition, levels = c(1,2,3))


library(MASS)

fit <- glm(satellites ~ ., data = crabs, family=poisson(link=log))
summary(fit)

stepAIC(fit)

fit.2<- step(glm(satellites~., data= crabs, family=poisson(link=log)))
summary(fit.2)

round(confint(fit),3)


####################################################################################
####################################################################################
####################################################################################
#iteratively reweighted least squares algorithm
crabs$color = factor(crabs$color, levels =c(1,2,3,4))
crabs$spine_condition = factor(crabs$spine_condition, levels = c(1,2,3))
library(msme)

irls.poi <- irls(satellites ~ .,
                 family = "poisson",
                 link = "log",
                 data = crabs)
summary(irls.poi)


# 4.4
# NEW IRWLS using the deviance
IRWLS <- function(x,y,tolerance,lev){
  # x        : predictor
  # y        : binary responce
  # tolerance: stopping criterion
  # lev      : level for the confidence intervals
  Dev       = 0
  delta.Dev = 2*tolerance
  n         = length(x)
  mu <- rep(mean(y), n)     # initialize mu 
  eta <- log(mu)                 # initialize eta
  while ( abs(delta.Dev) > tolerance) {     
    w <- mu                      # weight = variance
    z <- eta + (y - mu)/(mu)     # working response
    mod <- lm(z ~ x, weights = w)       # weighted regression
    eta <- mod$fit                      # linear predictor
    mu <- exp(eta)                        # fitted value
    Dev.old   = Dev
    print(mu)
    Dev     = 2*sum(y*log(y/mu)-(y-mu))
    print(Dev)
    delta.Dev = Dev- Dev.old
    print(abs(delta.Dev))
  }
  model.coef    = mod$coefficients
  model.se      = sqrt(diag(summary(mod)$cov.unscaled))
  lower         = mod$coefficients-qnorm(1-(1-lev)/2)*model.se
  upper         = mod$coefficients+qnorm(1-(1-lev)/2)*model.se
  CI            = cbind(lower,upper)
  Z             = mod$coefficients/model.se
  pvalues       = 2*pnorm(abs(Z), lower.tail = FALSE)
  
  list(coeff=model.coef,se=model.se,default.glm.ConfInt=CI,z.stat=Z,p.values=pvalues)
}
y=as.numeric(crabs$color)
x=as.numeric(crabs$satellites)
mymodel <- IRWLS(x,y,0.00001,0.95)
mymodel

