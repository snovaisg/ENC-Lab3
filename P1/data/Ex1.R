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


fit <- glm(satellites ~ ., data = crabs, family=poisson(link=log))
summary(fit)

fit.2<- step(glm(satellites~., data= crabs, family=poisson(link=log)))
summary(fit.2)
