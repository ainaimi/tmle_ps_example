
library(tidyverse)
library(tmle)

set.seed(1)
n <- 250
W <- matrix(rnorm(n*3), ncol=3)
A <- rbinom(n,1, 1/(1+exp(-(.2*W[,1] - .1*W[,2] + .4*W[,3]))))
Y <- A + 2*W[,1] + W[,3] + W[,2]^2 + rnorm(n)

ranger_learner <- create.Learner("SL.ranger", tune=list(min.node.size = c(30,60)))
svm_learner <- create.Learner("SL.svm",tune=list(nu = c(.25,.5)))
sl.lib <- c(ranger_learner$names,svm_learner$names,"SL.glm")

tmle_obj <- tmle(Y, A, W, family="gaussian", Q.SL.library = sl.lib,g.SL.library = sl.lib)

ps_data <- tibble(A=as.factor(A),PS=tmle_obj$g$g1W)

ggplot(ps_data) + geom_density(aes(PS,group=A,color=A)) + ylab("Density") + xlab("Propensity Score")
