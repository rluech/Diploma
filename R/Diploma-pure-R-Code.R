
# --- R code for Diploma.Rmd --------------------------------------------------

load('~/git/diploma/data/data_predictors.RData')

hh.composition <- as.data.frame(hh.composition)
predictors <- as.data.frame(predictors)

if(all(hh.composition$hh == predictors$hh))
  predictors <- cbind(
    hh = predictors[,1], hhsize = hh.composition$hhsize, predictors[,-1]
  )

predictors$hhsize <- factor(predictors$hhsize)
levels(predictors$hhsize) <- paste0("hhsize", levels(predictors$hhsize))

pred.log <- cbind(predictors[, (1:2)], lapply(predictors[, -(1:2)] + 1, log))

# pred.log <- predictors
# pred.log[, 3:ncol(pred.log)] <- lapply(predictors[, -(1:2)] + 1, log)

# --- plot log transformation -------------------------------------------------

vars <- list(
  time    = c('day_weekend_17to20','day_workday_17to20'),
  channel = c('chn_arts','chn_kids'),
  program = c('prg_sport','prg_news')
)
par(mfrow = c(2,3))
plot(predictors[, vars$time])
plot(predictors[, vars$channel], main = 'viewing in seconds')
plot(predictors[, vars$program])
plot(pred.log[, vars$time])
plot(pred.log[, vars$channel], main = 'log of viewing in seconds')
plot(pred.log[, vars$program])

# --- plot clustering ---------------------------------------------------------

fun <- function(x, main = NULL, col = adjustcolor(as.integer(pred.log$hhsize), .65))
  plot(x, main = main, bg = col, pch = 21, cex = 1, col = col, frame.plot = FALSE)

par(mfcol = c(1,3))
fun(pred.log[, vars$time])
legend('topleft', paste('hhsize', 1:5), pch = 21, bty = 'n', 
       pt.bg = adjustcolor(1:5, .65), col = adjustcolor(1:5, .65))
fun(pred.log[, vars$channel], main = 'Clusters of hhsize')
fun(pred.log[, vars$program])

# --- corelation matrix -------------------------------------------------------

x <- pred.log[, -1]
x$hhsize <- as.integer(x$hhsize)
heatmap(y <- cor(x))

z <- sort(y['hhsize', -match('hhsize', colnames(y))])
z[c(which.max(z), which.min(z))]

# --- PCA ---------------------------------------------------------------------

pca <- prcomp(pred.log[,-(1:2)], center = TRUE, scale. = TRUE)

x <- t(summary(pca)$importance[, 1:6])

# plot 01
par(mfcol = c(1,1))
plot(pca)

# plot 02
par(mfrow = c(2,3))
# layout(matrix(c(0,1,0,2,3,4), 2, byrow = TRUE))
fun(pca$x[, c(1,2)])
fun(pca$x[, c(1,3)])
fun(pca$x[, c(2,3)])
legend('topright', paste('hhsize', 1:5), pch = 21, bty = 'n', 
       pt.bg = adjustcolor(1:5, .65), col = adjustcolor(1:5, .65))

biplot(pca, choices = c(1,2))
biplot(pca, choices = c(1,3))
biplot(pca, choices = c(2,3))

# --- classifying by chance ---------------------------------------------------

x <- replicate(100, sample(1:5, nrow(hh.composition), replace = TRUE))
round(mean(apply(x, 2, function(y) mean(y == hh.composition$hhsize))), 2)

p <- prop.table(table(hh.composition$hhsize))
c(hhsize = round(p*100, 2))

x <- replicate(100, sample(1:5, nrow(hh.composition), replace = TRUE, prob = p))
round(mean(apply(x, 2, function(y) mean(y == hh.composition$hhsize))), 2)

sum(p^2)

# --- split into train and test data ------------------------------------------

library(caret)
set.seed(999)
train <- caret::createDataPartition(pred.log$hhsize, p = .6, list = FALSE)
d <- list(train = pred.log[train, -1], test = pred.log[-train, -1])

x <- cbind(`train n` = table(d$train$hhsize), `test n` = table(d$test$hhsize))
x <- cbind(x, cbind(`train %` = x[,1]/sum(x[,1]), `test %` = x[,2]/sum(x[,2])))
round(x, 2)

# --- Statistical Learning Model Specification --------------------------------

library(nnet)
m.mnr <- multinom(hhsize ~ ., data = d$train, trace = TRUE, maxit = 500)

library(e1071)
m.svm.linear <- svm(hhsize ~ ., data = d$train, kernel = "linear", cost = 1)
m.svm.radial <- svm(hhsize ~ ., data = d$train, kernel = "radial", cost = 15, 
                    gamma = 0.01)

library(randomForest)
set.seed(7)
m.rf <- randomForest(hhsize ~ ., data = d$train, imortance = TRUE, 
                     strata = d$train$hhsize, 
                     sampsize = rep(min(table(d$train$hhsize)), 5) * .7
                     )

# --- performance -------------------------------------------------------------

models <- list(
  multinomial  = m.mnr, 
  randomforest = m.rf, 
  svm.linear   = m.svm.linear,
  svm.radial   = m.svm.radial
  )

pred <- list(
  train = as.data.frame(lapply(models, predict)),
  test  = as.data.frame(lapply(models, predict, newdata = d$test))
)

# --- Accuracy ---

calc.acc <- function(predicted, observed) mean(predicted == observed)

tabl.acc <- rbind(
  train = sapply(pred$train, calc.acc, observed = d$train$hhsize),
  test  = sapply(pred$test,  calc.acc, observed = d$test$hhsize)
)

# --- Cohens Kappa ---

library(psych)
calc.kappa <- function(predicted, observed, param) {
  mx <- table(observed = observed, predicted = predicted)
  cohen.kappa(mx)[[param]]
}

tabl.kappa <- rbind(
  train = sapply(pred$train, calc.kappa, observed = d$train$hhsize, 'weighted.kappa'),
  test  = sapply(pred$test,  calc.kappa, observed = d$test$hhsize, 'weighted.kappa')
)

# --- Agreement Confusion Matrix ---

agree <- list(
  train = lapply(pred$train, calc.kappa, observed = d$train$hhsize, 'agree'),
  test  = lapply(pred$test,  calc.kappa, observed = d$test$hhsize, 'agree')
)
agree <- do.call(rbind, lapply(agree$test, as.data.frame))
agree$model <- sub('\\.\\d+', '', rownames(agree))
names(agree)[3] <- 'agreement'

library(ggplot2)
ggplot(agree, aes(observed, predicted, agreement)) + 
  geom_tile(aes(fill = agreement)) + 
  theme_bw() +
  facet_grid( ~ model)

# --- Model Search ------------------------------------------------------------

require(MASS)

# m.mnr.best <- MASS::stepAIC(m.mnr)
# save(m.mnr.best, file = './data/stepAIC.RData')
load('~/git/diploma/data/stepAIC.RData')
m.mnr.best <- res

x <- data.frame(
  c(m.mnr$AIC, m.mnr.best$AIC),
  c(length(colnames(coef(m.mnr))[-1]), length(colnames(coef(m.mnr.best))[-1]))
)
dimnames(x) <- list(c('full model','best model'), c('AIC','Terms'))

anova(m.mnr.best, m.mnr, test = 'Chisq')

# m.mnr.best2 <- MASS::stepAIC(m.mnr, scope = . ~ .^2, direction = 'forward')

# --- Variance Importance -----------------------------------------------------

m <- list(multinom.full = m.mnr, multinom.best = m.mnr.best, randomforest = m.rf)
par(mfrow = c(1,3))
for(i in names(m)){
  vars <- caret::varImp(m[[i]])
  vars <- setNames(vars[[1]], rownames(vars))
  dotchart(tail(sort(vars), 15), main = i)
}

plot(m.rf, main = 'Error Rate by Tree Iteration', col = c(8,1:5))
legend('topright', c('overall OOB', paste('hhsize', 1:5)), bty = 'n', fill = c(8,1:5))

# --- Partial Plot ------------------------------------------------------------

classes <- levels(d$train$hhsize)
impvar  <- c("chn_kids", "prg_news","chn_generalistpublic", "day_workday_17to20","prg_commercial")
for(j in seq_along(impvar)){
  png(paste0('~/git/diploma/data/partialplot.rf.',impvar[j],'.png'))
  h <- lapply(classes, function(x) partialPlot(m.rf, d$train, impvar[j], x, plot = FALSE))
  y.lim <- range(lapply(h, `[[`, 'y'))
  partialPlot(m.rf, d$train, impvar[j], classes[1], ylim = y.lim, xlab = impvar[j],
              main = paste("Partial Dependence on", impvar[j]), rug = FALSE)
  for (i in 2:5) 
    partialPlot(m.rf, d$train, impvar[j], classes[i], col = i, rug = FALSE, add = TRUE)
  legend('topright', paste('hhsize', 1:5), bty = 'n', lty = 1, col = 1:5, horiz = TRUE, cex = .8)
  dev.off()
}

partialPlot(m.rf, d$train, 'prg_commercial')

library(effects)
e <- setNames(lapply(impvar, Effect, m.mnr), impvar)

# for(j in seq_along(e)){
#   png(paste0('~/git/diploma/data/partialplot.mnr.', names(e)[j],'.png'))
#   plot(e[[j]], multiline = TRUE, colors = 1:5)
#   dev.off()
# }

j <- 5
png(paste0('~/git/diploma/data/partialplot.mnr.', names(e)[j],'.png'))
plot(e[[j]], multiline = TRUE, colors = 1:5)
dev.off()

# lapply(e, plot, multiline = TRUE, colors = 1:5)

# --- log odds of hhsize by a specific predictor ------------------------------

pred.prob <- predict(m.mnr, type="probs", newdata=d$test)
round(head(pred.prob), 3)

rowSums(pred.prob)

