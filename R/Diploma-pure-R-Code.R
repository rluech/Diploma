
# R code for Diploma.Rmd

load('~/git/diploma/data/data_predictors.RData')

if(all(hh.composition$hh == predictors$hh))
  predictors <- cbind(
    hh = predictors[,1], hhsize = hh.composition$hhsize, predictors[,-1]
  )

predictors$hhsize <- factor(predictors$hhsize)
levels(predictors$hhsize) <- paste0("hhsize", levels(predictors$hhsize))

pred.log <- cbind(predictors[, (1:2)], lapply(predictors[, -(1:2)] + 1, log))

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

# --- PCA ---------------------------------------------------------------------

pca <- prcomp(pred.log[,-(1:2)], center = TRUE, scale. = FALSE)
#par(mfcol = c(2,2))
layout(matrix(c(0,1,0,2,3,4), 2, byrow = TRUE))
plot(pca)
screeplot(pca)
legend('topright', paste('hhsize', 1:5), pch = 21, bty = 'n', 
       pt.bg = adjustcolor(1:5, .65), col = adjustcolor(1:5, .65))
fun(pca$x[, c(1,2)])
fun(pca$x[, c(1,3)])
fun(pca$x[, c(3,2)])

t(summary(pca)$importance[, 1:10])

par(mfcol = c(1,1))
biplot(pca, choices = 2:3)
