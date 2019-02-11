
load("~/diplom/data/data_predictors.RData")

# all(hh.composition$hh == predictors$hh)
d <- cbind(hhsize = hh.composition$hhsize, predictors[,-1])

# classification or regression, target as.factor or as.integer?
d$hhsize <- factor(d$hhsize)
levels(d$hhsize) <- paste0("hhsize", levels(d$hhsize))

d[1:6, 1:5]
matrix(names(d), ncol = 3)

par(mfrow = c(2,3))
cols <- c('day_workday_02to06','day_weekend_17to20','day_workday_17to20')
for(i in seq_along(cols)) plot(d[, cols[-i]])
for(i in seq_along(cols)) plot(log(d[, cols[-i]]) + 1)

par(mfrow = c(2,3))
cols <- c('chn_arts','chn_paytv', 'chn_kids')
for(i in seq_along(cols)) plot(d[, cols[-i]])
for(i in seq_along(cols)) plot(log(d[, cols[-i]]) + 1)

par(mfrow = c(2,3))
cols <-  c('prg_kids','prg_sport','prg_news')
for(i in seq_along(cols)) plot(d[, cols[-i]])
for(i in seq_along(cols)) plot(log(d[, cols[-i]]) + 1)

# log transformation log(x + 1)
d[, -1] <- lapply(d[, -1] + 1, log)


fun <- function(x, cols, col = adjustcolor(as.integer(x$hhsize), .65))
  plot(x[, cols], bg = col, pch = 21, cex = 2, col = col, frame.plot = FALSE)

par(mfcol = c(2,2))
cols <- c('day_workday_06to08','day_weekend_17to20')
fun(d, cols)
cols <- c('day_workday_17to20','day_weekend_17to20')
fun(d, cols)
cols <- c('chn_arts','chn_kids')
fun(d, cols)
legend('topright', paste('hhsize', 1:5), bty = 'n', fill = adjustcolor(1:5, .65))
cols <- c('prg_info','prg_movie')
fun(d, cols)

pca <- prcomp(d[,-1])
par(mfcol = c(1,1))
plot(pca)
par(mfrow = c(2,2))
fun(pca$x, 1:2, col = adjustcolor(as.integer(d$hhsize), .45))
fun(pca$x, c(1,3), col = adjustcolor(as.integer(d$hhsize), .45))
legend('topright', paste('hhsize', 1:5), bty = 'n', fill = adjustcolor(1:5, .65))
fun(pca$x, 2:3, col = adjustcolor(as.integer(d$hhsize), .45))
fun(pca$x, 3:4, col = adjustcolor(as.integer(d$hhsize), .45))



set.seed(1)
d <- setNames(split(d, runif(nrow(d)) > .6), c("train","test"))

# summary
tbl <- function(x){
  x <- table(x$hhsize)
  x <- rbind(n = x, `%` = round(prop.table(x) * 100, 2))
  t(cbind(x, total = round(rowSums(x))))
}
lapply(d, tbl)

# chance is 1/5
mean(hh.composition$hhsize == sample(1:5, nrow(hh.composition), replace = TRUE))


library(e1071)

# linear
# svm.linear.tune <- tune.svm(
#   hhsize ~ ., data = d$train, kernel = "linear", 
#   cost  = seq(0.01, 1, length.out = 5) # seq(0.01, 10, length.out = 3) => 5
#   )
# summary(svm.linear.tune)
# plot(svm.linear.tune)

svm.linear <- svm(
  hhsize ~ ., data = d$train, kernel = "linear",
  cost = 1 # 7.8 # svm.linear.tune$best.parameters$cost
)

performance.svm.linear <- list(
  train = list(
    accuracy = mean(d$train$hhsize == predict(svm.linear)),
    confusion = table(true = d$train$hhsize, predict = predict(svm.linear))
  ),
  test = list(
    accuracy = mean(d$test$hhsize == predict(svm.linear, d$test)),
    confusion = table(true = d$test$hhsize, predict = predict(svm.linear, d$test))
  )
)


svm.radial <- svm(
  hhsize ~ ., data = d$train, kernel = "radial",
  gamma = 0.01, # svm.radial.tune$best.parameters$gamma
  cost = 15     # svm.radial.tune$best.parameters$cost
)

performance.svm.radial <- list(
  train = list(
    accuracy = mean(d$train$hhsize == predict(svm.radial)),
    confusion = table(true = d$train$hhsize, predict = predict(svm.radial))
  ),
  test = list(
    accuracy = mean(d$test$hhsize == predict(svm.radial, d$test)),
    confusion = table(true = d$test$hhsize, predict = predict(svm.radial, d$test))
  )
)
performance.svm.radial


library(randomForest)

rf <- randomForest(
  hhsize ~ ., data = d$train, imortance = TRUE, 
  strata = d$train$hhsize, sampsize = rep(min(table(d$train$hhsize)), 5) # 67
)

plot(rf, main = 'Error rate vs number of trees', col = c(8,1:5))
legend(400, 0.7, c('overall OOB', paste('hhsize', 1:5)), bty = 'n', 
       fill = c(8,1:5))

performance.rf <- list(
  train = list(
    accuracy = mean(d$train$hhsize == predict(rf)),
    # confusion = table(true = d$train$hhsize, predict = predict(rf))
    confusion = rf$confusion # the same
  ),
  test = list(
    accuracy = mean(d$test$hhsize == predict(rf, d$test)),
    confusion = table(true = d$test$hhsize, predict = predict(rf, d$test))
  )
)
performance.rf
# cohen.kappa(performance.rf$train$confusion[,-6])
# cohen.kappa(performance.rf$test$confusion)

plot(rf, main = 'Error rate vs number of trees', col = c(8,1:5))
legend(400, 0.7, c('overall OOB', paste('hhsize', 1:5)), bty = 'n', 
       fill = c(8,1:5))

varImpPlot(rf)

partialPlot(rf, d$train, x.var = "chn_kids")
for (i in 2:5)
  partialPlot(rf, d$train, x.var = "chn_kids", add = TRUE, col = i,
              which.class = levels(d$train$hhsize)[i]
  )


partialPlot(rf, d$train, x.var = "prg_news")
for (i in 2:5)
  partialPlot(rf, d$train, x.var = "prg_news", add = TRUE, col = i,
              which.class = levels(d$train$hhsize)[i]
  )