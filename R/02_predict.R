
# import data

load("~/diplom/data/data_predictors.RData")

head(hh.composition)
head(predictors[, 1:8])
cbind(names(predictors))

# target hhsize, train/test data
if(all(hh.composition$hh == predictors$hh)) 
  d <- cbind(hhsize = hh.composition$hhsize, predictors[,-1])

# d$hhsize <- factor(d$hhsize)
# levels(d$hhsize) <- paste0("hhsize", levels(d$hhsize))

head(d)

set.seed(1)
d <- setNames(split(d, runif(nrow(d)) > .6), c("train","test"))

# summary
tbl <- function(x){
  x <- table(x)
  x <- rbind(n = x, prop = prop.table(x)*100)
  cbind(x, total = round(rowSums(x)))
}
tbl(hh.composition$hhsize)
tbl(d$train$hhsize)
tbl(d$test$hhsize)

# chance is 1/5
mean(hh.composition$hhsize == sample(1:5, nrow(hh.composition), replace = TRUE))

# --- support vector machine --------------------------------------------------

library(e1071)

# linear
svm.linear.tune <- tune.svm(
  hhsize ~ ., data = d$train, kernel = "linear", 
  cost  = seq(0.01, 1, length.out = 5) # seq(0.01, 10, length.out = 3) => 5
  )
summary(svm.linear.tune)
plot(svm.linear.tune)

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


# gauss
svm.radial.tune <- tune.svm(
  hhsize ~ ., data = d$train, kernel = "radial", 
  cost  = seq(0.01, 20, length.out = 7),
  gamma = seq(0.01, 20, length.out = 7) 
)
summary(svm.radial.tune)
plot(svm.radial.tune)

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

library('psych')
cohen.kappa(performance.svm.radial$train$confusion)
cohen.kappa(performance.svm.radial$test$confusion)

# --- Random Forest -----------------------------------------------------------

library(randomForest)

pairs(sqrt(predictors[,2:4])) # cbind(names(predictors))
pairs(sqrt(predictors[,14:16]))
pairs(sqrt(predictors[,35:48]))

ds <- list(
  train = cbind(hhsize = d$train$hhsize, sqrt(d$train[,-1])),
  test  = cbind(hhsize = d$test$hhsize, sqrt(d$test[,-1]))
)

plot(sqrt(ds$train[,c(4,8)]), bg = ds$train$hhsize, pch = 21)
plot(sqrt(ds$train[,c(14,8)]), bg = ds$train$hhsize, pch = 21)
plot(sqrt(ds$train[,c('prg_talk','prg_movie')]), bg = ds$train$hhsize, pch = 21)

min(cor(ds$train[,35:48]))
max(cor(ds$train[,35:48]))

pca <- prcomp(ds$train[,-1])
plot(pca)
plot(pca$x[,1:2], bg = ds$train$hhsize, pch = 21, col = NA, cex = 2)
legend(1.3, 0.6, paste('hhsize', 1:5), bty = 'n', fill = 1:5)

rf <- randomForest(
  hhsize ~ ., data = ds$train, imortance = TRUE, 
  strata = ds$train$hhsize, sampsize = rep(min(table(ds$train$hhsize)), 5) # 67
  )
rf$confusion
plot(rf, main = 'Error rate vs number of trees', col = c(8,1:5))
legend(400, 0.7, c('overall OOB', paste('hhsize', 1:5)), bty = 'n', 
       fill = c(8,1:5))

varImpPlot(rf)

v <- "chn_kids"
partialPlot(rf, d$train, x.var = "chn_kids")
for (i in 2:5)
  partialPlot(rf, d$train, x.var = "chn_kids", add = TRUE, col = i,
              which.class = levels(d$train$hhsize)[i]
              )

performance.rf <- list(
  train = list(
    accuracy = mean(ds$train$hhsize == predict(rf)),
    # confusion = table(true = d$train$hhsize, predict = predict(rf))
    confusion = rf$confusion # the same
  ),
  test = list(
    accuracy = mean(ds$test$hhsize == predict(rf, ds$test)),
    confusion = table(true = ds$test$hhsize, predict = predict(rf, ds$test))
  )
)

cohen.kappa(performance.rf$train$confusion[,-6])
cohen.kappa(performance.rf$test$confusion)
