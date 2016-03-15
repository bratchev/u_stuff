## rider churn analysis
# Bori Ratchev

### Boilerplate to set up parallel computing
setwd("/Users/borisratchev/data/boris/work/entrepreneurship/jobs/software/2016_search/uber/R")
library("foreach")
library("iterators")
library("parallel")
if (exists("cl") && is(cl, "cluster")) {
  stopCluster(cl)
}
cl <- makeCluster(4, homogeneous = TRUE, useXDR = FALSE)
library("doParallel")
registerDoParallel(cl)

### Our libraries
library("data.table")
library("party")
library("randomForest")
library("gbm")
library("caret")
library("pROC")
source("lattice.R")
graphics.files <- TRUE
graphics.files <- !interactive()
source("Graphics.R")

### Load data
df1 <- fread("data/Data.csv")

set.seed(301)

names(df1)
summary(df1)

df1$signup_date = as.Date(df1$signup_date)
df1$last_trip_date = as.Date(df1$last_trip_date)
df1$uber_black_user = as.factor(df1$uber_black_user)
df1$city = as.factor(df1$city)
df1$phone = ifelse(df1$phone =='', 'unknown', df1$phone)
df1$avg_rating_of_driver[is.na(df1$avg_rating_of_driver)] <- -1
df1$avg_rating_by_driver[is.na(df1$avg_rating_by_driver)] <- -1
df1$phone = as.factor(df1$phone)
## Our output variable:
df1[, retained := factor((last_trip_date >= "2014-06-01"), levels = c(TRUE, FALSE), labels = c("yes", "no"))]

summary(df1)
str(df1)

## Look for correlations for retained
Graphics("churned-splom")
splom(df1[city=="King's Landing", list(
                  retained,                
                  avg_dist,                        
                  avg_rating_of_driver, 
                  avg_rating_by_driver,
                  avg_surge,                     
                  phone,                 
                  surge_pct,             
                  uber_black_user,       
                  weekday_pct,           
                  trips_in_first_30_days
                )], 
      lower.panel = dm.panel, upper.panel = panel.correlation, as.matrix = TRUE)
Graphics.off()

# The most promising vars only:
splom(df1[, list(
  retained,                
  avg_rating_by_driver,
  phone,                 
  surge_pct,             
  uber_black_user,       
  weekday_pct
)], 
lower.panel = dm.panel, upper.panel = panel.correlation, as.matrix = TRUE)



## Create training and test sets. Variable selection not shown.
train.rows <- sample.int(NROW(df1), floor(0.80 * NROW(df1)))
train.data <- df1[train.rows]
test.data <- df1[-train.rows]

form <- retained ~                
  avg_dist +                        
  avg_rating_of_driver + 
  avg_rating_by_driver +
  avg_surge +                     
  phone +                 
  surge_pct +             
  uber_black_user +       
  weekday_pct +           
  trips_in_first_30_days - 1


train.mf <- model.frame(form, data = train.data)
train.y  <- model.response(train.mf)
train.mm <- model.matrix(form, train.mf)

test.mf <- model.frame(form, data = test.data)
test.y  <- model.response(test.mf)
test.mm <- model.matrix(form, test.mf)

## Clean up
rm(train.mf, test.mf)

## Try glm. I have done the model selection
model.glm <- glm(form, family = binomial(), data = train.data)
summary(model.glm)
Graphics("03-01-churn.1-glm-bwplot")
bwplot(predict(model.glm, type = "response") ~ retained, data = train.data)
Graphics.off()
p.glm <- predict(model.glm, newdata = test.data, type = "response")
roc.glm <- pROC::roc(test.data$retained, p.glm)
Graphics("03-01-churn.1-glm-roc")
plot(roc.glm)
Graphics.off()

aucs <- data.frame(method = "glm", auc = auc(roc.glm))

## Try tree
model.tree <- ctree(form, data = train.data)
Graphics("03-01-churn.1-tree-plot")
plot(model.tree)
Graphics.off()


model.tree.pred <- predict(model.tree, type = "response")
summary(model.tree.pred)
table(train.data$retained, model.tree.pred)

p.tree <- predict(model.tree, newdata = test.data, type = "prob")
p.tree <- sapply(p.tree, `[`, 2)
roc.tree <- pROC::roc(test.data$retained, p.tree)

Graphics("03-01-churn.1-tree-roc")
plot(roc.tree)
Graphics.off()

aucs <- rbind(aucs, data.frame(method = "tree", auc = auc(roc.tree)))

## Bagging trees
model.RF <- randomForest(x = train.mm, y = train.y, ntree = 250, importance = TRUE, do.trace = 50)
print(model.RF)
plot(model.RF)
table(train.data$retained, model.RF$predicted)

p.RF <- predict(model.RF, newdata = test.mm, type = "prob")[, "yes"]
roc.RF <- pROC::roc(test.data$retained, p.RF)
Graphics("03-01-churn.1-rf-roc")
plot(roc.RF)
Graphics.off()

aucs <- rbind(aucs, data.frame(method = "RF", auc = auc(roc.RF)))


## Boosting trees
model.gbm <-
  gbm(I(as.numeric(retained) - 1) ~ 
        avg_dist + avg_rating_of_driver + avg_rating_by_driver  + phone + surge_pct + uber_black_user + weekday_pct + trips_in_first_30_days,
      distribution = "bernoulli", data = train.data[city=="Winterfell",],
      n.trees = 1000, interaction.depth = 1,
      shrinkage = 2e-2, verbose = TRUE)
Graphics("03-01-churn.1-gbm-summary")
summary(model.gbm)
Graphics.off()

# Show how variable affects output:
par(mfrow=c(2,2))
plot(model.gbm, i = "avg_rating_by_driver")
plot(model.gbm, i = "weekday_pct")
plot(model.gbm, i = "surge_pct")
plot(model.gbm, i = "phone")
par(mfrow=c(1,1))

# Show how they are distributed:
par(mfrow=c(2,2))
plot(density(df1$avg_rating_by_driver), main="Avg Rating by Driver")
plot(density(df1$weekday_pct), main="Pct On Weekday")
plot(density(df1$surge_pct), main="Pct with Surge")
plot(df1$phone, main="Phone Type")
par(mfrow=c(1,1))

Graphics("03-01-churn.1-gbm-perf")
(n.trees <- gbm.perf(model.gbm, method = "OOB"))
Graphics.off()

p <- predict(model.gbm, newdata = train.data, n.trees = n.trees, type = "response")
Graphics("03-01-churn.1-gbm-bwplot")
bwplot(p ~ retained, data = train.data)
Graphics.off()

## calibrate.plot(y = as.numeric(train.data$churn.1)-1, p = predict(model.gbm, newdata = train.data, n.trees = n.trees, type = "response"))

p.gbm <- predict(model.gbm, newdata = test.data, n.trees = n.trees, type = "response")
bwplot(p.gbm ~ retained, data = test.data)

roc.gbm <- pROC::roc(test.data$retained, p.gbm)
Graphics("03-01-churn.1-gbm-roc")
plot(roc.gbm)
Graphics.off()

aucs <- rbind(aucs, data.frame(method = "gbm", auc = auc(roc.gbm)))
print(aucs[order(aucs$auc),])

# We now know tht the gbm model has the best auc. We spend a bit more time on variable importance here:
# The overall model has this order:
summary(model.gbm)
#Astapor
summary(model.gbm_A)
#King's
summary(model.gbm_K)
#Winterfell
summary(model.gbm_W)
## Clean up
rm(model.gbm, model.RF, model.tree, model.glm)


########################################################################
quit("no")
########################################################################
