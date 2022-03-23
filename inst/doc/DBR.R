### R code from vignette source 'DBR.Rnw'

###################################################
### code chunk number 1: DBR.Rnw:100-101
###################################################
old <- options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)


###################################################
### code chunk number 2: DBR.Rnw:275-279
###################################################
library("DBR")
data("pain")
df <- pain
df$age <- as.integer(df$age)


###################################################
### code chunk number 3: DBR.Rnw:281-285 (eval = FALSE)
###################################################
## source("../R/util.R")
## source("../R/dbr.R")
## df <- read_csv("../../../manuscript/sim/v3/data.csv")
## df$age <- as.integer(df$age)


###################################################
### code chunk number 4: DBR.Rnw:288-289
###################################################
summary(df)


###################################################
### code chunk number 5: DBR.Rnw:294-295
###################################################
plot(df)


###################################################
### code chunk number 6: DBR.Rnw:300-304 (eval = FALSE)
###################################################
## ret <- with(df, {
##   print(cor.test(severity, interference, method = "spearman"))
##   print(cor.test(age, interference, method = "spearman"))
## })


###################################################
### code chunk number 7: DBR.Rnw:306-314
###################################################
ret <- with(df, {
  suppressWarnings(
    print(cor.test(severity, interference, method = "spearman"))
  )
  suppressWarnings(
    print(cor.test(age, interference, method = "spearman"))
  )
})


###################################################
### code chunk number 8: DBR.Rnw:320-324 (eval = FALSE)
###################################################
## est.1 <- dbr(
##   formula = interference ~ severity + age
##   , data = df
## )


###################################################
### code chunk number 9: DBR.Rnw:326-327
###################################################
est.1 <- readRDS("est_1.rds")


###################################################
### code chunk number 10: DBR.Rnw:332-333 (eval = FALSE)
###################################################
## summary(est.1)


###################################################
### code chunk number 11: DBR.Rnw:340-341
###################################################
cat(readLines('summary_1.txt'), sep = '\n')


###################################################
### code chunk number 12: DBR.Rnw:352-353
###################################################
setdiff(0:70, round(7 * sort(unique(df$interference))))


###################################################
### code chunk number 13: DBR.Rnw:361-370 (eval = FALSE)
###################################################
## est.2 <- dbr(
##   formula = interference ~ severity + age
##   , data = df
##   , control = dbr.control(
##     nsmp = 1000
##     , nburnin = 500
##     , estimate_left_buffer = T
##     , estimate_right_buffer = T
##   ), yunique = 0:70 / 7)


###################################################
### code chunk number 14: DBR.Rnw:373-374 (eval = FALSE)
###################################################
## summary(est.2)


###################################################
### code chunk number 15: DBR.Rnw:381-382
###################################################
cat(readLines('summary_2.txt'), sep = '\n')


###################################################
### code chunk number 16: DBR.Rnw:392-397 (eval = FALSE)
###################################################
## pred_point <- predict(est.2, newdata = df, type = "point")
## hist(pred_point, breaks = 100, col = "grey"
##      , xlab = "Pain Inteference"
##      , main = "Histogram of Point Predictions"
##      )


###################################################
### code chunk number 17: DBR.Rnw:400-405
###################################################
pred_point <- readRDS("pred_point.rds")
hist(pred_point, breaks = 100, col = "grey"
     , xlab = "Pain Inteference"
     , main = "Histogram of Point Predictions"
     )


###################################################
### code chunk number 18: DBR.Rnw:410-415 (eval = FALSE)
###################################################
## pred_sample <- predict(est.2, newdata = df, type = "sample")
## hist(pred_sample, breaks = 100, col = "grey"
##      , xlab = "Pain Inteference"
##      , main = "Histogram of Sample Predictions"
##      )


###################################################
### code chunk number 19: DBR.Rnw:418-423
###################################################
pred_sample <- readRDS("pred_sample.rds")
hist(pred_sample, breaks = 100, col = "grey"
     , xlab = "Pain Inteference"
     , main = "Histogram of Sample Predictions"
     )


###################################################
### code chunk number 20: DBR.Rnw:443-444
###################################################
sessionInfo()


###################################################
### code chunk number 21: DBR.Rnw:447-448
###################################################
options(old)


