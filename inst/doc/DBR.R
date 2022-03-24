### R code from vignette source 'DBR.Rnw'

###################################################
### code chunk number 1: DBR.Rnw:100-101
###################################################
old <- options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)


###################################################
### code chunk number 2: DBR.Rnw:271-275
###################################################
library("DBR")
data("pain")
df <- pain
df$age <- as.integer(df$age)


###################################################
### code chunk number 3: DBR.Rnw:277-281 (eval = FALSE)
###################################################
## source("../R/util.R")
## source("../R/dbr.R")
## df <- read_csv("../../../manuscript/sim/v3/data.csv")
## df$age <- as.integer(df$age)


###################################################
### code chunk number 4: DBR.Rnw:284-285
###################################################
summary(df)


###################################################
### code chunk number 5: DBR.Rnw:290-291
###################################################
plot(df)


###################################################
### code chunk number 6: DBR.Rnw:296-300 (eval = FALSE)
###################################################
## ret <- with(df, {
##   print(cor.test(severity, interference, method = "spearman"))
##   print(cor.test(age, interference, method = "spearman"))
## })


###################################################
### code chunk number 7: DBR.Rnw:302-310
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
### code chunk number 8: DBR.Rnw:316-320 (eval = FALSE)
###################################################
## est.1 <- dbr(
##   formula = interference ~ severity + age
##   , data = df
## )


###################################################
### code chunk number 9: DBR.Rnw:322-323
###################################################
est.1 <- readRDS("est_1.rds")


###################################################
### code chunk number 10: DBR.Rnw:328-329 (eval = FALSE)
###################################################
## summary(est.1)


###################################################
### code chunk number 11: DBR.Rnw:336-337
###################################################
cat(readLines('summary_1.txt'), sep = '\n')


###################################################
### code chunk number 12: DBR.Rnw:348-349
###################################################
setdiff(0:70, round(7 * sort(unique(df$interference))))


###################################################
### code chunk number 13: DBR.Rnw:357-366 (eval = FALSE)
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
### code chunk number 14: DBR.Rnw:369-370 (eval = FALSE)
###################################################
## summary(est.2)


###################################################
### code chunk number 15: DBR.Rnw:377-378
###################################################
cat(readLines('summary_2.txt'), sep = '\n')


###################################################
### code chunk number 16: DBR.Rnw:388-393 (eval = FALSE)
###################################################
## pred_point <- predict(est.2, newdata = df, type = "point")
## hist(pred_point, breaks = 100, col = "grey"
##      , xlab = "Pain Inteference"
##      , main = "Histogram of Point Predictions"
##      )


###################################################
### code chunk number 17: DBR.Rnw:396-401
###################################################
pred_point <- readRDS("pred_point.rds")
hist(pred_point, breaks = 100, col = "grey"
     , xlab = "Pain Inteference"
     , main = "Histogram of Point Predictions"
     )


###################################################
### code chunk number 18: DBR.Rnw:406-411 (eval = FALSE)
###################################################
## pred_sample <- predict(est.2, newdata = df, type = "sample")
## hist(pred_sample, breaks = 100, col = "grey"
##      , xlab = "Pain Inteference"
##      , main = "Histogram of Sample Predictions"
##      )


###################################################
### code chunk number 19: DBR.Rnw:414-419
###################################################
pred_sample <- readRDS("pred_sample.rds")
hist(pred_sample, breaks = 100, col = "grey"
     , xlab = "Pain Inteference"
     , main = "Histogram of Sample Predictions"
     )


###################################################
### code chunk number 20: DBR.Rnw:439-440
###################################################
sessionInfo()


###################################################
### code chunk number 21: DBR.Rnw:443-444
###################################################
options(old)


