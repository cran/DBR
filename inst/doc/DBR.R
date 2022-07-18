### R code from vignette source 'DBR.Rnw'

###################################################
### code chunk number 1: DBR.Rnw:104-105
###################################################
old <- options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)


###################################################
### code chunk number 2: DBR.Rnw:282-285
###################################################
library("DBR")
data("pain")
summary(pain)


###################################################
### code chunk number 3: DBR.Rnw:295-296
###################################################
plot(pain)


###################################################
### code chunk number 4: DBR.Rnw:299-300
###################################################
cor.test(pain$severity, pain$interference, method = "spearman")


###################################################
### code chunk number 5: DBR.Rnw:303-304
###################################################
cor.test(pain$age, pain$interference, method = "spearman")


###################################################
### code chunk number 6: DBR.Rnw:311-315 (eval = FALSE)
###################################################
## est_dbr_default <- dbr(
##   formula = interference ~ severity + age
##   , data = pain
## )


###################################################
### code chunk number 7: DBR.Rnw:318-319
###################################################
setdiff(0:70, round(7 * sort(unique(pain$interference))))


###################################################
### code chunk number 8: DBR.Rnw:326-330 (eval = FALSE)
###################################################
## hist(
##   pain$interference, breaks = 100, xlab = "Interference"
##   , main = "Histogram of Pain Interference Score"
## )


###################################################
### code chunk number 9: DBR.Rnw:338-347 (eval = FALSE)
###################################################
## est_dbr_short <- dbr(
##   formula = interference ~ severity + age
##   , data = pain
##   , yunique = 0:70 / 7
##   , control = dbr.control(
##     estimate_left_buffer = TRUE
##     , estimate_right_buffer = TRUE
##   )
## )


###################################################
### code chunk number 10: DBR.Rnw:349-350
###################################################
est_dbr_short <- readRDS("est_dbr_short.rds")


###################################################
### code chunk number 11: DBR.Rnw:358-359 (eval = FALSE)
###################################################
## summary(est_dbr_short)


###################################################
### code chunk number 12: DBR.Rnw:361-362
###################################################
summary(est_dbr_short, make_plot = F)


###################################################
### code chunk number 13: DBR.Rnw:372-382 (eval = FALSE)
###################################################
## est_dbr_long <- dbr(
##   formula = interference ~ severity + age
##   , data = pain
##   , yunique = 0:70 / 7
##   , control = dbr.control(
##     estimate_left_buffer = TRUE
##     , estimate_right_buffer = TRUE
##     , nsmp = 1000, nburnin = 500
##   )
## )


###################################################
### code chunk number 14: DBR.Rnw:384-385
###################################################
est_dbr_long <- readRDS("est_dbr_long.rds")


###################################################
### code chunk number 15: DBR.Rnw:395-396
###################################################
coda_wrapper(est_dbr_long, coda::geweke.diag, frac1 = 0.15)


###################################################
### code chunk number 16: DBR.Rnw:404-410 (eval = FALSE)
###################################################
## pred_point <- predict(est_dbr_long, newdata = df, type = "point")
## head(pred_point)
## hist(pred_point, breaks = 100, col = "grey"
##      , xlab = "Pain Inteference"
##      , main = "Histogram of Point Predictions"
##      )


###################################################
### code chunk number 17: DBR.Rnw:412-419
###################################################
pred_point <- readRDS("pred_point.rds")
head(pred_point)
hist(pred_point, breaks = 100, col = "grey"
     , xlab = "Pain Inteference"
     , main = "Histogram of Point Predictions"
     , xlim = c(0, 10)
     )


###################################################
### code chunk number 18: DBR.Rnw:424-429 (eval = FALSE)
###################################################
## pred_sample <- predict(est_dbr_long, newdata = df, type = "sample")
## hist(pred_sample, breaks = 100, col = "grey"
##      , xlab = "Pain Inteference"
##      , main = "Histogram of Sample Predictions"
##      )


###################################################
### code chunk number 19: DBR.Rnw:431-436
###################################################
pred_sample <- readRDS("pred_sample.rds")
hist(pred_sample, breaks = 100, col = "grey"
     , xlab = "Pain Inteference"
     , main = "Histogram of Sample Predictions"
     )


###################################################
### code chunk number 20: DBR.Rnw:440-441
###################################################
ks.test(pred_sample, pain$interference)


###################################################
### code chunk number 21: DBR.Rnw:447-448
###################################################
summary(est_dbr_long, make_plot = F)


###################################################
### code chunk number 22: DBR.Rnw:453-454 (eval = FALSE)
###################################################
## coef_dbr_long <- coef(est_dbr_long)


###################################################
### code chunk number 23: DBR.Rnw:456-457
###################################################
coef_dbr_long <- readRDS("coef_dbr_long.rds")


###################################################
### code chunk number 24: DBR.Rnw:466-481
###################################################
plot(coef_dbr_long$severity$X$severity, coef_dbr_long$severity$y
     , xlab = "severity", ylab = "interference"
     , main = "DBR vs. linear regression", type = "l"
     , ylim = c(0, 10))
lines(
  coef_dbr_long$severity$X$severity
  , predict(
    lm(interference ~ age + severity, pain)
    , newdata = coef_dbr_long$severity$X
  )
  , col = "red", lty = 2
)
legend("topleft", legend = c("DBR", "linear regression")
       , pch = rep(-1, 2), col = c("black", "red")
       , lty = c(1, 2))


###################################################
### code chunk number 25: DBR.Rnw:505-506
###################################################
sessionInfo()


###################################################
### code chunk number 26: DBR.Rnw:509-510
###################################################
options(old)


