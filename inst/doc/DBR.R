### R code from vignette source 'DBR.Rnw'

###################################################
### code chunk number 1: DBR.Rnw:111-112
###################################################
old <- options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)


###################################################
### code chunk number 2: DBR.Rnw:307-310
###################################################
library("DBR")
data("pain")
summary(pain)


###################################################
### code chunk number 3: DBR.Rnw:320-321
###################################################
plot(pain)


###################################################
### code chunk number 4: DBR.Rnw:325-326
###################################################
cor.test(pain$severity, pain$interference, method = "spearman")


###################################################
### code chunk number 5: DBR.Rnw:329-330
###################################################
cor.test(pain$age, pain$interference, method = "spearman")


###################################################
### code chunk number 6: DBR.Rnw:337-341 (eval = FALSE)
###################################################
## est_dbr_default <- dbr(
##   formula = interference ~ severity + age
##   , data = pain
## )


###################################################
### code chunk number 7: DBR.Rnw:344-345
###################################################
setdiff(0:70, round(7 * sort(unique(pain$interference))))


###################################################
### code chunk number 8: DBR.Rnw:352-356
###################################################
hist(
  pain$interference, breaks = 100, xlab = "Interference"
  , main = "Histogram of Pain Interference Score"
)


###################################################
### code chunk number 9: DBR.Rnw:364-375
###################################################
my.seed <- 0
set.seed(my.seed)
est_dbr_short <- dbr(
  formula = interference ~ severity + age
  , data = pain
  , yunique = 0:70 / 7
  , control = dbr.control(
    estimate_left_buffer = TRUE
    , estimate_right_buffer = TRUE
  )
)


###################################################
### code chunk number 10: DBR.Rnw:384-385 (eval = FALSE)
###################################################
## plot(est_dbr_short)


###################################################
### code chunk number 11: DBR.Rnw:395-406 (eval = FALSE)
###################################################
## set.seed(my.seed)
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
### code chunk number 12: DBR.Rnw:408-409
###################################################
est_dbr_long <- readRDS("est_2.rds")


###################################################
### code chunk number 13: DBR.Rnw:414-415 (eval = FALSE)
###################################################
## plot(est_dbr_long)


###################################################
### code chunk number 14: DBR.Rnw:423-424
###################################################
coda_wrapper(est_dbr_long, coda::geweke.diag, frac1 = 0.15)


###################################################
### code chunk number 15: DBR.Rnw:432-439
###################################################
set.seed(my.seed)
pred_point <- predict(est_dbr_long, newdata = pain, type = "point")
head(pred_point)
hist(pred_point, breaks = 100, col = "grey"
     , xlab = "Pain Inteference"
     , main = "Histogram of Point Predictions"
     )


###################################################
### code chunk number 16: DBR.Rnw:444-450
###################################################
set.seed(my.seed)
pred_sample <- predict(est_dbr_long, newdata = pain)
hist(pred_sample, breaks = 100, col = "grey"
     , xlab = "Pain Inteference"
     , main = "Histogram of Sample Predictions"
     )


###################################################
### code chunk number 17: DBR.Rnw:454-455
###################################################
ks.test(pred_sample, pain$interference)


###################################################
### code chunk number 18: DBR.Rnw:461-462
###################################################
coef(est_dbr_long, prob = c(0.05, 0.5, 0.95))


###################################################
### code chunk number 19: DBR.Rnw:467-468 (eval = FALSE)
###################################################
## summary_dbr_long <- summary(est_dbr_long)


###################################################
### code chunk number 20: DBR.Rnw:470-471
###################################################
summary_dbr_long <- summary(est_dbr_long, make_plot = F)


###################################################
### code chunk number 21: DBR.Rnw:480-495
###################################################
plot(summary_dbr_long$severity$X$severity, summary_dbr_long$severity$y
     , xlab = "severity", ylab = "interference"
     , main = "DBR vs. linear regression", type = "l"
     , ylim = c(0, 10))
lines(
  summary_dbr_long$severity$X$severity
  , predict(
    lm(interference ~ age + severity, pain)
    , newdata = summary_dbr_long$severity$X
  )
  , col = "red", lty = 2
)
legend("topleft", legend = c("DBR", "linear regression")
       , pch = rep(-1, 2), col = c("black", "red")
       , lty = c(1, 2))


###################################################
### code chunk number 22: DBR.Rnw:519-520
###################################################
sessionInfo()


###################################################
### code chunk number 23: DBR.Rnw:523-524
###################################################
options(old)


