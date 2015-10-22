#-------------------------------------------------------------------------
# This R script called 'run_analysis_reg_models.R' analyzes the car data "mtcars"
# available as R data frame via data(mtcars).
#
#-------------------------------------------------------------------------
# Necessary library calls in R:
library(plyr)
library(dplyr)
library(Hmisc)
library(reshape2)
library(ggplot2)
library(knitr)
#
#-------------------------------------------------------------------------
data(mtcars)
dim(mtcars)
head(mtcars, 10)
#
par(mfrow = c(1,2))
par(mar = c(4,4,2,2))
facet_names <- list("0" = "Automatic", "1" = "Manual")
facet_labeller <- function(am, value){
  value <- as.character(value)
  return(facet_names[value])
}
g1 <- ggplot(mtcars, aes(mpg)) + geom_histogram(binwidth = 2, col = "blue")
g2 <- g1 + facet_grid(.~ am, labeller = facet_labeller) 
g3 <- g2 + labs(title = "Miles per gallon for automatic or manual transmission") 
g4 <- g3 + labs(x = "Miles per gallon [mpg]", y = "frequency")
g4
#png(filename = "plot_mtcars_mpg.png", width = 480, height = 480, units = "px", pointsize = 12, bg = "white")
#dev.copy(png, file = "/Users/markjack/data/plot_mtcars_mpg.png")
#dev.off()
#-------------------------------------------------------------------------
#
mtcars_melt <- melt(mtcars, id.vars = "am", measure.vars = "mpg", na.rm = TRUE)
mtcars_select <- group_by(mtcars_melt, am, variable)                       
mtcars_mean <- ddply(mtcars_select, c("am"), summarise, mean = mean(value, na.rm = TRUE))
mtcars_median <- ddply(mtcars_select, c("am"), summarise, median = median(value, na.rm = TRUE))
mtcars_sd <- ddply(mtcars_select, c("am"), summarise, sd = sd(value, na.rm = TRUE))
mtcars_min <- ddply(mtcars_select, c("am"), summarise, min = min(value, na.rm = TRUE))
mtcars_max <- ddply(mtcars_select, c("am"), summarise, max = max(value, na.rm = TRUE))
mtcars_stat <- mtcars_mean
mtcars_stat$median <- mtcars_median$median
mtcars_stat$sd <- mtcars_sd$sd 
mtcars_stat$min <- mtcars_min$min 
mtcars_stat$max <- mtcars_max$max
#
mtcars_melt_wt   <- melt(mtcars, id.vars = "am", measure.vars = "wt", na.rm = TRUE)
mtcars_select_wt <- group_by(mtcars_melt_wt, am, variable)                       
mtcars_mean_wt   <- ddply(mtcars_select_wt, c("am"), summarise, mean = mean(value, na.rm = TRUE))
mtcars_median_wt <- ddply(mtcars_select_wt, c("am"), summarise, median = median(value, na.rm = TRUE))
mtcars_sd_wt   <- ddply(mtcars_select_wt, c("am"), summarise, sd = sd(value, na.rm = TRUE))
mtcars_min_wt  <- ddply(mtcars_select_wt, c("am"), summarise, min = min(value, na.rm = TRUE))
mtcars_max_wt  <- ddply(mtcars_select_wt, c("am"), summarise, max = max(value, na.rm = TRUE))
mtcars_stat_wt <- mtcars_mean_wt
mtcars_stat_wt$median <- mtcars_median_wt$median
mtcars_stat_wt$sd  <- mtcars_sd_wt$sd 
mtcars_stat_wt$min <- mtcars_min_wt$min 
mtcars_stat_wt$max <- mtcars_max_wt$max
#
mtcars_melt_hp  <- melt(mtcars, id.vars = "am", measure.vars = "hp", na.rm = TRUE)
mtcars_select_hp <- group_by(mtcars_melt_hp, am, variable)                       
mtcars_mean_hp   <- ddply(mtcars_select_hp, c("am"), summarise, mean = mean(value, na.rm = TRUE))
mtcars_median_hp <- ddply(mtcars_select_hp, c("am"), summarise, median = median(value, na.rm = TRUE))
mtcars_sd_hp   <- ddply(mtcars_select_hp, c("am"), summarise, sd = sd(value, na.rm = TRUE))
mtcars_min_hp  <- ddply(mtcars_select_hp, c("am"), summarise, min = min(value, na.rm = TRUE))
mtcars_max_hp  <- ddply(mtcars_select_hp, c("am"), summarise, max = max(value, na.rm = TRUE))
mtcars_stat_hp <- mtcars_mean_hp
mtcars_stat_hp$median <- mtcars_median_hp$median
mtcars_stat_hp$sd  <- mtcars_sd_hp$sd 
mtcars_stat_hp$min <- mtcars_min_hp$min 
mtcars_stat_hp$max <- mtcars_max_hp$max
#
head(mtcars_stat)
head(mtcars_stat_wt)
head(mtcars_stat_hp)
#
#-------------------------------------------------------------------------
lm_mtcars <- lm(data = mtcars, mpg ~ factor(am))
summary(lm(data = mtcars, mpg ~ factor(am)))$coef
#
lm_mtcars_wt <- lm(data = mtcars, mpg ~ factor(am) + wt)
summary(lm(data = mtcars, mpg ~ factor(am) + wt))$coef
#
lm_mtcars_wt_hp <- lm(data = mtcars, mpg ~ factor(am) + wt + hp)
summary(lm(data = mtcars, mpg ~ factor(am) + wt + hp))$coef
#
lm_mtcars_wt_hp_am <- lm(data = mtcars, mpg ~ factor(am) + wt + hp + factor(am)*wt)
summary(lm(data = mtcars, mpg ~ factor(am) + wt + hp + factor(am)*wt))$coef
#
lm_mtcars_wt_hp_am2 <- lm(data = mtcars, mpg ~ factor(am) + wt + hp + factor(am)*wt + factor(am)*hp)
summary(lm(data = mtcars, mpg ~ factor(am) + wt + hp + factor(am)*wt + factor(am)*hp))$coef
#  
#-------------------------------------------------------------------------
anova(lm_mtcars, lm_mtcars_wt, lm_mtcars_wt_hp, lm_mtcars_wt_hp_am, lm_mtcars_wt_hp_am2)
#
#-------------------------------------------------------------------------
lm_mtcars_wt_am <- lm(data = mtcars, mpg ~ factor(am) + wt + factor(am)*wt)
summary(lm(data = mtcars, mpg ~ factor(am) + wt + factor(am)*wt))$coef
#
lm_mtcars_hp_am <- lm(data = mtcars, mpg ~ factor(am) + hp)
summary(lm(data = mtcars, mpg ~ factor(am) + hp))$coef
#
#-------------------------------------------------------------------------
confint(lm_mtcars_wt_am)
confint(lm_mtcars_hp_am)
#
#-------------------------------------------------------------------------
e_wt_am <- rstudent(lm_mtcars_wt_am)
e_hp_am <- rstudent(lm_mtcars_hp_am)
fit_wt_am <- predict(lm_mtcars_wt_am)
fit_hp_am <- predict(lm_mtcars_hp_am)
#
#-------------------------------------------------------------------------
par(mfrow = c(1,2))
plot(mtcars$wt, mtcars$mpg, pch=19, xlab = "weight [in 1000 lbs]", ylab = "miles per gallon [mpg]", 
     bg = "lightblue", col = "black", cex = 2, frame = FALSE)
points(mtcars$wt, mtcars$mpg, pch=19, col=((mtcars$am==0)*1+1))
abline(c(lm_mtcars_wt_am$coef[1],lm_mtcars_wt_am$coef[3]), col="red", lwd=3)
abline(c(lm_mtcars_wt_am$coef[1]+lm_mtcars_wt_am$coef[2],lm_mtcars_wt_am$coef[3]+lm_mtcars_wt_am$coef[4]), 
       col="black", lwd=3)
legend('topright', c("manual", "autom."), pch=19, col=c("red", "black"), inset = .05)
#
plot(mtcars$hp, mtcars$mpg, pch=19, xlab = "horse power [hp]", ylab = "miles per gallon [mpg]", 
     bg = "lightblue", col = "black", cex = 2, frame = FALSE)
points(mtcars$hp, mtcars$mpg, pch=19, col=((mtcars$am==0)*1+1))
abline(c(lm_mtcars_hp_am$coef[1],lm_mtcars_hp_am$coef[3]), col="red", lwd=3)
abline(c(lm_mtcars_hp_am$coef[1]+lm_mtcars_hp_am$coef[2],lm_mtcars_hp_am$coef[3]), col="black", lwd=3)
legend('topright', c("manual","autom."), pch=19, col=c("red", "black"), inset = .05)
#
#dev.copy(png, file = "/Users/markjack/data/plot_lm_mpg_hp_hist.png")
#dev.off()
#
#-------------------------------------------------------------------------
par(mfrow = c(2,1))
plot(mtcars$wt, e_wt_am, xlab = "weight [in 1000 lbs]", ylab = "normalized residuals [t-dist.]", bg = "lightblue", 
     col = "black", cex = 2, pch = 21, frame = FALSE)
abline(h=0, lwd=2)
for (i in 1 : n)
  lines(c(mtcars$wt[i], mtcars$wt[i]), c(e_wt_am[i], 0), col = "red", lwd = 2)
#
plot(mtcars$hp, e_hp_am, xlab = "horse power [hp]", ylab = "normalized residuals [t-dist.]", bg = "lightblue", 
     col = "black", cex = 2, pch = 21, frame = FALSE)
abline(h=0, lwd=2)
for (i in 1 : n)
  lines(c(mtcars$hp[i], mtcars$hp[i]), c(e_hp_am[i], 0), col = "red", lwd = 2)
#dev.copy(png, file = "/Users/markjack/data/plot_resid_mpg_hp.png")
#dev.off()
#
#-------------------------------------------------------------------------
par(mar = c(4,4,2,2))
par(mfrow = c(1,1))
histogram(e_wt_am, nint = 7, col = "blue", 
          ylab = "frequency", xlab = "normalized residuals [t-dist.]", 
          main = "Distribution of residuals for 'mpg vs wt' plot")
#dev.copy(png, file = "/Users/markjack/data/plot_resid_mpg_hist.png")
#dev.off()
#
#-------------------------------------------------------------------------
par(mfrow = c(1,1))
histogram(e_hp_am, nint = 7, col = "blue", 
          ylab = "frequency", xlab = "normalized residuals [t-dist.]", 
          main = "Distribution of residuals for 'mpg vs hp' plot")
#dev.copy(png, file = "/Users/markjack/data/plot_resid_hp_hist.png")
#dev.off()
#
#-------------------------------------------------------------------------
par(mfrow = c(1,2))
plot(fit_wt_am, e_wt_am, pch=19, xlab = "miles per gallon [predicted for 'mpg~wt']", 
     ylab = "normalized residuals [t-dist.]", bg = "lightblue", 
     col = "black", cex = 2, frame = FALSE)
points(fit_wt_am, e_wt_am, pch=19, col=((mtcars$am==0)*1+1))
legend('topleft', c("manual","autom."), pch=19, col=c("red", "black"), inset = .05)
#
plot(fit_hp_am, e_hp_am, pch=19, xlab = "miles per gallon [predicted for 'mpg~hp']", 
     ylab = "normalized residuals [t-dist.]", bg = "lightblue", 
     col = "black", cex = 2, frame = FALSE)
points(fit_hp_am, e_hp_am, pch=19, col=((mtcars$am==0)*1+1))
legend('topleft', c("manual","autom."), pch=19, col=c("red", "black"), inset = .05)
#
#dev.copy(png, file = "/Users/markjack/data/plot_fit_res_mpg_hp_hist.png")
#dev.off()
