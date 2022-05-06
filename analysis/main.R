library(data.table)
library(ggplot2)
library(ggpubr)
library(broom)
library(AICcmodavg)
library(tidyverse)
library(rstatix)
library(lme4)
library(moments)
library(diptest)
library(mixtools)

child.data <- read.csv("..\\parent.csv", header = TRUE, 
                      colClasses = c(
                        "factor", # object detected
                        "numeric", # duration
                        "factor", # day
                        "factor", # week
                        "numeric", # log duration
                        "factor", # ID
                        "numeric", "numeric", "numeric", "numeric", "numeric", # ADOS
                        "numeric", "numeric", "numeric", "numeric", # DAS-II
                        "numeric", "numeric", "numeric", "numeric" # ADI-R
                        ))

summary(child.data)

child.data %>% group_by(ID) %>% get_summary_stats(log_duration, type='common')
bxp <- ggboxplot(child.data[child.data$ID == "EXP05", ], x = "week", y = "log_duration", add = "point")

# checking for outliers
child.data %>% group_by(ID) %>% identify_outliers(log_duration)

# remove outliers by participant
list_quantiles <- tapply(child.data$log_duration, child.data$ID, quantile)

ids <- sort(unique(child.data$ID), decreasing=TRUE)

Q1s <- sapply(1:length(ids), function(i) list_quantiles[[i]][2])
Q3s <- sapply(1:length(ids), function(i) list_quantiles[[i]][4])

IQRs <- tapply(child.data$log_duration, child.data$ID, IQR)

Lowers <- Q1s - 1.5*IQRs
Uppers <- Q3s + 1.5*IQRs

datas <- split(child.data, child.data$ID)

data_no_outlier <- NULL
for (i in 1:length(ids)){
  out <- subset(datas[[i]], datas[[i]]$log_duration > Lowers[i] & datas[[i]]$log_duration < Uppers[i])
  data_no_outlier <- rbind(data_no_outlier, out)
}

dim(data_no_outlier)

bxp <- ggboxplot(data_no_outlier[data_no_outlier$ID == "EXP07", ], x = "week", y = "log_duration", add = "point")
bxp

# normality assumption
ggdensity(data_no_outlier, x = "duration", fill = "lightgray", title = "duration") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggqqplot(data_no_outlier, "duration", facet.by = "ID")

## calculate skewness coefficient
skewness(data_no_outlier$duration, na.rm = TRUE)

## transform skewed data
data_no_outlier$log_duration <- log10(data_no_outlier$duration)

## confirm normality **
ggdensity(data_no_outlier[data_no_outlier$week == "1", ], x = "log_duration", fill = "lightgray", title = "transformed duration") +
  stat_overlay_normal_density(color = "red", linetype = "dashed") + facet_wrap(~ID)

ggdensity(data_no_outlier[data_no_outlier$ID == "EXP02", ], x = "log_duration", fill = "lightgray", title = "transformed duration") +
  stat_overlay_normal_density(color = "red", linetype = "dashed") + facet_wrap(~week)

## Hartigan's dip test for multimodality
dip.test(data_no_outlier[data_no_outlier$ID == "EXP05" & data_no_outlier$week == "1" , ]$log_duration)
dip.test(data_no_outlier[data_no_outlier$ID == "EXP05", ]$log_duration)
dip.test(data_no_outlier$log_duration)

## determine modes with Gaussian Mixed Model
plot_mix_comps <- function(x, mu, sigma, lam) {
  lam * dnorm(x, mu, sigma)
}
mixmdl <- normalmixEM(data_no_outlier[data_no_outlier$ID == "EXP05" & data_no_outlier$week == "1" , ]$log_duration, k = 2)

## plot GMM
data.frame(x = mixmdl$x) %>%
  ggplot() +
  geom_histogram(aes(x, ..density..), binwidth = 1, colour = "black", 
                 fill = "white") +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(mixmdl$mu[1], mixmdl$sigma[1], lam = mixmdl$lambda[1]),
                colour = "red", lwd = 1.5) +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(mixmdl$mu[2], mixmdl$sigma[2], lam = mixmdl$lambda[2]),
                colour = "blue", lwd = 1.5) +
  ylab("Density")

mixmdl$mu # modal means
mixmdl$sigma # modal standard deviation
mixmdl$lambda # the mixing weights, e.g., red represents x% and blue represents y% of the input data

## determine which component / mode each point fall into
post.df <- as.data.frame(cbind(x = mixmdl$x, mixmdl$posterior))
post.df

post_right <- post.df[post.df$comp.1 < post.df$comp.2, ]
post_right

ggdensity(post_right, x = "x", fill = "lightgray", title = "transformed duration") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
skewness(post_right$x, na.rm = TRUE)
dip.test(post_right$x)

## conclusion: use tests robust to non-normality

# random effects linear model
fit.child <- lmer(log_duration ~ object * week + (1 | ID), data = data_no_outlier)
summary(fit.child)
anova(fit.child)
