library(boot)
library(tidyverse)
library(car)
library(lme4)
source("utils.R")
source("stepAIC.R")

# Load data
load("kos.RData")
kos <- kos %>% 
  left_join(read.table("kos_data.csv", sep=",", header=T) %>% 
              mutate(ID = ID.mereni,
                     loc = ID.lokality,
                     moonlight = X..moon.illumination,
                     alan = as.numeric(gsub(",", ".", ALAN)),
                     noise.db = hluk.db,
                     noise_db = case_when(noise.db == "< 50" ~ 50,
                                          noise.db %in% c("50-55", "50 - 55") ~ 55,
                                          noise.db %in% c("55 - 60", "55 - 60 ") ~ 60,
                                          noise.db == "60 - 65" ~ 65,
                                          noise.db == "65 - 70" ~ 70,
                                          noise.db == "70 - 75" ~ 75),
                     noise_fac = as.factor(noise_db)) %>% 
              select(ID, loc, GPS, noise_db, noise_fac, alan, moonlight, moon.phase))
kos_sc <- kos %>% mutate_if(is.numeric, scale)

# Set the number of bootstrap samples
R <- 2000

# Start of singing -----------------------------------------------------------------------------------------------------------
full1 <- lmer(start ~ alan + noise_fac + day + alan:noise_fac + alan:day + noise_fac:day +
                mtemp+prec_sunrise+press_sunrise+humid_sunrise+wind_sunrise+cloud+moonlight +
                (1|loc),
              data=kos_sc)
full2 <- lmer(start ~ alan + noise_db + day + alan:noise_db + alan:day + noise_db:day +
                mtemp+prec_sunrise+press_sunrise+humid_sunrise+wind_sunrise+cloud+moonlight +
                (1|loc),
              data=kos_sc)
AIC(full1, full2)
red1 <- stepAIC(full1)
red2 <- stepAIC(full2)
AIC(red1, red2)
mod1 <- red2
print(formula(mod1))
(s <- summary(mod1))
car::Anova(mod1)
performance::r2(mod1)
round(importance(mod1, rterm="(1|loc)"), 2)
round(importance2(mod1), 2)

# Light pollution : Day
nd <- cbind(
  expand.grid(alan = seq(min(kos_sc$alan), max(kos_sc$alan), l=100),
              day = c(min(kos_sc$day), median(kos_sc$day), max(kos_sc$day))),
  noise_db = 0,
  wind_sunrise = 0,
  mtemp = 0)
bb <- bootMer(mod1, FUN=function(x) predict(x, nd, re.form=NA), nsim = 999)
nd$y <- bb$t0
nd$lwr <- apply(bb$t, 2, quantile, 0.025)
nd$upr <- apply(bb$t, 2, quantile, 0.9757)
ylab="Onset of dawn singing (min. relative to sunrise)"
ggplot(nd, aes(x=alan*sd(kos$alan) + mean(kos$alan), y=y*sd(kos$start) + mean(kos$start), 
                                      fill=as.factor(day*sd(kos$day)+mean(kos$day)), 
                                      group=day)) +
  geom_line(aes(color=as.factor(day*sd(kos$day)+mean(kos$day)))) +
  geom_ribbon(aes(ymin=lwr*sd(kos$start) + mean(kos$start), ymax=upr*sd(kos$start) + mean(kos$start)), alpha=.3) +
  # geom_point(data=kos_sc, aes(y=start, color=as.factor(day)), alpha=.3) +
  labs(x="Light pollution", y=ylab, fill="Day", color="Day") +
  theme_bw()

# Noise pollution : Day
nd <- cbind(
  expand.grid(noise_db = seq(min(kos_sc$noise_db), max(kos_sc$noise_db), l=100),
              day = c(min(kos_sc$day), median(kos_sc$day), max(kos_sc$day))),
  alan = 0,
  wind_sunrise = 0,
  mtemp = 0)
bb <- bootMer(mod1, FUN=function(x) predict(x, nd, re.form=NA), nsim = 999)
nd$y <- bb$t0
nd$lwr <- apply(bb$t, 2, quantile, 0.025)
nd$upr <- apply(bb$t, 2, quantile, 0.9757)
ggplot(nd, aes(x=noise_db*sd(kos$noise_db), y=y*sd(kos$start) + mean(kos$start), 
               fill=as.factor(day*sd(kos$day)+mean(kos$day)), 
               group=day)) +
  geom_line(aes(color=as.factor(day*sd(kos$day)+mean(kos$day)))) +
  geom_ribbon(aes(ymin=lwr*sd(kos$start) + mean(kos$start), ymax=upr*sd(kos$start) + mean(kos$start)), alpha=.3) +
  # geom_point(data=kos_sc, aes(y=start, color=as.factor(day)), alpha=.3) +
  labs(x="Noise pollution (dB)", y=ylab, fill="Day", color="Day") +
  theme_bw()

# Light pollution : Noise pollution
nd <- cbind(
  expand.grid(alan = seq(min(kos_sc$alan), max(kos_sc$alan), l=100),
              noise_db = c(-1,0,1)),
  day = 0,
  wind_sunrise = 0,
  mtemp = 0)
bb <- bootMer(mod1, FUN=function(x) predict(x, nd, re.form=NA), nsim = 999)
nd$y <- bb$t0
nd$lwr <- apply(bb$t, 2, quantile, 0.025)
nd$upr <- apply(bb$t, 2, quantile, 0.9757)
ggplot(nd, aes(x=alan*sd(kos$alan), y=y*sd(kos$start) + mean(kos$start), 
               fill=as.factor(round(noise_db*sd(kos$noise_db)+mean(kos$noise_db), 1)), 
               group=noise_db)) +
  geom_line(aes(color=as.factor(round(noise_db*sd(kos$noise_db)+mean(kos$noise_db),1)))) +
  geom_ribbon(aes(ymin=lwr*sd(kos$start) + mean(kos$start), ymax=upr*sd(kos$start) + mean(kos$start)), alpha=.3) +
  # geom_point(data=kos_sc, aes(y=start, color=as.factor(noise_db)), alpha=.3) +
  labs(x="Light pollution", y=ylab, fill="Noise pollution", color="Noise pollution") +
  theme_bw()

# Light pollution : Day : Noise pollution
nd <- cbind(
  expand.grid(alan = seq(min(kos_sc$alan), max(kos_sc$alan), l=100),
              day = c(min(kos_sc$day), median(kos_sc$day), max(kos_sc$day)),
              noise_db = (c(50,60,70)-mean(kos$noise_db))/sd(kos$noise_db)),
  wind_sunrise = 0,
  mtemp = 0)
bb <- bootMer(mod1, FUN=function(x) predict(x, nd, re.form=NA), nsim = 999)
nd$y <- bb$t0
nd$lwr <- apply(bb$t, 2, quantile, 0.025)
nd$upr <- apply(bb$t, 2, quantile, 0.9757)
ylab="Onset of dawn singing (min. relative to sunrise)"
ggplot(nd, aes(x=alan*sd(kos$alan)+mean(kos$alan), y=y*sd(kos$start) + mean(kos$start), 
               fill=as.factor(day*sd(kos$day)+mean(kos$day)), 
               group=day)) +
  geom_line(aes(color=as.factor(day*sd(kos$day)+mean(kos$day)))) +
  geom_ribbon(aes(ymin=lwr*sd(kos$start) + mean(kos$start), ymax=upr*sd(kos$start) + mean(kos$start)), alpha=.3) +
  # geom_point(data=kos_sc, aes(y=start, color=as.factor(day)), alpha=.3) +
  facet_wrap(~paste("Noise pollution =", round((noise_db*sd(kos$noise_db) + mean(kos$noise_db)), 0), "dB")) +
  labs(x="Light pollution", y=ylab, fill="Day", color="Day") +
  theme_bw()
ggsave("all.png", height = 18, width = 24, units = "cm", dpi=300)

# End of singing -------------------------------------------------------------------------------------------------------------
full1 <- lmer(end ~ alan + noise_fac + day + alan:noise_fac + alan:day + noise_fac:day +
                etemp+prec_sunset+press_sunset+humid_sunset+wind_sunset+cloud+moonlight +
                (1|loc),
              data=kos_sc)
full2 <- lmer(end ~ alan + noise_db + day + alan:noise_db + alan:day + noise_db:day +
                etemp+prec_sunset+press_sunset+humid_sunset+wind_sunset+cloud+moonlight +
                (1|loc),
              data=kos_sc)
AIC(full1, full2)
red1 <- stepAIC(full1)
red2 <- stepAIC(full2)
AIC(red1, red2)
mod2 <- red2
print(formula(mod2))
(s <- summary(mod2))
car::Anova(mod2)
performance::r2(mod2)
round(importance(red1, rterm="(1|loc)"), 2)
round(importance2(red1), 2)

# Morning length -------------------------------------------------------------------------------------------------------------
full1 <- lmer(l_morning ~ alan + noise_fac + day + alan:noise_fac + alan:day + noise_fac:day +
                mtemp+prec_sunrise+press_sunrise+humid_sunrise+wind_sunrise+cloud+moonlight +
                (1|loc),
              data=kos_sc)
full2 <- lmer(l_morning ~ alan + noise_db + day + alan:noise_db + alan:day + noise_db:day +
                mtemp+prec_sunrise+press_sunrise+humid_sunrise+wind_sunrise+cloud+moonlight +
                (1|loc),
              data=kos_sc)
AIC(full1, full2)
red1 <- stepAIC(full1)
red2 <- stepAIC(full2)
AIC(red1, red2)
mod3 <- red2
print(formula(mod3))
(s <- summary(mod3))
car::Anova(mod3)
performance::r2(mod3)
round(importance(mod3, rterm="(1|loc)"), 2)
round(importance2(mod3), 2)

# Light pollution : Noise pollution
ylab="Dawn chorus duration (minutes)"
nd <- cbind(
  expand.grid(alan = seq(min(kos_sc$alan), max(kos_sc$alan), l=100),
              noise_db = (c(50,60,70)-mean(kos$noise_db))/sd(kos$noise_db)),
  press_sunrise = mean(kos_sc$humid_sunrise),
  day = 0)
bb <- bootMer(mod3, FUN=function(x) predict(x, nd, re.form=NA), nsim = 999)
nd$y <- bb$t0
nd$lwr <- apply(bb$t, 2, quantile, 0.025)
nd$upr <- apply(bb$t, 2, quantile, 0.9757)
ggplot(nd, aes(x=alan*sd(kos$alan), y=y*sd(kos$l_morning) + mean(kos$l_morning), 
               fill=as.factor(round(noise_db*sd(kos$noise_db)+mean(kos$noise_db), 1)), 
               group=noise_db)) +
  geom_line(aes(color=as.factor(round(noise_db*sd(kos$noise_db)+mean(kos$noise_db),1)))) +
  geom_ribbon(aes(ymin=lwr*sd(kos$l_morning) + mean(kos$l_morning), ymax=upr*sd(kos$l_morning) + mean(kos$l_morning)), alpha=.3) +
  # geom_point(data=kos_sc, aes(y=l_morning, color=as.factor(noise_db)), alpha=.3) +
  labs(x="Light pollution", y=ylab, fill="Noise pollution", color="Noise pollution") +
  theme_bw()

# ...
nd <- cbind(
  expand.grid(alan = seq(min(kos_sc$alan), max(kos_sc$alan), l=100),
              day = c(min(kos_sc$day), median(kos_sc$day), max(kos_sc$day)),
              noise_db = (c(50,60,70)-mean(kos$noise_db))/sd(kos$noise_db)),
  press_sunrise = 0)
bb <- bootMer(mod3, FUN=function(x) predict(x, nd, re.form=NA), nsim = 999)
nd$y <- bb$t0
nd$lwr <- apply(bb$t, 2, quantile, 0.025)
nd$upr <- apply(bb$t, 2, quantile, 0.9757)
ylab="Onset of dawn singing (min. relative to sunrise)"
ggplot(nd, aes(x=alan*sd(kos$alan)+mean(kos$alan), y=y*sd(kos$l_morning) + mean(kos$l_morning), 
               fill=as.factor(day*sd(kos$day)+mean(kos$day)), 
               group=day)) +
  geom_line(aes(color=as.factor(day*sd(kos$day)+mean(kos$day)))) +
  geom_ribbon(aes(ymin=lwr*sd(kos$l_morning) + mean(kos$l_morning), ymax=upr*sd(kos$l_morning) + mean(kos$l_morning)), alpha=.3) +
  # geom_point(data=kos_sc, aes(y=l_morning, color=as.factor(day)), alpha=.3) +
  facet_wrap(~paste("Noise pollution =", round((noise_db*sd(kos$noise_db) + mean(kos$noise_db)), 0), "dB")) +
  labs(x="Light pollution", y=ylab, fill="Day", color="Day") +
  theme_bw()

# Evening length -------------------------------------------------------------------------------------------------------------
# full model
full1 <- lmer(l_evening ~ alan + noise_fac + day + alan:noise_fac + alan:day + noise_fac:day +
                etemp+prec_sunset+press_sunset+humid_sunset+wind_sunset+cloud+moonlight +
                (1|loc),
              data=kos_sc)
full2 <- lmer(l_evening ~ alan + noise_db + day + alan:noise_db + alan:day + noise_db:day +
                etemp+prec_sunset+press_sunset+humid_sunset+wind_sunset+cloud+moonlight +
                (1|loc),
              data=kos_sc)
AIC(full1, full2)
red1 <- stepAIC(full1)
red2 <- stepAIC(full2)
AIC(red1, red2)
mod4 <- red2
print(formula(mod4))
(s <- summary(mod4))
car::Anova(mod4)
performance::r2(mod4)
round(importance(mod4, rterm="(1|loc)"), 2)
round(importance2(mod4), 2)

# Light pollution : Day
nd <- cbind(
  expand.grid(alan = seq(min(kos_sc$alan), max(kos_sc$alan), l=100),
              day = c(min(kos_sc$day), median(kos_sc$day), max(kos_sc$day))),
  noise_db = 0,
  press_sunset = 0,
  humid_sunset = 0,
  cloud = "Partly cloudy")
bb <- bootMer(mod4, FUN=function(x) predict(x, nd, re.form=NA), nsim = 999)
nd$y <- bb$t0
nd$lwr <- apply(bb$t, 2, quantile, 0.025)
nd$upr <- apply(bb$t, 2, quantile, 0.9757)
ylab="Dusk chorus duration (minutes)"
ggplot(nd, aes(x=alan*sd(kos$alan), y=y*sd(kos$l_evening) + mean(kos$l_evening), 
               fill=as.factor(day*sd(kos$day)+mean(kos$day)), 
               group=day)) +
  geom_line(aes(color=as.factor(day*sd(kos$day)+mean(kos$day)))) +
  geom_ribbon(aes(ymin=lwr*sd(kos$l_evening) + mean(kos$l_evening), ymax=upr*sd(kos$l_evening) + mean(kos$l_evening)), alpha=.3) +
  # geom_point(data=kos_sc, aes(y=l_evening, color=as.factor(day)), alpha=.3) +
  labs(x="Light pollution", y=ylab, fill="Day", color="Day") +
  theme_bw()

# Noise pollution
nd <- data.frame(
  noise_db = seq(min(kos_sc$noise_db), max(kos_sc$noise_db), l=100),
  alan=0,
  day=0,
  press_sunset = 0,
  humid_sunset = 0,
  cloud = "Partly cloudy")
bb <- bootMer(mod4, FUN=function(x) predict(x, nd, re.form=NA), nsim = 999)
nd$y <- bb$t0
nd$lwr <- apply(bb$t, 2, quantile, 0.025)
nd$upr <- apply(bb$t, 2, quantile, 0.9757)
ylab="Dusk chorus duration (minutes)"
ggplot(nd, aes(x=noise_db*sd(kos$noise_db), y=y*sd(kos$l_evening) + mean(kos$l_evening))) +
  geom_line() +
  geom_ribbon(aes(ymin=lwr*sd(kos$l_evening) + mean(kos$l_evening), ymax=upr*sd(kos$l_evening) + mean(kos$l_evening)), alpha=.3) +
  # geom_point(data=kos_sc, aes(y=l_evening, color=as.factor(day)), alpha=.3) +
  labs(x="Noise pollution (dB)", y=ylab) +
  theme_bw()

# Morning intensity ----------------------------------------------------------------------------------------------------------
full1 <- lmer(i_morning ~ alan + noise_fac + day + alan:noise_fac + alan:day + noise_fac:day +
                mtemp+prec_sunrise+press_sunrise+humid_sunrise+wind_sunrise+cloud+moonlight +
                (1|loc),
              data=kos_sc)
full2 <- lmer(i_morning ~ alan + noise_db + day + alan:noise_db + alan:day + noise_db:day +
                mtemp+prec_sunrise+press_sunrise+humid_sunrise+wind_sunrise+cloud+moonlight +
                (1|loc),
              data=kos_sc)
AIC(full1, full2)
red1 <- stepAIC(full1)
red2 <- stepAIC(full2)
AIC(red1, red2)
mod5 <- red2
print(formula(mod5))
(s <- summary(mod5))
car::Anova(mod5)
performance::r2(mod5)
round(importance(mod5, rterm="(1|loc)"), 2)
round(importance2(mod5), 2)

# Light pollution : Noise pollution
ylab="Dawn chorus duration (minutes)"
nd <- cbind(
  expand.grid(alan = seq(min(kos_sc$alan), max(kos_sc$alan), l=100),
              noise_db = (c(50,60,70)-mean(kos$noise_db))/sd(kos$noise_db)),
  press_sunrise = mean(kos_sc$humid_sunrise),
  day = 0)
bb <- bootMer(mod5, FUN=function(x) predict(x, nd, re.form=NA), nsim = 999)
nd$y <- bb$t0
nd$lwr <- apply(bb$t, 2, quantile, 0.025)
nd$upr <- apply(bb$t, 2, quantile, 0.9757)
ggplot(nd, aes(x=alan*sd(kos$alan), y=y*sd(kos$l_morning) + mean(kos$l_morning), 
               fill=as.factor(round(noise_db*sd(kos$noise_db)+mean(kos$noise_db), 1)), 
               group=noise_db)) +
  geom_line(aes(color=as.factor(round(noise_db*sd(kos$noise_db)+mean(kos$noise_db),1)))) +
  geom_ribbon(aes(ymin=lwr*sd(kos$l_morning) + mean(kos$l_morning), ymax=upr*sd(kos$l_morning) + mean(kos$l_morning)), alpha=.3) +
  # geom_point(data=kos_sc, aes(y=l_morning, color=as.factor(noise_db)), alpha=.3) +
  labs(x="Light pollution", y=ylab, fill="Noise pollution", color="Noise pollution") +
  theme_bw()

# ...
nd <- cbind(
  expand.grid(alan = seq(min(kos_sc$alan), max(kos_sc$alan), l=100),
              day = c(min(kos_sc$day), median(kos_sc$day), max(kos_sc$day)),
              noise_db = (c(50,60,70)-mean(kos$noise_db))/sd(kos$noise_db)),
  press_sunrise = 0)
bb <- bootMer(mod5, FUN=function(x) predict(x, nd, re.form=NA), nsim = 999)
nd$y <- bb$t0
nd$lwr <- apply(bb$t, 2, quantile, 0.025)
nd$upr <- apply(bb$t, 2, quantile, 0.9757)
ylab="Onset of dawn singing (min. relative to sunrise)"
ggplot(nd, aes(x=alan*sd(kos$alan)+mean(kos$alan), y=y*sd(kos$l_morning) + mean(kos$l_morning), 
               fill=as.factor(day*sd(kos$day)+mean(kos$day)), 
               group=day)) +
  geom_line(aes(color=as.factor(day*sd(kos$day)+mean(kos$day)))) +
  geom_ribbon(aes(ymin=lwr*sd(kos$l_morning) + mean(kos$l_morning), ymax=upr*sd(kos$l_morning) + mean(kos$l_morning)), alpha=.3) +
  # geom_point(data=kos_sc, aes(y=l_morning, color=as.factor(day)), alpha=.3) +
  facet_wrap(~paste("Noise pollution =", round((noise_db*sd(kos$noise_db) + mean(kos$noise_db)), 0), "dB")) +
  labs(x="Light pollution", y=ylab, fill="Day", color="Day") +
  theme_bw()

# Evening intensity ----------------------------------------------------------------------------------------------------------
# full model
full1 <- lmer(i_evening ~ alan + noise_fac + day + alan:noise_fac + alan:day + noise_fac:day +
                etemp+prec_sunset+press_sunset+humid_sunset+wind_sunset+cloud+moonlight +
                (1|loc),
              data=kos_sc)
full2 <- lmer(i_evening ~ alan + noise_db + day + alan:noise_db + alan:day + noise_db:day +
                etemp+prec_sunset+press_sunset+humid_sunset+wind_sunset+cloud+moonlight +
                (1|loc),
              data=kos_sc)
AIC(full1, full2)
red1 <- stepAIC(full1)
red2 <- stepAIC(full2)
AIC(red1, red2)
mod6 <- red2
print(formula(mod6))
(s <- summary(mod6))
car::Anova(mod6)
performance::r2(mod6)
round(importance(mod6, rterm="(1|loc)"), 2)
round(importance2(mod6), 2)

# Light pollution : Day
nd <- cbind(
  expand.grid(alan = seq(min(kos_sc$alan), max(kos_sc$alan), l=100),
              day = c(min(kos_sc$day), median(kos_sc$day), max(kos_sc$day))),
  noise_db = 0,
  press_sunset = 0,
  humid_sunset = 0,
  cloud = "Partly cloudy")
bb <- bootMer(mod6, FUN=function(x) predict(x, nd, re.form=NA), nsim = 999)
nd$y <- bb$t0
nd$lwr <- apply(bb$t, 2, quantile, 0.025)
nd$upr <- apply(bb$t, 2, quantile, 0.9757)
ylab="Dusk chorus duration (minutes)"
ggplot(nd, aes(x=alan*sd(kos$alan), y=y*sd(kos$l_evening) + mean(kos$l_evening), 
               fill=as.factor(day*sd(kos$day)+mean(kos$day)), 
               group=day)) +
  geom_line(aes(color=as.factor(day*sd(kos$day)+mean(kos$day)))) +
  geom_ribbon(aes(ymin=lwr*sd(kos$l_evening) + mean(kos$l_evening), ymax=upr*sd(kos$l_evening) + mean(kos$l_evening)), alpha=.3) +
  # geom_point(data=kos_sc, aes(y=l_evening, color=as.factor(day)), alpha=.3) +
  labs(x="Light pollution", y=ylab, fill="Day", color="Day") +
  theme_bw()

# Noise pollution
nd <- data.frame(
  noise_db = seq(min(kos_sc$noise_db), max(kos_sc$noise_db), l=100),
  alan=0,
  day=0,
  press_sunset = 0,
  humid_sunset = 0,
  cloud = "Partly cloudy")
bb <- bootMer(mod6, FUN=function(x) predict(x, nd, re.form=NA), nsim = 999)
nd$y <- bb$t0
nd$lwr <- apply(bb$t, 2, quantile, 0.025)
nd$upr <- apply(bb$t, 2, quantile, 0.9757)
ylab="Dusk chorus duration (minutes)"
ggplot(nd, aes(x=noise_db*sd(kos$noise_db), y=y*sd(kos$l_evening) + mean(kos$l_evening))) +
  geom_line() +
  geom_ribbon(aes(ymin=lwr*sd(kos$l_evening) + mean(kos$l_evening), ymax=upr*sd(kos$l_evening) + mean(kos$l_evening)), alpha=.3) +
  # geom_point(data=kos_sc, aes(y=l_evening, color=as.factor(day)), alpha=.3) +
  labs(x="Noise pollution (dB)", y=ylab) +
  theme_bw()
