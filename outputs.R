library(tidyverse)
library(lme4)
library(car)
library(boot)
library(lmeresampler)
source("utils.R")

load("data.RData")
load("models.RData")
models <- list(mod1,mod2,mod3,mod4) %>% 
  set_names(c("Onset of dawn singing",
              "Cessation of dusk singing",
              "Dawn chorus duration",
              "Dusk chorus duration"))
R <- 999

# Table of coefficients --------------------------------------------------------------------------------------------------------
coeftab <- lapply(names(models), function(name){
  models[[name]] %>% 
    summary %>% 
    coef %>% 
    as_tibble(rownames = "Predictor") %>% 
    set_names(c("Predictor","est","se","t")) %>% 
    mutate(
      p = 2*pnorm(abs(t), lower.tail = F),
      !!name := paste(sprintf(est, fmt='%#.3f'), 
                      " \u00b1 ", 
                      sprintf(se, fmt='%#.4f'), 
                      case_when(p < 0.001 ~ '***',
                                p < 0.01 ~ '**',
                                p < 0.05 ~ '*',
                                TRUE ~ ""),
                      sep="")) %>% 
    select(c("Predictor",name)) %>% 
    mutate(Predictor = sapply(Predictor, function(pred) ifelse(grepl(":",pred), pred, strsplit(pred, split = "_", fixed = T)[[1]][1])))
}) %>% 
  reduce(full_join)
write.table(coeftab, file = "coef_tab.csv", sep=";", row.names = F)

# Explained variance -----------------------------------------------------------------------------------------------------------
r2s <- lapply(models, function(m){
  importance(m, rterm = "(1|loc)") %>% 
    set_names(gsub("_sunrise", "", names(.))) %>% 
    set_names(gsub("_sunset", "", names(.))) %>% 
    set_names(gsub("etemp", "temp", names(.))) %>% 
    set_names(gsub("mtemp", "temp", names(.)))
}) %>% set_names(names(models)) %>% 
  bind_rows %>% 
  mutate(label = factor(names(models), levels = c("Onset of dawn singing",
                                                  "Cessation of dusk singing",
                                                  "Dawn chorus duration",
                                                  "Dusk chorus duration")))
ggplot(r2s %>% 
         pivot_longer(1:7) %>% 
         mutate(name = factor(name, levels = c("All","alan","noise_db","day","wind","cloud","humid"),
                              labels = c("All variables", "Light pollution", "Noise pollution", "Day of the year",
                                         "Wind", "Cloudiness", "Air humidity"))), 
       aes(x=name, y=value*100)) +
  geom_col() +
  facet_wrap(~label) +
  labs(y = "Explained variance (%)", x="") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1))
ggsave("explained_variance.png", width = 18, height = 14, units = "cm", dpi=300)

# Predictor significance -------------------------------------------------------------------------------------------------------
lapply(names(models), function(name){
  model <- models[[name]]
  b <- bootMer(model, FUN = function(m)
    c(getME(m, "beta"), getME(m, "sigma"), getME(m, "sigma")*getME(m, "theta")) %>% 
      setNames(c(names(coef(m)[[1]]), "sigma_res", "sigma_loc")), 
    type="parametric", nsim = R)
  lapply(c(0.95, 0.99, 0.999), function(level) confint(b, level=level, type="perc") %>% 
           as_tibble(rownames = "term") %>% 
           set_names(c("term","lower","upper")) %>% 
           mutate(!!paste0("sign",level) := lower > 0 | upper < 0)) %>% 
    reduce(left_join, by=join_by(term)) %>%
    mutate(sign = case_when(sign0.999 ~ "***",
                            sign0.99 ~ "**",
                            sign0.95 ~ "*",
                            TRUE ~ ""),
           !!name := paste0(sprintf(b$t0, fmt='%#.3f'),
                            " (",
                            sprintf(lower.x, fmt='%#.3f'),
                            ", ",
                            sprintf(upper, fmt='%#.3f'),
                            ")",
                            sign),
           term = sapply(term, function(x) ifelse(grepl(":|sigma",x), x, strsplit(x, split="_", fixed = T)[[1]][1]))) %>% 
    select(term, name)
}) %>% 
  reduce(full_join) %>%
  write.table("coeftab.csv", sep=";", row.names = F)

# Predictor effect plots -------------------------------------------------------------------------------------------------------

# Start of singing --------------------------
Anova(mod1)
ylab="Onset of dawn singing (min. relative to sunrise)"
nd <- cbind(
  expand.grid(alan = seq(min(kos_sc$alan), max(kos_sc$alan), l=100),
              day = c(min(kos_sc$day), median(kos_sc$day), max(kos_sc$day)),
              noise_db = (c(50,60,70)-mean(kos$noise_db))/sd(kos$noise_db)),
  wind_sunrise = 0,
  mtemp = 0)
bb <- bootMer(mod1, FUN=function(x) predict(x, nd, re.form=NA), nsim = R)
nd$y <- bb$t0
nd$lwr <- apply(bb$t, 2, quantile, 0.025)
nd$upr <- apply(bb$t, 2, quantile, 0.9757)
ggplot(nd, aes(x=alan*sd(kos$alan)+mean(kos$alan), y=y*sd(kos$start_rel) + mean(kos$start_rel), 
               fill=as.factor(day*sd(kos$day)+mean(kos$day)), 
               group=day)) +
  geom_line(aes(color=as.factor(day*sd(kos$day)+mean(kos$day)))) +
  geom_ribbon(aes(ymin=lwr*sd(kos$start_rel) + mean(kos$start_rel), ymax=upr*sd(kos$start_rel) + mean(kos$start_rel)), alpha=.3) +
  # geom_point(data=kos_sc, aes(y=start, color=as.factor(day)), alpha=.3) +
  facet_wrap(~paste("Noise pollution =", round((noise_db*sd(kos$noise_db) + mean(kos$noise_db)), 0), "dB")) +
  labs(x="Light pollution", y=ylab, fill="Day", color="Day") +
  theme_bw()
ggsave("start_1.png", height = 14, width = 18, units = "cm", dpi=300)

nd <- expand.grid(alan = 0,
              mtemp = 0,
              day = median(kos_sc$day),
              noise_db = (60-mean(kos$noise_db))/sd(kos$noise_db),
              wind_sunrise = seq(min(kos_sc$wind_sunrise), max(kos_sc$wind_sunrise), l=100))
bb <- bootMer(mod1, FUN=function(x) predict(x, nd, re.form=NA), nsim = R)
nd$y <- bb$t0
nd$lwr <- apply(bb$t, 2, quantile, 0.025)
nd$upr <- apply(bb$t, 2, quantile, 0.9757)
ggplot(nd, aes(x=wind_sunrise*sd(kos$wind_sunrise)+mean(kos$wind_sunrise), y=y*sd(kos$start_rel) + mean(kos$start_rel))) +
  geom_line() +
  geom_point(data=kos_sc, aes(y=start_rel*sd(kos$start_rel)+mean(kos$start_rel)), alpha = .2) +
  geom_ribbon(aes(ymin=lwr*sd(kos$start_rel) + mean(kos$start_rel), ymax=upr*sd(kos$start_rel) + mean(kos$start_rel)), alpha=.3) +
  labs(x="Wind speed (km/h)", y=ylab) +
  theme_bw()
ggsave("start_2.png", height = 12, width = 12, units = "cm", dpi=300)


# End of singing ----------------------------
Anova(mod2)
ylab = "Cessation of dusk singing (min. relative to sunset)"
nd <- expand.grid(alan = seq(min(kos_sc$alan), max(kos_sc$alan), l=100),
                  day = median(kos_sc$day),
                  press_sunset = 0,
                  wind_sunset = 0,
                  cloud = c("No clouds", "Partly cloudy", "Cloudy"))
bb <- bootMer(mod2, FUN=function(x) predict(x, nd, re.form=NA), nsim = R)
nd$y <- bb$t0
nd$lwr <- apply(bb$t, 2, quantile, 0.025)
nd$upr <- apply(bb$t, 2, quantile, 0.9757)
p1 <- ggplot(nd %>% filter(cloud=="No clouds"), 
       aes(x=alan*sd(kos$alan)+mean(kos$alan), y=y*sd(kos$end_rel, na.rm=T) + mean(kos$end_rel, na.rm=T))) +
  geom_line() +
  geom_point(data=kos_sc, aes(y=end_rel*sd(kos$end_rel, na.rm=T)+mean(kos$end_rel, na.rm=T)), alpha = .2) +
  geom_ribbon(aes(ymin=lwr*sd(kos$end_rel, na.rm=T) + mean(kos$end_rel, na.rm=T), 
                  ymax=upr*sd(kos$end_rel, na.rm=T) + mean(kos$end_rel, na.rm=T)), alpha=.3) +
  labs(x="Light pollution", y=ylab) +
  theme_bw()

nd <- expand.grid(wind_sunset = seq(min(kos_sc$wind_sunset), max(kos_sc$wind_sunset), l=100),
                  day = median(kos_sc$day),
                  press_sunset = 0,
                  alan = 0,
                  cloud = "No clouds")
bb <- bootMer(mod2, FUN=function(x) predict(x, nd, re.form=NA), nsim = R)
nd$y <- bb$t0
nd$lwr <- apply(bb$t, 2, quantile, 0.025)
nd$upr <- apply(bb$t, 2, quantile, 0.9757)
p2 <- ggplot(nd,
             aes(x=wind_sunset*sd(kos$wind_sunset)+mean(kos$wind_sunset), 
                 y=y*sd(kos$end_rel, na.rm=T) + mean(kos$end_rel, na.rm=T))) +
  geom_line() +
  geom_point(data=kos_sc, aes(y=end_rel*sd(kos$end_rel, na.rm=T)+mean(kos$end_rel, na.rm=T)), alpha = .2) +
  geom_ribbon(aes(ymin=lwr*sd(kos$end_rel, na.rm=T) + mean(kos$end_rel, na.rm=T), 
                  ymax=upr*sd(kos$end_rel, na.rm=T) + mean(kos$end_rel, na.rm=T)), alpha=.3) +
  labs(x="Wind speed (km/h)", y=ylab) +
  theme_bw()

nd <- expand.grid(day = seq(min(kos_sc$day), max(kos_sc$day), l=100),
                  wind_sunset = 0,
                  press_sunset = 0,
                  alan = 0,
                  cloud = "No clouds")
bb <- bootMer(mod2, FUN=function(x) predict(x, nd, re.form=NA), nsim = R)
nd$y <- bb$t0
nd$lwr <- apply(bb$t, 2, quantile, 0.025)
nd$upr <- apply(bb$t, 2, quantile, 0.9757)
p3 <- ggplot(nd,
             aes(x=day*sd(kos$day)+mean(kos$day), 
                 y=y*sd(kos$end_rel, na.rm=T) + mean(kos$end_rel, na.rm=T))) +
  geom_line() +
  geom_point(data=kos_sc, aes(y=end_rel*sd(kos$end_rel, na.rm=T)+mean(kos$end_rel, na.rm=T)), alpha = .2) +
  geom_ribbon(aes(ymin=lwr*sd(kos$end_rel, na.rm=T) + mean(kos$end_rel, na.rm=T), 
                  ymax=upr*sd(kos$end_rel, na.rm=T) + mean(kos$end_rel, na.rm=T)), alpha=.3) +
  labs(x="Day", y=ylab) +
  theme_bw()
cowplot::plot_grid(p1,p2,p3)
ggsave("end.png", height = 21, width = 21, units = "cm", dpi=300)

# Morning intensity -------------------------
Anova(mod3)
ylab="Dawn chorus duration (minutes)"
nd <- cbind(
  expand.grid(alan = seq(min(kos_sc$alan), max(kos_sc$alan), l=100),
              noise_db = (c(50,60,70)-mean(kos$noise_db))/sd(kos$noise_db)),
  humid_sunrise = 0,
  press_sunrise = 0,
  day = 0)
bb <- bootMer(mod3, FUN=function(x) predict(x, nd, re.form=NA), nsim = 999)
nd$y <- bb$t0
nd$lwr <- apply(bb$t, 2, quantile, 0.025)
nd$upr <- apply(bb$t, 2, quantile, 0.9757)
ggplot(nd, aes(x=alan*sd(kos$alan) + mean(kos$alan), y=y*sd(kos$l_morning) + mean(kos$l_morning), 
               fill=as.factor(round(noise_db*sd(kos$noise_db)+mean(kos$noise_db), 1)), 
               group=noise_db)) +
  geom_line(aes(color=as.factor(round(noise_db*sd(kos$noise_db)+mean(kos$noise_db),1)))) +
  geom_ribbon(aes(ymin=lwr*sd(kos$l_morning) + mean(kos$l_morning), ymax=upr*sd(kos$l_morning) + mean(kos$l_morning)), alpha=.3) +
  # geom_point(data=kos_sc, aes(y=l_morning, color=as.factor(noise_db)), alpha=.3) +
  labs(x="Light pollution", y=ylab, fill="Noise pollution", color="Noise pollution") +
  theme_bw()
ggsave("l_morning_1.png", height = 10, width = 14, units = "cm", dpi=300)

nd <- expand.grid(day = seq(min(kos_sc$day), max(kos_sc$day), l=100),
                  noise_db = 0,
                  humid_sunrise = 0,
                  press_sunrise = 0,
                  alan=0)
bb <- bootMer(mod3, FUN=function(x) predict(x, nd, re.form=NA), nsim = 999)
nd$y <- bb$t0
nd$lwr <- apply(bb$t, 2, quantile, 0.025)
nd$upr <- apply(bb$t, 2, quantile, 0.9757)
ggplot(nd, aes(x=day*sd(kos$day) + mean(kos$day), y=y*sd(kos$l_morning) + mean(kos$l_morning))) +
  geom_line() +
  geom_ribbon(aes(ymin=lwr*sd(kos$l_morning) + mean(kos$l_morning), ymax=upr*sd(kos$l_morning) + mean(kos$l_morning)), alpha=.3) +
  geom_point(data=kos_sc, aes(y=l_morning*sd(kos$l_morning) + mean(kos$l_morning)), alpha=.3) +
  labs(x="Day", y=ylab) +
  theme_bw()
ggsave("l_morning_2.png", height = 12, width = 12, units = "cm", dpi=300)

# Evening intensity -------------------------
Anova(mod4)
ylab="Dusk chorus duration (minutes)"
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
(p1 <- ggplot(nd, aes(x=alan*sd(kos$alan) + mean(kos$alan), y=y*sd(kos$l_evening) + mean(kos$l_evening), 
               fill=as.factor(day*sd(kos$day)+mean(kos$day)), 
               group=day)) +
  geom_line(aes(color=as.factor(day*sd(kos$day)+mean(kos$day)))) +
  geom_ribbon(aes(ymin=lwr*sd(kos$l_evening) + mean(kos$l_evening), ymax=upr*sd(kos$l_evening) + mean(kos$l_evening)), alpha=.3) +
  # geom_point(data=kos_sc, aes(y=l_evening, color=as.factor(day)), alpha=.3) +
  labs(x="Light pollution", y=ylab, fill="Day", color="Day") +
  theme_bw())
ggsave("l_evening_1.png", height = 10, width = 14, units = "cm", dpi=300)
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
(p2 <- ggplot(nd, aes(x=noise_db*sd(kos$noise_db) + mean(kos$noise_db), y=y*sd(kos$l_evening) + mean(kos$l_evening))) +
  geom_line() +
  geom_ribbon(aes(ymin=lwr*sd(kos$l_evening) + mean(kos$l_evening), ymax=upr*sd(kos$l_evening) + mean(kos$l_evening)), alpha=.3) +
  # geom_point(data=kos_sc, aes(y=l_evening, color=as.factor(day)), alpha=.3) +
  labs(x="Noise pollution (dB)", y=ylab) +
  theme_bw())
ggsave("l_evening_2.png", height = 10, width = 14, units = "cm", dpi=300)
ggsave("l_evening_both.png", plot = cowplot::plot_grid(p1,p2,nrow=1), height = 10, width = 18, units = "cm", dpi=300)

nd <- data.frame(
  noise_db = 0,
  alan=0,
  day=0,
  press_sunset = 0,
  humid_sunset = seq(min(kos_sc$humid_sunset), max(kos_sc$humid_sunset), l=100),
  cloud = "No clouds")
bb <- bootMer(mod4, FUN=function(x) predict(x, nd, re.form=NA), nsim = 999)
nd$y <- bb$t0
nd$lwr <- apply(bb$t, 2, quantile, 0.025)
nd$upr <- apply(bb$t, 2, quantile, 0.9757)
ggplot(nd, aes(x=humid_sunset*sd(kos$humid_sunset) + mean(kos$humid_sunset), y=y*sd(kos$l_evening) + mean(kos$l_evening))) +
    geom_line() +
    geom_ribbon(aes(ymin=lwr*sd(kos$l_evening) + mean(kos$l_evening), ymax=upr*sd(kos$l_evening) + mean(kos$l_evening)), alpha=.3) +
    geom_point(data=kos_sc, aes(y=l_evening*sd(kos$l_evening) + mean(kos$l_evening)), alpha=.3) +
    labs(x="Air humidity (%)", y=ylab) +
    theme_bw()
ggsave("l_evening_3.png", height = 12, width = 12, units = "cm", dpi=300)
