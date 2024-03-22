library(tidyverse)
library(lme4)
library(car)
source("utils.R")

load("data.RData")
kos_sc <- kos %>% mutate_if(is.numeric, scale)

# Multicollineariry check
vif(lm(start_rel ~ alan + noise_db + day + alan:noise_db + alan:day + noise_db:day +
         temp_sunrise+prec_sunrise+press_sunrise+humid_sunrise+wind_sunrise+cloud+moonlight, data=kos_sc))
vif(lm(start_rel ~ alan + noise_db + day + alan:noise_db + alan:day + noise_db:day +
         mtemp+prec_sunrise+press_sunrise+humid_sunrise+wind_sunrise+cloud+moonlight, data=kos_sc))
vif(lm(start_rel ~ alan + noise_db + day + alan:noise_db + alan:day + noise_db:day +
         temp_sunset+prec_sunset+press_sunset+humid_sunset+wind_sunset+cloud+moonlight, data=kos_sc))
vif(lm(start_rel ~ alan + noise_db + day + alan:noise_db + alan:day + noise_db:day +
         etemp+prec_sunset+press_sunset+humid_sunset+wind_sunset+cloud+moonlight, data=kos_sc))

# Start of singing -----------------------------------------------------------------------------------------------------------
mod1 <- lmer(start_rel ~ alan + noise_db + day + alan:noise_db + alan:day + noise_db:day +
                mtemp+prec_sunrise+press_sunrise+humid_sunrise+wind_sunrise+cloud+moonlight +
                (1|loc),
              data=kos_sc) %>% 
  stepAIC()

# End of singing -------------------------------------------------------------------------------------------------------------
mod2 <- lmer(end_rel ~ alan + noise_db + day + alan:noise_db + alan:day + noise_db:day +
                etemp+prec_sunset+press_sunset+humid_sunset+wind_sunset+cloud+moonlight +
                (1|loc),
              data=kos_sc) %>% 
  stepAIC()

# Morning length -------------------------------------------------------------------------------------------------------------
mod3 <- lmer(l_morning ~ alan + noise_db + day + alan:noise_db + alan:day + noise_db:day +
                mtemp+prec_sunrise+press_sunrise+humid_sunrise+wind_sunrise+cloud+moonlight +
                (1|loc),
              data=kos_sc) %>% 
  stepAIC()

# Evening length -------------------------------------------------------------------------------------------------------------
mod4 <- lmer(l_evening ~ alan + noise_db + day + alan:noise_db + alan:day + noise_db:day +
                etemp+prec_sunset+press_sunset+humid_sunset+wind_sunset+cloud+moonlight +
                (1|loc),
              data=kos_sc) %>% 
  stepAIC()
isSingular(mod4)

save(mod1, mod2, mod3, mod4, file = "models.RData")
