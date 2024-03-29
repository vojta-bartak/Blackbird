library(tidyverse)

kosL <- read.table("data_long.txt", sep='\t', header=T) %>% 
  full_join(read.table("data_long_extended.txt", sep='\t', header=T)) %>% 
  mutate(
    intervaly = strptime(intervaly, format="%H:%M")
  )
kos <- read.table("data_short.txt", sep='\t', header=T) %>% 
  mutate_at(c("start","end","sunrise","sunset"), strptime, format="%H:%M") %>% 
  mutate(
    day = strptime(datum, format="%d.%m.%Y")$yday,
    start_rel = as.numeric(difftime(start, sunrise, units = "mins")),
    end_rel = as.numeric(difftime(end, sunset, units = "mins"))
  )
kos <- kos %>% 
  left_join(
    kosL %>% 
      left_join(kos %>% select(ID, sunrise, sunset)) %>% 
      group_by(ID) %>% 
      summarize(l_morning = sum(song[intervaly >= sunrise - 3*3600 & intervaly < sunrise + 15*60]),
                l_evening = sum(song[intervaly >= sunset & intervaly <= sunset + 3*3600]),
                cloud = factor(mean(oblacnost), levels = 0:2, labels = c("No clouds", "Partly cloudy", "Cloudy")),
                temp_sunrise = teplota[abs(intervaly-sunrise) == min(abs(intervaly-sunrise))],
                temp_sunset = teplota[abs(intervaly-sunset) == min(abs(intervaly-sunset))],
                wind_sunrise = vitr[abs(intervaly-sunrise) == min(abs(intervaly-sunrise))],
                wind_sunset = vitr[abs(intervaly-sunset) == min(abs(intervaly-sunset))],
                humid_sunrise = vlhkost[abs(intervaly-sunrise) == min(abs(intervaly-sunrise))],
                humid_sunset = vlhkost[abs(intervaly-sunset) == min(abs(intervaly-sunset))],
                press_sunrise = tlak[abs(intervaly-sunrise) == min(abs(intervaly-sunrise))],
                press_sunset = tlak[abs(intervaly-sunset) == min(abs(intervaly-sunset))],
                prec_sunrise = dest[abs(intervaly-sunrise) == min(abs(intervaly-sunrise))],
                prec_sunset = dest[abs(intervaly-sunset) == min(abs(intervaly-sunset))])
  ) %>% 
  mutate(mtemp = lm(temp_sunrise~day, .)$residuals,
         etemp = lm(temp_sunset~day, .)$residuals) %>% 
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

save(kos, file = "data.RData")
