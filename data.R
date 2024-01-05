require(plyr)

# Zakladni datove sady
# -----------------------------------------------------------------------------------------
kosL <- read.table("data_long.txt", sep='\t', header=T)
kosLe <- read.table("data_long_extended.txt", sep='\t', header=T)
kosL$intervaly <- strptime(kosL$intervaly, format="%H:%M")
kosLe$intervaly <- strptime(kosLe$intervaly, format="%H:%M")
kosS <- read.table("data_short.txt", sep='\t', header=T)
kosS$start <- strptime(kosS$start, format="%H:%M")
kosS$end <- strptime(kosS$end, format="%H:%M")
kosS$sunrise <- strptime(kosS$sunrise, format="%H:%M")
kosS$sunset <- strptime(kosS$sunset, format="%H:%M")
kosS$datum <- strptime(kosS$datum, format="%d.%m.%Y")
kosS$day <- kosS$datum$yday


ggplot(kosL, aes(x=as.POSIXct(intervaly))) +
  geom_histogram()

ggplot(kosLe, aes(x=as.POSIXct(intervaly))) +
  geom_histogram()

ggplot(kosL[kosL$ID %in% kosLe$ID,], aes(x=as.POSIXct(intervaly), y=song)) +
  geom_col(color="yellow") +
  geom_col(data=kosLe, alpha=.5) +
  geom_segment(data = kosS[kosS$ID %in% kosLe$ID,], 
               aes(x=as.POSIXct(sunrise), xend=as.POSIXct(sunrise), y=0, yend=10), lty=2, color="red") +
  facet_wrap(~ID)

unique(kosL$ID)
unique(kosLe$ID)

# Vysvetlovane promenne
# -----------------------------------------------------------------------------------------
kos <- kosS

# zacatek a konec zpevu
kos$start <- as.numeric(difftime(kosS$start, kosS$sunrise, units = "mins"))
kos$end <- as.numeric(difftime(kosS$end, kosS$sunset, units = "mins"))

# delka zpevu rano a vecer: 3 hodiny pred a 3 hod po vychodem/zapadu slunce
l <- join(data.frame(ID=kosL$ID, song=kosL$song, interval = kosL$intervaly),
          data.frame(ID=kosS$ID, sunrise=kosS$sunrise, sunset=kosS$sunset, start=kosS$start, end=kosS$end), match = "first")
le <- join(data.frame(ID=kosLe$ID, song=kosLe$song, interval = kosLe$intervaly),
          data.frame(ID=kosS$ID, sunrise=kosS$sunrise, sunset=kosS$sunset, start=kosS$start, end=kosS$end), match = "first")
l_morning <- ddply(l, .(ID), summarize,
                   l_morning=sum(song[interval >= sunrise - 3*3600 & 
                                      interval < sunrise + 15*60]))
l_evening <- ddply(l, .(ID), summarize,
                   l_evening=sum(song[interval >= sunset & 
                                      interval <= sunset + 3*3600]))
le_morning <- ddply(le, .(ID), summarize,
                   le_morning=sum(song[interval >= sunrise - 3*3600 & 
                                        interval < sunrise + 15*60]))
le_evening <- ddply(le, .(ID), summarize,
                   le_evening=sum(song[interval >= sunset & 
                                        interval <= sunset + 3*3600]))
kos <- join(kos, l_morning, match = "first") 
kos <- join(kos, l_evening, match = "first")
kos <- join(kos, le_morning, match = "first") 
kos <- join(kos, le_evening, match = "first")
rm(l_morning, l_evening, le_morning, le_evening)

# delka zpevu hodinu a pul po zacatku pro extended data
le_m <- ddply(le, .(ID), summarize,
            le_m=sum(song[interval > start - 15*60 & interval <= start + 90*60]))
kos <- join(kos, le_m, match = "first") 

# delka zpevu rano a vecer: 1 hod pred a 1 hod po vychodem/zapadu slunce
l1_morning <- ddply(l, .(ID), summarize,
                   l1_morning=sum(song[interval >= sunrise - 3600 & 
                                        interval <= sunrise]))
l1_evening <- ddply(l, .(ID), summarize,
                   l1_evening=sum(song[interval >= sunset & 
                                        interval <= sunset + 3600]))
kos <- join(kos, l1_morning, match = "first") 
kos <- join(kos, l1_evening, match = "first") 
rm(l1_morning, l1_evening)

# delka zpevu rano a vecer: 1 hod po a 1 hod pred vychodu/zapadem slunce
l1.aft.sunrise <- ddply(l, .(ID), summarize,
                        l1.aft.sunrise=sum(song[interval >= sunrise & 
                                          interval <= sunrise + 3600]))
l1.bef.sunset <- ddply(l, .(ID), summarize,
                       l1.bef.sunset=sum(song[interval >= sunset - 3600 & 
                                          interval <= sunset]))
kos <- join(kos, l1.aft.sunrise, match = "first") 
kos <- join(kos, l1.bef.sunset, match = "first") 
rm(l1.aft.sunrise, l1.bef.sunset)

# delka zpevu rano a vecer: 1 hod po a 1 hod pred zacatku/koncem zpevu
l1 <- join(data.frame(ID=kosL$ID, song=kosL$song, interval = kosL$intervaly),
           data.frame(ID=kosS$ID, start=kosS$start, end=kosS$end), match = "first")
l1.aft.start <- ddply(l1, .(ID), summarize,
                        l1.aft.start=sum(song[interval >= start & 
                                                  interval <= start + 3600]))
l1.bef.end <- ddply(l1, .(ID), summarize,
                       l1.bef.end=sum(song[interval >= end - 3600 & 
                                                interval <= end]))
kos <- join(kos, l1.aft.start, match = "first") 
kos <- join(kos, l1.bef.end, match = "first") 
rm(l1.aft.start, l1.bef.end, l1)

# intenzita zpevu 1 resp. 3 hodiny pred/po vychodem/zapadu slunce
i1_morning <- ddply(l, .(ID), summarize, i1_morning=mean(song[interval >= sunrise - 3600 &
                                                              interval <= sunrise]))
i1_evening <- ddply(l, .(ID), summarize, i1_evening=mean(song[interval >= sunset &
                                                              interval <= sunset + 3600]))
i3_morning <- ddply(l, .(ID), summarize, i3_morning=mean(song[interval >= start &
                                                              interval <= sunrise]))
i3_evening <- ddply(l, .(ID), summarize, i3_evening=mean(song[interval >= sunset &
                                                              interval <= end]))
i1_morning[is.na(i1_morning)] <- 0
i1_evening[is.na(i1_evening)] <- 0
i3_morning[is.na(i3_morning)] <- 0
i3_evening[is.na(i3_evening)] <- 0
kos <- join(kos, i1_morning, match = "first") 
kos <- join(kos, i1_evening, match = "first")
kos <- join(kos, i3_morning, match = "first") 
kos <- join(kos, i3_evening, match = "first")
rm(l, i1_morning, i1_evening, i3_morning, i3_evening)

# Diskretni prediktory
# -----------------------------------------------------------------------------------------
kos$noise <- factor(as.numeric(kos$typ %in% c("hluk", "hluk+světlo", "světlo + hluk", 
                                              "světlo + hluk ", "světlo+hluk")),
                    labels = c("Without noise", "With noise"))
kos$light <- factor(as.numeric(kos$typ %in% c("světlo", "hluk+světlo", "světlo + hluk", 
                                              "světlo + hluk ", "světlo+hluk")),
                    labels = c("Normal", "Light pollution"))
kos <- join(kos, aggregate(oblacnost~ID, kosL, mean), match = "first")
kos$cloud <- factor(kos$oblacnost, labels = c("No clouds", "Partly cloudy", "Cloudy"))


# Teplota v ruznych casech
# -----------------------------------------------------------------------------------------
t <- join(data.frame(ID=kosL$ID, temp=kosL$teplota, interval = kosL$intervaly),
          data.frame(ID=kosS$ID, start=kosS$start, end=kosS$end, sunrise=kosS$sunrise,
                      sunset=kosS$sunset), match = "first")
t$start <- abs(as.numeric(difftime(t$start, t$interval, units = "mins")))
t$end <- abs(as.numeric(difftime(t$end, t$interval, units = "mins")))
t$sunrise <- abs(as.numeric(difftime(t$sunrise, t$interval, units = "mins")))
t$sunset <- abs(as.numeric(difftime(t$sunset, t$interval, units = "mins")))
t_start <- ddply(t, .(ID), summarize, temp_start=temp[start == min(start)][1])
t_end <- ddply(t, .(ID), summarize, temp_end=temp[end == min(end)][1])
t_sunrise <- ddply(t, .(ID), summarize, temp_sunrise=temp[sunrise == min(sunrise)][1])
t_sunset <- ddply(t, .(ID), summarize, temp_sunset=temp[sunset == min(sunset)][1])
t_morning <- ddply(t, .(ID), summarize,
                   temp_morning=mean(temp[interval>=strptime("4:00", format="%H:%M") &
                                          interval<=strptime("8:00", format="%H:%M")]))
t_evening <- ddply(t, .(ID), summarize,
                   temp_evening=mean(temp[interval>=strptime("17:00", format="%H:%M") &
                                          interval<=strptime("22:00", format="%H:%M")]))
kos <- join(kos, t_start, match = "first")
kos <- join(kos, t_end, match = "first")
kos <- join(kos, t_sunrise, match = "first")
kos <- join(kos, t_sunset, match = "first")
kos <- join(kos, t_morning, match = "first")
kos <- join(kos, t_evening, match = "first")
kos$mtemp <- lm(temp_sunrise~day, kos)$residuals
kos$etemp <- lm(temp_sunset~day, kos)$residuals
rm(t, t_start, t_end, t_sunrise, t_sunset, t_morning, t_evening)


# Vitr v ruznych casech
# -----------------------------------------------------------------------------------------
w <- join(data.frame(ID=kosL$ID, wind=kosL$vitr, interval = kosL$intervaly),
          data.frame(ID=kosS$ID, start=kosS$start, end=kosS$end, sunrise=kosS$sunrise,
                     sunset=kosS$sunset), match = "first")
w$start <- abs(as.numeric(difftime(w$start, w$interval, units = "mins")))
w$end <- abs(as.numeric(difftime(w$end, w$interval, units = "mins")))
w$sunrise <- abs(as.numeric(difftime(w$sunrise, w$interval, units = "mins")))
w$sunset <- abs(as.numeric(difftime(w$sunset, w$interval, units = "mins")))
w_start <- ddply(w, .(ID), summarize, wind_start=wind[start == min(start)][1])
w_end <- ddply(w, .(ID), summarize, wind_end=wind[end == min(end)][1])
w_sunrise <- ddply(w, .(ID), summarize, wind_sunrise=wind[sunrise == min(sunrise)][1])
w_sunset <- ddply(w, .(ID), summarize, wind_sunset=wind[sunset == min(sunset)][1])
w_morning <- ddply(w, .(ID), summarize,
                   wind_morning=mean(wind[interval>=strptime("4:00", format="%H:%M") &
                                            interval<=strptime("8:00", format="%H:%M")]))
w_evening <- ddply(w, .(ID), summarize,
                   wind_evening=mean(wind[interval>=strptime("17:00", format="%H:%M") &
                                            interval<=strptime("22:00", format="%H:%M")]))
kos <- join(kos, w_start, match = "first")
kos <- join(kos, w_end, match = "first")
kos <- join(kos, w_sunrise, match = "first")
kos <- join(kos, w_sunset, match = "first")
kos <- join(kos, w_morning, match = "first")
kos <- join(kos, w_evening, match = "first")
kos$mwind <- lm(wind_sunrise~day, kos)$residuals
kos$ewind <- lm(wind_sunset~day, kos)$residuals
rm(w, w_start, w_end, w_sunrise, w_sunset, w_morning, w_evening)


# Vlhkost v ruznych casech
# -----------------------------------------------------------------------------------------
t <- join(data.frame(ID=kosL$ID, temp=kosL$vlhkost, interval = kosL$intervaly),
          data.frame(ID=kosS$ID, start=kosS$start, end=kosS$end, sunrise=kosS$sunrise,
                     sunset=kosS$sunset), match = "first")
t$start <- abs(as.numeric(difftime(t$start, t$interval, units = "mins")))
t$end <- abs(as.numeric(difftime(t$end, t$interval, units = "mins")))
t$sunrise <- abs(as.numeric(difftime(t$sunrise, t$interval, units = "mins")))
t$sunset <- abs(as.numeric(difftime(t$sunset, t$interval, units = "mins")))
t_start <- ddply(t, .(ID), summarize, humid_start=temp[start == min(start)][1])
t_end <- ddply(t, .(ID), summarize, humid_end=temp[end == min(end)][1])
t_sunrise <- ddply(t, .(ID), summarize, humid_sunrise=temp[sunrise == min(sunrise)][1])
t_sunset <- ddply(t, .(ID), summarize, humid_sunset=temp[sunset == min(sunset)][1])
t_morning <- ddply(t, .(ID), summarize,
                   humid_morning=mean(temp[interval>=strptime("4:00", format="%H:%M") &
                                            interval<=strptime("8:00", format="%H:%M")]))
t_evening <- ddply(t, .(ID), summarize,
                   humid_evening=mean(temp[interval>=strptime("17:00", format="%H:%M") &
                                            interval<=strptime("22:00", format="%H:%M")]))
kos <- join(kos, t_start, match = "first")
kos <- join(kos, t_end, match = "first")
kos <- join(kos, t_sunrise, match = "first")
kos <- join(kos, t_sunset, match = "first")
kos <- join(kos, t_morning, match = "first")
kos <- join(kos, t_evening, match = "first")
kos$mhumid <- lm(humid_sunrise~day, kos)$residuals
kos$ehumid <- lm(humid_sunset~day, kos)$residuals
rm(t, t_start, t_end, t_sunrise, t_sunset, t_morning, t_evening)


# Tlak v ruznych casech
# -----------------------------------------------------------------------------------------
t <- join(data.frame(ID=kosL$ID, temp=kosL$tlak, interval = kosL$intervaly),
          data.frame(ID=kosS$ID, start=kosS$start, end=kosS$end, sunrise=kosS$sunrise,
                     sunset=kosS$sunset), match = "first")
t$start <- abs(as.numeric(difftime(t$start, t$interval, units = "mins")))
t$end <- abs(as.numeric(difftime(t$end, t$interval, units = "mins")))
t$sunrise <- abs(as.numeric(difftime(t$sunrise, t$interval, units = "mins")))
t$sunset <- abs(as.numeric(difftime(t$sunset, t$interval, units = "mins")))
t_start <- ddply(t, .(ID), summarize, press_start=temp[start == min(start)][1])
t_end <- ddply(t, .(ID), summarize, press_end=temp[end == min(end)][1])
t_sunrise <- ddply(t, .(ID), summarize, press_sunrise=temp[sunrise == min(sunrise)][1])
t_sunset <- ddply(t, .(ID), summarize, press_sunset=temp[sunset == min(sunset)][1])
t_morning <- ddply(t, .(ID), summarize,
                   press_morning=mean(temp[interval>=strptime("4:00", format="%H:%M") &
                                             interval<=strptime("8:00", format="%H:%M")]))
t_evening <- ddply(t, .(ID), summarize,
                   press_evening=mean(temp[interval>=strptime("17:00", format="%H:%M") &
                                             interval<=strptime("22:00", format="%H:%M")]))
kos <- join(kos, t_start, match = "first")
kos <- join(kos, t_end, match = "first")
kos <- join(kos, t_sunrise, match = "first")
kos <- join(kos, t_sunset, match = "first")
kos <- join(kos, t_morning, match = "first")
kos <- join(kos, t_evening, match = "first")
kos$mpress <- lm(press_sunrise~day, kos)$residuals
kos$epress <- lm(press_sunset~day, kos)$residuals
rm(t, t_start, t_end, t_sunrise, t_sunset, t_morning, t_evening)


# Dest v ruznych casech
# -----------------------------------------------------------------------------------------
t <- join(data.frame(ID=kosL$ID, temp=kosL$dest, interval = kosL$intervaly),
          data.frame(ID=kosS$ID, start=kosS$start, end=kosS$end, sunrise=kosS$sunrise,
                     sunset=kosS$sunset), match = "first")
t$start <- abs(as.numeric(difftime(t$start, t$interval, units = "mins")))
t$end <- abs(as.numeric(difftime(t$end, t$interval, units = "mins")))
t$sunrise <- abs(as.numeric(difftime(t$sunrise, t$interval, units = "mins")))
t$sunset <- abs(as.numeric(difftime(t$sunset, t$interval, units = "mins")))
t_start <- ddply(t, .(ID), summarize, prec_start=temp[start == min(start)][1])
t_end <- ddply(t, .(ID), summarize, prec_end=temp[end == min(end)][1])
t_sunrise <- ddply(t, .(ID), summarize, prec_sunrise=temp[sunrise == min(sunrise)][1])
t_sunset <- ddply(t, .(ID), summarize, prec_sunset=temp[sunset == min(sunset)][1])
t_morning <- ddply(t, .(ID), summarize,
                   prec_morning=mean(temp[interval>=strptime("4:00", format="%H:%M") &
                                            interval<=strptime("8:00", format="%H:%M")]))
t_evening <- ddply(t, .(ID), summarize,
                   prec_evening=mean(temp[interval>=strptime("17:00", format="%H:%M") &
                                            interval<=strptime("22:00", format="%H:%M")]))
kos <- join(kos, t_start, match = "first")
kos <- join(kos, t_end, match = "first")
kos <- join(kos, t_sunrise, match = "first")
kos <- join(kos, t_sunset, match = "first")
kos <- join(kos, t_morning, match = "first")
kos <- join(kos, t_evening, match = "first")
kos$mprec <- lm(prec_sunrise~day, kos)$residuals
kos$eprec <- lm(prec_sunset~day, kos)$residuals
rm(t, t_start, t_end, t_sunrise, t_sunset, t_morning, t_evening)




# Ulozeni
# -----------------------------------------------------------------------------------------
save(kosL, kosLe, kosS, kos, file = "kos.RData")
