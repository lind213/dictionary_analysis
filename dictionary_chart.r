library(RSQLite)
library(tidyverse)
library(lubridate)

# path to sqlite3 file
urle <- "<html_path>"
download.file(urle, destfile = "words.sqlite3", cacheOK=TRUE)

# connect to db
con <- dbConnect(drv=RSQLite::SQLite(), dbname="words.sqlite3")

# exclude sqlite_sequence
tables <- tables[tables != "sqlite_sequence"]

lDataFrames <- vector("list", length=length(tables))

# get data table
for (i in seq(along=tables)) {
  lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "'", sep=""))
}

# create tibble - overview over hits per day
dataFrame_2 <- lDataFrames[[2]] %>% 
  as_tibble() %>% 
  rename(ord = `_ord`) %>% 
  rename(date = `_date`) %>% 
  rename(id = `_id`) %>% 
  mutate(date_2 = dmy_hms(date),
         day = wday(date_2),
         hour = hour(date_2),
         week = isoweek(date_2),
         minute = minute(date_2),
         minutes = minute/60,
         time = hour + minutes,
         days = ifelse(day == 2, "Monday",
                       ifelse(day == 3, "Tuesday",
                              ifelse(day == 4, "Wednesday",
                                     ifelse(day == 5, "Thursday",
                                            ifelse(day == 6, "Friday",
                                                   ifelse(day == 7, "Saturday", "Sunday")))))),
         days = factor(days, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
         Uke = as.factor(Uke))
  
#hits per participant
dataFrame_1 <- lDataFrames[[3]] %>% 
  as_tibble()
nrow(dataFrame_2)/dataFrame_1$sum

# chart showing hits every day and time of day
dataFrame_2 %>% 
  ggplot(aes(time, color = week)) +
  geom_freqpoly(binwidth = 1) +
  # Military time format
  scale_x_continuous(breaks = c(3, 6, 9, 12, 15, 18, 21)) +
  facet_wrap(~days)