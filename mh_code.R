library (dplyr)
library (ggplot2)
library (readr)
library (lubridate)
library (pracma)

co_est2020 <- read_csv("co-est2020.csv")

county_top <- co_est2020 %>%
  select(COUNTY, STATE, CTYNAME, POPESTIMATE2020) %>%
  filter(COUNTY != "000") %>%
  mutate(fips = paste0(STATE, COUNTY)) %>%
  select(fips, CTYNAME, POPESTIMATE2020) %>%
  top_n(8,POPESTIMATE2020)


cases <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

omicron_top_ungrouped <- cases %>%
  filter(date >=as.Date("2021-12-20"), fips %in% county_top$fips) %>%
  mutate(wk=week(date)+52*(year(date)==2022), day=as.numeric(date-as.Date('2021-12-20'))) %>%
  filter(78 > day) %>%
  select(date, day, cases, deaths, fips, county, state) %>%
  mutate(newcases=c(diff(cases), NA)) %>%
  filter(newcases!=0)

omicron_top <- cases %>%
  filter(date >=as.Date("2021-12-20"), fips %in% county_top$fips) %>%
  mutate(wk=week(date)+52*(year(date)==2022),day=as.numeric(date-as.Date('2021-12-20'))) %>%
  filter(78 > day) %>%
  select(date, day, cases, deaths, fips, county, state) %>%
  group_by(fips) %>%
  mutate(newcases=c(diff(cases), NA)) %>%
  filter(newcases!=0)

fips_Pred <- as.data.frame(paste0("fips_", county_top$fips))
fips_Pred$fips <- county_top$fips
fips_Pred$ddn <- lapply(county_top$fips, function (zz) omicron_top %>%
                          filter(fips == zz) %>%
                          select(day, date, newcases, fips))
fips_Pred$lm <- lapply(fips_Pred$ddn, function(yy)  lm(newcases ~ poly(day, degree = 6, raw = TRUE),  data = yy))
fips_Pred$Pred <- lapply(fips_Pred$lm, function (xx) predict(xx))
fips_Pred$P_max <- lapply(fips_Pred$Pred, function (ww) max(ww))
fips_Pred$match <- lapply(fips_Pred$Pred, function (vv) na.omit(match(fips_Pred$P_max,vv)))


peak_days <- list()
peak_dates <- list()
fips_vec <- as.list(county_top$fips)
match_vec <- as.list(fips_Pred$match)

for (i in fips_vec) {dayta_frame <- 
  omicron_top_ungrouped %>% 
  filter(fips == i) %>%
  select(day)
dayy <- dayta_frame[match_vec[[match(i, fips_vec)]], "day"]
peak_days <- append(peak_days, dayy)}
county_top$peak_day <- peak_days


for (i in fips_vec) {datea_frame <- 
  omicron_top_ungrouped %>% 
  filter(fips == i) %>%
  select(date)
dates <- datea_frame[match_vec[[match(i, fips_vec)]], "date"]
peak_dates <- c(peak_dates, dates)}
county_top$peak_date <- peak_dates
#need to fix this date, tried as.Date, as_date, ymd, etc without success. Unclear why not working as peak_dates list shows it as date already. 

for (i in fips_vec) {county_plot <- 
  omicron_top %>% 
  filter(fips == i) %>%
  select(day, newcases, fips)
c19lm <- lm(newcases ~ poly(day, degree = 6, raw = TRUE),  data = county_plot)
county_plot$Pred <- predict(c19lm)
j = ggplot(county_plot, aes(day,newcases)) +
  geom_line() +
  geom_line(aes(day,Pred,color="red"))
ggsave(j, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}
