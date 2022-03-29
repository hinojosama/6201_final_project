library (dplyr)
library (ggplot2)
library (readr)
library (lubridate)
library (pracma)

co_est2020 <- read_csv("co-est2020.csv")
cases <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", skip_empty_rows = FALSE)

top500_fips <- co_est2020 %>%
  select(COUNTY, STATE, POPESTIMATE2020) %>%
  filter(COUNTY != "000") %>%
  mutate(fips = paste0(STATE, COUNTY)) %>%
  select(fips, POPESTIMATE2020) %>%
  top_n(500,POPESTIMATE2020) 

exc_fips <- setdiff(top500_fips$fips, cases$fips)

pop_NYC_df <- top500_fips %>% 
  filter(fips %in% exc_fips)
pop_NYC <- sum(pop_NYC_df$POPESTIMATE2020)

county_top <- co_est2020 %>%
  select(COUNTY, STATE, STNAME, CTYNAME, POPESTIMATE2020) %>%
  filter(COUNTY != "000") %>%
  mutate(fips = paste0(STATE, COUNTY)) %>%
  filter(fips != "36047", fips != "36081", fips != "36061", fips != "36005",fips != "36085") %>%
  select(fips, CTYNAME, STNAME, POPESTIMATE2020) %>%
  add_row(fips = "99999", CTYNAME = "New York City", STNAME ="New York", POPESTIMATE2020 = as.numeric(pop_NYC))%>%
  top_n(20,POPESTIMATE2020)

county_top_death <- county_top

cases_tidy_nyc <- cases %>%
  filter(county=="New York City") %>%
  mutate(fips = "99999")

cases_tidy <- cases %>%
  filter(county != "New York City") %>%
  bind_rows(cases_tidy_nyc)


omicron_top_death <- cases_tidy %>%
  filter(date >=as.Date("2021-12-20"), fips %in% county_top_death$fips) %>%
  mutate(wk=week(date)+52*(year(date)==2022),day=as.numeric(date-as.Date('2021-12-20'))) %>%
  select(date, day, cases, deaths, fips, county, state) %>%
  group_by(fips) %>%
  mutate(newdeaths=c(diff(deaths), NA)) %>%
  filter(newdeaths!=0)

fips_Pred_D <- as.data.frame(paste0("fips_", county_top_death$fips))
fips_Pred_D$fips <- county_top_death$fips
fips_Pred_D$ddn <- lapply(county_top_death$fips, function (zz) omicron_top_death %>%
                          filter(fips == zz) %>%
                          select(day, date, newdeaths, fips))
fips_Pred_D$lm <- lapply(fips_Pred_D$ddn, function(yy) lm(newdeaths ~ poly(day, degree = 6, raw = TRUE),  data = yy))
fips_Pred_D$Pred <- lapply(fips_Pred_D$lm, function (xx) predict(xx))
fips_Pred_D$P_max <- lapply(fips_Pred_D$Pred, function (ww) max(ww))

county_top_death$death_peak <- round(as.double(fips_Pred_D$P_max), digits = 1)
county_top_death$percap_death_pk <- round(county_top_death$death_peak/ (county_top_death$POPESTIMATE2020/100000), digit = 1)

fips_vec_D <- as.list(county_top_death$fips)

for (i in fips_vec_D) {county_plot_D <- 
  omicron_top_death %>% 
  filter(fips == i) %>%
  select(day, newdeaths, fips)
c19lm <- lm(newdeaths ~ poly(day, degree = 6, raw = TRUE),  data = county_plot_D)
county_plot_D$Pred <- predict(c19lm)
j <- ggplot(county_plot_D, aes(day,newdeaths)) +
  geom_line() +
  geom_line(aes(day,Pred,color="red"))
ggsave(j, file=paste0("death_plot_", i,".png"), width = 14, height = 10, units = "cm")
}
