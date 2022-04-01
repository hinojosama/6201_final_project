library (dplyr)
library (ggplot2)
library (readr)
library (lubridate)
library (pracma)

#make dataframes from the source data
co_est2020 <- read_csv("co-est2020.csv")
cases <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", skip_empty_rows = FALSE)

#these are the top 500 rows of covid death data by population in the fips
top500_fips <- co_est2020 %>%
  select(COUNTY, STATE, POPESTIMATE2020) %>%
  filter(COUNTY != "000") %>%
  mutate(fips = paste0(STATE, COUNTY)) %>%
  select(fips, POPESTIMATE2020) %>%
  top_n(500,POPESTIMATE2020) 

#list of fips in the census data that are not in the covid data
#basically just the 5 NYC fips
exc_fips <- setdiff(top500_fips$fips, cases$fips)

#calculate the pop of NYC summing up the pop of its 5 fips
pop_NYC_df <- top500_fips %>% 
  filter(fips %in% exc_fips)
pop_NYC <- sum(pop_NYC_df$POPESTIMATE2020)

#make a table with census data from the source data not including county "000" rows 
#(state level data) that uses the state and name to add a fips column to match up with 
#census data for populations. Then filter out the NYC fips which are missing from 
#covid data and add back a row of the combined NYC info under 99999 fips. Finally 
#select the top 500
county_top_death <- co_est2020 %>%
  select(COUNTY, STATE, STNAME, CTYNAME, POPESTIMATE2020) %>%
  filter(COUNTY != "000") %>%
  mutate(fips = paste0(STATE, COUNTY)) %>%
  filter(fips != "36047", fips != "36081", fips != "36061", fips != "36005",fips != "36085") %>%
  select(fips, CTYNAME, STNAME, POPESTIMATE2020) %>%
  add_row(fips = "99999", CTYNAME = "New York City", STNAME ="New York", POPESTIMATE2020 = as.numeric(pop_NYC))%>%
  top_n(500,POPESTIMATE2020)

#make a table of only the covid data assigned to NYC county fill in the fip = "99999"
cases_tidy_nyc <- cases %>%
  filter(county=="New York City") %>%
  mutate(fips = "99999")

#remove the NYC rows still missng fips and add the new ones made above with "99999"
cases_tidy <- cases %>%
  filter(county != "New York City") %>%
  bind_rows(cases_tidy_nyc)

#make a table of omicron cases estimated to start after date 12/20/21 and only of the 
#fips we have identified above the top 500 fips.  Then add a day column (days since start of 
#omicron). Select only the columns of interest and group the information by fips
#for calculation of newdeaths.  (if not grouped data will be erroneously calculated 
#sequentially by date). Finally filter out rows where newdeath = 0.
omicron_top_death <- cases_tidy %>%
  filter(date >=as.Date("2021-12-20"), fips %in% county_top_death$fips) %>%
  mutate(wk=week(date)+52*(year(date)==2022),day=as.numeric(date-as.Date('2021-12-20'))) %>%
  select(date, day, cases, deaths, fips, county, state) %>%
  group_by(fips) %>%
  mutate(newdeaths=c(diff(deaths), NA)) %>%
  filter(newdeaths!=0)

#make a table with rows organized by a fips label (perhaps redundant step)
#and fill in the fips from above table
fips_Pred_D <- as.data.frame(paste0("fips_", county_top_death$fips))
fips_Pred_D$fips <- county_top_death$fips

#for each of these fips fill in another column that includes the corresponding slice 
#of omicron death data
fips_Pred_D$ddn <- lapply(county_top_death$fips, function (zz) omicron_top_death %>%
                            filter(fips == zz) %>%
                            select(day, date, newdeaths, fips))

#for each of these slices of data create a model of the data using lm polynomial function,
#saving as column lm
fips_Pred_D$lm <- 
  lapply(fips_Pred_D$ddn, function(yy) lm(newdeaths ~ poly(day, degree = 6, raw = TRUE),  data = yy))

#for each of these models create predicted value using pred function, saving as column Pred
fips_Pred_D$Pred <- lapply(fips_Pred_D$lm, function (xx) predict(xx))

#for each of these fips find the maximum (our peak) death of the model
fips_Pred_D$P_max <- lapply(fips_Pred_D$Pred, function (ww) max(ww))

#create column in the county top death table for our death peaks and one for 
#peak per capita 
county_top_death$death_peak <- round(as.double(fips_Pred_D$P_max), digits = 1)
county_top_death$percap_death_pk <- 
  round(county_top_death$death_peak/ (county_top_death$POPESTIMATE2020/100000), digit = 1)

fips_vec_D <- as.list(county_top_death$fips)

#check the final table for missing values, should be zero
sum(is.na(county_top_death))


#county_top_death is tidy and ready for use in generating heat map of peak 
#Omicron deaths per Capita


#This plot code below works (not a heatmap) and is commented out to avoid generation on sourcing
# for (i in fips_vec_D) {county_plot_D <-
#   omicron_top_death %>%
#   filter(fips == i) %>%
#   select(day, newdeaths, fips)
# c19lm <- lm(newdeaths ~ poly(day, degree = 6, raw = TRUE),  data = county_plot_D)
# county_plot_D$Pred <- predict(c19lm)
# j <- ggplot(county_plot_D, aes(day,newdeaths)) +
#   geom_line() +
#   geom_line(aes(day,Pred,color="red"))
# ggsave(j, file=paste0("death_plot_", i,".png"), width = 14, height = 10, units = "cm")
# }
