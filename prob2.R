library (dplyr)
library (ggplot2)
library (readr)
library (lubridate)
library (pracma)
library (tidyr)

#prob2 requires modified output of prob1 total death instead of peak death so first
#bring in the death per capita code from prob1:

#make dataframes from the source data
co_est2020 <- read_csv("co-est2020.csv")
cases <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", skip_empty_rows = FALSE)


#these are the top 510 fips in the census data by population 
#need more than 500 due to differences in this 2020 census data and the 
#vaccination data which uses 2019 data and includes PR/GU but not HI and 1 fips
#in MA as will be shown below. 
#make a table with census data not including county "000" rows 
#(state level data) that uses the state and name to add a fips column to match up with 
#census data for populations.
top500_fips <- co_est2020 %>%
  select(COUNTY, STATE, POPESTIMATE2020) %>%
  filter(COUNTY != "000") %>%
  mutate(fips = paste0(STATE, COUNTY)) %>%
  select(fips, POPESTIMATE2020) %>%
  top_n(510,POPESTIMATE2020) 

#list of fips in the census data that are not in the covid data
#basically just the 5 NYC fips
exc_fips <- setdiff(top500_fips$fips, cases$fips)

#calculate the pop of NYC summing up the pop of its 5 fips
pop_NYC_df <- top500_fips %>% 
  filter(fips %in% exc_fips)
pop_NYC <- sum(pop_NYC_df$POPESTIMATE2020)


#As we did above make a table of census data.  Now will filter out the NYC fips which 
#are missing from covid data and add back a row of the combined NYC info under 99999 
#fips. Finally select the top 510
county_top_death <- co_est2020 %>%
  select(COUNTY, STATE, STNAME, CTYNAME, POPESTIMATE2020) %>%
  filter(COUNTY != "000") %>%
  mutate(fips = paste0(STATE, COUNTY)) %>%
  filter(fips != "36047", fips != "36081", fips != "36061", fips != "36005",fips != "36085") %>%
  select(fips, CTYNAME, STNAME, POPESTIMATE2020) %>%
  add_row(fips = "99999", CTYNAME = "New York City", STNAME ="New York", POPESTIMATE2020 = as.numeric(pop_NYC))%>%
  top_n(510,POPESTIMATE2020)

#make a table of only the covid data assigned to NYC county fill in the fip = "99999"
cases_tidy_nyc <- cases %>%
  filter(county=="New York City") %>%
  mutate(fips = "99999")

#remove the NYC rows still missng fips and add the new ones made above with 99999
cases_tidy <- cases %>%
  filter(county != "New York City") %>%
  bind_rows(cases_tidy_nyc)

#make a table of Omicron cases estimated to start after date 12/20/21 and only of the 
#fips we have identified above the top 510 fips.  Then add a day column (days since start of 
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

#search the omicron_top_death table for large negative values which we may wish 
#censor.  
lg_corrections <- omicron_top_death %>% 
  filter(newdeaths < 0)
View(lg_corrections)

#This reveals a largely Massachusets problem.  As many of these are likely
#true and accurate do not wish to exclude all.  As there seems to be a systematic
#issue with Mass will exclude instances when new deaths is less than negative 45
omicron_top_death <- omicron_top_death %>% 
  filter(newdeaths > -45)

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
fips_Pred_D$total_death <- 
  lapply(fips_Pred_D$ddn, function(yy) sum(yy$newdeaths))


#create column in the county top death table for our death peaks and one for 
#peak per capita 
county_top_death$total_death <- as.numeric(fips_Pred_D$total_death)
county_top_death$total_death_per_cap <- 
  round(county_top_death$total_death / (county_top_death$POPESTIMATE2020/100000), digit = 2)

#check the final table for missing values, should be zero
sum(is.na(county_top_death))


#prob2 begins

#documentation regarding dataset available:
#https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh
#https://covid.cdc.gov/covid-data-tracker/#county-view

#make dataframe from source data
county_vac_0 <- read_csv("COVID-19_Vaccinations_in_the_United_States_County.csv")

#using the same NYC fips from above slice out the vaccination data for NYC as of this date
pop_NYC_2019df <- county_vac_0 %>%
  filter(Date == "03/28/2022") %>%
  select(Date, FIPS, Recip_County, Recip_State, Completeness_pct, 
         Series_Complete_Yes, Series_Complete_Pop_Pct, Series_Complete_5PlusPop_Pct,
         Census2019, Census2019_5PlusPop) %>%
  filter(FIPS %in% exc_fips)

#calculations for summarised NYC data for each variable
Completeness_pct_NYC <-mean(pop_NYC_2019df$Completeness_pct) 
Series_Complete_Yes_NYC <- sum(pop_NYC_2019df$Series_Complete_Yes)
Series_Complete_Pop_Pct_NYC <- mean(pop_NYC_2019df$Series_Complete_Pop_Pct)
Series_Complete_5PlusPop_Pct_NYC <- mean(pop_NYC_2019df$Series_Complete_5PlusPop_Pct)
Census2019_NYC <- sum(pop_NYC_2019df$Census2019)
Census2019_5PlusPop_NYC <- sum(pop_NYC_2019df$Census2019_5PlusPop)

#make a table with vaccination data from the source data as of date below
#Then filter out the NYC fips as well as the Puerto Rico and Guam which are 
#available in our above covid data and add back a row of the combined NYC info 
#under 99999 fips. Finally select the top 504 as will need to drop out 4 empty rows below 
county_vac_1 <- county_vac_0 %>% 
  filter(Date == "03/28/2022") %>%
  select(Date, FIPS, Recip_County, Recip_State, Completeness_pct, 
         Series_Complete_Yes, Series_Complete_Pop_Pct, Series_Complete_5PlusPop_Pct,
         Census2019, Census2019_5PlusPop) %>%
  filter(FIPS != "36047", FIPS != "36081", FIPS != "36061", FIPS != "36005",
         FIPS != "36085", Recip_State != "PR", Recip_State !="GU") %>%
  add_row(Date = "03/28/2022", FIPS = "99999", Recip_County = "New York City", Recip_State = "NY", 
          Completeness_pct = Completeness_pct_NYC, Series_Complete_Yes = Series_Complete_Yes_NYC,
          Series_Complete_Pop_Pct = Series_Complete_Pop_Pct_NYC, 
          Series_Complete_5PlusPop_Pct = Series_Complete_5PlusPop_Pct_NYC,
          Census2019 =  Census2019_NYC, 
          Census2019_5PlusPop =  Census2019_5PlusPop_NYC)%>%
  top_n(504, Census2019)

#find the empty rows in our vaccination data and drop them out
county_vac_na <- county_vac_1 %>% 
  mutate(miss = is.na(Series_Complete_Pop_Pct)) %>%
  filter(miss == TRUE)
county_vac_2 <- drop_na(county_vac_1)

#before joining below first clean up some of the columns and drop the redundant
county_vac_2 <- rename(county_vac_2, fips = FIPS, county = Recip_County, state = Recip_State)
county_top_death <- county_top_death %>% select(-c(STNAME,CTYNAME))


#Left join the data by fips with vaccination table on left
prob2_df <- left_join(county_vac_2, county_top_death, by = c("fips" = "fips"))

#check the final table for missing values, should be zero
sum(is.na(prob2_df))
#another way to check if any missing values, should be empty table
prob2_df_na <- which(is.na(prob2_df))
View(prob2_df_na)

#prob2_df is tidy and ready for use in generating plots of (peak) Omicron deaths
#per Capita vs current vaccination rate for each county..

#2. Plot total Omicron deaths per capita vs the current county vaccination rate.
ggplot(prob2_df, aes(Series_Complete_Pop_Pct, total_death_per_cap)) + 
  geom_point(size = .75) +
  geom_smooth() +
  labs(title = "Omicron Deaths vs Vaccinations", subtitle = "500 largest counties by population", 
       x = "% of Population Vaccinated", y = "Deaths per Capita", caption = "Total COVID19 
       Omicron variant deaths per capita versus current completed vaccinaion rate in the 
       five hundred largest counties by population in the United States. Regression shown in 
       blue with standard error in grey") +
  annotate(geom = "text", x = 80, y = 130, label = "Standard error shown in grey") +
  theme_bw()
ggsave("deaths_vs_vax.png", width = 14, height = 10, units = "cm")

#fips with vaccination rate < 50%
low_vax <- prob2_df %>% 
  filter(Series_Complete_Pop_Pct <= 50)
