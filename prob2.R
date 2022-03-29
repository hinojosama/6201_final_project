library (dplyr)
library (ggplot2)
library (readr)
library (lubridate)
library (pracma)
library (tidyr)

#documentation regarding dataset available:
#https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh
#https://covid.cdc.gov/covid-data-tracker/#county-view


county_vac_0 <- read_csv("COVID-19_Vaccinations_in_the_United_States_County.csv")
county_vac_1 <- county_vac_0 %>% 
  filter(Date == "03/28/2022") %>%
  select(Date, FIPS, Recip_County, Recip_State, Completeness_pct, 
         Series_Complete_Yes, Series_Complete_Pop_Pct, Series_Complete_5PlusPop_Pct,
         Census2019, Census2019_5PlusPop) %>%
  top_n(500, Census2019)

county_vac_na <- county_vac_1 %>% 
  mutate(miss = is.na(Series_Complete_Pop_Pct)) %>%
  filter(miss == TRUE)
View(county_vac_na)
#could try to find the data for the following directly from Hawaii/ Barstable MA 
#health depts etc but in my opinion we should just drop as follows

county_vac_2 <- drop_na(county_vac_1)
View(county_vac_2)
