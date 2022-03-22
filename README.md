# 6201_final_project

This repository is for a TSCI 6201 class final project to develop a 15 minute presentation of 1) a heatmap of Omicron peak rate per capita in the 500 most populous counties in the US, 2) a plot of 1 vs county vaccination rate, and 3) a brief associated literature review. 


Full Assignment Posting:

Due May 4 by 11:59pm  Points 0 Submitting a file upload Available Mar 9 at 12am - May 5 at 11:59pm about 2 months
Hi All,

We will continue an (abbreviated) COVID analysis from the Team programming assignment with same teams. Working in a team, please complete the following assignment and prepare a 15 minute presentation for last class day in which each Team member presents at least one slide. There is only one assignment per team. Grading will be based on Teamwork evaluation and presentation quality.

1. Make a heatmap of omicron Peak per capita deaths for 500 most populous counties. Need to get county population information.

You can download county data from here:

https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates/2020-evaluation-estimates/2010s-counties-total.html (Links to an external site.)

2. Plot total Omicron deaths per capita vs the current county vaccination rate.

 Center for Disease Control (CDC) for COVID 19 vaccinations for each county within each states of the United States from the website:

https://www.cdc.gov/coronavirus/2019-ncov/vaccines/distributing/reporting-counties.html (Links to an external site.)

Link to data:

https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh

3. Compare what you found with relevant literature. This is a non-programming task.

4. Collect 1-3 methodology and answers and place into PowerPoint and Upload.


The above builds on previous assignment:


Using the COVID-19 data from the New York Times estimate the dates of the start, peak, and end of the omicron surge for all US counties. You may exclude counties with lower populations. (Try for 100 most populous counties, if cannot do that, try for the 5 most populous counties)

How to download COVID cases/deaths:

cases <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

Looking at 1 county:

omicron <- cases %>% dplyr::filter(state=="Texas",county=="Bexar",date >=as.Date("2021-12-20")) %>% 
     mutate(wk=week(date)+52*(year(date)==2022),day=as.numeric(date-as.Date('2021-12-20'))) %>% select(date,day,cases,county,deaths,state)  %>% mutate(newcases=c(diff(cases),NA))  %>% filter(newcases!=0) 

ggplot(omicron,aes(day,newcases,color=county))+geom_line() 

Can POTENTIALLY Estimate peak as follows:

omicron$daySq <- omicron$day^2

c19Fit <- lm(newcases ~ day + daySq,data=omicron)

omicron$Pred <- predict(c19Fit)

ggplot(omicron,aes(day,newcases))+geom_line() +geom_line(aes(day,Pred,color="red"))

futureData <- data.frame(day=0:70,daySq=(0:70)^2)

futureData$Pred <- predict(c19Fit,newdata = futureData)

ggplot(futureData,aes(day,Pred))+geom_line() 

Make 3 heatmaps of the US Counties that display this date (Start, Peak, and End) information (Make dates the heat variable by converting to number as 'days since 2021-10-01'.) (If short of time, you can do a histogram)

Hints for heatmaps of counties:

Please see link below.
https://datavizpyr.com/how-to-make-us-state-and-county-level-maps-in-r/ (Links to an external site.)
Another link:
https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html (Links to an external site.)

 Center for Disease Control (CDC) for COVID 19 vaccinations for each county within each states of the United States from the website:

https://www.cdc.gov/coronavirus/2019-ncov/vaccines/distributing/reporting-counties.html (Links to an external site.)

