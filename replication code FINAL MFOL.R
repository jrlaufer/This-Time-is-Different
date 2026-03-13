library(tidyverse)
library(ggplot2)
library(readr)
library(fixest)
library(modelsummary)
library(gsynth)
library(xtable)

midtermsUPDATED <- read_csv("finaldata.csv")

#####FEOLS####

##Main models for Table 1
turnout2 <- feols(turnout ~ post*treat2 + leaners + race + lowinc + highinc + married + nodegree + agecat + education | county_fips + year, midtermsUPDATED, cluster = "county_fips")
summary(turnout2)

reg2 <- feols(reg ~ post*treat2 + leaners + race + lowinc + highinc  + married + nodegree + education + agecat | county_fips + year, midtermsUPDATED, cluster = "county_fips")
summary(reg2)

###Appendix - Robustness Check (state*year FEs)
turnout3 <- feols(turnout ~ post*treat2 + leaners + race + lowinc + highinc + married + nodegree + agecat + education + state*year | county_fips, midtermsUPDATED, cluster = "county_fips")
summary(turnout3)

reg3 <- feols(reg ~ post*treat2 + leaners + race + lowinc + highinc  + married + nodegree + education + agecat + state*year | county_fips, midtermsUPDATED, cluster = "county_fips")
summary(reg3)

##Synthetic Controls - Youth

youth <- subset(midtermsUPDATED, agecat == "Youth (18-29)")

youth$Democratic <- ifelse(youth$pid3_leaner == 1, 1, 0)
youth$nonwhite <- ifelse(youth$race != "White", 1, 0)
youth$nodegree2 <- ifelse(youth$nodegree == "No College Degree", 1, 0)

youthturnoutreg <- youth %>% 
  group_by(county_fips, year) %>%
  summarise(mean_reg = mean(reg, na.rm = TRUE),
            mean_turnout = mean(turnout, na.rm = TRUE),
            respondent_pop = length(case_id),
            number_nocollege = sum(nodegree2),  
            number_lowinc = sum(lowinc),
            number_Democratic = sum(Democratic),
            number_nonwhite = sum(nonwhite)) %>%
  ungroup()

#percentages
youthturnoutreg$respondent_pop <- as.numeric(youthturnoutreg$respondent_pop)
youthturnoutreg$pctcollege <- 1 - (youthturnoutreg$number_nocollege/youthturnoutreg$respondent_pop)
youthturnoutreg$pctlowinc <- youthturnoutreg$number_lowinc/youthturnoutreg$respondent_pop
youthturnoutreg$pctDem <- youthturnoutreg$number_Democratic/youthturnoutreg$respondent_pop
youthturnoutreg$pctnonwhite <- youthturnoutreg$number_nonwhite/youthturnoutreg$respondent_pop


list2 <- youthturnoutreg %>%
  count(county_fips) %>%
  filter(n == 3) %>%
  pull(county_fips)

youthturnoutreg$treat <- ifelse(youthturnoutreg$county_fips =="01047" & youthturnoutreg$year =="2018" | youthturnoutreg$county_fips =="01073"& youthturnoutreg$year =="2018" | youthturnoutreg$county_fips =="01101"
                                & youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="01101"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="04013"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="06001"
                                & youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="06037"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="06059"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="08005"
                                & youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="08031"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="09001"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="11001"
                                & youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="12001"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="12005"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="12009"
                                & youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="12011"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="12015"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="12031"
                                & youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="12033"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="12057"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="12071"
                                & youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="12073"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="12086"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="12095"
                                & youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="12097"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="12099"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="12103"
                                & youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="12105"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="12115"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="12127"
                                & youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="13121"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="17031"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="19113"
                                & youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="19193"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="20209"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="22033"
                                & youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="22055"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="22071"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="24005"
                                & youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="27027"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="27053"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="27123"
                                & youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="28049"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="29510"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="31055"
                                & youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="32003"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="34027"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="35001"
                                & youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="36047"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="36081"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="37081"
                                & youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="38015"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="38085"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="42017"
                                & youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="42101"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="45019"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="48029"
                                & youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="48113"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="48141"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="48201"
                                & youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="49035"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="51003"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="51059"
                                & youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="51121"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="55025"& youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="55079"
                                & youthturnoutreg$year =="2018" |   youthturnoutreg$county_fips =="55105" & youthturnoutreg$year =="2018", 1, 0)

table(youthturnoutreg$treat) 

youthturnoutreg <- youthturnoutreg %>% 
  filter(county_fips %in% list2)
sum(length(youthturnoutreg$county_fips))
dim(youthturnoutreg)

system.time(
  out <- gsynth(mean_turnout ~ treat + pctcollege + pctlowinc + pctnonwhite + pctDem, data = youthturnoutreg, 
                index = c("county_fips","year"), force = "two-way", 
                CV = TRUE, r = c(0, 5), se = TRUE, cl = "county_fips",
                inference = "parametric", nboots = 1000, 
                parallel = FALSE, min.T0 = 2)
)

sum.out <- print(out)

print(xtable(sum.out), type = "latex")

plot(out)
plot(out, type = "counterfactual", raw = "none", main = "Syntheic Control Model of County-Level Youth Voter Turnout", xlab = "Year", ylab = "Turnout Rate")

