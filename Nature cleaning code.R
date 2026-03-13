library(foreign)
library(tidyverse)
library(ggplot2)
library(haven)

`cumulative_2006-2024` <- readRDS("C:/Users/Alice/Dropbox/MFOL/cumulative_2006-2024.rds")

midterms <- subset(`cumulative_2006-2024`, year == c(2010, 2014, 2018)) #now without 2022

midterms$treat <- ifelse(midterms$county_fips =="01047" |   midterms$county_fips =="01073" |   midterms$county_fips =="01101"
                         |   midterms$county_fips =="01101" |   midterms$county_fips =="04013" |   midterms$county_fips =="06001"
                         |   midterms$county_fips =="06037" |   midterms$county_fips =="06059" |   midterms$county_fips =="08005"
                         |   midterms$county_fips =="08031" |   midterms$county_fips =="09001" |   midterms$county_fips =="11001"
                         |   midterms$county_fips =="12001" |   midterms$county_fips =="12005" |   midterms$county_fips =="12009"
                         |   midterms$county_fips =="12011" |   midterms$county_fips =="12015" |   midterms$county_fips =="12031"
                         |   midterms$county_fips =="12033" |   midterms$county_fips =="12057" |   midterms$county_fips =="12071"
                         |   midterms$county_fips =="12073" |   midterms$county_fips =="12086" |   midterms$county_fips =="12095"
                         |   midterms$county_fips =="12097" |   midterms$county_fips =="12099" |   midterms$county_fips =="12103"
                         |   midterms$county_fips =="12105" |   midterms$county_fips =="12115" |   midterms$county_fips =="12127"
                         |   midterms$county_fips =="13121" |   midterms$county_fips =="17031" |   midterms$county_fips =="19113"
                         |   midterms$county_fips =="19193" |   midterms$county_fips =="20209" |   midterms$county_fips =="22033"
                         |   midterms$county_fips =="22055" |   midterms$county_fips =="22071" |   midterms$county_fips =="24005"
                         |   midterms$county_fips =="27027" |   midterms$county_fips =="27053" |   midterms$county_fips =="27123"
                         |   midterms$county_fips =="28049" |   midterms$county_fips =="29510" |   midterms$county_fips =="31055"
                         |   midterms$county_fips =="32003" |   midterms$county_fips =="34027" |   midterms$county_fips =="35001"
                         |   midterms$county_fips =="36047" |   midterms$county_fips =="36081" |   midterms$county_fips =="37081"
                         |   midterms$county_fips =="38015" |   midterms$county_fips =="38085" |   midterms$county_fips =="42017"
                         |   midterms$county_fips =="42101" |   midterms$county_fips =="45019" |   midterms$county_fips =="48029"
                         |   midterms$county_fips =="48113" |   midterms$county_fips =="48141" |   midterms$county_fips =="48201"
                         |   midterms$county_fips =="49035" |   midterms$county_fips =="51003" |   midterms$county_fips =="51059"
                         |   midterms$county_fips =="51121" |   midterms$county_fips =="55025" |   midterms$county_fips =="55079"
                         |   midterms$county_fips =="55105", 1, 0)


length(unique(midterms$county_fips[midterms$treat == 1])) 
midterms$post <- ifelse(midterms$year == 2018, 1, 0)

##DVs
midterms$reg <-  ifelse(midterms$vv_regstatus == "Active", 1, 0)

midterms$turnout <-  ifelse(midterms$vv_turnout_gvm == "Voted", 1, 0)

#pid3_leaner: Partisan identity (including leaners) - p 27
midterms$leaners <- factor(midterms$pid3_leaner, levels = c("1", "2", "3", "8"), labels = c("Democratic", "Republican", "Independent", "Not Sure"))

#age: Age - p 16: binary (under 30; over 30 subgroups)
midterms$agecat <- ifelse(midterms$age > 29, 0, 1) #under 30 = 1
midterms$agecat <- factor(midterms$agecat, levels = c("0", "1"), labels = c("(Non-Youth) 30+", "Youth (18-29)"))


midterms$education <- factor(midterms$educ, levels = c("1", "2", "3", "4", "5", "6"), labels = c("No High School Diploma", "High School Graduate",
                                                                                                 "Some College", "2 Year Degree", "4 Year Degree",
                                                                                                 "Post-Grad"))
midterms$nodegree <- ifelse(midterms$educ == 1 | midterms$educ == 2 | midterms$educ == 3, 1, 0)
midterms$nodegree <- factor(midterms$nodegree, levels = c("1", "0"), labels = c("No College Degree", "College Degree"))
#no degree = 1; (up to and including some college)

#marstat = maritial status (married = 1)
midterms$married <- ifelse(midterms$marstat == 1, 1, 0)
midterms$married <- factor(midterms$married,  levels = c("0", "1"), labels = c("Not Married", "Married"))

#faminc - family income 27-28
midterms$income <- factor(midterms$faminc, levels = c("Less than 10k", "10k - 20k", "20k - 30k", "30k - 40k",
                                                      "40k - 50k", "50k - 60k", "60k - 70k", "70k - 80k", "80k - 100k",
                                                      "100k - 120k", "120k - 150k", "150k+", "Prefer not to say"), 
                          labels = c("Less than $10k", "$10k - $20k", "$20k - $30k", "$30k - $40k",
                                     "$40k - $50k", "$50k - $60k", "$60k - $70k", "$70k - $80k", "$80k - $100k",
                                     "$100k - $120k", "$120k - $150k", "$150k+", "Prefer not to say"))


midterms$lowinc <- ifelse(midterms$faminc == "Less than 10k" | midterms$faminc == "10k - 20k" 
                          | midterms$faminc ==  "20k - 30k" | midterms$faminc ==  "30k - 40k", 1, 0)

midterms$highinc <- ifelse(midterms$faminc == "100k - 120k" | midterms$faminc == "$120k - $150k" 
                           | midterms$faminc ==  "$150k+", 1, 0)

table(midterms$lowinc)
table(midterms$highinc)

#race 
midterms$race <- factor(midterms$race, levels = c("1", "2", "3", "4", "5", "6", "7", "8"), 
                        labels = c("White", "Black", "Hispanic", "Asian", "Native American", "Mixed Race",
                                   "Other", "Middle Eastern"))

buffer <- read.dbf("C:/Users/Alice/Dropbox/MFOL/Controlcounties100miles.dbf")


buffer$far <- 1

buffer <- buffer %>%
rename("county_fips" = "FULLFIPS")

View(midterms)
farcontrols <- midterms %>%
  left_join(buffer, by = "county_fips") %>%
  filter(far == 1) %>%
  distinct(county_fips, year)


midtermsUPDATED <- midterms %>%
  left_join(farcontrols %>% mutate(far = 1),
            by = c("county_fips", "year")) %>%
  mutate(
    far    = ifelse(is.na(far), 0, 1),
    treat2 = ifelse(treat == 1, 1, 0)
  )


midtermsUPDATED <- midtermsUPDATED %>% 
  filter(far == 1 & treat2 == 0 | far == 0 & treat2 == 1)



write.csv(midtermsUPDATED, "C:/Users/Alice/Dropbox/MFOL/finaldata.csv")