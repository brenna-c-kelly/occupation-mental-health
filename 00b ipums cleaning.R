

library(dplyr)
library(stringr)

ipums <- read.csv("misc/ipums ut.csv")
names(ipums) <- tolower(names(ipums))

head(demo)
head(ipums)

table(is.na(ipums$occ1990))

table(ipums$occsoc == "")

names(demo)
head(ipums)
table(ipums$hispan)
 

# creating a binary indicator (yes / no) for hispanic
ipums$hispan <- ifelse(ipums$hispan == 0, 0, 1)

# definition for race (source: https://usa.ipums.org/usa-action/variables/RACE#codes_section)
ipums$race[ipums$race == 1] <- "white"
ipums$race[ipums$race == 2] <- "black"
ipums$race[ipums$race %in% c(3, 6)] <- "aian or nhpi"
ipums$race[ipums$race %in% c(4, 5)] <- "asian"
ipums$race[ipums$race == 7] <- "other"
ipums$race[ipums$race %in% c(8, 9)] <- "two or more"

# definition for marital status (source: https://usa.ipums.org/usa-action/variables/MARST#codes_section)
ipums$marst[ipums$marst == 1] <- "Married"
ipums$marst[ipums$marst %in% c(2, 3)] <- "Separated"
ipums$marst[ipums$marst == 4] <- "Divorced"
ipums$marst[ipums$marst == 5] <- "Widowed"
ipums$marst[ipums$marst == 6] <- "Never Married"

# definition for sex
ipums$sex <- ifelse(ipums$sex == 1, "Male", "Female")

# definition for education (source: https://usa.ipums.org/usa-action/variables/EDUC#codes_section)
ipums$educ[ipums$educ == 0] <- "unknown"
ipums$educ[ipums$educ %in% c(1:5)] <- "some hs"
ipums$educ[ipums$educ == 6] <- "hs"
ipums$educ[ipums$educ %in% c(7:9)] <- "some college"
ipums$educ[ipums$educ == 10] <- "college"
ipums$educ[ipums$educ == 11] <- "post college"

ipums$occ1990 <- str_pad(ipums$occ1990, 3, pad = "0")
ipums$npboss90[ipums$npboss90 > 999] <- NA

ipums$omeid <- 1:nrow(ipums)
table(ipums$race)
ipums <- ipums %>%
  mutate(race_ethn = ifelse(hispan == 1, "hispanic", race)) %>%
  mutate(race_ethn = ifelse(race_ethn == "white", "white, nonhispanic", race_ethn)) %>%
  select(omeid, occ1990, age, sex, 
         race_ethn, npboss90,
         marst, educ)
  #mutate(race_ethn_simple = ifelse(hispanic_flag == 1, "hispanic", race_def))

table(ipums$race_ethn)

names(demo_cut)
names(ipums)

# joining with demo

demo <- read.csv("../PHI/demo.csv")
demo$egooccupationcode90 <- str_pad(demo$egooccupationcode90, 3, pad = "0")

demo_cut <- demo %>%
  select(omeid, egooccupationcode90, age,
         gender, race, npses, egomaritalstatus,
         education_def)

demo_cut$bin <- "suicide"
ipums$bin <- "not suicide"
names(ipums) <- names(demo_cut)

demo_ipums <- rbind(demo_cut, ipums)


write.csv(demo_ipums, "../PHI/demo_ipums.csv")




  
  