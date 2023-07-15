
library(haven)
library(dplyr)
library(stringr)
library(tidyverse)

jim <- read.csv("/Users/brenna/Downloads/case_hasdate.csv")
names(jim) <- tolower(names(jim))

red_cx <- read.csv("../PHI/demo_2023.csv")
names(red_cx) <- tolower(names(red_cx))

red_cx <- red_cx %>%
  filter(mannerdeath %in% c("Suicide", "SUICIDE"))

demo <- read.csv("/PHI/demo_2022_12.csv")
names(demo) <- tolower(names(demo))

demo <- demo %>%
  filter(dmanner %in% c("Suicide", "SUICIDE"))



summary(test)

#demo_cx <- merge(demo, red_cx, by = "omeid")

#test <- merge(demo_cx, jim, by = "redcapid")

names(demo)
head(demo)

#demo <- demo %>%
#  filter(mannerdeath == 3)
names(demo)
names(jim)
#test <- merge(jim, demo, by.x = "RedCapID", by.y = "")

#demo <- read.csv("../demographic data.csv")
demo <- read.csv("../PHI/demographic data 120722.csv")
#demo <- read.csv("../PHI/demo_2022_12.csv")

kids <- demo %>%
  filter(age < 18)
table(kids$egooccupation)
names(demo) <- tolower(names(demo))

#kids$egooccupation_class <- ifelse(kids$egooccupation == "STUDENTS", "student", "other")
#kids$egooccupation_class <- ifelse(kids$egooccupation == "", "missing", kids$egooccupation_class)

#table(kids$egooccupation_class)  
#aggregate(kids$age, by = list(kids$egooccupation_class), FUN = mean)

#kids$age_f <- as.factor(round(kids$age, 0))
#table(kids$age_f, kids$egooccupation_class)

# cleaning
#     occupation
names(demo)
demo$egooccupation[demo$egooccupation == "STUDENT"] <- "STUDENTS"
demo$egooccupation[demo$egooccupation == "HOMEMAKER"] <- "HOMEMAKERS"
demo$egooccupation[demo$egooccupation == "DID NOT WORK"] <- "UNEMPLOYED"
demo$egooccupation[demo$egooccupation == "UNKNOWN CODE"] <- "UNKNOWN"
#demo$egooccupation[demo$egooccupation == ""] <- "UNKNOWN"
#     employment
demo$employmentstatus[demo$employmentstatus == "E"] <- "Employed"
demo$employmentstatus[demo$employmentstatus == "R"] <- "Retired"
demo$employmentstatus[demo$employmentstatus == "S"] <- "Self-Employed"
demo$employmentstatus[demo$employmentstatus == "U"] <- "Unemployed"
demo$employmentstatus[demo$employmentstatus == ""] <- "Unknown"

demo$egooccupation[is.na(demo$egooccupation) &
                     demo$employmentstatus == "Retired"] <- "RETIRED"
demo$egooccupation[is.na(demo$egooccupation) &
                     demo$employmentstatus == "Unemployed"] <- "UNEMPLOYED"
demo$egooccupationcode90[is.na(demo$egooccupationcode90) &
                           demo$employmentstatus == "Unemployed"] <- "999"
demo$egooccupationcode90[is.na(demo$egooccupationcode90) &
                           demo$employmentstatus == "Retired"] <- "999"

#     occupation code
demo$egooccupationcode90 <- str_pad(demo$egooccupationcode90, 3, pad = "0")
#     marital status
table(demo$egomaritalstatus)
demo$egomaritalstatus[demo$egomaritalstatus %in% 
                        c("NULL", "", "Unknown or Un",
                          "Other")] <- "Unknown"
demo$egomaritalstatus[demo$egomaritalstatus == "Married but S"] <- "Separated"
#     gender
demo$gender[demo$gender == 0] <- "Female"
demo$gender[demo$gender == 1] <- "Male"
#     smoking
demo$tobaccouse[is.na(demo$tobaccouse)] <- "U"
demo$tobaccouse[demo$tobaccouse == ""] <- "U"
#     race
demo$race[demo$race == 1] <- "white, nonhispanic"
demo$race[demo$race == 2] <- "black, nonhispanic"
demo$race[demo$race == 3] <- "aian or nhpi"
demo$race[demo$race == 4] <- "asian"
demo$race[demo$race == 5] <- "two or more"
demo$race[demo$race == 6] <- "hispanic"
demo$race[demo$race == 90] <- "other"

table(demo$educationmax) #Note: education does not match EducationMax (more reliable)
demo$education_def[demo$educationmax == 1] <- "some hs"
demo$education_def[demo$educationmax == 2] <- "hs"
demo$education_def[demo$educationmax == 3] <- "some college"
demo$education_def[demo$educationmax == 4] <- "college"
demo$education_def[demo$educationmax == 5] <- "post college"
demo$education_def[demo$educationmax %in% c(9, "NULL")] <- "unknown"

# filter, only adults who are not students, homemakers, or unknown
#   code 915 = student
#   code 914 = homemaker
#   code 999 = unknown
#demo <- demo %>%
  #filter(age >= 10)# %>%
  #filter(!egooccupation %in% c("STUDENTS", "HOMEMAKERS", "UNKNOWN")) %>%
  #filter(!egooccupationcode90 %in% c("914", "915", "999"))

# ~300 are missing "occupation code," but not "occupation"

demo_a <- demo  %>%
  filter(egooccupation != "") %>% # exclude missing occupations
  filter(!is.na(egooccupation)) %>%
  group_by(egooccupation) %>%
  # FIX: for those missing code only, fill with code from same occupation.text
  fill(egooccupationcode90)
demo_b <- demo %>%
  filter(egooccupation == "" | is.na(egooccupation))

demo <- rbind(demo_a, demo_b)

demo$egooccupation[demo$egooccupation == ""] <- NA

# ~1883 are missing "occupation," but not "occupation code"
occ_codes <- read.delim("misc/occupation codes.txt")
names(occ_codes) <- tolower(names(occ_codes))
occ_codes <- occ_codes %>%
  filter(occupation != "")
# FIX: merge demo to occ to get "occupation" from 1990 census occ_codes
demo <- merge(demo, occ_codes, by.x = "egooccupationcode90", by.y = "occ", all.x = TRUE) %>%
  select(-c(egooccupationcode80, egooccupation))

prop.table(table(is.na(demo$egooccupation), is.na(demo$egooccupationcode90)))

prop.table(table(demo$mannerdeath == 3))


demo <- demo %>%
  filter(mannerdeath == "3")
table(demo$mannerdeath, demo$year_death)


#missing <- demo %>%
#  filter(is.na(egooccupationcode90))
#table(missing$occupation)

# final exclusions
#table()
#demo <- demo %>%
  #filter(mannerdeath == 3) %>% # only suicide deaths
#  filter(!is.na(occupation)) #%>%
  #   code 917 = unemployed
  #   code 916 = volunteer
  #   code 400 = unknown
  #   code 882 = unknown
  #filter(!egooccupationcode90 %in% c("400", "882",
  #                                   "916", "917"))

# write clean file
write.csv(demo, "../PHI/demo.csv", row.names = FALSE)
########################################################################################
# remove NA from red_cx
demo <- read.csv("../PHI/demo.csv")
table(is.na(demo$egooccupation), demo$year_death)
demo$egooccupationcode90 <- str_pad(demo$egooccupationcode90, 3, pad = "0")

######    pause > grab ipums "controls"
#demo <- read.csv("../PHI/demo_ipums.csv")
#demo$egooccupationcode90 <- str_pad(demo$egooccupationcode90, 3, pad = "0")

# converting 1990 census codes to 2000 census codes
library(haven)
occ_cx <- read_dta("misc/occ2000_occ1990dd_update.dta")

occ_cx <- occ_cx %>%
  filter(!duplicated(occ1990dd)) # deal with the one-to-many issue later

occ_cx$occ <- str_pad(occ_cx$occ, 3, pad = "0")
occ_cx$occ1990dd <- str_pad(occ_cx$occ1990dd, 3, pad = "0")

names(occ_cx) <- c("occ", "egooccupationcode90")

length(unique(demo$omeid))

demo_occ <- demo %>%
  left_join(occ_cx, by = "egooccupationcode90") %>%
  #filter(!is.na(omeid)) %>% # complete cases
  group_by(omeid) %>%
  fill(occ, .direction = c("updown")) #%>%
  #distinct() # deal with the one-to-many issue later

demo_occ <- demo %>%
  left_join(occ_cx, by = "egooccupationcode90") %>%
  #filter(!is.na(omeid)) %>% # complete cases
  group_by(omeid) %>%
  fill(occ, .direction = c("updown"))
demo_occ <- demo_occ[!duplicated(demo_occ$omeid), ]




# write clean file
write.csv(demo_occ, "../PHI/demo_all_cases.csv", row.names = FALSE)
#write.csv(demo_occ, "../PHI/demo_occ_ipums.csv", row.names = FALSE)
