

library(dplyr)

#demo <- read.csv("../demographic data.csv")
demo <- read.csv("../PHI/demographic data 120722.csv")

names(demo) <- tolower(names(demo))

demo <- demo %>%
  filter(mannerdeath == 3) %>% # only suicide deaths
  select(-c(egooccupationcode80))%>% # 80 is empty
  group_by(egooccupation) %>% # some are missing code only, fill with code from same occupation.text
  fill(egooccupationcode90)

# cleaning
#     occupation
demo$egooccupation[demo$egooccupation == "STUDENT"] <- "STUDENTS"
demo$egooccupation[demo$egooccupation == "HOMEMAKER"] <- "HOMEMAKERS"
demo$egooccupation[demo$egooccupation == "DID NOT WORK"] <- "UNEMPLOYED"
demo$egooccupation[demo$egooccupation == "UNKNOWN CODE"] <- "UNKNOWN"
#     employment
demo$employmentstatus[demo$employmentstatus == "E"] <- "Employed"
demo$employmentstatus[demo$employmentstatus == "R"] <- "Retired"
demo$employmentstatus[demo$employmentstatus == "S"] <- "Self-Employed"
demo$employmentstatus[demo$employmentstatus == "U"] <- "Unemployed"
demo$employmentstatus[demo$employmentstatus == ""] <- "Unknown"
demo$employmentstatus[demo$egooccupation %in% c("DID NOT WORK")] <- "Unemployed"
demo$employmentstatus[demo$egooccupation %in% c("UNKNOWN")] <- "Unknown"
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

write.csv(demo, "../PHI/demo.csv")

