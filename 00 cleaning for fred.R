

library(dplyr)
library(stringr)


names(demo) <- tolower(names(demo))
head(demo)

# cases only
demo <- demo %>%
  filter(suicidecasecontrol == 1)

# cleaning
#     occupation
demo$egooccupation[demo$egooccupation == "HOMEMAKER"] <- "HOMEMAKERS"
demo$egooccupation[demo$egooccupation == "DID NOT WORK"] <- "UNEMPLOYED"
demo$egooccupation[demo$egooccupation == "UNKNOWN CODE"] <- "UNKNOWN"
#     employment
demo$employmentstatus[demo$employmentstatus == "R"] <- "Retired"
demo$employmentstatus[demo$employmentstatus == "S"] <- "Self-Employed"
demo$employmentstatus[demo$employmentstatus == "U"] <- "Unemployed"
demo$employmentstatus[demo$employmentstatus == ""] <- "Unknown"
#     occupation code
demo$egooccupationcode90 <- str_pad(demo$egooccupationcode90, 3, pad = "0")
#     marital status
table(demo$egomaritalstatus)
demo$egomaritalstatus[demo$egomaritalstatus %in% 
                        c("NULL", "", "Unknown",
                          "Other")] <- "Unknown"
demo$egomaritalstatus[is.na(demo$egomaritalstatus)] <- "Unknown"
demo$egomaritalstatus[demo$egomaritalstatus == "Married but Separated"] <- "Separated"
#     gender
demo$gender[demo$gender == 0] <- "Female"
demo$gender[demo$gender == 1] <- "Male"
#     smoking
demo$tobaccouse[is.na(demo$tobaccouse)] <- "U"
demo$tobaccouse[demo$tobaccouse == ""] <- "U"
#     race
table(demo$race)
demo$race[demo$race == "5. White" & demo$spanish %in% c("N", "U")] <- "white, nonhispanic"
demo$race[demo$race == "Presumed Not white, Cannot be Classified" & demo$spanish %in% c("N", "U")] <- "white, nonhispanic"
table(demo$race, demo$spanish)
demo$race[demo$spanish == "Y"] <- "hispanic"

demo$race[demo$race == "4. Black or African American"] <- "black"
demo$race[demo$race == "1. American Indian or Alaska Native"] <- "aian"
demo$race[demo$race == "3. Native Hawaiian or Other Pacific Islander"] <- "nhpi"
demo$race[demo$race == "2. Asian"] <- "asian"

demo$race[demo$race == "6. Multiple Races"] <- "multiple races"
demo$race[demo$race == "7. Unknown"] <- "other"

table(demo$educationmax) #Note: education does not match EducationMax (more reliable)
demo$education_def[demo$educationmax == 1] <- "some hs"
demo$education_def[demo$educationmax == 2] <- "hs"
demo$education_def[demo$educationmax == 3] <- "some college"
demo$education_def[demo$educationmax == 4] <- "college"
demo$education_def[demo$educationmax == 5] <- "post college"
demo$education_def[demo$educationmax %in% c(9, "NULL")] <- "unknown"

# This filter should be applied as necessary to filter
  # - only adults
  # -who are not students, homemakers, or unknown
    #   code 915 = student
    #   code 914 = homemaker
    #   code 999 = unknown
demo <- demo %>%
  filter(dage >= 18) #%>%
  #filter(!egooccupation %in% c("STUDENTS", "HOMEMAKERS", "UNKNOWN")) %>%
  #filter(!egooccupationcode90 %in% c("914", "915", "999"))

# ~300 are missing "occupation code," but not "occupation"
demo_a <- demo  %>%
  filter(egooccupation != "") %>% # exclude missing occupations
  filter(!is.na(egooccupation)) #%>%
  #group_by(egooccupation) %>%
  # FIX: for those missing code only, fill with code from same occupation.text
  #fill(egooccupationcode90)

table(is.na(demo$egooccupationcode90),
      is.na(demo$egooccupationcode80))

demo_b <- demo %>%
  filter(egooccupation == "" | is.na(egooccupation))

demo <- rbind(demo_a, demo_b) # merge the fixed datasets


prop.table(table(is.na(suicide$egooccupation)))

demo$egooccupation[demo$egooccupation == ""] <- NA

# ~1883 are missing "occupation," but not "occupation code"
occ_codes <- read.delim("misc/occupation codes.txt")
names(occ_codes) <- tolower(names(occ_codes))
occ_codes <- occ_codes %>%
  filter(occupation != "")
# FIX: merge demo to occ to get "occupation" from 1990 census occ_codes
demo_test <- merge(demo, occ_codes, by.x = "egooccupationcode90", by.y = "occ", all.x = TRUE) %>%
  select(-c(egooccupationcode80))

missing <- demo %>%
  filter(is.na(egooccupationcode90) & is.na(egooccupationcode80))
table(missing$egooccupationcode90)

# final exclusions

demo <- demo %>%
  #filter(mannerdeath == 3) %>% # only suicide deaths
  filter(!is.na(occupation)) %>%
  #   code 917 = unemployed
  #   code 916 = volunteer
  #   code 400 = unknown
  #   code 882 = unknown
  filter(!egooccupationcode90 %in% c("400", "882",
                                     "916", "917"))

# write clean file
write.csv(demo, "../PHI/demo.csv", row.names = FALSE)

table(is.na(demo$egooccupation), demo$year_death)


######    pause > grab ipums "controls"
demo <- read.csv("../PHI/demo_ipums.csv")
demo$egooccupationcode90 <- str_pad(demo$egooccupationcode90, 3, pad = "0")

# converting 1990 census codes to 2000 census codes
occ_cx <- read_dta("misc/occ2000_occ1990dd_update.dta")

occ_cx <- occ_cx %>%
  filter(!duplicated(occ1990dd)) # deal with the one-to-many issue later

occ_cx$occ <- str_pad(occ_cx$occ, 3, pad = "0")
occ_cx$occ1990dd <- str_pad(occ_cx$occ1990dd, 3, pad = "0")

names(occ_cx) <- c("occ", "egooccupationcode90")

demo_occ <- demo %>%
  left_join(occ_cx, by = "egooccupationcode90") %>%
  distinct() # deal with the one-to-many issue later

# write clean file
write.csv(demo_occ, "../PHI/demo_occ_ipums.csv", row.names = FALSE)
