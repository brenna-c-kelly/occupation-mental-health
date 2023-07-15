
library(dplyr)
library(stringr)
library(tidyverse)
library(data.table)

# merging demo & ors
demo <- read.csv("../PHI/demo_all_cases.csv")

ors <- read.csv("data/ors_spread.csv")
census_soc_cx <- read.csv("misc/census to soc 2000.csv")

# prepping for merge with ors
census_soc_cx$census_2000 <- str_pad(census_soc_cx$census_2000,
                                     3, pad = "0")
census_soc_cx$soc_2000 <- gsub("-", "", census_soc_cx$soc_2000)

head(ors$soc_2018_code)
head(census_soc_cx$soc_2000)
# ors > census codes
occ_soc <- merge(ors, census_soc_cx, by.x = "soc_2018_code", by.y = "soc_2000")
names(demo)
# complete case analysis only
demo_cc <- merge(demo, occ_soc, by.x = "occ", by.y = "census_2000", all.x = TRUE)

# if not excluding, don't worry about interpreting this table
table(demo_cc$egooccupationcode90 %in% c("914", "915", "999"))
#   code 915 = student
#   code 914 = homemaker
#   code 999 = unknown

demo_cc <- demo_cc %>%
  mutate(outdoor_p = percent_of_workers_exposed_to_outdoors -
           (percent_of_workers_exposed_to_outdoors_occasionally + 
              percent_of_workers_exposed_to_outdoors_seldom)) %>%
  mutate(outdoors = ifelse(outdoor_p > 50, "outdoor", "indoor")) %>%
  mutate(home = ifelse(egooccupationcode90 %in% c("914", "999"), "home", "away")) %>% # telework is very rare (only lawyers, probably doesn't apply)
  mutate(school = ifelse(egooccupationcode90 == "915" | 
                           grepl("teacher", occupation) == TRUE | 
                           grepl("Teachers' aides", occupation) == TRUE, "school", "not school")) %>%
  mutate(vehicle = ifelse(grepl("driver", occupation) == TRUE |
                            grepl("truck and tractor", occupation) == TRUE | 
                            grepl("vehicle operator", occupation) == TRUE, "vehicle", "not vehicle")) %>%
  mutate(work_loc = case_when(vehicle == "vehicle" ~ "in a vehicle",
                              home == "home" ~ "indoors at residence",
                              outdoors == "outdoor" ~ "outdoors",
                              school == "school" ~ "school")) %>%
  mutate(work_loc = ifelse(is.na(work_loc), "indoors away from residence", work_loc)) %>%
  mutate(sex = ifelse(gender == "Male", 1, 2))

table(demo_cc$work_loc)

table(demo_cc$gender)

# smoking supplement
phe <- read.csv("../PHI/phewas.csv")
names(phe) <- tolower(names(phe))

phe$redcapid <- str_pad(phe$redcapid, 7, pad = "0")
phe$icd9_string <- as.factor(phe$icd9_string)

phe <- phe %>%
  filter(grepl("tobacco", icd9_string, ignore.case = TRUE)) %>%
  filter(!duplicated(redcapid, icd_code)) %>%
  group_by(redcapid) %>%
  spread(icd9_string, icd9_string) %>%
  fill(.direction = "up")

names(phe)[16] <- "tobacco_exposure"
names(phe)[17] <- "tobacco_hx"
names(phe)[18] <- "tobacco_abuse_counseling"
names(phe)[19] <- "tobacco_use_dx"
names(phe)[20] <- "tobacco_use_dx_preg_complication"
names(phe)[21] <- "tobacco_use_dx_preg_no_complication"
names(phe)[22] <- "tobacco_use_dx_unspecified"
names(phe)[23] <- "toxic_effects_of_tobacco"

if_missing <- function(x) (
  ifelse(is.na(x) == FALSE, 1, 0)
)

phe <- phe %>%
  mutate_at(c("tobacco_exposure", "tobacco_hx",
              "tobacco_abuse_counseling",
              "tobacco_use_dx", 
              "tobacco_use_dx_preg_complication", 
              "tobacco_use_dx_preg_no_complication", 
              "tobacco_use_dx_unspecified", 
              "toxic_effects_of_tobacco"), if_missing)
names(phe)
icd <- select(phe, -c(2:15))

red_cx <- read.csv("../PHI/demo_2023.csv")
names(red_cx) <- tolower(names(red_cx))
red_cx <- red_cx[, c("redcapid", "omeid")]

table(is.na(demo_cc$omeid))
table(is.na(demo_cc$redcapid))
demo_cc <- merge(demo_cc, red_cx, by = "omeid", all.x = TRUE) # 1 w/o omedi
demo_cc <- merge(demo_cc, icd, by = "redcapid", all.x = TRUE)


demo_cc$phe_code <- demo_cc$tobacco_exposure +
  demo_cc$tobacco_hx + demo_cc$tobacco_abuse_counseling +
  demo_cc$tobacco_use_dx + demo_cc$tobacco_use_dx_preg_complication +
  demo_cc$tobacco_use_dx_preg_no_complication +
  demo_cc$tobacco_use_dx_unspecified + demo_cc$toxic_effects_of_tobacco
demo_cc$phe_code <- ifelse(is.na(demo_cc$phe_code), 0, 1)

demo_cc <- demo_cc %>%
  mutate(smoke_exposure = ifelse(phe_code == 1 | tobaccouse == "Y", 1, 2)) %>%
  #mutate(smoke_exposure = ifelse(!is.na(icd9_string) | tobaccouse == "Y", 1, 2)) %>%
  mutate(smoke_upd = ifelse(tobaccouse == "Y", 1, 0)) %>%
  mutate(smoke_both = ifelse(phe_code == 1 & smoke_upd == 1, 1, 0)) %>%
  #mutate(smoke_exposure_only = ifelse(icd9_string == %in%))
  mutate(age = round(age, digits = 0)) %>%
  mutate(work_loc = ifelse(employmentstatus == "Retired", "indoors at residence", work_loc))

prop.table(table(demo_cc$smoke_exposure))

smokers <- demo_cc %>%
  filter(smoke_exposure == 1)

prop.table(table(smokers$smoke_upd)) # 89% updb
prop.table(table(smokers$phe_code)) # 18% phe
prop.table(table(smokers$smoke_both)) # 7& both

table(demo_cc$sex)

## adding unique id
demo_cc$proxy_id <- 1:nrow(demo_cc)
id_links <- demo_cc[, c("proxy_id", "redcapid", "omeid")]
write.csv(id_links, "id_links.csv", row.names = FALSE)

table(demo_cc$employmentstatus, demo_cc$work_loc)

table(is.na(demo_cc$redcapid), is.na(demo_cc$omeid))
demo_cc <- demo_cc[!duplicated(demo_cc$omeid), ]

table(demo_cc$sex)

demo_cc$missing_occ <- ifelse(is.na(demo_cc$occ) & is.na(demo_cc$occupation), 
                              "missing", "not missing")
demo_cc$age <- round(demo_cc$age, 0)
demo_cc$work_loc[demo_cc$employmentstatus %in% 
                   c("Retired", "Unemployed")] <- "indoors at residence"
demo_cc$work_loc[demo_cc$age < 5] <- "indoors at residence"
demo_cc$work_loc[demo_cc$age > 5 & demo_cc$age < 18] <- "school"
demo_cc$work_loc_code <- case_when(demo_cc$work_loc == "in a vehicle" ~ 5,
                                   demo_cc$work_loc == "indoors at residence" ~ 1,
                                   demo_cc$work_loc == "indoors away from residence" ~ 1,
                                   demo_cc$work_loc == "outdoors" ~ 3,
                                   demo_cc$work_loc == "school" ~ 4)


demo_fred <- demo_cc[ , c("proxy_id", "employmentstatus", "age", "sex",
                          "work_loc", "work_loc_code", "smoke_exposure")]
head(demo_cc)
id_links <- demo_cc[, c("proxy_id", "redcapid", "omeid")]
write.csv(id_links, "id_links.csv", row.names = FALSE)

write.csv(demo_fred, "demo_fred.csv", row.names = FALSE)


demo_cc[which(is.na(demo_cc$omeid)), ]

