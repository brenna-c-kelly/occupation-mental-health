
library(stringr)

# merging demo & ors

#demo <- read.csv("../PHI/demo_occ.csv")
demo <- read.csv("../PHI/demo_occ_ipums.csv")
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

# complete case analysis only
demo_cc <- merge(demo, occ_soc, by.x = "occ", by.y = "census_2000")

table(demo_cc$education_def)
names(demo_cc)
summary(demo_cc$percent_of_workers_exposed_to_hazardous_contaminants)

#ggplot(demo_cc, aes(education_def, percent_of_workers_exposed_to_hazardous_contaminants)) +
#  geom_point(aes(color = race, alpha = 0.5)) +
#  xlab("Nam-Powers Socioeconomic Status Score") +
#  ylab("Count") +
#  #xlim(0, 99) +
#  theme_bw()

table(demo_cc$gender, demo_cc$race)

table(demo_cc$gender, demo_cc$bin)



