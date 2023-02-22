
library(stringr)

# merging demo & ors

demo <- read.csv("../PHI/demo_occ.csv")
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
names(test)

# complete case analysis only
demo_cc <- merge(demo, occ_soc, by.x = "occ", by.y = "census_2000")

rm(test)

demo_ors <- merge(demo, ors, by.x = "", by.y = "")

head(ors$soc_2018_code)

# sanity check (demo + occ codes)




