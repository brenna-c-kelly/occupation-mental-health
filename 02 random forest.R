
# random forest classification: occupation survey data

library(fpc)
library(vip)
library(pdp)
library(tree)
library(mlr3)
library(stats)
library(dplyr)
library(purrr)
library(scales)
library(cluster)
library(tidyverse)
library(mlr3tuning)
library(mlr3learners)
library(RColorBrewer)

set.seed(5678)

#demo_cc$tobacco_use <- ifelse(demo_cc$tobaccouse == "U", "N", demo_cc$tobaccouse)

#demo_cc$death_type <- ifelse(demo_cc$mannerdeath == 3, "suicide", "not suicide")
#table(demo_cc$methoddeath)
demo_cc$bin <- as.factor(demo_cc$bin)
# very small > first pass

demo_cc <- demo_cc %>%
  mutate_if(is.character, as.factor)


#demo_cc[, c(3:6, 8:10, 13:14, 17, 21:275, 278)]
names(demo_cc)
all.df <- demo_cc[, c(6:11, 14:268)]
# identify the task
task_pe <- TaskClassif$new(id = "omeid", backend = all.df, 
                           target = "bin")

#task_pe$col_roles$feature <- setdiff(task_pe$col_roles$feature,
#                                     c("X", "Y")) #make any exclusiong

measure = msr("classif.auc") # performance measure
lrn_ct = lrn("classif.rpart", predict_type = "prob") # learner
# resampling strategy
resamp_hout = rsmp("holdout", ratio = 0.8)
resamp_hout$instantiate(task_pe)
# run resampler, examine performance
rr = resample(task_pe, lrn_ct, resamp_hout, store_models = TRUE)
rr$score(measure)
rr$aggregate(measure)

# ~0.75 AUC
# ~0.987
# ~ 0.68

# training
lrn_ct$train(task_pe)
lrn_ct$model


vip(lrn_ct$model, num_features = 30, scale = TRUE)
x <- vip(lrn_ct$model, num_features = 30, scale = TRUE)

myvars <- names(all.df)
myvars <- myvars[-c(1, 266)]
myvars <- x$data$Variable


pdp::partial(lrn_ct$model, 
             train = task_pe$data(),
             pred.var = c("egomaritalstatus",
                          "percent_of_workers_with_basic_people_skills"), 
             prob = TRUE,
             plot = TRUE, rug = TRUE, 
             plot.engine = "ggplot2") + theme_light()

pdp::partial(lrn_ct$model, 
             train = task_pe$data(),
             pred.var = c("percent_of_workers_where_supervisor_is_present",
                          "percent_of_workers_work_reviewed_by_supervisor_more_than_once_per_day"), 
             prob = TRUE,
             plot = TRUE, rug = TRUE, 
             plot.engine = "ggplot2") + theme_light()


for (i in myvars) {
  
  p1 <- pdp::partial(lrn_ct$model, 
                     train = task_pe$data(),
                     pred.var = i, 
                     prob = TRUE,
                     plot = TRUE, rug = TRUE, 
                     plot.engine = "ggplot2") + theme_light() + 
    ggtitle(paste("PDP:", i)) 
  print(p1)
}



myvars <- c("percent_of_workers_gross_manipulation_is_required_occasionally",
            "percent_of_workers_keyboarding_is_required_frequently")
for (i in myvars) {
  # Compute partial dependence data for lstat and rm
  pd <- pdp::partial(lrn_ct$model, 
                     train = task_pe$data(), 
                     pred.var = c("percent_of_workers_supervisory_duties_are_required", i), chull = TRUE, prob = TRUE)
  
  # Default PDP
  # pdp1 <- plotPartial(pd, main = i)
  # print(pdp1)
  p1 <- ggplot(pd, aes(x = "percent_of_workers_supervisory_duties_are_required", y = "yhat", col = i)) + 
    geom_line(linewidth = 1.5) + 
    ggtitle(paste("PDP:", i)) +
    theme_bw()
  print(p1)
  
}


pd <- partial(lrn_ct$model, 
              train = task_pe$data(), 
              pred.var = c("percent_of_workers_lifting_or_carrying_25_lbs_and_less_than_or_equal_to_50_lbs_is_required_occasionally"),
                           #"npses"),
              chull = TRUE, prob = TRUE)
pdp1 <- plotPartial(pd, main = "Chimerism (BM, prob)")


