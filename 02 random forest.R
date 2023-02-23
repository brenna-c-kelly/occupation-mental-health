
# random forest classification: occupation survey data

library(iml)
library(fpc)
library(vip) #
library(pdp) #
library(tree)
library(mlr3) #
library(stats)
library(dplyr)
library(purrr)
library(DALEX)
library(scales)
library(ranger)
library(cluster)
library(DALEXtra)
library(tidyverse)
library(rpart.plot)
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

not_suicide <- demo_cc %>%
  filter(bin == "not suicide")
remove_n <- nrow(not_suicide)*0.95
not_suicide <- not_suicide[-sample(1:nrow(not_suicide), remove_n), ]

suicide <- demo_cc %>%
  filter(bin == "suicide")

demo_cc <- rbind(not_suicide, suicide)

#demo_cc[, c(3:6, 8:10, 13:14, 17, 21:275, 278)]
names(demo_cc)
all.df <- demo_cc[, c(6:11, 14:268)]
# identify the task
task_pe <- TaskClassif$new(id = "omeid", backend = all.df, 
                           target = "bin")

#task_pe$col_roles$feature <- setdiff(task_pe$col_roles$feature,
#                                     c("X", "Y")) #make any exclusiong

measure = msr("classif.auc") # performance measure
lrn_ct = mlr3::lrn("classif.rpart", predict_type = "prob") # learner
# resampling strategy
resamp_hout = rsmp("cv", folds = 5)
resamp_hout$instantiate(task_pe)
# run resampler, examine performance
rr = mlr3::resample(task_pe, lrn_ct, resamp_hout, store_models = TRUE)
rr$score(measure)
rr$aggregate(measure)

vip::vip(lrn_ct$model, num_features = 26, scale = TRUE)

# quick surrogate with ranger
all.df <- all.df %>%
  drop_na() %>%
  droplevels()

rf <- ranger(bin ~ ., data = all.df)
all.df$yhat <- predict(rf, data = all.df)$predictions

surrogate <- rpart(yhat ~ ., all.df)
plot(surrogate$variable.importance)

plot(surrogate$variable.importance)
head(surrogate$variable.importance)

rpart.plot(surrogate, clip.right.labs = FALSE, branch = .3, under = TRUE,
           fallen.leaves = FALSE)
rpart.rules(surrogate, cover = TRUE)


test <- select_if(all.df, is.numeric)



ranger_exp = DALEX::explain(rf,
                            data = all.df,
                            y = all.df[, "bin"],
                            label = "Ranger Penguins",
                            model_info = model_info(rf),
                            colorize = FALSE)

DALEX::model_performance(ranger_exp)

ranger_effect = model_parts(ranger_exp)
plot(ranger_effect, show_boxplots = FALSE)

# partial dependency with iml
X <- Boston[which(names(Boston) != "medv")]
predictor <- Predictor$new(rf, data = X, y = Boston$medv)

model = Predictor$new(lrn_ct, data = all.df, y = all.df$bin)
imp <- FeatureImp$new(predictor, loss = "mae")


ggplot(surrogate, aes(x = percent_of_day_where_standing_is_required_mean, y = yhat)) +
  geom_line(size = 1.5) +
  ggtitle("PDP: pbc_cd3") + theme_bw()




num_features = c("percent_of_day_where_standing_is_required_mean",
                 "pounds_maximum_weight_lifted_or_carried_mean")
effect = FeatureEffects$new(model)
plot(effect, features = num_features)

# partial dependency with Shapley (pkg: iml)
x = all.df[which(names(all.df) != "bin")]
model = Predictor$new(lrn_ct, data = all.df, y = "bin")


data.frame(all.df[2, ])
x.interest = data.frame(all.df[264, ])
str(x.interest)
shapley = Shapley$new(model, x.interest = x.interest)
plot(shapley)

# ~0.75 AUC
# ~0.987
# ~ 0.68

# training
lrn_ct$train(task_pe)
lrn_ct$model


# partial dependency
pd <- pdp::partial(lrn_ct$train, c("percent_of_day_where_standing_is_required_mean", 
                        "pounds_maximum_weight_lifted_or_carried_mean"), prob = TRUE, 
              which.class = 2)

pd <- partial(fit.rf, c("pbc_cd3", "hla"), prob = TRUE, chull = TRUE)
plotPartial(pd, main = "% day standing, mean pounds carried")


ggplot(pd, aes(x = percent_of_day_where_standing_is_required_mean, y = yhat)) +
  geom_line(size = 1.5) +
  ggtitle("PDP: pbc_cd3") + theme_bw()

pd <- partial(fit.rf, c("txage", "tbi"), prob = TRUE, 
              which.class = 2)
ggplot(pd, aes(x = txage, y = yhat, col = tbi))+
  geom_line(size = 1.5) +
  ggtitle("PDP: Age and TBI") + theme_bw()

model
lrn_ct$model
x <- vip(lrn_ct$model, num_features = 15, scale = TRUE)
x$data$Variable







length(myvars)
vip(lrn_ct$model, num_features = 26, scale = TRUE)
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
              pred.var = c("percent_of_workers_lifting_or_carrying_25_lbs_and_less_than_or_equal_to_50_lbs_is_required_occasionally",
                           "npses"),
              chull = TRUE, prob = TRUE)
pdp1 <- plotPartial(pd, main = "Suicide (carrying moderate weight, SES)")


