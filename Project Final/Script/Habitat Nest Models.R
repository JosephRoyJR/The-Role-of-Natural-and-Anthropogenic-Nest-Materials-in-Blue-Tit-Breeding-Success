##### TOP- Urbanization and Materials ######
# Joseph Roy
# Created on 24/07/2023
# Last Edited on- 08/08/2023

##### Load Data #####

rm(list = ls()) # Clearing the workspace

# Load libraries and data

source("Script/Habitat Nest Models.R")


DATA <- read.csv("Data/BreedNestEnv.csv")


##### Libraries #####

load_libraries <- function() {
  library(dplyr)
  library(lubridate)
  library(lme4)
  library(sjPlot)
  library(ggplot2)
  library(performance)
  library(see)
  library(report)
  library(ggplot2)
  library(ggpubr)
  library(dplyr)
  library(cowplot)
  library(gridExtra)
  library(lmerTest)
  library(fitdistrplus)
  library(ggcorrplot)
  library("FactoMineR")
  library(factoextra)
  library(car)
  library(broom.mixed)
  library(tidyr)
  library(gtsummary)
  library(grid)
  library(gridExtra)
  library(report)
  library(gtsummaryExtra)
  library(flextable)

}

load_libraries()

# Factorizing the data
DATA$Urban.rural <- as.factor(DATA$Habitat) #making factors factors 
DATA$Site <- as.factor(DATA$Site)


#view data
View(DATA)


# Graphical summary of data
library(fitdistrplus)  #to find the distribtion of data 
descdist(DATA$Total, discrete = FALSE, boot = 100)
hist(DATA$Total)

##### summary of data #####

library(gtsummary)
library(gt)
library(webshot2)

summary_data<- select(DATA, Total, Anthro, Moss, Feather, Wool.Hair, Habitat) #selecting numerical values


summary_data_table <- tbl_summary(summary_data, by = Habitat, statistic = list(all_continuous() ~ "{mean}, ({median}), ({sd})"))


# save it as html using gtsummary
# by changing the file name you can save it as html or word or pdf for png u need webshot2

#summary_data_table %>% 
#  as_gt() %>% 
#  gtsave("Output/Plots/summary_data_table.png")

#summary(summary_data) #summary of data
#table_summary

library(flextable)

summary_data_table %>% 
  as_flex_table() %>%
  flextable::autofit() %>%
  save_as_docx(path= "Output/Plots/summary_data_table.docx")


##### 1. Nesting Materials #####
##### 1.1 Total~ Materials #####


# Creating a linear model for total nest 

library(lmerTest) # to get p-values for the LMER model

# effcet of each material on total nest (Can be said connected to PCA)
modtotal<- lmer(Total~ Anthro+Moss+Feather+Wool.Hair+(1|Site), 
                data= DATA)

summary(modtotal, REML=F) # to get p-values for the LMER model


report(modtotal)

# one way of saving the table as png using gtsummaryExtra

library(gtsummary)
library(gtsummaryExtra)

# creating a table using gtsummary
#table_model

modtotal_table <- modtotal %>%
  tbl_regression(intercept = TRUE,
                 label = list(
                   `(Intercept)` = "Intercept",
                   `Anthro` = "Anthro", 
                   `Moss` = "Moss",
                   `Feather` = "Feather",
                   `Wool.Hair` = "Wool.Hair")) %>% 
  add_significance_stars(hide_ci = FALSE, hide_p = FALSE)

# saving it as png using gtsummaryExtra  
#modtotal_table %>% 
#  as_gt() %>% 
#  gtsave("Output/Plots/modtotal_table.png")


## better way of saving ##
# save it as docx using flextable (This is better for fitting and scaling)
# you can copy paste it to word or ppt as text and scale it as you want

library(flextable)

modtotal_table %>% 
  as_flex_table() %>%
  flextable::autofit() %>%
  save_as_docx(path= "Output/Plots/modtotal_table.docx")

# use save_as_image to save it as png or jpeg



# another way to get the table  

# convert model summary to tidy format
# model_summary <- broom::tidy(modtotal)

# check if gridExtra package is installed, if not install it
# if (!require("gridExtra")) {
#   install.packages("gridExtra")
# }

# check if grid package is installed, if not install it
# if (!require("grid")) {
#   install.packages("grid")
# }

# load required libraries
# library(gridExtra)
# library(grid)

# create a table
# table <- gridExtra::tableGrob(model_summary)

# write model summary to csv file
# write.csv(model_summary, file = "model_summary.csv")



##### 1.2 PCA Nest #####

# https://www.datacamp.com/tutorial/pca-analysis-r
# PCA- To see overall composition of urban and rural nests


library('corrr')
library(ggcorrplot)
library("FactoMineR")
library(factoextra)

colSums(is.na(DATA)) # Removing NAs

library(dplyr)
numerical_data<- select(DATA, Anthro, Moss, Feather, Wool.Hair) 

#selecting numerical values

# create summary statistics for numerical data

summary(numerical_data)



head(numerical_data)

#data_normalized <- scale(numerical_data)
#(data_normalized) #normalizing data
# but I havent utilized normaliztion As my data is in same unit for all variables. 

PCA<- prcomp(numerical_data)# doing PCA. 
names(PCA)
summary(PCA)


print(PCA$rotation) #to see the PCs 

PCA_scree <- fviz_eig(PCA, addlabels = TRUE, ggtheme = theme_minimal() + theme(axis.title.x = element_text("Principal Components"), axis.title.y = element_text("Percent Explained Variance"), plot.title = element_blank())) # Screeplots to see explanatory power of PCs
#save it using ggsave

ggsave(filename = "Output/Plots/PCA_scree.jpg",
       plot = PCA_scree, 
       device = "jpeg", 
       width = 100,
       height = 100,
       units = "mm"
       )

#PCA_cos2<- fviz_cos2(PCA, choice = "var", axes = 1)
# to see how much contribution is to the axis 1 and 2 by 
# this is supposed to be between 0-1 but here the value goes up than 1
# I am not  sure why, but I havent utilized normaliztion As my data is in same unit for all variables. 



PCA_contrib <- fviz_contrib(PCA, choice = "var", axes = c(1, 2), top = 50, ggtheme = theme_minimal() + theme(plot.title = element_blank()))
# https://rpubs.com/pg2000in/PrincipalComponentAnalysis 


ggsave(filename = "Output/Plots/PCA_contrib.jpg",
       plot = PCA_contrib, 
       device = "jpeg", 
       width = 100, 
       height = 100, 
       units = "mm")

# plan was to save both scree and cos2 in one plot but size is well 'freedom is being single' Jr.
#library(ggpubr)
#scree_cos2 <-ggarrange(PCA_scree, PCA_contr, ncol = 2, nrow = 1)

#Finally plotting the PC biplot
PCA_nest<- fviz_pca_biplot(PCA, label= "var", 
                habillage = DATA$Habitat,
                addEllipses = T) #Plotting PCA


library(ggplot2)

# saving it 
PCA_nest <- fviz_pca_biplot(PCA, label = "var", 
              habillage = DATA$Habitat,
              addEllipses = TRUE,
              repel = TRUE) +
  xlab("PC1") +
  ylab("PC2") +
  ggtitle(NULL)

ggsave(filename = "Output/Plots/PCA_nest.jpg",
       plot = PCA_nest, 
       device = "jpeg", 
       width = 100, 
       height = 100, 
       units = "mm")



##### 1.3 Saving PC1 to data #####


# Print a summary of the PCA result
summary(PCA)

# Extract PC1 scores
pc1_scores <- PCA$x[, "PC1"]

# Add PC1 scores to your data
DATA$PC1 <- pc1_scores

hist(DATA$PC1)

# save data as csv

write.csv(DATA, file = "Data/BreedNestEnv.csv")


##### Variation in PC1 #####
# linear model on PC1 and Anthro (Will show how much of the variation in PC1 is explained by Anthro)

#mod_pc_an<- lmer(PC1~ Anthro+ (1|Site), 
 #               data= DATA)

#summary(mod_pc_an, REML=F)


# Check for multicollinearity I have these here because I was having more variables like HPD and TD but I removed them as they were not significant.

#library(car)
#vif(mod_pc_env) # No multicollinearity



# linear model on PC1, IS100 and Habitat

mod_pc_is<- lmer(PC1~ IS100+ (1|Site), 
                data= DATA)

summary(mod_pc_is, REML=F)

# doing the same for habitat

mod_pc_hab<- lmer(PC1~ Habitat+ (1|Site), 
                data= DATA)

summary(mod_pc_hab, REML=F)

# Combining both IS100 and Habitat

mod_pc_is_hab<- lmer(PC1~ IS100+Habitat+ (1|Site), 
                data= DATA)

summary(mod_pc_is_hab, REML=F)


# plot a model using sjPlot

library(sjPlot)

plot_model(mod_pc_is_hab, type = "pred", terms = c("IS100")) +
  xlab("Impervious Surface Area- 100m") +
  ylab("Principal Component 1") +
  ggtitle(NULL)+ 
  theme_minimal()
# save it using ggsave

ggsave(filename = "Output/Plots/PC1_IS100.jpg",
       plot = plot_model(mod_pc_is_hab, type = "pred", terms = c("IS100")) +
         xlab("Impervious Surface Area- 100m") +
         ylab("PC1 (Variation in Nest Materials)") +
         ggtitle(NULL)+ 
         theme_minimal(), 
       device = "jpeg", 
       width = 100, 
       height = 100, 
       units = "mm")

# create a model summary table using tbl_regression

library(gtsummary)

mod_pc_is_hab_tbl <- mod_pc_is_hab %>%
  tbl_regression(intercept = TRUE) %>% 
  add_significance_stars(hide_ci = T, hide_p = FALSE, hide_se = TRUE)

#save using flextable package
# library(flextable)

mod_pc_is_hab_tbl %>% 
  as_flex_table() %>%
  flextable::autofit() %>%
  save_as_docx(path= "Output/Plots/mod_pc_is_hab_tbl.docx")




# run an AIC comparison of the above 3 models

anova(mod_pc_hab, mod_pc_is, mod_pc_is_hab, test = "Chisq")

# model with IS100 and Habitat is the best model

# REML criterion at convergence: 382.4

# Scaled residuals:
#     Min      1Q  Median      3Q     Max
# -2.0462 -0.6061 -0.1826  0.6289  2.8246 

# Random effects:
#  Groups   Name        Variance Std.Dev.
#  Site     (Intercept)  0.00    0.000
#  Residual             23.39    4.836
# Number of obs: 64, groups:  Site, 10

# Fixed effects:
#              Estimate Std. Error      df t value Pr(>|t|)  
# (Intercept)    1.1236     0.8421 61.0000   1.334    0.187
# IS100         -0.0746     0.0315 61.0000  -2.368    0.021 *
# HabitatUrban   0.1244     1.5677 61.0000   0.079    0.937
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Correlation of Fixed Effects:
#             (Intr) IS100
# IS100       -0.020
# HabitatUrbn -0.524 -0.636
# optimizer (nloptwrap) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')

# linear model on PC1 and TD100

#mod_pc_td<- lmer(PC1~ TD100+ (1|Site), 
#                data= DATA)

#summary(mod_pc_td, REML=F)

# linear model on PC1 and HPD500

#mod_pc_hpd<- lmer(PC1~ HPD500+ (1|Site), 
#                data= DATA)

#summary(mod_pc_hpd, REML=F)

# run a comparison of the models

#anova(mod_pc_an, mod_pc_env, mod_pc_is, mod_pc_td, mod_pc_hpd, test = "Chisq")

# Lasso regression to test wich variables are important
# Load required package

#install.packages("glmnet")
#library(glmnet)

# Prepare the data
#x <- model.matrix(PC1~IS100+TD100+HPD500, data=DATA)[,-1]
#y <- DATA$PC1

# Fit the model
#set.seed(123)  # for reproducibility
#cv.lasso <- cv.glmnet(x, y, alpha=1)

# Get coefficients
#coef(cv.lasso, s="lambda.min")

#print(cv.lasso)

# model with pc1 and habitat variables


##### 3.1 Environmental variables/ Habitat #####


modtotal1<- lmer(Total~ IS100+ Habitat+ (1|Site), 
                data= DATA)

summary(modtotal1, REML=F)

# Load required package
library(sjPlot)

# Create plotb using sjPlot
total_100_Hab<- plot_model(modtotal1, type = "pred", terms = c("IS100", "Habitat"), 
       show.values = TRUE, title = " ")


ggsave(filename = "Output/Plots/total_100_Hab.jpeg" ,
       plot = total_100_Hab, 
       device = "jpeg", 
       width = 100, 
       height = 100, 
       units = "mm") 


library(report)

report(modtotal1)  

# plot the residuals
plot(modtotal1, which = 1)

# qqplot with fitted line

qqline(resid(modtotal1))


# Run a linear model on each material and environment

# for anthro
modAnthro<- lmer(Anthro~ IS100+ Habitat +(1|Site), 
               data= DATA)

#shapiro.test(DATA$Anthro) #normality test I DONT KNOW why i did that here but its good to know

summary(modAnthro, REML=F)    

# plot model with sjPlot

Anthro_100_Hab<- plot_model(modAnthro, type = "pred", terms = c("IS100", "Habitat"), 
       show.values = TRUE, title = " ")

#model for moss

modMoss<- lmer(Moss~ IS100+ Habitat+ (1|Site), 
                data= DATA)

summary(modMoss, REML=F)

# create plot with sjPlot

Moss_100_Hab<- plot_model(modMoss, type = "pred", terms = c("IS100", "Habitat"), 
       show.values = TRUE, title = " ")

# model for feather
modFeather<- lmer(Feather~ IS100+ Habitat+ (1|Site), 
                data= DATA)


summary(modFeather, REML=F)

# create plot with sjPlot

Feather_100_Hab<- plot_model(modFeather, type = "pred", terms = c("IS100", "Habitat"), 
       show.values = TRUE, title = " ")

# model for wool  hair

modWool.Hair<- lmer(Wool.Hair~ IS100+ Habitat+ (1|Site), 
                data= DATA)

summary(modWool.Hair, REML=F)

# create plot with sjPlot

Wool.Hair_100_Hab<- plot_model(modWool.Hair, type = "pred", terms = c("IS100", "Habitat"), 
       show.values = TRUE, title = " ")

# combine all the plots using ggarrange

library(ggpubr)


# add the plot to the document
Material_Env_All <- ggarrange(total_100_Hab, Anthro_100_Hab, Moss_100_Hab, Feather_100_Hab, Wool.Hair_100_Hab, 
       labels = c("A", "B", "C", "D", "E"),
       ncol = 2, nrow = 3)

library(officer)

#save_as_doc #docx #doc
# install and load the officer package
install.packages("officer")
library(officer)

#word_document() #docx #doc 
# create a new Word document
Material_Env_All_Doc <- read_docx()

# add the plot to the document
Material_Env_All_Doc <- body_add_gg(Material_Env_All_Doc, value = Material_Env_All)

# save the document
print(Material_Env_All_Doc, target = "Output/Plots/Material_Env_All.docx")




# run a model disgnosis with dharma package

#install.packages("DHARMa")

#library(DHARMa)

# check model assumptions using DHARMa 

#residuals <- simulateResiduals(modWool.Hair, n=1000)

#test <- testResiduals(residuals)

# to test normality of data #normality_test
#shapiro.test(DATA$Wool.Hair)

##### 6. Breeding success #####

# Find hatching success 
DATA$hatch_per <- DATA$hatchlings/DATA$clutch_size

#remove rows that are greater than 100

DATA$hatch_per[DATA$hatch_per > 100] <- NA

# Find fledging success

DATA$fledg_per <- DATA$fledglings/DATA$hatchlings


# change the fledgling success to 0 if it is NA (NA occurs when there is no hatchlings)

DATA$fledg_per[is.na(DATA$fledg_per)] <- 0

# find unhatced eggs percentage

DATA$unhat_per <- DATA$unhatched/DATA$clutch_size

# save the data as csv file with data with hatching success in data folder

#write.csv(DATA, "Data/BreedNestEnv.csv")

#View(DATA)




##### 6.1 Breeding vs Nest Materials #####

##### Exploration 

# Run a logistic mixed-effects model on fledging success
# mod_fledg_all <- glmer(cbind(fledg_per, 100 - fledg_per) ~ 
#                          Anthro + Moss + Feather + Wool.Hair + (1|Site), 
#                       data = DATA, family = binomial)

# summary(mod_fledg_all)

# Run a poisson mixed-effects model on hatching success
# mod_hatch_all <- glmer(hatchlings ~ clutch_size+Anthro + Moss + Feather + Wool.Hair + (1|Site), 
#                       data = DATA, family = poisson)

# summary(mod_hatch_all)

# Run a poisson mixed-effects model on clutch size
# mod_clutch_all <- glmer(clutch_size ~ 
#                           Anthro + Moss + Feather + Wool.Hair + (1|Site), 
#                         data = DATA, family = poisson)

# summary(mod_clutch_all)

# Run a logistic mixed-effects model on hatching success
# mod_hatch_all<- glmer(hatch_per ~ 
#                           Anthro + Moss + Feather + Wool.Hair + (1|Site), 
#                         data = DATA, family = binomial)

# summary(mod_hatch_all)

# Run a logistic mixed-effects model on fledging success
# mod_fledg_all<- glmer(fledg_per ~ 
#                           Anthro + Moss + Feather + Wool.Hair + (1|Site), 
#                         data = DATA, family = binomial)

# summary(mod_fledg_all)

# Run a logistic mixed-effects model on unhatched eggs percentage
# control <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
# mod_unhatch_all <- glmer(unhat_per ~ Anthro + Moss + Feather + Wool.Hair + (1|Site), data = DATA, family = binomial, control = control)

# summary(mod_unhatch_all)

# Adding a small constant (To change 0 and 1 values, suggested by lot of people)
# epsilon <- 0.0001 #just a small number
# DATA$fledg_per_adjusted <- ifelse(DATA$fledg_per == 0, DATA$fledg_per + epsilon,
#                                   ifelse(DATA$fledg_per == 1, DATA$fledg_per - epsilon, DATA$fledg_per))

# Run a logistic mixed-effects model on adjusted fledging success
# mod_fledg_all <- glmer(fledg_per_adjusted ~ Anthro + Moss + Feather + Wool.Hair + (1|Site), 
#                        data = DATA, family = binomial)

# summary(mod_fledg_all)

# report package i think
# table_test<- report_table(mod_fledg_all)
 
# Save the table as a csv file
# write.csv(table_test, "table_test.csv")


##### Final ######

# for clutch size, using poisson distribution as it is count data

library(lme4)

mod_clutch_all <- glm(clutch_size ~ Anthro +Moss + Feather + Wool.Hair+0,
                        data = DATA, family = poisson)

summary(mod_clutch_all)

performance::check_model(mod_clutch_all)

# create a table with the results

library(gtsummary)

#mod_clutch_all_table <- mod_clutch_all %>%
#  tbl_regression(intercept = TRUE, 
 #                label = list(
  #                 `Anthro` = "Anthro", 
   #                `Moss` = "Moss",
    #               `Feather` = "Feather",
     #              `Wool.Hair` = "Wool.Hair"),
      #           include_se = FALSE,
       #          tidy_fun = broom.helpers::tidy_parameters) %>% 
 # add_significance_stars(hide_ci = FALSE, hide_p = FALSE)

#save using flextable package
# library(flextable)

#mod_clutch_all_table %>% 
 # as_flex_table() %>%
  #flextable::autofit() %>%
  #save_as_docx(path= "Output/Plots/mod_clutch_all_table.docx")


# create a model for hatchlings using poisson distribution as it is count data


# doing same with hatching success

mod_hatch_all<-  glm(hatch_per ~ Anthro + Moss + Feather + Wool.Hair+0, data = DATA, family = binomial ("logit"), weights = DATA$clutch_size)



summary(mod_hatch_all)

performance::check_model(mod_hatch_all)

# create a plot using sjplot package for mod_hatch_all
library(sjPlot)
plot_hatch_moss<- plot_model(mod_hatch_all, type = "pred", terms = c("Moss[all]"))+ ylab("Hatching Success (%)")+ xlab("Moss (g)")+ theme_bw()+ ggtitle(NULL)
#plot_hatch_anthro<- plot_model(mod_hatch_all, type = "pred", terms = c("Anthro[all]"))



# doing same with unhatched eggs

mod_unhatch_all<-  glm(unhat_per ~ Anthro + Moss + Feather + Wool.Hair+0, data = DATA, family = binomial ("logit"), weights = DATA$clutch_size)


summary(mod_unhatch_all)

#For the unhatched percentage, the estimates for the variables are as follows: Anthro (0.085038), Moss (-0.106361), Feather (-0.179863), and Wool.Hair (-0.005508). Moss is statistically significant with a p-value close to zero, indicating that a unit increase in Moss weight is expected to decrease the log-odds of unhatched egg percentage by 0.106361, all other variables held constant. Feather, with a p-value of 0.0919, is close to being significant at the 10% level, suggesting a potential negative effect on unhatched egg percentage. Both Anthro and Wool.Hair are not statistically significant with p-values of 0.3484 and 0.8916 respectively, suggesting they have no significant impact on the unhatched egg percentage. 

# for fledging, using binomial distribution as it is a proportion after a long time of searching
mod_fledg_all<-  glm(fledg_per ~ Anthro + Moss + Feather + Wool.Hair+0, data = DATA, family = binomial ("logit"), weights = DATA$clutch_size)


summary(mod_fledg_all)

plot_fledg_moss <- plot_model(mod_fledg_all, type = "pred", terms = c("Moss[all]")) +
  ylab("Fledgling Success (%)") +
  xlab("Moss (g)")+
  ggtitle(NULL)+ theme_bw()
#plot_fledg_anthro<- plot_model(mod_fledg_all, type = "pred", terms = c("Anthro[all]"))

# combine both plots using ggarrange function from ggpubr package

library(ggpubr)

plot_hf_moss<- ggarrange(plot_hatch_moss, plot_fledg_moss, ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

# save using ggsave function from ggplot2 package

ggsave("Output/Plots/plot_hf_moss.png", plot_hf_moss, width = 200, height = 100, units = "mm", dpi = 300)




#ggarrange(plot_hatch_anthro, plot_fledg_anthro, ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")

# trying to account for overdispersion in the model

# Quasi-binomial distribution (To account for overdispersion)

#mod_unhatch_all <- glm(unhat_per ~ Anthro + Moss + Feather + Wool.Hair,
 #           family = quasibinomial("logit"), data = DATA, weights = DATA$clutch_size)


#summary(mod_unhatch_all)



#plot(mod_unhatch_all) # plot residuals vs fitted values

#library( performance )
#performance::check_model (mod_unhatch_all) # produces interesting diagnostic plots



 
# this didnt work beacuse Dharma package doesnt work with quasibinomial so skipped it 

# Run GLMM with a random intercept for Site (Random intercept for Site and ID is used to account overdispersion)

# Load the lme4 package
# library(lme4)

# Fit a generalized linear mixed-effects model for unhatched eggs
#mod_unhatch_all <- glmer(unhat_per ~ Anthro + Moss + Feather + Wool.Hair + (1|Site)+ (1|ID),
#             family = binomial("logit"), data = DATA, weights = DATA$clutch_size)
# this model is working the best

# Show model summary
# summary(mod_unhatch_all)

# Perform residual analysis
# residuals <- simulateResiduals(mod_unhatch_all, n=1000)
# test <- testResiduals(residuals)

# create similarr model for hatching success
# mod_hatch_all <- glmer(hatch_per ~ Anthro + Moss + Feather + Wool.Hair + (1|Site)+ (1|ID),
#             family = binomial("logit"), data = DATA, weights = DATA$clutch_size)

# Show model summary
# summary(mod_hatch_all, reml=FALSE)

# do the same for fledging success
# mod_fledg_all <- glmer(fledg_per ~ Anthro + Moss + Feather + Wool.Hair + (1|Site)+ (1|ID),
#             family = binomial("logit"), data = DATA, weights = DATA$clutch_size)

# Show model summary
# summary(mod_fledg_all, reml=FALSE)

# ID as a random effect accounts for overdispersion and outliers, but takes away all significance from the model


# look at anthro alone 

library(lme4)
mod_anthro_unhatch <- glmer(unhatched_eggs ~ Anthro+ Habitat+ IS100+ (1|Site), data = DATA, family = poisson, weights = DATA$clutch_size)

summary(mod_anthro_unhatch)

report(mod_anthro_unhatch)

# create a plot for anthro vs unhatched eggs
# this currently has habitat as a fixed effect, I didnt report it in the paper, it is just anthro as other models are just anthro

library(sjPlot)
library(ggplot2)

plot_anthro_unhatch <- plot_model(mod_anthro_unhatch, type = "pred", terms = c("Anthro[all]", "Habitat [all]")) +
  ylab("Number of Unhatched Eggs") +
  xlab("Anthropogenic Material (g)")+
  ggtitle(NULL)+ theme_bw()


# save using ggsave function from ggplot2 package

ggsave("Output/Plots/plot_anthro_unhatch.png", plot_anthro_unhatch, width = 100, height = 100, units = "mm", dpi = 300)


#summary_statistics, regression_table

library(gtsummary)
mod_anthro_unhatch_tbl <- mod_anthro_unhatch %>%
  tbl_regression(intercept = TRUE,
                 label = list(
                   `(Intercept)` = "Intercept",
                   `Anthro` = "Anthro")) %>% 
  add_significance_stars(hide_ci = TRUE, hide_p = FALSE, hide_se = TRUE)

#save using flextable package
# library(flextable)

library(flextable)
mod_anthro_unhatch_tbl %>% 
  as_flex_table() %>%
  flextable::autofit() %>%
  save_as_docx(path= "Output/Plots/mod_anthro_unhatch_tbl_Whabitat.docx")

# Same for clutch size#########
mod_anthro_clutch <- glmer(clutch_size ~ Anthro+ Habitat+ IS100+ (1|Site), data = DATA, family = poisson, weights = DATA$clutch_size)

summary(mod_anthro_clutch)

report(mod_anthro_clutch)

library(car)
vif(lm(clutch_size ~ Anthro + Habitat + IS100, data=DATA))


# create a plot for anthro vs clutch size

plot_anthro_clutch <- plot_model(mod_anthro_clutch, type = "pred", terms = c("Anthro[all]")) +
  ylab("Clutch Size") +
  xlab("Anthropogenic Material (g)")+
  ggtitle(NULL)+ theme_bw()

# create a model for anthro vs hatchlings

mod_anthro_hatch <- glmer(hatchlings ~ Anthro+Habitat+ IS100+ (1|Site), data = DATA, family = poisson, weights = DATA$clutch_size)

summary(mod_anthro_hatch)

report(mod_anthro_hatch)

# create a plot for anthro vs hatchlings

plot_anthro_hatch <- plot_model(mod_anthro_hatch, type = "pred", terms = c("Anthro[all]")) +
  ylab("Number of Hatchlings") +
  xlab("Anthropogenic Material (g)")+
  ggtitle(NULL)+ theme_bw()

# low R2 value, not a good model

# create a model for anthro vs fledglings

mod_anthro_fledg <- glmer(fledglings ~ Anthro+ (1|Site), data = DATA, family = poisson, weights = DATA$clutch_size)

summary(mod_anthro_fledg)

report(mod_anthro_fledg)

# create a plot for anthro vs fledglings

plot_anthro_fledg <- plot_model(mod_anthro_fledg, type = "pred", terms = c("Anthro[all]")) +
  ylab("Number of Fledglings") +
  xlab("Anthropogenic Material (g)")+
  ggtitle(NULL)+ theme_bw()



##### Extra #####

##### 7 Doing combined PCA ##### 

#https://www.datacamp.com/tutorial/pca-analysis-r
# Doing a PCA to see if there is any correlation between the variables and the habitats

library('corrr')
library(ggcorrplot)
library("FactoMineR")
library(factoextra)

colSums(is.na(DATA)) # Removing NAs
# remove rows with NA values
DATA <- DATA[complete.cases(DATA), ]

# selecting only the numerical data
library(dplyr)
num_dat_all <- dplyr::select(DATA, clutch_size, hatchlings, fledglings, unhatched_eggs, Anthro, Moss, Feather, Wool.Hair)


#correlation matrix
# here the data needs to be normalized (which can be also done by coorelation matrix as well)
# different from the first PCA where all units were in grams
# here the units are counts and weight so need to be normalized
# i believe creation of correlation matrix normalizes the data
correlation_matrix <- cor(num_dat_all)

#plotting correlation matrix

ggcorrplot(correlation_matrix, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3, method = "circle", colors = c("tomato2", "white", "springgreen3"), title = "Correlation Matrix of Breeding Data")

#PCA

PCA_br_ne <- prcomp(num_dat_all, scale = TRUE) # use scale = TRUE to normalise the data data 

# in the previous PCA we normalized data before adding it to prcomb so didnt use scale = TRUE

#summary of PCA
summary(PCA_br_ne)


fviz_eig(PCA_br_ne, addlabels = TRUE) #eigen values 
fviz_cos2(PCA_br_ne, choice = "var", axes = 1:2) # contribution to PC 1 and 2 


#plotting PCA with habitats
PCA_br_ne_plot<-fviz_pca_biplot(PCA_br_ne, label= "var",
                habillage = DATA$Habitat,
                addEllipses = T)

# save the plot to an object and save it not directly from model
# save PCA1_plot using ggsave

library(ggplot2)

# save PCA1_plot using ggsave  in output folder


ggsave(filename = "Output/Plots/PCA_br_ne.jpeg" ,
       plot = PCA_br_ne_plot, 
       device = "jpeg", 
       width = 200, 
       height = 200, 
       units = "mm") 

       
       



##### LDA ####

library(MASS)

# LDA

# add numerical data to lda
# 1 select data
nst_dat_lda <- dplyr::select(DATA, Anthro, Moss, Feather, Wool.Hair, Habitat)

# 2 scale the data to have mean 0 and standard deviation 1
nst_dat_lda[1:4] <- scale(nst_dat_lda[1:4])


# 3 We can use the apply() function to verify that each predictor variable now has a mean of 0 and a standard deviation of 1:
apply(nst_dat_lda[1:4], 2, mean) 

apply(nst_dat_lda[1:4], 2, sd) 

#4 Create Training and Test Samples

#make this example reproducible
set.seed(1)

#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(nst_dat_lda), replace=TRUE, prob=c(0.7,0.3))
train <- nst_dat_lda[sample, ]
test <- nst_dat_lda[!sample, ] 

#fit LDA model
model <- lda(Habitat~., data=train)

#view model output
print(model)

#use LDA model to make predictions on test data
predicted <- predict(model, test)

names(predicted)

#view predicted class for first six observations in test set
head(predicted$class)

#view posterior probabilities for first six observations in test set
head(predicted$posterior)

#view linear discriminants for first six observations in test set
head(predicted$x)

#find accuracy of model
mean(predicted$class==test$Habitat)


# You might need to install the caret package if you haven't already
# install.packages('caret')

# Load the caret package if you haven't already

if(!require("caret")){
  install.packages("caret")
  library(caret)
}


# Generate the confusion matrix
confusionMatrix <- confusionMatrix(as.factor(predicted$class), as.factor(test$Habitat))
print(confusionMatrix)

#define data to plot
lda_plot <- cbind(train, predict(model)$x)


#create plot
lda_plot_img<- ggplot(lda_plot, aes(LD1)) +
  geom_density(aes(color = Habitat)) +
  ylab("Density")+ theme_bw()


#save lda_plot using ggsave  in output folder

ggsave(filename = "Output/Plots/lda_plot.jpeg" ,
       plot = lda_plot_img, 
       device = "jpeg", 
       width = 100, 
       height = 100, 
       units = "mm")

# contribution to ld1

# First, let's create a named vector with your loadings
loadings <- c(Anthro = 0.7322996, Moss = -0.2055307, Feather = 0.4714808, `Wool.Hair` = 0.8079382)

# Then, we can create a horizontal barplot with these loadings
library(ggplot2)

loadings_df <- data.frame(variable = names(loadings), loading = loadings)

Var_Con_LDA<- ggplot(loadings_df, aes(x = loading, y = variable)) +
            geom_bar(stat = "identity", fill = "steelblue") +
            xlab("Loadings") +
            ylab("Variable") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5))

#save Var_Con_LDA using ggsave  in output folder

ggsave(filename = "Output/Plots/Var_Con_LDA.jpeg" ,
       plot = Var_Con_LDA, 
       device = "jpeg", 
       width = 100, 
       height = 100, 
       units = "mm")


# add all the nest variables to cbind 

# 1 select data

nest_dat<- dplyr::select(DATA, Anthro, Moss, Feather, Wool.Hair, Habitat, clutch_size, hatchlings, fledglings, unhatched_eggs)

# run a manoava test to see if there is a difference in the nest variables between the habitats, IS100 and TD100


#install.packages("car")

library(car)


# run a manoava test to see if there is a difference in the nest_dat between the habitats, IS100 and TD100

manova_test <- manova(cbind(Total,Anthro,Moss,Feather,Wool.Hair) ~ IS100+TD100, data = DATA)


summary(manova_test, test = "Pillai")




