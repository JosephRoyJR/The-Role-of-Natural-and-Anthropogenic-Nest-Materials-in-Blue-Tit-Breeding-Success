##### TOP- Urbanization and Materials ######
# Joseph Roy
# Created on 08/08/2023
# Last Edited on- 08/08/2023

##### Load Data #####

rm(list = ls()) # Clearing the workspace

# Load libraries and data

source("Script/Final_Models.R")


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
  library(corrr)
  library(ggcorrplot)
  library(FactoMineR)
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
  library(webshot2)
  library(gt)

}

load_libraries()

# Factorizing the data
DATA$Urban.rural <- as.factor(DATA$Habitat) #making factors factors 
DATA$Site <- as.factor(DATA$Site)

#view data
View(DATA)

##### summary of data #####

#library(gtsummary)
#library(gt)
#library(webshot2)

summary_data<- select(DATA, Total, Anthro, Moss, Feather, Wool.Hair, Habitat) #selecting numerical values


summary_data_table <- tbl_summary(summary_data, by = Habitat, statistic = list(all_continuous() ~ "{mean}, ({median}), ({sd})"))


#library(flextable)

summary_data_table %>% 
  as_flex_table() %>%
  flextable::autofit() %>%
  save_as_docx(path= "Output/Data/summary_data_table.docx")

##### 1.2 PCA Nest #####

# https://www.datacamp.com/tutorial/pca-analysis-r
# PCA- To see overall composition of urban and rural nests


#library('corrr')
#library(ggcorrplot)
#library("FactoMineR")
#library(factoextra)

colSums(is.na(DATA)) # Checking NAs

#library(dplyr)
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
#summary(PCA)

# Extract PC1 scores
#pc1_scores <- PCA$x[, "PC1"]

# Add PC1 scores to your data
#DATA$PC1 <- pc1_scores

hist(DATA$PC1)

# save data as csv

#write.csv(DATA, file = "Data/BreedNestEnv.csv")


##### 1.4 Linear model on PC1 #####

# linear model on PC1, IS100 and Habitat

mod_pc_is<- lmer(Total~ IS100+ (1|Site), 
                data= DATA)

summary(mod_pc_is, REML=F)

# doing same for TD100

mod_pc_td<- lmer(Total~ TD100+ (1|Site), 
                data= DATA)

summary(mod_pc_td, REML=F)

# craeting a model for both IS100 and TD100

mod_pc_istd<- lmer(Total~ IS100+TD100+ (1|Site), 
                data= DATA)

# doing the same for habitat

mod_pc_ishab<- lmer(Total~ IS100+ Habitat+ (1|Site), 
                data= DATA)

summary(mod_pc_ishab, REML=F)

#create a model for TD100 and Habitat

mod_pc_tdhab<- lmer(Total~ TD100+Habitat+ (1|Site), 
                data= DATA)

# Combining both IS100 and Habitat

mod_pc_istdhab<- lmer(Total~ IS100+TD100+Habitat+ (1|Site), 
                data= DATA)

summary(mod_pc_is_hab, REML=F)

# add interaction term

mod_pc_istdhab_int<- lmer(Total~ IS100*TD100+ IS100*Habitat+ TD100+Habitat+ (1|Site), 
                data= DATA)

summary(mod_pc_istdhab_int, REML=F)

# run an AIC comparison of the above models

library(lmerTest)

anova(mod_pc_is, mod_pc_td,mod_pc_istd, mod_pc_ishab, mod_pc_tdhab, mod_pc_istdhab, mod_pc_istdhab_int)


as_flextable(model_sel_IS100)

# plot a model using sjPlot

library(sjPlot)

plot_model(mod_pc_is, type = "pred", terms = c("IS100")) +
  xlab("Impervious Surface Area- 100m") +
  ylab("Principal Component 1") +
  ggtitle(NULL)+ 
  theme_minimal()
# save it using ggsave

ggsave(filename = "Output/Plots/PC1_IS100.jpg",
       plot = plot_model(mod_pc_is_hab, type = "pred", terms = c("IS100")) +
         xlab("Impervious Surface Area- 100m") +
         ylab("PC1") +
         ggtitle(NULL)+ 
         theme_minimal(), 
       device = "jpeg", 
       width = 100, 
       height = 100, 
       units = "mm")

# create a model summary table using tbl_regression

library(gtsummary)

mod_pc_is_hab_tbl <- mod_pc_is %>%
  tbl_regression(intercept = TRUE) %>% 
  add_significance_stars(hide_ci = T, hide_p = FALSE, hide_se = TRUE)

#save using flextable package
# library(flextable)

mod_pc_is_hab_tbl %>% 
  as_flex_table() %>%
  flextable::autofit() %>%
  save_as_docx(path= "Output/Plots/mod_pc_is_hab_tbl.docx")


# model with IS100 is the best model 

##### 3.1 Environmental variables/ Habitat #####


modtotal1<- lmer(Total~ IS100+ (1|Site), 
                data= DATA)

summary(modtotal1, REML=F)

# Random effects:
#  Groups   Name        Variance Std.Dev.
#  Site     (Intercept) 0.5444   0.7378  
#  Residual             1.2084   1.0993
# Number of obs: 64, groups:  Site, 10
# 
# Fixed effects:
#              Estimate Std. Error        df t value Pr(>|t|)
# (Intercept)  0.551660   0.335040  7.128980   1.647    0.143
# IS100        0.006616   0.008470 18.354748   0.781    0.445

# Load required package
library(sjPlot)

# Create plotb using sjPlot
total_100_Hab<- plot_model(modtotal1, type = "pred", terms = c("IS100" ), 
       show.values = TRUE, title = " ") + ylab ("Total (g)")+ theme_minimal()



library(report)

report(modtotal1)  

# plot the residuals
plot(modtotal1, which = 1)

# qqplot with fitted line

qqline(resid(modtotal1))


# Run a linear model on each material and environment

# for anthro
modAnthro<- lmer(Anthro~ IS100 +(1|Site), 
               data= DATA)

#shapiro.test(DATA$Anthro) #normality test I DONT KNOW why i did that here but its good to know

summary(modAnthro, REML=F)    
# plot model with sjPlot

Anthro_100_Hab<- plot_model(modAnthro, type = "pred", terms = c("IS100"), 
       show.values = TRUE, title = " ")+ ylab ("Anthropogenic Material (g)")+ theme_minimal()

# This code block shows the results of a mixed-effects model with random effects for Site and fixed effects for IS100. The output includes the estimates, standard errors, degrees of freedom, t-values, and p-values for the fixed effects. The random effects table shows the variance and standard deviation for the intercept at the Site level and the residual variance and standard deviation. The model was fit to 64 observations from 10 different sites.
# Random effects:
# Groups   Name        Variance Std.Dev.
# Site     (Intercept) 0.5444   0.7378  
# Residual             1.2084   1.0993
# Number of obs: 64, groups:  Site, 10
# 
# Fixed effects:
# Estimate Std. Error        df t value Pr(>|t|)
# (Intercept)  0.551660   0.335040  7.128980   1.647    0.143
# IS100        0.006616   0.008470 18.354748   0.781    0.445



# add a column in DATA where Anthro is 0 if it is 0 and 1 if it is >0

#DATA$Anthro10<- ifelse(DATA$Anthro>0, 1, 0)

#View(DATA)

# save the new data frame as a csv to breednestenv

#write.csv(DATA, "Data/BreedNestEnv.csv", row.names = FALSE)

# find the percentage of rows with Anthro >0

sum(DATA$Anthro10)/nrow(DATA)*100 # 71.875% nests had anthropogenic material


# find the percentage of rows with Anthro equal to 0

sum(DATA$Anthro2==0)/nrow(DATA) # 28.125% nests had no anthropogenic material

#model for moss

modMoss<- lmer(Moss~ IS100+ (1|Site), 
                data= DATA)

summary(modMoss, REML=F)    

# create plot with sjPlot

Moss_100_Hab<- plot_model(modMoss, type = "pred", terms = c("IS100"), 
       show.values = TRUE, title = " ")+ylab("Moss (g)")+ theme_minimal()

#Scaled residuals:
#    Min      1Q  Median      3Q     Max
#-2.0146 -0.6215 -0.1928  0.6541  2.8188

#Random effects:
# Groups   Name        Variance Std.Dev.
# Site     (Intercept)  0.00    0.000
# Residual             22.81    4.776   
#Number of obs: 64, groups:  Site, 10

#Fixed effects:
#            Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept) 15.52222    0.70810 62.00000  21.921  < 2e-16 ***
#IS100       -0.07199    0.02400 62.00000  -2.999  0.00389 **

# model for feather
modFeather<- lmer(Feather~ IS100+ (1|Site), 
                data= DATA)


summary(modFeather, REML=F)

# create plot with sjPlot

Feather_100_Hab<- plot_model(modFeather, type = "pred", terms = c("IS100"), 
       show.values = TRUE, title = " ")+ylab("Feather (g)")+ theme_minimal()




# Random effects:
# Groups   Name        Variance Std.Dev.
# Site     (Intercept) 2.3151   1.5215
# Residual             0.9509   0.9752
# Number of obs: 64, groups:  Site, 10
# 
# Fixed effects:
# Estimate Std. Error        df t value Pr(>|t|)  
# (Intercept)  1.462621   0.555468  5.855174   2.633   0.0398 *
# IS100       -0.001109   0.010239 42.363930  -0.108   0.9143
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1



# model for wool  hair

modWool.Hair<- lmer(Wool.Hair~ IS100+ (1|Site), 
                data= DATA)

summary(modWool.Hair, REML=F)

# create plot with sjPlot

library(ggplot2)

Wool.Hair_100_Hab <- plot_model(modWool.Hair, type = "pred", terms = c("IS100"), 
                                 show.values = TRUE, title = " ")+ ylab("Wool+Hair (g)")+ theme_minimal()


# combine all the plots using ggarrange

library(ggpubr)


# add the plot to the document
Material_Env_All <- ggarrange(total_100_Hab, Anthro_100_Hab, Moss_100_Hab, Feather_100_Hab, Wool.Hair_100_Hab, 
       labels = c("A", "B", "C", "D", "E"),
       ncol = 2, nrow = 3)


# save the plot with the minimal theme
ggsave("Output/Plots/Material_Env_All.png", Material_Env_All, width = 200, height = 250, units = "mm")

#### Breeding Success#### 

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




##### Unhatched modanthro ####

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

  # create a plot for anthro vs unhatched eggs


