

# Setting the working directory
source("Script/Habitat Nest Exploration.R")



# Importing the data

DATA <- read.csv("Data/BreedNestEnv.csv")

library(ggplot2)
library(dplyr)
library(esquisse)
library(ggplot2)
library(esquisse)


esquisser()





library(ggplot2)

Anthro_Habitat<- ggplot(DATA) +
 aes(x = Anthro, y = Habitat, fill = Habitat) +
 geom_boxplot() +
 scale_fill_hue(direction = 1) +
 coord_flip() +
 theme_minimal() +
 xlab("Anthropogenic Material (g)")



# create a similar plot for the other variables

# Feather/Habitat

Feather_Habitat<- ggplot(DATA) +
 aes(x = Feather, y = Habitat, fill = Habitat) +
 geom_boxplot() +
 scale_fill_hue(direction = 1) +
 coord_flip() +
 theme_minimal() +
 xlab("Feather (g)")


# Moss/Habitat

Moss_Habitat<- ggplot(DATA) +
 aes(x = Moss, y = Habitat, fill = Habitat) +
 geom_boxplot() +
 scale_fill_hue(direction = 1) +
 coord_flip() +
 theme_minimal() +
 xlab("Moss+Grass (g)")


# Wool/ Habitat
woool_habitat<- ggplot(DATA) +
  aes(x = Wool.Hair, y = Habitat, fill = Habitat) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  coord_flip() +
  xlab("Wool+Hair (g)")

# Total and urban rural(Violin)
total_violin<- ggplot(DATA) +
 aes(x = Habitat, y = Total, fill = Habitat) +
 geom_violin(adjust = 1L, scale = "area", alpha = 0.5) +
 geom_boxplot(width = 0.3, fill = "white", outlier.shape = NA, alpha = 0.5) +
 geom_jitter(aes(color = Habitat), width = 0.2, height = 0, alpha = 0.5) +
 scale_fill_manual(values = c(Rural = "#F8766D", Urban = "#35AA7B")) +
 theme_minimal() +
 ylab("Total (g)")


  # combine all the plots into one plot ggarrange

library(ggpubr)
library(ggpubr)

# Create a list of plots
plots <- list(total_violin, Anthro_Habitat, Feather_Habitat, Moss_Habitat, woool_habitat)

# Add a common legend to the plots
library(ggpubr)

all_raw_val <- ggarrange(plotlist = plots, labels = c("A", "B", "C", "D", "E"), ncol = 3, nrow = 3, common.legend = TRUE, legend = "none")

# Save the plot using ggsave to plots folder
ggsave(filename = "Output/Plots/all_raw_val.jpg",
  plot = all_raw_val, 
  device = "jpeg", 
  width = 190,
  height = 210,
  units = "mm")





# This is a good method but I didnt use it here as it was making the plot too small


# Load the officer package to create and edit Microsoft Word documents in R
# library(officer)

# Create a new Word document
# all_raw_val_doc <- read_docx()

# Add the plot to the document
# all_raw_val_doc <- body_add_gg(all_raw_val_doc, value = all_raw_val)

# Save the document to the specified file path
# print(all_raw_val_doc, target = "Output/Plots/all_raw_val_doc.docx")

# This script loads the officer package to create and edit Microsoft Word documents in R. It then creates a new Word document and adds a plot to it. Finally, it saves the document to the specified file path.



# I didnt use the rest of  codes just for exploring the data

# Total and urban rural (Boxplot)
ggplot(DATA) +
 aes(x = Habitat, y = Total, fill = Habitat) +
 geom_boxplot() +
 scale_fill_manual(values = c(Rural = "#F8766D", 
Urban = "#35AA7B")) +
 theme_bw()

#Anthro/Nest  Colour by Urban Rural)
ggplot(DATA) +
  aes(x = ID, y = Anthro, colour = Habitat) +
  geom_point(shape = "circle", size = 2.8) +
  scale_color_hue(direction = 1) +
  theme_minimal()

# Anthro/Sites
ggplot(DATA) +
  aes(x = Site, y = Anthro, fill = Habitat) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  theme_minimal()


# Feather/ Sites (Coloured by Urban Rural)
ggplot(DATA) +
 aes(x = Site, y = Feather, fill = Habitat) +
 geom_boxplot() +
 scale_fill_hue(direction = 1) +
 theme_bw()

#Site/Moss (Coloured by Urban Rual)
ggplot(DATA) +
 aes(x = Site, y = Moss, fill = Habitat) +
 geom_boxplot() +
 scale_fill_hue(direction = 1) +
 coord_flip() +
 theme_bw() +
 facet_wrap(vars(Habitat))



#moss/sites
ggplot(DATA) +
 aes(x = Site, y = Moss, fill = Habitat) +
 geom_col() +
 scale_fill_hue(direction = 1) +
 theme_minimal()+ coord_flip()

#feather/sites
ggplot(DATA) +
 aes(x = Site, y = Feather, fill = Habitat) +
 geom_col() +
 scale_fill_hue(direction = 1) +
 theme_minimal()

#arthropod/sites
ggplot(DATA) +
 aes(x = Site, y = Arthropod, fill = Habitat) +
 geom_boxplot() +
 scale_fill_hue(direction = 1) +
 theme_minimal()

# Date and arthropod
ggplot(DATA) +
  aes(x = Date, y = Arthropod, fill = Site) +
  geom_col() +
  scale_fill_viridis_d(option = "viridis", 
                       direction = 1) +
  theme_minimal()

