dirYear <- "2022"
dirProject <- "2022-04-23-food-inflation-poor-household"

here::i_am(paste(dirYear, dirProject, "src", "visualize.R", sep = "/"))


# Packages ----
library(paletteer) 


# Plot ----

# Color palette for the charts
paletteer_d("ggthemes::Miller_Stone")
