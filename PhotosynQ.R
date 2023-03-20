library(tidyverse)
library(PhotosynQ)

PhotosynQ::login("jarojas@uark.edu")

ID <- 14908

Lettuce_path <- PhotosynQ::getProject(ID)

df <- Lettuce_path$`Photosynthesis RIDES 2.0` %>%
  filter(status == "submitted")
