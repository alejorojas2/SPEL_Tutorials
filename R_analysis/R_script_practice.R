# Importing text files using base R
Spinach_CFUs <- read.delim("C:/Users/18705/Desktop/R_practice/Spinach_CFUs.tsv", sep = "\t")


# Importing data from excel
library(readxl)
Spinach_CFU_Data <- read_excel("R_analysis/Spinach Pilot 2 CFU Data.xlsx",
                                                           range = "A3:E21", col_types = c("text",
                                                                                           "date", "text", "numeric", "text"))
#Checking the strcuture of the new table
str(Spinach_CFU_Data)

Spinach_CFU_Data$`CFU/mL`

#### Both commands below are not longer required since you fixed the excel
#Deleting the character value from the data table
#Spinach_CFU_Data$`CFU/mL`[12] <- ""

#Transforming column as numeric
#Spinach_CFU_Data$`CFU/mL` <- as.numeric(Spinach_CFU_Data$`CFU/mL`)
#############################


# Creating a column with CFU in 50 mL solutions
Spinach_CFU_Data$CFU_50mL <- Spinach_CFU_Data$`CFU/mL`*50

################
# This step is to separate reps from time into new columns, but it could be also
# done fixing the excel.  I will use library tidyverse

library(tidyverse)

#Extracting the rep number as new column
Spinach_CFU_Data <- Spinach_CFU_Data %>%
  mutate(rep_num = str_sub(Rep, 1, 1)) %>%
  mutate(rep_num = as.numeric(rep_num)) %>%
  mutate(loc_simple = str_sub(Location, 1, 4))

#Extracting the sampling time and setting everything as days post infection (dpi)
Spinach_CFU_Data$Time_Inoc <- str_extract(Spinach_CFU_Data$Rep, "\\d{1,2} [A-Z.a-z]{2,3}") %>% 
  str_replace("Day|Hr", "") %>% 
  str_replace("16", "1") %>%
  as.numeric()

#Setting up the levels for inoulation time so they appear in order in graphs
#Spinach_CFU_Data$Time_Inoc <- factor(Spinach_CFU_Data$Time_Inoc, levels = c("1 dpi",
                                                                            "7 dpi",
                                                                            "12 dpi"))
# Double checking the variables (columns) in the data
str(Spinach_CFU_Data)

######### ggplot ########
# Plotting results
# ggplot works in layers, so let's call the data first defining the dataset
# then using aesthetics - aes, to define x and y for the graph

ggplot(data = Spinach_CFU_Data, aes(x = Time_Inoc, y = CFU_50mL))

# Now we need to decide what kind of geom we want to see (i.e. points = geom_point, boxplot = geom_box)

ggplot(data = Spinach_CFU_Data, aes(x = Time_Inoc, y = CFU_50mL)) +
  geom_point()

# Now we need to color by treatment
ggplot(data = Spinach_CFU_Data, aes(x = Time_Inoc, y = CFU_50mL)) +
  geom_point(aes(color = `Treatment (Inoculum Concentration)`))

# We could increase the point size and correct the labels
ggplot(data = Spinach_CFU_Data, aes(x = Time_Inoc, y = CFU_50mL)) +
  geom_point(aes(color = `Treatment (Inoculum Concentration)`), size = 3) +
  labs(x = "Days post inoculation (dpi)", y = "CFU per 50 mL")

# Let's try a box plot
ggplot(data = Spinach_CFU_Data, aes(x = as_factor(Time_Inoc), y = CFU_50mL)) +
  geom_boxplot(aes(fill = `Treatment (Inoculum Concentration)`), alpha = 0.5) +
  labs(x = "Days post inoculation (dpi)", y = "CFUs per 50 mL") +
  guides(fill = guide_legend("Treatment")) +
  theme(text = element_text(size=14, face = "bold"))
  
#Let's try a line plot

ggplot(data = Spinach_CFU_Data, aes(x = Time_Inoc, y = CFU_50mL)) +
  geom_point(aes(color = `Treatment (Inoculum Concentration)`), alpha = 0.5, size = 4) +
  #geom_line(aes(color = `Treatment (Inoculum Concentration)`)) +
  stat_summary(aes(color = `Treatment (Inoculum Concentration)`), fun = mean, 
               geom = "line", size = 2) +
  labs(x = "Days post inoculation (dpi)", y = "CFUs per 50 mL") +
  guides(color = guide_legend("Treatment")) +
  theme(text = element_text(size=14, face = "bold"))

#Summarizing
Spinach_df_sum <- Spinach_CFU_Data %>%
  group_by(`Treatment (Inoculum Concentration)`, Time_Inoc) %>%
  summarise(
    sd = sd(CFU_50mL, na.rm = TRUE),
    se = sd(CFU_50mL, na.rm = TRUE)/sqrt(length(CFU_50mL)),
    CFU_50mL = mean(CFU_50mL)
  )


#Plot with error bars
ggplot(data = Spinach_df_sum, aes(x = Time_Inoc, y = CFU_50mL)) +
  geom_errorbar(aes(ymin = CFU_50mL-se, ymax = CFU_50mL+se, 
                    color = `Treatment (Inoculum Concentration)`), width = 0.5,
                position = position_dodge(0.2), size = 1.2) +
  geom_point(aes(color = `Treatment (Inoculum Concentration)`),
             position = position_dodge(0.2), size = 3) +
  geom_line(aes(color = `Treatment (Inoculum Concentration)`, group = `Treatment (Inoculum Concentration)`), , size = 1.5) + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_x_continuous(breaks = seq(1,14,2)) +
  labs(x = "Days post inoculation (dpi)", y = "CFUs per 50 mL") +
  guides(color = guide_legend("Treatment")) +
  theme(text = element_text(size=14, face = "bold"))
  