# Set Up ----
## packages
if(!require("pacman")){
  install.packages("pacman")
}
p_load("tidyverse","here","fs","janitor","readxl",
       "modelsummary","skimr","gtsummary")

## Files and Folders
raw_data <- here("Data","raw")
inter_data <- here("Data","inter")

# Data
data_waste <- read_excel(here(raw_data,
                              "WARD WISE FOOD WASTE QUANTITY.xlsx")) %>% 
  clean_names()
data_waste <- data_waste %>% 
  mutate(ward=as.factor(ward),
         n_adults = household_size-(no_of_children+no_of_senior_citizens))

# Summary statistics of total food wastage (aggregate)

datasummary(~total*(mean+N+sd+median),data=data_waste %>% 
              drop_na())


## Summary statistics of total food wastage (wardwise)

datasummary(ward~total*(mean+N+sd+median),data=data_waste %>% 
              drop_na())
# Summary statistics of awf annd uwf
datasummary(~afw*(mean+N+sd+median),data=data_waste %>% 
              drop_na()) # afw aggregate


datasummary(ward~afw*(mean+N+sd+median),data=data_waste %>% 
              drop_na()) #afw warwise

datasummary(~ufw*(mean+N+sd+median),data=data_waste %>% 
              drop_na()) #ufw aggregate


datasummary(ward~ufw*(mean+N+sd+median),data=data_waste %>% 
              drop_na())


data_waste <- data_waste %>% 
  drop_na %>% 
  mutate(across(c(6:9),~total/.x,
                .names = "average_{.col}"))


datasummary(~average_household_size*(mean+N+sd+median),data = data_waste)

## box plot
data_waste %>% 
  ggplot()+
  aes(y=total,colour = ward)+
  geom_boxplot()
