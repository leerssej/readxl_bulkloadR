# R Script - dplyr predominant
# Author: leerssej
# Date;  Thu Apr 06 14:38:52 2017 
 
# Desc: Generate a table that links object expenses to grant expenses to object expenses
# Desc: STI can filter out expenses (all program codes >1000) and
# Desc: see where they are being reported as expensed again in our system.
# Desc: 

library(tidyverse)
library(magrittr)
library(RPostgres)
library(googlesheets)
library(readxl)
library(ggmap)
library(geosphere)
library(stringdist)
library(recordlinkage)

###### 1. Load Data ######
path = "/Users/Koyot/Dropbox (BrightBytes)/Financial Transparency Portal/03_Implementation/FIN Pipeline Source Files/2015-2016/Facts/LEP_Issues/"

# finco_cmplt <- read_csv("/Users/Koyot/Dropbox (BrightBytes)/Financial Transparency Portal/03_Implementation/FIN Pipeline Source Files/2015-2016/Facts/LEP_Issues/colorado-financial-public-data-pipeline-v-1-Run#1-export-source-lep-facts.csv", )
# glimpse(finco_cmplt)
finco_cmplt <- read_csv(paste0(path, "colorado-financial-public-data-pipeline-v-1-Run#1-export-source-lep-facts.csv"), col_types = cols(.default = "c"))
glimpse(finco_cmplt)

###### 2. filter and join 594 coded objects to the grant codes to which they are assigned ######
# and were later spent out from 
finco_594grants <-
    finco_cmplt %>%
    # filter(district_code == "980", school_code == "469") %>% 
    filter(object_source_code == "594")
glimpse(finco_594grants)    
finco_594grants

finco_594grants_objects <- 
    finco_594grants %>% 
    # rename_all(funs())
    left_join(finco_cmplt, by = c("district_code", "school_code", "grant_code")) %>% 
    filter(as.integer(object_source_code.y) < 1000) #%>% 
    # filter(grant_code == "3113")
glimpse(finco_594grants_objects)

# save data, and write into a .csv for later restart
save(finco_594grants_objects, file = paste0(path, "finco_594grants_objects"))
write_csv(finco_594grants_objects, paste0(path, "finco_594grants_objects.csv"), na = "")
load(paste0(path, "finco_594grants_objects"))
glimpse(finco_594grants_objects)

###### 3. Assessing the breadth of the issue ######
# Not sure of the best way to do this.
## 2 approaches taken in the excel output were 
### 1) getting a snapshot of distinct districts involved.
### 2) getting a snapshot of distinct districts and schools involved.
## Waiting for feedback on the best way to quantify the issues for Evan.

# finco_594grants_counts <- 
#     finco_594grants %>% 
#     group_by()