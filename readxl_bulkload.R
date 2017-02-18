# R Script - dplyr predominant
# Author: leerssej
# Date;  Sat Feb 11 15:01:42 2017 
 
# Desc: Loading and Bulk survey of 
## Desc: * header names
## Desc: * column types
# Desc: Loading of Character only Datasets
# Desc: Assembly of 
## Desc: * completely bound dataframes
## Desc: * completely header-free data only bound dataframes
## Desc: * completely bound first-instance-header dataframes

library(tidyverse)
library(readxl)

# read excel names with readxl hidden function
read_excel_headers <- function(excel_file)
{
    require("readxl")
    header_row <- 
        readxl:::xlsx_col_names(excel_file)
    header_row 
}

read_excel_width <- function(excel_file)
{
    require("readxl")
    header_length <- 
        length(readxl:::xlsx_col_names(excel_file))
    header_length 
}

# Function to read all the types with readxl hidden function
read_excel_coltypes <- function(excel_file)
{
    require("readxl")
    column_types <-
        readxl:::xlsx_col_types(excel_file, nskip = 1, n = 1)
    column_types
}
# Read excel dataframe
## With headers
### All types are recast `as.character``
read_charxl_strong <- function(excel_file)
{
    require("readxl")
    num_columns <- length(readxl:::xlsx_col_types(excel_file, nskip = 0, n = 1))
    datasheet <- readxl::read_excel(excel_file,
                                       col_types = rep("text", num_columns),
                                       col_names = F)
    datasheet 
}

# Read excel dataframe
## With headers all clipped - 
### All types are recast `as.character`
read_charxl_headless <- function(excel_file)
{
    require("readxl")
    num_columns <- length(readxl:::xlsx_col_types(excel_file, nskip = 0, n = 1))
    headlessdatasheet <- readxl::read_excel(excel_file,
                                       col_types = rep("text", num_columns),
                                       col_names = F,
                                       skip = 1)
    headlessdatasheet 
}

# function to character read and convert excel files
Tbl_convert <- function (file_)
{
    read_excel(paste0(file_,".xlsx")) %>%
    write.csv(paste0(file_,".csv"), na = "", row.names = F)
}

chopem <- function(element) {
    substr(element, 1, 5)
}

#### Edit File Type HERE ####
file_extension <- ".xlsx"
## Test local directory without spaces
file_path <- "C:/Users/Koyot/Documents/GitHub/readxl_bulkloadR/FailStack_readxl"
setwd(file_path)

# names of files to process
file_names <- 
    list.files(
        path = file_path,
        pattern = paste0("*", file_extension))
file_names

# generate a list for autoprocessing file tree in gitbash
file_ <- 
    sapply(file_names, chopem)
file_

# Throwdown all the .csv
sapply(file_, Tbl_convert)
           
# Table of all the columns counts
Tbl_widths <- 
    bind_cols(data_frame(file_names),
              data.frame(do.call("rbind", lapply(file_names, read_excel_width)))) %>% 
    rename(num_cols = do.call..rbind...lapply.file_names..read_excel_width..)
Tbl_widths

# Table of all the Headers
Tbl_headers <- 
    bind_cols(data_frame(file_names),
        data.frame(do.call("rbind", lapply(file_names, read_excel_headers))))
glimpse(Tbl_headers)

# Table of all the Types
Tbl_types <- 
    bind_cols(data_frame(file_names), 
    data.frame(do.call("rbind", lapply(file_names, read_excel_coltypes))))
Tbl_types




# Table of the Data aligned Strongly
# Completely bound dataframe
Tbl_complete <- 
    file_names %>% 
    map_df(~read_charxl_strong(.))
glimpse(Tbl_complete)

# Completely header-free bound dataframes
Tbl_dataOnly <- 
    file_names %>% map_df(~read_charxl_headless(.))
glimpse(Tbl_dataOnly)

# Table of the Data aligned cleanly
# Completely bound first-instance-header dataframes
## With headers all clipped - 
### All types are recast `as.character`
#### NB: Prudent wranglers will not rely on merging dataframes unreviewed.
#### NB: Specifically, the headers are not integrated in final dataframe 
#### NB: This means that dataframes are essentially row_bound.
#### NB: Be sure that you have checked that columns align properly between 
#### NB: dataframes. Use Header tables diligently.
Tbl_complete_clean <- 
    bind_rows(Tbl_headers[1,-1], file_names %>% map_df(~read_charxl_headless(.)))
glimpse(Tbl_complete_clean)
warnings()

Tbl_CompleteIntegerTest <- 
    Tbl_complete %>% 
    mutate(int = as.numeric(X11) %% 1) %>% 
    filter(int > 0)
glimpse(Tbl_CompleteIntegerTest)

#singleton in 10L type sampling and cast
Tbl0880 <- 
    read_excel("0880_.xlsx")
warnings()

