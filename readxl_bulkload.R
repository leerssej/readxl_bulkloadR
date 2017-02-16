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

#### Edit File Type HERE ####
file_extension <- ".xlsx"
## Test local directory without spaces
file_path <- "C:/Users/Koyot/Documents/GitHub/readxl_bulkloadR/FailStack"
# To navigate to a file directory that travels through space filled directory names -
# Just escape all the backslashes.
file_path <- "C:\\Users\\Koyot\\Dropbox (BrightBytes)\\Financial Transparency Portal\\03_Implementation\\FIN Pipeline Source Files\\2015-2016\\Facts\\FailStack\\"
setwd(file_path)

# variables
file_names <- 
    list.files(
        path = file_path,
        pattern = paste0("*", file_extension))

# Table of all the columns counts
Tbl_widths <- 
    bind_cols(data_frame(file_names),
              data.frame(do.call("rbind", lapply(file_names, read_excel_width)))) %>% 
    rename(num_cols = do.call..rbind...lapply.file_names..read_excel_width..)
glimpse(Tbl_widths)
names(Tbl_widths)
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
## With headers interspersed
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

# Table of the Data aligned cleanly
## With headers all clipped - 
### All types are recast `as.character`
#### NB: Prudent wranglers will not rely on merging dataframes unreviewed.
#### NB: Specifically, the headers are not integrated in final dataframe 
#### NB: This means that dataframes are essentially row_bound.
#### NB: Be sure that you have checked that columns align properly between 
#### NB: dataframes. Use Header tables diligently.
read_charxl_clean <- function(excel_file)
{
    require("readxl")
    num_columns <- length(readxl:::xlsx_col_types(excel_file, nskip = 0, n = 1))
    headlessdatasheet <- readxl::read_excel(excel_file,
                                       col_types = rep("text", num_columns),
                                       col_names = F,
                                       skip = 1)
    headlessdatasheet 
}

# Completely bound dataframe
Tbl_complete <- 
    file_names %>% 
    map_df(~read_charxl_strong(.))
glimpse(tbl_complete)

# Completely header-free bound dataframes
Tbl_dataonly <- 
    file_names %>% map_df(~read_charxl_clean(.))
glimpse(tbl_dataonly)

# Completely bound first-instance-header dataframes
Tbl_complete_clean <- 
    bind_rows(tbl_headers[1,-1], file_names %>% map_df(~read_charxl_clean(.)))
glimpse(tbl_complete_clean)

Tbl_CompleteIntegerTest <- 
    Tbl_complete %>% 
    mutate(int = as.numeric(X3) %% 1) %>% 
    filter(int > 0)
glimpse(Tbl_CompleteIntegerTest)

10.5 %% 1

