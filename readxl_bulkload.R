# R Script - dplyr predominant
# Author: leerssej
# Date;  Sat Feb 11 15:01:42 2017 
 
# Desc: Loading and Bulk survey of 
## Desc: * header names
## Desc: * column types
# Desc: Loading of Character only Datasets
# Desc: Assembly of 
## Desc: * completely bound dataframes
## Desc: * completely header-free bound dataframes
## Desc: * completely bound first-instance-header dataframes

library(tidyverse)
library(readxl)

# read excel names with readxl hidden function
read_excel_header <- function(excel_file)
{
    require("readxl")
    header_row <- 
        readxl:::xlsx_col_names(excel_file)
    header_row 
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

# variables
file_list <- list.files(pattern=paste0("*", file_extension))

# Table of all the Headers
tbl_headers <- 
    bind_cols(data_frame(file_list),
        data.frame(do.call("rbind", lapply(file_list, read_excel_header))))
glimpse(tbl_headers)

# Table of all the Types
tbl_types <- 
    bind_cols(data_frame(file_list), 
    data.frame(do.call("rbind", lapply(file_list, read_excel_coltypes))))
tbl_types

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
tbl_complete <- 
    file_list %>% 
    map_df(~read_charxl_strong(.))
glimpse(tbl_complete)

# Completely header-free bound dataframes
tbl_dataonly <- 
    file_list %>% map_df(~read_charxl_clean(.))
glimpse(tbl_dataonly)

# Completely bound first-instance-header dataframes
tbl_complete_clean <- 
    bind_rows(tbl_headers[1,-1], file_list %>% map_df(~read_charxl_clean(.)))
glimpse(tbl_complete_clean)
