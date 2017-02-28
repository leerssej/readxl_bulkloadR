#!/usr/bin/Rscript
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
# install.packages("tidyverse", repos = "https://cran.cnr.berkeley.edu")


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
read_charxl_full <- function(excel_file)
{
    require("readxl")
    num_columns <- length(readxl:::xlsx_col_types(excel_file, nskip = 0, n = 1))
    datasheet <- readxl::read_excel(excel_file,
                                       col_types = rep("text", num_columns),
                                       col_names = F)
    datasheet 
}

Tbl_convertAllText2Numeric <- function (file_)
{
    read_charxl_full(paste0(file_,".xlsx")) %>%
    mutate(AmountInteger = suppressWarnings(as.integer(.[[ncol(.)]]))) %>% # last col to int
    select(1:10, ncol(.)) %>% # select 10 + Corrected Amount
    filter(!is.na(AmountInteger)) %>% 
    mutate_all(funs(as.integer)) %>% 
    write.csv(paste0(file_,".csv"), na = "", row.names = F)
}


chopem <- function(element) {
    substr(element, 1, 5)
}

#### Edit File Type HERE ####
file_extension <- ".xlsx"
## Test local directory without spaces
file_path <- "C:/Users/Koyot/Documents/GitHub/readxl_bulkloadR/bulkloadR_staging/"
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
# write my_branch_list to run the git branch building
write.table(file_, "my_branch_list", na = "", row.names = F, sep = " ", col.names = F, quote = F, eol = " ")
file_

# Table of all the columns counts
Tbl_widths <- 
    bind_cols(data_frame(file_names),
              data.frame(do.call("rbind", lapply(file_names, read_excel_width)))) %>% 
    rename(num_cols = do.call..rbind...lapply.file_names..read_excel_width..)
Tbl_widths
write.csv(Tbl_widths, "../analysis/Tbl_widths.csv", na = "", row.names = F)

# Table of all the Headers
Tbl_headers <- 
    bind_cols(data_frame(file_names),
        data.frame(do.call("rbind", lapply(file_names, read_excel_headers))))
glimpse(Tbl_headers)
write.csv(Tbl_headers, "../analysis/Tbl_headers.csv", na = "", row.names = F)

# Table of all the Types
Tbl_types <- 
    bind_cols(data_frame(file_names), 
    data.frame(do.call("rbind", lapply(file_names, read_excel_coltypes))))
Tbl_types
write.csv(Tbl_types, "../analysis/Tbl_types.csv", na = "", row.names = F)

## all columns to text, last column to numeric
sapply(file_, Tbl_convertAllText2Numeric)
warnings()

# csv1500 <-
#     read.csv("1500_.csv", stringsAsFactors = F, na.strings = c("", " ", "NA"))
# glimpse(csv1500)
# 
