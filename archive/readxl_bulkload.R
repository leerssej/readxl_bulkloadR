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

Tbl_convertAmtNumeric <- function (file_)
{
    read_excel(paste0(file_,".xlsx")) %>%
        mutate(AmountNumeric = as.numeric(.[[ncol(.)]])) %>% 
        mutate(AmountRound = ceiling(.[[ncol(.)]])) %>%
        select(1:10, ncol(.)) %>% 
        write.csv(paste0(file_,".csv"), na = "", row.names = F)
}

Tbl_convertAllTextLastCol2Numeric <- function (file_)
{
    read_charxl_full(paste0(file_,".xlsx")) %>%
        mutate(AmountNumeric = as.numeric(.[[ncol(.)]])) %>% 
        mutate(AmountRound = ceiling(.[[ncol(.)]])) %>%
        select(1:10, ncol(.)) %>% 
        write.csv(paste0(file_,".csv"), na = "", row.names = F)
}

Tbl_convertAllText2Numeric <- function (file_)
{
    read_charxl_full(paste0(file_,".xlsx")) %>%
        mutate(AmountNumeric = suppressWarnings(as.numeric(.[[ncol(.)]]))) %>% # last col to num
        mutate(AmountRound = ceiling(.[[ncol(.)]])) %>% #round last col
        select(1:10, ncol(.)) %>% # select 10 + Corrected Amount
        filter(!is.na(AmountRound)) %>% 
        mutate_all(funs(as.numeric)) %>% 
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
# 
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

# Table of the Data aligned fullly
# Completely bound dataframe
Tbl_complete <- 
    file_names %>% 
    map_df(~read_charxl_full(.))
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
    mutate(int = suppressWarnings(as.numeric(X11)) %% 1) %>% 
    filter(int > 0)
glimpse(Tbl_CompleteIntegerTest)

# # #singleton for function development
# Tbl0881 <-
#     read_charxl_full("1380_.xlsx") %>% 
#     mutate(AmountNumeric = suppressWarnings(as.numeric(.[[ncol(.)]]))) %>% 
#     mutate(AmountRound = ceiling(.[[ncol(.)]])) %>%
#     select(1:10, ncol(.)) %>% 
#     filter(!is.na(AmountRound)) %>% 
#     mutate_all(funs(as.numeric))
# glimpse(Tbl0881)

# 
# Throwdown all the .csv
## straight to .csv
# sapply(file_, Tbl_convert)
## last column to numeric, then to ceiling
# sapply(file_, Tbl_convertAmtNumeric)
## all columns to text, last column to numeric
# sapply(file_, Tbl_convertAllTextNumeric)
## all columns to text, last column to numeric
sapply(file_, Tbl_convertAllText2Numeric)
warnings()
