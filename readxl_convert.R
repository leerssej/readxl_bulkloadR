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

read_excel_width <- function(excel_file)
{
    require("readxl")
    header_length <- 
        length(readxl:::xlsx_col_names(excel_file))
    header_length 
}

# read excel names with readxl hidden function
read_excel_headers <- function(excel_file)
{
    require("readxl")
    header_row <- 
        readxl:::xlsx_col_names(excel_file) %>% 
        .[1:11]
    header_row 
}

# Function to read all the types with readxl hidden function
read_excel_coltypes <- function(excel_file)
{
    require("readxl")
    column_types <-
        readxl:::xlsx_col_types(excel_file, nskip = 1, n = 1) %>% 
        .[1:11]
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

as.double_spprssWrns <- function(variables) 
{
    suppressWarnings(as.double(variables))
}

as.integer_spprssWrns <- function(variables) 
{
    suppressWarnings(as.integer(variables))
}

# Read in just the data
## stripping out anything that can't get converted to doubles
## ## ## filtering on the amount column ## ## ##
read_charxl_data <- function(excel_file) 
{
    dataOnly <- 
        read_charxl_full(excel_file) %>% 
        mutate_all(funs(as.double_spprssWrns)) %>% 
        filter(!is.na(X11))
    dataOnly
}

# Use dataOnly frame to generate count of rows of data in file
dataRowCount <- function(excel_file) 
{
    dataRowCount <- 
        read_charxl_data(excel_file) %>% 
        nrow
    dataRowCount
}

sumOfColumn <- function(excel_file, col) 
{
    sum_of_column <- 
         read_charxl_data(excel_file) %>% 
         select_(col) %>% 
         summarise_all(funs(sum)) 
    sum_of_column
}

# warnings()

Tbl_convertAllText2Numeric <- function (file_)
{
    read_charxl_full(paste0(file_,".xlsx")) %>%
    select(1:11) %>% 
    mutate(AmountDouble = suppressWarnings(as.double(.[[ncol(.)]]))) %>% # last col to int
    mutate(AmountRoundup = ceiling(.[[ncol(.)]])) %>% # round last col
    select(1:10, ncol(.)) %>% # select 10 + Corrected Amount
    filter(suppressWarnings(!is.na(AmountRoundup))) %>% 
    mutate_all(funs(as.integer_spprssWrns(.))) %>% 
    write.csv(paste0(file_,".csv"), na = "", row.names = F)
}

# Tbl_convertAllText2Numeric <- function (file_)
# {
#     read_charxl_full(paste0(file_,".xlsx")) %>%
#     mutate_all(funs(intSpprssWarns)) %>% # convert all cols to ints
#     select(1:11) %>% # just the columns that we need
#     filter(suppressWarnings(!is.na(X11))) %>% # drop the headers if they had been there
#     write.csv(paste0(file_,".csv"), na = "", row.names = F)
# }

chopem <- function(element) {
    substr(element, 1, 5)
}

### csv side of universe

read_csv_data <- function(csv_file) 
{
    csv_data <- 
        read.csv(paste0(csv_file, ".csv"),
                 stringsAsFactors = F,
                 na.strings = c("", " ", "NA")) %>% 
        mutate_all(funs(as.double_spprssWrns)) %>% 
        filter(!is.na(AmountRound))
    csv_data
}
glimpse(read_csv_data("1530_"))

df_csv <- read.csv(paste0(csv_file, ".csv"), stringsAsFactors = F, na.strings = c("", " ", "NA"))
glimpse(df_csv)


#### Edit File Type HERE ####
file_extension <- "xlsx"
## Test local directory without spaces
file_path <- "C:/Users/Koyot/Documents/GitHub/readxl_bulkloadR/bulkloadR_staging/"
setwd(file_path)

# names of files to process
file_names <- 
    list.files(
        path = file_path,
        pattern = paste0("*.", file_extension))
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

Tbl_dataRowCounts_xlsx <-
    bind_cols(data_frame(file_names),
              data.frame(do.call("rbind", lapply(file_names, dataRowCount)))) %>%
    rename(num_cols = do.call..rbind...lapply.file_names..dataRowCount..)
Tbl_dataRowCounts_xlsx
write.csv(Tbl_dataRowCounts_xlsx, "Tbl_dataRowCounts_xlsx.csv", na = "", row.names = F)

list_of_sums_xlsx <- lapply(file_names, sumOfColumn, ~X11)
Tbl_AmountSums_xlsx <-
    bind_cols(data_frame(file_),
              data.frame(do.call("rbind", list_of_sums)))

# Launch into csv side of the universe
Tbl_dataRowCounts_csv <-
    bind_cols(data_frame(file_names),
              data.frame(do.call("rbind", lapply(file_names, dataRowCount)))) %>%
    rename(num_cols = do.call..rbind...lapply.file_names..dataRowCount..)
Tbl_dataRowCounts_xlsx
write.csv(Tbl_dataRowCounts_xlsx, "Tbl_dataRowCounts_xlsx.csv", na = "", row.names = F)

list_of_sums_csv <- lapply(file_names, sumOfColumn, ~X11)
Tbl_AmountSums_csv <-
    bind_cols(data_frame(file_),
              data.frame(do.call("rbind", list_of_sums)))

Tbl_da

%>%
    rename_("Ttl_Amounts" = ~X11)
Tbl_AmountSums
write.csv(Tbl_AmountSums, "Tbl_AmountSums.csv", na = "", row.names = F)

## all columns to text, last column to numeric
sapply(file_, Tbl_convertAllText2Numeric)
warnings()
