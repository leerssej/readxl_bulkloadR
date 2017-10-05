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
rm(list=ls())

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
dataRowCount_excel <- function(excel_file)
{
    dataRowCount <-
        read_charxl_data(excel_file) %>%
        nrow
    dataRowCount
}

# Use dataOnly frame to generate sum of specified column
<<<<<<< Updated upstream
sumOfColumn_excel <- function(excel_file) 
{
    sum_of_column <- 
        read_charxl_data(excel_file) %>% 
        select(ncol(.)) %>% 
        summarise_all(funs(sum)) %>%
        collect %>% 
        .[[1]]
=======
sumOfColumn_excel <- function(excel_file, col)
{
    sum_of_column <-
         read_charxl_data(excel_file) %>%
         select_(col) %>%
         summarise_all(funs(sum))
>>>>>>> Stashed changes
    sum_of_column
}

# warnings()

Tbl_convertAllText2Numeric <- function (file_)
{
    read_charxl_full(paste0(file_,".xlsx")) %>%
    select(1:11) %>%
    mutate(AmountDouble = suppressWarnings(as.double(.[[ncol(.)]]))) %>% # last col to int
    mutate(AmountRoundup = round(.[[ncol(.)]])) %>% # round last col
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

<<<<<<< Updated upstream
# csv side of universe
## Read in csv file and extract out only the data
read_csv_data <- function(csv_file) 
=======
### csv side of universe

read_csv_data <- function(csv_file, filter_col)
>>>>>>> Stashed changes
{
    csv_data <-
        read.csv(paste0(csv_file, ".csv"),
                 stringsAsFactors = F,
<<<<<<< Updated upstream
                 na.strings = c("", " ", "NA")) %>% 
        mutate_all(funs(as.double_spprssWrns)) %>% 
        filter_(!is.na(ncol(.)))
=======
                 na.strings = c("", " ", "NA")) %>%
        mutate_all(funs(as.double_spprssWrns)) %>%
        filter_(!is.na(filter_col))
>>>>>>> Stashed changes
    csv_data
}

# glimpse(read_csv_data("1530_", "AmountRoundup"))

# Use dataOnly frame to generate count of rows of data in file
<<<<<<< Updated upstream
dataRowCount_csv <- function(csv_file) 
{
    dataRowCount <- 
        read_csv_data(csv_file) %>% 
=======
dataRowCount_csv <- function(csv_file, filter_col)
{
    dataRowCount <-
        read_csv_data(csv_file, filter_col) %>%
>>>>>>> Stashed changes
        nrow
    dataRowCount
}

# Use dataOnly frame to generate sum of specified column
<<<<<<< Updated upstream
sumOfColumn_csv <- function(csv_file) 
{
    sum_of_column <- 
        read_csv_data(csv_file) %>% 
        select(ncol(.)) %>% 
        summarise_all(funs(sum)) %>% 
        collect %>% 
        .[[1]]
=======
sumOfColumn_csv <- function(csv_file, sum_col, filter_col)
{
    sum_of_column <-
        read_csv_data(csv_file, filter_col) %>%
        select_(sum_col) %>%
        summarise_all(funs(sum))
>>>>>>> Stashed changes
    sum_of_column
}

# sumOfColumn_csv("1530_", "AmountRoundup", "AmountRoundup")

#### Edit File Type HERE ####
file_extension <- "xlsx"
## Test local directory without spaces
# file_path <- "C:/Users/Koyot/Documents/GitHub/readxl_bulkloadR/bulkloadR_staging/"
# file_path <- "/Users/jensleerssen/Documents/GitHub"
# setwd(file_path)
getwd()

# names of files to process
file_names <-
    list.files(
        path = file_path,
        pattern = paste0("*.", file_extension))
print("file_names:")
file_names


# generate a list for autoprocessing file tree in gitbash
file_ <-
    sapply(file_names, chopem)
# write my_branch_list to run the git branch building
write.table(file_, "my_branch_list", na = "", row.names = F, sep = " ", col.names = F, quote = F, eol = " ")
print("file_:")
file_
cat("\n")

# Table of all the columns counts
Tbl_widths <-
    bind_cols(data_frame(file_names),
              data.frame(do.call("rbind", lapply(file_names, read_excel_width)))) %>%
    rename(num_cols = do.call..rbind...lapply.file_names..read_excel_width..)
print("Tbl_Widths:")
Tbl_widths
write.csv(Tbl_widths, "../analysis/Tbl_widths.csv", na = "", row.names = F)
cat("\n")


# Table of all the Headers
Tbl_headers <-
    bind_cols(data_frame(file_names),
              data.frame(do.call("rbind", lapply(file_names, read_excel_headers))))
print("Tbl_headers:")
glimpse(Tbl_headers)
write.csv(Tbl_headers, "../analysis/Tbl_headers.csv", na = "", row.names = F)
cat("\n")


# Table of all the Types
Tbl_types <-
    bind_cols(data_frame(file_names),
              data.frame(do.call("rbind", lapply(file_names, read_excel_coltypes))))
print("Tbl_types:")
Tbl_types
write.csv(Tbl_types, "../analysis/Tbl_types.csv", na = "", row.names = F)
cat("\n")


# Excel Specific Summary MetaTables
## data Row Counts
Tbl_dataRowCounts_xlsx <-
    bind_cols(data_frame(file_),
              data.frame(do.call("rbind", lapply(file_names, dataRowCount_excel)))) %>%
    rename(num_dRows = do.call..rbind...lapply.file_names..dataRowCount_excel..)
print("Tbl_dataRowCounts_xlsx")
Tbl_dataRowCounts_xlsx
write.csv(Tbl_dataRowCounts_xlsx, "../analysis/Tbl_dataRowCounts_xlsx.csv", na = "", row.names = F)
cat("\n")


print("Building Excel MetaTables")
## AmountTotals summary Excel
### List of totals which avoids closure issues
# print("1530,12")
# sumOfColumn_excel("1530_.xlsx")
print("list_of_sums_xlsx")
list_of_sums_xlsx <- lapply(file_names, sumOfColumn_excel)
glimpse(list_of_sums_xlsx)
### Bound Table
print("Tbl_AmountSums_xlsx")
Tbl_AmountSums_xlsx <-
    bind_cols(data_frame(file_),
<<<<<<< Updated upstream
              data.frame(do.call("rbind", list_of_sums_xlsx))) %>% 
    rename(Ttl_Amounts = do.call..rbind...list_of_sums_xlsx.)
print("Tbl_AmountSums_xlsx")
=======
              data.frame(do.call("rbind", list_of_sums_xlsx))) %>%
    rename_("Ttl_Amounts" = ~X11)
>>>>>>> Stashed changes
Tbl_AmountSums_xlsx
write.csv(Tbl_AmountSums_xlsx, "../analysis/Tbl_AmountSums_xlsx.csv", na = "", row.names = F)
cat("\n")


print("Constructing the .csv")
# Launch into csv side of the universe
## all columns to text, last column to numeric
## the last column is also ceilinged to the integer penny
sapply(file_, Tbl_convertAllText2Numeric)
warnings()

print("Building csv MetaTables")
# csv Specific Summary MetaTables
## data Row Counts
Tbl_dataRowCounts_csv <-
    bind_cols(data_frame(file_),
<<<<<<< Updated upstream
              data.frame(do.call("rbind", lapply(file_, dataRowCount_csv)))) %>% 
    rename(num_dRows = do.call..rbind...lapply.file_..dataRowCount_csv..)
print("Tbl_dataRowCounts_csv")
=======
              data.frame(do.call("rbind", lapply(file_, dataRowCount_csv, "AmountRoundup")))) %>%
    rename(num_dRows = do.call..rbind...lapply.file_..dataRowCount_csv...AmountRoundup...)
>>>>>>> Stashed changes
Tbl_dataRowCounts_csv
write.csv(Tbl_dataRowCounts_csv, "../analysis/Tbl_dataRowCounts_csv.csv", na = "", row.names = F)
cat("\n")


## AmountTotals summary csv
### List of totals which avoids closure issues
# sumOfColumn_csv("0890_")
print("list_of_sums_xlsx")
list_of_sums_csv <- lapply(file_, sumOfColumn_csv)
glimpse(list_of_sums_csv)

### Bound Table
print("Tbl_AmountSums_csv")
Tbl_AmountSums_csv <-
    bind_cols(data_frame(file_),
<<<<<<< Updated upstream
              data.frame(do.call("rbind", list_of_sums_csv))) %>% 
    rename(Ttl_Amounts = do.call..rbind...list_of_sums_csv.)
print("Tbl_AmountSums_csv")
=======
              data.frame(do.call("rbind", list_of_sums_csv))) %>%
    rename_("Ttl_Amounts" = "AmountRoundup")
>>>>>>> Stashed changes
Tbl_AmountSums_csv
write.csv(Tbl_AmountSums_csv, "../analysis/Tbl_AmountSums_csv.csv", na = "", row.names = F)
cat("\n")

# Integrity cross check
## RowCounts CrossCheck
print("RowCounts Integrity Cross Check:")
print("csv")
Tbl_dataRowCounts_csv
print("xlsx")
Tbl_dataRowCounts_xlsx
identical(Tbl_dataRowCounts_csv, Tbl_dataRowCounts_xlsx)
all.equal(Tbl_dataRowCounts_csv, Tbl_dataRowCounts_xlsx)
cat("\n")

## AmountSums CrossCheck
print("AmountSums Integrity Cross Check:")
print("csv")
Tbl_AmountSums_csv
print("xlsx")
Tbl_AmountSums_xlsx
identical(Tbl_AmountSums_csv, Tbl_AmountSums_xlsx)
all.equal(Tbl_AmountSums_csv, Tbl_AmountSums_xlsx)

