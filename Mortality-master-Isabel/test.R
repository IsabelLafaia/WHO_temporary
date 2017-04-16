#############################################################################
# CHUNK 1: ReadWHOdata
#############################################################################

# Load libraries
library(stringr)
library(readr)
library(tibble)

# Set the directory where you have saved the data.
# This is not a great solution but it works and is easier for the moment, Later it should change.

# Just for myself
setwd("../Desktop/Self_Study/Data_Science/Mortality-master/Mortality-master-Isabel")

setwd("OrigData/")

# Select the data files (exclude .zip, .doc, .xls)
csvWHO <- list.files(path = ".", full.names = FALSE) %>%
str_subset("^((?!zip|doc|xls).)*$")

#> csvWHO
#[1] "LICENSE"              "Mort.csv"             "Morticd10_part1"     
#[4] "Morticd10_part2"      "MortIcd7"             "Morticd8"            
#[7] "Morticd9"             "pop"                  "PopSample.csv"       
#[10] "Read_Word_Doc.Rmd"    "SampleStudy.Rmd"      "Trial_before_Git.Rmd"

ptm <- proc.time() # to time the code
# Use the lapply to read all the data into one list
dataList <- lapply(csvWHO, read_csv, guess_max = min(300000, Inf))
proc.time() - ptm # check processing time

#summary(dataList)
#      Length Class  Mode
# [1,]  1     tbl_df list
# [2,]  7     tbl_df list
# [3,] 39     tbl_df list
# [4,] 39     tbl_df list
# [5,] 39     tbl_df list
# [6,] 39     tbl_df list
# [7,] 39     tbl_df list
# [8,] 33     tbl_df list
# [9,]  7     tbl_df list
#[10,]  1     tbl_df list
#[11,]  1     tbl_df list
#[12,]  1     tbl_df list
# each element of dataList has the content of a different file.

# Extract the data into data.frames (tbl) but start by giving them the dataset you want.
# I convert them to lower case as they are not consistent in the original.
names(dataList) <- str_to_lower(csvWHO)
list2env(dataList, envir = .GlobalEnv)

# We just named the elements of dataList
#
#> summary(dataList)
#                     Length Class  Mode
#license               1     tbl_df list
#mort.csv              7     tbl_df list
#morticd10_part1      39     tbl_df list
#morticd10_part2      39     tbl_df list
#morticd7             39     tbl_df list
#morticd8             39     tbl_df list
#morticd9             39     tbl_df list
#pop                  33     tbl_df list
#popsample.csv         7     tbl_df list
#read_word_doc.rmd     1     tbl_df list
#samplestudy.rmd       1     tbl_df list
#trial_before_git.rmd  1     tbl_df list

# The command:
# list2env(dataList, envir = .GlobalEnv)
# makes each of the elements in dataList a different variable in our working environment.
#
#> ls()
# [1] "csvWHO"               "dataList"             "license"             
# [4] "mort.csv"             "morticd10_part1"      "morticd10_part2"     
# [7] "morticd7"             "morticd8"             "morticd9"            
#[10] "pop"                  "popsample.csv"        "ptm"                 
#[13] "read_word_doc.rmd"    "samplestudy.rmd"      "trial_before_git.rmd"


# dataList is not longer needed so we will remove it.
rm(dataList)
# Removing the ICD 7, 8, 9 data as we will not use them for the moment.
# Also we do not want to have problems with the memory
#rm(morticd7, morticd8, morticd9, notes, country_codes)

morticd <- rbind(morticd10_part1, morticd10_part2) # sum up the 2 tables


# Some garbage collection to free up the memory (although this is something that is
# automatically by kernel). In future we may not need this step.
gc()
setwd("..") # Getting out of the OrigData folder.


#############################################################################
# CHUNK 2: ReadICDdata_alternative (by Isabel L.)
# Check the alternative chunk next, using instead the file icd10cm_codes_2017.txt
#############################################################################

library(docxtractr)

# Read the docx file
# The original file was a .doc file. I had to save it as docx in order to use the function read_docx.
doc <- read_docx("OrigData/Documentation_15sep2016.docx")

# Content of doc -- 18 tables
#
#> summary(doc)
#      Length Class         Mode     
#docx   2     xml_document  list     
#ns    20     xml_namespace character
#tbls  18     xml_nodeset   list     
#cmnts  0     xml_nodeset   list     
#path   1     -none-        character


# Check what's inside
docx_tbl_count(doc) #> [1] 18
docx_describe_tbls(doc) 

# docx_describe_tbls(doc)  --- This tells nbr of cells, nbr of rows, if uniform table, if has header or not.
#
# Example:
# Table 18
#  total cells: 456
#   row count  : 228
#   uniform    : likely!
#   has header : likely! => possibly [country, name]
#
# In many cases, it seems like R cannot recognize the table headers

# Extract only tables 16 and 18 -- header=TRUE tells R the first row is the header
docTable1 <- docx_extract_tbl(doc, tbl_number = 1, header = TRUE) # Table 16 = Codes for causes of deaths
docTable2 <- docx_extract_tbl(doc, tbl_number = 2, header = TRUE) # Table 16 = Codes for causes of deaths
docTable3 <- docx_extract_tbl(doc, tbl_number = 3, header = TRUE) # Table 16 = Codes for causes of deaths
docTable4 <- docx_extract_tbl(doc, tbl_number = 4, header = TRUE) # Table 16 = Codes for causes of deaths
docTable5 <- docx_extract_tbl(doc, tbl_number = 5, header = TRUE) # Table 16 = Codes for causes of deaths
docTable6 <- docx_extract_tbl(doc, tbl_number = 6, header = TRUE) # Table 16 = Codes for causes of deaths
docTable7 <- docx_extract_tbl(doc, tbl_number = 7, header = TRUE) # Table 16 = Codes for causes of deaths
docTable8 <- docx_extract_tbl(doc, tbl_number = 8, header = TRUE) # Table 16 = Codes for causes of deaths
docTable9 <- docx_extract_tbl(doc, tbl_number = 9, header = TRUE) # Table 16 = Codes for causes of deaths
docTable10 <- docx_extract_tbl(doc, tbl_number = 10, header = TRUE) # Table 16 = Codes for causes of deaths
docTable11 <- docx_extract_tbl(doc, tbl_number = 11, header = TRUE) # Table 16 = Codes for causes of deaths
docTable12 <- docx_extract_tbl(doc, tbl_number = 12, header = TRUE) # Table 16 = Codes for causes of deaths
docTable13 <- docx_extract_tbl(doc, tbl_number = 13, header = TRUE) # Table 16 = Codes for causes of deaths
docTable14 <- docx_extract_tbl(doc, tbl_number = 14, header = TRUE) # Table 16 = Codes for causes of deaths
docTable15 <- docx_extract_tbl(doc, tbl_number = 15, header = TRUE) # Table 16 = Codes for causes of deaths
docTable16 <- docx_extract_tbl(doc, tbl_number = 16, header = TRUE) # Table 16 = Codes for causes of deaths
docTable17 <- docx_extract_tbl(doc, tbl_number = 17, header = TRUE) # Table 17 = Continuation of Table16
docTable18 <- docx_extract_tbl(doc, tbl_number = 18, header = TRUE) # Table 18 = Codes for countries

#############################################################################
# CHUNK 2: ALTERNATIVE USING THE FILE icd10cm_codes_2017.txt
#############################################################################

# The tab is not recognized. In fact, it is not tab but a sequence of white 
# spaces. Therefore, we need to separate the codes from the names (start at 
# the 8th character) and then eliminate white spaces in the code strings.
library(tidyverse)
disease_codes <- read_tsv('icd10cm_codes_2017.txt', col_names = "textline") %>%
 separate(textline, into = c("Code", "Cause"), sep = 8) %>%
 mutate(Code = gsub(" ", "", Code, fixed = TRUE))

#############################################################################
# CHUNK 3: nmllData
#############################################################################

library(dplyr)

#Get neoplasm, melanoma, lymphoma and leukaemia codes from the table in Documentation_15sep2016.doc

# Attention: docTable16 and docTable17 both have codes for diseases, but one calls it "code" and the other "Code".
# This is why we need to rename the code column in docTable16.
#nmllCodes <- docTable16 %>%
#        rename(Code = code) %>%
#        bind_rows(docTable17) %>%
#        filter(
#        grepl("neoplasm", str_to_lower(Cause)) |
#        grepl("melanoma", str_to_lower(Cause)) |
#        grepl("lymphoma", str_to_lower(Cause)) |
#        grepl("leukaemia", str_to_lower(Cause))
#        )

nmllCodes <- disease_codes %>%
        filter(
        grepl("neoplasm", str_to_lower(Cause)) |
        grepl("melanoma", str_to_lower(Cause)) |
        grepl("lymphoma", str_to_lower(Cause)) |
        grepl("leukaemia", str_to_lower(Cause))
        )

# Just to check, first 3 rows of nmll
#> head(nmllCodes,3)
#  Code Detailed List Numbers                                              Cause Detailed codes
#1 1026               C00-D48                                          Neoplasms           <NA>
#2 1027               C00-C14 Malignant neoplasm of lip, oral cavity and pharynx           <NA>
#3 1028                   C15                   Malignant neoplasm of oesophagus           <NA>


#Dataset with all the mortality data from neoplasm, melanoma, lymphoma and leukaemia
nmllData <- morticd %>% filter(Cause %in% nmllCodes$Code)

# Before and after the filtering
#> nrow(morticd10_part1)
#[1] 1382615
#> nrow(nmllData)
#[1] 1706

rm(morticd10_part1, morticd10_part2, csvWHO, doc) # I do not remove the docTables because I will need it later


#############################################################################
# CHUNK 4: popLong
#############################################################################

library(tidyr)

# Transform the population data
# 1st: eliminate the Admin1, SubDiv, Lb and Age columns
# 2nd: transform from wide to long: age groups are not variables, but rather different values of the variable age
# 3rd: the age groups are identified as Pop1, Pop2, ... Need to create new column with simply 1, 2, ... and this way it will agree with the data in the table with deaths.
# 4th: eliminate the old Age column.
popLong <- pop   %>%
        select(-Admin1, -SubDiv, -Lb) %>%
        gather("Age", "Population", 5:30) %>%
        mutate(GroupAge  = as.numeric(str_replace(Age,"Pop", ""))) %>%
        select(-Age)

# Compare before and after
#> head(pop)
## A tibble: 6 × 33
#  Country Admin1 SubDiv  Year   Sex Frmat    Pop1   Pop2   Pop3  Pop4  Pop5  Pop6   Pop7   Pop8   Pop9  Pop10  Pop11  Pop12  Pop13  Pop14
#    <int>  <int>  <chr> <int> <int> <chr>   <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#1    1060     NA   <NA>  1980     1    07  137100   3400  15800    NA    NA    NA  39200     NA  22700     NA  12600     NA  12600     NA
#2    1060     NA   <NA>  1980     2    07  159000   4000  18400    NA    NA    NA  45400     NA  26300     NA  14600     NA  14600     NA
#3    1125     NA   <NA>  1955     1    02 5051500 150300 543400    NA    NA    NA 615300 603000 501600 414700 379600 356200 376000 293000
#4    1125     NA   <NA>  1955     2    02 5049400 145200 551000    NA    NA    NA 613400 600700 502200 422000 428100 342800 365100 255100
#5    1125     NA   <NA>  1956     1    02 5353700 158700 576600    NA    NA    NA 653000 639400 531500 439400 402200 377300 398500 310600
#6    1125     NA   <NA>  1956     2    02 5351400 153600 584800    NA    NA    NA 650900 636300 531400 446500 453600 363200 387100 270500
## ... with 13 more variables: Pop15 <dbl>, Pop16 <dbl>, Pop17 <dbl>, Pop18 <dbl>, Pop19 <dbl>, Pop20 <dbl>, Pop21 <dbl>, Pop22 <dbl>,
##   Pop23 <dbl>, Pop24 <dbl>, Pop25 <dbl>, Pop26 <int>, Lb <int>

#> head(popLong)
## A tibble: 6 × 6
#  Country  Year   Sex Frmat Population GroupAge
#    <int> <int> <int> <chr>      <dbl>    <dbl>
#1    1060  1980     1    07     137100        1
#2    1060  1980     2    07     159000        1
#3    1125  1955     1    02    5051500        1
#4    1125  1955     2    02    5049400        1
#5    1125  1956     1    02    5353700        1
#6    1125  1956     2    02    5351400        1

rm(pop) # No need for the old pop table anymore

#############################################################################
# CHUNK 5: mortalityPop
#############################################################################

# There are 4 steps here:
# 1st: transform the mortality data (just like we did for the population data)
# 2nd: combining the mortality data with the population data (command left_join)
# 3rd: add a new column with the rate (number of deaths divided by the population)
# 4th: eliminate the age group nbr 1 (all ages)

# Left join means everything from mortality will be kept and it will add 
# the additional columns of population that find a match in Country, Year, 
# Sec, Frmat, GroupAge.

mortality <- nmllData %>%
        select(-Admin1,
        -SubDiv,
        -IM_Frmat,
        -IM_Deaths1,
        -IM_Deaths2,
        -IM_Deaths3,
        -IM_Deaths4) %>%
        gather("Age", "Deaths", 7:32) %>%
        mutate(Deaths = as.numeric(Deaths), GroupAge  = as.numeric(str_replace(Age, "Deaths", "")))  %>%
        select(-Age)

mortalityPop <- nmllData %>%
        select(-Admin1,
        -SubDiv,
        -IM_Frmat,
        -IM_Deaths1,
        -IM_Deaths2,
        -IM_Deaths3,
        -IM_Deaths4) %>%
        gather("Age", "Deaths", 7:32) %>%
        mutate(Deaths = as.numeric(Deaths), GroupAge  = as.numeric(str_replace(Age, "Deaths", "")))  %>%
        select(-Age) %>%
        left_join(popLong, by = c("Country"="Country", "Year"="Year", "Sex"="Sex", "Frmat"="Frmat", "GroupAge" = "GroupAge" )) %>%
        mutate(Rate = (100000 * Deaths) / Population) %>%
        filter( GroupAge != 1 )


#############################################################################
# CHUNK 6: (NEW CHUNK) trying to match Saghir's report
#############################################################################

# Austria is 4010
#countryCode <- docTable18$country[docTable18$name == 'Austria']
countryCode <- 4010

# Breast cancer is 1036
#causeCode <- docTable16 %>%
#        rename(Code = code) %>%
#        bind_rows(docTable17) %>%
#        filter(grepl("breast", str_to_lower(Cause)))


# Note: there are 72 different types of breast cancer
# No cases of leukaemia
# 18 kidney cancers
# 8 prostate cancers -- good to test

causeCode <- nmllCodes %>%
        filter(grepl("prostate", str_to_lower(Cause)))

# Look for cases of prostate cancer in Austria
testData <- mortalityPop %>%
        filter(Cause %in% causeCode$Code,
	  	   between(Year, 1998, 2002),
		   Country == as.integer(countryCode))


AustriaBreastCancer <- mortalityPop %>%
        filter(Country == as.integer(countryCode)) 

testData <- mortalityPop %>%
        filter(Cause %in% causeCode$Code,
	  	   between(Year, 1998, 2002),
		   Sex == 2,
		   between(GroupAge, 2, 11)),
		   Country == as.integer(countryCode)) 

# Note: there is a problem with the country code!!

# What I want: number of deaths ...
# for breast (Cause = 1036)
# from 1998 to 2002 (identified as 2000) (Year = 1968:1972)
# for females (Sex = 2)
# for 0-29 years (GroupAge = 2:11)
# for Austria (Country = countryCode, see above)

mortalityPop
