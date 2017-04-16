#############################################################################
#
# AUSTRIA
#
#############################################################################

#############################################################################
# CHUNK 1: Load libraries
#############################################################################

library(stringr)
library(readr)
library(tibble)
library(dplyr)
library(tidyr)
library(docxtractr)

#############################################################################
# CHUNK 2: Read the WHO mortality and population data
#############################################################################

# Set the directory where the data is located.
setwd("../Desktop/Self_Study/Data_Science/Mortality-master/Mortality-master-Isabel")
setwd("OrigData/")

# Select the data files (exclude .zip, .doc, .xls)
csvWHO <- list.files(path = ".", full.names = FALSE) %>%
str_subset("^((?!zip|doc|xls).)*$")

# !! Check csvWHO

# Use the lapply to read all the data into one list
ptm <- proc.time() # time it (1/2)
dataList <- lapply(csvWHO, read_csv, guess_max = min(300000, Inf))
proc.time() - ptm # time it (2/2)
rm(ptm)

# !! Check summary(dataList)

# Extract the data into data.frames (tbl).
# Convert them to lower case as they are not consistent in the original.
names(dataList) <- str_to_lower(csvWHO)
list2env(dataList, envir = .GlobalEnv)
rm(csvWHO)
rm(dataList)


country <- "Austria"
countryCode <- country_codes$country[country_codes$name == country]
austria_icd7 <- morticd7 %>% filter(Country == countryCode)
austria_icd8 <- morticd8 %>% filter(Country == countryCode)
austria_icd9 <- morticd9 %>% filter(Country == countryCode)
austria_icd10_1 <- morticd10_part1 %>% filter(Country == countryCode)
austria_icd10_2 <- morticd10_part2 %>% filter(Country == countryCode)

# For all countries
# morticd7 -- 1950 to 1972
# morticd8 -- 1968 to 2008
# morticd9 -- 1979 to 2013
# morticd10_part1 -- 1988 to 2004
# morticd10_part2 -- 2005 to 2015

# For austria (not necessarily for other countries)
# morticd7 -- 1955 to 1968
# morticd8 -- 1969 to 1979
# morticd9 -- 1980 to 2001
# morticd10_part1 -- 2002 to 2004
# morticd10_part2 -- 2005 to 2014



# Bind all datasets together (each dataset has a diff. time range)
morticd <- rbind(morticd7, morticd8, morticd9, morticd10_part1, morticd10_part2) # sum up the 2 tables. Later need to include morticd7/8/9 too.
rm(morticd7)
rm(morticd8)
rm(morticd9)
rm(morticd10_part1)
rm(morticd10_part2)

# Some garbage collection to free up the memory. Maybe not needed in the future.
gc()
setwd("..") # Getting out of the OrigData folder.


#############################################################################
# CHUNK 3: reading codes for diseases (new format: 2002 onwards)
#############################################################################

# File: icd10cm_codes_2017.txt  --- disease codes with the new format icd10
# used from 2002 onwards. File downloaded from:
# ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD10CM/2017/

# This is a tab-separated file, even though the tab is not recognized. 
# In fact, it is not tab but a sequence of white spaces. Therefore, 
# we need to separate the codes from the names (start at the 8th character) 
# and then eliminate white spaces in the code strings.

disease_codes <- read_tsv('icd10cm_codes_2017.txt', col_names = "textline") %>%
 separate(textline, into = c("Code", "Cause"), sep = 8) %>%
 mutate(Code = gsub(" ", "", Code, fixed = TRUE))

#Get neoplasm, melanoma, lymphoma and leukaemia codes
nmllCodes_new <- disease_codes %>%
        filter(
        grepl("neoplasm", str_to_lower(Cause)) |
        grepl("melanoma", str_to_lower(Cause)) |
        grepl("lymphoma", str_to_lower(Cause)) |
        grepl("leukaemia", str_to_lower(Cause))
        )

#############################################################################
# CHUNK 4: reading codes for diseases (old format: 1955 to 2001)
#############################################################################

# OrigData/Documentation_15sep2016.docx  --- disease codes using the old 
# format valid from 1955 to 2001. 

# The original file was a .doc file. I had to save it as docx in order to use the function read_docx.
doc <- read_docx("OrigData/Documentation_15sep2016.docx")

# Check summary(doc)
# Check docx_tbl_count(doc)
# Check docx_describe_tbls(doc) 
# Content of doc -- 18 tables

# In many cases, it seems like R cannot recognize the table headers

# We need tables 16, 17 and 18 -- header=TRUE tells R the first row is the header
docTable16 <- docx_extract_tbl(doc, tbl_number = 16, header = TRUE) # Table 16 = Codes for causes of deaths (1/2)
docTable17 <- docx_extract_tbl(doc, tbl_number = 17, header = TRUE) # Table 17 = Codes for causes of deaths (2/2)
rm(doc)

# Attention: docTable16 and docTable17 both have codes for diseases, but one calls it "code" and the other "Code".
# This is why we need to rename the code column in docTable16.
nmllCodes_old <- docTable16 %>%
        rename(Code = code) %>%
        bind_rows(docTable17) %>%
        filter(
        grepl("neoplasm", str_to_lower(Cause)) |
        grepl("melanoma", str_to_lower(Cause)) |
        grepl("lymphoma", str_to_lower(Cause)) |
        grepl("leukaemia", str_to_lower(Cause))
        )
rm(docTable16)
rm(docTable17)


#############################################################################
# CHUNK 5: merging old and new format disease codes
#############################################################################

# Check the format of nmll_codes_Old and nmll_codes_new and merge.
nmllCodes <- nmllCodes_old %>%
		select(Code,Cause) %>%
		bind_rows(nmllCodes_new)


#############################################################################
# CHUNK 6: filtering mortality data for Austria
#############################################################################

country <- "Austria"
countryCode <- country_codes$country[country_codes$name == country]
austriaData <- morticd %>% filter(Country == countryCode)

# 57.954 out of 4.820.616 rows are from Austria


#############################################################################
# CHUNK 7: transforming the pop table from wide to long -- popLong
#############################################################################

# Transform the population data
# 1st: eliminate the Admin1, SubDiv, Lb and Age columns
# 2nd: transform from wide to long: age groups are not variables, but rather different values of the variable age
# 3rd: the age groups are identified as Pop1, Pop2, ... Need to create new column with simply 1, 2, ... and this way it will agree with the data in the table with deaths.
# 4th: eliminate the old Age column.

popLong <- pop   %>%
        select(-Admin1, -SubDiv, -Lb) %>%
        gather("Age", "Population", Pop1:Pop26) %>%
        mutate(GroupAge  = as.numeric(str_replace(Age,"Pop", ""))) %>%
        select(-Age)
rm(pop) # No need for the old pop table anymore


#############################################################################
# CHUNK 8: transforming the mortality data from wide to long -- nmllDataLong
#############################################################################

# Transform the mortality data (just like we did for the population data)
austriaDataLong <- austriaData %>%
        select(-Admin1,
        -SubDiv,
        -IM_Frmat,
        -IM_Deaths1,
        -IM_Deaths2,
        -IM_Deaths3,
        -IM_Deaths4) %>%
        gather("Age", "Deaths", Deaths1:Deaths26) %>%
        mutate(Deaths = as.numeric(Deaths), GroupAge  = as.numeric(str_replace(Age, "Deaths", "")))  %>%
        select(-Age)

#############################################################################
# CHUNK 9: mortalityPop = merging population and deaths datasets
#############################################################################

# Left join means everything from mortality will be kept and it will add 
# the additional columns of population that find a match in Country, Year, 
# Sec, Frmat, GroupAge.

# combining the mortality data with the population data (command left_join)
austriaMortPop <- austriaDataLong %>%
        	left_join(popLong, 
		by = c("Country"="Country", 
			"Year"="Year", 
			"Sex"="Sex", 
			"Frmat"="Frmat", 
			"GroupAge" = "GroupAge" )) 


#############################################################
# Exploratory analysis
#############################################################

data1975 <- austriaMortPop %>% filter(between(Year,1975-2,1975+2)) # 73.996 records
data1985 <- austriaMortPop %>% filter(between(Year,1985-2,1985+2)) # 95.784 records
data1995 <- austriaMortPop %>% filter(between(Year,1995-2,1995+2)) # 96.200 records

codes1975 <- unique(data1975$Cause) # 315 codes
codes1985 <- unique(data1985$Cause) # 391 codes
codes1995 <- unique(data1995$Cause) # 392 codes

# The codes changed a lot from 1975 to 1985.
# The codes are the same in 1985 and 1995.
mutual_1975_1985 <- codes1975[codes1975 %in% codes1985] # 47
mutual_1975_1995 <- codes1975[codes1975 %in% codes1995] # 47
mutual_1985_1995 <- codes1985[codes1985 %in% codes1995] # 391
