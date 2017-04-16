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
# CHUNK 2: ReadWHOdata
#############################################################################

# Set the directory where the data is located.
# Not a great solution but works and is easier for now.
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

# !! Check summary(dataList)

# Extract the data into data.frames (tbl).
# Convert them to lower case as they are not consistent in the original.
names(dataList) <- str_to_lower(csvWHO)
list2env(dataList, envir = .GlobalEnv)
rm(csvWHO)
rm(dataList)

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
# CHUNK 3: reading codes for diseases (old and new formats)
#############################################################################

## Part 1: icd10cm_codes_2017.txt  --- codes from 2002 onwards
#  This file contains the codes with the new format icd10. 
#  I found this in the internet, not in the website of WHO.

# The tab is not recognized. In fact, it is not tab but a sequence of white 
# spaces. Therefore, we need to separate the codes from the names (start at 
# the 8th character) and then eliminate white spaces in the code strings.

disease_codes <- read_tsv('icd10cm_codes_2017.txt', col_names = "textline") %>%
 separate(textline, into = c("Code", "Cause"), sep = 8) %>%
 mutate(Code = gsub(" ", "", Code, fixed = TRUE))

# CodePrefix is always a letter (valid from 2002 to 2014)
disease_codes_alt <- disease_codes %>%
	separate(Code, into = c("CodePrefix","CodeNumber"), sep = 1) %>%
	separate(CodeNumber, into = c("Code","Subcode"), sep = 2)


#Get neoplasm, melanoma, lymphoma and leukaemia codes
nmllCodes_new <- disease_codes %>%
        filter(
        grepl("neoplasm", str_to_lower(Cause)) |
        grepl("melanoma", str_to_lower(Cause)) |
        grepl("lymphoma", str_to_lower(Cause)) |
        grepl("leukaemia", str_to_lower(Cause))
        )

## Part 2: OrigData/Documentation_15sep2016.docx  --- codes from 1955 to 2001
#  This file contains the old disease codes. To make sure we're not missing
#  any data, it's better to merge new and old codes.

# Read the docx file
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
docTable18 <- docx_extract_tbl(doc, tbl_number = 18, header = TRUE) # Table 18 = Codes for countries
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

# Check the format of nmll_codes_Old and nmll_codes_new and merge.
nmllCodes <- nmllCodes_old %>%
		select(Code,Cause) %>%
		bind_rows(nmllCodes_new)

#############################################################################
# CHUNK 4: filtering mortality data with codes in nmllCodes
#############################################################################

#Dataset with all the mortality data from neoplasm, melanoma, lymphoma and leukaemia
#nmllData <- morticd %>% filter(Cause %in% nmllCodes$Code)

countryCode <- country_codes$country[country_codes$name == "Austria"]
austriaData <- morticd %>% 
		filter(Country == countryCode) %>%
		separate(Cause, into = c("CodePrefix","CodeNumber"), sep = 1) %>%
		separate(CodeNumber, into = c("Code","Subcode"), sep = 2)

# Numeric prefix -- before 2001
#
# Prefix 1, 2: 1955 - 2001
# Prefix 3, 5, 7, 8: 1969 - 1979
# Prefix 4: 1969 - 2001
# Prefix 6: 1971 - 1979

# Letter prefix -- 2002 - 2014
#
# Prefix A: 1955 - 2014
# Prefix B: 1980 - 2014
# Prefix C, D, E, F, G, I, J, K, L, M, N, O, P, Q, R, V, X, W: 2002 - 2014
# Prefix H: 2002 - 2014 except 2011, 2012
# Prefix S, T, U, Z: nothing
 


# 318909 rows filtered with nmllCodes_new
# 4604 rows filtered with nmllCodes_old
# 323513 rows filtered in total

#############################################################################
# CHUNK 4: popLong
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
# CHUNK 5: nmllDataLong
#############################################################################

# Transform the mortality data (just like we did for the population data)
nmllDataLong <- nmllData %>%
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

rm(nmllData)

#############################################################################
# CHUNK 6: mortalityPop = merging population and deaths datasets
#############################################################################

# Left join means everything from mortality will be kept and it will add 
# the additional columns of population that find a match in Country, Year, 
# Sec, Frmat, GroupAge.

# combining the mortality data with the population data (command left_join)
mortalityPop <- nmllDataLong %>%
        	left_join(popLong, 
		by = c("Country"="Country", 
			"Year"="Year", 
			"Sex"="Sex", 
			"Frmat"="Frmat", 
			"GroupAge" = "GroupAge" )) 

#############################################################################
# CHUNK 7: add column with death rates and filter out entries with GroupAge 1
#############################################################################

# add a new column with the rate (number of deaths divided by the population)
# eliminate the age group nbr 1 (all ages)
mortalityPop <- mortalityPop %>%
        	mutate(Rate = (100000 * Deaths) / Population) 

#%>% filter( GroupAge != 1 )


#############################################################################
# CHUNK 8: (NEW CHUNK) trying to match Saghir's report
#############################################################################


# Note: there are 72 different types of breast cancer
# No cases of leukaemia
# 18 kidney cancers
# 8 prostate cancers -- good to test

# Parameters
cause <- "breast"
country <- "Austria"
yearGroup <- 2010

############## THIS SHOULD LATER GO INTO A SEPARATE FUNCTION ##################

# There is a problem with the cause codes which size can go from 3 to 6 
# whereas the codes in mortalityPop can only go from 3 to 4.
# The problem is: how to identify where should we cut the cause code? 
# How to know whether the cause code has 1 or 2 or 3 extra digits?

causeCode <- filter(nmllCodes, grepl(cause, str_to_lower(Cause)))
countryCode <- country_codes$country[country_codes$name == country]

cases1 <- mortalityPop %>%
        filter(Cause %in% causeCode$Code) %>%
		filter(between(Year,yearGroup-2,yearGroup+2)) %>%
		filter(Country == countryCode)

cases1 <- mortalityPop %>%
        filter(Cause == "C50") %>%
		filter(between(Year,yearGroup-2,yearGroup+2)) %>%
		filter(Country == countryCode)

## Age 0-29  -- group 2  to 11
## Age 30-64 -- group 12 to 18
## Age 65-++ -- group 19 to 26
cases1 %>% 
	filter(between(GroupAge, 1, 26)) %>%
	select(Deaths) %>%
	sum(na.rm = TRUE)

#see all cases for austria, 2010, groupage 1
austria2010 <- mortalityPop %>%
		filter(between(Year,yearGroup-2,yearGroup+2)) %>%
		filter(Country == countryCode) %>% 
		filter(GroupAge == 1) %>%
		group_by(Cause, Sex) %>%
		summarise(totalDeaths = sum(Deaths))

breast <- mortalityPop %>%
	filter(between(Year,2010-2,2010+2)) %>%
	filter(between(GroupAge,2,11))
	
b <- breast[startsWith(breast$Cause,"C50"),]
b %>% select(Deaths) %>% sum()	

prostate <- mortalityPop %>%
	filter(between(Year,2010-2,2010+2)) %>%
	filter(between(GroupAge,19,26))
	
p <- prostate[startsWith(prostate$Cause,"C61"),]
p %>% select(Deaths) %>% sum()

	

# Prostate cancer, all ages, 2010
# Austria: report = 8943, code = 5701
# Belgium: report = 9430, code = 7284
# Denmark: report = 7586, code = 5825
# Finland: report = 6331, code = 4165
# France:  report = 61187, code = 44597
# Germany: report = 107977, code = 63540
	


cut_mortality <- mortalityPop %>%
 separate(Cause, into = c("Code", "Remaining"), sep = 3) 

cut_codes <- nmllCodes %>% separate(Code, into = c("Code","Remaining"), sep = 3)
cut_breastCode <- filter(cut_codes, grepl("breast", str_to_lower(Cause)))

aux <-cut_mortality[startsWith(cut_mortality$Code,unique(cut_breastCode$Code)),]

aux1 <- aux %>%
	filter(between(Year,2010-2,2010+2)) %>%
	filter(GroupAge == 1) %>% 
	select(Deaths) %>% 
	sum()

nmllCodes