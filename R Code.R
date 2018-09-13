
# Fetching datasets

companies <- read.delim("companies.txt", stringsAsFactors = FALSE)
rounds2 <- read.csv("rounds2.csv", stringsAsFactors = FALSE)


# To avoid error due to case sensitive nature of R

rounds2$company_permalink <- tolower(rounds2$company_permalink)  
companies$permalink <- tolower(companies$permalink)


# Call packages tidyr and dplyr
library(tidyr)
library(dplyr)


# No. of unique companies in rounds2

j <- distinct(rounds2, company_permalink)
count(j)


# No. of unique companies in companies

k <- distinct(companies, permalink)
count(k)


master_frame <- merge(rounds2, companies, by.x = "company_permalink", by.y = "permalink")

# How many observations are present in master_frame?
nrow(master_frame)

# Are there any companies in the rounds2 file which are not present in companies?

#     No. The inner merge shows that no. of rows in master_frame are equal to that in rounds2.
#     So, there are no rows dropped.
#     This implies that there is no company in rounds2 file which is not present in companies file.



# Summarising along funding type to find average funding of each type

type_wise_gp <- group_by(master_frame, funding_round_type)
type_wise_avg <- summarise(type_wise_gp, avg_funding = mean(raised_amount_usd, na.rm = TRUE))

# I am ommitting observations where raised_amount_usd is not available by na.rm = T
# But we are deliberately keeping observations where raised_amount_usd = 0,
# as removing them will distort the actual average value.

type_wise_avg


# The most suitable investment type for Spark.
# (Average funding per investment round between 5 million to 15 million USD)

filter(type_wise_avg, avg_funding >= 5000000 & avg_funding <= 15000000)

# We find that venture type funding is the only one most suitable for Spark Funds
# having average funding per round between 5 million and 15 million


# Filter data for venture type funding only 
venture_data <- filter(master_frame, funding_round_type == "venture") 

# Summarising by country code to find total funding of each country code

country_wise_gp <- group_by(venture_data, country_code)
country_wise_total <- summarise(country_wise_gp, total_funding = sum(raised_amount_usd, na.rm = TRUE))

# Arrange in descending order of total funding and select top 9 rows
arrange(country_wise_total, desc(total_funding))

# 3rd highest is collective amount of companies whose country codes are not present in companies file


# From the given list of countries where English is an official language,
# by general convention for country codes, we have -

# Top English speaking country = United States (USA)
# 2nd English speaking country = United Kingdom (GBR)
# 3rd English speaking country = India (IND)


# 1. Extract the primary sector of each category list from the category_list column

library(stringr)

# Splitting category_list column on basis of 1st occurence of "|" into 2 columns
# and assigning 1st column to "primary_sector" column in master_frame 

master_frame$primary_sector <- str_split_fixed(master_frame$category_list,"[|]", 2)[,1]


# 2. Mapping each primary sector to one of the eight main sectors

# Fetch mapping file. Column names contain special charecters.
# To keep the main sector names in same form, check.names = FALSE

mapping <- read.csv("mapping.csv", stringsAsFactors = F, check.names = FALSE)

sort(mapping$category_list)

# We see that category_list contains distorted names of many categories
# e.g.
#       "Natural Language Processing" spelled as   "0tural Language Processing"
#       "Nanotechnology"              spelled as   "0notechnology"
#       "Natural Resources"           spelled as   "0tural Resources"

# So here is a common anomaly where somehow pattern "na" is changed to "0" in the strings containing "na"

mapping$category_list <- str_replace_all(mapping$category_list, "0", "na")

sort(mapping$category_list)


# But now, "0" in catergory name "Enterprise 2.0"  is also replaced by "na" which was not meant to be replaced
mapping$category_list <- str_replace_all(mapping$category_list, "2.na", "2.0")


mapping$category_list[which(str_detect(mapping$category_list, "0"))]
# Anomaly removed.


# Convert mapping from wide to long format
long_mapping <- gather(mapping, main_sector, my_val, c(2:10))

# Cleaning mapping data
new_mapping <- long_mapping[-(which(long_mapping$my_val == 0 | 
                                    long_mapping$main_sector == "Blanks")),]

new_mapping <- new_mapping[, -3]


# Merge master_frame with new_mapping; clean & long form of mapping

# To avoid error due to case sensitive nature of R

new_mapping$category_list <- toupper(new_mapping$category_list)
master_frame$primary_sector <- toupper(master_frame$primary_sector)


# Mapping by outer merge as inner merge is causing loss of data
mapped_master <- merge(master_frame, new_mapping, by.x = "primary_sector", by.y = "category_list", all = TRUE)

# I have deliberately kept outer merge because
# inner merge will cause loss of data of rounds whose main sector is not available
# which may lead to erroneous result (like % of total, total count, total sum etc.)


# Now we know -
#           Most suitable funding type for Spark Funds - venture
#           Top 3 English speaking countries - USA, GBR and IND
#           Range of funding preferred by Spark Funds - Between 5 million and 15 million


usa_df <- filter(mapped_master,
                   country_code == "USA" 
                 & funding_round_type == "venture" 
                 & raised_amount_usd >= 5000000 
                 & raised_amount_usd <= 15000000)

usa_gp <- group_by(usa_df, main_sector)
usa_summary <- summarise(usa_gp, total_of_investments = sum(raised_amount_usd), no_of_investments = n())


gbr_df <- filter(mapped_master, 
                 country_code == "GBR" 
                 & funding_round_type == "venture" 
                 & raised_amount_usd >= 5000000 
                 & raised_amount_usd <= 15000000)

gbr_gp <- group_by(gbr_df, main_sector)
gbr_summary <- summarise(gbr_gp, total_of_investments = sum(raised_amount_usd), no_of_investments = n())


ind_df <- filter(mapped_master, 
                 country_code == "IND" 
                 & funding_round_type == "venture" 
                 & raised_amount_usd >= 5000000 
                 & raised_amount_usd <= 15000000)

ind_gp <- group_by(ind_df, main_sector)
ind_summary <- summarise(ind_gp, total_of_investments = sum(raised_amount_usd), no_of_investments = n())


# Data Frame D1 for Top country USA
D1 <- merge(usa_df, usa_summary, by = "main_sector")

# Data Frame D2 for 2nd country GBR
D2 <- merge(gbr_df, gbr_summary, by = "main_sector")

# Data Frame D3 for 3rd country IND
D3 <- merge(ind_df, ind_summary, by = "main_sector")



# 1. Total number of Investments (count) for each country 

# Since not specified, we are including where main sector is NA

sum(distinct(D1, no_of_investments))
sum(distinct(D2, no_of_investments))
sum(distinct(D3, no_of_investments))


# 2. Total amount of investment (USD) for each country

# Since not specified, we are including where main sector is NA

sum(distinct(D1, total_of_investments))
sum(distinct(D2, total_of_investments))
sum(distinct(D3, total_of_investments))


# 3 to 8. Top, second and 3rd sectors (no. of investments-wise)
#         and respective Number of investments, for each country

arrange(usa_summary, desc(no_of_investments))[1:3,]
arrange(gbr_summary, desc(no_of_investments))[1:3,]
arrange(ind_summary, desc(no_of_investments))[1:3,]


# 9. For point 3 (top sector count-wise), company  which received the highest investment, in each country

# For USA
usa_others <- filter(D1, main_sector == "Others")
usa_others_gp <- group_by(usa_others, company_permalink, name)
usa_others_companies <- summarise(usa_others_gp, total_investment = sum(raised_amount_usd))

arrange(usa_others_companies, desc(total_investment))[1,]

# For GBR
gbr_others <- filter(D2, main_sector == "Others")
gbr_others_gp <- group_by(gbr_others, company_permalink, name)
gbr_others_companies <- summarise(gbr_others_gp, total_investment = sum(raised_amount_usd))

arrange(gbr_others_companies, desc(total_investment))[1,]

# For IND
ind_others <- filter(D3, main_sector == "Others")
ind_others_gp <- group_by(ind_others, company_permalink, name)
ind_others_companies <- summarise(ind_others_gp, total_investment = sum(raised_amount_usd))

arrange(ind_others_companies, desc(total_investment))[1,]


# 10. For point 4 (second best sector count-wise), company which received the highest investment, in each country

# For USA
usa_2nd_best <- filter(D1, main_sector == "Social, Finance, Analytics, Advertising")
usa_2nd_best_gp <- group_by(usa_2nd_best, company_permalink, name)
usa_2nd_best_companies <- summarise(usa_2nd_best_gp, total_investment = sum(raised_amount_usd))

arrange(usa_2nd_best_companies, desc(total_investment))[1,]

# For GBR
gbr_2nd_best <- filter(D2, main_sector == "Social, Finance, Analytics, Advertising")
gbr_2nd_best_gp <- group_by(gbr_2nd_best, company_permalink, name)
gbr_2nd_best_companies <- summarise(gbr_2nd_best_gp, total_investment = sum(raised_amount_usd))

arrange(gbr_2nd_best_companies, desc(total_investment))[1,]

# For IND
ind_2nd_best <- filter(D3, main_sector == "Social, Finance, Analytics, Advertising")
ind_2nd_best_gp <- group_by(ind_2nd_best, company_permalink, name)
ind_2nd_best_companies <- summarise(ind_2nd_best_gp, total_investment = sum(raised_amount_usd))

arrange(ind_2nd_best_companies, desc(total_investment))[1,]

### --- ### --- ###### --- ### --- ###### --- ### --- ###### --- ### --- ###



###### --------- ############## --------- ############## --------- ##########