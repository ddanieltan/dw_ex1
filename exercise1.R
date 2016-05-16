library(dplyr)
library(tidyr)
library(xlsx)
refine_original <- read.xlsx("refine_original.xlsx",1)
tbl_df(refine_original)

###
#1: Clean up brand names
refine_clean <- refine_original

#convert all values to lowercase
refine_clean$company <- tolower(refine_clean$company)

#using a look up table to replace mis-spellings
lut <- c("phillips" = "philips", "philips" = "philips", "phllips" = "philips", "fillips" = "philips", "phlips"="philips", "phillps"="philips",
         "akzo" = "akzo", "ak zo" = "akzo", "akz0" = "akzo", "van houten"="van houten", "unilever"="unilever","unilver"="unilever")
refine_clean$company <- lut[refine_clean$company]

#easy way to check all company values are unique
arrange(refine_clean,company)

###
#2: Separate product code and number
refine_sep <- refine_clean
refine_sep<-separate(refine_sep,Product.code...number.,sep="-",into=c("product_code","product_number"))

###
#3: Add product categories
refine_prod <-refine_sep

#create a data frame to map product_code and product_category, then join it with the main refine df
category_df <- data.frame(c("p","v","x","q"),c("Smartphone","TV","Laptop","Tablet"))
colnames(category_df) <- c("product_code","product_category")
refine_prod<-full_join(refine_prod,category_df,by="product_code")

###
#4: Add full address for geocoding
refine_geo <- refine_prod
refine_geo <- unite(refine_geo, full_address, address, city, country, sep=",")

###
#5: Create dummy variables for company and product category
refine_dummy <- refine_geo

refine_dummy <- refine_dummy %>%
  mutate(company_philips =ifelse(company=="philips",1,0)) %>%
  mutate(company_akzo =ifelse(company=="akzo",1,0)) %>%
  mutate(company_van_houten =ifelse(company=="van houten",1,0)) %>%
  mutate(company_unilever =ifelse(company=="unilever",1,0))
