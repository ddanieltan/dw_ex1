library(dplyr)
library(tidyr)
library(xlsx)
library(stringi)
refine_original <- read.xlsx("refine_original.xlsx",1)
tbl_df(refine_original)

###
#1: Clean up brand names
refine_clean <- refine_original

#convert all values to lowercase
refine_clean$company <- tolower(refine_clean$company)

#search for strings beginning with p|f, a, v, u and replacing them with philips, akzo, van houten, unilever respectively
refine_clean$company <- refine_clean$company %>%
  stri_replace_all_regex("^p.*","philips") %>%
  stri_replace_all_regex("^f.*","philips") %>%
  stri_replace_all_regex("^a.*","akzo") %>%
  stri_replace_all_regex("^v.*","van houten") %>%
  stri_replace_all_regex("^u.*","unilever")

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

###
#Submission
write.csv(refine_dummy,file="refine_clean.csv")