# To generate a nonnegative file for further processsing
# ------------------------------------------------------------------

dat <- readRDS("/Users/zhaoyikai/Desktop/Demo_MI_project/wv5.rds")
#dat6 <- readRDS("/Users/zhaoyikai/Desktop/Demo_MI_project/wv6.rds")
polity = readxl::read_xls("polity_2017.xls")
country_code = readxl::read_xlsx("country_code_5.xlsx", col_names = F)
colnames(country_code) = c("code", "country")

# load polity scores and get those country names
polity_6up_2005 = polity %>%
  filter(year == 2005 & democ >=6) %>%
  dplyr::select(country, year, democ)

country_name_polity = polity_6up_2005$country 
print(country_name_polity)


# Manually append the countries that were messed up in the codebook
append_df_5 = data.frame("code" = c(288, 320, 344, 356, 360, 364, 368, 380, 400, 458, 504, 604, 704, 710, 724, 756, 818, 900, 901),
                         'country' = c('Ghana', 'Guatemala', 'Hong Kong', 'India', 'Indonesia', 
                                       'Iran', 'Iraq', 'Italy', 'Jordan', 'Malaysia', 'Morocco',
                                       'Peru', 'Viet Nam', 'South Africa', 'Spain', 'Switzerland', 'Egypt', 'West Germany', 'East Germany' ))

# Establish the country dict and get intersected countries
country_code_5_complete = rbind(country_code, append_df_5) # combine the two df s together
colnames(country_code_5_complete) = c("code", "country")
country_dict_5 <- setNames(country_code_5_complete$country, country_code_5_complete$code)
country_w_wv5_polity6 = intersect(country_name_polity, country_dict_5)

# find the dict that is mutual to country_name_polity and country_dict_5
# This is the dict we would map the dat$V2A column
country_dict_full = country_dict_5[country_dict_5 %in% country_w_wv5_polity6]
dat$country = country_dict_full[as.character(dat$V2A)] # now there are many NAs in dat$country that r not >6

# Export the new non_negative dataset
demo_vars = c("country", "V152", "V153", "V154", "V155", "V156","V157", "V161") 

nonnegtive = dat %>% 
  dplyr::select(demo_vars) %>%
  filter(country %in% country_w_wv5_polity6) %>%
  filter_all(all_vars(. > 0)) # Now the non-negative dataset has 37 countries

nonnegtive[,2:length(nonnegtive)] <- lapply(nonnegtive[, 2:length(nonnegtive)], factor)
colnames(nonnegtive) = c("country", "tax", "religion", "free_election", "state_aid",
                        "Army", "civil_rights", "women")

# write out csv file
write.csv(nonnegtive, "nonzero_ds.csv", row.names = FALSE)

# Demostrate All cases are negative in NZ, Guatemala, Colombia, Italy
# NZ = dat %>%
#   dplyr::select(demo_vars) %>%
#   filter(country == "New Zealand")
# nrow(NZ) # 954 ROWS
# sapply(NZ, function(a){sum(a<0)}) 
# ----------------------------------------------------



