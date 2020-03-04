# Set the directory
# setwd("/Users/zhaoyikai/Comparative_pol_measurement_project/After_dissertation/wave6/")

# To generate a nonnegative file for further processsing
# ------------------------------------------------------------------

dat6 <- readRDS("/Users/zhaoyikai/Desktop/Demo_MI_project/wv6.rds")
polity = readxl::read_xls("/Users/zhaoyikai/Comparative_pol_measurement_project/polity_2017.xls")
country_code_6 = readxl::read_xlsx("/Users/zhaoyikai/Comparative_pol_measurement_project/country_code_6.xlsx", col_names = TRUE)

#colnames(country_code_6) = c("code", "country")

# load polity scores and get those country names
polity_6up_2012 = polity %>%
  filter(year == 2012 & democ >=6) %>%
  dplyr::select(country, year, democ)

country_name_polity = polity_6up_2012$country 


# Manually append the countries that were messed up in the codebook
append_df = data.frame('Code' = c(752, 276, 170, 392, 410, 792, 
                                  528, 634, 705, 724, 780 ),
                       'Label' = c('Sweden', 'Germany', 'Columbia', 'Japan', 'South Korea', 'Turkey',
                                   'Netherlands', 'Qatar', 'Slovenia', 'Spain', 'Trinidad and Tobago'))

# Establish the country dict and get intersected countries
country_code_6_complete = rbind(country_code_6, append_df) # combine the two df s together
colnames(country_code_6_complete) = c("code", "country")
country_dict_6 <- setNames(country_code_6_complete$country, country_code_6_complete$code)
country_w_wv6_polity6 = intersect(country_name_polity, country_dict_6) # of length 33

# find the dict that is mutual to country_name_polity and country_dict_5
# This is the dict we would map the dat$V2A column
country_dict_full = country_dict_6[country_dict_6 %in% country_w_wv6_polity6]
dat6$country = country_dict_full[as.character(dat6$V2A)] # now there are many NAs in dat$country that r not >6

# Export the new non_negative dataset
demo_vars = c("country", "V131", "V132", "V133", "V134", "V135",
  "V136", "V139")

nonnegative = dat6 %>% 
  dplyr::select(demo_vars) %>%
  filter(country %in% country_w_wv6_polity6) %>%
  filter_all(all_vars(. > 0)) # Now the non-negative dataset has 32 countries

nonnegative[,2:length(nonnegative)] <- lapply(nonnegative[, 2:length(nonnegative)], factor)
colnames(nonnegative) = c("country", "tax", "religion", "free_election", "state_aid",
                         "Army", "civil_rights", "women")

# write out csv file
write.csv(nonnegative, "nonzero_ds_wv6.csv", row.names = FALSE)

# Demostrate All cases are negative in NZ, Guatemala, Colombia, Italy
# NZ = dat %>%
#   dplyr::select(demo_vars) %>%
#   filter(country == "New Zealand")
# nrow(NZ) # 954 ROWS
# sapply(NZ, function(a){sum(a<0)}) 
# ----------------------------------------------------

