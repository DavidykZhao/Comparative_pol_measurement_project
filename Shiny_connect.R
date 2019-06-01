install.packages('rsconnect')
library(rsconnect)

rsconnect::setAccountInfo(name='yikai-zhao',
                          token='12389A9B53A08CC115D3FD54D4F123F3',
                          secret='')

rsconnect::deployApp('/Users/zhaoyikai/Comparative_pol_measurement_project/Shiny_app')
