---------------------------------------------------------------------------------------------------------------
  #---------- This code merges in census data for counts of households by size and income, calculates "resonable
  #---------- levels of consumption by household size, and constructs affordability metrics
  #---------------------------------------------------------------------------------------------------------------

rm(list = ls()) #cleaning workspace, rm for remove
#dev.off() #clearing the plots
getwd()


my_packages <- c("rgeos","raster","tidyverse","magrittr","haven","readxl","tools","usethis","RColorBrewer","ggrepel","sf","tictoc","lubridate","tidycensus")
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)


# read in summary table (wide form)
d <- read_csv("Data/OWRS/summary_table_cleaned_wide_formatv2.csv")
View(d)

# Calculate reasonable consumption: Let's say reasonable consumption is 50gpcd 
# e.g. https://awwa.onlinelibrary.wiley.com/doi/full/10.5942/jawwa.2018.110.0002

hh_sizes<-c(1:7) #household sizes in census go from 1 to 7
gpcd<-50 # set gpcd for affordability threshold

d<-crossing(d,hh_sizes)
d%<>%mutate(reasonable_consumption_kgal=(gpcd*hh_sizes*365/12)/1000)
d$tier_0<-as.numeric(d$tier_0)

#clean up water rates a bit, more, need to remove NAs from tier widths and tiers
d%<>%mutate(tier_0 = case_when(is.na(tier_0) ~ 0,
                               TRUE ~ tier_0),
            tier_1 = case_when(is.na(tier_1) ~ 10^5,
                               TRUE ~ tier_1),
            tier_2 = case_when(is.na(tier_2) ~ 10^6,
                               TRUE ~ tier_2),                               
            tier_3 = case_when(is.na(tier_3) ~ 10^7,
                               TRUE ~ tier_3),
            tier_4 = case_when(is.na(tier_4) ~ 10^8,
                               TRUE ~ tier_4),                                           
            tier_5 = case_when(is.na(tier_5) ~ 10^9,
                               TRUE ~ tier_5),
            tier_6 = case_when(is.na(tier_6) ~ 10^10,
                               TRUE ~ tier_6),
            tier_7 = case_when(is.na(tier_7) ~ 10^11,
                               TRUE ~ tier_7),                                           
            tier_8 = case_when(is.na(tier_8) ~ 10^12,
                               TRUE ~ tier_8),
            
            tier_1_price = case_when(is.na(tier_1_price) ~ 0,
                                     TRUE ~ tier_1_price),
            tier_2_price = case_when(is.na(tier_2_price) ~ 0,
                                     TRUE ~ tier_2_price),                               
            tier_3_price = case_when(is.na(tier_3_price) ~ 0,
                                     TRUE ~ tier_3_price),
            tier_4_price = case_when(is.na(tier_4_price) ~ 0,
                                     TRUE ~ tier_4_price),                                           
            tier_5_price = case_when(is.na(tier_5_price) ~ 0,
                                     TRUE ~ tier_5_price),
            tier_6_price = case_when(is.na(tier_6_price) ~ 0,
                                     TRUE ~ tier_6_price),
            tier_7_price = case_when(is.na(tier_7_price) ~ 0,
                                     TRUE ~ tier_7_price),                                           
            tier_8_price = case_when(is.na(tier_8_price) ~ 0,
                                     TRUE ~ tier_8_price)
            
            )
            

#Calculate monthly water bill at each consumption level

#function takes in summary data frame, and a vector of volumes, returns data frame with revenue for blocks and total water bill
volRevCalc<-function(data,vol){
  data%<>%mutate(
 # rev1=tier_1_price*pmax(pmin(vol,tier_0),0),
  rev1=tier_1_price*pmax((pmin(vol-tier_0,tier_1-tier_0)),0),
  rev2=tier_2_price*pmax((pmin(vol-tier_1,tier_2-tier_1)),0),
  rev3=tier_3_price*pmax((pmin(vol-tier_2,tier_3-tier_2)),0),
  rev4=tier_4_price*pmax((pmin(vol-tier_3,tier_4-tier_3)),0),
  rev5=tier_5_price*pmax((pmin(vol-tier_4,tier_5-tier_4)),0),
  rev6=tier_6_price*pmax((pmin(vol-tier_5,tier_6-tier_5)),0),
  rev7=tier_7_price*pmax((pmin(vol-tier_6,tier_7-tier_6)),0),
  rev8=tier_8_price*pmax((pmin(vol-tier_7,tier_8-tier_7)),0)
  )
  
  total_bill=data$service_charge + data$rev1 + data$rev2 + data$rev3 + data$rev4 + data$rev5 + data$rev6 + data$rev7 + data$rev8


return(total_bill)
  
}

d%<>%mutate(total_bill_by_hhsize=volRevCalc(d,d$reasonable_consumption_kgal),
                                            mean_bill=volRevCalc(d,d$usage_ccf*0.748))

d$approximate_median_income[which(d$median_income_category=="$150,000 to $199,999")]<-175000

##### AFFORDABILITY METRIC 1: % Median HH Income
# The most commonly used affordability metric is
# the average water bill as a % of median area HH income (let's call perc_MHI)
d%<>%mutate(perc_MHI=100*12*mean_bill/approximate_median_income)


##### AFFORDABILITY METRIC 2: Hours of Labor at Minimum Wage (HM)
# See https://awwa.onlinelibrary.wiley.com/doi/full/10.5942/jawwa.2018.110.0002

mw = 12 #assume uniform minimum wage in CA at $12/hr. In future versions, would merge in relevant minimum wages
d%<>%mutate(MH=total_bill_by_hhsize/mw)

##### AFFORDABILITY METRIC 3: WARi 
# See
