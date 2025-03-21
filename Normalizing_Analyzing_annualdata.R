library(readr)
library(dplyr)

## Function for 0,1 normalizing, function 2 = ignore nulls
normal01 <- function(x){(x-min(x))/(max(x)-min(x))}
normal01_narm <- function(x){
  (x-min(x, na.rm=TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))
  }


## Load in each file as pdx_annual_2013_19 and pdx_annual_2020_23
pdx_annual_2013_19 <- read_csv("C:/Users/justi/Documents/WQ_GreenGent_Proj/Normalizing_Analyzing/RawData_fromGIS/pdx_cbg_lc_gicount_gilength_race_ed_hisp_inc_2013_2019.csv")
pdx_annual_2020_23 <- read_csv("C:/Users/justi/Documents/WQ_GreenGent_Proj/Normalizing_Analyzing/RawData_fromGIS/pdx_cbg_lc_gicount_gilength_race_ed_hisp_inc_2020_2023.csv")

## Turning NAs in GI Count to 0
pdx_annual_2013_19$GICount_2013 <- ifelse(is.na(pdx_annual_2013_19$GICount_2013) , 0, pdx_annual_2013_19$GICount_2013)
pdx_annual_2013_19$GICount_2014 <- ifelse(is.na(pdx_annual_2013_19$GICount_2014) , 0, pdx_annual_2013_19$GICount_2014)
pdx_annual_2013_19$GICount_2015 <- ifelse(is.na(pdx_annual_2013_19$GICount_2015) , 0, pdx_annual_2013_19$GICount_2015)
pdx_annual_2013_19$GICount_2016 <- ifelse(is.na(pdx_annual_2013_19$GICount_2016) , 0, pdx_annual_2013_19$GICount_2016)
pdx_annual_2013_19$GICount_2017 <- ifelse(is.na(pdx_annual_2013_19$GICount_2017) , 0, pdx_annual_2013_19$GICount_2017)
pdx_annual_2013_19$GICount_2018 <- ifelse(is.na(pdx_annual_2013_19$GICount_2018) , 0, pdx_annual_2013_19$GICount_2018)
pdx_annual_2013_19$GICount_2019 <- ifelse(is.na(pdx_annual_2013_19$GICount_2019) , 0, pdx_annual_2013_19$GICount_2019)
pdx_annual_2020_23$GICount_2020 <- ifelse(is.na(pdx_annual_2020_23$GICount_2020) , 0, pdx_annual_2020_23$GICount_2020)
pdx_annual_2020_23$GICount_2021 <- ifelse(is.na(pdx_annual_2020_23$GICount_2021) , 0, pdx_annual_2020_23$GICount_2021)
pdx_annual_2020_23$GICount_2022 <- ifelse(is.na(pdx_annual_2020_23$GICount_2022) , 0, pdx_annual_2020_23$GICount_2022)
pdx_annual_2020_23$GICount_2023 <- ifelse(is.na(pdx_annual_2020_23$GICount_2023) , 0, pdx_annual_2020_23$GICount_2023)




## Calculating GI Count per Sq Km ##
pdx_annual_2013_19$GICount_2013_sqkm <- pdx_annual_2013_19$GICount_2013/pdx_annual_2013_19$Area_SqKm
pdx_annual_2013_19$GICount_2014_sqkm <- pdx_annual_2013_19$GICount_2014/pdx_annual_2013_19$Area_SqKm
pdx_annual_2013_19$GICount_2015_sqkm <- pdx_annual_2013_19$GICount_2015/pdx_annual_2013_19$Area_SqKm
pdx_annual_2013_19$GICount_2016_sqkm <- pdx_annual_2013_19$GICount_2016/pdx_annual_2013_19$Area_SqKm
pdx_annual_2013_19$GICount_2017_sqkm <- pdx_annual_2013_19$GICount_2017/pdx_annual_2013_19$Area_SqKm
pdx_annual_2013_19$GICount_2018_sqkm <- pdx_annual_2013_19$GICount_2018/pdx_annual_2013_19$Area_SqKm
pdx_annual_2013_19$GICount_2019_sqkm <- pdx_annual_2013_19$GICount_2019/pdx_annual_2013_19$Area_SqKm
pdx_annual_2020_23$GICount_2020_sqkm <- pdx_annual_2020_23$GICount_2020/pdx_annual_2020_23$Area_SqKm
pdx_annual_2020_23$GICount_2021_sqkm <- pdx_annual_2020_23$GICount_2021/pdx_annual_2020_23$Area_SqKm
pdx_annual_2020_23$GICount_2022_sqkm <- pdx_annual_2020_23$GICount_2022/pdx_annual_2020_23$Area_SqKm
pdx_annual_2020_23$GICount_2023_sqkm <- pdx_annual_2020_23$GICount_2023/pdx_annual_2020_23$Area_SqKm

## Calculating GI Length per Sq Km ##
pdx_annual_2013_19$GILength_m_2013_sqkm <- pdx_annual_2013_19$GILength_m_2013/pdx_annual_2013_19$Area_SqKm
pdx_annual_2013_19$GILength_m_2014_sqkm <- pdx_annual_2013_19$GILength_m_2014/pdx_annual_2013_19$Area_SqKm
pdx_annual_2013_19$GILength_m_2015_sqkm <- pdx_annual_2013_19$GILength_m_2015/pdx_annual_2013_19$Area_SqKm
pdx_annual_2013_19$GILength_m_2016_sqkm <- pdx_annual_2013_19$GILength_m_2016/pdx_annual_2013_19$Area_SqKm
pdx_annual_2013_19$GILength_m_2017_sqkm <- pdx_annual_2013_19$GILength_m_2017/pdx_annual_2013_19$Area_SqKm
pdx_annual_2013_19$GILength_m_2018_sqkm <- pdx_annual_2013_19$GILength_m_2018/pdx_annual_2013_19$Area_SqKm
pdx_annual_2013_19$GILength_m_2019_sqkm <- pdx_annual_2013_19$GILength_m_2019/pdx_annual_2013_19$Area_SqKm
pdx_annual_2020_23$GILength_m_2020_sqkm <- pdx_annual_2020_23$GILength_m_2020/pdx_annual_2020_23$Area_SqKm
pdx_annual_2020_23$GILength_m_2021_sqkm <- pdx_annual_2020_23$GILength_m_2021/pdx_annual_2020_23$Area_SqKm
pdx_annual_2020_23$GILength_m_2022_sqkm <- pdx_annual_2020_23$GILength_m_2022/pdx_annual_2020_23$Area_SqKm
pdx_annual_2020_23$GILength_m_2023_sqkm <- pdx_annual_2020_23$GILength_m_2023/pdx_annual_2020_23$Area_SqKm

## Calculating White Proportions ##
pdx_annual_2013_19$prop_white_2013 <- pdx_annual_2013_19$Estimate_Total_Whitealone_2013/pdx_annual_2013_19$Estimate_Total_2013
pdx_annual_2013_19$prop_white_2014 <- pdx_annual_2013_19$Estimate_Total_Whitealone_2014/pdx_annual_2013_19$Estimate_Total_2014
pdx_annual_2013_19$prop_white_2015 <- pdx_annual_2013_19$Estimate_Total_Whitealone_2015/pdx_annual_2013_19$Estimate_Total_2015
pdx_annual_2013_19$prop_white_2016 <- pdx_annual_2013_19$Estimate_Total_Whitealone_2016/pdx_annual_2013_19$Estimate_Total_2016
pdx_annual_2013_19$prop_white_2017 <- pdx_annual_2013_19$Estimate_Total_Whitealone_2017/pdx_annual_2013_19$Estimate_Total_2017
pdx_annual_2013_19$prop_white_2018 <- pdx_annual_2013_19$Estimate_Total_Whitealone_2018/pdx_annual_2013_19$Estimate_Total_2018
pdx_annual_2013_19$prop_white_2019 <- pdx_annual_2013_19$Estimate_Total_Whitealone_2019/pdx_annual_2013_19$Estimate_Total_2019
pdx_annual_2020_23$prop_white_2020 <- pdx_annual_2020_23$Estimate_Total_Whitealone_2020/pdx_annual_2020_23$Estimate_Total_2020
pdx_annual_2020_23$prop_white_2021 <- pdx_annual_2020_23$Estimate_Total_Whitealone_2021/pdx_annual_2020_23$Estimate_Total_2021
pdx_annual_2020_23$prop_white_2022 <- pdx_annual_2020_23$Estimate_Total_Whitealone_2022/pdx_annual_2020_23$Estimate_Total_2022
pdx_annual_2020_23$prop_white_2023 <- pdx_annual_2020_23$Estimate_Total_Whitealone_2023/pdx_annual_2020_23$Estimate_Total_2023

## Calculating Black Proportions ##
pdx_annual_2013_19$prop_black_2013 <- pdx_annual_2013_19$Estimate_Total_BlackorAfricanAmericanalone_2013/pdx_annual_2013_19$Estimate_Total_2013
pdx_annual_2013_19$prop_black_2014 <- pdx_annual_2013_19$Estimate_Total_BlackorAfricanAmericanalone_2014/pdx_annual_2013_19$Estimate_Total_2014
pdx_annual_2013_19$prop_black_2015 <- pdx_annual_2013_19$Estimate_Total_BlackorAfricanAmericanalone_2015/pdx_annual_2013_19$Estimate_Total_2015
pdx_annual_2013_19$prop_black_2016 <- pdx_annual_2013_19$Estimate_Total_BlackorAfricanAmericanalone_2016/pdx_annual_2013_19$Estimate_Total_2016
pdx_annual_2013_19$prop_black_2017 <- pdx_annual_2013_19$Estimate_Total_BlackorAfricanAmericanalone_2017/pdx_annual_2013_19$Estimate_Total_2017
pdx_annual_2013_19$prop_black_2018 <- pdx_annual_2013_19$Estimate_Total_BlackorAfricanAmericanalone_2018/pdx_annual_2013_19$Estimate_Total_2018
pdx_annual_2013_19$prop_black_2019 <- pdx_annual_2013_19$Estimate_Total_BlackorAfricanAmericanalone_2019/pdx_annual_2013_19$Estimate_Total_2019
pdx_annual_2020_23$prop_black_2020 <- pdx_annual_2020_23$Estimate_Total_BlackorAfricanAmericanalone_2020/pdx_annual_2020_23$Estimate_Total_2020
pdx_annual_2020_23$prop_black_2021 <- pdx_annual_2020_23$Estimate_Total_BlackorAfricanAmericanalone_2021/pdx_annual_2020_23$Estimate_Total_2021
pdx_annual_2020_23$prop_black_2022 <- pdx_annual_2020_23$Estimate_Total_BlackorAfricanAmericanalone_2022/pdx_annual_2020_23$Estimate_Total_2022
pdx_annual_2020_23$prop_black_2023 <- pdx_annual_2020_23$Estimate_Total_BlackorAfricanAmericanalone_2023/pdx_annual_2020_23$Estimate_Total_2023

## Calculating Asian Proportions ##
pdx_annual_2013_19$prop_asian_2013 <- pdx_annual_2013_19$Estimate_Total_Asianalone_2013/pdx_annual_2013_19$Estimate_Total_2013
pdx_annual_2013_19$prop_asian_2014 <- pdx_annual_2013_19$Estimate_Total_Asianalone_2014/pdx_annual_2013_19$Estimate_Total_2014
pdx_annual_2013_19$prop_asian_2015 <- pdx_annual_2013_19$Estimate_Total_Asianalone_2015/pdx_annual_2013_19$Estimate_Total_2015
pdx_annual_2013_19$prop_asian_2016 <- pdx_annual_2013_19$Estimate_Total_Asianalone_2016/pdx_annual_2013_19$Estimate_Total_2016
pdx_annual_2013_19$prop_asian_2017 <- pdx_annual_2013_19$Estimate_Total_Asianalone_2017/pdx_annual_2013_19$Estimate_Total_2017
pdx_annual_2013_19$prop_asian_2018 <- pdx_annual_2013_19$Estimate_Total_Asianalone_2018/pdx_annual_2013_19$Estimate_Total_2018
pdx_annual_2013_19$prop_asian_2019 <- pdx_annual_2013_19$Estimate_Total_Asianalone_2019/pdx_annual_2013_19$Estimate_Total_2019
pdx_annual_2020_23$prop_asian_2020 <- pdx_annual_2020_23$Estimate_Total_Asianalone_2020/pdx_annual_2020_23$Estimate_Total_2020
pdx_annual_2020_23$prop_asian_2021 <- pdx_annual_2020_23$Estimate_Total_Asianalone_2021/pdx_annual_2020_23$Estimate_Total_2021
pdx_annual_2020_23$prop_asian_2022 <- pdx_annual_2020_23$Estimate_Total_Asianalone_2022/pdx_annual_2020_23$Estimate_Total_2022
pdx_annual_2020_23$prop_asian_2023 <- pdx_annual_2020_23$Estimate_Total_Asianalone_2023/pdx_annual_2020_23$Estimate_Total_2023

## Calculating Hispanic Proportion ##
pdx_annual_2013_19$prop_hisp_2013 <- pdx_annual_2013_19$Estimate_Total_Hisp_2013/pdx_annual_2013_19$Estimate_Total_2013
pdx_annual_2013_19$prop_hisp_2014 <- pdx_annual_2013_19$Estimate_Total_Hisp_2014/pdx_annual_2013_19$Estimate_Total_2014
pdx_annual_2013_19$prop_hisp_2015 <- pdx_annual_2013_19$Estimate_Total_Hisp_2015/pdx_annual_2013_19$Estimate_Total_2015
pdx_annual_2013_19$prop_hisp_2016 <- pdx_annual_2013_19$Estimate_Total_Hisp_2016/pdx_annual_2013_19$Estimate_Total_2016
pdx_annual_2013_19$prop_hisp_2017 <- pdx_annual_2013_19$Estimate_Total_Hisp_2017/pdx_annual_2013_19$Estimate_Total_2017
pdx_annual_2013_19$prop_hisp_2018 <- pdx_annual_2013_19$Estimate_Total_Hisp_2018/pdx_annual_2013_19$Estimate_Total_2018
pdx_annual_2013_19$prop_hisp_2019 <- pdx_annual_2013_19$Estimate_Total_Hisp_2019/pdx_annual_2013_19$Estimate_Total_2019
pdx_annual_2020_23$prop_hisp_2020 <- pdx_annual_2020_23$Estimate_Total_Hisp_2020/pdx_annual_2020_23$Estimate_Total_2020
pdx_annual_2020_23$prop_hisp_2021 <- pdx_annual_2020_23$Estimate_Total_Hisp_2021/pdx_annual_2020_23$Estimate_Total_2021
pdx_annual_2020_23$prop_hisp_2022 <- pdx_annual_2020_23$Estimate_Total_Hisp_2022/pdx_annual_2020_23$Estimate_Total_2022
pdx_annual_2020_23$prop_hisp_2023 <- pdx_annual_2020_23$Estimate_Total_Hisp_2023/pdx_annual_2020_23$Estimate_Total_2023

## Calculating Proportion with a Bachelor's Degree ##
pdx_annual_2013_19$prop_bach_2013 <- pdx_annual_2013_19$Estimate_Bachelor_2013/pdx_annual_2013_19$Estimate_TotalAdult_2013
pdx_annual_2013_19$prop_bach_2014 <- pdx_annual_2013_19$Estimate_Bachelor_2014/pdx_annual_2013_19$Estimate_TotalAdult_2014
pdx_annual_2013_19$prop_bach_2015 <- pdx_annual_2013_19$Estimate_Bachelor_2015/pdx_annual_2013_19$Estimate_TotalAdult_2015
pdx_annual_2013_19$prop_bach_2016 <- pdx_annual_2013_19$Estimate_Bachelor_2016/pdx_annual_2013_19$Estimate_TotalAdult_2016
pdx_annual_2013_19$prop_bach_2017 <- pdx_annual_2013_19$Estimate_Bachelor_2017/pdx_annual_2013_19$Estimate_TotalAdult_2017
pdx_annual_2013_19$prop_bach_2018 <- pdx_annual_2013_19$Estimate_Bachelor_2018/pdx_annual_2013_19$Estimate_TotalAdult_2018
pdx_annual_2013_19$prop_bach_2019 <- pdx_annual_2013_19$Estimate_Bachelor_2019/pdx_annual_2013_19$Estimate_TotalAdult_2019
pdx_annual_2020_23$prop_bach_2020 <- pdx_annual_2020_23$Estimate_Bachelor_2020/pdx_annual_2020_23$Estimate_TotalAdult_2020
pdx_annual_2020_23$prop_bach_2021 <- pdx_annual_2020_23$Estimate_Bachelor_2021/pdx_annual_2020_23$Estimate_TotalAdult_2021
pdx_annual_2020_23$prop_bach_2022 <- pdx_annual_2020_23$Estimate_Bachelor_2022/pdx_annual_2020_23$Estimate_TotalAdult_2022
pdx_annual_2020_23$prop_bach_2023 <- pdx_annual_2020_23$Estimate_Bachelor_2023/pdx_annual_2020_23$Estimate_TotalAdult_2023


## Converting 250,000+ in Median Income file to 250000 so as.numeric can convert it
pdx_annual_2013_19$Estimate_MedInc_2013 <- ifelse(pdx_annual_2013_19$Estimate_MedInc_2013 == '250,000+', '250000', pdx_annual_2013_19$Estimate_MedInc_2013)
pdx_annual_2013_19$Estimate_MedInc_2014 <- ifelse(pdx_annual_2013_19$Estimate_MedInc_2014 == '250,000+', '250000', pdx_annual_2013_19$Estimate_MedInc_2014)
pdx_annual_2013_19$Estimate_MedInc_2015 <- ifelse(pdx_annual_2013_19$Estimate_MedInc_2015 == '250,000+', '250000', pdx_annual_2013_19$Estimate_MedInc_2015)
pdx_annual_2013_19$Estimate_MedInc_2016 <- ifelse(pdx_annual_2013_19$Estimate_MedInc_2016 == '250,000+', '250000', pdx_annual_2013_19$Estimate_MedInc_2016)
pdx_annual_2013_19$Estimate_MedInc_2017 <- ifelse(pdx_annual_2013_19$Estimate_MedInc_2017 == '250,000+', '250000', pdx_annual_2013_19$Estimate_MedInc_2017)
pdx_annual_2013_19$Estimate_MedInc_2018 <- ifelse(pdx_annual_2013_19$Estimate_MedInc_2018 == '250,000+', '250000', pdx_annual_2013_19$Estimate_MedInc_2018)
pdx_annual_2013_19$Estimate_MedInc_2019 <- ifelse(pdx_annual_2013_19$Estimate_MedInc_2019 == '250,000+', '250000', pdx_annual_2013_19$Estimate_MedInc_2019)
pdx_annual_2020_23$Estimate_MedInc_2020 <- ifelse(pdx_annual_2020_23$Estimate_MedInc_2020 == '250,000+', '250000', pdx_annual_2020_23$Estimate_MedInc_2020)
pdx_annual_2020_23$Estimate_MedInc_2021 <- ifelse(pdx_annual_2020_23$Estimate_MedInc_2021 == '250,000+', '250000', pdx_annual_2020_23$Estimate_MedInc_2021)
pdx_annual_2020_23$Estimate_MedInc_2022 <- ifelse(pdx_annual_2020_23$Estimate_MedInc_2022 == '250,000+', '250000', pdx_annual_2020_23$Estimate_MedInc_2022)
pdx_annual_2020_23$Estimate_MedInc_2023 <- ifelse(pdx_annual_2020_23$Estimate_MedInc_2023 == '250,000+', '250000', pdx_annual_2020_23$Estimate_MedInc_2023)

## Converting Median Income field to Numeric, some have "-" instead of null, others have 250,000+ this converts those to null
pdx_annual_2013_19$estimate_medinc_num_2013 <- as.numeric(pdx_annual_2013_19$Estimate_MedInc_2013)
pdx_annual_2013_19$estimate_medinc_num_2014 <- as.numeric(pdx_annual_2013_19$Estimate_MedInc_2014)
pdx_annual_2013_19$estimate_medinc_num_2015 <- as.numeric(pdx_annual_2013_19$Estimate_MedInc_2015)
pdx_annual_2013_19$estimate_medinc_num_2016 <- as.numeric(pdx_annual_2013_19$Estimate_MedInc_2016)
pdx_annual_2013_19$estimate_medinc_num_2017 <- as.numeric(pdx_annual_2013_19$Estimate_MedInc_2017)
pdx_annual_2013_19$estimate_medinc_num_2018 <- as.numeric(pdx_annual_2013_19$Estimate_MedInc_2018)
pdx_annual_2013_19$estimate_medinc_num_2019 <- as.numeric(pdx_annual_2013_19$Estimate_MedInc_2019)
pdx_annual_2020_23$estimate_medinc_num_2020 <- as.numeric(pdx_annual_2020_23$Estimate_MedInc_2020)
pdx_annual_2020_23$estimate_medinc_num_2021 <- as.numeric(pdx_annual_2020_23$Estimate_MedInc_2021)
pdx_annual_2020_23$estimate_medinc_num_2022 <- as.numeric(pdx_annual_2020_23$Estimate_MedInc_2022)
pdx_annual_2020_23$estimate_medinc_num_2023 <- as.numeric(pdx_annual_2020_23$Estimate_MedInc_2023)


## Filling in NAs in median income values, only need to start with 2015 ##
## Lap 1, averaging
pdx_annual_2013_19$estimate_medinc_num_2015 <- ifelse(is.na(pdx_annual_2013_19$estimate_medinc_num_2015), ((pdx_annual_2013_19$estimate_medinc_num_2014+pdx_annual_2013_19$estimate_medinc_num_2016)/2), pdx_annual_2013_19$estimate_medinc_num_2015)
pdx_annual_2013_19$estimate_medinc_num_2016 <- ifelse(is.na(pdx_annual_2013_19$estimate_medinc_num_2016), ((pdx_annual_2013_19$estimate_medinc_num_2015+pdx_annual_2013_19$estimate_medinc_num_2017)/2), pdx_annual_2013_19$estimate_medinc_num_2016)
pdx_annual_2013_19$estimate_medinc_num_2017 <- ifelse(is.na(pdx_annual_2013_19$estimate_medinc_num_2017), ((pdx_annual_2013_19$estimate_medinc_num_2016+pdx_annual_2013_19$estimate_medinc_num_2018)/2), pdx_annual_2013_19$estimate_medinc_num_2017)
pdx_annual_2013_19$estimate_medinc_num_2018 <- ifelse(is.na(pdx_annual_2013_19$estimate_medinc_num_2018), ((pdx_annual_2013_19$estimate_medinc_num_2017+pdx_annual_2013_19$estimate_medinc_num_2019)/2), pdx_annual_2013_19$estimate_medinc_num_2018)
pdx_annual_2013_19$estimate_medinc_num_2019 <- ifelse(is.na(pdx_annual_2013_19$estimate_medinc_num_2019), pdx_annual_2013_19$estimate_medinc_num_2018, pdx_annual_2013_19$estimate_medinc_num_2019)
pdx_annual_2020_23$estimate_medinc_num_2020 <- ifelse(is.na(pdx_annual_2020_23$estimate_medinc_num_2020), pdx_annual_2020_23$estimate_medinc_num_2021, pdx_annual_2020_23$estimate_medinc_num_2020)
pdx_annual_2020_23$estimate_medinc_num_2021 <- ifelse(is.na(pdx_annual_2020_23$estimate_medinc_num_2021), ((pdx_annual_2020_23$estimate_medinc_num_2020+pdx_annual_2020_23$estimate_medinc_num_2022)/2), pdx_annual_2020_23$estimate_medinc_num_2021)
pdx_annual_2020_23$estimate_medinc_num_2022 <- ifelse(is.na(pdx_annual_2020_23$estimate_medinc_num_2022), ((pdx_annual_2020_23$estimate_medinc_num_2021+pdx_annual_2020_23$estimate_medinc_num_2023)/2), pdx_annual_2020_23$estimate_medinc_num_2022)
pdx_annual_2020_23$estimate_medinc_num_2023 <- ifelse(is.na(pdx_annual_2020_23$estimate_medinc_num_2023), pdx_annual_2020_23$estimate_medinc_num_2022, pdx_annual_2020_23$estimate_medinc_num_2023)

## Lap 2, one sided fill ins
pdx_annual_2013_19$estimate_medinc_num_2015 <- ifelse(is.na(pdx_annual_2013_19$estimate_medinc_num_2015),pdx_annual_2013_19$estimate_medinc_num_2014, pdx_annual_2013_19$estimate_medinc_num_2015)
pdx_annual_2013_19$estimate_medinc_num_2016 <- ifelse(is.na(pdx_annual_2013_19$estimate_medinc_num_2016),pdx_annual_2013_19$estimate_medinc_num_2017, pdx_annual_2013_19$estimate_medinc_num_2016)
pdx_annual_2013_19$estimate_medinc_num_2016 <- ifelse(is.na(pdx_annual_2013_19$estimate_medinc_num_2016),pdx_annual_2013_19$estimate_medinc_num_2015, pdx_annual_2013_19$estimate_medinc_num_2016)
pdx_annual_2013_19$estimate_medinc_num_2017 <- ifelse(is.na(pdx_annual_2013_19$estimate_medinc_num_2017),pdx_annual_2013_19$estimate_medinc_num_2018, pdx_annual_2013_19$estimate_medinc_num_2017)
pdx_annual_2013_19$estimate_medinc_num_2017 <- ifelse(is.na(pdx_annual_2013_19$estimate_medinc_num_2017),pdx_annual_2013_19$estimate_medinc_num_2016, pdx_annual_2013_19$estimate_medinc_num_2017)
pdx_annual_2013_19$estimate_medinc_num_2018 <- ifelse(is.na(pdx_annual_2013_19$estimate_medinc_num_2018),pdx_annual_2013_19$estimate_medinc_num_2017, pdx_annual_2013_19$estimate_medinc_num_2018)
pdx_annual_2013_19$estimate_medinc_num_2019 <- ifelse(is.na(pdx_annual_2013_19$estimate_medinc_num_2019),pdx_annual_2013_19$estimate_medinc_num_2018, pdx_annual_2013_19$estimate_medinc_num_2019)
pdx_annual_2020_23$estimate_medinc_num_2023 <- ifelse(is.na(pdx_annual_2020_23$estimate_medinc_num_2023),pdx_annual_2020_23$estimate_medinc_num_2022, pdx_annual_2020_23$estimate_medinc_num_2023)
pdx_annual_2020_23$estimate_medinc_num_2022 <- ifelse(is.na(pdx_annual_2020_23$estimate_medinc_num_2022),pdx_annual_2020_23$estimate_medinc_num_2023, pdx_annual_2020_23$estimate_medinc_num_2022)
pdx_annual_2020_23$estimate_medinc_num_2022 <- ifelse(is.na(pdx_annual_2020_23$estimate_medinc_num_2022),pdx_annual_2020_23$estimate_medinc_num_2021, pdx_annual_2020_23$estimate_medinc_num_2022)
pdx_annual_2020_23$estimate_medinc_num_2021 <- ifelse(is.na(pdx_annual_2020_23$estimate_medinc_num_2021),pdx_annual_2020_23$estimate_medinc_num_2020, pdx_annual_2020_23$estimate_medinc_num_2021)
pdx_annual_2020_23$estimate_medinc_num_2021 <- ifelse(is.na(pdx_annual_2020_23$estimate_medinc_num_2021),pdx_annual_2020_23$estimate_medinc_num_2022, pdx_annual_2020_23$estimate_medinc_num_2021)
pdx_annual_2020_23$estimate_medinc_num_2020 <- ifelse(is.na(pdx_annual_2020_23$estimate_medinc_num_2020),pdx_annual_2020_23$estimate_medinc_num_2021, pdx_annual_2020_23$estimate_medinc_num_2020)

######################################################################################
######## Start here for normalizing after NA fill ####################################
######################################################################################
library(readr)
library(dplyr)

## load in new data, need to rerun normalization ######
pdx_annual_2020_23 <- read_csv("C:/Users/justi/Documents/WQ_GreenGent_Proj/Normalizing_Analyzing/Exports_R/NA_Fill/pdx_annualdata_2020_2023_full_NAFill_manualNAFill.csv")
pdx_annual_2013_19 <- read_csv("C:/Users/justi/Documents/WQ_GreenGent_Proj/Normalizing_Analyzing/Exports_R/NA_Fill/pdx_annualdata_2013_2019_full_NAFill.csv")


## Function for 0,1 normalizing, function 2 = ignore nulls
normal01 <- function(x){(x-min(x))/(max(x)-min(x))}
normal01_narm <- function(x){
  (x-min(x, na.rm=TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))
}


## Normalizing Values that will go to correlation plot ##

## Land Cover Normalization Developed ##
pdx_annual_2013_19$prop_dev_2013_nor <- normal01(pdx_annual_2013_19$prop_dev_2013)
pdx_annual_2013_19$prop_dev_2014_nor <- normal01(pdx_annual_2013_19$prop_dev_2014)
pdx_annual_2013_19$prop_dev_2015_nor <- normal01(pdx_annual_2013_19$prop_dev_2015)
pdx_annual_2013_19$prop_dev_2016_nor <- normal01(pdx_annual_2013_19$prop_dev_2016)
pdx_annual_2013_19$prop_dev_2017_nor <- normal01(pdx_annual_2013_19$prop_dev_2017)
pdx_annual_2013_19$prop_dev_2018_nor <- normal01(pdx_annual_2013_19$prop_dev_2018)
pdx_annual_2013_19$prop_dev_2019_nor <- normal01(pdx_annual_2013_19$prop_dev_2019)
pdx_annual_2020_23$prop_dev_2020_nor <- normal01(pdx_annual_2020_23$prop_dev_2020)
pdx_annual_2020_23$prop_dev_2021_nor <- normal01(pdx_annual_2020_23$prop_dev_2021)
pdx_annual_2020_23$prop_dev_2022_nor <- normal01(pdx_annual_2020_23$prop_dev_2022)
pdx_annual_2020_23$prop_dev_2023_nor <- normal01(pdx_annual_2020_23$prop_dev_2023)

## Land Cover Normalization Forest ##
pdx_annual_2013_19$prop_for_2013_nor <- normal01(pdx_annual_2013_19$prop_for_2013)
pdx_annual_2013_19$prop_for_2014_nor <- normal01(pdx_annual_2013_19$prop_for_2014)
pdx_annual_2013_19$prop_for_2015_nor <- normal01(pdx_annual_2013_19$prop_for_2015)
pdx_annual_2013_19$prop_for_2016_nor <- normal01(pdx_annual_2013_19$prop_for_2016)
pdx_annual_2013_19$prop_for_2017_nor <- normal01(pdx_annual_2013_19$prop_for_2017)
pdx_annual_2013_19$prop_for_2018_nor <- normal01(pdx_annual_2013_19$prop_for_2018)
pdx_annual_2013_19$prop_for_2019_nor <- normal01(pdx_annual_2013_19$prop_for_2019)
pdx_annual_2020_23$prop_for_2020_nor <- normal01(pdx_annual_2020_23$prop_for_2020)
pdx_annual_2020_23$prop_for_2021_nor <- normal01(pdx_annual_2020_23$prop_for_2021)
pdx_annual_2020_23$prop_for_2022_nor <- normal01(pdx_annual_2020_23$prop_for_2022)
pdx_annual_2020_23$prop_for_2023_nor <- normal01(pdx_annual_2020_23$prop_for_2023)


## GI Normalization ##
## GI Normalization Count per Sq Km ##
pdx_annual_2013_19$GICount_2013_sqkm_nor <- normal01(pdx_annual_2013_19$GICount_2013_sqkm)
pdx_annual_2013_19$GICount_2014_sqkm_nor <- normal01(pdx_annual_2013_19$GICount_2014_sqkm)
pdx_annual_2013_19$GICount_2015_sqkm_nor <- normal01(pdx_annual_2013_19$GICount_2015_sqkm)
pdx_annual_2013_19$GICount_2016_sqkm_nor <- normal01(pdx_annual_2013_19$GICount_2016_sqkm)
pdx_annual_2013_19$GICount_2017_sqkm_nor <- normal01(pdx_annual_2013_19$GICount_2017_sqkm)
pdx_annual_2013_19$GICount_2018_sqkm_nor <- normal01(pdx_annual_2013_19$GICount_2018_sqkm)
pdx_annual_2013_19$GICount_2019_sqkm_nor <- normal01(pdx_annual_2013_19$GICount_2019_sqkm)
pdx_annual_2020_23$GICount_2020_sqkm_nor <- normal01(pdx_annual_2020_23$GICount_2020_sqkm)
pdx_annual_2020_23$GICount_2021_sqkm_nor <- normal01(pdx_annual_2020_23$GICount_2021_sqkm)
pdx_annual_2020_23$GICount_2022_sqkm_nor <- normal01(pdx_annual_2020_23$GICount_2022_sqkm)
pdx_annual_2020_23$GICount_2023_sqkm_nor <- normal01(pdx_annual_2020_23$GICount_2023_sqkm)

## GI Normalization Length per Sq Km
pdx_annual_2013_19$GILength_m_2013_sqkm_nor <- normal01(pdx_annual_2013_19$GILength_m_2013_sqkm)
pdx_annual_2013_19$GILength_m_2014_sqkm_nor <- normal01(pdx_annual_2013_19$GILength_m_2014_sqkm)
pdx_annual_2013_19$GILength_m_2015_sqkm_nor <- normal01(pdx_annual_2013_19$GILength_m_2015_sqkm)
pdx_annual_2013_19$GILength_m_2016_sqkm_nor <- normal01(pdx_annual_2013_19$GILength_m_2016_sqkm)
pdx_annual_2013_19$GILength_m_2017_sqkm_nor <- normal01(pdx_annual_2013_19$GILength_m_2017_sqkm)
pdx_annual_2013_19$GILength_m_2018_sqkm_nor <- normal01(pdx_annual_2013_19$GILength_m_2018_sqkm)
pdx_annual_2013_19$GILength_m_2019_sqkm_nor <- normal01(pdx_annual_2013_19$GILength_m_2019_sqkm)
pdx_annual_2020_23$GILength_m_2020_sqkm_nor <- normal01(pdx_annual_2020_23$GILength_m_2020_sqkm)
pdx_annual_2020_23$GILength_m_2021_sqkm_nor <- normal01(pdx_annual_2020_23$GILength_m_2021_sqkm)
pdx_annual_2020_23$GILength_m_2022_sqkm_nor <- normal01(pdx_annual_2020_23$GILength_m_2022_sqkm)
pdx_annual_2020_23$GILength_m_2023_sqkm_nor <- normal01(pdx_annual_2020_23$GILength_m_2023_sqkm)


## Normalizing Race Demographics ##

## Normalizing White Proportion ##
pdx_annual_2013_19$prop_white_2013_nor <- normal01(pdx_annual_2013_19$prop_white_2013)
pdx_annual_2013_19$prop_white_2014_nor <- normal01(pdx_annual_2013_19$prop_white_2014)
pdx_annual_2013_19$prop_white_2015_nor <- normal01(pdx_annual_2013_19$prop_white_2015)
pdx_annual_2013_19$prop_white_2016_nor <- normal01(pdx_annual_2013_19$prop_white_2016)
pdx_annual_2013_19$prop_white_2017_nor <- normal01(pdx_annual_2013_19$prop_white_2017)
pdx_annual_2013_19$prop_white_2018_nor <- normal01(pdx_annual_2013_19$prop_white_2018)
pdx_annual_2013_19$prop_white_2019_nor <- normal01(pdx_annual_2013_19$prop_white_2019)
pdx_annual_2020_23$prop_white_2020_nor <- normal01(pdx_annual_2020_23$prop_white_2020)
pdx_annual_2020_23$prop_white_2021_nor <- normal01(pdx_annual_2020_23$prop_white_2021)
pdx_annual_2020_23$prop_white_2022_nor <- normal01(pdx_annual_2020_23$prop_white_2022)
pdx_annual_2020_23$prop_white_2023_nor <- normal01(pdx_annual_2020_23$prop_white_2023)

## Normalizing Black Proportion ## 
pdx_annual_2013_19$prop_black_2013_nor <- normal01(pdx_annual_2013_19$prop_black_2013)
pdx_annual_2013_19$prop_black_2014_nor <- normal01(pdx_annual_2013_19$prop_black_2014)
pdx_annual_2013_19$prop_black_2015_nor <- normal01(pdx_annual_2013_19$prop_black_2015)
pdx_annual_2013_19$prop_black_2016_nor <- normal01(pdx_annual_2013_19$prop_black_2016)
pdx_annual_2013_19$prop_black_2017_nor <- normal01(pdx_annual_2013_19$prop_black_2017)
pdx_annual_2013_19$prop_black_2018_nor <- normal01(pdx_annual_2013_19$prop_black_2018)
pdx_annual_2013_19$prop_black_2019_nor <- normal01(pdx_annual_2013_19$prop_black_2019)
pdx_annual_2020_23$prop_black_2020_nor <- normal01(pdx_annual_2020_23$prop_black_2020)
pdx_annual_2020_23$prop_black_2021_nor <- normal01(pdx_annual_2020_23$prop_black_2021)
pdx_annual_2020_23$prop_black_2022_nor <- normal01(pdx_annual_2020_23$prop_black_2022)
pdx_annual_2020_23$prop_black_2023_nor <- normal01(pdx_annual_2020_23$prop_black_2023)

# Normalizing Asian Prop ##
pdx_annual_2013_19$prop_asian_2013_nor <- normal01(pdx_annual_2013_19$prop_asian_2013)
pdx_annual_2013_19$prop_asian_2014_nor <- normal01(pdx_annual_2013_19$prop_asian_2014)
pdx_annual_2013_19$prop_asian_2015_nor <- normal01(pdx_annual_2013_19$prop_asian_2015)
pdx_annual_2013_19$prop_asian_2016_nor <- normal01(pdx_annual_2013_19$prop_asian_2016)
pdx_annual_2013_19$prop_asian_2017_nor <- normal01(pdx_annual_2013_19$prop_asian_2017)
pdx_annual_2013_19$prop_asian_2018_nor <- normal01(pdx_annual_2013_19$prop_asian_2018)
pdx_annual_2013_19$prop_asian_2019_nor <- normal01(pdx_annual_2013_19$prop_asian_2019)
pdx_annual_2020_23$prop_asian_2020_nor <- normal01(pdx_annual_2020_23$prop_asian_2020)
pdx_annual_2020_23$prop_asian_2021_nor <- normal01(pdx_annual_2020_23$prop_asian_2021)
pdx_annual_2020_23$prop_asian_2022_nor <- normal01(pdx_annual_2020_23$prop_asian_2022)
pdx_annual_2020_23$prop_asian_2023_nor <- normal01(pdx_annual_2020_23$prop_asian_2023)

# Normalizing Hispanic Prop ##
pdx_annual_2013_19$prop_hisp_2013_nor <- normal01(pdx_annual_2013_19$prop_hisp_2013)
pdx_annual_2013_19$prop_hisp_2014_nor <- normal01(pdx_annual_2013_19$prop_hisp_2014)
pdx_annual_2013_19$prop_hisp_2015_nor <- normal01(pdx_annual_2013_19$prop_hisp_2015)
pdx_annual_2013_19$prop_hisp_2016_nor <- normal01(pdx_annual_2013_19$prop_hisp_2016)
pdx_annual_2013_19$prop_hisp_2017_nor <- normal01(pdx_annual_2013_19$prop_hisp_2017)
pdx_annual_2013_19$prop_hisp_2018_nor <- normal01(pdx_annual_2013_19$prop_hisp_2018)
pdx_annual_2013_19$prop_hisp_2019_nor <- normal01(pdx_annual_2013_19$prop_hisp_2019)
pdx_annual_2020_23$prop_hisp_2020_nor <- normal01(pdx_annual_2020_23$prop_hisp_2020)
pdx_annual_2020_23$prop_hisp_2021_nor <- normal01(pdx_annual_2020_23$prop_hisp_2021)
pdx_annual_2020_23$prop_hisp_2022_nor <- normal01(pdx_annual_2020_23$prop_hisp_2022)
pdx_annual_2020_23$prop_hisp_2023_nor <- normal01(pdx_annual_2020_23$prop_hisp_2023)

# Normalizing Bachelors Prop ##
pdx_annual_2013_19$prop_bach_2013_nor <- normal01(pdx_annual_2013_19$prop_bach_2013)
pdx_annual_2013_19$prop_bach_2014_nor <- normal01(pdx_annual_2013_19$prop_bach_2014)
pdx_annual_2013_19$prop_bach_2015_nor <- normal01(pdx_annual_2013_19$prop_bach_2015)
pdx_annual_2013_19$prop_bach_2016_nor <- normal01(pdx_annual_2013_19$prop_bach_2016)
pdx_annual_2013_19$prop_bach_2017_nor <- normal01(pdx_annual_2013_19$prop_bach_2017)
pdx_annual_2013_19$prop_bach_2018_nor <- normal01(pdx_annual_2013_19$prop_bach_2018)
pdx_annual_2013_19$prop_bach_2019_nor <- normal01(pdx_annual_2013_19$prop_bach_2019)
pdx_annual_2020_23$prop_bach_2020_nor <- normal01(pdx_annual_2020_23$prop_bach_2020)
pdx_annual_2020_23$prop_bach_2021_nor <- normal01(pdx_annual_2020_23$prop_bach_2021)
pdx_annual_2020_23$prop_bach_2022_nor <- normal01(pdx_annual_2020_23$prop_bach_2022)
pdx_annual_2020_23$prop_bach_2023_nor <- normal01(pdx_annual_2020_23$prop_bach_2023)

# Normalizing median income ##
pdx_annual_2013_19$estimate_medinc_num_2013_nor <- normal01_narm(pdx_annual_2013_19$estimate_medinc_num_2013)
pdx_annual_2013_19$estimate_medinc_num_2014_nor <- normal01_narm(pdx_annual_2013_19$estimate_medinc_num_2014)
pdx_annual_2013_19$estimate_medinc_num_2015_nor <- normal01_narm(pdx_annual_2013_19$estimate_medinc_num_2015)
pdx_annual_2013_19$estimate_medinc_num_2016_nor <- normal01_narm(pdx_annual_2013_19$estimate_medinc_num_2016)
pdx_annual_2013_19$estimate_medinc_num_2017_nor <- normal01_narm(pdx_annual_2013_19$estimate_medinc_num_2017)
pdx_annual_2013_19$estimate_medinc_num_2018_nor <- normal01_narm(pdx_annual_2013_19$estimate_medinc_num_2018)
pdx_annual_2013_19$estimate_medinc_num_2019_nor <- normal01_narm(pdx_annual_2013_19$estimate_medinc_num_2019)
pdx_annual_2020_23$estimate_medinc_num_2020_nor <- normal01_narm(pdx_annual_2020_23$estimate_medinc_num_2020)
pdx_annual_2020_23$estimate_medinc_num_2021_nor <- normal01_narm(pdx_annual_2020_23$estimate_medinc_num_2021)
pdx_annual_2020_23$estimate_medinc_num_2022_nor <- normal01_narm(pdx_annual_2020_23$estimate_medinc_num_2022)
pdx_annual_2020_23$estimate_medinc_num_2023_nor <- normal01_narm(pdx_annual_2020_23$estimate_medinc_num_2023)



## Subsetting to only vars for Correlation Analysis, review column numbers ##
pdx_annual_2013_19_normalizedcols <- pdx_annual_2013_19[c(1,8,484:553)]
pdx_annual_2020_23_normalizedcols <- pdx_annual_2020_23[c(1,3,283:322)]

## Writing to CSV Normalized Cols only and the full dataset
write.csv(pdx_annual_2013_19, "C:/Users/justi/Documents/WQ_GreenGent_Proj/Normalizing_Analyzing/Exports_R/pdx_annualdata_2013_2019_full_NAFill.csv", row.names=FALSE)
write.csv(pdx_annual_2020_23, "C:/Users/justi/Documents/WQ_GreenGent_Proj/Normalizing_Analyzing/Exports_R/pdx_annualdata_2020_2023_full_NAFill.csv", row.names=FALSE)
write.csv(pdx_annual_2013_19_normalizedcols, "C:/Users/justi/Documents/WQ_GreenGent_Proj/Normalizing_Analyzing/Exports_R/pdx_annualdata_2013_2019_normcols_NAFill.csv", row.names=FALSE)
write.csv(pdx_annual_2020_23_normalizedcols, "C:/Users/justi/Documents/WQ_GreenGent_Proj/Normalizing_Analyzing/Exports_R/pdx_annualdata_2020_2023_normcols_NAFill.csv", row.names=FALSE)


## Option to read in new file here before correlation analysis


## Rough Corr Plot 
cor(pdx_annual_2020_23_normalizedcols[,3:42], method = "spearman")
cormatrix <- cor(pdx_annual_2020_23_normalizedcols[,3:42], method = "spearman")

