## Function for 0,1 normalizing, function 2 = ignore nulls
normal01 <- function(x){(x-min(x))/(max(x)-min(x))}
normal01_narm <- function(x){
  (x-min(x, na.rm=TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))
}

## Applying function to normalize wildfire hazard score ##
pdx_hazard$wh_mean_nor <- normal01(pdx_hazard$wh_mean)

# Applying function to normalize landslide risk score ##
pdx_hazard$LS_score_nor <- normal01(pdx_hazard$LS_Score)

# Applying function to normalize temperature score ##
pdx_hazard$wtdavg_temp_nor <- normal01(pdx_hazard$WtdAvg_temp)

## Creating Combined Hazard Score ##
pdx_hazard$combined_hazard <- pdx_hazard$wh_mean_nor + pdx_hazard$LS_score_nor + pdx_hazard$wtdavg_temp_nor + pdx_hazard$F100Blue_N

## Creating Winter Hazard Score ##
pdx_hazard$winter_hazard <- pdx_hazard$LS_score_nor + pdx_hazard$F100Blue_N

## Creating Summer hazard Score ##
pdx_hazard$summer_hazard <- pdx_hazard$wh_mean_nor + pdx_hazard$wtdavg_temp_nor 

## Normalizing combined hazard score ##
pdx_hazard$combined_hazard_nor <- normal01(pdx_hazard$combined_hazard)

## Normalizing winter hazard
pdx_hazard$winter_hazard_nor <- normal01(pdx_hazard$winter_hazard)

## Normalizing summer hazard
pdx_hazard$summer_hazard_nor <- normal01(pdx_hazard$summer_hazard)


# Output to CSV ##
write.csv(pdx_hazard, "C:/Users/justi/Documents/WQ_GreenGent_Proj/Normalizing_Analyzing/Exports_R/pdx_hazard_full.csv")

