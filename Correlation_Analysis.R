library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)

pdx_annual_2020_23 <- read_csv("C:/Users/justi/Documents/WQ_GreenGent_Proj/Normalizing_Analyzing/Exports_R/pdx_annualdata_2020_2023_full_NAFill.csv")
pdx_annual_2013_19 <- read_csv("C:/Users/justi/Documents/WQ_GreenGent_Proj/Normalizing_Analyzing/Exports_R/pdx_annualdata_2013_2019_full_NAFill.csv")

##################################################################################
#### Correlation between race/inc/ed and forest cover ############################
##################################################################################

#### White ###########
white_forest_test_2013 <- data.frame(cor.test(pdx_annual_2013_19$prop_white_2013_nor, pdx_annual_2013_19$prop_for_2013_nor, method = 'spearman', exact=FALSE)[3:4])
white_forest_2013 <- data.frame(white_for_corr = white_forest_test_2013$estimate, white_for_pvalue = white_forest_test_2013$p.value)
white_forest_2013$year <- 2013
white_forest_test_2014 <- data.frame(cor.test(pdx_annual_2013_19$prop_white_2014_nor, pdx_annual_2013_19$prop_for_2014_nor, method = 'spearman', exact=FALSE)[3:4])
white_forest_2014 <- data.frame(white_for_corr = white_forest_test_2014$estimate, white_for_pvalue = white_forest_test_2014$p.value)
white_forest_2014$year <- 2014
white_forest_test_2015 <- data.frame(cor.test(pdx_annual_2013_19$prop_white_2015_nor, pdx_annual_2013_19$prop_for_2015_nor, method = 'spearman', exact=FALSE)[3:4])
white_forest_2015 <- data.frame(white_for_corr = white_forest_test_2015$estimate, white_for_pvalue = white_forest_test_2015$p.value)
white_forest_2015$year <- 2015
white_forest_test_2016 <- data.frame(cor.test(pdx_annual_2013_19$prop_white_2016_nor, pdx_annual_2013_19$prop_for_2016_nor, method = 'spearman', exact=FALSE)[3:4])
white_forest_2016 <- data.frame(white_for_corr = white_forest_test_2016$estimate, white_for_pvalue = white_forest_test_2016$p.value)
white_forest_2016$year <- 2016
white_forest_test_2017 <- data.frame(cor.test(pdx_annual_2013_19$prop_white_2017_nor, pdx_annual_2013_19$prop_for_2017_nor, method = 'spearman', exact=FALSE)[3:4])
white_forest_2017 <- data.frame(white_for_corr = white_forest_test_2017$estimate, white_for_pvalue = white_forest_test_2017$p.value)
white_forest_2017$year <- 2017
white_forest_test_2018 <- data.frame(cor.test(pdx_annual_2013_19$prop_white_2018_nor, pdx_annual_2013_19$prop_for_2018_nor, method = 'spearman', exact=FALSE)[3:4])
white_forest_2018 <- data.frame(white_for_corr = white_forest_test_2018$estimate, white_for_pvalue = white_forest_test_2018$p.value)
white_forest_2018$year <- 2018
white_forest_test_2019 <- data.frame(cor.test(pdx_annual_2013_19$prop_white_2019_nor, pdx_annual_2013_19$prop_for_2019_nor, method = 'spearman', exact=FALSE)[3:4])
white_forest_2019 <- data.frame(white_for_corr = white_forest_test_2019$estimate, white_for_pvalue = white_forest_test_2019$p.value)
white_forest_2019$year <- 2019
white_forest_test_2020 <- data.frame(cor.test(pdx_annual_2020_23$prop_white_2020_nor, pdx_annual_2020_23$prop_for_2020_nor, method = 'spearman', exact=FALSE)[3:4])
white_forest_2020 <- data.frame(white_for_corr = white_forest_test_2020$estimate, white_for_pvalue = white_forest_test_2020$p.value)
white_forest_2020$year <- 2020
white_forest_test_2021 <- data.frame(cor.test(pdx_annual_2020_23$prop_white_2021_nor, pdx_annual_2020_23$prop_for_2021_nor, method = 'spearman', exact=FALSE)[3:4])
white_forest_2021 <- data.frame(white_for_corr = white_forest_test_2021$estimate, white_for_pvalue = white_forest_test_2021$p.value)
white_forest_2021$year <- 2021
white_forest_test_2022 <- data.frame(cor.test(pdx_annual_2020_23$prop_white_2022_nor, pdx_annual_2020_23$prop_for_2022_nor, method = 'spearman', exact=FALSE)[3:4])
white_forest_2022 <- data.frame(white_for_corr = white_forest_test_2022$estimate, white_for_pvalue = white_forest_test_2022$p.value)
white_forest_2022$year <- 2022
white_forest_test_2023 <- data.frame(cor.test(pdx_annual_2020_23$prop_white_2023_nor, pdx_annual_2020_23$prop_for_2023_nor, method = 'spearman', exact=FALSE)[3:4])
white_forest_2023 <- data.frame(white_for_corr = white_forest_test_2023$estimate, white_for_pvalue = white_forest_test_2023$p.value)
white_forest_2023$year <- 2023

white_forest_allyears <- rbind(white_forest_2013, white_forest_2014, white_forest_2015, white_forest_2016, white_forest_2017, white_forest_2018, white_forest_2019, white_forest_2020, white_forest_2021, white_forest_2022, white_forest_2023)
white_forest_allyears <- white_forest_allyears %>% 
  mutate(plot_value = ifelse(white_for_pvalue > 0.05, NA, white_for_corr))
white_forest_lm <- lm(white_forest_allyears$plot_value ~ white_forest_allyears$year)
summary(white_forest_lm)

### Creating white forest plot, trend is significant including geom_smooth ###
white_for_plot <- ggplot(white_forest_allyears, aes(x=year, y=plot_value)) + geom_hline(yintercept = 0, color = "black", linewidth = 1) + 
  geom_point(size = 2.5) + scale_x_continuous("Year", labels = as.character(white_forest_allyears$year), breaks = white_forest_allyears$year) +
  ylab("Correlation") + ylim(-0.5, 0.5) + theme_bw()+theme(panel.grid.minor = element_blank(), axis.ticks.y=element_blank(), plot.title = element_text(hjust = 0.5), text=element_text(size=20), axis.text.x=element_text(angle = 45, hjust = 1)) + geom_smooth(method = "lm",color = "red",linewidth = 0.5)+
  ggtitle("White x Forest") 
white_for_plot


#### black ###########
black_forest_test_2013 <- data.frame(cor.test(pdx_annual_2013_19$prop_black_2013_nor, pdx_annual_2013_19$prop_for_2013_nor, method = 'spearman', exact=FALSE)[3:4])
black_forest_2013 <- data.frame(black_for_corr = black_forest_test_2013$estimate, black_for_pvalue = black_forest_test_2013$p.value)
black_forest_2013$year <- 2013
black_forest_test_2014 <- data.frame(cor.test(pdx_annual_2013_19$prop_black_2014_nor, pdx_annual_2013_19$prop_for_2014_nor, method = 'spearman', exact=FALSE)[3:4])
black_forest_2014 <- data.frame(black_for_corr = black_forest_test_2014$estimate, black_for_pvalue = black_forest_test_2014$p.value)
black_forest_2014$year <- 2014
black_forest_test_2015 <- data.frame(cor.test(pdx_annual_2013_19$prop_black_2015_nor, pdx_annual_2013_19$prop_for_2015_nor, method = 'spearman', exact=FALSE)[3:4])
black_forest_2015 <- data.frame(black_for_corr = black_forest_test_2015$estimate, black_for_pvalue = black_forest_test_2015$p.value)
black_forest_2015$year <- 2015
black_forest_test_2016 <- data.frame(cor.test(pdx_annual_2013_19$prop_black_2016_nor, pdx_annual_2013_19$prop_for_2016_nor, method = 'spearman', exact=FALSE)[3:4])
black_forest_2016 <- data.frame(black_for_corr = black_forest_test_2016$estimate, black_for_pvalue = black_forest_test_2016$p.value)
black_forest_2016$year <- 2016
black_forest_test_2017 <- data.frame(cor.test(pdx_annual_2013_19$prop_black_2017_nor, pdx_annual_2013_19$prop_for_2017_nor, method = 'spearman', exact=FALSE)[3:4])
black_forest_2017 <- data.frame(black_for_corr = black_forest_test_2017$estimate, black_for_pvalue = black_forest_test_2017$p.value)
black_forest_2017$year <- 2017
black_forest_test_2018 <- data.frame(cor.test(pdx_annual_2013_19$prop_black_2018_nor, pdx_annual_2013_19$prop_for_2018_nor, method = 'spearman', exact=FALSE)[3:4])
black_forest_2018 <- data.frame(black_for_corr = black_forest_test_2018$estimate, black_for_pvalue = black_forest_test_2018$p.value)
black_forest_2018$year <- 2018
black_forest_test_2019 <- data.frame(cor.test(pdx_annual_2013_19$prop_black_2019_nor, pdx_annual_2013_19$prop_for_2019_nor, method = 'spearman', exact=FALSE)[3:4])
black_forest_2019 <- data.frame(black_for_corr = black_forest_test_2019$estimate, black_for_pvalue = black_forest_test_2019$p.value)
black_forest_2019$year <- 2019
black_forest_test_2020 <- data.frame(cor.test(pdx_annual_2020_23$prop_black_2020_nor, pdx_annual_2020_23$prop_for_2020_nor, method = 'spearman', exact=FALSE)[3:4])
black_forest_2020 <- data.frame(black_for_corr = black_forest_test_2020$estimate, black_for_pvalue = black_forest_test_2020$p.value)
black_forest_2020$year <- 2020
black_forest_test_2021 <- data.frame(cor.test(pdx_annual_2020_23$prop_black_2021_nor, pdx_annual_2020_23$prop_for_2021_nor, method = 'spearman', exact=FALSE)[3:4])
black_forest_2021 <- data.frame(black_for_corr = black_forest_test_2021$estimate, black_for_pvalue = black_forest_test_2021$p.value)
black_forest_2021$year <- 2021
black_forest_test_2022 <- data.frame(cor.test(pdx_annual_2020_23$prop_black_2022_nor, pdx_annual_2020_23$prop_for_2022_nor, method = 'spearman', exact=FALSE)[3:4])
black_forest_2022 <- data.frame(black_for_corr = black_forest_test_2022$estimate, black_for_pvalue = black_forest_test_2022$p.value)
black_forest_2022$year <- 2022
black_forest_test_2023 <- data.frame(cor.test(pdx_annual_2020_23$prop_black_2023_nor, pdx_annual_2020_23$prop_for_2023_nor, method = 'spearman', exact=FALSE)[3:4])
black_forest_2023 <- data.frame(black_for_corr = black_forest_test_2023$estimate, black_for_pvalue = black_forest_test_2023$p.value)
black_forest_2023$year <- 2023

black_forest_allyears <- rbind(black_forest_2013, black_forest_2014, black_forest_2015, black_forest_2016, black_forest_2017, black_forest_2018, black_forest_2019, black_forest_2020, black_forest_2021, black_forest_2022, black_forest_2023)
black_forest_allyears <- black_forest_allyears %>% 
  mutate(plot_value = ifelse(black_for_pvalue > 0.05, NA, black_for_corr))
black_forest_lm <- lm(black_forest_allyears$plot_value ~ black_forest_allyears$year)
summary(black_forest_lm)

### Creating black forest plot, no significant trend, remove geom_smooth ###
black_for_plot <- ggplot(black_forest_allyears, aes(x=year, y=plot_value)) + geom_hline(yintercept = 0, color = "black", linewidth = 1) + 
  geom_point(size = 2.5) + scale_x_continuous("Year", labels = as.character(black_forest_allyears$year), breaks = black_forest_allyears$year) +
  ylab("Correlation") + ylim(-0.5, 0.5) + theme_bw()+theme(panel.grid.minor = element_blank(), axis.ticks.y=element_blank(),plot.title = element_text(hjust = 0.5), text=element_text(size=20), axis.text.x=element_text(angle = 45, hjust = 1)) + ggtitle("Black x Forest") #+ geom_smooth(method = "lm",color = "red",linewidth = 0.5)
black_for_plot


#### income ###########
income_forest_test_2013 <- data.frame(cor.test(pdx_annual_2013_19$estimate_medinc_num_2013_nor, pdx_annual_2013_19$prop_for_2013_nor, method = 'spearman', exact=FALSE)[3:4])
income_forest_2013 <- data.frame(income_for_corr = income_forest_test_2013$estimate, income_for_pvalue = income_forest_test_2013$p.value)
income_forest_2013$year <- 2013
income_forest_test_2014 <- data.frame(cor.test(pdx_annual_2013_19$estimate_medinc_num_2014_nor, pdx_annual_2013_19$prop_for_2014_nor, method = 'spearman', exact=FALSE)[3:4])
income_forest_2014 <- data.frame(income_for_corr = income_forest_test_2014$estimate, income_for_pvalue = income_forest_test_2014$p.value)
income_forest_2014$year <- 2014
income_forest_test_2015 <- data.frame(cor.test(pdx_annual_2013_19$estimate_medinc_num_2015_nor, pdx_annual_2013_19$prop_for_2015_nor, method = 'spearman', exact=FALSE)[3:4])
income_forest_2015 <- data.frame(income_for_corr = income_forest_test_2015$estimate, income_for_pvalue = income_forest_test_2015$p.value)
income_forest_2015$year <- 2015
income_forest_test_2016 <- data.frame(cor.test(pdx_annual_2013_19$estimate_medinc_num_2016_nor, pdx_annual_2013_19$prop_for_2016_nor, method = 'spearman', exact=FALSE)[3:4])
income_forest_2016 <- data.frame(income_for_corr = income_forest_test_2016$estimate, income_for_pvalue = income_forest_test_2016$p.value)
income_forest_2016$year <- 2016
income_forest_test_2017 <- data.frame(cor.test(pdx_annual_2013_19$estimate_medinc_num_2017_nor, pdx_annual_2013_19$prop_for_2017_nor, method = 'spearman', exact=FALSE)[3:4])
income_forest_2017 <- data.frame(income_for_corr = income_forest_test_2017$estimate, income_for_pvalue = income_forest_test_2017$p.value)
income_forest_2017$year <- 2017
income_forest_test_2018 <- data.frame(cor.test(pdx_annual_2013_19$estimate_medinc_num_2018_nor, pdx_annual_2013_19$prop_for_2018_nor, method = 'spearman', exact=FALSE)[3:4])
income_forest_2018 <- data.frame(income_for_corr = income_forest_test_2018$estimate, income_for_pvalue = income_forest_test_2018$p.value)
income_forest_2018$year <- 2018
income_forest_test_2019 <- data.frame(cor.test(pdx_annual_2013_19$estimate_medinc_num_2019_nor, pdx_annual_2013_19$prop_for_2019_nor, method = 'spearman', exact=FALSE)[3:4])
income_forest_2019 <- data.frame(income_for_corr = income_forest_test_2019$estimate, income_for_pvalue = income_forest_test_2019$p.value)
income_forest_2019$year <- 2019
income_forest_test_2020 <- data.frame(cor.test(pdx_annual_2020_23$estimate_medinc_num_2020_nor, pdx_annual_2020_23$prop_for_2020_nor, method = 'spearman', exact=FALSE)[3:4])
income_forest_2020 <- data.frame(income_for_corr = income_forest_test_2020$estimate, income_for_pvalue = income_forest_test_2020$p.value)
income_forest_2020$year <- 2020
income_forest_test_2021 <- data.frame(cor.test(pdx_annual_2020_23$estimate_medinc_num_2021_nor, pdx_annual_2020_23$prop_for_2021_nor, method = 'spearman', exact=FALSE)[3:4])
income_forest_2021 <- data.frame(income_for_corr = income_forest_test_2021$estimate, income_for_pvalue = income_forest_test_2021$p.value)
income_forest_2021$year <- 2021
income_forest_test_2022 <- data.frame(cor.test(pdx_annual_2020_23$estimate_medinc_num_2022_nor, pdx_annual_2020_23$prop_for_2022_nor, method = 'spearman', exact=FALSE)[3:4])
income_forest_2022 <- data.frame(income_for_corr = income_forest_test_2022$estimate, income_for_pvalue = income_forest_test_2022$p.value)
income_forest_2022$year <- 2022
income_forest_test_2023 <- data.frame(cor.test(pdx_annual_2020_23$estimate_medinc_num_2023_nor, pdx_annual_2020_23$prop_for_2023_nor, method = 'spearman', exact=FALSE)[3:4])
income_forest_2023 <- data.frame(income_for_corr = income_forest_test_2023$estimate, income_for_pvalue = income_forest_test_2023$p.value)
income_forest_2023$year <- 2023

income_forest_allyears <- rbind(income_forest_2013, income_forest_2014, income_forest_2015, income_forest_2016, income_forest_2017, income_forest_2018, income_forest_2019, income_forest_2020, income_forest_2021, income_forest_2022, income_forest_2023)
income_forest_allyears <- income_forest_allyears %>% 
  mutate(plot_value = ifelse(income_for_pvalue > 0.05, NA, income_for_corr))
income_forest_lm <- lm(income_forest_allyears$plot_value ~ income_forest_allyears$year)
summary(income_forest_lm)

### Creating income forest plot, trend is significant include geom_smooth ###
income_for_plot <- ggplot(income_forest_allyears, aes(x=year, y=plot_value)) + geom_hline(yintercept = 0, color = "black", linewidth = 1) + 
  geom_point(size = 2.5) + scale_x_continuous("Year", labels = as.character(income_forest_allyears$year), breaks = income_forest_allyears$year) +
  ylab("Correlation") + ylim(-0.5, 0.5) + theme_bw()+theme(panel.grid.minor = element_blank(), axis.ticks.y=element_blank(),plot.title = element_text(hjust = 0.5), text=element_text(size=20), axis.text.x=element_text(angle = 45, hjust = 1)) + geom_smooth(method = "lm",color = "red",linewidth = 0.5)+
  ggtitle("Income x Forest")
income_for_plot

#### bach ###########
bach_forest_test_2013 <- data.frame(cor.test(pdx_annual_2013_19$prop_bach_2013_nor, pdx_annual_2013_19$prop_for_2013_nor, method = 'spearman', exact=FALSE)[3:4])
bach_forest_2013 <- data.frame(bach_for_corr = bach_forest_test_2013$estimate, bach_for_pvalue = bach_forest_test_2013$p.value)
bach_forest_2013$year <- 2013
bach_forest_test_2014 <- data.frame(cor.test(pdx_annual_2013_19$prop_bach_2014_nor, pdx_annual_2013_19$prop_for_2014_nor, method = 'spearman', exact=FALSE)[3:4])
bach_forest_2014 <- data.frame(bach_for_corr = bach_forest_test_2014$estimate, bach_for_pvalue = bach_forest_test_2014$p.value)
bach_forest_2014$year <- 2014
bach_forest_test_2015 <- data.frame(cor.test(pdx_annual_2013_19$prop_bach_2015_nor, pdx_annual_2013_19$prop_for_2015_nor, method = 'spearman', exact=FALSE)[3:4])
bach_forest_2015 <- data.frame(bach_for_corr = bach_forest_test_2015$estimate, bach_for_pvalue = bach_forest_test_2015$p.value)
bach_forest_2015$year <- 2015
bach_forest_test_2016 <- data.frame(cor.test(pdx_annual_2013_19$prop_bach_2016_nor, pdx_annual_2013_19$prop_for_2016_nor, method = 'spearman', exact=FALSE)[3:4])
bach_forest_2016 <- data.frame(bach_for_corr = bach_forest_test_2016$estimate, bach_for_pvalue = bach_forest_test_2016$p.value)
bach_forest_2016$year <- 2016
bach_forest_test_2017 <- data.frame(cor.test(pdx_annual_2013_19$prop_bach_2017_nor, pdx_annual_2013_19$prop_for_2017_nor, method = 'spearman', exact=FALSE)[3:4])
bach_forest_2017 <- data.frame(bach_for_corr = bach_forest_test_2017$estimate, bach_for_pvalue = bach_forest_test_2017$p.value)
bach_forest_2017$year <- 2017
bach_forest_test_2018 <- data.frame(cor.test(pdx_annual_2013_19$prop_bach_2018_nor, pdx_annual_2013_19$prop_for_2018_nor, method = 'spearman', exact=FALSE)[3:4])
bach_forest_2018 <- data.frame(bach_for_corr = bach_forest_test_2018$estimate, bach_for_pvalue = bach_forest_test_2018$p.value)
bach_forest_2018$year <- 2018
bach_forest_test_2019 <- data.frame(cor.test(pdx_annual_2013_19$prop_bach_2019_nor, pdx_annual_2013_19$prop_for_2019_nor, method = 'spearman', exact=FALSE)[3:4])
bach_forest_2019 <- data.frame(bach_for_corr = bach_forest_test_2019$estimate, bach_for_pvalue = bach_forest_test_2019$p.value)
bach_forest_2019$year <- 2019
bach_forest_test_2020 <- data.frame(cor.test(pdx_annual_2020_23$prop_bach_2020_nor, pdx_annual_2020_23$prop_for_2020_nor, method = 'spearman', exact=FALSE)[3:4])
bach_forest_2020 <- data.frame(bach_for_corr = bach_forest_test_2020$estimate, bach_for_pvalue = bach_forest_test_2020$p.value)
bach_forest_2020$year <- 2020
bach_forest_test_2021 <- data.frame(cor.test(pdx_annual_2020_23$prop_bach_2021_nor, pdx_annual_2020_23$prop_for_2021_nor, method = 'spearman', exact=FALSE)[3:4])
bach_forest_2021 <- data.frame(bach_for_corr = bach_forest_test_2021$estimate, bach_for_pvalue = bach_forest_test_2021$p.value)
bach_forest_2021$year <- 2021
bach_forest_test_2022 <- data.frame(cor.test(pdx_annual_2020_23$prop_bach_2022_nor, pdx_annual_2020_23$prop_for_2022_nor, method = 'spearman', exact=FALSE)[3:4])
bach_forest_2022 <- data.frame(bach_for_corr = bach_forest_test_2022$estimate, bach_for_pvalue = bach_forest_test_2022$p.value)
bach_forest_2022$year <- 2022
bach_forest_test_2023 <- data.frame(cor.test(pdx_annual_2020_23$prop_bach_2023_nor, pdx_annual_2020_23$prop_for_2023_nor, method = 'spearman', exact=FALSE)[3:4])
bach_forest_2023 <- data.frame(bach_for_corr = bach_forest_test_2023$estimate, bach_for_pvalue = bach_forest_test_2023$p.value)
bach_forest_2023$year <- 2023

bach_forest_allyears <- rbind(bach_forest_2013, bach_forest_2014, bach_forest_2015, bach_forest_2016, bach_forest_2017, bach_forest_2018, bach_forest_2019, bach_forest_2020, bach_forest_2021, bach_forest_2022, bach_forest_2023)
bach_forest_allyears <- bach_forest_allyears %>% 
  mutate(plot_value = ifelse(bach_for_pvalue > 0.05, NA, bach_for_corr))
bach_forest_lm <- lm(bach_forest_allyears$plot_value ~ bach_forest_allyears$year)
summary(bach_forest_lm)

### Creating bach forest plot, only one plot point, unable to regress ###
bach_for_plot <- ggplot(bach_forest_allyears, aes(x=year, y=plot_value)) + geom_hline(yintercept = 0, color = "black", linewidth = 1) + 
  geom_point(size = 2.5) + scale_x_continuous("Year", labels = as.character(bach_forest_allyears$year), breaks = bach_forest_allyears$year) +
  ylab("Correlation") + ylim(-0.5, 0.5) + theme_bw()+theme(panel.grid.minor = element_blank(), axis.ticks.y=element_blank(),plot.title = element_text(hjust = 0.5), text=element_text(size=20), axis.text.x=element_text(angle = 45, hjust = 1)) +
  ggtitle("Bachelor's x Forest")#+ geom_smooth(method = "lm",color = "red",linewidth = 0.5)
bach_for_plot




##################################################################################
#### Correlation between race/inc/ed and gsi ############################
##################################################################################

#### White ###########
white_gsi_test_2013 <- data.frame(cor.test(pdx_annual_2013_19$prop_white_2013_nor, pdx_annual_2013_19$GILength_m_2013_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
white_gsi_2013 <- data.frame(white_gsi_corr = white_gsi_test_2013$estimate, white_gsi_pvalue = white_gsi_test_2013$p.value)
white_gsi_2013$year <- 2013
white_gsi_test_2014 <- data.frame(cor.test(pdx_annual_2013_19$prop_white_2014_nor, pdx_annual_2013_19$GILength_m_2014_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
white_gsi_2014 <- data.frame(white_gsi_corr = white_gsi_test_2014$estimate, white_gsi_pvalue = white_gsi_test_2014$p.value)
white_gsi_2014$year <- 2014
white_gsi_test_2015 <- data.frame(cor.test(pdx_annual_2013_19$prop_white_2015_nor, pdx_annual_2013_19$GILength_m_2015_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
white_gsi_2015 <- data.frame(white_gsi_corr = white_gsi_test_2015$estimate, white_gsi_pvalue = white_gsi_test_2015$p.value)
white_gsi_2015$year <- 2015
white_gsi_test_2016 <- data.frame(cor.test(pdx_annual_2013_19$prop_white_2016_nor, pdx_annual_2013_19$GILength_m_2016_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
white_gsi_2016 <- data.frame(white_gsi_corr = white_gsi_test_2016$estimate, white_gsi_pvalue = white_gsi_test_2016$p.value)
white_gsi_2016$year <- 2016
white_gsi_test_2017 <- data.frame(cor.test(pdx_annual_2013_19$prop_white_2017_nor, pdx_annual_2013_19$GILength_m_2017_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
white_gsi_2017 <- data.frame(white_gsi_corr = white_gsi_test_2017$estimate, white_gsi_pvalue = white_gsi_test_2017$p.value)
white_gsi_2017$year <- 2017
white_gsi_test_2018 <- data.frame(cor.test(pdx_annual_2013_19$prop_white_2018_nor, pdx_annual_2013_19$GILength_m_2018_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
white_gsi_2018 <- data.frame(white_gsi_corr = white_gsi_test_2018$estimate, white_gsi_pvalue = white_gsi_test_2018$p.value)
white_gsi_2018$year <- 2018
white_gsi_test_2019 <- data.frame(cor.test(pdx_annual_2013_19$prop_white_2019_nor, pdx_annual_2013_19$GILength_m_2019_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
white_gsi_2019 <- data.frame(white_gsi_corr = white_gsi_test_2019$estimate, white_gsi_pvalue = white_gsi_test_2019$p.value)
white_gsi_2019$year <- 2019
white_gsi_test_2020 <- data.frame(cor.test(pdx_annual_2020_23$prop_white_2020_nor, pdx_annual_2020_23$GILength_m_2020_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
white_gsi_2020 <- data.frame(white_gsi_corr = white_gsi_test_2020$estimate, white_gsi_pvalue = white_gsi_test_2020$p.value)
white_gsi_2020$year <- 2020
white_gsi_test_2021 <- data.frame(cor.test(pdx_annual_2020_23$prop_white_2021_nor, pdx_annual_2020_23$GILength_m_2021_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
white_gsi_2021 <- data.frame(white_gsi_corr = white_gsi_test_2021$estimate, white_gsi_pvalue = white_gsi_test_2021$p.value)
white_gsi_2021$year <- 2021
white_gsi_test_2022 <- data.frame(cor.test(pdx_annual_2020_23$prop_white_2022_nor, pdx_annual_2020_23$GILength_m_2022_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
white_gsi_2022 <- data.frame(white_gsi_corr = white_gsi_test_2022$estimate, white_gsi_pvalue = white_gsi_test_2022$p.value)
white_gsi_2022$year <- 2022
white_gsi_test_2023 <- data.frame(cor.test(pdx_annual_2020_23$prop_white_2023_nor, pdx_annual_2020_23$GILength_m_2023_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
white_gsi_2023 <- data.frame(white_gsi_corr = white_gsi_test_2023$estimate, white_gsi_pvalue = white_gsi_test_2023$p.value)
white_gsi_2023$year <- 2023

white_gsi_allyears <- rbind(white_gsi_2013, white_gsi_2014, white_gsi_2015, white_gsi_2016, white_gsi_2017, white_gsi_2018, white_gsi_2019, white_gsi_2020, white_gsi_2021, white_gsi_2022, white_gsi_2023)
white_gsi_allyears <- white_gsi_allyears %>% 
  mutate(plot_value = ifelse(white_gsi_pvalue > 0.05, NA, white_gsi_corr))
white_gsi_lm <- lm(white_gsi_allyears$plot_value ~ white_gsi_allyears$year)
summary(white_gsi_lm)

### Creating white gsi plot, trend is not significant, remove geom_smooth ###
white_gsi_plot <- ggplot(white_gsi_allyears, aes(x=year, y=plot_value)) + geom_hline(yintercept = 0, color = "black", linewidth = 1) + 
  geom_point(size = 2.5) + scale_x_continuous("Year", labels = as.character(white_gsi_allyears$year), breaks = white_gsi_allyears$year) +
  ylab("Correlation") + ylim(-0.5, 0.5) + theme_bw()+theme(panel.grid.minor = element_blank(), axis.ticks.y=element_blank(),plot.title = element_text(hjust = 0.5), text=element_text(size=20), axis.text.x=element_text(angle = 45, hjust = 1)) +
  ggtitle("White x Green Streets")#+ geom_smooth(method = "lm",color = "red",linewidth = 0.5)
white_gsi_plot


#### black ###########
black_gsi_test_2013 <- data.frame(cor.test(pdx_annual_2013_19$prop_black_2013_nor, pdx_annual_2013_19$GILength_m_2013_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
black_gsi_2013 <- data.frame(black_gsi_corr = black_gsi_test_2013$estimate, black_gsi_pvalue = black_gsi_test_2013$p.value)
black_gsi_2013$year <- 2013
black_gsi_test_2014 <- data.frame(cor.test(pdx_annual_2013_19$prop_black_2014_nor, pdx_annual_2013_19$GILength_m_2014_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
black_gsi_2014 <- data.frame(black_gsi_corr = black_gsi_test_2014$estimate, black_gsi_pvalue = black_gsi_test_2014$p.value)
black_gsi_2014$year <- 2014
black_gsi_test_2015 <- data.frame(cor.test(pdx_annual_2013_19$prop_black_2015_nor, pdx_annual_2013_19$GILength_m_2015_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
black_gsi_2015 <- data.frame(black_gsi_corr = black_gsi_test_2015$estimate, black_gsi_pvalue = black_gsi_test_2015$p.value)
black_gsi_2015$year <- 2015
black_gsi_test_2016 <- data.frame(cor.test(pdx_annual_2013_19$prop_black_2016_nor, pdx_annual_2013_19$GILength_m_2016_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
black_gsi_2016 <- data.frame(black_gsi_corr = black_gsi_test_2016$estimate, black_gsi_pvalue = black_gsi_test_2016$p.value)
black_gsi_2016$year <- 2016
black_gsi_test_2017 <- data.frame(cor.test(pdx_annual_2013_19$prop_black_2017_nor, pdx_annual_2013_19$GILength_m_2017_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
black_gsi_2017 <- data.frame(black_gsi_corr = black_gsi_test_2017$estimate, black_gsi_pvalue = black_gsi_test_2017$p.value)
black_gsi_2017$year <- 2017
black_gsi_test_2018 <- data.frame(cor.test(pdx_annual_2013_19$prop_black_2018_nor, pdx_annual_2013_19$GILength_m_2018_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
black_gsi_2018 <- data.frame(black_gsi_corr = black_gsi_test_2018$estimate, black_gsi_pvalue = black_gsi_test_2018$p.value)
black_gsi_2018$year <- 2018
black_gsi_test_2019 <- data.frame(cor.test(pdx_annual_2013_19$prop_black_2019_nor, pdx_annual_2013_19$GILength_m_2019_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
black_gsi_2019 <- data.frame(black_gsi_corr = black_gsi_test_2019$estimate, black_gsi_pvalue = black_gsi_test_2019$p.value)
black_gsi_2019$year <- 2019
black_gsi_test_2020 <- data.frame(cor.test(pdx_annual_2020_23$prop_black_2020_nor, pdx_annual_2020_23$GILength_m_2020_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
black_gsi_2020 <- data.frame(black_gsi_corr = black_gsi_test_2020$estimate, black_gsi_pvalue = black_gsi_test_2020$p.value)
black_gsi_2020$year <- 2020
black_gsi_test_2021 <- data.frame(cor.test(pdx_annual_2020_23$prop_black_2021_nor, pdx_annual_2020_23$GILength_m_2021_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
black_gsi_2021 <- data.frame(black_gsi_corr = black_gsi_test_2021$estimate, black_gsi_pvalue = black_gsi_test_2021$p.value)
black_gsi_2021$year <- 2021
black_gsi_test_2022 <- data.frame(cor.test(pdx_annual_2020_23$prop_black_2022_nor, pdx_annual_2020_23$GILength_m_2022_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
black_gsi_2022 <- data.frame(black_gsi_corr = black_gsi_test_2022$estimate, black_gsi_pvalue = black_gsi_test_2022$p.value)
black_gsi_2022$year <- 2022
black_gsi_test_2023 <- data.frame(cor.test(pdx_annual_2020_23$prop_black_2023_nor, pdx_annual_2020_23$GILength_m_2023_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
black_gsi_2023 <- data.frame(black_gsi_corr = black_gsi_test_2023$estimate, black_gsi_pvalue = black_gsi_test_2023$p.value)
black_gsi_2023$year <- 2023

black_gsi_allyears <- rbind(black_gsi_2013, black_gsi_2014, black_gsi_2015, black_gsi_2016, black_gsi_2017, black_gsi_2018, black_gsi_2019, black_gsi_2020, black_gsi_2021, black_gsi_2022, black_gsi_2023)
black_gsi_allyears <- black_gsi_allyears %>% 
  mutate(plot_value = ifelse(black_gsi_pvalue > 0.05, NA, black_gsi_corr))
black_gsi_lm <- lm(black_gsi_allyears$plot_value ~ black_gsi_allyears$year)
summary(black_gsi_lm)

### Creating black gsi plot, trend is significant ###
black_gsi_plot <- ggplot(black_gsi_allyears, aes(x=year, y=plot_value)) + geom_hline(yintercept = 0, color = "black", linewidth = 1) + 
  geom_point(size = 2.5) + scale_x_continuous("Year", labels = as.character(black_gsi_allyears$year), breaks = black_gsi_allyears$year) +
  ylab("Correlation") + ylim(-0.5, 0.5) + theme_bw()+theme(panel.grid.minor = element_blank(), axis.ticks.y=element_blank(),plot.title = element_text(hjust = 0.5), text=element_text(size=20), axis.text.x=element_text(angle = 45, hjust = 1)) + geom_smooth(method = "lm",color = "red",linewidth = 0.5)+
  ggtitle("Black x Green Streets")
black_gsi_plot


#### income ###########
income_gsi_test_2013 <- data.frame(cor.test(pdx_annual_2013_19$estimate_medinc_num_2013_nor, pdx_annual_2013_19$GILength_m_2013_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
income_gsi_2013 <- data.frame(income_gsi_corr = income_gsi_test_2013$estimate, income_gsi_pvalue = income_gsi_test_2013$p.value)
income_gsi_2013$year <- 2013
income_gsi_test_2014 <- data.frame(cor.test(pdx_annual_2013_19$estimate_medinc_num_2014_nor, pdx_annual_2013_19$GILength_m_2014_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
income_gsi_2014 <- data.frame(income_gsi_corr = income_gsi_test_2014$estimate, income_gsi_pvalue = income_gsi_test_2014$p.value)
income_gsi_2014$year <- 2014
income_gsi_test_2015 <- data.frame(cor.test(pdx_annual_2013_19$estimate_medinc_num_2015_nor, pdx_annual_2013_19$GILength_m_2015_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
income_gsi_2015 <- data.frame(income_gsi_corr = income_gsi_test_2015$estimate, income_gsi_pvalue = income_gsi_test_2015$p.value)
income_gsi_2015$year <- 2015
income_gsi_test_2016 <- data.frame(cor.test(pdx_annual_2013_19$estimate_medinc_num_2016_nor, pdx_annual_2013_19$GILength_m_2016_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
income_gsi_2016 <- data.frame(income_gsi_corr = income_gsi_test_2016$estimate, income_gsi_pvalue = income_gsi_test_2016$p.value)
income_gsi_2016$year <- 2016
income_gsi_test_2017 <- data.frame(cor.test(pdx_annual_2013_19$estimate_medinc_num_2017_nor, pdx_annual_2013_19$GILength_m_2017_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
income_gsi_2017 <- data.frame(income_gsi_corr = income_gsi_test_2017$estimate, income_gsi_pvalue = income_gsi_test_2017$p.value)
income_gsi_2017$year <- 2017
income_gsi_test_2018 <- data.frame(cor.test(pdx_annual_2013_19$estimate_medinc_num_2018_nor, pdx_annual_2013_19$GILength_m_2018_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
income_gsi_2018 <- data.frame(income_gsi_corr = income_gsi_test_2018$estimate, income_gsi_pvalue = income_gsi_test_2018$p.value)
income_gsi_2018$year <- 2018
income_gsi_test_2019 <- data.frame(cor.test(pdx_annual_2013_19$estimate_medinc_num_2019_nor, pdx_annual_2013_19$GILength_m_2019_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
income_gsi_2019 <- data.frame(income_gsi_corr = income_gsi_test_2019$estimate, income_gsi_pvalue = income_gsi_test_2019$p.value)
income_gsi_2019$year <- 2019
income_gsi_test_2020 <- data.frame(cor.test(pdx_annual_2020_23$estimate_medinc_num_2020_nor, pdx_annual_2020_23$GILength_m_2020_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
income_gsi_2020 <- data.frame(income_gsi_corr = income_gsi_test_2020$estimate, income_gsi_pvalue = income_gsi_test_2020$p.value)
income_gsi_2020$year <- 2020
income_gsi_test_2021 <- data.frame(cor.test(pdx_annual_2020_23$estimate_medinc_num_2021_nor, pdx_annual_2020_23$GILength_m_2021_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
income_gsi_2021 <- data.frame(income_gsi_corr = income_gsi_test_2021$estimate, income_gsi_pvalue = income_gsi_test_2021$p.value)
income_gsi_2021$year <- 2021
income_gsi_test_2022 <- data.frame(cor.test(pdx_annual_2020_23$estimate_medinc_num_2022_nor, pdx_annual_2020_23$GILength_m_2022_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
income_gsi_2022 <- data.frame(income_gsi_corr = income_gsi_test_2022$estimate, income_gsi_pvalue = income_gsi_test_2022$p.value)
income_gsi_2022$year <- 2022
income_gsi_test_2023 <- data.frame(cor.test(pdx_annual_2020_23$estimate_medinc_num_2023_nor, pdx_annual_2020_23$GILength_m_2023_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
income_gsi_2023 <- data.frame(income_gsi_corr = income_gsi_test_2023$estimate, income_gsi_pvalue = income_gsi_test_2023$p.value)
income_gsi_2023$year <- 2023

income_gsi_allyears <- rbind(income_gsi_2013, income_gsi_2014, income_gsi_2015, income_gsi_2016, income_gsi_2017, income_gsi_2018, income_gsi_2019, income_gsi_2020, income_gsi_2021, income_gsi_2022, income_gsi_2023)
income_gsi_allyears <- income_gsi_allyears %>% 
  mutate(plot_value = ifelse(income_gsi_pvalue > 0.05, NA, income_gsi_corr))
income_gsi_lm <- lm(income_gsi_allyears$plot_value ~ income_gsi_allyears$year)
summary(income_gsi_lm)

### Creating income gsi plot, trend is significant include geom_smooth ###
income_gsi_plot <- ggplot(income_gsi_allyears, aes(x=year, y=plot_value)) + geom_hline(yintercept = 0, color = "black", linewidth = 1) + 
  geom_point(size = 2.5) + scale_x_continuous("Year", labels = as.character(income_gsi_allyears$year), breaks = income_gsi_allyears$year) +
  ylab("Correlation") + ylim(-0.5, 0.5) + theme_bw()+theme(panel.grid.minor = element_blank(), axis.ticks.y=element_blank(),plot.title = element_text(hjust = 0.5), text=element_text(size=20), axis.text.x=element_text(angle = 45, hjust = 1)) + geom_smooth(method = "lm",color = "red",linewidth = 0.5)+
  ggtitle("Income x Green Streets")
income_gsi_plot

#### bach ###########
bach_gsi_test_2013 <- data.frame(cor.test(pdx_annual_2013_19$prop_bach_2013_nor, pdx_annual_2013_19$GILength_m_2013_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
bach_gsi_2013 <- data.frame(bach_gsi_corr = bach_gsi_test_2013$estimate, bach_gsi_pvalue = bach_gsi_test_2013$p.value)
bach_gsi_2013$year <- 2013
bach_gsi_test_2014 <- data.frame(cor.test(pdx_annual_2013_19$prop_bach_2014_nor, pdx_annual_2013_19$GILength_m_2014_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
bach_gsi_2014 <- data.frame(bach_gsi_corr = bach_gsi_test_2014$estimate, bach_gsi_pvalue = bach_gsi_test_2014$p.value)
bach_gsi_2014$year <- 2014
bach_gsi_test_2015 <- data.frame(cor.test(pdx_annual_2013_19$prop_bach_2015_nor, pdx_annual_2013_19$GILength_m_2015_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
bach_gsi_2015 <- data.frame(bach_gsi_corr = bach_gsi_test_2015$estimate, bach_gsi_pvalue = bach_gsi_test_2015$p.value)
bach_gsi_2015$year <- 2015
bach_gsi_test_2016 <- data.frame(cor.test(pdx_annual_2013_19$prop_bach_2016_nor, pdx_annual_2013_19$GILength_m_2016_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
bach_gsi_2016 <- data.frame(bach_gsi_corr = bach_gsi_test_2016$estimate, bach_gsi_pvalue = bach_gsi_test_2016$p.value)
bach_gsi_2016$year <- 2016
bach_gsi_test_2017 <- data.frame(cor.test(pdx_annual_2013_19$prop_bach_2017_nor, pdx_annual_2013_19$GILength_m_2017_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
bach_gsi_2017 <- data.frame(bach_gsi_corr = bach_gsi_test_2017$estimate, bach_gsi_pvalue = bach_gsi_test_2017$p.value)
bach_gsi_2017$year <- 2017
bach_gsi_test_2018 <- data.frame(cor.test(pdx_annual_2013_19$prop_bach_2018_nor, pdx_annual_2013_19$GILength_m_2018_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
bach_gsi_2018 <- data.frame(bach_gsi_corr = bach_gsi_test_2018$estimate, bach_gsi_pvalue = bach_gsi_test_2018$p.value)
bach_gsi_2018$year <- 2018
bach_gsi_test_2019 <- data.frame(cor.test(pdx_annual_2013_19$prop_bach_2019_nor, pdx_annual_2013_19$GILength_m_2019_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
bach_gsi_2019 <- data.frame(bach_gsi_corr = bach_gsi_test_2019$estimate, bach_gsi_pvalue = bach_gsi_test_2019$p.value)
bach_gsi_2019$year <- 2019
bach_gsi_test_2020 <- data.frame(cor.test(pdx_annual_2020_23$prop_bach_2020_nor, pdx_annual_2020_23$GILength_m_2020_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
bach_gsi_2020 <- data.frame(bach_gsi_corr = bach_gsi_test_2020$estimate, bach_gsi_pvalue = bach_gsi_test_2020$p.value)
bach_gsi_2020$year <- 2020
bach_gsi_test_2021 <- data.frame(cor.test(pdx_annual_2020_23$prop_bach_2021_nor, pdx_annual_2020_23$GILength_m_2021_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
bach_gsi_2021 <- data.frame(bach_gsi_corr = bach_gsi_test_2021$estimate, bach_gsi_pvalue = bach_gsi_test_2021$p.value)
bach_gsi_2021$year <- 2021
bach_gsi_test_2022 <- data.frame(cor.test(pdx_annual_2020_23$prop_bach_2022_nor, pdx_annual_2020_23$GILength_m_2022_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
bach_gsi_2022 <- data.frame(bach_gsi_corr = bach_gsi_test_2022$estimate, bach_gsi_pvalue = bach_gsi_test_2022$p.value)
bach_gsi_2022$year <- 2022
bach_gsi_test_2023 <- data.frame(cor.test(pdx_annual_2020_23$prop_bach_2023_nor, pdx_annual_2020_23$GILength_m_2023_sqkm_nor, method = 'spearman', exact=FALSE)[3:4])
bach_gsi_2023 <- data.frame(bach_gsi_corr = bach_gsi_test_2023$estimate, bach_gsi_pvalue = bach_gsi_test_2023$p.value)
bach_gsi_2023$year <- 2023

bach_gsi_allyears <- rbind(bach_gsi_2013, bach_gsi_2014, bach_gsi_2015, bach_gsi_2016, bach_gsi_2017, bach_gsi_2018, bach_gsi_2019, bach_gsi_2020, bach_gsi_2021, bach_gsi_2022, bach_gsi_2023)
bach_gsi_allyears <- bach_gsi_allyears %>% 
  mutate(plot_value = ifelse(bach_gsi_pvalue > 0.05, NA, bach_gsi_corr))
bach_gsi_lm <- lm(bach_gsi_allyears$plot_value ~ bach_gsi_allyears$year)
summary(bach_gsi_lm)

### Creating bach gsi plot, trend isn't significant, remove geom_smooth ###
bach_gsi_plot <- ggplot(bach_gsi_allyears, aes(x=year, y=plot_value)) + geom_hline(yintercept = 0, color = "black", linewidth = 1) + 
  geom_point(size = 2.5) + scale_x_continuous("Year", labels = as.character(bach_gsi_allyears$year), breaks = bach_gsi_allyears$year) +
  ylab("Correlation") + ylim(-0.5, 0.5) + theme_bw()+theme(panel.grid.minor = element_blank(), axis.ticks.y=element_blank(),plot.title = element_text(hjust = 0.5), text=element_text(size=20), axis.text.x=element_text(angle = 45, hjust = 1))+
  ggtitle("Bachelor's x Green Streets")#+ geom_smooth(method = "lm",color = "red",linewidth = 0.5)
bach_gsi_plot



#### Creating Arranged plots ####
corrplot_2col <- black_for_plot + black_gsi_plot + white_for_plot + white_gsi_plot + bach_for_plot +
  bach_gsi_plot + income_for_plot + income_gsi_plot + plot_layout(ncol=2, axes = "collect") 
corrplot_2col

