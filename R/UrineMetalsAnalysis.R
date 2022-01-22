#load libraries
library(tidyverse)
library(corrplot)
library(tableone)

#load data
df1 <- read.csv("Data/demographics_and_metalconcs.csv")

# Format categorical variables
df1$sex <- as.factor(df1$sex)
df1$highest_educ <- as.factor(df1$highest_educ)
df1$age_group <- as.factor(df1$age_group)
df1$yr_survey <- as.factor(df1$yr_survey)

#### Define functions for descriptive statistics ####
# Geometric mean function
gm_mean = function(x, na.rm=FALSE){
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}



#### Table1 - participant characteristics ####

catvars_tab1 <- c("sex")


CreateTableOne(vars = c("age", "highest_educ", "C...Body.Mass.Index"),
               strata = "sex",
               factorVars = catvars_tab1,
               data = df1)


# make a vector of metal concentration variable names
metvars <- c("Al_mcg_perL", "As_mcg_perL", "Cd_mcg_perL", 
             "Cr_mcg_perL", "Cu_mcg_perL", "Mn_mcg_perL", "Pb_mcg_perL", "Se_mcg_perL", 
             "Hg_mcg_perL", "Alcr_mcg_g", "Ascr_mcg_g", "Cdcr_mcg_g", "Crcr_mcg_g", 
             "Cucr_mcg_g", "Mncr_mcg_g", "Pbcr_mcg_g", "Secr_mcg_g", "Hgcr_mcg_g"
             )

# define quantiles for summary data
quants <- c(0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95)

# Calculate quantiles for each metal for each variable subcategory
data_quantiles <- lapply(1:length(metvars), function(j){
    lapply(1:length(catvars_tab1 ), function(i){
        df1 %>% 
            group_by(!!sym(catvars_tab1 [i])) %>%  
            summarise(enframe(quantile(!!sym(metvars [j]), quants,
                                       na.rm=FALSE), "quantile", "value")) %>% 
            rename(strata=1) %>% 
            pivot_wider(names_from = quantile, values_from = value, names_prefix = "P") %>% 
            mutate(var = !!(catvars_tab1 [i]),
                   outvar = !!(metvars[j]))
        })
})
data_quantiles <- bind_rows(data_quantiles)

# Calculate arithmetic and geometric means for each metal for each variable subcategory
data_means <- lapply(1:length(metvars), function(j){
    lapply(1:length(catvars_tab1 ), function(i){
        df1 %>% 
            group_by(!!sym(catvars_tab1 [i])) %>%  
            summarise(n = n(),
                  armean = mean(!!sym(metvars [j]), na.rm=FALSE),
                  geomean = gm_mean(!!sym(metvars [j]), na.rm=FALSE)) %>% 
            rename(strata=1) %>% 
            mutate(var = !!(catvars_tab1 [i]),
                   outvar = !!(metvars[j]))
    })
})
data_means <- bind_rows(data_means)


# Calculate overall quantile data
alldata_quantiles <- lapply(1:length(metvars), function(j){
    df1 %>%
        summarise(enframe(quantile(!!sym(metvars [j]), quants,
                               na.rm=FALSE), "quantile", "value")) %>% 
        pivot_wider(names_from = quantile, values_from = value, names_prefix = "P") %>% 
        mutate(outvar = !!(metvars[j]))
})

# Calculate overall arithmetic and geometric means
alldata_means <- lapply(1:length(metvars), function(j){
    df1 %>% 
        summarise(n = n(),
              armean = mean(!!sym(metvars [j]), na.rm=FALSE),
              geomean = gm_mean(!!sym(metvars [j]), na.rm=FALSE)) %>% 
        mutate(outvar = !!(metvars[j]))
})


# Combine the calculated summary data
all_data_summ <- left_join(bind_rows(alldata_means), bind_rows(alldata_quantiles))
all_data_summ$strata <- "All particpants"
all_data_summ$metal <- substr(all_data_summ$outvar, 1, 2)
all_data_summ$units <- substr(all_data_summ$outvar, 3, 20)

# build final table
table2 <- left_join (data_means, data_quantiles )
table2$metal <- substr(table2$outvar, 1, 2)
table2$units <- substr(table2$outvar, 3, 20)
table2 <- bind_rows(all_data_summ, table2) %>%
    dplyr::select(metal, var, strata, n, units, everything(), -outvar) %>% 
    arrange(metal)



# Write table 2 to file
write.csv(table2, "Results/table2.csv", row.names = FALSE)


# calculate quantiles creatinine data by sex
creatbysex_quantiles <- df1 %>% 
  group_by(sex) %>%  
  summarise(enframe(quantile(creatinine_gperL, quants,
                             na.rm=FALSE), "quantile", "value")) %>% 
  rename(strata=1) %>% 
  pivot_wider(names_from = quantile, values_from = value, names_prefix = "P") %>% 
  mutate(var = "sex",
         outvar = "creatinine_gperL")

# calculate arithmetic and geometric means creatinine data by sex
creatbysex_means <- df1 %>% 
  group_by(sex) %>%  
  summarise(n = n(),
            armean = mean(creatinine_gperL, na.rm=FALSE),
            geomean = gm_mean(creatinine_gperL, na.rm=FALSE)) %>% 
  rename(strata=1) %>% 
  mutate(var = "sex",
         outvar = "creatinine_gperL")

# Join cretinine summaries and save to file
creatbysex <- left_join(bind_rows(creatbysex_means), bind_rows(creatbysex_quantiles))
write.csv(creatbysex, "Results/creatinine.csv", row.names = FALSE)



#### Correlation plot
# First make a small dataframe so as to rename columns for the plot
dfcorr <- df1[, c("Alcr_mcg_g", "Ascr_mcg_g", "Cdcr_mcg_g", "Crcr_mcg_g", 
                  "Cucr_mcg_g", "Mncr_mcg_g", "Pbcr_mcg_g", "Secr_mcg_g", "Hgcr_mcg_g")]
names(dfcorr) <- c("Aluminium", "Arsenic", "Cadmium", "Chromium", "Copper", 
                   "Manganese", "Lead", "Selenium", "Mercury")
corrs <- cor(dfcorr, )

tiff("Results/corrplot.tiff", width=1000, height=700)
  corrplot(corrs, type = "lower", order = "hclust", diag = FALSE)
dev.off()



##### Compare urinary cadmium reference values to HBM4EU thresholds by age
hbm1_cdur_31to40yrs <- 0.5
hbm1_cdur_41to50yrs <- 0.8
hbm1_cdur_over50yrs <- 1

table(df1[df1$age >=31 & df1$age < 41, ]$Cd_mcg_perL > hbm1_cdur_31to40yrs)
table(df1[df1$age >=41 & df1$age < 51, ]$Cd_mcg_perL > hbm1_cdur_41to50yrs)
table(df1[df1$age >=51, ]$Cd_mcg_perL > hbm1_cdur_over50yrs)


##### Compare urinary mercury levels to German HBM threshold levels #####
hbm1_hg_ur <- 7
hbm2_hg_ur <- 25

table(df1$Hg_mcg_perL > hbm1_hg_ur)
table(df1$Hg_mcg_perL > hbm2_hg_ur)


