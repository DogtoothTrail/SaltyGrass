# Salty Grass Analysis
## description: code to analyze the salt by species greenhouse experiment
## authors: Abigail Bell, Nick Smith

## useful commands
### getwd()

## load packages
### install.packages('stringr')
library(stringr)
library(lme4)
library(car)
library(emmeans)

## load data
cn_data_raw = read.csv('../Data/saltygrass_cn.csv')
height_data_raw = read.csv('../Data/Finalized_Height_Data_Linear.csv')
mass_data_raw = read.csv("../Data/Mass_Measurements.csv")

## curate the CN data
### remove the QC values
cn_data_noQC = subset(cn_data_raw, sample_id != 'QC')

### change sample id to character
cn_data_noQC$sample_id = as.character(cn_data_noQC$sample_id)

### adding missing periods
cn_data_noQC$sample_id[2] = "LB.1.8.S"
cn_data_noQC$sample_id[3] = "LB.1.16.S"
cn_data_noQC$sample_id[4] = "LB.1.24.S"
cn_data_noQC$sample_id[5] = "LB.2.0.S"
cn_data_noQC$sample_id[6] = "LB.2.8.S"
cn_data_noQC$sample_id[7] = "LB.2.16.S"
cn_data_noQC$sample_id[8] = "LB.2.24.S"
cn_data_noQC$sample_id[9] = "LB.3.0.S"
cn_data_noQC$sample_id[10] = "LB.3.8.S"
cn_data_noQC$sample_id[11] = "LB.3.16.S"
cn_data_noQC$sample_id[98] = "SG.1.8.R"

### split the sample id into separate columns
sample_id_list_cn = strsplit(as.character(cn_data_noQC$sample_id), "[.]")
cn_data_noQC$species = unlist(lapply(sample_id_list_cn, `[[`, 1))
cn_data_noQC$block = unlist(lapply(sample_id_list_cn, `[[`, 2))
cn_data_noQC$treatment = unlist(lapply(sample_id_list_cn, `[[`, 3))
cn_data_noQC$organ = unlist(lapply(sample_id_list_cn, `[[`, 4))

cn_data = cn_data_noQC

## curate the height data
## split the sample id into separate columns
sample_id_list_height = strsplit(as.character(height_data_raw$Sample), "[.]")
height_data_raw$treatment = unlist(lapply(sample_id_list_height, `[[`, 2))
height_data_raw$rep = unlist(lapply(sample_id_list_height, `[[`, 3))
height_data_raw_sample_block =strsplit(unlist(lapply(sample_id_list_height, `[[`, 1)), 
                                       "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])", perl=TRUE)
height_data_raw$block = unlist(lapply(height_data_raw_sample_block, '[[', 2))
height_data_raw$species = unlist(lapply(height_data_raw_sample_block, '[[', 1))

height_data = height_data_raw

## curate the mass data

### split the sample id into seperate columns
sample_id_list_mass = strsplit(as.character(mass_data_raw$pot), "[.]")
mass_data_raw$species = unlist(lapply(sample_id_list_mass, `[[`, 1))
mass_data_raw$block = unlist(lapply(sample_id_list_mass, `[[`, 2))
mass_data_raw$treatment = unlist(lapply(sample_id_list_mass, `[[`, 3))

mass_data = mass_data_raw

## hypothesis testing

### Does salinity affect nitrogen concentration and does this vary by species?
### Increasing salinity will reduce nitrogen concentration becuase XXXXXXXX

#### take a look at the top 6 rows
### head(cn_data)

#### see what class each variable is
class(cn_data$species)
class(cn_data$treatment)
class(cn_data$organ)
class(cn_data$block)
class(cn_data$nitrogen_percent)

#### define independent variables as correct class (if necessary)

#### define the mixed effects model
percent_nitrogen_lmer = lmer(nitrogen_percent ~ treatment * species * organ + (1|block), 
                             data = cn_data) # fit the model
plot(resid(percent_nitrogen_lmer) ~ fitted(percent_nitrogen_lmer)) # check  the residuals
summary(percent_nitrogen_lmer) # look at slope coefficients
Anova(percent_nitrogen_lmer) # check statistical significance of main effects
cld(emmeans(percent_nitrogen_lmer, ~treatment))
cld(emmeans(percent_nitrogen_lmer, ~species))
cld(emmeans(percent_nitrogen_lmer, ~treatment*species))
cld(emmeans(percent_nitrogen_lmer, ~organ*species))

#### create new variable for CN
cn_data$cn = cn_data$carbon_weight / cn_data$nitrogen_weight

#### define the mixed effects model
percent_nitrogen_lmer = lmer(cn ~ treatment * species * organ + (1|block), 
                             data = cn_data) # fit the model
plot(resid(percent_nitrogen_lmer) ~ fitted(percent_nitrogen_lmer)) # check  the residuals
summary(percent_nitrogen_lmer) # look at slope coefficients
Anova(percent_nitrogen_lmer) # check statistical significance of main effects
cld(emmeans(percent_nitrogen_lmer, ~treatment)) # CN decreases with salt, interesting!
cld(emmeans(percent_nitrogen_lmer, ~species))
cld(emmeans(percent_nitrogen_lmer, ~treatment*species)) # SG is most sensitive
cld(emmeans(percent_nitrogen_lmer, ~organ*species))

### head(mass_data)
#### see what class each variable is
class(mass_data$species)
class(mass_data$block)
class(mass_data$treatment)
class(mass_data$mass)
class(mass_data$organ)

#### define independent variables as correct class (if necessary)
                                 
#### define the mixed effects model

mass_values_lmer = lmer(log(mass + 0.001) ~ treatment * species * organ + (1|block), 
                             data = mass_data)
plot(resid(mass_values_lmer) ~ fitted (mass_values_lmer)) # slight horn shape here, need to transform
summary(mass_values_lmer)
Anova(mass_values_lmer)
cld(emmeans(mass_values_lmer, ~treatment)) # mass decreases with salinity treatment
cld(emmeans(mass_values_lmer, ~species)) # BM is heaviest
cld(emmeans(mass_values_lmer, ~treatment * species)) # BG has highest sensitivity; others not strongly effected

### head(height_data)

#### see what class each variable is
class(height_data$treatment)
class(height_data$rep)
class(height_data$block)
class(height_data$species)
class(height_data$Height_cm)

#### define independent variables as correct class (if necessary)

#### make new variable for index of repeated measure
##### don't need this, as height_data$Sample is the index

#### define the mixed effects model
height_values_lmer = lmer(log(Height_cm) ~ treatment * species * (1|Sample) + (1|block), 
                             data = height_data)
plot(resid(height_values_lmer) ~ fitted(height_values_lmer)) # very skewed, need to transform
summary(height_values_lmer)
Anova(height_values_lmer)
cld(emmeans(height_values_lmer, ~treatment)) # salt treatment are shorter
cld(emmeans(height_values_lmer, ~species))
cld(emmeans(height_values_lmer, ~treatment*species)) # BG is most sensitive, others not as much

## make plots



