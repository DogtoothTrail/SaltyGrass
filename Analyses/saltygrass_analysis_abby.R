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

#### define independent variables as correct class
cn_data$species_factor = as.factor(cn_data$species)
cn_data$treatment_factor = as.factor(cn_data$treatment)
cn_data$organ_factor = as.factor(cn_data$organ)
cn_data$block_factor = as.factor(cn_data$block)

#### define the mixed effects model
percent_nitrogen_lmer = lmer(nitrogen_percent ~ treatment * species * organ + (1|block), 
                             data = cn_data)
summary(percent_nitrogen_lmer)
Anova(percent_nitrogen_lmer)
cld(emmeans(percent_nitrogen_lmer, ~treatment))
cld(emmeans(percent_nitrogen_lmer, ~treatment*species))
cld(emmeans(percent_nitrogen_lmer, ~organ*species))

## make plots



