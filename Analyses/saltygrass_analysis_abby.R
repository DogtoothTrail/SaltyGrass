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
library(ggplot2)
library(dplyr)
library(ciTools)

## load data
cn_data_raw = read.csv('../Data/saltygrass_cn.csv')
height_data_raw = read.csv('../Data/Finalized_Height_Data_Linear.csv')
mass_data_raw = read.csv("../Data/Mass_Measurements.csv")
germination_data_raw = read.csv("../Data/germination_data.csv")

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

## curate the germination data (nothing to do really)
germination_data = germination_data_raw

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
percent_nitrogen_lmer = lmer(log(nitrogen_percent) ~ treatment * species * organ + (1|block), 
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
cn_lmer = lmer(log(cn) ~ treatment * species * organ + (1|block), 
                             data = cn_data) # fit the model
plot(resid(cn_lmer) ~ fitted(cn_lmer)) # check  the residuals
summary(cn_lmer) # look at slope coefficients
Anova(cn_lmer) # check statistical significance of main effects
cld(emmeans(cn_lmer, ~treatment)) # CN decreases with salt, interesting!
cld(emmeans(cn_lmer, ~species))
cld(emmeans(cn_lmer, ~treatment*species)) # SG is most sensitive
cld(emmeans(cn_lmer, ~organ*species))

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
cld(emmeans(mass_values_lmer, ~treatment * organ))

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

### combine mass and percent N data to get whole plant N
cn_mass_data = left_join(mass_data, cn_data, by.x = pot, by.y = sample_id)
head(cn_mass_data)
cn_mass_data$nitrogen_percent_total = (cn_mass_data$nitrogen_percent / 100) * cn_mass_data$mass

nitrogen_percent_total_lmer = lmer(log(nitrogen_percent_total) ~ treatment * species * organ + (1|block), 
                        data = cn_mass_data)
plot(resid(nitrogen_percent_total_lmer) ~ fitted (nitrogen_percent_total_lmer)) # slight horn shape here, need to transform
summary(nitrogen_percent_total_lmer)
Anova(nitrogen_percent_total_lmer)
cld(emmeans(nitrogen_percent_total_lmer, ~treatment)) # nitrogen mass decreases with salinity treatment
cld(emmeans(nitrogen_percent_total_lmer, ~species)) # BM has most
cld(emmeans(nitrogen_percent_total_lmer, ~treatment * species)) # SG and BM least sensitive and have most

### does increasing salinity influence germination in any of the evaluated species

#### check variable class
class(germination_data$species)
class(germination_data$treatment)
class(germination_data$germination)

#### make new variable for treatment factor
germination_data$treatment_factor = as.factor(germination_data$treatment)

#### fit model and analyze the results
germination_lm = glm(germination ~ treatment_factor*species, data = germination_data, family = 'binomial')
plot(resid(germination_lm) ~ fitted(germination_lm))
Anova(germination_lm)
summary(germination_lm)

germination_data_probs = add_probs(germination_data, germination_lm, q = 1, comparison = "=")

germination_data_probs_group = group_by(germination_data_probs, species, treatment_factor)
summarise(germination_data_probs_group, mean_prob_equal_to1 = mean(prob_equal_to1), 
          sd_prob_equal_to1 = sd(prob_equal_to1))

cld(emmeans(germination_lm, ~treatment_factor, type = "response"))
cld(emmeans(germination_lm, ~treatment_factor*species, type = "response"))

## make plots

### mass plot
mass_data$salt_treatment = factor(mass_data$treatment, levels = c('0', '8', '16', '24'), ordered = T) 
mass_plot = ggplot(data = mass_data, aes(x = species, y = mass, fill = salt_treatment)) + 
  theme(legend.position = c(0.84, 0.8), # change where the legend is
        axis.title.y=element_text(size=rel(2), colour = 'black'), # change y axis title properties
        axis.title.x=element_text(size=rel(2), colour = 'black'), # change x axis title properties
        axis.text.x=element_text(size=rel(2), colour = 'black'), # change x axis text properties
        axis.text.y=element_text(size=rel(2), colour = 'black'), # change y axis text properties
        panel.background = element_rect(fill = 'white', colour = 'black'), # change background panel colors
        panel.grid.major = element_line(colour = "grey"), # change backgrond color
        legend.background = element_blank(), # change background of legend
        legend.box.background = element_rect(colour = "black")) + # change box of legend
  geom_boxplot() + # plot the data as a boxplot
  scale_fill_brewer(palette="Greys") + #use the greys palette (see: https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html)
  labs(fill = 'Salt treatment (ds/m)') + # change label for legend
  ylim(0, 8) + # change y-axis limits on graph
  ylab('Mass (g)') +  # change label on y-axis
  xlab('Species') + # change label on x axis 
  scale_x_discrete(labels=c("BG" = "B. gracilis", "BM" = "C. dactylon",
                              "LB" = "S. scoparium", "SG"= "B. curtipendula"))

### height plot
height_data$salt_treatment = factor(height_data$treatment, levels = c('0', '8', '16', '24'), ordered = T) 
height_plot = ggplot(data = height_data, aes(x = species, y = Height_cm, fill = salt_treatment)) + 
  theme(legend.position = c(0.84, 0.8), # change where the legend is
        axis.title.y=element_text(size=rel(2), colour = 'black'), # change y axis title properties
        axis.title.x=element_text(size=rel(2), colour = 'black'), # change x axis title properties
        axis.text.x=element_text(size=rel(2), colour = 'black'), # change x axis text properties
        axis.text.y=element_text(size=rel(2), colour = 'black'), # change y axis text properties
        panel.background = element_rect(fill = 'white', colour = 'black'), # change background panel colors
        panel.grid.major = element_line(colour = "grey"), # change backgrond color
        legend.background = element_blank(), # change background of legend
        legend.box.background = element_rect(colour = "black")) + # change box of legend
  geom_boxplot() + # plot the data as a boxplot
  scale_fill_brewer(palette="Greys") + #use the greys palette (see: https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html)
  labs(fill = 'Salt treatment (ds/m)') + # change label for legend
  ylim(0, 120) + # change y-axis limits on graph
  ylab('Height (cm)') +  # change label on y-axis
  xlab('Species') + # change label on x axis
  scale_x_discrete(labels=c("BG" = "B. gracilis", "BM" = "C. dactylon",
                          "LB" = "S. scoparium", "SG"= "B. curtipendula"))

### percent N plot
cn_data$salt_treatment = factor(cn_data$treatment, levels = c('0', '8', '16', '24'), ordered = T) 
percent_nitrogen_plot = ggplot(data = cn_data, aes(x = species, y = nitrogen_percent, fill = salt_treatment)) + 
  theme(legend.position = c(0.84, 0.8), # change where the legend is
        axis.title.y=element_text(size=rel(2), colour = 'black'), # change y axis title properties
        axis.title.x=element_text(size=rel(2), colour = 'black'), # change x axis title properties
        axis.text.x=element_text(size=rel(2), colour = 'black'), # change x axis text properties
        axis.text.y=element_text(size=rel(2), colour = 'black'), # change y axis text properties
        panel.background = element_rect(fill = 'white', colour = 'black'), # change background panel colors
        panel.grid.major = element_line(colour = "grey"), # change backgrond color
        legend.background = element_blank(), # change background of legend
        legend.box.background = element_rect(colour = "black")) + # change box of legend
  geom_boxplot() + # plot the data as a boxplot
  scale_fill_brewer(palette="Greys") + #use the greys palette (see: https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html)
  labs(fill = 'Salt treatment (ds/m)') + # change label for legend
  ylim(0, 6) + # change y-axis limits on graph
  ylab('Shoot N (%)') +  # change label on y-axis
  xlab('Species') + # change label on x axis
  scale_x_discrete(labels=c("BG" = "B. gracilis", "BM" = "C. dactylon",
                          "LB" = "S. scoparium", "SG"= "B. curtipendula"))

### CN plot
cn_data$salt_treatment = factor(cn_data$treatment, levels = c('0', '8', '16', '24'), ordered = T) 
cn_plot = ggplot(data = cn_data, aes(x = species, y = cn, fill = salt_treatment)) + 
  theme(legend.position = c(0.84, 0.8), # change where the legend is
        axis.title.y=element_text(size=rel(2), colour = 'black'), # change y axis title properties
        axis.title.x=element_text(size=rel(2), colour = 'black'), # change x axis title properties
        axis.text.x=element_text(size=rel(2), colour = 'black'), # change x axis text properties
        axis.text.y=element_text(size=rel(2), colour = 'black'), # change y axis text properties
        panel.background = element_rect(fill = 'white', colour = 'black'), # change background panel colors
        panel.grid.major = element_line(colour = "grey"), # change backgrond color
        legend.background = element_blank(), # change background of legend
        legend.box.background = element_rect(colour = "black")) + # change box of legend
  geom_boxplot() + # plot the data as a boxplot
  scale_fill_brewer(palette="Greys") + #use the greys palette (see: https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html)
  labs(fill = 'Salt treatment (ds/m)') + # change label for legend
  ylim(0, 150) + # change y-axis limits on graph
  ylab('Shoot C:N') +  # change label on y-axis
  xlab('Species') + # change label on x axis
  scale_x_discrete(labels=c("BG" = "B. gracilis", "BM" = "C. dactylon",
                          "LB" = "S. scoparium", "SG"= "B. curtipendula"))

### mass N plot
cn_mass_data$salt_treatment = factor(cn_mass_data$treatment, levels = c('0', '8', '16', '24'), ordered = T) 
mass_plot = ggplot(data = cn_mass_data, aes(x = species, y = nitrogen_percent_total, fill = salt_treatment)) + 
  theme(legend.position = c(0.84, 0.8), # change where the legend is
        axis.title.y=element_text(size=rel(2), colour = 'black'), # change y axis title properties
        axis.title.x=element_text(size=rel(2), colour = 'black'), # change x axis title properties
        axis.text.x=element_text(size=rel(2), colour = 'black'), # change x axis text properties
        axis.text.y=element_text(size=rel(2), colour = 'black'), # change y axis text properties
        panel.background = element_rect(fill = 'white', colour = 'black'), # change background panel colors
        panel.grid.major = element_line(colour = "grey"), # change backgrond color
        legend.background = element_blank(), # change background of legend
        legend.box.background = element_rect(colour = "black")) + # change box of legend
  geom_boxplot() + # plot the data as a boxplot
  scale_fill_brewer(palette="Greys") + #use the greys palette (see: https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html)
  labs(fill = 'Salt treatment (ds/m)') + # change label for legend
  ylim(0, 0.08) + # change y-axis limits on graph
  ylab('Mass N (g)') +  # change label on y-axis
  xlab('Species') + # change label on x axis
  scale_x_discrete(labels=c("BG" = "B. gracilis", "BM" = "C. dactylon",
                          "LB" = "S. scoparium", "SG"= "B. curtipendula"))

### germination plot
germination_probs = summary(emmeans(germination_lm, ~treatment_factor*species, type = "response"))
germination_probs$salt_treatment = factor(germination_probs$treatment, levels = c('0', '8', '16', '24'), ordered = T) 
germination_plot = ggplot(data = germination_probs, aes(x = species, y = prob, fill = salt_treatment)) + 
  theme(legend.position = 'right', # change where the legend is
        axis.title.y=element_text(size=rel(2), colour = 'black'), # change y axis title properties
        axis.title.x=element_text(size=rel(2), colour = 'black'), # change x axis title properties
        axis.text.x=element_text(size=rel(2), colour = 'black'), # change x axis text properties
        axis.text.y=element_text(size=rel(2), colour = 'black'), # change y axis text properties
        panel.background = element_rect(fill = 'white', colour = 'black'), # change background panel colors
        panel.grid.major = element_line(colour = "grey"), # change backgrond color
        legend.background = element_blank(), # change background of legend
        legend.box.background = element_rect(colour = "black")) + # change box of legend
  geom_col(position = "dodge", col = 'black') + # plot the data as a barplot
  geom_errorbar(aes(ymin = prob - SE, ymax = prob + SE), position = position_dodge(0.9), width = 0.4) + # add error bars
  scale_fill_brewer(palette="Greys") + #use the greys palette (see: https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html)
  labs(fill = 'Salt treatment (ds/m)') + # change label for legend
  ylim(0, 1) + # change y-axis limits on graph
  ylab('Prob. of germination') +  # change label on y-axis
  xlab('Species') + # change label on x axis
  scale_x_discrete(labels=c("BG" = "B. gracilis", "BM" = "C. dactylon",
                          "LB" = "S. scoparium", "SG"= "B. curtipendula"))

## make tables

### percent nitrogen table
write.csv(Anova(percent_nitrogen_lmer), 'tables/percent_nitrogen_anova.csv')

### cn table
write.csv(Anova(cn_lmer), 'tables/cn_anova.csv')

### mass table
write.csv(Anova(mass_values_lmer), 'tables/mass_anova.csv')

### height table
write.csv(Anova(height_values_lmer), 'tables/height_anova.csv')

### total n table
write.csv(Anova(nitrogen_percent_total_lmer), 'tables/total_n_anova.csv')

### germination table
write.csv(Anova(germination_lm), 'tables/germination_anova.csv')


