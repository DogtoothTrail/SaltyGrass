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
library(gtable)
library(grid)
library(gridExtra)

## define a function for multi-panel plots
multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
}

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

## print data (for publication)
# write.csv(cn_data, '../Data/curated_from_analysis/cn_data.csv', row.names = F)
# write.csv(mass_data, '../Data/curated_from_analysis/mass_data.csv', row.names = F)
# write.csv(germination_data, '../Data/curated_from_analysis/germination_data.csv', row.names = F)

## hypothesis testing

### Does salinity affect nitrogen concentration and does this vary by species?
### Increasing salinity will reduce nitrogen concentration becuase XXXXXXXX

#### take a look at the top 6 rows
### head(cn_data)

#### create continuous treatment variable
cn_data$treatment_continuous = as.numeric(cn_data$treatment)

#### see what class each variable is
class(cn_data$species)
class(cn_data$treatment)
class(cn_data$organ)
class(cn_data$block)
class(cn_data$nitrogen_percent)

#### define independent variables as correct class (if necessary)

#### define the mixed effects model
# percent_nitrogen_lmer = lmer(log(nitrogen_percent) ~ treatment * species * organ + (1|block), 
#                              data = cn_data) # fit the model
# plot(resid(percent_nitrogen_lmer) ~ fitted(percent_nitrogen_lmer)) # check  the residuals
# summary(percent_nitrogen_lmer) # look at slope coefficients
# Anova(percent_nitrogen_lmer, test.statistic = 'F') # check statistical significance of main effects
# cld(emmeans(percent_nitrogen_lmer, ~treatment))
# cld(emmeans(percent_nitrogen_lmer, ~species))
# cld(emmeans(percent_nitrogen_lmer, ~treatment*species))
# cld(emmeans(percent_nitrogen_lmer, ~treatment*species), alpha = 0.1)
# cld(emmeans(percent_nitrogen_lmer, ~organ*species))

percent_nitrogen_lmer_continuous = lmer(log(nitrogen_percent) ~ treatment_continuous * species * organ + (1|block), 
                             data = cn_data) # fit the model
Anova(percent_nitrogen_lmer_continuous, test.statistic = 'F')
test(emtrends(percent_nitrogen_lmer_continuous, ~species, var = 'treatment_continuous'))
cld(emtrends(percent_nitrogen_lmer_continuous, ~species, var = 'treatment_continuous'))

# #### percent change for BG
# (exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species))[3,3]) - 
#     exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species))[1,3])) / 
#   exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species))[1,3])
# (exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species*organ))[19,4]) - 
#     exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species*organ))[17,4])) / 
#   exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species*organ))[17,4])
# (exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species*organ))[3,4]) - 
#     exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species*organ))[1,4])) / 
#   exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species*organ))[1,4])
# #### percent change for SG
# (exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species))[15,3]) - 
#     exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species))[13,3])) / 
#   exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species))[13,3])
# (exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species*organ))[31,4]) - 
#     exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species*organ))[29,4])) / 
#   exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species*organ))[29,4])
# (exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species*organ))[15,4]) - 
#     exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species*organ))[13,4])) / 
#   exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species*organ))[13,4])
# #### percent change for LB
# (exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species*organ))[27,4]) - 
#     exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species*organ))[25,4])) / 
#   exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species*organ))[25,4])
# (exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species*organ))[11,4]) - 
#     exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species*organ))[9,4])) / 
#   exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species*organ))[9,4])
# #### percent change for BM
# (exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species*organ))[23,4]) - 
#     exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species*organ))[21,4])) / 
#   exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species*organ))[21,4])
# (exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species*organ))[7,4]) - 
#     exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species*organ))[5,4])) / 
#   exp(summary(emmeans(percent_nitrogen_lmer, ~treatment*species*organ))[5,4])

#### create new variable for CN
cn_data$cn = cn_data$carbon_weight / cn_data$nitrogen_weight

#### define the mixed effects model
# cn_lmer = lmer(log(cn) ~ treatment * species * organ + (1|block), 
#                              data = cn_data) # fit the model
# plot(resid(cn_lmer) ~ fitted(cn_lmer)) # check  the residuals
# summary(cn_lmer) # look at slope coefficients
# Anova(cn_lmer, test.statistic = 'F') # check statistical significance of main effects
# cld(emmeans(cn_lmer, ~treatment)) # CN decreases with salt, interesting!
# cld(emmeans(cn_lmer, ~species))
# cld(emmeans(cn_lmer, ~treatment*species)) # SG is most sensitive
# cld(emmeans(cn_lmer, ~treatment*species), alpha = 0.1) # SG is most sensitive
# cld(emmeans(cn_lmer, ~organ*species))

cn_lmer_continuous = lmer(log(cn) ~ treatment_continuous * species * organ + (1|block), 
                                        data = cn_data) # fit the model
Anova(cn_lmer_continuous, test.statistic = 'F')
test(emtrends(cn_lmer_continuous, ~species, var = 'treatment_continuous'))
cld(emtrends(cn_lmer_continuous, ~species, var = 'treatment_continuous'))

#### percent change root
# (exp(summary(emmeans(cn_lmer, ~treatment*organ))[3, 3]) - 
#   exp(summary(emmeans(cn_lmer, ~treatment*organ))[1, 3])) /
#   exp(summary(emmeans(cn_lmer, ~treatment*organ))[1, 3])
# 
# #### percent change for BG
# (exp(summary(emmeans(cn_lmer, ~treatment*species))[3,3]) - 
#     exp(summary(emmeans(cn_lmer, ~treatment*species))[1,3])) / 
#   exp(summary(emmeans(cn_lmer, ~treatment*species))[1,3])
# (exp(summary(emmeans(cn_lmer, ~treatment*species*organ))[19,4]) - 
#     exp(summary(emmeans(cn_lmer, ~treatment*species*organ))[17,4])) / 
#   exp(summary(emmeans(cn_lmer, ~treatment*species*organ))[17,4])
# (exp(summary(emmeans(cn_lmer, ~treatment*species*organ))[3,4]) - 
#     exp(summary(emmeans(cn_lmer, ~treatment*species*organ))[1,4])) / 
#   exp(summary(emmeans(cn_lmer, ~treatment*species*organ))[1,4])
# #### percent change for SG
# (exp(summary(emmeans(cn_lmer, ~treatment*species))[15,3]) - 
#     exp(summary(emmeans(cn_lmer, ~treatment*species))[13,3])) / 
#   exp(summary(emmeans(cn_lmer, ~treatment*species))[13,3])
# (exp(summary(emmeans(cn_lmer, ~treatment*species*organ))[31,4]) - 
#     exp(summary(emmeans(cn_lmer, ~treatment*species*organ))[29,4])) / 
#   exp(summary(emmeans(cn_lmer, ~treatment*species*organ))[29,4])
# (exp(summary(emmeans(cn_lmer, ~treatment*species*organ))[15,4]) - 
#     exp(summary(emmeans(cn_lmer, ~treatment*species*organ))[13,4])) / 
#   exp(summary(emmeans(cn_lmer, ~treatment*species*organ))[13,4])
# #### percent change for LB
# (exp(summary(emmeans(cn_lmer, ~treatment*species*organ))[27,4]) - 
#     exp(summary(emmeans(cn_lmer, ~treatment*species*organ))[25,4])) / 
#   exp(summary(emmeans(cn_lmer, ~treatment*species*organ))[25,4])
# (exp(summary(emmeans(cn_lmer, ~treatment*species*organ))[11,4]) - 
#     exp(summary(emmeans(cn_lmer, ~treatment*species*organ))[9,4])) / 
#   exp(summary(emmeans(cn_lmer, ~treatment*species*organ))[9,4])
# #### percent change for BM
# (exp(summary(emmeans(cn_lmer, ~treatment*species*organ))[23,4]) - 
#     exp(summary(emmeans(cn_lmer, ~treatment*species*organ))[21,4])) / 
#   exp(summary(emmeans(cn_lmer, ~treatment*species*organ))[21,4])
# (exp(summary(emmeans(cn_lmer, ~treatment*species*organ))[7,4]) - 
#     exp(summary(emmeans(cn_lmer, ~treatment*species*organ))[5,4])) / 
#   exp(summary(emmeans(cn_lmer, ~treatment*species*organ))[5,4])

### head(mass_data)

#### mass data continuous treatment
mass_data$treatment_continuous = as.numeric(mass_data$treatment)

#### see what class each variable is
class(mass_data$species)
class(mass_data$block)
class(mass_data$treatment)
class(mass_data$mass)
class(mass_data$organ)

#### define independent variables as correct class (if necessary)
                                 
#### define the mixed effects model

# mass_values_lmer = lmer(log(mass + 0.001) ~ treatment * species * organ + (1|block), 
#                              data = mass_data)
# plot(resid(mass_values_lmer) ~ fitted (mass_values_lmer)) # slight horn shape here, need to transform
# summary(mass_values_lmer)
# Anova(mass_values_lmer, test.statistic = 'F')
# cld(emmeans(mass_values_lmer, ~treatment)) # mass decreases with salinity treatment
# cld(emmeans(mass_values_lmer, ~species)) # BM is heaviest
# cld(emmeans(mass_values_lmer, ~treatment * species)) # BG has highest sensitivity; others not strongly effected
# cld(emmeans(mass_values_lmer, ~treatment * species), alpha = 0.1) # BG has highest sensitivity; others not strongly effected
# cld(emmeans(mass_values_lmer, ~treatment, at = list(species = 'BG')))
# cld(emmeans(mass_values_lmer, ~treatment * organ))
# cld(emmeans(mass_values_lmer, ~treatment, at = list(organ = 'R')))
# cld(emmeans(mass_values_lmer, ~treatment, at = list(organ = 'S')))
# (exp(summary(emmeans(mass_values_lmer, ~treatment * organ))[3, 3]) - 
#     exp(summary(emmeans(mass_values_lmer, ~treatment * organ))[1, 3])) / 
#   exp(summary(emmeans(mass_values_lmer, ~treatment * organ))[1, 3])
# (exp(summary(emmeans(mass_values_lmer, ~treatment * organ))[7, 3]) - 
#     exp(summary(emmeans(mass_values_lmer, ~treatment * organ))[5, 3])) / 
#   exp(summary(emmeans(mass_values_lmer, ~treatment * organ))[5, 3])

mass_values_lmer_continuous = lmer(log(mass + 0.001) ~ treatment_continuous * species * organ + (1|block), 
                        data = mass_data)
Anova(mass_values_lmer_continuous, test_statistic = 'F')
test(emtrends(mass_values_lmer_continuous, ~species, var = 'treatment_continuous'))
cld(emtrends(mass_values_lmer_continuous, ~species, var = 'treatment_continuous'))

# ### biomass responses for BG
# (exp(summary(emmeans(mass_values_lmer, ~treatment*species))[3,3]) - 
#     exp(summary(emmeans(mass_values_lmer, ~treatment*species))[1,3])) / 
#   exp(summary(emmeans(mass_values_lmer, ~treatment*species))[1,3])
# (exp(summary(emmeans(mass_values_lmer, ~treatment*species*organ))[19,4]) - 
#     exp(summary(emmeans(mass_values_lmer, ~treatment*species*organ))[17,4])) / 
#   exp(summary(emmeans(mass_values_lmer, ~treatment*species*organ))[17,4])
# (exp(summary(emmeans(mass_values_lmer, ~treatment*species*organ))[3,4]) - 
#     exp(summary(emmeans(mass_values_lmer, ~treatment*species*organ))[1,4])) / 
#   exp(summary(emmeans(mass_values_lmer, ~treatment*species*organ))[1,4])
# ### biomass responses for BM
# (exp(summary(emmeans(mass_values_lmer, ~treatment*species))[7,3]) - 
#     exp(summary(emmeans(mass_values_lmer, ~treatment*species))[5,3])) / 
#   exp(summary(emmeans(mass_values_lmer, ~treatment*species))[5,3])
# ### biomass responses for LB
# (exp(summary(emmeans(mass_values_lmer, ~treatment*species))[11,3]) - 
#     exp(summary(emmeans(mass_values_lmer, ~treatment*species))[9,3])) / 
#   exp(summary(emmeans(mass_values_lmer, ~treatment*species))[9,3])
# ### biomass responses for SG
# (exp(summary(emmeans(mass_values_lmer, ~treatment*species))[15,3]) - 
#     exp(summary(emmeans(mass_values_lmer, ~treatment*species))[13,3])) / 
#   exp(summary(emmeans(mass_values_lmer, ~treatment*species))[13,3])
# 
# (exp(summary(emmeans(mass_values_lmer, ~treatment*species*organ))[31,4]) - 
#     exp(summary(emmeans(mass_values_lmer, ~treatment*species*organ))[29,4])) / 
#   exp(summary(emmeans(mass_values_lmer, ~treatment*species*organ))[29,4])
# (exp(summary(emmeans(mass_values_lmer, ~treatment*species*organ))[15,4]) - 
#     exp(summary(emmeans(mass_values_lmer, ~treatment*species*organ))[13,4])) / 
#   exp(summary(emmeans(mass_values_lmer, ~treatment*species*organ))[13,4])

###

#### see what class each variable is
# class(height_data$treatment)
# class(height_data$rep)
# class(height_data$block)
# class(height_data$species)
# class(height_data$Height_cm)

#### define independent variables as correct class (if necessary)

#### make new variable for index of repeated measure
##### don't need this, as height_data$Sample is the index

#### define the mixed effects model
# height_values_lmer = lmer(log(Height_cm) ~ treatment * species * (1|Sample) + (1|block), 
#                              data = height_data)
# plot(resid(height_values_lmer) ~ fitted(height_values_lmer)) # very skewed, need to transform
# summary(height_values_lmer)
# Anova(height_values_lmer, test.statistic = 'F')
# cld(emmeans(height_values_lmer, ~treatment)) # salt treatment are shorter
# cld(emmeans(height_values_lmer, ~species))
# cld(emmeans(height_values_lmer, ~treatment*species)) # BG is most sensitive, others not as much

### combine mass and percent N data to get whole plant N
# cn_mass_data = left_join(mass_data, cn_data, by.x = pot, by.y = sample_id)
# head(cn_mass_data)
# cn_mass_data$nitrogen_percent_total = (cn_mass_data$nitrogen_percent / 100) * cn_mass_data$mass
# 
# nitrogen_percent_total_lmer = lmer(log(nitrogen_percent_total) ~ treatment * species * organ + (1|block), 
#                         data = cn_mass_data)
# plot(resid(nitrogen_percent_total_lmer) ~ fitted (nitrogen_percent_total_lmer)) # slight horn shape here, need to transform
# summary(nitrogen_percent_total_lmer)
# Anova(nitrogen_percent_total_lmer, test.statistic = 'F')
# cld(emmeans(nitrogen_percent_total_lmer, ~treatment)) # nitrogen mass decreases with salinity treatment
# cld(emmeans(nitrogen_percent_total_lmer, ~species)) # BM has most
# cld(emmeans(nitrogen_percent_total_lmer, ~treatment * species)) # SG and BM least sensitive and have most

### does increasing salinity influence germination in any of the evaluated species

#### continuous germination data


#### check variable class
class(germination_data$species)
class(germination_data$treatment)
class(germination_data$germination)

#### make new variable for treatment factor
germination_data$treatment_factor = as.factor(germination_data$treatment)

#### fit model and analyze the results
# germination_lm = glm(germination ~ treatment_factor*species, data = germination_data, family = 'binomial')
# plot(resid(germination_lm) ~ fitted(germination_lm))
# Anova(germination_lm, test.statistic = 'F')
# summary(germination_lm)
# cld(emmeans(germination_lm, ~treatment_factor, at = list(species = 'BG')))
# cld(emmeans(germination_lm, ~treatment_factor, at = list(species = 'BM')))
# cld(emmeans(germination_lm, ~treatment_factor, at = list(species = 'LB')))
# cld(emmeans(germination_lm, ~treatment_factor, at = list(species = 'SG')))
# 
# germination_data_probs = add_probs(germination_data, germination_lm, q = 1, comparison = "=")
# 
# germination_data_probs_group = group_by(germination_data_probs, species, treatment_factor)
# summarise(germination_data_probs_group, mean_prob_equal_to1 = mean(prob_equal_to1), 
#           sd_prob_equal_to1 = sd(prob_equal_to1))
# 
# cld(emmeans(germination_lm, ~treatment_factor, type = "response"))
# cld(emmeans(germination_lm, ~treatment_factor*species, type = "response"))

germination_lm_continuous = glm(germination ~ treatment*species, data = germination_data, family = 'binomial')
Anova(germination_lm_continuous, test.statistic = 'F')
test(emtrends(germination_lm_continuous, ~species, var = 'treatment'))
cld(emtrends(germination_lm_continuous, ~species, var = 'treatment'))
cld(emmeans(germination_lm_continuous, ~species))

#### calculate change in germination probability for each species
# summary(emmeans(germination_lm, ~treatment_factor*species, type = "response"))[4, 3] - 
#   summary(emmeans(germination_lm, ~treatment_factor*species, type = "response"))[1, 3]
# summary(emmeans(germination_lm, ~treatment_factor*species, type = "response"))[16, 3] - 
#   summary(emmeans(germination_lm, ~treatment_factor*species, type = "response"))[13, 3]
# summary(emmeans(germination_lm, ~treatment_factor*species, type = "response"))[12, 3] - 
#   summary(emmeans(germination_lm, ~treatment_factor*species, type = "response"))[9, 3]
# summary(emmeans(germination_lm, ~treatment_factor*species, type = "response"))[8, 3] - 
#   summary(emmeans(germination_lm, ~treatment_factor*species, type = "response"))[5, 3]

## make plots

### mass plot (continuous)
test(emtrends(mass_values_lmer_continuous, ~species, var = 'treatment_continuous'))
mass_slopes = summary(emtrends(mass_values_lmer_continuous, ~species, var = 'treatment_continuous'))[,c(1,2)]
mass_intercepts = summary(emmeans(mass_values_lmer_continuous, ~species, 
                                  at = list(treatment_continuous = 0)))[,c(1,2)]
mass_trend_BG = exp(mass_intercepts[1,2] + mass_slopes[1,2] * seq(0, 24, 1))
mass_trend_BM = exp(mass_intercepts[2,2] + mass_slopes[2,2] * seq(0, 24, 1))
mass_trend_LB = exp(mass_intercepts[3,2] + mass_slopes[3,2] * seq(0, 24, 1))
mass_trend_SG = exp(mass_intercepts[4,2] + mass_slopes[4,2] * seq(0, 24, 1))

mass_trends = data.frame(seq(0, 24, 1), mass_trend_BG, mass_trend_BM, mass_trend_LB, mass_trend_SG)
colnames(mass_trends) = c('treatment', 'mass_trend_BG', 'mass_trend_BM', 'mass_trend_LB', 'mass_trend_SG')

mass_plot_continuous = ggplot(data = mass_data, 
                              aes(x = treatment_continuous, y = mass, 
                                  colour = species, shape = organ)) +
  theme(legend.position = 'right', 
        # legend.justification = c(1, 1), # change where the legend is
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.title.y=element_text(size=rel(2), colour = 'black'), # change y axis title properties
        axis.title.x=element_text(size=rel(2), colour = 'black'), # change x axis title properties
        axis.text.x=element_text(size=rel(2), colour = 'black'), # change x axis text properties
        axis.text.y=element_text(size=rel(2), colour = 'black'), # change y axis text properties
        panel.background = element_rect(fill = 'white', colour = 'black'), # change background panel colors
        panel.grid.major = element_line(colour = "grey"), # change backgrond color
        legend.background = element_blank(), # change background of legend
        legend.box.background = element_rect(colour = "black"),
        plot.tag = element_text(size = 30),
        legend.text.align = 0) + # change box of legend
  scale_colour_manual(values = c('red', 'blue', 'orange', 'purple'), 
                      name = "Species",
                      labels=c("BG" = expression(italic("B. gracilis")), 
                                        "BM" = expression(italic("C. dactylon")),
                                        "LB" = expression(italic("S. scoparium")), 
                                        "SG"= expression(italic("B. curtipendula")))) +
  scale_shape_manual(values = c(17, 15), 
                     name = "Organ",
                     labels = c("R" = "Root", "S" = "Shoot")) +
  geom_jitter(height = 0, width = 1, size = 4, alpha = 0.5) +
  geom_line(data = mass_trends, aes(x = treatment, y = mass_trend_BG, shape = NULL, colour = NULL), 
            colour = 'red', size = 4, lty = 1) +
  geom_line(data = mass_trends, aes(x = treatment, y = mass_trend_BM, shape = NULL, colour = NULL), 
            colour = 'blue', size = 4, lty = 2) +
  geom_line(data = mass_trends, aes(x = treatment, y = mass_trend_LB, shape = NULL, colour = NULL), 
            colour = 'orange', size = 4, lty = 1) +
  geom_line(data = mass_trends, aes(x = treatment, y = mass_trend_SG, shape = NULL, colour = NULL), 
            colour = 'purple', size = 4, lty = 1) +
  ylab('Mass (g)') +
  xlab('Salinity treatment (dS/m)') +
  ylim(c(0, 8))

jpeg(filename = "plots/revised/revised2/Figure2.jpeg", width = 8, height = 8, 
     units = 'in', res = 600)
plot(mass_plot_continuous)
dev.off()

### mass plot
# mass_data$salt_treatment = factor(mass_data$treatment, levels = c('0', '8', '16', '24'), ordered = T) 
# mass_plot_shoot = ggplot(data = subset(mass_data, organ == 'S'), aes(x = species, y = mass, fill = salt_treatment)) + 
#   theme(legend.position = c(1, 1), 
#         legend.justification = c(1, 1), # change where the legend is
#         legend.text = element_text(size = 16),
#         legend.title = element_text(size = 16),
#         axis.title.y=element_text(size=rel(2), colour = 'black'), # change y axis title properties
#         axis.title.x=element_text(size=rel(2), colour = 'black'), # change x axis title properties
#         axis.text.x=element_text(size=rel(2), colour = 'black'), # change x axis text properties
#         axis.text.y=element_text(size=rel(2), colour = 'black'), # change y axis text properties
#         panel.background = element_rect(fill = 'white', colour = 'black'), # change background panel colors
#         panel.grid.major = element_line(colour = "grey"), # change backgrond color
#         legend.background = element_blank(), # change background of legend
#         legend.box.background = element_rect(colour = "black"),
#         plot.tag = element_text(size = 30)) + # change box of legend
#   geom_boxplot() + # plot the data as a boxplot
#   scale_fill_brewer(palette="Greys") + #use the greys palette (see: https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html)
#   labs(fill = 'Salinity treatment (dS/m)') + # change label for legend
#   ylim(0, 8) + # change y-axis limits on graph
#   ylab('Shoot mass (g)') +  # change label on y-axis
#   xlab('') + # change label on x axis 
#   scale_x_discrete(limits = c("BM", "SG", "BG", "LB"),
#                    labels=c("BG" = expression(italic("B. gracilis")), 
#                             "BM" = expression(italic("C. dactylon")),
#                             "LB" = expression(italic("S. scoparium")), 
#                             "SG"= expression(italic("B. curtipendula")))) +
#   labs(tag = "A")
# 
# mass_plot_root = ggplot(data = subset(mass_data, organ == 'R'), aes(x = species, y = mass, fill = salt_treatment)) + 
#   theme(legend.position = 'none', # change where the legend is
#         axis.title.y=element_text(size=rel(2), colour = 'black'), # change y axis title properties
#         axis.title.x=element_text(size=rel(2), colour = 'black'), # change x axis title properties
#         axis.text.x=element_text(size=rel(2), colour = 'black'), # change x axis text properties
#         axis.text.y=element_text(size=rel(2), colour = 'black'), # change y axis text properties
#         panel.background = element_rect(fill = 'white', colour = 'black'), # change background panel colors
#         panel.grid.major = element_line(colour = "grey"), # change backgrond color
#         legend.background = element_blank(), # change background of legend
#         legend.box.background = element_rect(colour = "black"),
#         plot.tag = element_text(size = 30)) + # change box of legend
#   geom_boxplot() + # plot the data as a boxplot
#   scale_fill_brewer(palette="Greys") + #use the greys palette (see: https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html)
#   labs(fill = 'Salt treatment (ds/m)') + # change label for legend
#   ylim(0, 4) + # change y-axis limits on graph
#   ylab('Root mass (g)') +  # change label on y-axis
#   xlab('Species') + # change label on x axis 
#   scale_x_discrete(limits = c("BM", "SG", "BG", "LB"),
#                    labels=c("BG" = expression(italic("B. gracilis")), 
#                             "BM" = expression(italic("C. dactylon")),
#                             "LB" = expression(italic("S. scoparium")), 
#                             "SG"= expression(italic("B. curtipendula")))) +
#   labs(tag = "B")
# 
# 
# jpeg(filename = "plots/revised/Figure2.jpeg", width = 8, height = 12, units = 'in', res = 600)
# multiplot(mass_plot_shoot, mass_plot_root, cols=1)
# dev.off()

### height plot
# height_data$salt_treatment = factor(height_data$treatment, levels = c('0', '8', '16', '24'), ordered = T) 
# height_plot = ggplot(data = height_data, aes(x = species, y = Height_cm, fill = salt_treatment)) + 
#   theme(legend.position = c(0.84, 0.8), # change where the legend is
#         axis.title.y=element_text(size=rel(2), colour = 'black'), # change y axis title properties
#         axis.title.x=element_text(size=rel(2), colour = 'black'), # change x axis title properties
#         axis.text.x=element_text(size=rel(2), colour = 'black'), # change x axis text properties
#         axis.text.y=element_text(size=rel(2), colour = 'black'), # change y axis text properties
#         panel.background = element_rect(fill = 'white', colour = 'black'), # change background panel colors
#         panel.grid.major = element_line(colour = "grey"), # change backgrond color
#         legend.background = element_blank(), # change background of legend
#         legend.box.background = element_rect(colour = "black")) + # change box of legend
#   geom_boxplot() + # plot the data as a boxplot
#   scale_fill_brewer(palette="Greys") + #use the greys palette (see: https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html)
#   labs(fill = 'Salt treatment (ds/m)') + # change label for legend
#   ylim(0, 120) + # change y-axis limits on graph
#   ylab('Height (cm)') +  # change label on y-axis
#   xlab('Species') + # change label on x axis
#   scale_x_discrete(labels=c("BG" = expression(italic("B. gracilis")), 
#                             "BM" = expression(italic("C. dactylon")),
#                             "LB" = expression(italic("S. scoparium")), 
#                             "SG"= expression(italic("B. curtipendula"))))
# 
# jpeg(filename = "plots/height_plot.jpeg", width = 600, height = 450, units = 'px')
# print(height_plot)
# dev.off()

### percent N plot
####continuous
test(emtrends(percent_nitrogen_lmer_continuous, ~species, var = 'treatment_continuous'))
percent_nitrogen_slopes = summary(emtrends(percent_nitrogen_lmer_continuous, ~species, var = 'treatment_continuous'))[,c(1,2)]
percent_nitrogen_intercepts = summary(emmeans(percent_nitrogen_lmer_continuous, ~species, 
                                              at = list(treatment_continuous = 0)))[,c(1,2)]
percent_nitrogen_trend_BG = exp(percent_nitrogen_intercepts[1,2] + percent_nitrogen_slopes[1,2] * seq(0, 24, 1))
percent_nitrogen_trend_BM = exp(percent_nitrogen_intercepts[2,2] + percent_nitrogen_slopes[2,2] * seq(0, 24, 1))
percent_nitrogen_trend_LB = exp(percent_nitrogen_intercepts[3,2] + percent_nitrogen_slopes[3,2] * seq(0, 24, 1))
percent_nitrogen_trend_SG = exp(percent_nitrogen_intercepts[4,2] + percent_nitrogen_slopes[4,2] * seq(0, 24, 1))

percent_nitrogen_trends = data.frame(seq(0, 24, 1), percent_nitrogen_trend_BG, percent_nitrogen_trend_BM, 
                                     percent_nitrogen_trend_LB, percent_nitrogen_trend_SG)
colnames(percent_nitrogen_trends) = c('treatment', 'percent_nitrogen_trend_BG', 'percent_nitrogen_trend_BM', 
                                      'percent_nitrogen_trend_LB', 'percent_nitrogen_trend_SG')

percent_nitrogen_plot_continuous = ggplot(data = cn_data, 
                                          aes(x = treatment_continuous, y = nitrogen_percent, 
                                              colour = species, shape = organ)) +
  theme(legend.position = 'right', 
        # legend.justification = c(1, 1), # change where the legend is
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.title.y=element_text(size=rel(2), colour = 'black'), # change y axis title properties
        axis.title.x=element_text(size=rel(2), colour = 'black'), # change x axis title properties
        axis.text.x=element_text(size=rel(2), colour = 'black'), # change x axis text properties
        axis.text.y=element_text(size=rel(2), colour = 'black'), # change y axis text properties
        panel.background = element_rect(fill = 'white', colour = 'black'), # change background panel colors
        panel.grid.major = element_line(colour = "grey"), # change backgrond color
        legend.background = element_blank(), # change background of legend
        legend.box.background = element_rect(colour = "black"),
        plot.tag = element_text(size = 30),
        legend.text.align = 0) + # change box of legend
  scale_colour_manual(values = c('red', 'blue', 'orange', 'purple'), 
                      name = "Species",
                      labels=c("BG" = expression(italic("B. gracilis")), 
                               "BM" = expression(italic("C. dactylon")),
                               "LB" = expression(italic("S. scoparium")), 
                               "SG"= expression(italic("B. curtipendula")))) +
  scale_shape_manual(values = c(17, 15), 
                     name = "Organ",
                     labels = c("R" = "Root", "S" = "Shoot")) +
  geom_jitter(height = 0, width = 1, size = 4, alpha = 0.5) +
  geom_line(data = percent_nitrogen_trends, aes(x = treatment, y = percent_nitrogen_trend_BG, shape = NULL, colour = NULL), 
            colour = 'red', size = 4, lty = 1) +
  geom_line(data = percent_nitrogen_trends, aes(x = treatment, y = percent_nitrogen_trend_BM, shape = NULL, colour = NULL), 
            colour = 'blue', size = 4, lty = 2) +
  geom_line(data = percent_nitrogen_trends, aes(x = treatment, y = percent_nitrogen_trend_LB, shape = NULL, colour = NULL), 
            colour = 'orange', size = 4, lty = 2) +
  geom_line(data = percent_nitrogen_trends, aes(x = treatment, y = percent_nitrogen_trend_SG, shape = NULL, colour = NULL), 
            colour = 'purple', size = 4, lty = 1) +
  ylab('Nitrogen (%)') +
  xlab('Salinity treatment (dS/m)') +
  ylim(c(0, 6)) +
  labs(tag = "A")

test(emtrends(cn_lmer_continuous, ~species, var = 'treatment_continuous'))
cn_slopes = summary(emtrends(cn_lmer_continuous, ~species, var = 'treatment_continuous'))[,c(1,2)]
cn_intercepts = summary(emmeans(cn_lmer_continuous, ~species, 
                                at = list(treatment_continuous = 0)))[,c(1,2)]
cn_trend_BG = exp(cn_intercepts[1,2] + cn_slopes[1,2] * seq(0, 24, 1))
cn_trend_BM = exp(cn_intercepts[2,2] + cn_slopes[2,2] * seq(0, 24, 1))
cn_trend_LB = exp(cn_intercepts[3,2] + cn_slopes[3,2] * seq(0, 24, 1))
cn_trend_SG = exp(cn_intercepts[4,2] + cn_slopes[4,2] * seq(0, 24, 1))

cn_trends = data.frame(seq(0, 24, 1), cn_trend_BG, cn_trend_BM, 
                       cn_trend_LB, cn_trend_SG)
colnames(cn_trends) = c('treatment', 'cn_trend_BG', 'cn_trend_BM', 
                        'cn_trend_LB', 'cn_trend_SG')

cn_plot_continuous = ggplot(data = cn_data, 
                            aes(x = treatment_continuous, y = cn, 
                                colour = species, shape = organ)) +
  theme(legend.position = 'right', 
        # legend.justification = c(1, 1), # change where the legend is
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.title.y=element_text(size=rel(2), colour = 'black'), # change y axis title properties
        axis.title.x=element_text(size=rel(2), colour = 'black'), # change x axis title properties
        axis.text.x=element_text(size=rel(2), colour = 'black'), # change x axis text properties
        axis.text.y=element_text(size=rel(2), colour = 'black'), # change y axis text properties
        panel.background = element_rect(fill = 'white', colour = 'black'), # change background panel colors
        panel.grid.major = element_line(colour = "grey"), # change backgrond color
        legend.background = element_blank(), # change background of legend
        legend.box.background = element_rect(colour = "black"),
        plot.tag = element_text(size = 30),
        legend.text.align = 0) + # change box of legend
  scale_colour_manual(values = c('red', 'blue', 'orange', 'purple'), 
                      name = "Species",
                      labels=c("BG" = expression(italic("B. gracilis")), 
                               "BM" = expression(italic("C. dactylon")),
                               "LB" = expression(italic("S. scoparium")), 
                               "SG"= expression(italic("B. curtipendula")))) +
  scale_shape_manual(values = c(17, 15), 
                     name = "Organ",
                     labels = c("R" = "Root", "S" = "Shoot")) +
  geom_jitter(height = 0, width = 1, size = 4, alpha = 0.5) +
  geom_line(data = cn_trends, aes(x = treatment, y = cn_trend_BG, shape = NULL, colour = NULL), 
            colour = 'red', size = 4, lty = 1) +
  geom_line(data = cn_trends, aes(x = treatment, y = cn_trend_BM, shape = NULL, colour = NULL), 
            colour = 'blue', size = 4, lty = 2) +
  geom_line(data = cn_trends, aes(x = treatment, y = cn_trend_LB, shape = NULL, colour = NULL), 
            colour = 'orange', size = 4, lty = 2) +
  geom_line(data = cn_trends, aes(x = treatment, y = cn_trend_SG, shape = NULL, colour = NULL), 
            colour = 'purple', size = 4, lty = 1) +
  ylab('C:N') +
  xlab('Salinity treatment (dS/m)') +
  ylim(c(0, 120)) +
  labs(tag = "B")

jpeg(filename = "plots/revised/revised2/Figure3.jpeg", width = 9, height = 16, 
     units = 'in', res = 600)
multiplot(percent_nitrogen_plot_continuous, cn_plot_continuous, cols = 1)
dev.off()

# 
# 
# 
# 
# cn_data$salt_treatment = factor(cn_data$treatment, levels = c('0', '8', '16', '24'), ordered = T) 
# percent_nitrogen_plot_shoot = ggplot(data = subset(cn_data, organ == 'S'), aes(x = species, y = nitrogen_percent, fill = salt_treatment)) + 
#   theme(legend.position = c(0, 1), # change where the legend is
#         legend.justification = c(0, 1), # change where the legend is
#         legend.text = element_text(size = 16),
#         legend.title = element_text(size = 16),
#         axis.title.y=element_text(size=rel(2), colour = 'black'), # change y axis title properties
#         axis.title.x=element_text(size=rel(2), colour = 'black'), # change x axis title properties
#         axis.text.x=element_text(size=rel(2), colour = 'black'), # change x axis text properties
#         axis.text.y=element_text(size=rel(2), colour = 'black'), # change y axis text properties
#         panel.background = element_rect(fill = 'white', colour = 'black'), # change background panel colors
#         panel.grid.major = element_line(colour = "grey"), # change backgrond color
#         legend.background = element_blank(), # change background of legend
#         legend.box.background = element_rect(colour = "black"),
#         plot.tag = element_text(size = 30)) + # change box of legend
#   geom_boxplot() + # plot the data as a boxplot
#   scale_fill_brewer(palette="Greys") + #use the greys palette (see: https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html)
#   labs(fill = 'Salinity treatment (dS/m)') + # change label for legend
#   ylim(0, 3) + # change y-axis limits on graph
#   ylab('Shoot N (%)') +  # change label on y-axis
#   xlab('') + # change label on x axis
#   scale_x_discrete(limits = c("BM", "SG", "BG", "LB"),
#                    labels=c("BG" = expression(italic("B. gracilis")), 
#                             "BM" = expression(italic("C. dactylon")),
#                             "LB" = expression(italic("S. scoparium")), 
#                             "SG"= expression(italic("B. curtipendula")))) +
#   labs(tag = "A")
# 
# percent_nitrogen_plot_root = ggplot(data = subset(cn_data, organ == 'R'), aes(x = species, y = nitrogen_percent, fill = salt_treatment)) + 
#   theme(legend.position = 'none', # change where the legend is
#         axis.title.y=element_text(size=rel(2), colour = 'black'), # change y axis title properties
#         axis.title.x=element_text(size=rel(2), colour = 'black'), # change x axis title properties
#         axis.text.x=element_text(size=rel(2), colour = 'black'), # change x axis text properties
#         axis.text.y=element_text(size=rel(2), colour = 'black'), # change y axis text properties
#         panel.background = element_rect(fill = 'white', colour = 'black'), # change background panel colors
#         panel.grid.major = element_line(colour = "grey"), # change backgrond color
#         legend.background = element_blank(), # change background of legend
#         legend.box.background = element_rect(colour = "black"),
#         plot.tag = element_text(size = 30)) + # change box of legend
#   geom_boxplot() + # plot the data as a boxplot
#   scale_fill_brewer(palette="Greys") + #use the greys palette (see: https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html)
#   labs(fill = 'Salt treatment (ds/m)') + # change label for legend
#   ylim(0, 3) + # change y-axis limits on graph
#   ylab('Root N (%)') +  # change label on y-axis
#   xlab('') + # change label on x axis
#   scale_x_discrete(limits = c("BM", "SG", "BG", "LB"),
#                    labels=c("BG" = expression(italic("B. gracilis")), 
#                             "BM" = expression(italic("C. dactylon")),
#                             "LB" = expression(italic("S. scoparium")), 
#                             "SG"= expression(italic("B. curtipendula")))) +
#   labs(tag = "B")
# 
# # jpeg(filename = "plots/percent_nitrogen_plot.jpeg", width = 600, height = 900, units = 'px')
# # multiplot(percent_nitrogen_plot_shoot, percent_nitrogen_plot_root, cols=1)
# # dev.off()

# ### CN plot
# cn_data$salt_treatment = factor(cn_data$treatment, levels = c('0', '8', '16', '24'), ordered = T) 
# cn_plot_shoot = ggplot(data = subset(cn_data, organ == 'S'), aes(x = species, y = cn, fill = salt_treatment)) + 
#   theme(legend.position = 'none', # change where the legend is
#         axis.title.y=element_text(size=rel(2), colour = 'black'), # change y axis title properties
#         axis.title.x=element_text(size=rel(2), colour = 'black'), # change x axis title properties
#         axis.text.x=element_text(size=rel(2), colour = 'black'), # change x axis text properties
#         axis.text.y=element_text(size=rel(2), colour = 'black'), # change y axis text properties
#         panel.background = element_rect(fill = 'white', colour = 'black'), # change background panel colors
#         panel.grid.major = element_line(colour = "grey"), # change backgrond color
#         legend.background = element_blank(), # change background of legend
#         legend.box.background = element_rect(colour = "black"),
#         plot.tag = element_text(size = 30)) + # change box of legend
#   geom_boxplot() + # plot the data as a boxplot
#   scale_fill_brewer(palette="Greys") + #use the greys palette (see: https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html)
#   labs(fill = 'Salt treatment (ds/m)') + # change label for legend
#   ylim(0, 100) + # change y-axis limits on graph
#   ylab('Shoot C:N') +  # change label on y-axis
#   xlab('Species') + # change label on x axis
#   scale_x_discrete(limits = c("BM", "SG", "BG", "LB"),
#                    labels=c("BG" = expression(italic("B. gracilis")), 
#                             "BM" = expression(italic("C. dactylon")),
#                             "LB" = expression(italic("S. scoparium")), 
#                             "SG"= expression(italic("B. curtipendula")))) +
#   labs(tag = "C")
# 
# cn_plot_root = ggplot(data = subset(cn_data, organ == 'R'), aes(x = species, y = cn, fill = salt_treatment)) + 
#   theme(legend.position = 'none', # change where the legend is
#         axis.title.y=element_text(size=rel(2), colour = 'black'), # change y axis title properties
#         axis.title.x=element_text(size=rel(2), colour = 'black'), # change x axis title properties
#         axis.text.x=element_text(size=rel(2), colour = 'black'), # change x axis text properties
#         axis.text.y=element_text(size=rel(2), colour = 'black'), # change y axis text properties
#         panel.background = element_rect(fill = 'white', colour = 'black'), # change background panel colors
#         panel.grid.major = element_line(colour = "grey"), # change backgrond color
#         legend.background = element_blank(), # change background of legend
#         legend.box.background = element_rect(colour = "black"),
#         plot.tag = element_text(size = 30)) + # change box of legend
#   geom_boxplot() + # plot the data as a boxplot
#   scale_fill_brewer(palette="Greys") + #use the greys palette (see: https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html)
#   labs(fill = 'Salt treatment (ds/m)') + # change label for legend
#   ylim(0, 100) + # change y-axis limits on graph
#   ylab('Root C:N') +  # change label on y-axis
#   xlab('Species') + # change label on x axis
#   scale_x_discrete(limits = c("BM", "SG", "BG", "LB"),
#                    labels=c("BG" = expression(italic("B. gracilis")), 
#                             "BM" = expression(italic("C. dactylon")),
#                             "LB" = expression(italic("S. scoparium")), 
#                             "SG"= expression(italic("B. curtipendula")))) +
#   labs(tag = "D")
# 
# # jpeg(filename = "plots/cn_plot.jpeg", width = 600, height = 900, units = 'px')
# # multiplot(cn_plot_shoot, cn_plot_root, cols=1)
# # dev.off()
# 
# jpeg(filename = "plots/revised/Figure3_v2.jpeg", width = 16, height = 14, units = 'in', res = 600)
# multiplot(percent_nitrogen_plot_shoot, percent_nitrogen_plot_root, 
#           cn_plot_shoot, cn_plot_root, cols=2)
# dev.off()

### mass N plot
# cn_mass_data$salt_treatment = factor(cn_mass_data$treatment, levels = c('0', '8', '16', '24'), ordered = T) 
# mass_n_plot_shoot = ggplot(data = subset(cn_mass_data, organ == 'S'), aes(x = species, y = nitrogen_percent_total, fill = salt_treatment)) + 
#   theme(legend.position = c(0.84, 0.8), # change where the legend is
#         axis.title.y=element_text(size=rel(2), colour = 'black'), # change y axis title properties
#         axis.title.x=element_text(size=rel(2), colour = 'black'), # change x axis title properties
#         axis.text.x=element_text(size=rel(2), colour = 'black'), # change x axis text properties
#         axis.text.y=element_text(size=rel(2), colour = 'black'), # change y axis text properties
#         panel.background = element_rect(fill = 'white', colour = 'black'), # change background panel colors
#         panel.grid.major = element_line(colour = "grey"), # change backgrond color
#         legend.background = element_blank(), # change background of legend
#         legend.box.background = element_rect(colour = "black")) + # change box of legend
#   geom_boxplot() + # plot the data as a boxplot
#   scale_fill_brewer(palette="Greys") + #use the greys palette (see: https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html)
#   labs(fill = 'Salt treatment (ds/m)') + # change label for legend
#   ylim(0, 0.1) + # change y-axis limits on graph
#   ylab('Shoot mass N (g)') +  # change label on y-axis
#   xlab('') + # change label on x axis
#   scale_x_discrete(labels=c("BG" = expression(italic("B. gracilis")), 
#                             "BM" = expression(italic("C. dactylon")),
#                             "LB" = expression(italic("S. scoparium")), 
#                             "SG"= expression(italic("B. curtipendula"))))
# 
# mass_n_plot_root = ggplot(data = subset(cn_mass_data, organ == 'R'), aes(x = species, y = nitrogen_percent_total, fill = salt_treatment)) + 
#   theme(legend.position = 'none', # change where the legend is
#         axis.title.y=element_text(size=rel(2), colour = 'black'), # change y axis title properties
#         axis.title.x=element_text(size=rel(2), colour = 'black'), # change x axis title properties
#         axis.text.x=element_text(size=rel(2), colour = 'black'), # change x axis text properties
#         axis.text.y=element_text(size=rel(2), colour = 'black'), # change y axis text properties
#         panel.background = element_rect(fill = 'white', colour = 'black'), # change background panel colors
#         panel.grid.major = element_line(colour = "grey"), # change backgrond color
#         legend.background = element_blank(), # change background of legend
#         legend.box.background = element_rect(colour = "black")) + # change box of legend
#   geom_boxplot() + # plot the data as a boxplot
#   scale_fill_brewer(palette="Greys") + #use the greys palette (see: https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html)
#   labs(fill = 'Salt treatment (ds/m)') + # change label for legend
#   ylim(0, 0.02) + # change y-axis limits on graph
#   ylab('Root mass N (g)') +  # change label on y-axis
#   xlab('Species') + # change label on x axis
#   scale_x_discrete(labels=c("BG" = expression(italic("B. gracilis")), 
#                             "BM" = expression(italic("C. dactylon")),
#                             "LB" = expression(italic("S. scoparium")), 
#                             "SG"= expression(italic("B. curtipendula"))))
# 
# jpeg(filename = "plots/mass_n_plot.jpeg", width = 600, height = 900, units = 'px')
# multiplot(mass_n_plot_shoot, mass_n_plot_root, cols=1)
# dev.off()

### germination plot
Anova(germination_lm_continuous, test.statistic = 'F')
test(emtrends(germination_lm_continuous, ~species, var = 'treatment', type = 'response'))
# germination_slopes = summary(emtrends(germination_lm_continuous, ~species, 
#                                       var = 'treatment', type = 'response'))[,c(1,2)]
# germination_intercepts = summary(emmeans(germination_lm_continuous, ~species, 
#                                               at = list(treatment_continuous = 0), type = 'response'))[,c(1,2)]
# germination_trend_BG = (germination_intercepts[1,2] + germination_slopes[1,2] * seq(0, 24, 1))
# germination_trend_BM = (germination_intercepts[2,2] + germination_slopes[2,2] * seq(0, 24, 1))
# germination_trend_LB = (germination_intercepts[3,2] + germination_slopes[3,2] * seq(0, 24, 1))
# germination_trend_SG = (germination_intercepts[4,2] + germination_slopes[4,2] * seq(0, 24, 1))
# 
# germination_trends = data.frame(seq(0, 24, 1), germination_trend_BG, germination_trend_BM, 
#                                 germination_trend_LB, germination_trend_SG)
# colnames(germination_trends) = c('treatment', 'germination_trend_BG', 'germination_trend_BM', 
#                                       'germination_trend_LB', 'germination_trend_SG')

# germination_plot_continuous = ggplot(data = mass_data, 
#                               aes(x = germination_data, y = germination, 
#                                   colour = species, shape = organ)) +
#   theme(legend.position = 'right', 
#         # legend.justification = c(1, 1), # change where the legend is
#         legend.text = element_text(size = 16),
#         legend.title = element_text(size = 16),
#         axis.title.y=element_text(size=rel(2), colour = 'black'), # change y axis title properties
#         axis.title.x=element_text(size=rel(2), colour = 'black'), # change x axis title properties
#         axis.text.x=element_text(size=rel(2), colour = 'black'), # change x axis text properties
#         axis.text.y=element_text(size=rel(2), colour = 'black'), # change y axis text properties
#         panel.background = element_rect(fill = 'white', colour = 'black'), # change background panel colors
#         panel.grid.major = element_line(colour = "grey"), # change backgrond color
#         legend.background = element_blank(), # change background of legend
#         legend.box.background = element_rect(colour = "black"),
#         plot.tag = element_text(size = 30),
#         legend.text.align = 0) + # change box of legend
#   scale_colour_manual(values = c('red', 'blue', 'orange', 'purple'), 
#                       name = "Species",
#                       labels=c("BG" = expression(italic("B. gracilis")), 
#                                "BM" = expression(italic("C. dactylon")),
#                                "LB" = expression(italic("S. scoparium")), 
#                                "SG"= expression(italic("B. curtipendula")))) +
#   scale_shape_manual(values = c(17, 15), 
#                      name = "Organ",
#                      labels = c("R" = "Root", "S" = "Shoot")) +
#   # geom_jitter(height = 0, width = 1, size = 4, alpha = 0.5) +
#   geom_line(data = germination_trends, aes(x = treatment, y = germination_trend_BG, shape = NULL, colour = NULL), 
#             colour = 'red', size = 4, lty = 1) +
#   geom_line(data = germination_trends, aes(x = treatment, y = germination_trend_BM, shape = NULL, colour = NULL), 
#             colour = 'blue', size = 4, lty = 2) +
#   geom_line(data = germination_trends, aes(x = treatment, y = germination_trend_LB, shape = NULL, colour = NULL), 
#             colour = 'orange', size = 4, lty = 1) +
#   geom_line(data = germination_trends, aes(x = treatment, y = germination_trend_SG, shape = NULL, colour = NULL), 
#             colour = 'purple', size = 4, lty = 1) +
#   ylab('Probability of germination') +
#   xlab('Salinity treatment (dS/m)') +
#   ylim(c(0, 1))

germination_plot_continuous = ggplot(data = germination_data, 
                                     aes(y = germination, x = treatment, colour = species,
                                         linetype = species)) +
  theme(legend.position = 'right', 
        # legend.justification = c(1, 1), # change where the legend is
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.title.y=element_text(size=rel(2), colour = 'black'), # change y axis title properties
        axis.title.x=element_text(size=rel(2), colour = 'black'), # change x axis title properties
        axis.text.x=element_text(size=rel(2), colour = 'black'), # change x axis text properties
        axis.text.y=element_text(size=rel(2), colour = 'black'), # change y axis text properties
        panel.background = element_rect(fill = 'white', colour = 'black'), # change background panel colors
        panel.grid.major = element_line(colour = "grey"), # change backgrond color
        legend.background = element_blank(), # change background of legend
        legend.box.background = element_rect(colour = "black"),
        plot.tag = element_text(size = 30),
        legend.text.align = 0) +
  scale_colour_manual(values = c('red', 'blue', 'orange', 'purple'), 
                      name = "Species",
                      labels=c("BG" = expression(italic("B. gracilis")), 
                               "BM" = expression(italic("C. dactylon")),
                               "LB" = expression(italic("S. scoparium")), 
                               "SG"= expression(italic("B. curtipendula")))) +
  scale_linetype_manual(values = c(1, 2, 2, 2), guide = 'none') +
  geom_jitter(height = 0, width = 1, alpha = 0.3, pch = 4, size = 6) +
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE, lwd = 2) +
  xlab('Salinity treatment (dS/m)') +
  ylab('Prob. of germination')
  

jpeg(filename = "plots/revised/revised2/Figure1.jpeg", width = 10, height = 8, 
     units = 'in', res = 600)
plot(germination_plot_continuous)
dev.off()








# germination_probs = summary(emmeans(germination_lm, ~treatment*species, type = "response"))
# germination_probs$salt_treatment = factor(germination_probs$treatment, levels = c('0', '8', '16', '24'), ordered = T) 
# germination_plot = ggplot(data = germination_probs, aes(x = species, y = prob, fill = salt_treatment)) + 
#   theme(legend.position = 'right', # change where the legend is
#         axis.title.y=element_text(size=rel(2), colour = 'black'), # change y axis title properties
#         axis.title.x=element_text(size=rel(2), colour = 'black'), # change x axis title properties
#         axis.text.x=element_text(size=rel(2), colour = 'black'), # change x axis text properties
#         axis.text.y=element_text(size=rel(2), colour = 'black'), # change y axis text properties
#         panel.background = element_rect(fill = 'white', colour = 'black'), # change background panel colors
#         panel.grid.major = element_line(colour = "grey"), # change backgrond color
#         legend.background = element_blank(), # change background of legend
#         legend.box.background = element_rect(colour = "black")) + # change box of legend
#   geom_col(position = "dodge", col = 'black') + # plot the data as a barplot
#   geom_errorbar(aes(ymin = prob - SE, ymax = prob + SE), position = position_dodge(0.9), width = 0.4) + # add error bars
#   scale_fill_brewer(palette="Greys") + #use the greys palette (see: https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html)
#   labs(fill = 'Salt treatment (dS/m)') + # change label for legend
#   ylim(0, 1) + # change y-axis limits on graph
#   ylab('Prob. of germination') +  # change label on y-axis
#   xlab('Species') +   # change label on x axis
#   # scale_x_discrete(limits = rev(levels(treatment_factor*species))) +
#   scale_x_discrete(limits = c("BM", "SG", "BG", "LB"),
#                    labels=c("BG" = expression(italic("B. gracilis")), 
#                             "BM" = expression(italic("C. dactylon")), 
#                             "LB" = expression(italic("S. scoparium")), 
#                             "SG" = expression(italic("B. curtipendula"))))
#                   
# 
# jpeg(filename = "plots/revised/Figure1.jpeg", width = 12, height = 7, units = 'in', res = 600)
# plot(germination_plot)
# dev.off()

## make tables

### percent nitrogen table
write.csv(Anova(percent_nitrogen_lmer_continuous, test.statistic = 'F'), 
          'tables/revised2/percent_nitrogen_anova_F.csv')

### cn table
write.csv(Anova(cn_lmer_continuous, test.statistic = 'F'), 
          'tables/revised2/cn_anova_F.csv')

### mass table
write.csv(Anova(mass_values_lmer_continuous, test.statistic = 'F'), 
          'tables/revised2/mass_anova_F.csv')

### height table
# write.csv(Anova(height_values_lmer), 'tables/height_anova.csv')

### total n table
# write.csv(Anova(nitrogen_percent_total_lmer, test.statistic = 'F'), 'tables/total_n_anova.csv')

### germination table
write.csv(Anova(germination_lm_continuous, test.statistic = 'F'), 
          'tables/revised2/germination_anova_F.csv')

## slope and intercept tables
### germination
write.csv(test(emtrends(germination_lm_continuous, ~species, 
                                var = 'treatment')),
          'tables/revised2/slopes_intercepts/germination_slope.csv')
cld(emtrends(germination_lm_continuous, ~species, 
              var = 'treatment'))

### mass
write.csv(test(emtrends(mass_values_lmer_continuous, ~species, 
                        var = 'treatment_continuous')),
          'tables/revised2/slopes_intercepts/mass_slope.csv')
cld(emtrends(mass_values_lmer_continuous, ~species, 
             var = 'treatment_continuous'))

### percent N
write.csv(test(emtrends(percent_nitrogen_lmer_continuous, ~species, 
                        var = 'treatment_continuous')),
          'tables/revised2/slopes_intercepts/percent_nitrogent_slope.csv')
cld(emtrends(percent_nitrogen_lmer_continuous, ~species, 
             var = 'treatment_continuous'))

### C:N
write.csv(test(emtrends(cn_lmer_continuous, ~species, 
                        var = 'treatment_continuous')),
          'tables/revised2/slopes_intercepts/cn_slope.csv')
cld(emtrends(cn_lmer_continuous, ~species, 
             var = 'treatment_continuous'))
