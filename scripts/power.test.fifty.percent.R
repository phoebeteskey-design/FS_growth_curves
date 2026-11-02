#Cleaning grazed bltr data and converting to percentages
#Extracting at what height 50% of the weight is
#Running power test with that sd

grazed_bltr <- read.csv("raw_data/bltr_grazed.csv")
head(grazed_bltr)
library(tidyverse)
library(pwr)
grazed_bltr1 <- mutate(grazed_bltr, perc_height = cut_cm/total_height*100)
grazed_bltr2 <- mutate(grazed_bltr1, perc_weight = weight_g_remaining/total_weight*100)
#interpolate the exact % height where 50% of the weight is for each individual
#have to exclude sample 2 because 50% of the weight was below the first cut taken
fifty_perc <- grazed_bltr2 %>% group_by(individual) %>% 
  filter(min(perc_weight) <=50 & max(perc_weight) >= 50) %>% 
  summarise(height_50 = approx(x = perc_weight, y = perc_height, xout = 50)$y)
#get standard deviation of those heights for the power test
sd_50 <- sd(fifty_perc$height_50)
print(sd_50)

#power test with new sd

margin_of_error <- 5
effect_size <- margin_of_error/sd_50
#leaving n as Null since we want to know the sample size
#significance and power level are standard
sample_size <- pwr.t.test(d = effect_size,
                          n = NULL,
                          sig.level = 0.05,
                          power = 0.8,
                          type = "one.sample",
                          #this means we want a plus or minus 5, so 5 units taller or shorter
                          alternative = "two.sided")
print(sample_size)
#39 samples if we want 50% of the weight to be spot on

#the margin of error here represents plus or minus 5% of the weight. So we want our final estimate
#of the weight removed (biomass) to be within a 5% window of the true weight.
#Is this appropriate for this project?

#A conservative stocking rate is 27% utilization
#What would that be in % weight remaining?
#Would conservative stocking rate vary with individual species of grasses?
