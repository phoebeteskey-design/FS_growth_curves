#Power analysis on grazed BLTR samples

#Using height only for this test

library(pwr)
library(tidyverse)
#grazed sample heights
grazed_BLTR_height_cm <- c(66, 71, 56, 60, 75, 52, 66, 58, 80, 53, 49, 70, 49)
#standard deviation of heights
bltr_sd <- sd(grazed_BLTR_height_cm)
#margin of error I decided on 2 inches, or 5cm
margin_of_errror <- 5
effect_size <- margin_of_errror/bltr_sd
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
#According to this test we need 35 samples