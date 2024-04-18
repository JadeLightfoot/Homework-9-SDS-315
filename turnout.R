library(mosaic)
library(tidyverse)
library(MatchIt)

# read in the data
turnout=read.csv('../data/turnout.csv')

# naive conclusion: the GOTV calls increased turnout
mean(voted1998 ~ GOTV_call, data=turnout)

# but those who received a call differed from those who didn't
mean(AGE ~ GOTV_call, data=turnout) # older
mean(AGE ~ voted1998, data=turnout) # older

mean(GOTV_call ~ MAJORPTY, data=turnout) # more likely to be registered with a major party
mean(voted1998 ~ MAJORPTY, data=turnout) 

mean(GOTV_call ~ voted1996, data=turnout)
mean(voted1998 ~ voted1996 , data=turnout) # more likely to have voted in 1996 (prior election)


# let's match!
# create a balanced data set from an unbalanced one

# find matches
turnout_match = matchit(GOTV_call ~ AGE + MAJORPTY + voted1996, data = turnout, ratio=5)

# check balance
# Check covariate balance
summary(turnout_match)

# extract the matched data and analyze
turnout_matched = match.data(turnout_match)

prop(voted1998 ~ GOTV_call, data=turnout_matched)
prop.test(voted1998 ~ GOTV_call, data=turnout_matched)


# get confidence intervals by bootstrapping
boot_turnout = do(1000)*lm(voted1998 ~ GOTV_call, data=resample(turnout_matched))
confint(boot_turnout)
