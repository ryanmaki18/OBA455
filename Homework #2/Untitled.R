##1 

rm(list = ls())
library(tidyverse)
library("FNN")
pl = read.csv("personal_loan.csv")

#a
summary(pl$age)
sd(pl$age)

summary(pl$experience)
sd(pl$experience)

summary(pl$income)
sd(pl$income)

summary(pl$ccavg)
sd(pl$ccavg)

summary(pl$family)
sd(pl$family)

summary(pl$mortgage)
sd(pl$mortgage)

#b
  
tapply...



#c
temp = apply(pl[c("online", "cd_account", "credit_card", "securities_account")], 2, table)

#d


rm(list = ls())

##2
library(tidyverse)
library("FNN")
t = read.csv("Tayko.csv")

#a
summary(t$freq)
sd(t$freq)

summary(t$last_update)
sd(t$last_update)

#b
temp1 = t %>%
  group_by(spending) %>%
  summarise(average_web = mean(web), sd_web = sd(web)) %>%
  ungroup()
temp2 = t %>%
  group_by(spending) %>%
  summarise(average_gender = mean(gender), sd_web = sd(gender)) %>%
  ungroup()

#c
temp3 = apply(t[c("web", "gender", "address_res", "address_us")], 2, table)

#d
t = read.csv("Tayko.csv")

tayko.slr = lm(spending~freq+ web, t)

summary(tayko.slr)

#e

tayko.slr1 = lm(spending ~ freq + address_res, t)

summary(tayko.slr1)


tayko.slr2 = lm(spending ~ freq + last_update + address_res,  t)

summary(tayko.slr2)


