## Loading libraries
library(plm)
library(tidyverse)
library(readr)
library(gplots)
library(foreign)

library(dplyr)
library(stringr)

## Reading Data
database = read.csv("/Users/peterlee/Documents/Zimbabwe data/Book7.csv",header=TRUE)

simplereg = lm(codtad.0.17 ~ plwh.all, data=database)
summary(simplereg)

# Dummy Variable
dummyvar = lm(codtad.0.17 ~ plwh.all + factor(Year) + factor(States), data=database)
summary(dummyvar)

# Within estimator
within = plm(codtad.0.17 ~ plwh.all + factor(Year), index="States", model="within", data=database)
summary(within)

#Declare panel data
database.pd <- pdata.frame(database, index = c("States", "Year"), drop.index = TRUE)

#Look for unobserved heterogeneity
plotmeans(codtad.0.17 ~ States, main="Heterogeineity across countries", data=database)
plotmeans(codtad.0.17 ~ Year, main="Heterogeineity over time", data=database)

# Scatter and Fit graphs
database %>% filter(States != "NA", States != "Not classified") %>%
  ggplot(data = database, mapping = aes(x = plwh.all, y = codtad.0.17, color = States)) + 
  geom_point() + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(title = "CODTAD vs PLWH by State")
database %>% filter(Year != "NA", Year != "Not classified") %>%
  ggplot(data = database, mapping = aes(x = plwh.all, y = codtad.0.17, color = Year)) + 
  geom_point() + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(title = "CODTAD vs PLWH by Year")

database %>% filter(States != "NA", States != "Not classified") %>%
  ggplot(data = database, mapping = aes(x = ardo.0.17, y = codtad.0.17, color = States)) + 
  geom_point() + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(title = "CODTAD vs ARDO by State")
database %>% filter(Year != "NA", Year != "Not classified") %>%
  ggplot(data = database, mapping = aes(x = ardo.0.17, y = codtad.0.17, color = Year)) + 
  geom_point() + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(title = "CODTAD vs ARDO by Year")

database %>% filter(States != "NA", States != "Not classified") %>%
  ggplot(data = database, mapping = aes(x = amp.per.1000, y = codtad.0.17, color = States)) + 
  geom_point() + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(title = "CODTAD vs AMP by State")
database %>% filter(Year != "NA", Year != "Not classified") %>%
  ggplot(data = database, mapping = aes(x = amp.per.1000, y = codtad.0.17, color = Year)) + 
  geom_point() + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(title = "CODTAD vs AMP by Year")

database %>% filter(States != "NA", States != "Not classified") %>%
  ggplot(data = database, mapping = aes(x = ard.all, y = codtad.0.17, color = States)) + 
  geom_point() + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(title = "CODTAD vs ARD by State")
database %>% filter(Year != "NA", Year != "Not classified") %>%
  ggplot(data = database, mapping = aes(x = ard.all, y = codtad.0.17, color = Year)) + 
  geom_point() + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(title = "CODTAD vs ARD by Year")

database %>% filter(States != "NA", States != "Not classified") %>%
  ggplot(data = database, mapping = aes(x = hiani.all, y = codtad.0.17, color = States)) + 
  geom_point() + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(title = "CODTAD vs HIANI by State")
database %>% filter(Year != "NA", Year != "Not classified") %>%
  ggplot(data = database, mapping = aes(x = hiani.all, y = codtad.0.17, color = Year)) + 
  geom_point() + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(title = "CODTAD vs HIANI by Year")

database %>% filter(States != "NA", States != "Not classified") %>%
  ggplot(data = database, mapping = aes(x = nhi.all, y = codtad.0.17, color = States)) + 
  geom_point() + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(title = "CODTAD vs NHI by State")
database %>% filter(Year != "NA", Year != "Not classified") %>%
  ggplot(data = database, mapping = aes(x = nhi.all, y = codtad.0.17, color = Year)) + 
  geom_point() +  
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(title = "CODTAD vs NHI by Year")

# Remove NA values
database <- database[-c(375:706), ]
dim(database)

summary(database)

database.pd <- database.pd[-c(375:706), ]
dim(database.pd)

summary(database.pd)

# OLS Model
ols <- lm(codtad.0.17 ~ log(plwh.all) + ardo.0.17 + log(ard.all) + log(amp.per.1000) + log(hiani.all) + log(nhi.all), data = database)
summary(ols)

# OLS Streamlined
ols <- lm(codtad.0.17 ~ log(plwh.all) + ardo.0.17, data = database)
summary(ols)

# FE Model
fe <- plm(codtad.0.17 ~ log(plwh.all) + ardo.0.17 + log(ard.all) + log(amp.per.1000) + log(hiani.all) + log(nhi.all), data = database, index = c("States", "Year"), model = "within")
summary(fe)

# RE Model
re <- plm(codtad.0.17 ~ log(plwh.all) + ardo.0.17 + log(ard.all) + log(amp.per.1000) + log(hiani.all) + log(nhi.all), data = database, index = c("States", "Year"), model = "random")
summary(re)

#FE RE Selection
phtest (fe, re) # hausman test

fgls <- pggls(codtad.0.17 ~ log(plwh.all) + ardo.0.17 + log(ard.all) + log(amp.per.1000) + log(hiani.all) + log(nhi.all), data = database, model = "pooling")
summary(fgls)
pcdtest(fgls, test = c("cd"))
pcdtest(fgls, test = c("lm"))

library(lmtest)
bptest(codtad.0.17 ~ log(plwh.all) + ardo.0.17 + log(ard.all) + log(amp.per.1000) + log(hiani.all) + log(nhi.all), data = database, studentize=F)

database %>% 
  ggplot(mapping = aes(x = plwh.all, y = codtad.0.17)) + 
  geom_point(shape = 16) + 
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1) + 
  theme_bw() + 
  labs(title = "People Living With HIV/AIDS vs Orphan Case Correlation Test")

#expandR
install.packages("Rcpp", dependencies = TRUE)
library(ExPanDaR)
ExPanDaR::ExPanD()