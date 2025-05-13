# OBAT Analysis --------- 

# Libraries
library(tidyverse)

# Reading data
obat <- read_csv("C:/Users/hebers/Downloads/OBAT Project_DataAnalysis.csv")

# Basic demographics --------- 
p.famplan <- ggplot(obat, aes(`Family planning missing`)) + geom_bar()
p.famplan

p.famplanfu <- ggplot(obat, aes(`Family Planning Follow Up`)) + geom_bar()
p.famplanfu

p.papsm <- ggplot(obat, aes(`Pap smear missing`)) + geom_bar()
p.papsm

p.papsmfu <- ggplot(obat, aes(`Pap smear Follow Up`)) + geom_bar()
p.papsmfu

p.prep <- ggplot(obat, aes(`PrEP missing`)) + geom_bar()
p.prep

p.prepfu <- ggplot(obat, aes(`PrEP Follow Up`)) + geom_bar()
p.prepfu

age.hist <- hist(obat$Age)
age.hist

p.race <- ggplot(obat, aes(Race)) + geom_bar()
p.race

p.eth <- ggplot(obat, aes(Ethnicity)) + geom_bar()
p.eth

p.lang <- ggplot(obat, aes(Langauge)) + geom_bar()
p.lang

p.mar <- ggplot(obat, aes(`Martial status`)) + geom_bar()
p.mar

# Binary data ---------- 
obat2 <- obat
obat2$`Family planning missing` <- ifelse(obat$`Family planning missing` == "Yes", 
                                          1, 0)
obat2$`Pap smear missing` <- ifelse(obat$`Pap smear missing` == "Yes", 1, 0)
obat2$`PrEP missing` <- ifelse(obat$`PrEP missing` == "Yes", 1, 0)

# Age and race vs missing visits
agerace.famp <- glm(`Family planning missing` ~ Age + Race, family = "binomial", 
                    data = obat2)
summary(agerace.famp)
