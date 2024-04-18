###############################################
# Data Preparation for IST 558 Final
# Maya A. Dalton
# April 2024
###############################################
rm(list=ls())

# Load libraries
library(readr)
library(haven)
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(corrr)
library(ggcorrplot)
library(stats)
library(factoextra)
library(FactoMineR)

# Set working directory
setwd("~/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/IST 558/Final Project/Dalton_IST558Final_Rep")

backsliding <- read_csv("little_meng.csv")
backsliding.c <- backsliding %>% 
  select(country_name, year, oindex)          #Selecting objective measure

vdem <- read_csv("vdem.csv")
vdem.c <- vdem %>%
  select(country_name, year, v2x_polyarchy, e_polcomp)   #Selecting subjective measure & new objective measure of legislative comp

ecav <- read_excel("ecav.xls")
ecav <- ecav %>%
  mutate(year = format(as.Date(Date, format="%Y/%m/%d"),"%Y"), # Extract year from date variable
         year = as.numeric(year),
         election_year = format(as.Date(Electiondate, format="%Y/%m/%d"),"%Y"),
         election_year = as.numeric(election_year)) 
ecav.c <- ecav %>%
  select(country, year, EventViolence)             #Selecting new objective measure using EV

###############################################
###############################################
###############################################
# New Objective Measure
###############################################
###############################################
###############################################

df <- left_join(vdem.c, ecav.c, by=c('country_name'='country', "year"))
df.c <- df %>%
  subset(v2x_polyarchy >= 0.5) %>%
  mutate(e_polcomp = case_when(
    e_polcomp == -88 ~ NA,
    e_polcomp == -77 ~ NA,
    e_polcomp == -66 ~ NA,
    .default = as.numeric(e_polcomp)
    )) %>%
  drop_na() #Drop NA

df.c <- df %>%
  mutate(
    new_oindex = case_when(
      e_polcomp == 1 & EventViolence == 1 ~ 0, # No competition & EV = high 
      e_polcomp == 2 & EventViolence == 1 ~ 0.1, # Restricted comp & EV = high
      e_polcomp == 3 & EventViolence == 1 ~ 0.2, # Repressed comp & EV
      e_polcomp >= 1 & e_polcomp <= 3 & EventViolence == 0 ~ 0.3, # Restrictied/no competition & no EV 
      
      e_polcomp == 4 & EventViolence == 1 ~ 0.4, # Uninstitutionalized comp & EV = moderate
      e_polcomp == 4 & EventViolence == 0 ~ 0.5, # Uninstitutionalized comp & EV = moderate
      
      e_polcomp >= 5 & e_polcomp <= 8 & EventViolence == 1 ~ 0.6, # Factional comp & EV = moderate
      e_polcomp >= 5 & e_polcomp <= 8 & EventViolence == 0 ~ 0.7, # Factional comp & no EV = moderate
      
      e_polcomp == 9 & EventViolence == 1 ~ 0.8, # Institutionalized comp & EV = low
      e_polcomp == 9 & EventViolence == 0 ~ 0.9, # Institutionalized comp & no EV = low
      e_polcomp == 10 & EventViolence == 0 ~ 0, # Institutionalized comp & no EV = low
    )
  ) %>%
  drop_na(new_oindex)

###############################################
# Combining all datasets
###############################################

df <- left_join(backsliding.c, df.c, 
                by=c('country_name', "year")) %>%
  drop_na() %>%
  mutate(time = case_when(
    year == 1989 ~ 1,
    year == 1990 ~ 2,
    year == 1991 ~ 3,
    year == 1992 ~ 4,
    year == 1993 ~ 5,
    year == 1994 ~ 6,
    year == 1995 ~ 7,
    year == 1996 ~ 8,
    year == 1997 ~ 9,
    year == 1998 ~ 10,
    year == 1999 ~ 11,
    year == 2001 ~ 12,
    year == 2002 ~ 13,
    year == 2003 ~ 14,
    year == 2004 ~ 15,
    year == 2005 ~ 16,
    year == 2006 ~ 17,
    year == 2007 ~ 18,
    year == 2008 ~ 19,
    year == 2009 ~ 20,
    year == 2010 ~ 21,
    year == 2011 ~ 22,
    year == 2012 ~ 23,
    year == 2013 ~ 24
  )) %>%
  select(time, oindex, v2x_polyarchy, new_oindex)

df.fin <- df %>%
  select(oindex, v2x_polyarchy, new_oindex)

write.csv(df.fin , "dalton_df.csv", row.names=FALSE) #Save dataset

###############################################
###############################################
###############################################
# Plots of Trends Over Time
###############################################
###############################################
###############################################

###### Subjective Measure ######
png("~/Dropbox/Apps/Overleaf/IST 558/Figures/subj_trends.png", res=100)
ggplot(df.c, aes(x=year, y=v2x_polyarchy)) +
  geom_line(stat="smooth")+
  labs(y="Democracy Index", x="Year", title="Changes in Democracy Over Time",
       subtitle="Subjective Measure from VDEM")+
  theme_minimal()
dev.off()

###### Objective Measure ######
png("~/Dropbox/Apps/Overleaf/IST 558/Figures/obj_trends.png", res=100)
ggplot(backsliding.c, aes(x=year, y=oindex)) +
  geom_line(stat="smooth")+
  labs(y="Democracy Index", x="Year", title="Changes in Backsliding Over Time",
       subtitle = "Objective Measure from Little & Meng (2024)")+
  theme_minimal()
dev.off()

###### Electoral Violence ######
df.c <- df.c %>%
  group_by(year) %>%
  mutate(total_vio = sum(EventViolence, na.rm=T))

png("~/Dropbox/Apps/Overleaf/IST 558/Figures/ev_trends.png", res=100)
ggplot(df.c, aes(x=year, y=total_vio)) +
  geom_line(stat="smooth")+
  labs(y="Electoral Violence Events", x="Year", title="Electoral Violence Over Time")+
  theme_minimal()
dev.off()

###### Political Competition ######
png("~/Dropbox/Apps/Overleaf/IST 558/Figures/comp_trends.png", res=100)
ggplot(df.c, aes(x=year, y=e_polcomp)) +
  geom_line(stat="smooth")+
  labs(y="Political Competition", x="Year", title="Competition Over Time")+
  theme_minimal()
dev.off()

###### New Objective Measure ######
png("~/Dropbox/Apps/Overleaf/IST 558/Figures/newindex_trends.png", res=100)
ggplot(df.c, aes(x=year, y=new_oindex)) +
  geom_line(stat="smooth")+
  labs(y="New Objective Index", x="Year", title="Democratic Backsliding Over Time",
       subtitle="Measure of Electoral Violence and Political Competition")+
  theme_minimal()
dev.off()

###############################################
###############################################
###############################################
# Regular PCA 
###############################################
###############################################
###############################################

###### Correlation Plot ######
df_norm <- scale(df.fin)
colnames(df_norm) <- c("Little/Meng Index", "Polyarchy", "New Index")
cor_mat <- cor(df_norm)

png("~/Dropbox/Apps/Overleaf/IST 558/Figures/corr_plot.png", res=100)
ggcorrplot(cor_mat, colors = c("#BACDB0", "#729B79", "darkgreen"),
           title="Correlation Between Indices", lab=TRUE, lab_col = "white")
dev.off()

###### PCA Analysis ######
res.pca <- PCA(df.fin,  graph = FALSE)

# Extract the PCA loadings and scores
scores <- res.pca$ind$coord[,1]
df.fin$scores <- scores

# Biplot
png("~/Dropbox/Apps/Overleaf/IST 558/Figures/pca_biplot.png", res=100)
fviz_pca_var(res.pca, repel=T, col.circle = "white", title="PCA Biplot",
             ggtheme = theme_minimal())
dev.off()

# Correlation Plot
corrMat <- cor(res.pca$ind$coord, df.fin, use="complete.obs")
colnames(corrMat) <- c("Little/Meng Index", "Polyarchy", "New Index", "Factor Scores")

png("~/Dropbox/Apps/Overleaf/IST 558/Figures/pca_corr_plot.png", res=100)
ggcorrplot(corrMat, lab="TRUE", lab_col = "white", colors = c("#BACDB0", "#729B79", "darkgreen"),
           legend.title="Correlation", title="Correlation Between Indices\nand Factor Scores", 
           ggtheme = theme_minimal())
dev.off()

# Trends over time
df$scores <- scores
png("~/Dropbox/Apps/Overleaf/IST 558/Figures/pca_trends.png", res=100)
ggplot(df, aes(x=time, y=scores)) +
  geom_line(stat="smooth")+
  labs(y="Factor Analysis Scores", x="Year", title="Democratic Backsliding Over Time")+
  theme_minimal()
dev.off()
