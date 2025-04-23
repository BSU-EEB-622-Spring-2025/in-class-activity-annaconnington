library(brms)
library(tidyverse)
library(ggplot2)
library(bayesplot)
library(ggplot2)
library(marginaleffects)
library(collapse)
library(pROC)
library(MLmetrics)
library(modelr) 
library(tibble)
library(ggdist)

### Data cleaning ###

  recordings <- read.csv("recordings.csv")
  sensor <- read.csv("sensorinfo.csv")
  recordings$watertemp <- as.numeric(recordings$watertemp)
  recordings$sensorid <- as.character(recordings$sensorid)
  recordings$dayid <- as.character(recordings$dayid)
  str(recordings)
  
  sensor$sensorid <- as.character(sensor$sensorid)
  str(sensor)

  
### Model Building ###
  
  song.mod<-brm(songlength ~ boatnoise+watertemp+boatactivity, data = recordings, family = "Gamma"(link=log))
  
  summary(song.mod)
  mcmc_plot(song.mod) + theme_bw()
  plot_predictions(song.mod, condition = c("boatnoise")) + theme_bw()
  plot(song.mod)
  mcmc_plot(song.mod, pars="^b_")
  pp_check(song.mod)
  
  
  
  
  bayes_R2(song.mod) 
  performance::mae(song.mod)

  
  freq.mod<-brm(totsongs~boatnoise+watertemp+boatactivity, data = recordings, family = "negbinomial" (link = "log"))
  
  
  summary(freq.mod)
  mcmc_plot(freq.mod) + theme_bw()
  plot_predictions(freq.mod, condition = c("boatnoise")) + theme_bw()
  plot(freq.mod)
  
  bayes_R2(freq.mod) 
  performance::mae(freq.mod)


