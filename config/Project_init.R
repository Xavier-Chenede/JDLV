## Create project tree and initialize pathway

#install package
        # install.packages("here")
        # install.packages("ProjectTemplate")
        # install.packages("tidyverse")
        # install.packages("patchwork")
        # install.packages("gganimate")
        # install.packages("gifski")
        # install.packages("plot.matrix")





#libraries
library(tidyverse)
library(glue)
library(rlang)
library(plot.matrix)
library(gganimate)
library(gifski)

#Manage directory
setwd("C:/Users/xsche/OneDrive/Xav/informatique/R/projects")

#set project tree
library(ProjectTemplate)
create.project(project.name='jeu_de_la_vie', template="full")

#set relative path
here::here

#divers

