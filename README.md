# hw08 - Ojaswi Malik

## Welcome

This HW Assignment is distributed in 2 parts:

  1. Part 1: Gapminder and geonames - Effect of Population Density on Life Expectancy

  2. Part 2: Webscraping - Internet Usage Data

## Packages you should have installed

  - library(tidyverse)
  - library(rvest)
  - library(coefplot)
  - library(gapminder)
  - library(geonames)
  - library(countrycode)
  - library(here)
  
##  To access the geonames API  by using the geonames() package you need to:
  1. Register an account on [geonames website](http://www.geonames.org/)
  2. Enable [free web services](http://www.geonames.org/enablefreewebservice)
  3. Open .Rprofie and store your username
      - file.edit(here::here(".Rprofile"))
      - options("geonamesUsername" = "your_user_name")

## Files in this repo that should be executed

### For Part 1: Gapminder and geonames - Effect of Population Density on Life Expectancy

   1. [R Markdown](gapminder.Rmd): This is the R Markdown file with all the code which can be run to execute the same results as me

   2. [Markdown file](gapminder.md): This is the successfully knitted file in github_document format

   3. [files folder](gapminder_files): This folder has all the figures generated from the .md file stored in .png format

### For Part 2: Webscraping - Internet Usage Data

   1. [R Markdown](internet.Rmd): This is the R Markdown file with all the code which can be run to execute the same results as me

   2. [Markdown file](internet.md): This is the successfully knitted file in github_document format

   3. [files folder](internet_files): This folder has all the figures generated from the .md file stored in .png format

