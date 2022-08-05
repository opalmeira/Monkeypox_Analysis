# Monkeypox_Analysis
This script does some data manipulation and analysis on a dataset of monkeypox cases in 2022

# Instructions to run monkeypox_code.R

Input file: Monkeypox.zip/Monkepox.csv

The Monkeypox dataset was downloaded from https://www.kaggle.com/datasets/hanzlanawaz/monkeypox-outbreak?resource=download
Monkeypox is an infectious disease caused by orthopoxvirus. In May of 2022 there was an oubreak of the disease. According to the World Health Organization(WHO), this is the first time that many monkeypox cases and clusters have been reported concurrently in non-endemic and endemic countries in widely disparate geographical area (WHO). Thus, it is important to understand the course of the disease.

The code was written in R version 3.6.3 (2020-02-29).
It manipulates a dataset of monkeypox cases registered around the world and does some statistical analysis in order to answer the following questions:
1. Which country reported more confirmed cases of monkepox infection?
2. In what age range there were more confirmed cases of monkepox infection?
3. Is there a relationship between country and age in the cases?

You must have the following R packages to run this script: **dplyr, readr, ggplot2, gridExtra**
You must also have the file Monkeypox.zip in the same directory as the script
Upon downloading the script file and storing it in a directory, make sure the file is in executable mode.
The script is runnable on the command line and it requires the user's interaction to continue running since it generates plots along the execution through a pop up R window. You can move the graph around the screen, and to continue the program, you simply need to press any key on the terminal when required.
The output texts generated during execution briefly explains the rational of the data manipulation, analysis and results.
