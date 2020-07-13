# AmazonTreeMortality
Adriane Esquivel Muelbert - School of Geography, Earth and Environmental Sciences, University of Birmingham, UK.
adriane.esquivel@gmail.com

Codes from the analyses on spatial patterns of tree mortality across Amazonian forests published in the article Esquivel-Muelbert et al. (2020) How and Why Amazon trees die. 

Code developed and tested using the R statistical platform R v 3.5.2 (R Development Core Team, 2015).

## 01_Master.R 
Describes the analyses shown in the paper including plot-level analyses and the survival analyses (tree-level)

Codes for Figures 1, 2 and 3 are provided here

## 02_functions_plot_level.R
Functions used to perform plot-level analyses and plot Figures 1 and 2

## 03_functions_survival_analyses.R
Functions used to perform survival analyses and plot Figure 3

## Data required

Data are available at: http://www.forestplots.net/en/publications#data

### d01_plot_rates.csv
Plot code 

Stem mortality rates (% y-1)

plot location (latitude and longitude in decimal degrees)

region

### d03_census_mod.csv
Plot code 

m.rates.likstanding - Stem mortality rates (% y-1) for trees that died standing 

m.rates.notstanding - Stem mortality rates (% y-1) for trees that died broken/uprooted 

p.MoD.likstanding - proportion of dead trees that died standing

p.MoD.notstanding - proportion of dead trees that died broken/uprooted 

census interval lenght (y)

region

### d04_surv_matrix.csv
plot code

region

dead (0/1)

interval (i.e. interval during which tree was monitored in years)

D (mm, diameter before the last observation)

DrelGR(mm, relative growth prior last observation)

MaxD(mm, maximum diameter for the species)

MeangrD (mm, mean growth rate for the species)

WD (g cm-3, species-level wood density)

WDA (mm, species-level water-defici affiliation)
