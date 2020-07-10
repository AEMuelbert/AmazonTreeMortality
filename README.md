# AmazonTreeMortality
Codes from the analyses on spatial patterns of tree mortality across Amazonian forests published in the article Esquivel-Muelbert et al. 2020

## 01_Master.R 
Describes the analyses shown in the paper including plot-level analyses and the survival analyses (tree-level)

Codes for Figures 1, 2 and 3 are provided here

## 02_functions_plot_level.R
Functions used to perform plot-level analyses and plot Figures 1 and 2

## 03_functions_survival_analyses.R
Functions used to perform survival analyses and plot Figure 3

## Data required
### 01_plot_rates.csv
Mortality rates per plot, plot location (latitude and longitude), region and plot code.

### 02_census_rates.csv
Mortality rates per census, census number, region and plot code.

### 03_census_mod.csv
Mortality rates per census for each mode of death, proportion of dead trees within each mode of death, census interval lenght,region and plot code. 
Modes of death: standing (likstanding) and broken/uprooted (notstanding)

### 04_surv_matrix.csv
Treeid, plot code, region, dead (0/1)

interval (i.e. interval during which tree was monitored)

D40 (diameter before the last observation)

DrelGR0(relative growth prior last observation)

MaxD(maximum diameter for the species)

MeangrD (mean growth rate for the species)

WD (species-level wood density)

WDA.gen (species-level water-defici affiliation)
