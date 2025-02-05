# ShinyBrAPPs

The ShinyBrAPPs package contains shiny applications designed for plant breeders. An international collaboration of developers from CIRAD and the IBP have been working together as part of the [IAVAO](https://www.iavao.org/) breeders community to develop these ShinyBrAPPs, in support of national breeding programs in western Africa. 

These applications can be connected to datasources that implement BrAPI compliant web services. BrAPI, for Breeding API is a standardized application programming interface (API) specification for breeding and related agricultural data. BrAPI compliance offers these systems the opportunity to add functionalities in a modular way through the development of external plugin applications that can quickly fulfill specific needs for this group of breeders and scientists.

Currently, ShinyBraPPs applications are connected to the Breeding Management System ([BMS](https://bmspro.io/)) and/or [Gigwa](https://www.southgreen.fr/content/gigwa). They are accessible directly from those systems interface.

## Installation

Install remotes package if not already done

``` r
install.packages(remotes)
```
Install snpclust package

``` r
remotes::install_github("IntegratedBreedingPlatform/ShinyBrAPPs")
```
or for latest development version: 
``` r
remotes::install_github("IntegratedBreedingPlatform/ShinyBrAPPs@dev")
```


## BMS trial data explorer
The “BMS trial data explorer” retrieves data from a single multi-location trial and displays data counts and summary box-plots for all variables measured in different studies. It also provides an interactive distribution plot to easily select observations that require curation and a report of candidate issues that needs to be addressed by the breeder.  
``` r
shinybrapps::run_trialdataxplor()
```


## STABrAPP
The “STABrAPP” tool is an application for single trial mixed model analysis. It basically provides a GUI to the StatGen-STA R package.
``` r
shinybrapps::run_stabrapp()
```

## BrAVISE
BrAVISE is a decision support tool helping breeders to select germplasm according to their various characteristics and save this germplasm list into BMS. BrAVISE is an application for Genotype by Environment (GxE) analysis. It provides a GUI to the statgenGxE R package
``` r
shinybrapps::run_decision()
```

## snpclust
“snpclust” tool enables a user to check and manually correct the clustering of fluorescence based SNP genotyping data. snpclust is not included to this package yet. It is accessible here : https://github.com/jframi/snpclust


