# Observer data climate reconstruction

This is the repository for code and data for the analysis in 

John Tipton, Mevin Hooten, Simon Goring, John Williams. Reconstruction of spatio-temporal temperature processes from sparse historical records using probabilistic principal component regression. Advances in Statistical Climatology, Meteorology, and Oceanography. (In Review).

### Installation of `R` packages 

Users must install the two tarball packages `myFunctions_1.0.tar.gz` and `fortFunctions_1.0.tar.gz` to load the necessary c++ helper functions for code to run. Once downloaded, move to the download directory and use the commands

```
R CMD INSTALL myFunctions_1.0.tar.gz
```
and
```
R CMD INSTALL fortFunctions_1.0.tar.gz
```
to install the necessary R packages

### R 

#### `process-data.R`
This file converts the raw data into an .RData file for use in the file `observer.Rmd`.

#### `observer.Rmd`
This R markdown file runs all analyses in the paper and produces all plots included in the paper as well as diagnostic/model checks not included in the publication. `observer.Rmd` takes approximately 24 hours to run on my 2015 Macbook Pro laptop.

### Data


[![DOI](https://zenodo.org/badge/61319535.svg)](https://zenodo.org/badge/latestdoi/61319535)
