# donut-effect
Replication code for "The Donut Effect of Covid-19 on Cities" by Arjun Ramani and Nicholas Bloom.
Click here for the most recent draft of our working paper: https://www.nber.org/system/files/working_papers/w28876/w28876.pdf 


## Replication file instructions
This repository contains all data and scripts necessary to replicate the figures and tables found in the paper. You may need R version 3.6.2 or later to successfully run.

### Creating all figures and tables from intermediate datasets

To replicate all figures and tables using intermediate data stored in the `data` folder, run:
  - `all_R_figures.R` to create all figures that use R
  - `all_stata_figures.do` to create all figures that use Stata
  - `tables.R` to create all regression tables

### Creating intermediate datasets (used to create figures and tables)

Some of the datasets used in this process must be downloaded from various locations. To ease this process, we have stored each of these datasets in the `data/external_data` folder. The locations of all datasets can be found in the files inside the `scripts` folder. To create the intermediate datasets used for the figures and tables from scratch, run
- `create_all_datasets.R` to create the Zillow and USPS datasets
  - `create_all_datasets.R` has a dependency on `zip_bus_patterns.R` which sources from `scripts/census-api.R`. You must obtain a Census API key from https://www.census.gov/data/developers/guidance/api-user-guide.html and store the key in `scripts/census-api.R` by including a line as follows: `key = "INSERT_YOUR_KEY_HERE`

### Branches
- `nber-wp` contains code for the original May 2021 version of the NBER working paper
- `main` contains code for the updated version of the paper (in-progress)

