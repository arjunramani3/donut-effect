# donut-effect
Replication code for "The Donut Effect of Covid-19 on Cities" by Arjun Ramani and Nicholas Bloom.
Click here for the most recent draft of our working paper: 


## Replication file instructions
This repository contains all data and scripts necessary to replicate the figures and tables found in the paper. 

To replicate all figures and tables using intermediate data stores in the `data` folder, run:
  - `all_R_figures.R` to create all figures that use R
  - `all_stata_figures.do` to create all figures that use Stata
  - `tables.R` to create all regression tables


To create the intermediate datasets used for the figures and tables from scratch, run
- `create_all_datasets.R` to create the Zillow and USPS datasets
  - Some of the datasets used in this process must be downloaded from various locations. To ease this process, we have stored each of these datasets in the `data/external_data` folder. The locations of all datasets can be found in the files inside the `scripts` folder.


