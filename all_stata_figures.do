///////////////////////////////
//binscatter of YoY Price Change on density
//NBER fig 5a
///////////////////////////////

clear
// Set working directory before creation of each figure
cd "/Users/arjun/Documents/zillow/thesis/donut-effect"

import delimited "./data/zhvi_panel_zips_top12.csv", stringcols(1 12) numericcols(2 3 6 7 8 9 10) 
rename v13 population2
gen log_density = log10(density2019)
binscatter post_pct_change log_density [fweight = population2], controls(pre_pct_change) absorb(metroshort)	///
	legend(off) ///
	xtitle("Persons per sq mile") ///
	ytitle("Percent change home value index") ///
	yscale(range(15 25 30 35 40 45)) ///
	ylabel(15 20 25 30 35 40 45) ///
	xscale(range(2 5)) ///
	xlabel(2 "100" 3 "1,000" 4 "10,000" 5 "100,000") ///
	xsize(5) ysize(5) ///
	graphregion(margin(medlarge))
graph export "./figures-tables/fig6a.png", replace


///////////////////////////////
//NBER Figure 5b
//binscatter of cumulative flows since pandemic on log density
///////////////////////////////
clear
cd "/Users/arjun/Documents/zillow/thesis/donut-effect"

import delimited "./data/usps_panel_zips_top12.csv"
rename v21 population2
gen log_density = log10(density2019)
gen log_wfh = log10(wfh_emp)
gen log_dist_cbd = log10(dist_to_cbd)

binscatter post_pop log_density [fweight = population2], controls(pre_pop) absorb(metroshort)	///
	legend(off) ///
	xtitle("Persons per sq mile") ///
	ytitle("Net inflow as a percent of population") ///
	yscale(range(-8 4)) ///
	ylabel(-8 -6 -4 -2 0 2 4) ///	
	xscale(range(2 5)) ///
	xlabel(2 "100" 3 "1,000" 4 "10,000" 5 "100,000") ///
	xsize(5) ysize(5) ///
	graphregion(margin(medlarge))
	graph export "./figures-tables/fig6b.png", replace


///////////////////////////////
//NBER Figure 5c
//binscatter of cumulative business flows since pandemic on density
///////////////////////////////
clear
cd "/Users/arjun/Documents/zillow/thesis/donut-effect"

import delimited "./data/usps_panel_zips_top12.csv"
rename v21 population2
gen log_density = log10(density2019)
gen log_wfh = log10(wfh_emp)
gen log_dist_cbd = log10(dist_to_cbd)
binscatter post_bus log_density [fweight = population2], controls(pre_bus) absorb(metroshort)	///
	legend(off) ///
	xtitle("Persons per sq mile") ///
	ytitle("Net inflow as a percent of stock")  ///
	yscale(range(-8 8)) ///
	ylabel(-8 -6 -4 -2 0 2 4 6 8) ///
	xscale(range(2 5)) ///
	xlabel(2 "100" 3 "1,000" 4 "10,000" 5 "100,000") ///
	xsize(5) ysize(5) ///
	graphregion(margin(medlarge))
	graph export "./figures-tables/fig6c.png", replace
	
///////////////////////////////
//binscatter of YoY Price Change on density MSA
//NBER fig 6a
///////////////////////////////

clear
cd "/Users/arjun/Documents/zillow/thesis/donut-effect"

import delimited "./data/msa_zhvi.csv", numericcols(2 15)
rename v5 population2
gen log_density = log10(density2019)

binscatter post_pct_change log_density [fweight = population2], controls(pre_pct_change)	///
	xtitle("Persons per sq mile") ///
	ytitle("Percent change home value index") ///
	yscale(range(15 45)) ///
	ylabel(15 20 25 30 35 40 45) ///
	xscale(range(2 3)) ///
	xlabel(2 "100" 3 "1,000") ///
	xsize(5) ysize(5) ///
	graphregion(margin(medlarge))
graph export "./figures-tables/fig7a.png", replace

///////////////////////////////
//NBER Figure 6b
//binscatter of cumulative net population inflow on density MSA
///////////////////////////////

clear
cd "/Users/arjun/Documents/zillow/thesis/donut-effect"

import delimited "./data/msa_USPS.csv"
rename v13 population2
gen log_density = log10(density2019)

binscatter post_pop log_density [fweight = population2], controls(pre_pop) ///
	xtitle("Persons per sq mile") ///
	ytitle("Net inflow as a percent of population") ///
	yscale(range(-8 4)) ///
	ylabel(-8 -6 -4 -2 0 2 4) ///
	xscale(range(2 3 4)) ///
	xlabel(2 "100" 3 "1,000" 4 "10,000") ///
	xsize(5) ysize(5) ///
	graphregion(margin(medlarge))
graph export "./figures-tables/fig7b.png", replace


///////////////////////////////
//NBER Figure 6c
//binscatter of cumulative business inflow on density MSA
///////////////////////////////

clear
cd "/Users/arjun/Documents/zillow/thesis/donut-effect"

import delimited "./data/msa_USPS.csv"
rename v13 population2
gen log_density = log10(density2019)

binscatter post_bus log_density [fweight = population2], controls(pre_bus)	///
	xtitle("Persons per sq mile") ///
	ytitle("Net inflow as a pecent of stock") ///
	yscale(range(-8 8)) ///
	ylabel(-8 -6 -4 -2 0 2 4 6 8) ///
	xscale(range(2 3 4)) ///
	xlabel(2 "100" 3 "1,000" 4 "10,000") ///
	xsize(5) ysize(5) ///
	graphregion(margin(medlarge))
graph export "./figures-tables/fig7c.png", replace

///////////////////////////////
//NBER Appendix A3 (a)
//binscatter of rent change on net inflows
///////////////////////////////

clear
cd "/Users/arjun/Documents/zillow/thesis/donut-effect"

import delimited "./data/zori_USPS.csv", numericcols(12 13)
rename v23 population2
gen log_density = log10(density2019)

binscatter post_pct_change post_pop [fweight = population2], controls(pre_pct_change log_density) absorb(metroshort) ///
	xtitle("Net inflow as a percent of population") ///
	ytitle("Percent change rental index") ///
	graphregion(margin(medlarge))

graph export "./figures-tables/appendix_a2a.png", replace
 

///////////////////////////////
//NBER Appendix A4 (b)
//binscatter of price change on net inflows
///////////////////////////////

clear
cd "/Users/arjun/Documents/zillow/thesis/donut-effect"

import delimited "./data/zhvi_USPS.csv", numericcols(12 13)
rename v23 population2
gen log_density = log10(density2019)

binscatter post_pct_change post_pop [fweight = population2], controls(pre_pct_change log_density) absorb(metroshort)	///
	xtitle("Net inflow as a percent of population") ///
	ytitle("Percent change home value index") ///
	graphregion(margin(medlarge))
graph export "./figures-tables/appendix_a2b.png", replace
 

 

