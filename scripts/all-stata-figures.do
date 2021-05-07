///////////////////////////////
//binscatter of YoY Price Change on density
//NBER fig 5a
///////////////////////////////

clear

global out "/Users/arjun/Documents/zillow/thesis/charts"

import delimited "/Users/arjun/Documents/zillow/thesis/data/zhvi_sfh_panel_zips_top12.csv", stringcols(1 12) numericcols(6 7 8 9 10) 
rename v13 population2
gen log_density = log10(density2019)
binscatter post_pct_change log_density [fweight = population2], controls(pre_pct_change) absorb(metroshort)	///
	legend(off) ///
	xtitle("Persons per sq mile") ///
	ytitle("Percent change home value index") ///
	yscale(range(6 11)) ///
	ylabel(6 7 8 9 10 11) ///
	xscale(range(2 5)) ///
	xlabel(2 "100" 3 "1,000" 4 "10,000" 5 "100,000") ///
	xsize(5) ysize(5) ///
	graphregion(margin(medlarge))
graph export "$out/paper/fig5a.png", replace


///////////////////////////////
//NBER Figure 5b
//binscatter of cumulative flows since pandemic on log density
///////////////////////////////
clear
global out "/Users/arjun/Documents/zillow/thesis/charts/paper"

import delimited "/Users/arjun/Documents/zillow/thesis/data/usps_panel_zips_top12.csv"
rename v21 population2
gen log_density = log10(density2019)
gen log_wfh = log10(wfh_emp)
gen log_dist_cbd = log10(dist_to_cbd)

binscatter post_pop log_density [fweight = population2], controls(pre_pop) absorb(metroshort)	///
	legend(off) ///
	xtitle("Persons per sq mile") ///
	ytitle("Net inflow as a percent of population") ///
	yscale(range(-10 5)) ///
	ylabel(-10 -5 0 5) ///	
	xscale(range(2 5)) ///
	xlabel(2 "100" 3 "1,000" 4 "10,000" 5 "100,000") ///
	xsize(5) ysize(5) ///
	graphregion(margin(medlarge))
	graph export "$out/fig5b.png", replace


///////////////////////////////
//NBER Figure 5c
//binscatter of cumulative business flows since pandemic on density
///////////////////////////////
clear
global out "/Users/arjun/Documents/zillow/thesis/charts/paper"

import delimited "/Users/arjun/Documents/zillow/thesis/data/usps_panel_zips_top12.csv"
rename v21 population2
gen log_density = log10(density2019)
gen log_wfh = log10(wfh_emp)
gen log_dist_cbd = log10(dist_to_cbd)
binscatter post_bus log_density [fweight = population2], controls(pre_bus) absorb(metroshort)	///
	legend(off) ///
	xtitle("Persons per sq mile") ///
	ytitle("Net inflow as a percent of stock")  ///
	yscale(range(-5 5)) ///
	xscale(range(2 5)) ///
	xlabel(2 "100" 3 "1,000" 4 "10,000" 5 "100,000") ///
	xsize(5) ysize(5) ///
	graphregion(margin(medlarge))
	graph export "$out/fig5c.png", replace
	
///////////////////////////////
//binscatter of YoY Price Change on density MSA
//NBER fig 6a
///////////////////////////////

clear

global out "/Users/arjun/Documents/zillow/thesis/charts"

import delimited "/Users/arjun/Documents/zillow/thesis/data/msa_zhvi.csv", numericcols(2 15)
rename v5 population2
gen log_density = log10(density2019)

binscatter post_pct_change log_density [fweight = population2], controls(pre_pct_change)	///
	xtitle("Persons per sq mile") ///
	ytitle("Percent change home value index") ///
	yscale(range(6 11)) ///
	ylabel(6 7 8 9 10 11) ///
	xscale(range(2 3)) ///
	xlabel(2 "100" 3 "1,000") ///
	xsize(5) ysize(5) ///
	graphregion(margin(medlarge))
graph export "$out/paper/fig6a.png", replace

///////////////////////////////
//NBER Figure 6b
//binscatter of cumulative net population inflow on density MSA
///////////////////////////////

clear

global out "/Users/arjun/Documents/zillow/thesis/charts/paper"

import delimited "/Users/arjun/Documents/zillow/thesis/data/msa_USPS.csv"
rename v13 population2
gen log_density = log10(density2019)

binscatter post_pop log_density [fweight = population2], controls(pre_pop) ///
	xtitle("Persons per sq mile") ///
	ytitle("Net inflow as a percent of population") ///
	yscale(range(-10 5)) ///
	ylabel(-10 -5 0 5) ///
	xscale(range(2 3 4)) ///
	xlabel(2 "100" 3 "1,000" 4 "10,000") ///
	xsize(5) ysize(5) ///
	graphregion(margin(medlarge))
graph export "$out/fig6b.png", replace


///////////////////////////////
//NBER Figure 6c
//binscatter of cumulative business inflow on density MSA
///////////////////////////////

clear

global out "/Users/arjun/Documents/zillow/thesis/charts/paper"

import delimited "/Users/arjun/Documents/zillow/thesis/data/msa_USPS.csv"
rename v13 population2
gen log_density = log10(density2019)

binscatter post_bus log_density [fweight = population2], controls(pre_bus)	///
	xtitle("Persons per sq mile") ///
	ytitle("Net inflow as a pecent of stock") ///
	yscale(range(-4 4)) ///
	ylabel(-4 -2 0 2 4) ///
	xscale(range(2 3 4)) ///
	xlabel(2 "100" 3 "1,000" 4 "10,000") ///
	xsize(5) ysize(5) ///
	graphregion(margin(medlarge))
graph export "$out/fig6c.png", replace

///////////////////////////////
//NBER Appendix A2 (a)
//binscatter of YoY Rent Change on WFH exposure
///////////////////////////////

clear

global out "/Users/arjun/Documents/zillow/thesis/charts"

import delimited "/Users/arjun/Documents/zillow/thesis/data/zori_sfh_panel_zips_top12.csv", numericcols(1 2)
rename v13 population2
gen log_density = log10(density2019)

binscatter post_pct_change wfh_emp [fweight=population2], controls(pre_pct_change log_density) absorb(metroshort)	///
	legend(off) ///
	xtitle("WFH share of residents") ///
	ytitle("YoY Percent change in rent") ///
	xsize(5) ysize(5) ///
	graphregion(margin(medlarge))
graph export "$out/appendixA2_a.png", replace

///////////////////////////////
//NBER Appendix A2 (b)
//binscatter of YoY Price Change on WFH exposure
///////////////////////////////

clear

global out "/Users/arjun/Documents/zillow/thesis/charts"

import delimited "/Users/arjun/Documents/zillow/thesis/data/zhvi_sfh_panel_zips_top12.csv", stringcols(12) numericcols(6 7 8 9 10 20) 
rename v13 population2
gen log_density = log10(density2019)
gen log_deaths_capita = log10(deaths_capita+1)

binscatter post_pct_change wfh_emp [fweight = population2], controls(pre_pct_change log_density) absorb(metroshort)	///
	legend(off) ///
	xtitle("WFH share of residents") ///
	ytitle("YoY Percent change in price")
graph export "$out/appendixA2_b.png", replace

///////////////////////////////
//NBER Appendix A3 (a)
//binscatter of rent change on net inflows
///////////////////////////////

clear

global out "/Users/arjun/Documents/zillow/thesis/charts/paper"

import delimited "/Users/arjun/Documents/zillow/thesis/data/zori_USPS.csv", numericcols(12 13)
rename v23 population2
gen log_density = log10(density2019)

binscatter post_pct_change post_pop [fweight = population2], controls(pre_pct_change log_density) absorb(metroshort) ///
	xtitle("Net inflow as a percent of population") ///
	ytitle("Percent change rental index") ///

graph export "$out/fig7a.png", replace
 

///////////////////////////////
//NBER Appendix A4 (b)
//binscatter of price change on net inflows
///////////////////////////////

clear

global out "/Users/arjun/Documents/zillow/thesis/charts/paper"

import delimited "/Users/arjun/Documents/zillow/thesis/data/zhvi_USPS.csv", numericcols(12 13)
rename v23 population2
gen log_density = log10(density2019)

binscatter post_pct_change post_pop [fweight = population2], controls(pre_pct_change log_density) absorb(metroshort)	///
	xtitle("Net inflow as a percent of population") ///
	ytitle("Percent change home value index")
graph export "$out/fig7b.png", replace
 

 

