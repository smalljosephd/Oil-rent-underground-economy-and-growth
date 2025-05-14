//***The relationship between oil, shadow economy and economic growth in Nigeria****//

***Directory settings****
global path "C:/Users/user/Dropbox/Second UPSI paper/1 Empirical and data"
global clean_dir = "${path}/1_data/3_clean"
global raw_dir = "${path}/1_data/1_raw"
global intermediate_dir = "${path}/1_data/2_intermediate"
global output_dir = "${path}/3_output"

**Clear the current dataset, variables and graphs in memory
clear
cap drop _all
cap graph drop _all

**Import annual data (Excel) for interpolation
import excel using "${intermediate_dir}/OIL, UE, AND ECO DATA.xlsx", cellrange(A1:Q44) sheet(Sheet1) firstrow

// Convert all variable names in the dataset to lowercase
rename *, lower

tsset yr

ipolate ms_shadow_eco yr, gen(se) epolate


foreach i in ms_shadow_eco oil_rent_gdp fiscal_balance domestic_investment gross_debt govt_revenue_gdp access_to_electricity {
ipolate `i' yr, gen(`i'_) epolate // Take the natural logarithms of the variables	
}


///Export data
export excel using "${intermediate_dir}/OIL, UE, AND ECO DATA.xlsx", sheet(Interpolated, replace) firstrow(variables)


