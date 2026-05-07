SELECT DISTINCT
    tot.geoUnit,
    tot.year,
    tot.Total_gov_gdp_spent_on_edu,
    ups.Percent_gdp_spent_upper_secondary_edu,
    ter.Percent_gdp_spent_tertiary_edu,
	sec.Percent_gdp_spent_secondary_edu,
	lit.[Literacy rate adult total (% of people ages 15 and above)],
	lows.Percent_gdp_spent_lower_secondary_edu,
	pri.Percent_gdp_spent_primary_edu,
	ag1.Percent_of_15to24_in_edu,
	ag2.Percent_of_15to64_in_edu,
	ag3.Percent_of_25to54_in_edu,
	ag4.Percent_of_55to64_in_edu,
	ger.Gross_enroll_ratio_tertiaryschool,
	gdp.GDP_per_capita_current_US,
	rpop.Rural_population,
	ptr.Primary_PTR,
	ptr.Secondary_PTR,
	ptr.Tertiary_PTR,
	ptr.Average_Ptratio
FROM [DATS221_Project].[dbo].[gov gdp percent on edu total] as tot
LEFT JOIN [DATS221_Project].[dbo].[upper secondary gov gdp] as ups
    ON (tot.geoUnit = ups.geoUnit AND tot.year = ups.year)
LEFT JOIN [DATS221_Project].[dbo].[tertiary gov gdp] as ter
    ON (tot.geoUnit = ter.geoUnit AND tot.year = ter.year)
LEFT JOIN [DATS221_Project].[dbo].[secondary gov gdp] as sec
    ON (tot.geoUnit = sec.geoUnit AND tot.year = sec.year)
LEFT JOIN [DATS221_Project].[dbo].[literacy-rate-adults] as lit
    ON (tot.geoUnit = lit.geoUnit AND tot.year = lit.Year)
LEFT JOIN [DATS221_Project].[dbo].[lower secondary gov gdp] as lows
    ON (tot.geoUnit = lows.geoUnit AND tot.year = lows.year)
LEFT JOIN [DATS221_Project].[dbo].[primary gov gdp] as pri
    ON (tot.geoUnit = pri.geoUnit AND tot.year = pri.year)
LEFT JOIN [DATS221_Project].[dbo].[AG15T24] as ag1
    ON (tot.geoUnit = ag1.geoUnit AND tot.year = ag1.year)
LEFT JOIN [DATS221_Project].[dbo].[AG15T64] as ag2
    ON (tot.geoUnit = ag2.geoUnit AND tot.year = ag2.year)
LEFT JOIN [DATS221_Project].[dbo].[AG25T54] as ag3
    ON (tot.geoUnit = ag3.geoUnit AND tot.year = ag3.year)
LEFT JOIN [DATS221_Project].[dbo].[AG55T64] as ag4
    ON (tot.geoUnit = ag4.geoUnit AND tot.year = ag4.year)
LEFT JOIN [DATS221_Project].[dbo].[indicator-data-export_GER.5T8] as ger
    ON (tot.geoUnit = ger.geoUnit AND tot.year = ger.year)
LEFT JOIN [DATS221_Project].[dbo].[transformed_gdp_data] as gdp
    ON (tot.geoUnit = gdp.Country_Code AND tot.year = gdp.year)
LEFT JOIN [DATS221_Project].[dbo].[transformed_rural_population] as rpop
    ON (tot.geoUnit = rpop.Country_Code AND tot.year = rpop.year)
LEFT JOIN [DATS221_Project].[dbo].[OECD_Education_Ratios] as ptr
    ON (tot.geoUnit = ptr.Country_Code AND tot.year = ptr.year)