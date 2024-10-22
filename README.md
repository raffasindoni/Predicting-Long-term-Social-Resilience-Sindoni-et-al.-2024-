# Predicting-Long-term-Social-Resilience-Sindoni-et-al.-2024-
This GitHub serves as the public repository for the data processing of Sindoni et al. 2024

###
###
###
### (A) Read-Me Details 

# 01_build_wayback_ic_data.R: This script reads in datasets related to intentional communities (IC) from the Wayback Machine. The datasets contain historical information about these communities, and the script processes the data to prepare it for analysis, noting the dependency on the Wayback Machine for archival web data.
 	
# 02_build_present_day_ic_data.R: Similar to the first file, this script focuses on fetching data on intentional communities, but instead of using historical data, it retrieves current data from IC.org for the year 2024. It loads the necessary libraries and reads the dataset to prepare it for further analysis.
 
# 03_combine_ic_builds.R: This script combines the datasets generated in the previous two scripts, merging historical data from the Wayback Machine with the present-day IC.org data. It consolidates the two sources into a unified dataset for further study.

# 04_prepare_build_for_analysis.R: This script further prepares the combined dataset for analysis. It likely involves cleaning, filtering, or transforming the data in a way that makes it more suitable for statistical modeling or visualization.
 
# 05_OLS_regressions_sumstats_visuals.R: This file runs Ordinary Least Squares (OLS) regressions and generates summary statistics and visualizations based on the prepared dataset. 

# 06_pearson_coeff.R: This script calculates Pearson correlation coefficients between variables in the dataset. It focuses on examining the relationships between various factors related to intentional communities, identifying key correlations for analysis.
 
# 07_fig_2.R: This script is  responsible for generating a specific figure (figure 2).



###
###
###
### (B) Scrape Information


######PRESENT
# 2024 Scrape: In February of 2024, all publicly available Intentional Communities were scraped from IC.org, which includes the standard variables listed on the public website. 


#HISTORICAL

# Wayback Machine (2004 – 2023 communities): The following methodology describes the procedure from acquiring past data on Intentional Communitas from www.waybackmachine.com. Because scrapes were crowdsources and sporadic, this does not guarantee that the scraped datasets present in this analysis  are representative of the full universe of all communities. 

# •	2004-2014 Listings on IC.org 
#     1.	A list was put together of all communities using the alphabetical directory on wayback machine (http://directory.ic.org/iclist/cmtylist_A.php). IC stored information about all communities in a directory, with a different page for each letter. Scraper queried Wayback for all of these pages and collected names and URLs for each community. Limitation is not every letter is necessarily scraped at the same time (e.g. cmtylist_A might be scraped on 2005-07-01, but not cmtylist_G). 
#     2.	Using the list of communities in step 1, scraper queried Wayback for collected URLs and extracted relevant fields. The communities collected at this stage are limited by the presence of the community in alphabetical directory from step 1. Further, it is emphasized that a community collected from WayBack by URL does not mean there is a descriptive page available (e.g. knowledge of community with record_id=5937 URL from previous step does not guarantee existence of URL in Wayback).

# •	2014-2024 Listings on IC.org 
#   1.	Using URL Matching
#       •	 After 2014, IC transitioned from record_id model when referring to communities to using name/slug. Scraper queried Wayback for all URLs matching form  http://www.ic.org/directory/<INSERT SLUG HERE>.
#       •	Prior to 2019, IC stored all community data statically in the client HTML, making HTML scraping relatively straightforward. After 2019, IC transitioned to a dynamic loading model where all community data is sourced from an external database and returned in JSON format via a specialized JSON url. Wayback does frequently scrape the community URL, but does not frequently scrape the URL for the JSON data source. Therefore, even though Wayback might have many entries for a community post-2019, the data loaded at runtime usually only reflects the latest scrape of the JSON url. (e.g. Songaia webpage was scraped in 2021, but loads descriptive community data dynamically from 2024). Confidence that the community existed can be established by way of knowing it was archived in Wayback on a certain date. But there are gaps post-2019 in the descriptive data since the JSON URL being stored in Wayback is a prerequisite.
#   2.	Using Regions
#       •	After 2014, IC no longer used an alphabetical directory for displaying all communities and transitioned to a regional model. Scraper queried Wayback for all pages matching United States and all available states.

