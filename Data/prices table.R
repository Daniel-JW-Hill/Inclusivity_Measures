rm(list = ls())

#Set WD
library(here)
here()

#Load raw analysis data. 
library(readxl)
data = readxl::read_xlsx(here('Data',"Complete_data_final.xlsx"))

# Initial data file restructuring and edits to IDs and other important information.
source(here('Utilities',"initial_data_clean.R"))
data = initial_data_clean(data)

source(here('Utilities',"clean_GPS_variables.R"))
data = clean_GPS_variables(data)

# # Retrieve spatial data for each observation based on village GPS coords. 
source(here('Utilities',"get_spatial_data.R"))
data = get_spatial_data(data)

# Retrieve proxy asset valuations - indiv, others, and total
source(here('Utilities',"get_asset_values.R"))
data = get_asset_values(data)

# Clean and retrieve outcome participation measures
source(here('Utilities',"clean_coffee_marketing.R"))
source(here('Utilities',"get_participation_outcomes.R"))
data = clean_coffee_marketing(data)
data = get_participation_outcomes(data)

# Tidy up some of the Household and individual characteristics
source(here('Utilities', "clean_characteristics.R"))
data = clean_characteristics(data)

# Tidy up coffee tree and tenure information 
source(here('Utilities', "clean_land.R"))
data = clean_land(data)

# Retrieve relative price information
source(here('Utilities', "get_relative_prices.R"))
data = get_relative_prices(data)


data$price_summary = rowMeans(cbind(data$PRICE_HH_KAWAKOM,
                              data$PRICE_HH_GREATLAKES,
                              data$PRICE_HH_KYAKALANYI,
                              data$PRICE_HH_INTERSECTION,
                              data$PRICE_HH_FARMERGROUP,
                              data$PRICE_HH_OLAM,
                              data$PRICE_HH_TRADER,
                              data$PRICE_HH_OTHER), na.rm = TRUE)
summary(data$price_summary
        )
which(is.na(data$price_summary))

