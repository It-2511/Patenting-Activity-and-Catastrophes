# Library & Data Loading and naming (total of 5 data sets) --------------------------------------------------

setwd("C:/Users/Tabea/Documents/UNI/BACHELORARBEIT")

library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(fixest)
library(purrr)
library(binsreg)
library(plm)
library(lfe)
library(lmtest)
library(sandwich)
library(data.table)
library(tibble)



# This loads all the relevant data and gives it names. csv_read2 ist used to interprete semicolon-separated Excel sheets (csv_read has comma-separated as default.) 
Catastrophe_data <- read_csv2("Catastrophe_Data.csv")
Location_data <- read_csv2("location_disambiguated.csv")
# The application and assignee data I loaded directly as tsv because both data sets are too large to load in Excel and save as csv (which might be unneccesary anyway tbh). header = TRUE assures that the first row is read as column name and sep="\t" specifies the data to be tab-separated.
Application_data <- read.delim("g_application.tsv", header=TRUE, sep="\t")
Assignee_data <- read.delim("g_assignee_disambiguated.tsv", header=TRUE, sep="\t")

# Load county population data taken from census.gov
County_population <- read_xlsx("County_population_2022.xlsx")
# Load county GDP data taken from bea.gov
County_GDP <- read_xlsx("County_GDP_2022.xlsx")

# I created a dataset of selected counties from Virginia for their 2022 GDP (own calculations). This takes the combination areas from County_GDP, splits them into counties/independent cities and calculates a GDP estimate by multiplying the proportion of population this county has in the combination area with the GDP of the combination area. Whilst this is not perfect, this is as close as it gets because there is no official data on the separate counties and cities of those combination areas.
Virginia_GDP <- read_csv2("Virginia_GDP_2022.csv", col_types = cols(GDP_estimate = col_character()))
# Had GDP_estimate imported as string and then converted to numeric because R was struggeling with the scientific notation from Excel
Virginia_GDP$GDP_estimate <- as.numeric(Virginia_GDP$GDP_estimate)

# Filtering, Renaming and addition of filing_quarter to Application_data-------------------------------------------------

# This Renames County FIPS in the catastrophe data to FIPS in order to make the matching later on easier. 
Catastrophe_data <- Catastrophe_data %>% rename(FIPS = `County FIPS`)
# Remove the '' from County FIPS.
Catastrophe_data$`FIPS` <- gsub("^'|'$", "", Catastrophe_data$`FIPS`)

# This filters out any catastrophes where property damage was zero and removes the * from some of the county names.
Catastrophe_data <- Catastrophe_data %>%
  filter(`PropertyDmg(ADJ 2022)` > 0, Year >= 1976) %>%
  mutate(`County Name` = stringr::str_replace(`County Name`, "^\\*", ""))

# Remove dots used as thousands separators, but keep decimal points by means of telling R to keep only the points preceded by exactly one zero and no other digits. Furthermore convert variable from character to numeric.
Catastrophe_data$`PropertyDmgPerCapita(ADJ 2022)` <- as.numeric(
  gsub("(?<!\\b0)\\.(?=\\d{3})", "", Catastrophe_data$`PropertyDmgPerCapita(ADJ 2022)`, perl = TRUE)
)
# Check for any conversion issues (e.g., NAs after conversion). This gives out FALSE, therefore, there are none.
any(is.na(Catastrophe_data$`PropertyDmgPerCapita(ADJ 2022)`))

# Remove the brackets around some 'city' entries as well as City city cases
Location_data$county <- Location_data$county %>%
  gsub("\\(city\\)", "city", .) %>%      
  gsub("City city$", "City", .)            
# This removes the word 'Parish' from all relevant counties (Louisiana)
Location_data$county <- gsub(" Parish", "", Location_data$county)

# This only considers the entries from the US and removes the three MH state entries, which are the Marshallislands and therefore do not have a FIPS code.
Location_data <- Location_data %>% filter(disambig_country=="US", disambig_state != "MH")
# Remove columns that are not needed
Location_data <- Location_data %>%
  select(-FIPS )

# Restructure FIPS so that it is 5 cipher as performing this within excel caused issues
# Format state_fips as two cipher and county_fips as three cipher
Location_data$state_fips <- sprintf("%02d", as.numeric(Location_data$state_fips))
Location_data$county_fips <- sprintf("%03d", as.numeric(Location_data$county_fips))
# Combine them to be FIPS
Location_data$FIPS <- paste0(Location_data$state_fips, Location_data$county_fips)

# Within the assignee data set, I need to filter out the patent_id entries that include a letter and any entries where the location_id is missing.
Assignee_data <- Assignee_data %>%
  filter(!grepl("[A-Za-z]", patent_id) & !is.na(location_id) & location_id != "")

# Same goes for the application data, only I am adding another filter here: This one drops all values were the year is before 1976 and after 2024, i.e. a) actual patent applications before 1976 and b) possible faulty values
Application_data <- Application_data %>%
  filter(!grepl("[A-Za-z]", patent_id) & 
           as.numeric(substr(filing_date, 1, 4)) >= 1976 & 
           as.numeric(substr(filing_date, 1, 4)) <= 2024)

# Add a variable to get the quarter of the filing date. I decided to aggregate by quarter because the Catastrophe data is based on quarters and therefore, I can match them later on.
# Convert filing_date to date format and extract the month to create a new variable called filing_month. Also convert this new variable to numeric so R can work with it.
Application_data$filing_month <- as.numeric(format(as.Date(Application_data$filing_date, format="%Y-%m-%d"), "%m"))
# Create the variable filing_quarter based on the month the catastrophe happened in 
# Assign quarters based on the month
Application_data$filing_quarter <- with(Application_data,
    ifelse(filing_month >= 1 & filing_month <= 3, 1,            
    ifelse(filing_month >= 4 & filing_month <= 6, 2,            
    ifelse(filing_month >= 7 & filing_month <= 9, 3, 
           4))))    
# Remove filing month
Application_data$filing_month <- NULL

# Manipulation of County_population and County_GDP data ----------------------------------
# This removes rows 1 to 4. The empty space after the comma means "select all columns".
County_population <- County_population[-c(1:4, 3149:3154), ]  
# This removes columns 2 through 5. The empty space before the comma means "select all rows".
County_population <- County_population[ , -c(2:4, 6)] 

# Rename columns one and two accordingly.
colnames(County_population)[1:2] <- c("County", "Population_2022")

# Extract only the County name
# First, remove the leading dot
County_population$County <- sub("^\\.", "", County_population$County)
# Remove everything right to the word County, the word Parish and everything right to a comma
County_population$County <- sub("\\bCounty.*|\\bParish.*|,.*", "", County_population$County)
# Remove trailing spaces
County_population$County <- trimws(County_population$County)

# Add row for Windham and Fairfield, CT
# Manually add the new row to the beginning of County_Population using rbind
County_population <- County_population %>%
  add_row(County = "Windham", Population_2022 = 116418, .before = 1) %>%
  add_row(County = "Fairfield", Population_2022 = 959768, .before = 1)


# County GDP
# Remove columns 2 to 4 and 6 to 10, as well as rows 1 to 5 and 2997 to 3022 (the latter are the Virginia combination areas)
County_GDP <- County_GDP[-c(1:4), ]
County_GDP <- County_GDP[ ,-c(2:4, 6:10)]
County_GDP <- County_GDP[-c(2998:3021), ]
County_GDP <- County_GDP[-c(3195:3198), ]
# I noticed that some counties are called like states which ought to cause trouble in matching and regression results. I started removing the ones that have county duplicates separately, then noticed each state has an NA row before it. So instead, I am looping through the dataset and deleting each row that is preceded by a row of only NAs. I am creating a dataset of the deleted rows so I can check if it is indeed the 50 states. This is the case.
removed_rows <- data.frame()
i <- 1
while (i < nrow(County_GDP)) { # continues as long as i is less than total row number
  if (all(is.na(County_GDP[i, ]))) { # the case of all NAs
    removed_rows <- rbind(removed_rows, County_GDP[i + 1, ]) # stores the removed row in the created data frame
    County_GDP <- County_GDP[-(i + 1), ] # remove row after NA row
  }
  i <- i + 1
}

# Rename both columns
colnames(County_GDP)[1:2] <- c("County", "GDP_2022")

# Convert GDP to numeric
County_GDP$GDP_2022 <- as.numeric(County_GDP$GDP_2022)
# Remove rows containing NAs
County_GDP <- na.omit(County_GDP)

# Multiply GDP_2022 by 1000 to account for the fact that the unit is thousands of dollars 
County_GDP$GDP_2022 <- County_GDP$GDP_2022 * 1000

# Tiny adjustment removing Borough with one of the counties
County_GDP$County <- gsub(" Borough$", "", County_GDP$County)
County_population$County <- gsub(" Borough$", "", County_population$County)

# Remove all variables but GDP and County from Virginia_County_GDP
Virginia_GDP <- Virginia_GDP %>%
  select(-`COMBINATION AREA`, -`GDP Comb. Area`, -`Pop. County/city`, -`Population Comb. Area`, -`Population proportionate`) %>%
  rename(GDP_2022 = GDP_estimate)

# Add Virginia_GDP data to County_GDP after the 2901st row so that there won't be any issues with county_instance later on.
County_GDP <- rbind(County_GDP[1:2901, ], Virginia_GDP, County_GDP[2902:nrow(County_GDP), ])
rm(Virginia_GDP)
# Check for faulty and missing values etc. -------------------------------------
# The following lines check for missing values (NA). They return a logical matrix which has TRUE for NA values. These TRUE entries are then summed up. 
sum(is.na(Location_data))
# This checks for repeated location_id values in the Location_filtered data set. 
Location_data %>%
  group_by(location_id) %>%
  filter(n() > 1) %>%
  ungroup()
# Turns out there are no duplicates of any location_id values in this data set. 

# Checking for missing values in Catastrophe_data.
sum(is.na(Catastrophe_data))

# Check Assignee_data
sum(is.na(Assignee_data))
colSums(is.na(Assignee_data))
# This illustrated that the only data points missing concern assignee_type, therefore I will not do anything about that as all the information relevant for me appears to be complete.

# Check Application_data
sum(is.na(Application_data))
colSums(is.na(Application_data))
# The only missing data points here concern the column rule_47_flag, which does not pose a problem.

#This checks for dates of different format as I assumed there might be some separated by . instead of -
Application_data %>%
  filter(!grepl("^\\d{4}-\\d{2}-\\d{2}$", filing_date))

# This displays the number of groups, which should equal the number of unique patent ids. That turns out to be the case. 
n_groups <- nrow(Location_Patentcount)
n_groups

# Matching by location id -----------------------------------------

# This groups the assignee data by location_id to figure out how many patents there are for each location id and match with this. 
Location_Patentcount <- Assignee_data %>%
  group_by(location_id) %>%
  summarise(patent_count = n())

# This matches the Location_Patentcount by location_id with the respective other data set, i.e. Location_data.The resulting data set only keeps the entries from Location_Patentcount (i.e. originally Assignee_data) that have a match in Location_data_filtered, i.e. only the patents from the US. It also removes variables that aren't needed and sorts the data according to FIPS.
Matched_location_id_data <- Location_Patentcount %>%
  inner_join(Location_data, by = "location_id") %>%
  select(-disambig_country, -latitude, -longitude) %>%
  arrange(FIPS)

# Matching Assignee and Application data by patent id ----------------------------------------------------------------
# First, I will match the Assignee and the Application data by the shared variable patent_id.Also removing several variables from the joint data that I will not need and likely won't interest me regarding the analysis. 
Matched_patent_id_data <- Application_data %>%
  inner_join(Assignee_data, by = "patent_id") %>%
  select(location_id, everything()) %>% # Placing location_id at utmost left
  select(-patent_application_type, -series_code, -rule_47_flag, 
         -assignee_sequence, -disambig_assignee_individual_name_first, 
         -disambig_assignee_individual_name_last, -assignee_type)

# Define a data frame that includes the location_ids as well as the filing dates and filing quarters.
patent_filings_by_location <- Matched_patent_id_data %>%
  group_by(location_id) %>%
  summarise(
    filing_year = paste(filing_date, collapse = ","),
    filing_quarter = paste(filing_quarter, collapse = ",")
  )
# Now remove month and day so that only the year remains.
patent_filings_by_location <- patent_filings_by_location %>%
  mutate(filing_year = str_extract_all(filing_year, "\\d{4}") %>% # Extracts all 4 digit sequences and makes a list of these
           sapply(function(x) paste(x, collapse = ","))) # Collapses the years into a comma-separated string

# Next, I would like to sum up the number of patents and list all 
FIPS_Patentcount_with_dates <- Matched_location_id_data %>%
  left_join(patent_filings_by_location, by = "location_id") %>%
  group_by(FIPS, county, disambig_state) %>%
  summarise(
    total_patent_count = sum(patent_count),
    location_ids = paste(unique(location_id), collapse = ", "),
    filing_year = paste(filing_year, collapse = ", "),
    filing_quarter = paste(filing_quarter, collapse = ", ")
  ) %>%
  arrange(FIPS)

# The following code displays the number of patent ids that are unmatched in both datasets, just so I know how many that is.
Application_data %>%
  anti_join(Assignee_data, by = "patent_id") %>%
  nrow()
Assignee_data %>%
  anti_join(Application_data, by = "patent_id") %>%
  nrow()

# Visualisation of Catastrophe impact (More precisely: LogPropertyDmgPerCapita(ADJ 2022) and LogPropertyDmg(ADJ 2022)) -----------------------------------------------------------
# Now I want to have a closer look at the Catastrophe data, namely the scope of the impact, by looking at the variables PropertyDmgPerCapita(ADJ 2022) and PropertyDmg(ADJ 2022).

# Display summaray statistics, quantiles and a boxplot to get an idea of the distribution and identify outliers. 
summary(Catastrophe_data$`PropertyDmgPerCapita(ADJ 2022)`)
quantile(Catastrophe_data$`PropertyDmgPerCapita(ADJ 2022)`, probs = c(0.95, 0.99))

# Before this, I had a look at the box plot of the data and identified some extreme outliers. Originally, I filtered to have those removed. However, since I decided on a log scale, this was not needed anymore. So here, I applied a log transformation to the data and visualized. Without the log, the shape of the histogram was of little use.
hist(log10(Catastrophe_data$`PropertyDmgPerCapita(ADJ 2022)` + 1),  # Add 1 to avoid log(0) issues
     main = "Log Distribution of PropertyDmgPerCapita(ADJ 2022)",
     xlab = "Log10(PropertyDmgPerCapita(ADJ 2022))",
     breaks = 30,
     col = "lightblue",
     border = "black")


# I also want to have a look at just PropertyDmg(ADJ 2022) since I think this could also serve as a proxy for the impact of the catastrophe. Therefore, I will repeat that process.

# First, I will take a look at the raw boxplot to identify likely outliers and decide if I will be using a log scale.
boxplot(Catastrophe_data$`PropertyDmg(ADJ 2022)`, main = "Boxplot of PropertyDmg(ADJ 2022)", ylab = "PropertyDmg(ADJ 2022)")

# Apply a log transformation and create a histogram as done before with PropertyDmgPerCapita
hist(log10(Catastrophe_data$`PropertyDmg(ADJ 2022)` + 1),  # Add 1 to avoid log(0) issues
     main = "Log-Transformed Distribution of PropertyDmg (ADJ 2022)",
     xlab = "Log10(PropertyDmg (ADJ 2022))",
     breaks = 30,
     col = "lightblue",
     border = "black")


# Final matching by FIPS --------------------------------
# First, I need to group the catastrophes by FIPS. The information about the year, as well as PropertyDmg and PropertyDmgPerCapita as the proxies of the impact of the catastrophe are retained. 
Catastrophe_summarized <- Catastrophe_data %>%
  group_by(FIPS) %>%
  summarize(
    num_catastrophes = n(),
    Hazard = paste(Hazard, collapse = ";"),            
    Year = paste(Year, collapse = ";"),                
    PropertyDmg = paste(`PropertyDmg(ADJ 2022)`, collapse = ";"),
    PropertyDmgPerCapita = paste(`PropertyDmgPerCapita(ADJ 2022)`, collapse = ";"),
    Hazard_quarter = paste(Quarter, collapse = ";")
  )

# Then, merge with FIPS_Patentcount_with_dates by FIPS and rename some variables.
Final_data <- FIPS_Patentcount_with_dates %>%
  left_join(Catastrophe_summarized, by = "FIPS") %>%
  rename(
    Patentcount = total_patent_count,
    Hazard_year = Year,
    State = disambig_state,
    Filing_year = filing_year
  )

# Apply a log-transformation to PropertyDmg and PropertyDmgPerCapita and create two new variables for that
Final_data <- Final_data %>%
  mutate(
    `Log-PropertyDmg` = sapply(strsplit(PropertyDmg, ";"), function(x) paste(log10(as.numeric(x)), collapse = ";")),
    `Log-PropertyDmgPerCapita` = sapply(strsplit(PropertyDmgPerCapita, ";"), function(x) paste(log10(as.numeric(x)), collapse = ";"))
  )

# Filter out all FIPS codes that aren't mainland US (though this is optional) as well as any FIPS codes without catastrophes, and FIPS of Patentcount 0.
Final_data <- Final_data %>%
  filter(as.numeric(substr(FIPS, 1, 2)) <= 56, 
         !is.na(num_catastrophes), 
         Patentcount > 0) %>%
  select(-location_ids)

# Minor tweak, which would've made sense to incorporate before but it's easier now this way.
Final_data <- Final_data %>%
  mutate(county = ifelse(county == "La Salle", "LaSalle", county))

# This mutates Final_data with Hazard_years to add hazards to the data set
Final_data_hazard <- Final_data %>%
  mutate(Hazard_year = strsplit(Hazard_year, ";"),
         Hazard_quarter = strsplit(Hazard_quarter, ";"),
         Hazard = strsplit(Hazard, ";"),
         PropertyDmg = strsplit(PropertyDmg, ";"),
         PropertyDmgPerCapita = strsplit(PropertyDmgPerCapita, ";"),
         `Log-PropertyDmg` = strsplit(`Log-PropertyDmg`, ";"),
         `Log-PropertyDmgPerCapita` = strsplit(`Log-PropertyDmgPerCapita`, ";")) %>%
  unnest(cols = c(Hazard_year, PropertyDmg, Hazard, PropertyDmgPerCapita, `Log-PropertyDmg`, `Log-PropertyDmgPerCapita`, Hazard_quarter)) %>%  
  mutate(Hazard_year = as.numeric(Hazard_year),   
         PropertyDmg = as.numeric(PropertyDmg),     
         PropertyDmgPerCapita = as.numeric(PropertyDmgPerCapita),
         `Log-PropertyDmg` = as.numeric(`Log-PropertyDmg`),
         `Log-PropertyDmgPerCapita` = as.numeric(`Log-PropertyDmgPerCapita`),
         Hazard_quarter = as.numeric(Hazard_quarter)) %>%
  rename(County = county)

# Join with County_GDP and County_population
Final_data_hazard <- Final_data_hazard %>%
  left_join(County_GDP %>% 
              select(County, GDP_2022) %>% 
              distinct(County, .keep_all = TRUE), 
            by = "County") %>%
  left_join(County_population %>% 
              select(County, Population_2022) %>% 
              distinct(County, .keep_all = TRUE), 
            by = "County") %>%
# Now, create the county_instance variable manually
  arrange(County, FIPS) %>%  # Sort to ensure consistency by County and FIPS
  group_by(County) %>%
  mutate(county_instance = cumsum(!duplicated(FIPS))) %>%
  ungroup()

Final_data_hazard <- Final_data_hazard %>%
  # Remove leading/trailing spaces in Hazard for consistent matching
  mutate(Hazard = trimws(Hazard)) %>%
  # Group by FIPS, PropertyDmg, and Hazard_years to identify duplicates
  group_by(FIPS, PropertyDmg, Hazard_year) %>%
  # Remove any catastrophes of the same size for the same county and year since I noticed a lot of duplicate entries with different Hazard types. I am not preferencing certain hazard types in any way. This is accomplished by using a filter because the repeated values aren't neccessarily below one another.
  filter(row_number() == 1) %>%
  # Ungroup to revert the dataset to its original structure
  ungroup()

# Trial visualisation of patenting activity and catastrophes FIPS 01001 ---------------------------------------
# As a trial run, I would now like to display cumulative patents and catastrophes of FIPS 01001. 
# Filter data to visuaize trend for FIPS 01001 and split each year variable into separate years due to them being interpreted as characters
Final_data %>%
  filter(FIPS == '01001') %>%
  mutate(Filing_years = strsplit(Filing_years, ","),
         Hazard_years = strsplit(Hazard_years, ";")) -> final_data_filtered_fips

# Extract the filing and hazard years
filing_years <- unlist(final_data_filtered_fips$Filing_years)
hazard_years <- unlist(final_data_filtered_fips$Hazard_years)

# Convert both those years to numeric
filing_years <- as.numeric(filing_years)
hazard_years <- as.numeric(hazard_years)

# Count the occurrences of each year
patent_counts <- table(filing_years)
hazard_counts <- table(hazard_years)

# Define the year span for the x-axis from 1976 to 2024
years <- 1976:2024

# Cumulative sums for patents and catastrophes
patent_data <- cumsum(sapply(years, function(year) patent_counts[as.character(year)] %>% replace_na(0)))
hazard_data <- cumsum(sapply(years, function(year) hazard_counts[as.character(year)] %>% replace_na(0)))

# Plotting cumulative sums with lines and points
plot(years, patent_data, type = "o", col = "blue", lwd = 2, ylim = c(0, max(c(patent_data, hazard_data))),
     ylab = "Cumulative Count", xlab = "Year", main = "Cumulative Patents and Catastrophes for FIPS 01001 (1974-2024)", cex.main = 0.99)
lines(years, hazard_data, type = "o", col = "red", lwd = 2)
legend("topleft", legend = c("Patents", "Catastrophes"), col = c("blue", "red"), lwd = 2)



# Removal of data frames not needed anymore -------------------------------
rm(Assignee_data)
rm(Application_data)
gc()
# Pre-Regression merging/amendments of data sets ------------------------------------------------------------
# Ensure Hazard_years in Final_data_hazard is numeric (this will be needed later on and I put it here for better oversight)
Final_data_hazard <- Final_data_hazard %>%
  mutate(Hazard_year = as.numeric(Hazard_year),
         Hazard_quarter = as.numeric(Hazard_quarter))

# Idea: Create a data set that groups by fips and then adds the years 1976 to 2024 as well as four quarters for each year. 
# Extract the FIPS, county, and State from the previous dataset
fips_data <- Final_data %>%
  select(FIPS, county, State) %>%
  distinct()  # Ensure no duplicates
# Create a sequence of years from 1976 to 2024
years <- 1976:2024
# Use tidyr's crossing() function to create combinations of FIPS and years
fips_data <- fips_data %>%
  tidyr::crossing(Year = years) 
# Rename Year in fips_data to avoid duplicate column names
fips_data <- fips_data %>%
  rename(FIPS_Year = Year)
# Add a variable Year_Quarter of the format Q that has four quarters for each year
fips_data<- fips_data %>%
  group_by(FIPS, county, State, FIPS_Year) %>%
  reframe(Quarter = 1:4) %>%
  ungroup()

# Left-join with Final_data_hazard to add hazard-related information in two steps to keep county_instance in tact and not have NA values there
fips_data<- fips_data %>%
  left_join(
    Final_data_hazard %>%
      select(FIPS, county_instance) %>%
      distinct(),  
    by = "FIPS"
  )
fips_data <- fips_data %>%
  left_join(
    Final_data_hazard %>%
      select(FIPS, Hazard_year, Hazard_quarter, Hazard, PropertyDmg, PropertyDmgPerCapita, `Log-PropertyDmg`, `Log-PropertyDmgPerCapita`),
    by = c("FIPS", "FIPS_Year" = "Hazard_year", "Quarter" = "Hazard_quarter")
  )

# ALTERNATIVE APPROACH TO 'PATENTCOUNTS' IN ORDER TO INCLUDE FILING_QUARTER
# Join Location_data and Matched_patent_id_data by patent_id, displaying FIPS, filing_date and filing_quarter in the resulting data set. Also extract the year from filing_date and rename filing_year.
Patentsummary <- Location_data %>%
  inner_join(Matched_patent_id_data, by = "location_id") %>%
  select(FIPS, filing_year = filing_date, filing_quarter) %>%
  mutate(filing_year = as.numeric(substr(filing_year, 1, 4))) %>%  
  arrange(FIPS, filing_year) 
# Add number of patents per aggregated quarter, year and FIPS combination 
Patentsummary <- Patentsummary %>%
  group_by(FIPS, filing_year, filing_quarter) %>%
  mutate(Number_of_patents_FIPSquarter = n()) %>%
  ungroup()
# Aggregate by these three variables
Patentsummary <- Patentsummary %>%
  group_by(FIPS, filing_year, filing_quarter) %>%
  summarise(
    Number_of_patents_FIPSquarter = first(Number_of_patents_FIPSquarter)  
  ) %>%
  ungroup()

# This joins the data sets fips_data and Patentsummary in order to get sort of a time series of patent filings and catastrophes.
fips_data <- fips_data %>%
  left_join(Patentsummary %>%
              rename(Patentcount = Number_of_patents_FIPSquarter),  
            by = c("FIPS", "FIPS_Year" = "filing_year", "Quarter" = "filing_quarter" )) 
# Extract the first two digits (state FIPS) in fips_data
fips_data <- fips_data %>%
  mutate(state_fips = substr(FIPS, 1, 2)) %>%
  relocate(state_fips, .after = FIPS)

# Create a county_instance within each state (only one instance per county within the state) and count this so the first county of a name occurring multiple times will be 1, the next county of the same name will be 2 etc. 
County_population <- County_population %>%
  group_by(County) %>%
  mutate(county_instance = row_number())  
# Do the same for the GDP data
County_GDP <- County_GDP %>%
  group_by(County) %>%
  mutate(county_instance = row_number())

# Join the fips_data set with population and gdp data by count_instance
fips_data_with_population <- fips_data %>%
  left_join(County_population, by = c("county" = "County", "county_instance"))
fips_data_population_and_gdp <- fips_data_with_population %>%
  left_join(County_GDP, by = c("county" = "County", "county_instance"))
# Remove data set with just population
rm(fips_data_with_population)

# Add a variable for GDP per capita
fips_data_population_and_gdp <- fips_data_population_and_gdp %>%
  mutate(GDP_per_capita = GDP_2022 / Population_2022)
# Add a variable for Property Damage in relation to the size of the economy
fips_data_population_and_gdp <- fips_data_population_and_gdp %>%
  mutate(PropertyDmg_per_GDP = (PropertyDmg/GDP_2022) )

# One problem appears to be that PropertyDmgPerCapita in the Catastrophe dataset is not converted consistenly; a lot of the values are much too small. I entertainted the idea that PropertyDmg might be for all affected counties and PropertyDmgPerCapita county-wise, but this does not seem to be the case. Now assuming PropertyDmg and PropertyDmgPerCapita in the original dataset are both calculated county-wise, I can come up with a somewhat representative proxy for PropertyDmgPerCapita by calculating it with Population_2022. This does not take into account different population sizes in different years of course, but neither does the original SHELDUS data according to their website if I understood it correctly. 
fips_data_population_and_gdp <- fips_data_population_and_gdp %>%
  mutate(PropertyDmgPerCapita_corrected = if_else(Population_2022 > 0, PropertyDmg / Population_2022, NA_real_),
         Log_PropertyDmgPerCapita_manual = if_else(PropertyDmgPerCapita_corrected > 0, log10(PropertyDmgPerCapita_corrected), 0))


# Replace NAs of relevant numeric variables with 0s
fips_data_population_and_gdp <- fips_data_population_and_gdp %>%
  mutate(across(c(PropertyDmg, PropertyDmgPerCapita, Patentcount, `Log-PropertyDmg`, `Log-PropertyDmgPerCapita`, PropertyDmg_per_GDP, PropertyDmgPerCapita_corrected, Log_PropertyDmgPerCapita_manual), ~replace_na(., 0)))
# Remove variable Hazard so its NAs don't cause problems 
fips_data_population_and_gdp <- fips_data_population_and_gdp %>%
  select(-Hazard, -PropertyDmgPerCapita, -`Log-PropertyDmgPerCapita`)
fips_data_population_and_gdp <- fips_data_population_and_gdp %>%
  rename("Year" = "FIPS_Year")


# The regression returns a lot of NA values at the moment, which is due to the data sets County GDP and County Population not containing the right aggregation for Connecticut. I decided to add the County Population data for Connecticut manually since it's only a few counties and that way I won't encounter any more problems merging increasingly complicated data sets. The data points are estimates for 2021 from ct.gov.

# Create a new data frame containing only NAs so I know what to change
# Filter observations with NA in GDP_per_capita
na_observations <- fips_data_population_and_gdp %>%
  filter(is.na(GDP_per_capita))
view(na_observations)


fips_data_population_and_gdp <- fips_data_population_and_gdp %>%
  dplyr::mutate(Population_2022 = case_when(
    FIPS == "09003" ~ 896854, #Hartford
    FIPS == "09005" ~ 185000, #Litchfield
    FIPS == "09007" ~ 164759, #Middlesex
    FIPS == "09009" ~ 863700, #New Haven
    FIPS == "09011" ~ 268605, #New London
    FIPS == "09013" ~ 150293, #Tolland
    TRUE ~ Population_2022
  ))

# Re-run GDP per capita caculation with the updated data 
fips_data_population_and_gdp <- fips_data_population_and_gdp %>%
  mutate(GDP_per_capita = GDP_2022 / Population_2022)

# Regression I (Simple) ---------------------------------------------------
# Run a weighted regression using GDP per capita as weight, Year as fixed effect, clustering by FIPS code and using Log-PropertyDmg as independent variable since PropertyDmgPerCapita in the original dataset (Catastrophe_data) appears to have been inconsistently converted. Therefore, it does not make sense to apply a filter of Log-PropertyDmgPerCapita as done in the paper.
model_1 <- feols(Patentcount ~ `Log-PropertyDmg` | Year,
        data = fips_data_population_and_gdp,
        weights = ~ GDP_per_capita, 
        cluster = ~ FIPS)

summary(model_1)
# Estimate Year FE: 0.227, *
# Estimate Year & Quarter FE: 0.229, not significant
# Note: Log-PropertyDmg is likely not a 

# This performs the regression with PropertyDmg_per_GDP as the independent variable
model_2 <- feols(Patentcount ~ PropertyDmg_per_GDP | Year + Quarter,
        data = fips_data_population_and_gdp,
        weights = ~ GDP_per_capita, 
        cluster = ~ FIPS)

summary(model_2)
# Estimate for Year FE: -6.4e-05, *
# Estimate for Year & Quarter FE: -4.6e-05, *

# This takes the above created variable Log-PropertyDmgPerCapita_manual as independent variable. 
model_3 <- feols(Patentcounts ~ Log_PropertyDmgPerCapita_manual | Year,
                 data = fips_data_population_and_gdp,
                 weights = ~ GDP_per_capita,
                 cluster = ~ FIPS)

summary(model_3)
# Estimate: 0.791, not significant

# Visualisation II ------------------------------------------------------
# Boxplot of PropertyDmg_per_GDP
ggplot(fips_data_population_and_gdp, aes(y = total_PropertyDmg_per_GDP)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of PropertyDmg_per_GDP",
       y = "PropertyDmg_per_GDP") +
  theme_minimal()

# Removing data frames that are not needed to free up space
rm(Application_data, Assignee_data, County_GDP, County_population, data_no_zeros, duplicate_entries, fips_data, Catastrophe_summarized, FIPS_Patentcount_with_dates, Location_data, Location_Patentcount, Matched_location_id_data, Matched_patent_id_data, patent_filings_by_location, patentcounts, Final_data)
gc()


# Binned Regression -------------------------------------------------------
# Create the disaster_category based on PropertyDmg_per_GDP bins
fips_data_population_and_gdp <- fips_data_population_and_gdp %>%
  mutate(disaster_category = case_when( 
    PropertyDmg_per_GDP <= 0.01 ~ 1,  # No Disaster (1% or less of GDP)
    PropertyDmg_per_GDP > 0.01 & PropertyDmg_per_GDP <= 0.1 ~ 2,  # Small Disaster (Between 1% and 10% of GDP)
    PropertyDmg_per_GDP > 0.1 & PropertyDmg_per_GDP <= 1 ~ 3,  # Medium Disaster (Between 10% and 100% of GDP)
    PropertyDmg_per_GDP > 1 & PropertyDmg_per_GDP <= 5 ~ 4,  # Large Disaster (Between 100% and 500% of GDP)
    PropertyDmg_per_GDP > 5 ~ 5  # Very Large Disaster (More than 500% of GDP)
  ))

# Create the disaster_category based on PropertyDmg_per_GDP bins
fips_data_population_and_gdp <- fips_data_population_and_gdp %>%
  mutate(disaster_category = case_when(
    PropertyDmg_per_GDP <= 0.01 ~ 1,  # No Disaster (1% or less of GDP)
    PropertyDmg_per_GDP > 0.01 & PropertyDmg_per_GDP <= 0.1 ~ 2,  # Small Disaster (Between 1% and 10% of GDP)
    PropertyDmg_per_GDP > 0.1 & PropertyDmg_per_GDP <= 1 ~ 3,  # Medium Disaster (Between 10% and 100% of GDP)
    PropertyDmg_per_GDP > 1 & PropertyDmg_per_GDP <= 5 ~ 4,  # Large Disaster (Between 100% and 500% of GDP)
    PropertyDmg_per_GDP > 5 ~ 5  # Very Large Disaster (More than 500% of GDP)
  ))

# Run the binned regression using disaster_category as categorial variable and with fixed effects for FIPS and Year
# Using felm because it is designated for large datasets with multiple fixed effects 
binned_model <- felm(
  Patentcounts ~ factor(disaster_category) | FIPS + Year,
  data = fips_data_population_and_gdp
)

summary(binned_model)
# Estimate cat. 2: 7.88 ***
# Estimate cat. 3: 1.59 **
# Estimate cat. 4: -0.74
# Estimate cat. 5: -0.72 *

# Dynamic Regression ------------------------------------------------------
# Run a fixed effects panel regression, controlling for FIPS and Year fixed effects. Issue with this is that by using FIPS and Year separately, I do not account for the case of multiple disaster events within the same county and year.
# felm factors in the variance explained by the fixed effects, which is why the R2 is so high. 
simple_panel_model <- felm(
  Patentcounts ~ disaster_category | FIPS + Year,
  data = fips_data_population_and_gdp
)

summary(simple_panel_model)
# Estimate: -0.385, ***, R2=0.82

# Different method for running panel regression
# First, aggregate PropertyDmg_per_GDP by FIPS and Year
fips_data_population_and_gdp <- fips_data_population_and_gdp %>%
  group_by(FIPS, Year) %>%
  summarize(
    total_PropertyDmg_per_GDP = sum(PropertyDmg_per_GDP, na.rm = TRUE),  # Sum damage per GDP for the year in order to just have one entry per FIPS and Year 
    Patentcounts = first(Patentcounts)  
  ) %>%
  ungroup()

# Assign each FIPS-Year pair to a disaster category based on the aggregated damage and the previously determined bins. So this is similar to the code above but note to self NOT THE SAME
fips_data_population_and_gdp <- fips_data_population_and_gdp %>%
  mutate(disaster_category = case_when(
    total_PropertyDmg_per_GDP == 0 ~ 1,  # No Disaster
    total_PropertyDmg_per_GDP > 0 & total_PropertyDmg_per_GDP <= 0.01 ~ 2, # Small Disaster
    total_PropertyDmg_per_GDP > 0.01 & total_PropertyDmg_per_GDP <= 0.1 ~ 3,  # Medium Disaster
    total_PropertyDmg_per_GDP > 0.1 & total_PropertyDmg_per_GDP <= 1 ~ 4,  # Large Disaster
    total_PropertyDmg_per_GDP > 1 ~ 5,  # Very Large Disaster
  ))

# Regression 2 
model_formula <- Patentcounts ~ factor(disaster_category)

# Run the panel regression with fixed effects for FIPS and Year via index
# plm's R2 focuses on the variance explained after removing the fixed effects, which is why the R2 is typically much lower
panel_model_elevated <- plm(
  formula = model_formula,
  data = fips_data_population_and_gdp,
  index = c("FIPS", "Year"),
  model = "within"
)

summary(panel_model_elevated)

# Heteroskedasticity-robust standard errors
coeftest(panel_model_elevated, vcov = vcovHC(panel_model_elevated, type = "HC3"))