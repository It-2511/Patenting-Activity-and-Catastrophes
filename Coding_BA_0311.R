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

# This loads all the relevant data and gives it names. csv_read2 ist used to interprete semicolon-separated Excel sheets (csv_read has comma-separated as default.) 
Catastrophe_data <- read_csv2("Catastrophe_Data.csv")
Location_data <- read_csv2("location_disambiguated_2.1.1.csv")
# The application and assignee data I loaded directly as tsv because both data sets are too large to load in Excel and save as csv (which might be unneccesary anyway tbh). header = TRUE assures that the first row is read as column name and sep="\t" specifies the data to be tab-separated.
Application_data <- read.delim("g_application.tsv", header=TRUE, sep="\t")
Assignee_data <- read.delim("g_assignee_disambiguated.tsv", header=TRUE, sep="\t")

# Load county population data taken from census.gov
County_population <- read_xlsx("County_population_2022.xlsx")
# Load county GDP data taken from bea.gov
County_GDP <- read_xlsx("County_GDP_2022.xlsx")

# Filtering and Renaming-------------------------------------------------

# This Renames County FIPS in the catastrophe data to FIPS in order to make the matching later on easier. 
Catastrophe_data <- Catastrophe_data %>% rename(FIPS = `County FIPS`)
# Remove the '' from County FIPS to be able to match the data later on.
Catastrophe_data$`FIPS` <- gsub("^'|'$", "", Catastrophe_data$`FIPS`)

# This filters out any catastrophes where property damage was zero and removes the * from some of the county names.
Catastrophe_data <- Catastrophe_data %>%
  filter(`PropertyDmg(ADJ 2022)` > 0, Year >= 1976) %>%
  mutate(`County Name` = stringr::str_replace(`County Name`, "^\\*", ""))

# Remove dots used as thousands separators, but keep decimal points by means of telling R to keep only the points preceded by exactly one zero and no other digits. Furthermore, convert variable from character to numeric.
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

# Same goes for the application data, only I am adding another filter here: This one drops all values were the year is before 1976 and after 2024, i.e. a) actual patent applications before 1976 and b) faulty values
Application_data <- Application_data %>%
  filter(!grepl("[A-Za-z]", patent_id) & 
           as.numeric(substr(filing_date, 1, 4)) >= 1976 & 
           as.numeric(substr(filing_date, 1, 4)) <= 2024)

# Manipulation of County_population and County_GDP data ----------------------------------
# Starting with county population
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

# County GDP
# Remove columns 2 to 4 and 6 to 10, as well as rows 1 to 5
County_GDP <- County_GDP[-c(1:5), ]
County_GDP <- County_GDP[ ,-c(2:4, 6:10)]

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
# First, I will match the Assignee and the Application data by the shared variable patent_id.I am also moving patent_id to the utmost left for better overview and removing several variables from the joint data set that I will not need. 
Matched_patent_id_data <- Application_data %>%
  inner_join(Assignee_data, by = "patent_id") %>%
  select(location_id, everything()) %>%
  select(-patent_application_type, -series_code, -rule_47_flag, 
         -assignee_sequence, -disambig_assignee_individual_name_first, 
         -disambig_assignee_individual_name_last, -assignee_type)

# Define a variable that includes the location_ids as well as the filing dates
patent_filings_by_location <- Matched_patent_id_data %>%
  group_by(location_id) %>%
  summarise(filing_dates = paste(unique(filing_date), collapse = ","))
# Now remove month and day so that only the year remains.
patent_filings_by_location <- patent_filings_by_location %>%
  mutate(filing_dates = sapply(strsplit(filing_dates, ","), 
                               function(x) paste(as.numeric(substr(x, 1, 4)), collapse = ",")))


# Next, I would like to sum up the number of patents and list all 
FIPS_Patentcount_with_dates <- Matched_location_id_data %>%
  left_join(patent_filings_by_location, by = "location_id") %>%
  group_by(FIPS, county, disambig_state) %>%
  summarise(
    total_patent_count = sum(patent_count),
    location_ids = paste(unique(location_id), collapse = ", "),
    filing_dates = paste(unique(filing_dates), collapse = ", ")
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
    Hazard = paste(Hazard, collapse = "; "),            
    Year = paste(Year, collapse = "; "),                
    PropertyDmg = paste(`PropertyDmg(ADJ 2022)`, collapse = "; "),
    PropertyDmgPerCapita = paste(`PropertyDmgPerCapita(ADJ 2022)`, collapse = "; ")
  )

# Then, merge with FIPS_Patentcount_with_dates by FIPS and rename some variables.
Final_data <- FIPS_Patentcount_with_dates %>%
  left_join(Catastrophe_summarized, by = "FIPS") %>%
  rename(
    Patentcount = total_patent_count,
    Hazard_years = Year,
    State = disambig_state,
    Filing_years = filing_dates
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

# This mutas Final_data with Hazard_years to add hazards to the data set
Final_data_hazard <- Final_data %>%
  mutate(Hazard_years = strsplit(Hazard_years, ";"),
         PropertyDmg = strsplit(PropertyDmg, ";"),
         `Log-PropertyDmg` = strsplit(`Log-PropertyDmg`, ";"),
         Hazard = strsplit(Hazard, ";"),
         PropertyDmgPerCapita = strsplit(PropertyDmgPerCapita, ";"),
         `Log-PropertyDmgPerCapita` = strsplit(`Log-PropertyDmgPerCapita`, ";")) %>%
  unnest(cols = c(Hazard_years, PropertyDmg, Hazard, PropertyDmgPerCapita, `Log-PropertyDmg`, `Log-PropertyDmgPerCapita`)) %>%  
  mutate(Hazard_years = as.numeric(Hazard_years),   
         PropertyDmg = as.numeric(PropertyDmg),     
         PropertyDmgPerCapita = as.numeric(PropertyDmgPerCapita),
         `Log-PropertyDmg` = as.numeric(`Log-PropertyDmg`),
         `Log-PropertyDmgPerCapita` = as.numeric(`Log-PropertyDmgPerCapita`)) %>%
  rename(County = county)

# Work with a priority list to keep/eliminate certain counties because there is no GDP/Population data available and the matching is not done right.
# Define a priority list as a list of vectors, where each county has a vector of preferred states
priority_states <- list(
  "La Salle" = c("TX"),
  "Windham" = c("VT"),
  "Fairfield" = c("OH", "SC")
)

# Filter final_data_hazard to keep only preferred states for each county if listed in priority_states
Final_data_hazard <- Final_data_hazard %>%
  filter(!(County %in% names(priority_states) & !State %in% priority_states[County]))

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
  group_by(FIPS, PropertyDmg, Hazard_years) %>%
  # Filter out 'Wind' entries if there's a matching 'Severe Storm/Thunder Storm'
  filter(!(Hazard == "Wind" & any(Hazard == "Severe Storm/Thunder Storm"))) %>%
  # Ungroup to revert the dataset to its original structure
  ungroup()

# Trial visualisation of patenting activity and catastrophes FIPS 01001 ---------------------------------------
# As a trial run, I would now like to display cumulative patents and catastrophes of FIPS 01001. 
# Filter data to visuaize trend for FIPS 01001 and split each year variable into separate years due to them being interpreted as characters
Final_data_filtered %>%
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



# Regression I (Simple) ------------------------------------------------------------
# General approach: Create a data set for each year for each FIPS.

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

# Apply priority list filter as with Final_data_hazard
fips_data <- fips_data %>%
  filter(!(county %in% names(priority_states) & !State %in% unlist(priority_states[county])))


# Ensure Hazard_years in Final_data_hazard is numeric
Final_data_hazard <- Final_data_hazard %>%
  mutate(Hazard_years = as.numeric(Hazard_years))

# Now, join fips_data with only the selected columns from Final_data_hazard by FIPS and Year
# Step 1: Join `county_instance` separately to avoid NA values
fips_data <- fips_data %>%
  left_join(Final_data_hazard %>%
              select(FIPS, county_instance) %>%
              distinct(), # Only unique FIPS-county_instance pairs
            by = "FIPS") %>%
# Step 2: Add Hazard information, allowing county_instance to stay intact
  left_join(Final_data_hazard %>%
              select(FIPS, Hazard_years, Hazard, PropertyDmg, PropertyDmgPerCapita, `Log-PropertyDmg`, `Log-PropertyDmgPerCapita` ),
            by = c("FIPS", "FIPS_Year" = "Hazard_years"))


# This creates a data set 'patentcounts' that has one entry for each year patents were filed as well as the number of filed patents. 
# Split Filing_years into lists
patentcounts <- Final_data %>%
  mutate(Filing_years = strsplit(Filing_years, ",")) %>%  
  unnest(cols = c(Filing_years)) %>%  # Unnest to expand rows
  mutate(Filing_years = as.numeric(Filing_years)) %>%  
  group_by(FIPS, Filing_years) %>%  
  summarise(Number_of_patents = n(), .groups = "drop")  

# This joins the data sets fips_data_expanded and patentcounts in order to get sort of a time series of patent filings and catastrophes.
# Ensure unique entries in patentcounts for each FIPS and Filing_years
patentcounts <- patentcounts %>%
  distinct(FIPS, Filing_years, .keep_all = TRUE)
# Perform the left join with the cleaned-up patentcounts dataset
fips_data <- fips_data %>%
  left_join(patentcounts %>%
              rename(Patentcounts = Number_of_patents),  
            by = c("FIPS", "FIPS_Year" = "Filing_years")) %>%
  rename(Year = FIPS_Year)


# The following lines manipulate the county and GDP accordingly to be able to work with it.
# Extract the first two digits (state FIPS) in fips_data_expanded
fips_data <- fips_data %>%
  mutate(state_fips = substr(FIPS, 1, 2)) %>%
  relocate(state_fips, .after = FIPS)

# Create a county_instance within each state (only one instance per county within the state) and count this so the first county of a name occuring multiple times will be 1, the next county of the same name will be 2 etc. 
County_population <- County_population %>%
  group_by(County) %>%
  mutate(county_instance = row_number())  
# Do the same for the GDP data
County_GDP <- County_GDP %>%
  group_by(County) %>%
  mutate(county_instance = row_number())


# Join the data
fips_data_with_population <- fips_data %>%
  left_join(County_population, by = c("county" = "County", "county_instance"))
# Join population and GDP data
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
  mutate(across(c(PropertyDmg, PropertyDmgPerCapita, Patentcounts, `Log-PropertyDmg`, `Log-PropertyDmgPerCapita`), ~replace_na(., 0)))


# Actual regression

# Remove variable Hazard 
fips_data_population_and_gdp <- fips_data_population_and_gdp %>%
  select(-Hazard)

# Run a weighted regression using GDP per capita as weight, Year as fixed effect, clustering by FIPS code and using Log-PropertyDmg as independent variable since PropertyDmgPerCapita in the original dataset (Catastrophe_data) appears to have been inconsistently converted. Therefore, it does not make sense to apply a filter of Log-PropertyDmgPerCapita as done in the paper.
model_1 <- feols(Patentcounts ~ `Log-PropertyDmg` | Year,
        data = fips_data_population_and_gdp,
        weights = ~ GDP_per_capita, 
        cluster = ~ FIPS)

summary(model_1)
# Estimate: 1.304, *

# This performs the regression with PropertyDmg_per_GDP as the independent variable
model_2 <- feols(Patentcounts ~ PropertyDmg_per_GDP | Year,
        data = fips_data_population_and_gdp,
        weights = ~ GDP_per_capita, 
        cluster = ~ FIPS)

summary(model_2)
# Estimate: -6.4e-05, *

# This takes the above created variable Log-PropertyDmgPerCapita_manual as independent variable. 
model_3 <- feols(Patentcounts ~ Log_PropertyDmgPerCapita_manual | Year,
                 data = fips_data_population_and_gdp,
                 weights = ~ GDP_per_capita,
                 cluster = ~ FIPS)

summary(model_3)
# Estimate: 0.791, not significant