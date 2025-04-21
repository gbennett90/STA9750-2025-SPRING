# Download Data

if(!file.exists("data/mp01/nyc_payroll_export.csv")){
  dir.create("data/mp01", showWarnings=FALSE, recursive=TRUE)
  
  ENDPOINT <- "https://data.cityofnewyork.us/resource/k397-673e.json"
  
  if(!require("httr2")) install.packages("httr2")
  library(httr2)
  
  if(!require("jsonlite")) install.packages("jsonlite")
  library(jsonlite)
  
  if(!require("dplyr")) install.packages("dplyr")
  library(dplyr)
  
  if(!require("readr")) install.packages("readr")
  library(readr)
  
  BATCH_SIZE <- 50000
  OFFSET     <- 0
  END_OF_EXPORT <- FALSE
  ALL_DATA <- list()
  
  while(!END_OF_EXPORT){
    cat("Requesting items", OFFSET, "to", BATCH_SIZE + OFFSET, "\n")
    
    req <- request(ENDPOINT) |>
      req_url_query(`$limit`  = BATCH_SIZE, 
                    `$offset` = OFFSET)
    
    resp <- req_perform(req)
    
    batch_data <- fromJSON(resp_body_string(resp))
    
    ALL_DATA <- c(ALL_DATA, list(batch_data))
    
    if(NROW(batch_data) != BATCH_SIZE){
      END_OF_EXPORT <- TRUE
      
      cat("End of Data Export Reached\n")
    } else {
      OFFSET <- OFFSET + BATCH_SIZE
    }
  }
  
  ALL_DATA <- bind_rows(ALL_DATA)
  
  cat("Data export complete:", NROW(ALL_DATA), "rows and", NCOL(ALL_DATA), "columns.")
  
  write_csv(ALL_DATA, "data/mp01/nyc_payroll_export.csv")
}

# Import and clean the data

nyc_payroll <- read_csv("data/mp01/nyc_payroll_export.csv")

# Add a total salary column if it doesn't exist

nyc_clean <- nyc_payroll |>
  mutate(
    total_salary = base_salary + total_ot_paid + total_other_pay
  ) |>
  mutate(
    across(
      c("agency_name", "last_name", "first_name", "work_location_borough", 
        "title_description", "leave_status_as_of_june_30"), 
      str_to_title
    )
  )

# Take a glimpse at the cleaned data

glimpse(nyc_clean)

# Employee salary table for Eric L. Adams

adams_salary_table <- nyc_clean |>
  filter(first_name == "Eric", last_name == "Adams") |>
  select(fiscal_year, title_description, agency_name, total_salary)

# View the result
print(adams_salary_table)

# Visualize salary data using DT

library(DT)

adams_salary_table |>
  mutate(`Total Salary` = scales::dollar(total_salary)) |>
  datatable(options = list(searching = FALSE, paging = FALSE, info = FALSE))



# Compute total compensation for all employees using case_when
nyc_clean <- nyc_clean |>
  mutate(
    hourly_rate = base_salary / 2080,
    total_compensation = case_when(
      title_description %in% c("Mayor") ~ total_salary,
      !is.na(hourly_rate) & !is.na(regular_hours) & !is.na(ot_hours) ~ (hourly_rate * regular_hours) + (hourly_rate * 1.5 * ot_hours),
      !is.na(hourly_rate) & !is.na(regular_hours) & is.na(ot_hours) ~ hourly_rate * regular_hours,
      TRUE ~ NA_real_
    )
  )

# View the first few rows to verify the total compensation

head(nyc_clean)

# View the first few rows to check the compensation values
head(nyc_clean |>
       select(first_name, last_name, title_description, total_compensation))

# Check for any missing values in total_compensation
nyc_clean |>
  filter(is.na(total_compensation))

# Summarize total compensation for different titles

nyc_clean |>
  group_by(title_description) |>
  summarise(
    avg_total_compensation = mean(total_compensation, na.rm = TRUE)
  )

#  Which job title has the highest base rate of pay? (Assuming a standard 2,000-hour work year and no overtime.) Chair hourly rate $207
nyc_clean |>
  mutate(hourly_base_rate = base_salary / 2000) |>
  arrange(desc(hourly_base_rate)) |>
  slice(1) |>
  select(title_description, hourly_base_rate)

# Which individual and in what year had the single highest city total payroll (regular and overtime combined)? Mark Tettonis  $1,881,523
nyc_clean |>
  transform(total_payroll = total_salary + total_ot_paid) |>
  arrange(desc(total_payroll)) |>
  slice(1) |>
  select(fiscal_year, first_name, last_name, total_payroll)

# Which individual worked the most overtime hours in this data set? James Internicola 3693 hours
nyc_clean |>
  arrange(desc(ot_hours)) |>
  slice(1) |>
  select(first_name, last_name, ot_hours)

#  Which agency has the highest average total annual payroll (base and overtime pay per employee)? Office of Racial Equity $15314
nyc_clean |>
  group_by(agency_name) |>
  summarise(avg_total_payroll = mean(total_salary + total_ot_paid, na.rm = TRUE)) |>
  arrange(desc(avg_total_payroll)) |>
  slice(1)

# Which agency has the most employees on payroll in each year? Dept of Ed Pedagogical
nyc_clean |>
  group_by(fiscal_year, agency_name) |>
  summarise(employee_count = n()) |>
  group_by(fiscal_year) |>
  slice_max(employee_count, n = 1)

# Which agency has the highest overtime usage (compared to regular hours)? Board of Election 
nyc_clean |>
  group_by(agency_name) |>
  summarise(
    total_regular_hours = sum(regular_hours, na.rm = TRUE),
    total_ot_hours = sum(ot_hours, na.rm = TRUE),
    ot_to_regular_ratio = total_ot_hours / total_regular_hours
  ) |>
  arrange(desc(ot_to_regular_ratio)) |>
  slice(1)

# What is the average salary of employees who work outside the five boroughs? (i.e., whose work_location_borough is not one of the five counties.) $53,734
nyc_clean |>
  filter(!(work_location_borough %in% c("Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island"))) |>
  summarise(avg_salary_outside_boroughs = mean(base_salary, na.rm = TRUE))

#  How much has the city’s aggregate payroll grown over the past 10 years?
nyc_clean |>
  group_by(fiscal_year) |>
  summarise(aggregate_payroll = sum(total_salary + total_ot_paid, na.rm = TRUE)) |>
  filter(fiscal_year >= (max(fiscal_year) - 10)) |>
  arrange(fiscal_year)

