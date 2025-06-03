library(dplyr)
library(purrr)
library(readr)

# Function to translate Type
translate_type <- function(type) {
    type_translations <- c(Home = "Man", Dona = "Woman", Grup = "Band", `No binari` = "Non-binary")
    return(type_translations[type])
}

# Function to translate country
translate_country <- function(country) {
    country_translations <- c(`Puerto Rico` = "Puerto Rico", Brasil = "Brazil", 
                              `Estats Units` = "United States", Argentina = "Argentina", 
                              `Altres països` = "Other countries", `Corea del Sud` = "South Korea", 
                              `Regne Unit` = "United Kingdom", Nigèria = "Nigeria", 
                              Colombia = "Colombia", Espanya = "Spain", 
                              Canadà = "Canada", Itàlia = "Italy", França = "France")
    return(country_translations[country])
}

# List all CSV files in the current directory
files <- list.files(pattern = "\\.csv$")

# Function to process each file
process_file <- function(file_name) {
    # Read CSV file
    data <- read_csv(file_name)
    
    # Check if the required columns exist
    if("Type" %in% names(data) && "country" %in% names(data)) {
        # Translate the columns
        data$Type <- unname(map_chr(data$Type, translate_type))
        data$country <- unname(map_chr(data$country, translate_country))
        
        # Create a new file name with "_EN"
        new_file_name <- sub("\\.csv$", "_EN.csv", file_name)
        
        # Save the modified data back to CSV
        write_csv(data, new_file_name)
    } else {
        warning(paste("Required columns not found in", file_name))
    }
}

# Apply the function to each file
walk(files, process_file)

# Print completion message
cat("Translation completed for all files.\n")

