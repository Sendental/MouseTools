## code to prepare `DATASET` dataset goes here

# Read the unwrangled dataset
patient_temp_data <- read.csv("patient_data.csv")

# View the cleaned dataset
print(patient_temp_data)

usethis::use_data(patient_temp_data, overwrite = TRUE)
