## code to prepare `data_long` dataset goes here
raw_example_data <- readr::read_csv("raw_example_data/raw_example_data.csv")
usethis::use_data(raw_example_data, overwrite = TRUE)

