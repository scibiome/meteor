## code to prepare `example_data` dataset goes here
example_data <- readr::read_csv("example_data.csv")
usethis::use_data(example_data, overwrite = TRUE)
