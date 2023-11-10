## code to prepare `example_data` dataset goes here
covid_data <- readr::read_csv("inst/my_app/covid_data.csv")
usethis::use_data(covid_data, overwrite = TRUE)
