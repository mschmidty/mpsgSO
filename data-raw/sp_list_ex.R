## code to prepare `sp_list_ex` dataset goes here
library(tidyverse)
library(readxl)

data_raw <- read_excel("C:\\Users\\MichaelSchmidt2\\USDA\\Mountain Planning Service Group - SCC Library\\Cimarron Comanche NG\\Species List\\CURRENT 20240605_CCNG_SpeciesList.xlsx")


sp_list_ex <- data_raw |>
  select(common_name, scientific_name) |>
  bind_rows(
    tibble(
      common_name = c("Northern Goshawk"),
      scientific_name = c("Accipiter gentilis")
    )
  )
usethis::use_data(sp_list_ex, overwrite = TRUE)
