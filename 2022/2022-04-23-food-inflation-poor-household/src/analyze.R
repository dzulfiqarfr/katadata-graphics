dirYear <- "2022"
dirProject <- "2022-04-23-food-inflation-poor-household"

here::i_am(paste(dirYear, dirProject, "src", "analyze.R", sep = "/"))


# Packages ----

library(conflicted)
library(here)
library(tidyverse)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
library(httr)
library(jsonlite)
library(lubridate)


# Data ----

## Poverty rate and share of households with food expenditure > 65 percent ----
fsvaRaw <- read_csv(here(dirYear, dirProject, "data", "bkp-fsva-2021.csv"))

foodExpPovertyRaw <- fsvaRaw %>%
  rename(
    region = Wilayah,
    poverty_rate = `Kemiskinan (%)`,
    food_expenditure = `Pengeluaran Pangan (%)`
  ) %>% 
  select(region, poverty_rate, food_expenditure)

foodExpPovertyClean <- foodExpPovertyRaw %>% 
  separate(
    region,
    into = c("province", "city"),
    sep = "\\s-\\s"
  ) %>% 
  mutate(
    across(.cols = c(province, city), .fns = str_to_title),
    province = str_replace_all(
      province,
      c("Dki" = "DKI", "Daerah Istimewa" = "")
    )
  )

write_csv(
  foodExpPovertyClean,
  here(dirYear, dirProject, "result", "food-expenditure-poverty-rate.csv")
)


## Food, beverage and tobacco inflation ----

keyBPS <- Sys.getenv("keyBPS")

idFoodCPI <- "1905"

respFoodCPIraw <- GET(
  url = "https://webapi.bps.go.id/v1/api/list",
  query = list(
    model = "data",
    domain = "0000",
    var = idFoodCPI,
    key = keyBPS
  )
)

respFoodCPIparsed <- respFoodCPIraw %>% 
  content(type = "text") %>% 
  fromJSON()

anchor_regex <- function(data) {
  
  if (!any("val" %in% names(data))) {
    stop("`data` must contain `val` column.")
  }
  
  dataAnchored <- data %>% dplyr::mutate(val = paste0("^", val, "$"))
  
  return(dataAnchored)
  
}

idExpenditureGroup <- respFoodCPIparsed$turvar %>% 
  as_tibble() %>% 
  anchor_regex()

idRegion <- respFoodCPIparsed$vervar %>% as_tibble() %>% anchor_regex()

idYear <- respFoodCPIparsed$tahun %>% as_tibble() %>% anchor_regex()

foodCPIraw <- as_tibble(respFoodCPIparsed$datacontent)

foodCPIlong <- foodCPIraw %>% 
  pivot_longer(
    cols = everything(),
    names_to = c("id_region", "id_composite"),
    names_sep = idFoodCPI,
    values_to = "cpi"
  )

foodCPIclean <- foodCPIlong %>% 
  mutate(
    region = str_replace_all(id_region, deframe(idRegion)),
    id_expenditure_group = str_sub(id_composite, 1, 4),
    expenditure_group = str_replace_all(
      id_expenditure_group, 
      deframe(idExpenditureGroup)
    ),
    expenditure_group = str_replace_all(
      expenditure_group,
      c(
        "dan" = "&",
        "Minuman yang Tidak Beralkohol" = "Minuman nonalkohol"
      )
    ),
    expenditure_group = str_to_sentence(expenditure_group),
    expenditure_group = as_factor(expenditure_group),
    id_year = str_sub(id_composite, 5, 7),
    year = str_replace_all(id_year, deframe(idYear)),
    id_month = str_sub(id_composite, 8, 9),
    id_month = as.integer(id_month),
    month = case_when(
      id_month < 10 ~ paste0("0", id_month),
      TRUE ~ as.character(id_month)
    ),
    month = paste0("-", month, "-01"),
    date = as.Date(paste0(year, month))
  ) %>% 
  select(region, date, expenditure_group, cpi)

foodInflation <- foodCPIclean %>% 
  filter(region == "INDONESIA") %>% 
  group_by(expenditure_group) %>% 
  mutate(inflation = (cpi - lag(cpi, 12)) / lag(cpi, 12) * 100) %>% 
  ungroup() %>% 
  filter(!is.na(inflation)) %>% 
  select(date, expenditure_group, inflation)

foodInflation %>% 
  write_csv(
    here(
      dirYear, 
      dirProject, 
      "result",
      "inflation-food-overall-long.csv"
    )
  )

# Turn into a wide format for Datawrapper
foodInflationSplit <- foodInflation %>% 
  mutate(month = month(date), year = year(date)) %>% 
  relocate(month, year) %>% 
  select(-date) %>% 
  split(.$expenditure_group) %>% 
  map(pivot_wider, names_from = year, values_from = inflation)

argsCSVfoodInflation <- list(
  x = foodInflationSplit,
  file = list(
    here(dirYear, dirProject, "result", "inflation-food-overall-wide.csv"),
    here(dirYear, dirProject, "result", "inflation-food-wide.csv"),
    here(dirYear, dirProject, "result", "inflation-beverage-nonalcoholic-wide.csv"),
    here(dirYear, dirProject, "result", "inflation-tobacco-wide.csv"),
    here(dirYear, dirProject, "result", "inflation-beverage-alcoholic-wide.csv")
  )
)

pwalk(argsCSVfoodInflation, write_csv)
