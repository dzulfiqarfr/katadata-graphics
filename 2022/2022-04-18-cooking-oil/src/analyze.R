pathProject <- "2022/2022-04-18-cooking-oil"

here::i_am(paste(pathProject, "src", "analyze.R", sep = "/"))


# Packages ----

library(conflicted)
library(here)
library(fs)
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(readxl)
library(tidytext)
library(httr)
library(jsonlite)


# Helper ----

# Function to tidy cooking oil price data from the Trade Ministry
source(here(pathProject, "src", "R", "tidy-cooking-oil.R"))

# Functions to request and tidy cooking oil expenditure and consumption data
# from Statistics Indonesia (BPS)
source(here(pathProject, "src", "R", "bps.R"))


# Data ----

## Bulk cooking oil (curah) ----

lsPriceBulk <- dir_ls(here(pathProject, "data", "cooking-oil-bulk"))

listNameMonth <- c("jan", "feb", "mar", "apr")

priceBulkRaw <- lsPriceBulk |>
  map(read_excel) |>
  set_names(listNameMonth)

priceBulkClean <- priceBulkRaw |>
  map(tidy_cooking_oil, category = "curah") |>
  bind_rows()


## Cooking oil with a simple packaging ----

lsPriceBasic <- dir_ls(here(pathProject, "data", "cooking-oil-package-basic"))

priceBasicRaw <- lsPriceBasic |>
  map(read_excel) |>
  set_names(listNameMonth)

priceBasicClean <- priceBasicRaw |>
  map(tidy_cooking_oil, category = "kemasan sederhana") |>
  bind_rows()


## Cooking oil with a premium packaging ----

lsPricePremium <- dir_ls(
  here(pathProject, "data", "cooking-oil-package-premium")
)

pricePremiumRaw <- lsPricePremium |>
  map(read_excel) |>
  set_names(listNameMonth)

pricePremiumClean <- pricePremiumRaw |>
  map(tidy_cooking_oil, category = "kemasan premium") |>
  bind_rows()


## Merge cooking oil data ----

priceAll <- bind_rows(
  priceBulkClean, 
  priceBasicClean, 
  pricePremiumClean
) |>
  mutate(
    cooking_oil_type = as_factor(cooking_oil_type),
    date = as.Date(date),
    price = as.integer(price)
  )

# Index the data to the day the government scrapped the price cap for
# packaged cooking oils
priceIndex <- priceAll |>
  group_by(cooking_oil_type, date) |>
  summarize(price_mean = mean(price, na.rm = TRUE)) |>
  mutate(
    price_index = price_mean / first(price_mean[date == as.Date("2022-03-16")]),
    price_index = price_index * 100
  ) |>
  select(-price_mean) |>
  ungroup()

# Turn into a wide format for Datawrapper
priceIndexWide <- priceIndex |>
  pivot_wider(
    names_from = cooking_oil_type, 
    values_from = price_index
  )

priceIndexWide |>
  write_csv(here(pathProject, "result", "cooking-oil-price-index.csv"))
  

## Cash transfer per cooking oil price ----

cashTransferPerCookingOil <- priceAll |>
  group_by(cooking_oil_type) |>
  filter(date == last(date)) |>
  mutate(cash_transfer_per_cooking_oil = round(300000 / price, 0)) |>
  ungroup()

# Turn into a wide format for Datawrapper
cashTransferPerCookingOilWide <- cashTransferPerCookingOil |>
  select(-c(date, price)) |>
  pivot_wider(
    names_from = cooking_oil_type, 
    values_from = cash_transfer_per_cooking_oil
  )

cashTransferPerCookingOilWide |>
  write_csv(here(pathProject, "result", "cash-transfer-per-cooking-oil.csv"))


## Cooking oil consumption ----

argsRequest <- list(idVar = list("2103", "2119"))

oilConsumptionResp <- argsRequest |> 
  pmap(bps_request) |>
  set_names(c("consumption", "expenditure"))

argsTidy <- list(
  resp = oilConsumptionResp,
  subject = list("consumption", "expenditure"),
  idVar = list("2103", "2119")
)

oilConsumptionTidy <- pmap(argsTidy, bps_tidy)

oilConsumptionMerged <- oilConsumptionTidy[["consumption"]] |>
  left_join(oilConsumptionTidy[["expenditure"]])  

cookingOilCons <- oilConsumptionMerged |>
  filter(str_detect(category, "Minyak goreng"))

# Get province and island group data to add island group to the cooking oil
# consumption data
provinceIsland <- read_csv(here(pathProject, "data", "province-island.csv"))

provinceIslandPrefix <- provinceIsland |>
  mutate(
    # Use the first two digits of the province id for joining
    id_province_prefix = str_sub(bps_id, 1, 2),
    # Remove periods in `D.I. Yogyakarta`
    province_idn = str_remove_all(province_idn, "[:punct:]")
  ) |>
  rename(province = province_idn) |>
  select(id_province_prefix, province, island_group)

cookingOilConsIsland <- cookingOilCons |>
  mutate(id_province_prefix = str_sub(id_region, 1, 2)) |>
  left_join(provinceIslandPrefix) |>
  select(-id_region) |>
  relocate(id_province_prefix, region, province, island_group)

# Get household size data to calculate consumption per household
household <- read_excel(here(pathProject, "data", "bps-household-raw.xlsx"))

householdSize <- household |>
  slice(-c(1:2)) |>
  rename(province = 1, number_of_household = 2, household_size = 3) |>
  mutate(
    across(
      .cols = contains("household"), 
      .fns = ~ str_replace_all(.x, ",", ".")
    ),
    across(.cols = contains("household"), .fns = ~ str_remove_all(.x, "\\s")),
    across(.cols = contains("household"), .fns = as.double)
  ) |>
  select(-number_of_household)

cookingOilConsHousehold <- cookingOilConsIsland |>
  left_join(householdSize) |>
  group_by(province) |>
  mutate(
    consumption_hh = consumption * household_size,
    expenditure_hh = expenditure * household_size
  ) |>
  ungroup() |>
  select(
    -c(
      id_province_prefix , 
      category, 
      consumption, 
      expenditure, 
      household_size
    )
  )

cookingOilConsHouseholdSub <- cookingOilConsHousehold |> filter(year == 2021)

cookingOilConsHouseholdSub |>
  write_csv(
    here(pathProject, "result", "cooking-oil-consupmtion-household.csv")
  )