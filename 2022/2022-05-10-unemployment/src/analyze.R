pathProject <- "2022/2022-05-10-unemployment"

here::i_am(paste(pathProject, "src", "analyze.R", sep = "/"))


# Packages ----

library(conflicted)
library(here)
library(tidyverse)
library(fs)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")


# Data ----

lsData <- dir_ls(here(pathProject, "data", "weo"))

weoRaw <- lsData |> 
  map(read_csv, na = c("n/a", "")) |> 
  set_names("oct_19", "apr_22")

weoColumnRenamed <- weoRaw |> 
  map(
    rename,
    weo_country_code = `WEO Country Code`,
    iso3code = ISO,
    subject_code = `WEO Subject Code`,
    country = Country,
    subject_description = `Subject Descriptor`,
    subject_note = `Subject Notes`,
    unit = Units,
    scale = Scale,
    country_note = `Country/Series-specific Notes`,
    estimates_starts_after = `Estimates Start After`
  )

argsPivotLonger <- list(
  data = weoColumnRenamed,
  cols = list(as.character(seq(1980, 2024)), as.character(seq(1980, 2027)))
)

weoLong <- argsPivotLonger |> 
  pmap(
    pivot_longer,
    names_to = "year",
    names_transform = list(year = as.integer),
    values_to = "value",
    values_transform = list(value = as.double)
  )

weoIDN <- weoLong |> map(filter, iso3code == "IDN")


## Unemployment ----

weoUnempRecessionIDN <- weoIDN[["apr_22"]] |> 
  filter(
    subject_code == "LUR",
    year %in% c(seq(1997, 2005), seq(2019, 2027))
  ) |> 
  mutate(
    recession = case_when(
      year %in% seq(1997, 2005) ~ "Krisis finansial Asia",
      TRUE ~ "Pandemi COVID-19"
    ),
    category = case_when(
      year %in% seq(2022, 2027) ~ "Proyeksi", 
      TRUE ~ "Aktual"
    )
  ) |> 
  group_by(recession) |> 
  mutate(year_recession = seq(-1, 7)) |> 
  ungroup() |> 
  select(year_recession, recession, category, value)

# Turn into a wide format for Datawrapper
weoUnempRecessionIDNwide <- weoUnempRecessionIDN |> 
  pivot_wider(names_from = c(recession, category), values_from = value) 

weoUnempRecessionIDNwide |>
  write_csv(here(pathProject, "result", "unemployment-rate.csv"))


## Real GDP growth ----

weoGrowthIDN <- weoIDN |> 
  map(filter, subject_code == "NGDP_RPCH", year %in% seq(2019, 2024)) |>
  bind_rows(.id = "weo") |> 
  mutate(
    category = case_when(
      weo == "oct_19" & year > 2019 ~ "Proyeksi",
      weo == "apr_22" & year > 2021 ~ "Proyeksi",
      TRUE ~ "Aktual"
    )
  ) |> 
  select(weo, year, category, value) |> 
  rename(growth = value)

# Indonesia's GDP (at 2010 prices) in 2019 stood at Rp 10,949,155.40 billion,
# according to Statistics Indonesia (BPS)

weoGDPforecastIDN <- weoGrowthIDN |> 
  mutate(
    gdp = case_when(year == 2019 ~ 10949155.40),
    growth_decimal = growth / 100
  ) |> 
  group_by(weo) |> 
  mutate(
    gdp_forecast = (lag(gdp) * growth_decimal) + lag(gdp),
    gdp_forecast = case_when(is.na(gdp_forecast) ~ gdp, TRUE ~ gdp_forecast),
    gdp_forecast = case_when(
      is.na(gdp_forecast) ~ (lag(gdp_forecast) * growth_decimal) + lag(gdp_forecast),
      TRUE ~ gdp_forecast
    ),
    gdp_forecast = case_when(
      is.na(gdp_forecast) ~ (lag(gdp_forecast) * growth_decimal) + lag(gdp_forecast),
      TRUE ~ gdp_forecast
    ),
    gdp_forecast = case_when(
      is.na(gdp_forecast) ~ (lag(gdp_forecast) * growth_decimal) + lag(gdp_forecast),
      TRUE ~ gdp_forecast
    ),
    gdp_forecast = case_when(
      is.na(gdp_forecast) ~ (lag(gdp_forecast) * growth_decimal) + lag(gdp_forecast),
      TRUE ~ gdp_forecast
    ),
    gdp_index = gdp_forecast / first(gdp_forecast) * 100
  ) |> 
  ungroup() |> 
  select(weo, year, category, growth, gdp_forecast, gdp_index) |>
  rename(gdp = gdp_forecast)

# Turn into a wide format for Datawrapper
weoGDPforecastIDNwide <- weoGDPforecastIDN |> 
  select(weo, year, category, gdp_index) |> 
  pivot_wider(names_from = c(weo, category), values_from = gdp_index)

weoGDPforecastIDNwide |>
  write_csv(here(pathProject, "result", "gdp.csv"))