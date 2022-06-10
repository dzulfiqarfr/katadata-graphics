tidy_cooking_oil <- function(data, category) {
  
  stopifnot(category %in% c("curah", "kemasan sederhana", "kemasan premium"))
  
  categoryUpper <- toupper(category)
  
  # Remove unnecessary columns and rows
  dataNoUnnecessaryColumnRow <- data |> 
    dplyr::select(
      !tidyselect::all_of(
        paste0("PERKEMBANGAN HARGA MINYAK GORENG ", categoryUpper)
      )
    ) |> 
    dplyr::slice(-c(1:6))
  
  # Create a tibble containing column name with the corresponding date in 
  # year-month-date format to rename the columns
  dateColumnName <- dataNoUnnecessaryColumnRow |> 
    dplyr::slice(1) |> # The original date values are stored in first row now
    tidyr::pivot_longer(
      cols = tidyselect::everything(),
      names_to = "column_name",
      values_to = "date"
    ) |> 
    dplyr::filter(!(date %in% c("Lokasi", "Rata-Rata"))) |> 
    dplyr::mutate(
      day = readr::parse_number(date),
      day_leading_zero = dplyr::case_when(
        day < 10 ~ paste0("0", day),
        TRUE ~ as.character(day)
      ),
      month_b = stringr::str_sub(date, 1, 3),
      month_m = dplyr::case_when(
        month_b == "Jan" ~ "01",
        month_b == "Feb" ~ "02",
        month_b == "Mar" ~ "03",
        month_b == "Apr" ~ "04"
      ),
      ymd = paste("2022", month_m, day_leading_zero, sep = "-")
    ) |> 
    dplyr::select(ymd, column_name)
  
  # Remove the original date values in first row, then rename the columns
  dataColumnRenamed <- dataNoUnnecessaryColumnRow |> 
    dplyr::slice(-1) |> 
    dplyr::rename(province = ...2, tibble::deframe(dateColumnName))
  
  # Remove columns and rows containing only NAs
  dataNoAllNA <- dataColumnRenamed |> 
    dplyr::mutate(dplyr::across(.fns = ~ dplyr::na_if(.x, 0))) |> 
    dplyr::select_if(function(x) !all(is.na(x))) |> 
    dplyr::filter(!is.na(province))
  
  # Rename some province labels
  dataProvinceRenamed <- dataNoAllNA |> 
    dplyr::mutate(
      province = dplyr::case_when(
        stringr::str_detect(province, "Aceh") ~ "Aceh",
        province == "D.I. Yogyakarta" ~ "Yogyakarta",
        TRUE ~ province
      )
    )
  
  # Turn to long format
  dataTidy <- dataProvinceRenamed |> 
    tidyr::pivot_longer(
      cols = -province,
      names_to = "date",
      values_to = "price"
    ) |> 
    dplyr::mutate(cooking_oil_type = category) |> 
    dplyr::relocate(province, cooking_oil_type)
  
  return(dataTidy)
  
}
