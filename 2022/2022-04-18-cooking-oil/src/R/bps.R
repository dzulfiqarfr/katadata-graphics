bps_request <- function(idVar, 
                        keyBPS = Sys.getenv("keyBPS")) {
  
  stopifnot(!is.na(readr::parse_number(idVar)))
  
  respRaw <- httr::GET(
    "https://webapi.bps.go.id/v1/api/list",
    query = list(
      model = "data",
      domain = "0000",
      var = idVar,
      key = keyBPS
    )
  )
  
  respParsed <- respRaw |>
    httr::content(type = "text") |>
    jsonlite::fromJSON()
  
  return(respParsed)
  
}


bps_tidy <- function(resp, subject, idVar) {
  
  stopifnot(
    is.list(resp),
    is.character(subject),
    !is.na(readr::parse_number(idVar))
  )
  
  subjectSym <- dplyr::sym(subject)
  
  idCategory <- resp[["turvar"]] |> 
    tibble::as_tibble() |> 
    anchor_regex()
  
  idRegion <- resp[["vervar"]] |> 
    tibble::as_tibble() |> 
    anchor_regex()
  
  idYear <- resp[["tahun"]] |> 
    tibble::as_tibble() |> 
    anchor_regex()
  
  dataRaw <- tibble::as_tibble(resp$datacontent)
  
  dataLong <- dataRaw |> 
    tidyr::pivot_longer(
      cols = tidyselect::everything(),
      names_to = c("id_region", "id_composite"),
      names_sep = idVar,
      values_to = subject
    )
  
  dataTidy <- dataLong |> 
    dplyr::mutate(
      region = stringr::str_replace_all(id_region, tibble::deframe(idRegion)),
      id_category = stringr::str_sub(id_composite, 1, 4),
      category = stringr::str_replace_all(
        id_category, 
        tibble::deframe(idCategory)
      ),
      id_year = stringr::str_sub(id_composite, 5, 7),
      year = stringr::str_replace_all(id_year, tibble::deframe(idYear))
    ) |> 
    dplyr::select(id_region, region, category, year, !!subjectSym)
  
  return(dataTidy)
  
}


# Helper ----

anchor_regex <- function(df, col = val) {
  
  col <- rlang::enquo(col)
  
  dfAnchored <- df |> dplyr::mutate(!!col := paste0("^", !!col, "$"))
  
  return(dfAnchored)
  
}
