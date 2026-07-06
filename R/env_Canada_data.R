
#Temperature Environmental Canada Data

ec_read_csv_from_candidates <- function(urls, ...) {
  for (u in urls) {
    out <- tryCatch(
      {
        readr::read_csv(u, show_col_types = FALSE, progress = FALSE, ...)
      },
      error = function(e) {
        tryCatch(utils::read.csv(u, stringsAsFactors = FALSE, ...), error = function(e2) NULL)
      }
    )
    if (!is.null(out) && nrow(out) > 0) {
      return(as.data.frame(out))
    }
  }
  NULL
}

getEnvCanStations <- function(x){
  ###############################################
  ##      ENV Canada Data Scraping Sites      ##
  ###############################################
  # mirror-aware base URLs (ECCC now commonly serves from data-donnees.az.ec.gc.ca)
  base_urls <- c(
    "https://data-donnees.az.ec.gc.ca/data/substances/monitor/automated-fresh-water-quality-monitoring-and-surveillance-data/",
    "https://data-donnees.ec.gc.ca/data/substances/monitor/automated-fresh-water-quality-monitoring-and-surveillance-data/"
  )

  # Discover station data files from directory listing.
  discovered <- NULL
  used_base <- NULL
  for (b in base_urls) {
    discovered <- tryCatch(
      {
        html <- rvest::read_html(b)
        hrefs <- rvest::html_attr(rvest::html_nodes(html, "a"), "href")
        hrefs <- hrefs[!is.na(hrefs)]
        files <- basename(hrefs)
        files <- unique(files[grepl("^auto-water-qual-eau-.*\\.csv$", files, ignore.case = TRUE)])
        files <- files[!grepl("auto-water-qual-eau-(stations|tabledescriptions|variableinfo)\\.csv$", files, ignore.case = TRUE)]
        files
      },
      error = function(e) NULL
    )

    if (!is.null(discovered) && length(discovered) > 0) {
      used_base <- b
      break
    }
  }

  if (is.null(discovered) || length(discovered) == 0) {
    return(data.frame())
  }

  xdf <- tibble::tibble(
    filename = discovered,
    link = paste0(used_base, discovered),
    data_name = sub("^auto-water-qual-eau-", "", sub("\\.csv$", "", discovered, ignore.case = TRUE), ignore.case = TRUE)
  )

  xdf$STATION_NO <- sub("[-_].*$", "", xdf$data_name)

  station_meta_urls <- paste0(base_urls, "auto-water-qual-eau-stations.csv")
  envCan_stations <- ec_read_csv_from_candidates(station_meta_urls)
  if (is.null(envCan_stations)) {
    return(data.frame())
  }

  output <- dplyr::left_join(xdf, envCan_stations, by = "STATION_NO")
  output <- dplyr::distinct(output, STATION_NO, filename, .keep_all = TRUE)
  #this is what I want the table to plot
  #station_id <- unique(xdf$station_id)

}

#site_names <- unique(getEnvCanStations()$STATION_NO)[1:4]

getEnvCanData <- function(site_names, variable = "TEMPERATURE WATER"){
   station_list <- getEnvCanStations()%>%#
     dplyr::filter(STATION_NO %in% site_names)

   if (nrow(station_list) < 1) {
     return(data.frame(site_id = character(), date = as.Date(character()), tavg_wat_C = numeric()))
   }
   
   station_data <- lapply(station_list$link, function(u) ec_read_csv_from_candidates(c(u)))
   station_data <- station_data[!vapply(station_data, is.null, logical(1))]
   if (length(station_data) < 1) {
     return(data.frame(site_id = character(), date = as.Date(character()), tavg_wat_C = numeric()))
   }
   
   output <- dplyr::bind_rows(station_data)%>%
       dplyr::filter(grepl('Temperature|temperature|temp', VARIABLE))%>%
        mutate(site_id = STATION_NO,
               date = as.Date(DATE_TIME_HEURE, "%d/%m/%Y"))%>%
     group_by(site_id) %>%
     timetk::summarise_by_time( #for daily time steps from hourly.
       .date_var = date,
       .by       = "day", # Setup for monthly aggregation
       # Summarization
       tavg_wat_C = mean(as.numeric(gsub(",", ".", VALUE_VALEUR)), na.rm = TRUE))
   #need to get out of data to call select on group_by
   output <- output %>%
        dplyr::select(site_id, date, tavg_wat_C) %>%
        dplyr::filter(!is.na(date), is.finite(tavg_wat_C))
   
   return(output)
     
}

