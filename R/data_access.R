# list_bca_data <- function(){
#   url=utils::URLencode("https://abacus.library.ubc.ca/api/search?q=LAPUAB*&show_facets=true&subtree:'11272.1/AB2/LAPUAB'")
#   r<-httr::GET(url)
#   d <- httr::content(r)
# }
#

#' List available BCA datasets
#'
#' @return a tibble with available datasets
#' @export
list_bca_datasets <- function(){
  d<-rvest::read_html("https://abacus.library.ubc.ca/dataset.xhtml?persistentId=hdl:11272.1/AB2/LAPUAB")

  datasets <- d |>
    rvest::html_nodes("div.fileNameOriginal a") |>
    purrr::map_df(\(a)tibble(name=rvest::html_text(a),url=rvest::html_attr(a,"href"))) |>
    mutate(name=gsub("\\n\\s*","",.data$name)) |>
    filter(.data$name!="") |>
    filter(grepl("\\.zip",.data$name)) |>
    mutate(id=gsub(".+persistentId=","",.data$url) |> gsub("\\&.+","",x=_))

  d2<-rvest::read_html("https://abacus.library.ubc.ca/dataset.xhtml?persistentId=hdl:11272.1/AB2/NXRVP9")
  datasets2 <- d2 |>
    rvest::html_nodes("div.fileNameOriginal a") |>
    purrr::map_df(\(a)tibble(name=rvest::html_text(a),url=rvest::html_attr(a,"href"))) |>
    mutate(name=gsub("\\n\\s*","",.data$name)) |>
    filter(.data$name!="") |>
    filter(grepl("\\.zip",.data$name)) |>
    mutate(id=gsub(".+persistentId=","",.data$url) |> gsub("\\&.+","",x=_))

  d3<-rvest::read_html("https://abacus.library.ubc.ca/dataset.xhtml?persistentId=hdl:11272.1/AB2/WV3PO5")
  datasets3 <- d3 |>
    rvest::html_nodes("div.fileNameOriginal a") |>
    purrr::map_df(\(a)tibble(name=rvest::html_text(a),url=rvest::html_attr(a,"href"))) |>
    mutate(name=gsub("\\n\\s*","",.data$name)) |>
    filter(.data$name!="") |>
    filter(grepl("\\.zip",.data$name)) |>
    mutate(id=gsub(".+persistentId=","",.data$url) |> gsub("\\&.+","",x=_))
  d4<-rvest::read_html("https://abacus.library.ubc.ca/dataset.xhtml?persistentId=hdl:11272.1/AB2/UYF6JH")
  datasets4 <- d4 |>
    rvest::html_nodes("div.fileNameOriginal a") |>
    purrr::map_df(\(a)tibble(name=rvest::html_text(a),url=rvest::html_attr(a,"href"))) |>
    mutate(name=gsub("\\n\\s*","",.data$name)) |>
    filter(.data$name!="") |>
    filter(grepl("\\.txt",.data$name)) |>
    mutate(id=gsub(".+persistentId=","",.data$url) |> gsub("\\&.+","",x=_))

  revd<-bind_rows(datasets,datasets2,datasets3,datasets4) |>
    filter(grepl("bca_folios_spatial_file|Residential_inventory|Commercial_Inventory|Non-Residential|REVD\\d+_\\d+\\.zip",.data$name)) |>
    mutate(updated_at=stringr::str_extract(.data$name,"\\d{8}") |> as.Date(format="%Y%m%d")) |>
    mutate(updated_at=coalesce(.data$updated_at,stringr::str_extract(.data$name,"\\d{6}") |> paste0("01") |> as.Date(format="%Y%m%d"))) |>
    mutate(series=case_when(grepl("spatial",.data$name)~"spatial",
                            grepl("data_advice",.data$name)~"data_advice",
                            grepl("Residential_inventory",.data$name)~"residential_inventory",
                            grepl("Commercial_Inventory|Non-Residential",.data$name)~"commercial_inventory",
                            TRUE~"original")) |>
    mutate(revision=stringr::str_match(.data$name,"REVD(\\d{2})")[,2]) |>
    mutate(revision=coalesce(.data$revision,stringr::str_match(.data$name,"bca_folios_spatial_file_\\d{2}(\\d{2})")[,2])) |>
    mutate(revision=coalesce(.data$revision,stringr::str_match(.data$name,"Residential_inventory_\\d{2}(\\d{2})")[,2])) |>
    mutate(revision=coalesce(.data$revision,stringr::str_match(.data$name,"^\\d{2}(\\d{2})\\d{4}\\_.*")[,2])) |>
    mutate(Year=paste0("20",substr(.data$revision,1,2))) |>
    select(.data$Year,.data$revision,persistent_id=.data$id,.data$updated_at,.data$series) |>
    mutate(n=n(),.by=c("revision","series")) |>
    mutate(revision=ifelse(.data$n==1,.data$revision,paste0(.data$revision,"-",strftime(.data$updated_at,"%m")))) |>
    select(-.data$n) |>
    arrange(.data$revision)

  revd
}



download_bca_data <- function(persistent_id,api_key,path) {
  r<-httr::GET(url="https://abacus.library.ubc.ca/api/access/datafile/:persistentId/",
               query=list(persistentId=persistent_id),
               httr::add_headers("X-Dataverse-key"=api_key),
               httr::write_disk(path, overwrite=TRUE))
  if (r$status_code != 200) {
    stop(paste0("Problem downloading the data:\n",
                "Persistent ID: ",persistent_id,"\n",
                httr::content(r)$message))
  }
}

#' Get connection to BCA database
#'
#' @description
#' Will download the data if necessary.
#' @param version database version, requires revision number and persistent ID
#' @param api_key Abacus API key
#' @param cache_path cache path for local caching of the database
#' @param refresh optionally refresh the local database if set to \code{TRUE}
#' @return A database connection
#' @export
get_bca_sqlite_connection <- function(version = list(revision=22,persistent_id="hdl:11272.1/AB2/LAPUAB/HHU1YP"),
                                      api_key = Sys.getenv("ABACUS_API_TOKEN"),
                                      cache_path = Sys.getenv("ABACUS_CACHE_PATH"),
                                      refresh = FALSE) {
  if (nchar(cache_path)==0) stop("Local cache path needs to be provided.")
  if (as.integer(substr(version$revision,1,2))<=22) {
    path <- file.path(cache_path,paste0("REVD",version$revision,"_and_inventory_extracts.sqlite3"))
  } else {
    path <- dir(cache_path,full.names = TRUE,pattern=paste0("_REVD",version$revision,"\\.csv$")) |>
      last()
  }
  if (refresh || !file.exists(path)) {
    if (nchar(api_key)==0) stop("ABACUS Api Key needs to be provided.")
    message("Downloading BCA data from ABACUS...")
    if (is.null(version$persistent_id)) {
      dataset <- list_bca_datasets() |>
        filter(.data$revision==version$revision)
      if (nrow(dataset)>1) {
        for (i in 1:nrow(dataset)) {
          tmp<-tempfile(fileext=".zip")
          version$persistent_id=dataset$persistent_id[i]
          download_bca_data(version$persistent_id,api_key,tmp)
          #zip::zip_list(tmp)
          fs<-zip::unzip(tmp,exdir=cache_path)
          unlink(tmp)
        }
      } else {
        stop("No dataset found for revision ",version$revision)
      }
    }
  }
  con <- NULL
  if (as.integer(substr(version$revision,1,2))<=22) {
    con<-DBI::dbConnect(RSQLite::SQLite(), path)
  }
  con
}





BCA_TABLES <- c("address", "assessmentAreaKey", "commercialInventory", "folio", "folioDescription",
                "folioDescriptionTax", "jurisdictionKey", "legalDescription" , "metadata",
                "residentialInventory", "sales", "valuation", "value")

#' Get a connection to the BCA data table
#'
#' @description
#' Will download the data if necessary.
#' @param table name of the data table. Possibly values are
#' "address", "assessmentAreaKey", "commercialInventory", "folio", "folioDescription",
#' "folioDescriptionTax", "jurisdictionKey", "legalDescription" , "metadata",
#' "residentialInventory", "sales", "valuation", "value"
#' @param version database version, requires revision number and persistent ID
#' @param api_key Abacus API key
#' @param cache_path cache path for local caching of the database
#' @param assessment_areas optionally filter the data by assessment areas
#' @param refresh optionally refresh the local database if set to \code{TRUE}
#' @return A database connection to the residential inventory table
#' @export
get_bca_data <- function(table,
                         version = list(revision="22",persistent_id="hdl:11272.1/AB2/LAPUAB/HHU1YP"),
                         api_key = Sys.getenv("ABACUS_API_TOKEN"),
                         cache_path = Sys.getenv("ABACUS_CACHE_PATH"),
                         assessment_areas = NULL,
                         refresh = FALSE) {

  if (is.character(version)||is.numeric(version)) {
    version <- list(revision=as.character(version))
  }

  if (as.integer(substr(version$revision,1,2))>22 && table %in%c("residentialInventory","commercialInventory")) {
    version$series <- table
    d<-get_bca_inventory(version=version,
                         api_key=api_key,cache_path=cache_path,
                         assessment_areas=assessment_areas,refresh=refresh)
    return(d)
  }


  con <- get_bca_sqlite_connection(api_key=api_key,version=version,cache_path=cache_path,refresh=refresh)
  if (is.null(con)) {
    if (table %in% c("residentialInventory","commercialInventory")) {
      if (is.null(version$series)) {
        series <- ifelse(table=="residentialInventory","residential_inventory","commercial_inventory")
        version$series=series
      }
      d <- get_bca_inventory(version=version,api_key=api_key,cache_path=cache_path,
                        assessment_areas=assessment_areas,refresh=refresh)
    } else {
      files <- dir(cache_path,full.names=TRUE,pattern = paste0("REVD",version$revision,"\\.csv$"))
      d<-readr::read_csv(files[grepl(table,files)],col_types=readr::cols(.default="c"))
    }
  } else {
    if (!(table %in% BCA_TABLES)) {
      stop(paste0("table needs to be one of ",
                  paste0(BCA_TABLES,collapse = ", "),
                  "You supplied ",table,"."))
    }
    d<-dplyr::tbl(con,table)
  }

  d
}


#' Get inventory data
#'
#' @description
#' Will download the data if necessary.
#' @param version database version, requires revision number and persistent ID
#' @param api_key Abacus API key
#' @param cache_path cache path for local caching of the database
#' @param assessment_areas optionally filter the data by assessment areas
#' @param refresh optionally refresh the local database if set to \code{TRUE}
#' @return A database connection to the residential inventory table
#' @export
get_bca_inventory <- function(version = list(revision="24",
                                             series=c("residential_inventory","commercial_inventory")),
                              api_key = Sys.getenv("ABACUS_API_TOKEN"),
                              cache_path = Sys.getenv("ABACUS_CACHE_PATH"),
                              assessment_areas = NULL,
                              refresh = FALSE) {

  d <- NULL


  for (s in version$series) {
    if (s=="residentialInventory") {
      s <- "residential_inventory"
    } else if (s=="commercialInventory") {
      s <- "commercial_inventory"
    } else {
      stop("Series needs to be one of 'residential_inventory' or 'commercial_inventory'")
    }
    dataset <- list_bca_datasets() |>
      filter(.data$revision==version$revision,
             .data$series==s)
    stopifnot(nrow(dataset)==1)
    path <- file.path(cache_path,paste0(s,"_",version$revision))
    if (!dir.exists(path)|refresh|length(dir(path,pattern=s,ignore.case = TRUE))==0) {
      if (dir.exists(path)) {
        files <- dir(path,pattern=s,full.names=TRUE,ignore.case = TRUE)
        unlink(files)
      }
      tmp <- tempfile(fileext=".zip")
      download_bca_data(dataset$persistent_id,api_key=api_key,path=tmp)
      if (s=="commercial_inventory") {
        if (!dir.exists(path)) dir.create(path)
        file.copy(tmp,file.path(path,paste0(strftime(dataset$updated_at,"%Y%m%d"),"_commercial_inventory_extract.txt")))
      } else {
        fs<-utils::unzip(tmp,exdir=path)
        pp <- dir(path)
        if (length(pp)==1 && dir.exists(pp)) { # in some cases this ends up in a subdirectory
          cp_result <- file.copy(fs,path)
          unlink(pp,recursive = TRUE)
        }
      }
      unlink(tmp)
    }
    files <- dir(path,pattern=s,full.names=TRUE,ignore.case = TRUE)
    if (!is.null(assessment_areas) && s=="residential_inventory") {
      files <- files[grepl(paste0(c(paste0("_AA",assessment_areas,"_"),
                                    paste0("_A",assessment_areas,"_")),
                                  collapse="|"),files)]
    }
    dd<-readr::read_csv(files,col_types=readr::cols(.default="c"))

    if (!is.null(assessment_areas) && s=="commercial_inventory") {
      dd <- dd |>
        filter(.data$Area %in% assessment_areas)
    }
    d <- bind_rows(d,dd)
  }

  d
}



