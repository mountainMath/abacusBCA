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
  dplyr::tibble(Year=seq(2016,2022),revision=seq(16,22),
         persistent_id=c("hdl:11272.1/AB2/LAPUAB/TU8WL0",
                         "hdl:11272.1/AB2/LAPUAB/NWBUFO",
                         "hdl:11272.1/AB2/LAPUAB/ETZ7GQ",
                         "hdl:11272.1/AB2/LAPUAB/OJRVSH",
                         "hdl:11272.1/AB2/LAPUAB/IOZJQV",
                         "hdl:11272.1/AB2/LAPUAB/7ZYI7W",
                         "hdl:11272.1/AB2/LAPUAB/HHU1YP"))
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
  path <- file.path(cache_path,paste0("REVD",version$revision,"_and_inventory_extracts.sqlite3"))
  if (refresh || !file.exists(path)) {
    if (nchar(api_key)==0) stop("ABACUS Api Key needs to be provided.")
    message("Downloading BCA data from ABACUS...")
    tmp<-tempfile()
    r<-httr::GET(url="https://abacus.library.ubc.ca/api/access/datafile/:persistentId/",
              query=list(persistentId=version$persistent_id),
              httr::add_headers("X-Dataverse-key"=api_key),
              httr::write_disk(tmp, overwrite=TRUE))
    if (r$status_code != 200) {
      stop(paste0("Problem downloading the data:\n",httr::content(r)$message))
    }
    #zip::zip_list(tmp)
    fs<-zip::unzip(tmp,exdir=cache_path)
    unlink(tmp)
  }
  con<-DBI::dbConnect(RSQLite::SQLite(), path)
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
#' @param refresh optionally refresh the local database if set to \code{TRUE}
#' @return A database connection to the residential inventory table
#' @export
get_bca_data <- function(table,
                         version = list(revision=22,persistent_id="hdl:11272.1/AB2/LAPUAB/HHU1YP"),
                         api_key = Sys.getenv("ABACUS_API_TOKEN"),
                         cache_path = Sys.getenv("ABACUS_CACHE_PATH"),
                         refresh = FALSE) {
  if (!(table %in% BCA_TABLES)) {
    stop(paste0("table needs to be one of ",
                paste0(BCA_TABLES,collapse = ", "),
                "You supplied ",table,"."))
  }
  con <- get_bca_sqlite_connection(api_key=api_key,version=version,cache_path=cache_path,refresh=refresh)
  dplyr::tbl(con,table)
}

