# list_bca_data <- function(){
#   url=utils::URLencode("https://abacus.library.ubc.ca/api/search?q=LAPUAB*&show_facets=true&subtree:'11272.1/AB2/LAPUAB'")
#   r<-httr::GET(url)
#   d <- httr::content(r)
# }
#

#' Get connection to BCA database
#'
#' @description
#' Will download the data if necessary.
#' @param api_key Abacus API key
#' @param version database version, requires revision number and persistent ID
#' @param cache_path cache path for local caching of the database
#' @param refresh optionally refresh the local database if set to \code{TRUE}
#' @return A database connection
#' @export
get_bca_sqlite_connection <- function(api_key = Sys.getenv("ABACUS_API_TOKEN"),
                           version = list(revision=21,persistent_id="hdl:11272.1/AB2/LAPUAB/7ZYI7W"),
                           cache_path = Sys.getenv("ABACUS_CACHE_PATH"),
                           refresh = FALSE) {
  if (nchar(cache_path)==0) stop("Local cache path needs to be provided.")
  path <- file.path(cache_path,paste0("REVD",version$revision,"_and_inventory_extracts.sqlite3"))
  if (refresh || !file.exists(path)) {
    if (nchar(api_key)==0) stop("ABACUS Api Key needs to be provided.")
    tmp<-tempfile()
    httr::GET(url="https://abacus.library.ubc.ca/api/access/datafile/:persistentId/",
              query=list(persistentId=version$persistent_id),
              httr::add_headers("X-Dataverse-key"=api_key),
              httr::write_disk(tmp, overwrite=TRUE))
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
#' @param api_key Abacus API key
#' @param version database version, requires revision number and persistent ID
#' @param cache_path cache path for local caching of the database
#' @param refresh optionally refresh the local database if set to \code{TRUE}
#' @return A database connection to the residential inventory table
#' @export
get_bca_data <- function(table,
                         api_key = Sys.getenv("ABACUS_API_TOKEN"),
                         version = list(revision=21,persistent_id="hdl:11272.1/AB2/LAPUAB/7ZYI7W"),
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

