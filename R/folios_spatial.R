

#' Load BCA spatial polygons
#' #'
#' @description
#' Will download the data if necessary.
#' @param version database version, requires revision number and persistent ID
#' @param api_key Abacus API key
#' @param cache_path cache path for local caching of the database
#' @param refresh optionally refresh the local database if set to \code{TRUE}
#' @return A database connection to the residential inventory table
#' @export
get_bca_spatial_gpkg <- function(
                         version = list(revision="24"),
                         api_key = Sys.getenv("ABACUS_API_TOKEN"),
                         cache_path = Sys.getenv("ABACUS_CACHE_PATH"),
                         refresh = FALSE) {
  if (nchar(cache_path)==0) stop("Local cache path needs to be provided.")
  if (is.null(version$persistent_id)) {
    dataset <- list_bca_datasets() |>
      filter(revision==version$revision,series=="spatial")
    if (nrow(dataset)==1) {
      version$persistent_id=dataset$persistent_id
    } else {
      stop("No dataset found for revision ",version$revision)
    }
  }
  rev <- paste0(20,gsub("-","",version$revision))
  grep_path <- paste0("bca_folios_spatial_file_",rev)
  paths <- dir(cache_path,pattern=grep_path,full.names=TRUE)
  if (refresh || length(paths)==0) {
    tmp=tempfile()
    r<-httr::GET(url="https://abacus.library.ubc.ca/api/access/datafile/:persistentId/",
                 query=list(persistentId=version$persistent_id),
                 httr::add_headers("X-Dataverse-key"=api_key),
                 httr::write_disk(tmp, overwrite=TRUE))
    utils::unzip(tmp, exdir = cache_path)
  }
  if (length(paths)!=1) {
    stop("No database found in cache path ",cache_path,"\nfor revision ",version$revision)
  }
  file_path <- dir(paths,pattern="gpkg",full.names=TRUE)
  file_path
}


#' Load BCA spatial polygons
#' #'
#' @description
#' Will download the data if necessary.
#' @param query query to select and filer layers. Available layers are
#' * WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_ADDRESSES_SV
#' * WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_LEGAL_DESCRIPTS_SV
#' * WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_GNRL_PROP_VALUES_SV
#' * WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_SALES_SV
#' * WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV
#' * WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_LAND_CHARS_SV
#' @param version database version, requires revision number and persistent ID
#' @param api_key Abacus API key
#' @param cache_path cache path for local caching of the database
#' @param refresh optionally refresh the local database if set to \code{TRUE}
#' @return A database connection to the residential inventory table
#' @export
get_bca_spatial_folios <- function(query = "select * from \"WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV\"",
                                   version = list(revision="24"),
                                   api_key = Sys.getenv("ABACUS_API_TOKEN"),
                                   cache_path = Sys.getenv("ABACUS_CACHE_PATH"),
                                   refresh = FALSE) {

  file_path <- get_bca_spatial_gpkg(version = version,
                                    api_key = api_key,
                                    cache_path = cache_path,
                                    refresh = refresh)
  d<-sf::st_read(file_path,
                 query=query)
  d
}

