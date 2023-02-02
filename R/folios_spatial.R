

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
get_bca_spatial_folios <- function(query = "select * from \"WHSE_HUMAN_CULTURAL_ECONOMIC_BCA_FOLIO_DESCRIPTIONS_SV",
                         version = list(date="20221005",persistent_id="hdl:11272.1/AB2/NXRVP9/KTMVPW"),
                         api_key = Sys.getenv("ABACUS_API_TOKEN"),
                         cache_path = Sys.getenv("ABACUS_CACHE_PATH"),
                         refresh = FALSE) {
  if (nchar(cache_path)==0) stop("Local cache path needs to be provided.")
  path <- file.path(cache_path,"bca_folios_spatial_file_20221005.zip")
  if (refresh || !file.exists(path)) {
    tmp=tempfile()
    r<-httr::GET(url="https://abacus.library.ubc.ca/api/access/datafile/:persistentId/",
                 query=list(persistentId=version$persistent_id),
                 httr::add_headers("X-Dataverse-key"=api_key),
                 httr::write_disk(tmp, overwrite=TRUE))
    utils::unzip(tmp, exdir = path)
  }
  d<-sf::st_read(file.path(path,"bca_folios.gpkg"),
                 query=query)
  d
}




