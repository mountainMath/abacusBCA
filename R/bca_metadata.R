extract_actual_use_codes <- function(){
  actual_use_codes <- get_bca_data("folioDescription") %>%
    dplyr::group_by(.data$actualUseCode,.data$actualUseDescription) %>%
    dplyr::count(name="Number of folios") %>%
    dplyr::arrange(.data$actualUseCode) %>%
    dplyr::collect()

  usethis::use_data(actual_use_codes)
  #save(actual_use_codes, file="data/actual_use_codes.RData")
  NULL
}

extract_manual_class_codes <- function(){
  url <- "https://www.bcassessment.ca/Files/Misc/UserGuide/Tables/dataadv12.pdf"
  tmp <- tempfile()
  utils::download.file(url,tmp)
  ts<-tabulizer::extract_text(tmp)
  tmp2<-tempfile()
  writeLines(ts,tmp2)
  manual_class_codes<-readr::read_fwf(tmp2,col_positions = readr::fwf_positions(start=c(1,5),
                                                            end=c(4,NA),
                                                            col_names = c("manualClassCode","manualClassDescription")),
                  trim_ws=TRUE) %>%
    dplyr::filter(!is.na(.data$manualClassCode),.data$manualClassCode!="CODE",.data$manualClassCode!="----")

  usethis::use_data(manual_class_codes)
  #save(manual_codes, file="data/manual_codes.RData")
  NULL
}



