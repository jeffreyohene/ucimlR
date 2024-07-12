#' Get UCI Dataset
#'
#' Takes a dataset id and downloads a dataset from the UCI Machine Learning Repository.
#'
#'To view datasets you can browse this spreadsheet:
#' https://docs.google.com/spreadsheets/d/1_Smac2zek_Nvq7kLjOCvHduWs4or-6QgqRvOJV52nuA/edit?usp=sharing
#'
#' Placeholder: A function will be developed to see areas and links in RStudio
#' and a shorter url will be provided later
#'
#' @param id Character or numeric ID of the dataset.
#' @return None. Downloads the file and saves it to the current directory.
#' @examples
#' get_uci_dataset("5")
#' @export
#'

get_uci_dataset <- function(id) {

  # required libraries
  required_packages <- c("rvest", "httr")

  # install library if necessary, then load it
  for (package in required_packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }

  # handling to ensure id is a character
  if (!is.character(id)) {
    id <- as.character(id)
  }

  # build url
  base_url <- "https://archive.ics.uci.edu/dataset/"
  dest_url <- paste0(base_url, id)

  tryCatch({
    # parse the html content of the page
    page <- read_html(dest_url)

    # find download link based on a node) they use .zip so straightforward grab
    download_link <- page %>%
      html_node('a[href$=".zip"]') %>%
      html_attr('href')

    # after some tests some links break, this if block might be refactored later
    if (!is.null(download_link)) {
      # check if link is a relative url
      if (!grepl("^http", download_link)) {
        # build full url for the download link
        download_url <- paste0("https://archive.ics.uci.edu", download_link)
      } else {
        download_url <- download_link
      }

      # download file
      response <- GET(download_url, timeout(60))

      # outcome handling
      if (http_type(response) %in% c("application/zip", "application/octet-stream", "application/x-gzip", "application/x-tar", "text/plain")) {
        # save the file to the current directory
        content <- content(response, as = "raw")
        file_name <- basename(download_url)
        writeBin(content, file_name)
        message("File successfully downloaded to your project directory.")
      } else {
        warning("File download failed. Content type: ", http_type(response))
      }
    } else {
      warning("Download link not found.")
    }
  }, error = function(e) {
    warning("An error occurred: ", e$message)
  })
}
