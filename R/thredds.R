####################################
####################################
#### thredds_url()


####################################
####################################
#### thredds_extract()


####################################
####################################
#### thredds_download()

#' @title Download FVCOM outputs from the SAMS thredds server
#' @description This function is used to download FVCOM outputs from the SAMS thredds server.
#' @param file_name A character vector of file name(s) which specify the files to be downloaded. These should contain a unique identifier for each file (for example, a date_name, see \code{\link[fvcom.tbx]{date_name}}) but they do not need to match file names precisely.
#' @param pattern A character string containing a regular expression which is matched to identify files (e.g., \code{".nc"}).
#' @param server_catalog A character that defines the URL of the catalog.
#' @param dest_file A character that defines the directory in which to save file(s).
#' @param verbose A logical input that defines whether or not to print messages to the console. These can be useful for monitoring progress.
#' @param prompt A logical input that defines whether or not to prompt the user before each download.
#' @param ... Additional arguments passed to \code{\link[utils]{download.file}}.
#' @details To download files, the function first identifies all files on \code{server_catalog} that match \code{pattern}. For each inputted \code{file_name}, the function identifies the full file name and the necessary URL to download that file from the server (a different URL from the catalog) and then downloads the file. If any files cannot be downloaded, the function will print an error message but continue to attempt to download the next file, until there are no remaining files. Note the following: (a) the function is only designed to query a single catalog at a time (i.e., is not recursive, so it will not identify all files on a thredds server, only the specific files in \code{server_catalog}; (b) files are large (usually ~ 700 Mb) and are downloaded in sequence; and (c) the function requires the xml2 and rvest package to be installed.
#' @return The function downloads specified files from the SAMS thredds server (https://thredds.sams.ac.uk/thredds/) where WeStCOMS outputs are located.
#' @examples
#' \dontrun{
#' # Warning: if you run this code, it may take a long time to download the file
#' # ... which is nearly 1 Gb in size!
#' thredds_download(file_name = "20190702",
#'                       pattern = ".nc",
#'                       server = paste0("https://thredds.sams.ac.uk/thredds/",
#'                                       "catalog/scoats-westcoms1/Archive/",
#'                                       "netcdf_2019/catalog.html"),
#'                                       dest_file = tempdir(),
#'                                       verbose = TRUE,
#'                                       prompt = TRUE)
#' }
#'
#' @author Edward Lavender
#' @export
#'

thredds_download <-
  function(
    file_name,
    pattern = ".nc",
    server_catalog,
    dest_file,
    verbose = TRUE,
    prompt = FALSE,...){

    require_xml2  <- !requireNamespace("xml2", quietly = TRUE)
    require_rvest <- !requireNamespace("rvest", quietly = TRUE)
    if(require_xml2) stop("xml2 package is required for this function. Run `install.packages('xml2')` to install.")
    if(require_rvest) stop("rvest package is required for this function. Run `install.packages('rvest')` to install.")

    #### Checks
    ## server_catalog ends in 'catalog.html'
    if(substr(server_catalog, nchar(server_catalog)-11, nchar(server_catalog)) != "catalog.html") stop("server_catalog does not end in 'catalog.html'.")
    ## Define server_file
    server_file <- substr(server_catalog, 1, nchar(server_catalog) - 12)
    server_file <- gsub("https://thredds.sams.ac.uk/thredds/catalog",
                        "https://thredds.sams.ac.uk/thredds/fileServer",
                        server_file)

    ## Check dest_file is a directory
    if(!dir.exists(dest_file)) stop("Input to 'dest_file' is not a directory in existence.")
    if(substr(dest_file, nchar(dest_file), nchar(dest_file)) != "/") dest_file <- paste0(dest_file, "/")

    #### Extract all file names and html links from server_catalog
    if(verbose) cat("Step 1: Querying server_catalog to identify files... \n")
    html_set <- xml2::read_html(server_catalog)
    node_set <- rvest::html_nodes(html_set, "a")
    name_set <- rvest::html_text(node_set)
    if(length(name_set) == 0) stop("Unable to identify any files on server_catalog.")
    # link_set <- rvest::html_attr(node_set, "href")

    #### Extract links that match pattern specified
    if(verbose) cat("Step 2: Identifying file names which match pattern...\n")
    name_set_is <- grepl(pattern, name_set)
    if(!any(name_set_is)) stop("Unable to identify files on server_catalog which match pattern = '", pattern, "'.")
    name_set_ptn <- name_set[name_set_is]
    # link_set_ptn <- link_set[grepl(pattern, link_set)]

    #### Download files
    if(verbose) cat("Step 3: Beginning file downloads...\n")
    # Loop over each file....
    lout <-
      pbapply::pblapply(file_name, function(file){

        ## Check if file name and link can be identified
        # file = file_name[1]
        if(verbose)  cat(paste0("Step 3 continued: identifying name and URL of file ", file, ".\n"))
        name_is <- grepl(file, name_set_ptn)
        # link_is <- grepl(file, link_set_ptn)
        if(!any(name_is)){
          warn <- paste0("Unable to identify file name on server_catalog associated with file ", file,
          ". (Is this file listed on the server_catalog?) Skipping this file...\n")
          message(warn)
          return(NULL)
        }

        ## Extract file name and URL link:
        if(verbose) cat(paste0("Step 3 continued: dowloading file ", file, "...\n"))
        if(prompt) readline(prompt = "Press [enter] to continue or [Esc] to end... \n")
        name <- name_set_ptn[name_is]
        # link <- link_set_ptn[link_is]
        # link <- substr(link, 13, nchar(link))
        url <- paste0(server_file, name)

        ## Attempt to download file
        try_download <-
          tryCatch(utils::download.file(url = url,
                                        dest_file = paste0(dest_file, name)),
                   error = function(e) return(e)
          )

        ## Message if unable to download file
        if(inherits(try_download, "error")){
          warn <- paste0("Unable to download file", file, ". ",
                         "File name identified for this file ", name, ". ",
                         "URL obtained for this file: ", url, ". ",
                         "Error message followinf download.file(...): ", try_download, ".\n")
          message(warn)
        }

      })

  }


#### End of code.
####################################
####################################
