#' Function getHTML
#' 
#' Extracts HTML from an URL.
#' @param url Character string speficying a valid URL.
#' @param encoding Character value specifying the encoding. Defaults to "UTF-8".
#' @param save Logical value specifying whether the HTML-code should be saved to a csv-file.
#' @param prefix Character value specifying the beginning of the filename (in case of saving). If NULL (default) as.numeric(Sys.time()) is applied.
#' @details Extracts HTML from an URL. Assumes UTF-8 encoding by default. Returns a character element containing HTML-code.
#' @keywords scraping
#' @export
#' @examples
#' getHTML()

getHTML <- function (url = "https://scholar.google.de/citations?user=-TjY7oEAAAAJ&hl=de&oi=sra", 
    encoding = "UTF-8", save = F, prefix = NULL, closeCon = F, 
    method = "libcurl", headers = NULL, silent = T, curlGetHeaders = F, 
    browseURL = F, ...) 
{
    if (closeCon) {
        co = as.numeric(rownames(showConnections(all = T)))
        for (i in co[co > 2]) close(getConnection(i))
    }
    if (!silent) 
        message(paste0("Trying to get html from ", url))
    if (is.character(url)) {
        if (length(grep("(^http://|^https://|^ftp://|^file://)", 
            url)) == 0) 
            url = paste0("http://", url)
        if (curlGetHeaders) 
            message(paste(curlGetHeaders(url, verify = F), collapse = ""))
        if (browseURL) 
            browseURL(url)
        url = url(url, method = method, ...)
    }
    html = paste(readLines(url, encoding = encoding), collapse = "\n")
    if (save) 
        write.csv2(data.frame(gsub("[ ]+", " ", paste0("<", strsplit(html, 
            "<")[[1]]))), paste0(ifelse(is.character(prefix), 
            prefix, as.numeric(Sys.time())), "_result.csv"))
    if (closeCon) {
        co = as.numeric(rownames(showConnections(all = T)))
        for (i in co[co > 2]) close(getConnection(i))
    }
    return(invisible(html))
}
