get_nominatim_url <- function() {
  "https://nominatim.openstreetmap.org/search"
}

nominatim_check_for_error <- function(doc) {
  
}

nominatim_query <- function(query, quiet = FALSE, wait = TRUE, pad_wait = 5, encoding = "UTF-8") 
{
  if (missing(query)) 
    stop("query must be supplied", call. = FALSE)
  if (!is.list(query))
    stop("query must be a list of character strings")
  if (!is.logical(quiet)) 
    quiet <- FALSE
  if (!is.logical(wait)) 
    wait <- TRUE
  if (!is.numeric(pad_wait)) {
    message("pad_wait must be numeric; setting to 5s")
    pad_wait <- 5
  }
  if (pad_wait < 0) {
    warning("pad_wait must be positive; setting to 5s")
    pad_wait <- 5
  }
  if (!curl::has_internet()) 
    stop("Nominatim query unavailable without internet", call. = FALSE)
  message(sprintf("Waiting %s seconds", pad_wait))
  Sys.sleep(wait)
  if (!quiet) 
    message("Issuing query to Nominatim API ...")
  nominatim_url <- get_nominatim_url()
  res <- httr::GET(nominatim_url, query = query)
  if (!quiet) 
    message("Query complete!")
  if (class(res) == "result") 
    httr::stop_for_status(res)
  if (encoding == "pbf")
    doc <- httr::content(res, as = "raw")
  else if (class(res) == "raw") 
    doc <- rawToChar(res)
  else doc <- httr::content(res, as = "text", encoding = encoding, 
                            type = "application/xml")
  if (encoding != "pbf")
    nominatim_check_for_error(doc)
  return(doc)
}

nominatim_sf <- function(q, doc, quiet = TRUE, encoding) {
  if (missing(encoding)) 
    encoding <- "UTF-8"
  if (missing(q) & !quiet) 
    message("q missing: nominatim object will not include query")
#  else if (is(q, "overpass_query")) {
#    obj$bbox <- q$bbox
#    obj$overpass_call <- opq_string(q)
#  }
  if (is.character(q)) 
    nominatim_call <- q
  else
    stop("q must be a character string")
  if (missing(doc)) {
    doc <- nominatim_query(query = nominatim_call, quiet = quiet, 
                          encoding = encoding)
    timestamp <- osmdata:::get_timestamp()
  }
  else {
    if (is.character(doc)) {
      if (!file.exists(doc)) 
        stop("file ", doc, " does not exist")
      doc <- xml2::read_xml(doc)
    }
    obj$timestamp <- osmdata:::get_timestamp(doc)
    doc <- as.character(doc)
  }
  if (!quiet) 
    message("converting OSM data to sf format")

  # TODO: DO SHIT HERE...
}

# TESTING SHIT
nmq_query <- function(place) {
  list(format='json', limit=1, dedupe=0, polygon_geojson=1, q=place)
}

query <- nmq_query("Palmerston North, Manawatu, New Zealand")
query <- nmq_query("Manhattan, NYC, NY, USA")
