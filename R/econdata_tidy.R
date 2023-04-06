
unit_mult_switch <- function(x) switch(x, "3" = " Thousand", "6" = " Million", "9" = " Billion", "12" = " Trillion", x)

# http://www.southafrica-canada.ca/south-africas-nine-provinces/
province_switch <- function(x) switch(x,
      WC = "Western Cape", EC = "Eastern Cape", FS = "Free State", GP = "Gauteng", KZN = "KwaZulu-Natal",
      LM = "Limpopo", MP = "Mpumalanga", NC = "Northern Cape", NW = "North West",
      AU = "All urban areas", TC = "Total country", PU = "Primary urban", SU = "Secondary Urban", x)

null2NA <- function(x) if(is.null(x)) NA_character_ else x

make_meta_label <- function(m, meta, conceptname = TRUE) {
  lab <- NULL
  if(conceptname) {
    for (l in names(m)) {
      ml <- meta[[l]]
      if(ml$is_dimension) {
        cl <- unclass(ml$codelist$codes)
        add <- cl[[2L]][which(cl[[1L]] == m[[l]])]
        if(l != "MNEMONIC" && length(cn <- ml$concept$name)) add <- paste(cn, add, sep = ": ")
        lab <- c(lab, add)
      }
    }
  } else {
    for (l in names(m)) {
      if(meta[[l]]$is_dimension) {
        cl <- unclass(meta[[l]]$codelist$codes)
        lab <- c(lab, cl[[2L]][which(cl[[1L]] == m[[l]])])
      }
    }
  }
  return(paste(lab, collapse = ", "))
}

# Check using app: https://www.econdata.co.za/app
econdata_make_label <- function(x, meta, conceptname) {

  m <- attr(x, "metadata")

  if(!is.null(meta)) {
    lab <- make_meta_label(m, meta, conceptname)
  } else { # Hard coded label: as before
    PROVINCE <- if(length(m$PROVINCE)) m$PROVINCE else m$REGION
    lab <- paste0(m$LABEL,
                  if(length(m$COMMENT) && nchar(m$COMMENT) < 80L) paste0(": ", m$COMMENT) else "",
                  if(length(PROVINCE)) paste0(": ", province_switch(PROVINCE)) else "",
                  if(length(m$DISTRICT)) paste0(": ", m$DISTRICT) else ""," (",
                  m$UNIT_MEASURE,
                  if(length(m$UNIT_MULT)) unit_mult_switch(m$UNIT_MULT) else "",
                  if(length(m$BASE_PER)) paste(", Base =", m$BASE_PER) else "",
                  if(length(m$SEASONAL_ADJUST) && m$SEASONAL_ADJUST == "S") ", Seasonally Adjusted)" else ")")
  }

  return(c(lab, null2NA(m$SOURCE_IDENTIFIER)))
}

# (Optional) list names for multi-version calls
add_version_names <- function(x, elem = "Dataflow") {
  versions <- sapply(x, function(z) attr(z, "metadata")[[elem]][[3L]])
  if(length(versions) == length(x) && !anyDuplicated(versions)) names(x) <- paste0("v", versions)
  return(x)
}

econdata_wide <- function(x, prettymeta = TRUE, conceptname = TRUE, ...) {
  if(is.null(attributes(x))) return(lapply(add_version_names(x), econdata_wide, prettymeta, conceptname))
  meta <- if(prettymeta) get_metadata(x) else NULL
  d <- unlist2d(x, "code", row.names = "date", DT = TRUE) |>
    dcast(date ~ code, value.var = "OBS_VALUE") |>
    fmutate(date = as.Date(date))
  labs <- sapply(x, econdata_make_label, meta, conceptname)
  nam <- names(d)[-1L]
  vlabels(d) <- c("Date", labs[1L, nam])
  vlabels(d, "source.code")[-1L] <- labs[2L, nam]
  attr(d, "metadata") <- attr(x, "metadata")
  return(qDT(d, keep.attr = TRUE))
}


econdata_extract_metadata <- function(x, allmeta, origmeta, meta, conceptname) {
  if(!allmeta && length(x) == 0L) return(NULL) # Omits non-observed series.
  m <- attr(x, "metadata")
  if(origmeta && is.null(meta)) return(m)

  if(origmeta) {
    out <- list()
    for (p in names(m)) {
      cc_nam <- meta[[p]]$concept$name
      cl <- meta[[p]]$codelist
      if(!is.null(cl)) {
        cl <- unclass(cl$codes)
        out[[gsub(" ", "_", tolower(cc_nam))]] <- cl[[2L]][which(cl[[1L]] == m[[p]])]
      } else {
        out[[gsub(" ", "_", tolower(cc_nam))]] <- m[[p]]
      }
    }
    return(out)
  }

  # Hard coded metadata, as before...
  if(is.null(meta)) {
    PROVINCE <- if(length(m$PROVINCE)) m$PROVINCE else m$REGION
    return(list(source_code = null2NA(m$SOURCE_IDENTIFIER),
                frequency = null2NA(m$FREQ),
                label = null2NA(m$LABEL),
                province = if(length(PROVINCE)) province_switch(PROVINCE) else NA_character_,
                district = null2NA(m$DISTRICT),
                unit_measure = null2NA(m$UNIT_MEASURE),
                unit_mult = if(length(m$UNIT_MULT)) unit_mult_switch(m$UNIT_MULT) else NA_character_,
                base_period = null2NA(m$BASE_PER),
                seas_adjust = null2NA(m$SEASONAL_ADJUST),
                comment = null2NA(m$COMMENT)))
  }

  #    codes                                             code_names
  # 1      T                                                  Trend
  # 2      C                    Trend-cycle data, calendar adjusted
  # 3      R                Trend-cycle data, not calendar adjusted
  # 4      K                                     Calendar component
  # 5      X                                     Seasonal component
  # 6      M                       Seasonal and calendar components
  # 7      I                                    Irregular component
  # 8      N Neither seasonally adjusted nor calendar adjusted data
  # 9      S        Seasonally adjusted data, not calendar adjusted
  # 10     W        Calendar adjusted data, not seasonally adjusted
  # 11     Y                  Calendar and seasonally adjusted data

  # Now hybrid hard-coded format...
  return(list(source_code = null2NA(m$SOURCE_IDENTIFIER),
              frequency = null2NA(m$FREQ),
              label = make_meta_label(m[names(m) %!in% c("FREQ", "SOURCE_IDENTIFIER", "SEASONAL_ADJUST", "UNIT_MEASURE", "UNIT_MULT", "BASE_PER", "COMMENT")], meta, conceptname),
              unit_measure = null2NA(m$UNIT_MEASURE),
              unit_mult = if(length(m$UNIT_MULT)) make_meta_label(m["UNIT_MULT"], meta, FALSE) else NA_character_,
              base_period = null2NA(m$BASE_PER),
              seas_adjust = null2NA(m$SEASONAL_ADJUST),
              comment = null2NA(m$COMMENT)))

}

econdata_long <- function(x, combine = FALSE, allmeta = FALSE, origmeta = FALSE, prettymeta = TRUE, conceptname = TRUE, ...) {
  if(is.null(attributes(x))) {
    res <- lapply(add_version_names(x), econdata_long, combine, allmeta, origmeta, prettymeta, conceptname)
    return(if(combine) rbindlist(res, use.names = TRUE, fill = TRUE) else res)
  }
  meta <- if(prettymeta) get_metadata(x) else NULL
  d <- unlist2d(x, "code", row.names = "date", DT = TRUE) |>
       fmutate(date = as.Date(date), code = qF(code)) |>
       frename(OBS_VALUE = "value")
  m <- attr(x, "metadata")
  meta <- lapply(x, econdata_extract_metadata, allmeta && !combine, origmeta, meta, conceptname) |>
          rbindlist(use.names = origmeta, fill = origmeta)
  if(origmeta) names(meta) <- tolower(names(meta))
  meta$code <- if(allmeta && !combine) names(x) else names(x)[names(x) %in% levels(d$code)]
  meta$source <- null2NA(m$DataProvider[[2L]])
  meta$dataset <- null2NA(m$Dataflow[[2L]])
  meta$source_dataset <- null2NA(m$SOURCE_DATASET)
  meta$version <- null2NA(m$Dataflow[[3L]])
  setcolorder(meta, c("source", "dataset", "source_dataset", "version", "code", if(!origmeta) "source_code"))
  if(!allmeta) get_vars(meta, fnobs(meta) == 0L) <- NULL
  if(combine) {
    meta_fct <- dapply(meta, qF, drop = FALSE) # Factors for efficient storage
    code <- d$code
    d$code <- NULL
    add_vars(d, "front") <- ss(meta_fct, ckmatch(code, meta_fct$code), check = FALSE)
    return(d)
  }
  return(list(data = d, metadata = meta))
}

# Tidying the output of read_release()
econdata_tidy_release <- function(x) {
  axnull <- is.null(attributes(x))
  if(axnull && length(x) > 1L) {
    res <- lapply(x, econdata_tidy_release)
    return(add_version_names(res, elem = "Flowref"))
  }
  if(axnull) x <- x[[1L]]
  res <- rbindlist(x$Releases)
  res$Date <- as.POSIXct(res$Date)
  names(res) <- tolower(names(res))
  attr(res, "metadata") <- x$DataSet
  return(qDT(res, keep.attr = TRUE))
}

# This is just needed to get rid of the wide argument for documenting this together with read_econdata()
econdata_tidy_core <- function(x, wide = TRUE, release = FALSE, ...)
  if(release) econdata_tidy_release(x) else if(wide) econdata_wide(x, ...) else econdata_long(x, ...)

econdata_tidy <- function(x, ...) econdata_tidy_core(x, ...)
