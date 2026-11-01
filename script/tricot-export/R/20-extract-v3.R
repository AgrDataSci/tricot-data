# Read the raw ClimMob v3 object (class CM_list, as returned by
# ClimMobTools::getDataCM) without the tidy renaming of as.data.frame(),
# which strips "qst_" and accented letters and breaks matching with the
# question dictionary.

# all form fields with their form, moment, type and parsed question code
cm_fields = function(x) {

  mk = function(fi, form, moment, days) {
    if (!is.data.frame(fi) || nrow(fi) == 0) return(NULL)
    data.frame(form = form,
               moment = moment,
               moment_days = days,
               name = as.character(fi$name),
               odktype = as.character(col_or_na(fi, "odktype")),
               desc = as.character(col_or_na(fi, "desc")),
               rtable = as.character(col_or_na(fi, "rtable")),
               stringsAsFactors = FALSE)
  }

  out = list(mk(as.data.frame(x$registry$fields), "REG", "registration", 0))

  a = x$assessments
  if (is.data.frame(a) && nrow(a) > 0) {
    for (i in seq_len(nrow(a))) {
      out[[length(out) + 1]] = mk(a$fields[[i]],
                                  paste0("ASS", a$code[i]),
                                  trimws(as.character(a$desc[i])),
                                  suppressWarnings(as.integer(a$intervalindays[i])))
    }
  }

  f = do.call(rbind, out)
  f$column = paste(f$form, f$name, sep = "_")
  f$kind = "site"
  f$code = f$name
  f$slot = NA_character_

  parse = function(pattern, kind) {
    todo = f$kind == "site"
    r = regmatches(f$name, regexec(pattern, f$name))
    hit = todo & lengths(r) == 3
    f$kind[hit] <<- kind
    f$code[hit] <<- vapply(r[hit], `[`, "", 2)
    f$slot[hit] <<- vapply(r[hit], `[`, "", 3)
  }
  parse("^char_(.*)_(pos|neg)$", "rank")
  parse("^perf_(.*)_([123])$", "vs_local")
  parse("^(.*)_([abc])$", "option")

  f$moment_order = match(f$form, unique(f$form))
  f
}

# lookup tables as named character vectors, keyed by table name
cm_lookups = function(x) {
  tabs = list()
  add = function(lk) {
    if (!is.data.frame(lk) || nrow(lk) == 0) return(invisible())
    for (i in seq_len(nrow(lk))) {
      v = lk$values[[i]]
      if (!is.data.frame(v) || ncol(v) < 2) next
      tabs[[as.character(lk$name[i])]] <<- stats::setNames(as.character(v[[2]]),
                                                           as.character(v[[1]]))
    }
  }
  add(x$registry$lkptables)
  if (is.data.frame(x$assessments)) for (lk in x$assessments$lkptables) add(lk)
  tabs
}

# decode coded answers with exact matching, keep unknown codes as they are
decode_values = function(v, table, multi = FALSE) {
  v = as.character(v)
  if (is.null(table)) return(v)
  one = function(s) {
    s = s[!is_blank(s)]
    if (length(s) == 0) return(NA_character_)
    lab = unname(table[s])
    lab[is.na(lab)] = s[is.na(lab)]
    paste(lab, collapse = "; ")
  }
  if (!multi) {
    out = unname(table[v])
    out[is.na(out)] = v[is.na(out)]
    out[is_blank(v)] = NA_character_
    return(out)
  }
  vapply(strsplit(v, " +"), one, character(1))
}

# variety names in each package, one row per package
cm_packages = function(x) {
  p = x$packages
  items = lapply(p$comps, function(cp) {
    vapply(cp$technologies, function(t) as.character(t$alias_name[1]), character(1))
  })
  ncomp = lengths(items)
  if (any(ncomp != 3)) stop("Packages with other than three options are not supported")
  items = do.call(rbind, items)
  data.frame(package_id = as.character(p$package_id),
             item_A = items[, 1], item_B = items[, 2], item_C = items[, 3],
             stringsAsFactors = FALSE)
}

# "lat lon altitude accuracy" strings from ODK geopoints
parse_geopoint = function(s) {
  s = as.character(s)
  s[is_blank(s)] = NA_character_
  p = strsplit(s, " +")
  num = function(k) vapply(p, function(z) suppressWarnings(as.numeric(z[k])), numeric(1))
  out = data.frame(latitude = num(1), longitude = num(2), accuracy_m = num(4))
  bad = !is.na(out$latitude) &
    (abs(out$latitude) > 90 | abs(out$longitude) > 180 |
       (out$latitude == 0 & out$longitude == 0))
  out[bad, ] = NA
  out
}

# one coordinate per site, chosen by source priority
# trial plot > farm > other geopoint question > seed delivery point
cm_coordinates = function(x, f) {
  d = x$data
  geo = f[f$column %in% names(d) & (f$odktype %in% "geopoint" |
            grepl("geotrial|farmgoelocation|farmgeo|pointofdelivery|gps", f$name)), ]
  geo = geo[!grepl("^_", geo$name), ]
  n = nrow(d)
  res = data.frame(longitude = rep(NA_real_, n), latitude = NA_real_,
                   accuracy_m = NA_real_, coordinate_source = NA_character_,
                   stringsAsFactors = FALSE)
  if (nrow(geo) == 0) return(res)
  geo$source = ifelse(grepl("geotrial|plot", geo$name), "trial plot",
                ifelse(grepl("farm", geo$name), "farm",
                ifelse(grepl("pointofdelivery|delivery", geo$name), "seed delivery point",
                       "other geopoint")))
  geo$priority = match(geo$source, c("trial plot", "farm", "other geopoint", "seed delivery point"))
  geo = geo[order(geo$priority, geo$moment_order), ]
  for (k in seq_len(nrow(geo))) {
    g = parse_geopoint(d[[geo$column[k]]])
    take = is.na(res$longitude) & !is.na(g$longitude)
    res$longitude[take] = g$longitude[take]
    res$latitude[take] = g$latitude[take]
    res$accuracy_m[take] = g$accuracy_m[take]
    res$coordinate_source[take] = geo$source[k]
  }
  res$coordinate_source[is.na(res$coordinate_source)] = "missing"
  res
}
