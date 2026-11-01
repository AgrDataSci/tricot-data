# Small helpers shared by the tricot export pipeline.

`%||%` = function(a, b) {
  if (is.null(a) || length(a) == 0 || all(is_blank(a))) b else a
}

# lookup table by name, NULL when the field has none
get_lookup = function(lookups, name) {
  if (length(name) == 0 || is.na(name) || !name %in% names(lookups)) return(NULL)
  lookups[[name]]
}

# column of a data.frame, or NA when the column does not exist
col_or_na = function(df, name) {
  if (!is.null(df) && name %in% names(df)) df[[name]] else rep(NA, NROW(df))
}

# first element of a project field, as character
project_field = function(x, name) {
  v = x$project[[name]]
  if (is.null(v) || length(v) == 0) return(NA_character_)
  v = as.character(v[[1]])
  if (is.na(v) || trimws(v) == "") NA_character_ else trimws(v)
}

# stable integer seed from a text id, used so that a site always gets the
# same jitter no matter the row order or which other sites are exported
id_seed = function(id) {
  v = utf8ToInt(enc2utf8(as.character(id)))
  as.integer(sum(as.numeric(v) * seq_along(v) * 7919) %% 2147483646) + 1L
}

# run an expression with a fixed seed and restore the caller's RNG state
with_seed = function(seed, expr) {
  has_old = exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (has_old) old = get(".Random.seed", envir = globalenv())
  on.exit({
    if (has_old) assign(".Random.seed", old, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
      rm(".Random.seed", envir = globalenv())
  })
  set.seed(seed)
  expr
}

# clean variable name for the published data: lower case, no qst_ prefix
clean_code = function(x) {
  x = tolower(trimws(x))
  x = sub("^qst_", "", x)
  x = gsub("[^a-z0-9_]", "", x)
  x
}

is_blank = function(x) is.na(x) | trimws(as.character(x)) %in% c("", "None", "NA", "nan")

# rbind data frames that do not share all columns
rbind_fill = function(lst) {
  lst = lst[!vapply(lst, is.null, logical(1))]
  if (length(lst) == 0) return(NULL)
  cols = unique(unlist(lapply(lst, names)))
  do.call(rbind, lapply(lst, function(d) {
    for (v in setdiff(cols, names(d))) d[[v]] = NA
    d[cols]
  }))
}
