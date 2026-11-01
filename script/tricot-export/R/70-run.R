# Configuration per trial and the summary report of a run.

CONFIG_COLUMNS = c("project_id", "project_code", "project_name", "country", "pi_climmob",
                   "created", "include", "crop_name", "crop_taxon", "title", "description",
                   "objective", "trial_type", "experimental_site", "unit_of_analysis",
                   "institute", "ror_id", "pi", "pi_email", "program",
                   "partner_file", "genotype_file", "funder_file", "notes")

# one row per trial in the raw data, to be completed by hand
make_config_template = function(trials, file = "config/projects.csv") {
  rows = lapply(trials, function(x) {
    tech = tryCatch(as.character(x$combination$elements[[1]]$technology_name[1]),
                    error = function(e) NA_character_)
    data.frame(project_id = project_field(x, "project_id"),
               project_code = project_field(x, "project_cod"),
               project_name = project_field(x, "project_name"),
               country = project_field(x, "project_cnty"),
               pi_climmob = project_field(x, "project_pi"),
               created = substr(project_field(x, "project_creationdate"), 1, 10),
               include = NA,
               crop_name = tolower(tech),
               stringsAsFactors = FALSE)
  })
  out = do.call(rbind, rows)
  for (v in setdiff(CONFIG_COLUMNS, names(out))) out[[v]] = NA
  out = out[, CONFIG_COLUMNS]
  if (file.exists(file)) {
    old = utils::read.csv(file, stringsAsFactors = FALSE, na.strings = c("", "NA"))
    for (v in setdiff(CONFIG_COLUMNS, names(old))) old[[v]] = NA
    extra = setdiff(names(old), CONFIG_COLUMNS)
    for (v in extra) out[[v]] = NA
    out = rbind(old[, c(CONFIG_COLUMNS, extra)], out[!out$project_id %in% old$project_id, c(CONFIG_COLUMNS, extra)])
  }
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(out, file, row.names = FALSE, na = "")
  invisible(out)
}

project_config = function(config, project_id) {
  row = config[config$project_id == project_id, , drop = FALSE]
  if (nrow(row) == 0) return(list(include = FALSE, in_config = FALSE))
  cfg = lapply(as.list(row[1, ]), function(v) if (is_blank(v)) NULL else v)
  cfg$include = isTRUE(as.logical(row$include[1]))
  cfg$in_config = TRUE
  
  # a path that is filled but does not exist is a typo, not a missing file
  usable = function(path, column) {
    if (is.null(path)) return(FALSE)
    if (file.exists(path)) return(TRUE)
    warning(sprintf("%s: %s '%s' not found, column ignored", project_id, column, path),
            call. = FALSE)
    FALSE
  }
  
  # v1 sheets first
  if (usable(cfg$genotype_file, "genotype_file")) {
    g = read_sheet(cfg$genotype_file, readxl::excel_sheets(cfg$genotype_file)[1])
    if ("crop_name" %in% names(g) && !is.null(cfg$crop_name)) {
      g = g[tolower(trimws(g$crop_name)) %in% tolower(cfg$crop_name), ]
    }
    cfg$genotypes = g
  }
  if (usable(cfg$funder_file, "funder_file")) {
    fu = read_sheet(cfg$funder_file, readxl::excel_sheets(cfg$funder_file)[1])
    fu = fu[!is_blank(fu[[1]]), , drop = FALSE]
    if (nrow(fu) > 0) {
      cfg$funders = data.frame(funderName = fu[[1]], funderIdentifier = fu[[2]],
                               awardTitle = fu[[3]], awardNumber = fu[[4]],
                               stringsAsFactors = FALSE)
    }
  }
  
  # the returned partner workbook overrides what it contains
  if (usable(cfg$partner_file, "partner_file")) {
    pf = read_partner_file(cfg$partner_file, crop = cfg$crop_name)
    if (NROW(pf$genotypes) > 0) cfg$genotypes = pf$genotypes
    if (NROW(pf$authors) > 0) cfg$authors = pf$authors
    if (NROW(pf$funders) > 0) cfg$funders = pf$funders
  }
  cfg
}

# exact renaming of genotypes from the partner's genotype sheet
apply_genotype_names = function(res, g) {
  if (is.null(g) || !all(c("genotype_name", "final_genotype_name") %in% names(g))) return(res)
  g = g[!is_blank(g$final_genotype_name), ]
  i = match(res$plot_data$genotype_name, g$genotype_name)
  res$plot_data$genotype_name[!is.na(i)] = g$final_genotype_name[i[!is.na(i)]]
  j = match(res$genotypes$genotype_name, g$genotype_name)
  res$genotypes$genotype_name[!is.na(j)] = g$final_genotype_name[j[!is.na(j)]]
  res$genotypes = res$genotypes[!duplicated(res$genotypes$genotype_name), , drop = FALSE]
  # attributes are looked up by the final name
  g2 = g[!duplicated(g$final_genotype_name), ]
  g2$genotype_name = g2$final_genotype_name
  g2$final_genotype_name = NULL
  res$genotype_sheet = g2
  res
}

report_row = function(x, res = NULL, error = NULL) {
  base = data.frame(project_id = project_field(x, "project_id"),
                    project_code = project_field(x, "project_cod"),
                    country = project_field(x, "project_cnty"),
                    status = if (is.null(error)) "exported" else "failed",
                    message = if (is.null(error)) NA_character_ else error,
                    stringsAsFactors = FALSE)
  if (is.null(res)) return(base)
  r = res$report
  tab = function(t, k) if (is.null(t) || !k %in% names(t)) 0L else as.integer(t[[k]])
  rk = do.call(rbind, r$rankings)
  pd = res$plot_data
  unmapped = unique(res$fields$code[res$fields$publish & is.na(res$fields$canonical_id)])
  cbind(base, data.frame(
    crop = res$site_data$crop_name[1],
    sites = r$n_sites,
    xy_trial_plot = tab(r$coordinate_source, "trial plot"),
    xy_farm = tab(r$coordinate_source, "farm"),
    xy_other = tab(r$coordinate_source, "other geopoint"),
    xy_delivery_point = tab(r$coordinate_source, "seed delivery point"),
    xy_missing = tab(r$coordinate_source, "missing"),
    xy_removed_outside_country = tab(r$coordinate_source, "removed"),
    xy_near_border = tab(r$coordinate_flag, "near_border"),
    xy_reused_from_previous = r$reused_coordinates,
    rank_traits = length(unique(paste(pd$collection_moment, pd$trait)[pd$value_type == "rank"])),
    rank_traits_below_nmin = if (is.null(rk)) 0L else sum(rk[, "valid"] < max(2, floor(r$n_sites * 0.2))),
    rank_sites_valid = if (is.null(rk)) 0L else sum(rk[, "valid"]),
    rank_answers_dropped = if (is.null(rk)) 0L else sum(rk[, "answered"] - rk[, "valid"]),
    age_birth_year_converted = tab(r$age, "birth_year_converted"),
    age_removed = tab(r$age, "implausible_removed"),
    yield_values = sum(pd$trait %in% res$fields$variable[res$fields$is_yield]),
    yield_implausible = tab(r$yield_flags, "implausible_yield_per_area"),
    yield_outliers = tab(r$yield_flags, "outlier_vs_trial_median"),
    yield_kg_ha_derived = sum(pd$flag %in% "derived"),
    dropped_unit_not_confirmed = tab(r$dropped_fields, "unit or basis not confirmed in dictionary"),
    dropped_numeric_not_in_dictionary = tab(r$dropped_fields, "numeric question not in dictionary"),
    unmapped_codes = paste(unmapped, collapse = "; "),
    stringsAsFactors = FALSE))
}

# full pipeline for one trial, returns the list written to JSON
run_trial = function(x, dict, cfg, release, previous_xy = NULL, boundary_path = "data/gadm") {
  res = export_trial_v3(x, dict, cfg, radius_m = release$radius_m,
                        boundary_path = boundary_path, previous_xy = previous_xy,
                        nmin = release$nmin)
  res = apply_genotype_names(res, cfg$genotypes)
  if (!is.null(res$genotype_sheet)) cfg$genotypes = res$genotype_sheet
  meta = build_metadata(x, res, dict, cfg, release)
  ra = rank_analysis(res$plot_data, meta$genotypes, min_sites = release$min_sites)
  list(out = list(metadata = meta,
                  site_data = res$site_data,
                  plot_data = res$plot_data,
                  rank_analysis = ra),
       res = res)
}
