# Metadata, variable descriptions and JSON output for one trial.

SITE_COLUMNS = c(
  site_id = "Site identifier, trial id plus package code",
  trial_id = "ClimMob trial (project) identifier",
  country_iso3 = "Country, ISO 3166-1 alpha-3 code",
  country_name = "Country name",
  crop_name = "Crop tested in the trial",
  longitude = "Longitude in decimal degrees (WGS84), displaced to protect participants",
  latitude = "Latitude in decimal degrees (WGS84), displaced to protect participants",
  coordinate_source = "Where the original coordinate was taken (trial plot, farm, other geopoint, seed delivery point, missing, removed)",
  coordinate_uncertainty_m = "Radius in metres around the published point that contains the original point (displacement plus GPS accuracy)",
  coordinate_flag = "Check of the original coordinate against the country boundary (ok, near_border, missing, outside, swapped_candidate, sign_flip_candidate)",
  gender = "Gender of the participant",
  age = "Age of the participant in years at registration",
  plot_area_m2 = "Area of each plot in square metres, as reported")

PLOT_COLUMNS = c(
  site_id = "Site identifier, links to site_data",
  plot = "Plot letter within the site (A, B, C)",
  genotype_name = "Name of the genotype (variety) in the plot",
  collection_moment = "Data collection moment (form) as named in the trial",
  moment_days = "Planned days after registration of the collection moment",
  trait = "Variable name, harmonised across trials",
  trait_id = "Identifier of the harmonised question",
  trait_original = "Question code as used in the trial",
  value = "Value. For value_type rank, 1 is best and 3 is worst",
  value_type = "rank, vs_local, numeric, date, categorical, multiple or text",
  unit = "Unit of numeric values. Yield values refer to the entire plot",
  flag = "Quality flag (implausible_yield_per_area, outlier_vs_trial_median, derived)")

describe_variables = function(res, dict, crop) {
  f = res$fields[res$fields$publish, ]
  f$level = ifelse(f$kind == "site", "site", "plot")
  f$variable[f$value_kind == "age"] = "age"
  f$variable[f$is_gender] = "gender"
  f$variable[f$kind == "vs_local"] = paste0(f$variable[f$kind == "vs_local"], "_vs_local")
  f = f[!duplicated(paste(f$level, f$variable)), ]

  onto = dict$ontology[dict$ontology$crop == tolower(crop), ]
  vars = data.frame(
    level = f$level,
    variable_name = f$variable,
    description = f$description,
    harmonised_id = f$canonical_id,
    ontology_id = onto$ontology_id[match(f$canonical_id, onto$canonical_id)],
    value_type = f$value_kind,
    unit = ifelse(f$value_kind %in% c("numeric"), f$unit, NA),
    basis = ifelse(f$value_kind %in% c("numeric"), f$basis, NA),
    description_language = ifelse(is.na(f$lang), NA, f$lang),
    stringsAsFactors = FALSE)
  vars$unit[vars$value_type == "age"] = "year"
  vars$value_type[vars$value_type == "age"] = "numeric"

  # derived yield per hectare
  dv = unique(res$plot_data$trait[res$plot_data$flag %in% "derived"])
  if (length(dv) > 0) {
    vars = rbind(vars, data.frame(level = "plot", variable_name = dv,
      description = paste("Yield per hectare derived from", sub("_kg_ha$", "", dv),
                          "and the plot area"),
      harmonised_id = NA, ontology_id = NA, value_type = "numeric", unit = "kg/ha",
      basis = "hectare", description_language = "en", stringsAsFactors = FALSE))
  }

  # keep only variables that ended up in the data
  present = c(names(res$site_data), unique(res$plot_data$trait))
  vars = vars[vars$variable_name %in% present, ]

  # controlled vocabularies for categorical variables
  vars$controlled_vocabulary = NA_character_
  for (i in which(vars$value_type %in% c("categorical", "multiple", "rank", "vs_local"))) {
    v = if (vars$level[i] == "site") res$site_data[[vars$variable_name[i]]]
        else res$plot_data$value[res$plot_data$trait == vars$variable_name[i]]
    u = sort(unique(trimws(unlist(strsplit(as.character(v), ";")))))
    u = u[!is_blank(u)]
    if (length(u) > 0 && length(u) <= 50) vars$controlled_vocabulary[i] = paste(u, collapse = "|")
  }

  fixed = rbind(
    data.frame(level = "site", variable_name = names(SITE_COLUMNS), description = unname(SITE_COLUMNS)),
    data.frame(level = "plot", variable_name = names(PLOT_COLUMNS), description = unname(PLOT_COLUMNS)))
  fixed = fixed[fixed$level == "plot" | fixed$variable_name %in% names(res$site_data), ]
  for (v in setdiff(names(vars), names(fixed))) fixed[[v]] = NA
  fixed$description_language = "en"
  fixed$unit[fixed$variable_name == "age"] = "year"
  fixed$unit[fixed$variable_name == "plot_area_m2"] = "m2"
  fixed$unit[fixed$variable_name == "coordinate_uncertainty_m"] = "m"
  vars = rbind(fixed[, names(vars)], vars)
  vars = vars[!duplicated(paste(vars$level, vars$variable_name), fromLast = TRUE), ]
  rownames(vars) = NULL
  vars[, c("level", "variable_name", "description", "harmonised_id", "ontology_id",
           "value_type", "unit", "basis", "controlled_vocabulary", "description_language")]
}

build_metadata = function(x, res, dict, cfg, release) {
  na = function(v) if (is_blank(v %||% NA)) NULL else v
  crop = res$site_data$crop_name[1]
  dates = suppressWarnings(as.Date(substr(unlist(x$data[grep("_submitted_date$", names(x$data))]), 1, 10)))
  xy = res$site_data[!is.na(res$site_data$longitude), ]
  gen = res$site_data$gender %||% rep(NA, nrow(res$site_data))
  moments = unique(res$fields[res$fields$form != "REG", c("moment", "moment_days")])

  genotypes = res$genotypes
  if (!is.null(cfg$genotypes)) {
    g = cfg$genotypes
    i = match(genotypes$genotype_name, g$genotype_name)
    for (v in setdiff(names(g), "genotype_name")) genotypes[[v]] = g[[v]][i]
    has_role = !is_blank(genotypes$entry_type %||% NA)
    if ("entry_type" %in% names(genotypes)) genotypes$role[has_role] = genotypes$entry_type[has_role]
  }

  list(
    schema_version = "2.0",
    changelog = list(version = release$version, date = as.character(Sys.Date()),
                     notes = release$notes,
                     software = list(R = as.character(getRversion()),
                                     ClimMobTools = as.character(utils::packageVersion("ClimMobTools")),
                                     gosset = as.character(utils::packageVersion("gosset")),
                                     PlackettLuce = as.character(utils::packageVersion("PlackettLuce")))),
    identifier = release$doi,
    license = release$license,
    study = list(
      id = project_field(x, "project_id"),
      code = project_field(x, "project_cod"),
      title = cfg$title %||% project_field(x, "project_name"),
      description = cfg$description %||% project_field(x, "project_abstract"),
      type = cfg$trial_type %||% "on-farm",
      objective = na(cfg$objective),
      experimental_site = cfg$experimental_site %||% "farm",
      unit_of_analysis = cfg$unit_of_analysis %||% "genotype",
      country_iso2 = project_field(x, "project_cnty"),
      country_iso3 = res$site_data$country_iso3[1],
      country_name = res$site_data$country_name[1],
      source_platform = "ClimMob v3",
      collection_moments = moments),
    date = if (all(is.na(dates))) NULL else
      list(start = as.character(min(dates, na.rm = TRUE)),
           end = as.character(max(dates, na.rm = TRUE))),
    geo_location = if (nrow(xy) > 0) list(boundingBox = list(
      xmin = round(min(xy$longitude), 2), xmax = round(max(xy$longitude), 2),
      ymin = round(min(xy$latitude), 2), ymax = round(max(xy$latitude), 2))) else NULL,
    location_privacy = list(
      method = "Each site displaced to a random point within a fixed radius, reproducible per site id",
      radius_m = release$radius_m,
      admin_names = "District and village names are kept as entered by enumerators"),
    data_producer = list(
      name = cfg$institute %||% project_field(x, "project_affiliation"),
      identifier = na(cfg$ror_id),
      principal_investigator = cfg$pi %||% project_field(x, "project_pi"),
      email = cfg$pi_email %||% project_field(x, "project_piemail"),
      program = na(cfg$program)),
    crop = list(name = crop, taxon = na(cfg$crop_taxon)),
    participants = list(total = nrow(res$site_data),
                        women = sum(gen %in% "woman"),
                        men = sum(gen %in% "man"),
                        not_stated = sum(!gen %in% c("woman", "man"))),
    genotypes = genotypes,
    funder = cfg$funders,
    variables = describe_variables(res, dict, crop))
}

write_trial = function(out, dir) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  crop = gsub("[^a-z0-9]", "", tolower(out$metadata$crop$name))
  file = file.path(dir, paste0(crop, "-", out$metadata$study$id, ".json"))
  jsonlite::write_json(out, file, pretty = TRUE, auto_unbox = TRUE,
                       na = "null", null = "null", digits = NA)
  file
}
