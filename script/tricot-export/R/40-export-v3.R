# Export one ClimMob v3 trial (CM_list) into the release schema v2.0
#   metadata       study, crop, country, producer, variables, genotypes
#   site_data      one row per site (former block_data)
#   plot_data      long table, one row per site x plot x moment x trait
#   rank_analysis  Plackett-Luce log-worth per moment x trait x genotype

SYSTEM_FIELDS = "^_|^(surveyid|originid|qst162|qst163|instancename|rowuuid)$|^(clm|clc|cal)_|_oth$"
PII_FIELDS = paste0("farmername|participant_?name|^name$|^names$|telephone|telno|phone|mobile|",
                    "imei|deviceid|email|photo|picture|image|audio|video|signature|consent")
ADM_FIELDS = paste0("district|village|^adm[1-5]|_adm[1-5]|region|kebele|county|ward|commune|",
                    "municip|province|^state|departement|woreda|^zone|sector|^cell|parish|location")
KEEP_TEXT = paste0(ADM_FIELDS, "|localvariety|varietyname")

value_kind = function(odktype) {
  t = tolower(odktype)
  ifelse(t %in% c("integer", "decimal"), "numeric",
    ifelse(grepl("^date$", t), "date",
      ifelse(grepl("^select.?one", t), "categorical",
        ifelse(grepl("^select.?(all|multiple)", t), "multiple",
          ifelse(t == "text", "text",
            ifelse(t == "geopoint", "geopoint", "other"))))))
}

# decide which fields are published and with which name and unit
classify_fields = function(f, dict) {
  info = lookup_codes(f$code, dict)
  f = cbind(f, info)
  un = is.na(f$canonical_id)
  f$variable[un] = clean_code(f$code[un])
  f$description[un] = f$desc[un]
  f$value_kind = value_kind(f$odktype)
  f$value_kind[f$kind == "rank"] = "rank"
  f$value_kind[f$kind == "vs_local"] = "vs_local"

  nm = tolower(f$name)
  f$publish = TRUE
  f$reason = NA_character_
  drop = function(cond, why) {
    hit = f$publish & cond
    f$publish[hit] <<- FALSE
    f$reason[hit] <<- why
  }
  drop(grepl(SYSTEM_FIELDS, nm), "system field")
  drop(grepl(PII_FIELDS, nm), "personal information")
  drop(f$value_kind %in% c("geopoint"), "coordinates handled separately")
  drop(f$value_kind %in% c("other"), "unsupported type")
  drop(f$value_kind == "text" & !grepl(KEEP_TEXT, tolower(f$code)), "free text")
  is_age = grepl("^age|^agefr$|^age$", f$variable) & f$kind == "site"
  f$value_kind[is_age] = "age"
  num_ok = f$value_kind != "numeric" | f$action %in% "publish"
  drop(!num_ok & is.na(f$canonical_id), "numeric question not in dictionary")
  drop(!num_ok, "unit or basis not confirmed in dictionary")
  f$is_gender = f$kind == "site" & grepl("^gender|^sex|^genre", f$variable)
  f$is_yield = f$variable_class %in% "core_yield"
  f$is_plot_area = f$kind == "site" & (f$variable_class %in% "plot_area" |
                                         grepl("plotsize$|plotsizefr$", tolower(f$code)))
  f
}

convert_values = function(v, kind, rtable, lookups) {
  if (kind %in% c("numeric", "age")) return(suppressWarnings(as.numeric(as.character(v))))
  if (kind == "date") {
    d = suppressWarnings(as.Date(substr(as.character(v), 1, 10)))
    return(as.character(d))
  }
  if (kind %in% c("categorical", "multiple")) {
    return(decode_values(v, get_lookup(lookups, rtable), multi = kind == "multiple"))
  }
  out = trimws(as.character(v))
  out[is_blank(out)] = NA_character_
  out
}

export_trial_v3 = function(x, dict, cfg = list(), radius_m = 1500,
                           boundary_path = "data/gadm", previous_xy = NULL,
                           nmin = 0.2) {

  project_id = project_field(x, "project_id")
  d = as.data.frame(x$data, stringsAsFactors = FALSE)
  if (nrow(d) < 2) stop("No data in project ", project_id)

  f = cm_fields(x)
  f = f[f$column %in% names(d), ]
  f = classify_fields(f, dict)
  lookups = cm_lookups(x)

  # ---- sites and packages ----
  pk = cm_packages(x)
  d$package_id = as.character(d$REG_qst162)
  d = d[!is.na(d$package_id) & !duplicated(d$package_id) & d$package_id %in% pk$package_id, ]
  items = pk[match(d$package_id, pk$package_id), c("item_A", "item_B", "item_C")]
  site_id = paste(project_id, d$package_id, sep = "-")
  n = nrow(d)

  iso = iso2_to_iso3(project_field(x, "project_cnty"))
  crop = cfg$crop_name %||% tolower(as.character(x$combination$elements[[1]]$technology_name[1]))
  reg_year = rep(as.numeric(format(Sys.Date(), "%Y")), n)
  if ("REG__submitted_date" %in% names(d)) {
    ry = suppressWarnings(as.numeric(substr(as.character(d$REG__submitted_date), 1, 4)))
    reg_year[!is.na(ry)] = ry[!is.na(ry)]
  }

  # ---- coordinates ----
  xy = cm_coordinates(x, f[, c("form", "name", "odktype", "column", "moment_order")])
  xy = xy[match(rownames(d), rownames(x$data)), ]
  boundary = tryCatch(get_country(iso$iso3, boundary_path), error = function(e) NULL)
  coord_flag = if (is.null(boundary)) ifelse(is.na(xy$longitude), "missing", "not_checked")
               else check_country(xy$longitude, xy$latitude, boundary)
  bad_xy = coord_flag %in% c("outside", "swapped_candidate", "sign_flip_candidate")
  xy$longitude[bad_xy] = NA
  xy$latitude[bad_xy] = NA
  pub = jitter_sites(site_id, xy$longitude, xy$latitude, radius_m, boundary)
  pub = reuse_published(site_id, pub, previous_xy)

  site = data.frame(
    site_id = site_id,
    trial_id = project_id,
    country_iso3 = iso$iso3,
    country_name = iso$country_name,
    crop_name = crop,
    longitude = pub$longitude,
    latitude = pub$latitude,
    coordinate_source = ifelse(bad_xy, "removed", xy$coordinate_source),
    coordinate_uncertainty_m = ifelse(is.na(pub$longitude), NA,
                                      round(radius_m + ifelse(is.na(xy$accuracy_m), 0, xy$accuracy_m))),
    coordinate_flag = coord_flag,
    stringsAsFactors = FALSE)

  # ---- site level variables, first non-missing value across forms ----
  sf_ = f[f$publish & f$kind == "site", ]
  sf_ = sf_[order(sf_$moment_order), ]
  flags = list()
  for (v in unique(sf_$variable)) {
    g = sf_[sf_$variable == v, ]
    val = rep(NA, n)
    for (k in seq_len(nrow(g))) {
      cv = convert_values(d[[g$column[k]]], g$value_kind[k], g$rtable[k], lookups)
      take = is_blank(val) & !is_blank(cv)
      val[take] = cv[take]
    }
    if (g$value_kind[1] == "age") {
      a = clean_age(val, reg_year)
      val = a$value
      flags$age = table(a$flag)
      v = "age"
    }
    if (isTRUE(g$is_gender[1])) {
      val = clean_gender(val)
      v = "gender"
    }
    site[[v]] = val
  }

  # plot area in m2, used for yield per hectare
  area = rep(NA_real_, n)
  pa = f[f$is_plot_area & f$kind == "site", ]
  for (k in seq_len(nrow(pa))) {
    unit_col = f$column[f$form == pa$form[k] & grepl("sizeunit", f$name)][1]
    unit_lab = if (!is.na(unit_col)) {
      ut = f$rtable[f$column == unit_col][1]
      decode_values(d[[unit_col]], get_lookup(lookups, ut))
    } else rep(NA, n)
    a = plot_area_m2(d[[pa$column[k]]], unit_lab)
    area[is.na(area)] = a[is.na(area)]
  }
  site$plot_area_m2 = area

  # ---- plot level measurements (questions per option) ----
  of = f[f$publish & f$kind == "option", ]
  plot_rows = list()
  for (key in unique(paste(of$form, of$code))) {
    g = of[paste(of$form, of$code) == key, ]
    if (!all(c("a", "b", "c") %in% g$slot)) next
    for (s in c("a", "b", "c")) {
      r = g[g$slot == s, ][1, ]
      cv = convert_values(d[[r$column]], r$value_kind, r$rtable, lookups)
      plot_rows[[length(plot_rows) + 1]] = data.frame(
        site_id = site_id, plot = toupper(s),
        genotype_name = items[[paste0("item_", toupper(s))]],
        collection_moment = r$moment, moment_days = r$moment_days,
        trait = r$variable, trait_id = r$canonical_id, trait_original = r$code,
        value = as.character(cv), value_type = r$value_kind, unit = r$unit,
        flag = NA_character_, stringsAsFactors = FALSE)
    }
  }
  measured = do.call(rbind, plot_rows)
  if (is.null(measured)) measured = data.frame()
  if (nrow(measured) > 0) measured = measured[!is_blank(measured$value), ]

  # ---- yield checks and yield per hectare ----
  derived = NULL
  if (nrow(measured) > 0) {
    yv = measured$trait %in% f$variable[f$is_yield]
    if (any(yv)) {
      y = measured[yv, ]
      kg = as.numeric(y$value) * ifelse(y$unit %in% "g", 0.001, 1)
      a = site$plot_area_m2[match(y$site_id, site$site_id)]
      kgha = kg / a * 10000
      lim = max_yield_t_ha(crop) * 1000
      med = stats::ave(kg, y$trait, y$collection_moment,
                       FUN = function(z) stats::median(z[z > 0], na.rm = TRUE))
      flag = ifelse(!is.na(kgha) & kgha > lim, "implausible_yield_per_area",
              ifelse(is.na(kgha) & kg > 20 * med, "outlier_vs_trial_median", NA))
      measured$flag[which(yv)] = flag
      ok = !is.na(kgha) & is.na(flag)
      if (any(ok)) {
        derived = y[ok, ]
        derived$trait = paste0(derived$trait, "_kg_ha")
        derived$trait_id = NA
        derived$value = as.character(round(kgha[ok], 1))
        derived$unit = "kg/ha"
        derived$flag = "derived"
      }
    }
  }

  # ---- tricot rankings ----
  rk = f[f$publish & f$kind == "rank", ]
  rank_rows = list()
  dropped = list()
  for (key in unique(paste(rk$form, rk$code))) {
    g = rk[paste(rk$form, rk$code) == key, ]
    if (!all(c("pos", "neg") %in% g$slot)) next
    b = suppressWarnings(as.integer(d[[g$column[g$slot == "pos"][1]]]))
    w = suppressWarnings(as.integer(d[[g$column[g$slot == "neg"][1]]]))
    valid = !is.na(b) & !is.na(w) & b %in% 1:3 & w %in% 1:3 & b != w
    dropped[[key]] = c(answered = sum(!is.na(b) | !is.na(w)), valid = sum(valid))
    if (sum(valid) < max(2, floor(n * nmin))) next
    mid = 6L - b - w
    for (s in 1:3) {
      rank = ifelse(b == s, 1L, ifelse(w == s, 3L, ifelse(mid == s, 2L, NA)))
      rank_rows[[length(rank_rows) + 1]] = data.frame(
        site_id = site_id[valid], plot = LETTERS[s],
        genotype_name = items[[paste0("item_", LETTERS[s])]][valid],
        collection_moment = g$moment[1], moment_days = g$moment_days[1],
        trait = g$variable[1], trait_id = g$canonical_id[1], trait_original = g$code[1],
        value = as.character(rank[valid]), value_type = "rank", unit = NA_character_,
        flag = NA_character_, stringsAsFactors = FALSE)
    }
  }
  ranks = do.call(rbind, rank_rows)

  # ---- comparison with the local variety ----
  vl = f[f$publish & f$kind == "vs_local", ]
  vs_rows = list()
  for (key in unique(paste(vl$form, vl$code))) {
    g = vl[paste(vl$form, vl$code) == key, ]
    for (s in 1:3) {
      r = g[g$slot == as.character(s), ][1, ]
      if (is.na(r$column)) next
      raw = as.character(d[[r$column]])
      lab = tolower(decode_values(raw, get_lookup(lookups, r$rtable)))
      lab = ifelse(lab %in% c("1", "better"), "better", ifelse(lab %in% c("2", "worse"), "worse", lab))
      keep = !is_blank(lab)
      if (!any(keep)) next
      vs_rows[[length(vs_rows) + 1]] = data.frame(
        site_id = site_id[keep], plot = LETTERS[s],
        genotype_name = items[[paste0("item_", LETTERS[s])]][keep],
        collection_moment = r$moment, moment_days = r$moment_days,
        trait = paste0(r$variable, "_vs_local"), trait_id = r$canonical_id, trait_original = r$code,
        value = lab[keep], value_type = "vs_local", unit = NA_character_,
        flag = NA_character_, stringsAsFactors = FALSE)
    }
  }
  vs_local = do.call(rbind, vs_rows)

  parts = list(ranks, vs_local, if (nrow(measured) > 0) measured, derived)
  parts = parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0) stop("No rankings or measurements in project ", project_id)
  plot_data = do.call(rbind, parts)
  rownames(plot_data) = NULL

  # ---- metadata pieces ----
  geno = unique(unlist(items))
  genotypes = data.frame(genotype_name = geno,
                         role = ifelse(grepl("check|local|témoin|temoin", geno, ignore.case = TRUE),
                                       "check", NA_character_),
                         stringsAsFactors = FALSE)

  list(site_data = site,
       plot_data = plot_data,
       fields = f,
       genotypes = genotypes,
       lookups = lookups,
       report = list(n_sites = n,
                     coordinate_source = table(site$coordinate_source),
                     coordinate_flag = table(coord_flag),
                     reused_coordinates = sum(pub$reused),
                     rankings = dropped,
                     age = flags$age,
                     yield_flags = if (nrow(measured) > 0) table(measured$flag, useNA = "no") else NULL,
                     dropped_fields = table(f$reason, useNA = "no")))
}
