# Cleaning rules for the core variables: coordinates, age, gender, plot area
# and yield. Every rule flags what it changes so the validation report can
# show it.

# ---- country boundaries -------------------------------------------------

iso2_to_iso3 = function(iso2) {
  cc = geodata::country_codes()
  i = match(toupper(iso2), cc$ISO2)
  data.frame(iso3 = cc$ISO3[i], country_name = cc$NAME[i], stringsAsFactors = FALSE)
}

get_country = function(iso3, path) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  b = geodata::gadm(iso3, level = 0, path = path, resolution = 2)
  b = terra::as.data.frame(b, geom = "WKT")
  sf::st_make_valid(sf::st_as_sf(b, wkt = "geometry", crs = 4326))
}

# distance in metres from each point to the country polygon (0 = inside)
dist_to_country = function(lon, lat, boundary) {
  out = rep(NA_real_, length(lon))
  ok = !is.na(lon) & !is.na(lat)
  if (!any(ok)) return(out)
  p = sf::st_as_sf(data.frame(lon = lon[ok], lat = lat[ok]),
                   coords = c("lon", "lat"), crs = 4326)
  out[ok] = as.numeric(sf::st_distance(p, boundary))[seq_len(sum(ok))]
  out
}

# flag points outside the country and test the usual entry errors
check_country = function(lon, lat, boundary, tol_m = 5000) {
  d = dist_to_country(lon, lat, boundary)
  flag = ifelse(is.na(d), "missing",
           ifelse(d == 0, "ok",
             ifelse(d <= tol_m, "near_border", "outside")))
  out = which(flag == "outside")
  if (length(out) > 0) {
    sw = dist_to_country(lat[out], lon[out], boundary)
    sl = dist_to_country(-lon[out], lat[out], boundary)
    sa = dist_to_country(lon[out], -lat[out], boundary)
    flag[out[sw == 0]] = "swapped_candidate"
    flag[out[sw != 0 & (sl == 0 | sa == 0)]] = "sign_flip_candidate"
  }
  flag
}

# ---- coordinate protection ----------------------------------------------

# random point within radius_m of each site, reproducible per site id,
# redrawn when the displaced point leaves the country
jitter_sites = function(site_id, lon, lat, radius_m, boundary = NULL, tries = 10) {
  n = length(site_id)
  out = data.frame(longitude = rep(NA_real_, n), latitude = NA_real_)
  draw = function(k, t) {
    with_seed(id_seed(paste(site_id[k], t)), {
      ang = stats::runif(1, 0, 2 * pi)
      r = radius_m * sqrt(stats::runif(1))
      c(lon[k] + r * sin(ang) / (111320 * cos(lat[k] * pi / 180)),
        lat[k] + r * cos(ang) / 110540)
    })
  }
  todo = which(!is.na(lon) & !is.na(lat))
  for (try_i in seq_len(tries)) {
    if (length(todo) == 0) break
    xy = t(vapply(todo, draw, numeric(2), t = try_i))
    out$longitude[todo] = round(xy[, 1], 5)
    out$latitude[todo] = round(xy[, 2], 5)
    if (is.null(boundary)) break
    inside = dist_to_country(out$longitude[todo], out$latitude[todo], boundary) == 0
    todo = todo[!inside]
  }
  out
}

# reuse coordinates already published for the same site, so that no site
# ends up with two independent displacements across versions
reuse_published = function(site_id, xy, previous) {
  if (is.null(previous) || nrow(previous) == 0) return(cbind(xy, reused = FALSE))
  i = match(site_id, previous$site_id)
  hit = !is.na(i) & !is.na(previous$longitude[i])
  xy$longitude[hit] = previous$longitude[i[hit]]
  xy$latitude[hit] = previous$latitude[i[hit]]
  cbind(xy, reused = hit)
}

# coordinates from an earlier release (v1 JSON files used block_id)
read_published_coordinates = function(json_dir) {
  files = list.files(json_dir, pattern = "\\.json$", full.names = TRUE)
  out = lapply(files, function(fl) {
    j = try(jsonlite::fromJSON(fl), silent = TRUE)
    if (inherits(j, "try-error")) return(NULL)
    b = j$site_data %||% j$block_data
    if (is.null(b) || !all(c("longitude", "latitude") %in% names(b))) return(NULL)
    data.frame(site_id = as.character(b$site_id %||% b$block_id),
               longitude = as.numeric(b$longitude),
               latitude = as.numeric(b$latitude),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# ---- participants -------------------------------------------------------

# age in years; birth years are converted with the registration year,
# values outside 15 to 100 are set to NA
clean_age = function(age, reg_year) {
  a = suppressWarnings(as.numeric(as.character(age)))
  flag = ifelse(is.na(a), NA_character_, "ok")
  by = !is.na(a) & a >= 1900 & a <= reg_year
  a[by] = reg_year[by] - a[by]
  flag[by] = "birth_year_converted"
  bad = !is.na(a) & (a < 15 | a > 100)
  a[bad] = NA
  flag[bad] = "implausible_removed"
  list(value = round(a), flag = flag)
}

clean_gender = function(g) {
  g = tolower(trimws(as.character(g)))
  out = rep(NA_character_, length(g))
  out[g %in% c("woman", "women", "female", "femme", "mujer", "f")] = "woman"
  out[g %in% c("man", "men", "male", "homme", "hombre", "m")] = "man"
  other = !is_blank(g) & is.na(out)
  out[other] = "other or not stated"
  out
}

# ---- plot area and yield ------------------------------------------------

# plot area in square metres from a value and a unit label or free text
plot_area_m2 = function(value, unit_label = NA) {
  v = as.character(value)
  num = suppressWarnings(as.numeric(gsub(",", ".", gsub("[^0-9.,]", "", v))))
  u = tolower(paste(unit_label, v))
  fac = ifelse(grepl("hect|\\bha\\b", u), 10000,
         ifelse(grepl("acre", u), 4046.86,
         ifelse(grepl("squared met|m2|m²|metre|meter", u), 1, NA)))
  area = num * fac
  area[!is.na(area) & (area < 1 | area > 10000)] = NA
  area
}

# implausibility limit in t/ha for the derived yield, by crop group
max_yield_t_ha = function(crop) {
  crop = tolower(crop)
  ifelse(grepl("potato|cassava|yam|banana|plantain|taro|cocoyam|pumpkin|tomato|cabbage|amaranth|jute|okra|vegetable|pepper|eggplant|forage|grass", crop),
         80, 20)
}
