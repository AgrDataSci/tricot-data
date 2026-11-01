# Workbooks sent to partners to complete genotype, author and funder
# metadata. One workbook per partner, pre-filled from ClimMob and from the
# genotype sheets already curated for release v1. The returned workbook is
# referenced in config/projects.csv (column partner_file) and read by
# project_config().

ENTRY_TYPES = c(
  "Evaluation entry" = "Material evaluated for any reason, including early-stage material",
  "Release candidate" = "Late-stage or pre-release material",
  "Farmer check" = "Variety most commonly used by farmers in the area",
  "Commercial check" = "Competitive modern variety currently available for sale locally",
  "Historical check" = "Older released variety",
  "Comparative check" = "Variety from other research institutions or companies")

AUTHOR_ROLES = c(
  ProjectLeader = "Person who officially headed the project",
  ProjectManager = "Person who oversaw the details of the project",
  ProjectMember = "Person who contributed to the project",
  WorkPackageLeader = "Person who led a work package of the project",
  Supervisor = "Designated administrator over one or more groups or projects",
  DataCollector = "Person who gathered the data",
  DataCurator = "Person who reviewed, cleaned and standardised the data",
  DataManager = "Person responsible for maintenance and storage of the data",
  ContactPerson = "Person to contact about the data",
  Researcher = "Person involved in analysing the data or the results",
  Other = "Any other contribution, describe it in the remarks")

# genotype sheets curated for release v1 (metadata/trials-metadata/*/genotype-metadata.xlsx)
read_existing_genotypes = function(dir = "metadata/trials-metadata") {
  files = list.files(dir, pattern = "genotype-metadata\\.xlsx$", recursive = TRUE, full.names = TRUE)
  out = lapply(files, function(f) {
    g = read_sheet(f, readxl::excel_sheets(f)[1])
    if (!"genotype_name" %in% names(g)) return(NULL)
    g$crop_name = tolower(trimws(col_or_na(g, "crop_name")))
    g
  })
  rbind_fill(out)
}

# genotype names tested in each trial, one row per trial x genotype
trial_genotypes = function(x) {
  pk = cm_packages(x)
  data.frame(trial_id = project_field(x, "project_id"),
             genotype_name = unlist(pk[, c("item_A", "item_B", "item_C")], use.names = FALSE),
             stringsAsFactors = FALSE)
}

make_partner_requests = function(trials, config, dir = "requests",
                                 existing = read_existing_genotypes(),
                                 deadline = Sys.Date() + 15,
                                 group_by = "institute") {

  config = config[isTRUE_vec(config$include), ]
  if (nrow(config) == 0) stop("No trial has include = TRUE in config/projects.csv")
  ids = vapply(trials, project_field, "", name = "project_id")
  config$partner = ifelse(is_blank(config[[group_by]]), config$pi_climmob, config[[group_by]])

  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  files = character(0)

  for (partner in unique(config$partner)) {
    cf = config[config$partner == partner, ]
    tr = trials[ids %in% cf$project_id]
    if (length(tr) == 0) next

    # trials covered by this request
    trials_tab = data.frame(
      trial_id = cf$project_id, trial_code = cf$project_code, trial_name = cf$project_name,
      country = cf$country, crop_name = cf$crop_name, created = cf$created,
      sites = vapply(cf$project_id, function(p) {
        k = match(p, ids)
        if (is.na(k)) NA_real_ else as.numeric(nrow(trials[[k]]$data))
      }, numeric(1)),
      stringsAsFactors = FALSE)

    # genotypes, one row per crop x name, with the trials where they appear
    g = do.call(rbind, lapply(tr, trial_genotypes))
    g$crop_name = cf$crop_name[match(g$trial_id, cf$project_id)]
    g$trial_code = cf$project_code[match(g$trial_id, cf$project_id)]
    key = paste(g$crop_name, g$genotype_name, sep = "\r")
    geno = data.frame(
      institution = partner,
      crop_name = g$crop_name[!duplicated(key)],
      genotype_name = g$genotype_name[!duplicated(key)],
      trials = vapply(split(g$trial_code, key), function(v) paste(unique(v), collapse = "; "), "")[unique(key)],
      stringsAsFactors = FALSE)
    geno = geno[order(geno$crop_name, geno$genotype_name), ]
    fields = c("final_genotype_name", "entry_type", "release_year", "crossing_year",
               "origin", "target_trait", "remarks")
    for (v in fields) geno[[v]] = NA_character_
    if (!is.null(existing) && nrow(existing) > 0) {
      i = match(paste(geno$crop_name, geno$genotype_name), paste(existing$crop_name, existing$genotype_name))
      j = ifelse(is.na(i), match(geno$genotype_name, existing$genotype_name), i)
      for (v in intersect(fields, names(existing))) geno[[v]] = existing[[v]][j]
    }
    geno$final_genotype_name[is_blank(geno$final_genotype_name)] = geno$genotype_name[is_blank(geno$final_genotype_name)]

    # authors, starting from the PI recorded for each trial
    pis = unique(data.frame(name = ifelse(is_blank(cf$pi), cf$pi_climmob, cf$pi),
                            email = cf$pi_email, stringsAsFactors = FALSE))
    pis = pis[!is_blank(pis$name), , drop = FALSE]
    parts = strsplit(trimws(pis$name), " +")
    authors = data.frame(
      given_name = vapply(parts, function(p) paste(p[-length(p)], collapse = " "), ""),
      family_name = vapply(parts, function(p) p[length(p)], ""),
      orcid = NA, role = "ProjectLeader", affiliation_1 = partner, affiliation_2 = NA,
      email = pis$email, country = NA, city = NA, stringsAsFactors = FALSE)

    funders = data.frame(funder_name = NA, funder_identifier = NA, grant_title = NA,
                         grant_number = NA, note = NA)

    file = file.path(dir, paste0("tricot-data-request-", gsub("[^A-Za-z0-9]+", "-", partner), ".xlsx"))
    write_request_workbook(file, partner, deadline, trials_tab, geno, authors, funders)
    files = c(files, file)
  }
  invisible(files)
}

isTRUE_vec = function(v) vapply(v, function(z) isTRUE(as.logical(z)), logical(1))

write_request_workbook = function(file, partner, deadline, trials_tab, geno, authors, funders) {
  wb = openxlsx::createWorkbook()
  head = openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9E1F2", wrapText = TRUE,
                               valign = "top", fontName = "Arial", fontSize = 10)
  edit = openxlsx::createStyle(fgFill = "#FFF2CC", fontName = "Arial", fontSize = 10)
  body = openxlsx::createStyle(fontName = "Arial", fontSize = 10, valign = "top", wrapText = TRUE)

  readme = data.frame(item = c("What this is", "Your trials", "Genotypes", "Authors", "Funders",
                               "Deadline", "Licence", "Questions"),
    text = c(paste("We are publishing the tricot trial data of", partner,
                   "in an open dataset on Zenodo. This workbook collects what ClimMob does not store."),
             "Sheet 'trials' lists the trials included. Tell us if one should be left out.",
             paste("Sheet 'genotypes' lists every name used in your trials. Correct final_genotype_name",
                   "when the ClimMob name is an alias or misspelt, and fill the yellow columns. Names that",
                   "refer to the same genotype must get the same final_genotype_name."),
             "Sheet 'authors' lists who should be credited, with ORCID where available. Add rows as needed.",
             "Sheet 'funders' lists the grants that paid for the trials, with the funder ROR where available.",
             format(deadline, "%d %B %Y"),
             "The dataset is released under CC0 1.0. Citation of the dataset is requested.",
             "Reply to the person who sent you this workbook."),
    stringsAsFactors = FALSE)

  add = function(name, df, editable = character(0), widths = 18) {
    openxlsx::addWorksheet(wb, name)
    openxlsx::writeData(wb, name, df, headerStyle = head)
    openxlsx::addStyle(wb, name, body, rows = 2:(nrow(df) + 1), cols = seq_along(df),
                       gridExpand = TRUE)
    cols = which(names(df) %in% editable)
    if (length(cols) > 0) {
      openxlsx::addStyle(wb, name, edit, rows = 2:(nrow(df) + 200), cols = cols,
                         gridExpand = TRUE, stack = TRUE)
    }
    openxlsx::setColWidths(wb, name, cols = seq_along(df), widths = widths)
    openxlsx::freezePane(wb, name, firstRow = TRUE)
  }

  add("README", readme, widths = c(16, 110))
  add("trials", trials_tab, widths = c(16, 12, 40, 8, 14, 12, 8))
  gedit = c("final_genotype_name", "entry_type", "release_year", "crossing_year",
            "origin", "target_trait", "remarks")
  add("genotypes", geno, editable = gedit, widths = c(18, 14, 26, 22, 26, 18, 12, 12, 14, 18, 30))
  add("authors", authors, editable = names(authors), widths = c(16, 16, 22, 20, 28, 20, 30, 12, 12))
  add("funders", funders, editable = names(funders), widths = c(26, 30, 30, 18, 40))

  defs = rbind(data.frame(list = "entry_type", option = names(ENTRY_TYPES), description = unname(ENTRY_TYPES)),
               data.frame(list = "role", option = names(AUTHOR_ROLES), description = unname(AUTHOR_ROLES)))
  add("definitions", defs, widths = c(12, 20, 70))

  # drop-down lists that point to the definitions sheet
  ne = length(ENTRY_TYPES)
  openxlsx::dataValidation(wb, "genotypes", cols = which(names(geno) == "entry_type"),
                           rows = 2:(nrow(geno) + 200), type = "list",
                           value = sprintf("'definitions'!$B$2:$B$%d", ne + 1))
  openxlsx::dataValidation(wb, "authors", cols = which(names(authors) == "role"),
                           rows = 2:200, type = "list",
                           value = sprintf("'definitions'!$B$%d:$B$%d", ne + 2, ne + 1 + length(AUTHOR_ROLES)))
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}

# read a returned workbook into the pieces project_config() expects
read_partner_file = function(file, crop = NULL) {
  out = list()
  sh = readxl::excel_sheets(file)
  if ("genotypes" %in% sh) {
    g = read_sheet(file, "genotypes")
    g = g[!is_blank(g$genotype_name), ]
    if (!is.null(crop) && "crop_name" %in% names(g)) g = g[tolower(g$crop_name) %in% tolower(crop), ]
    out$genotypes = g
  }
  if ("authors" %in% sh) {
    a = read_sheet(file, "authors")
    out$authors = a[!is_blank(a$family_name), , drop = FALSE]
  }
  if ("funders" %in% sh) {
    fu = read_sheet(file, "funders")
    fu = fu[!is_blank(fu$funder_name), , drop = FALSE]
    if (nrow(fu) > 0) {
      out$funders = data.frame(funderName = fu$funder_name, funderIdentifier = fu$funder_identifier,
                               awardTitle = fu$grant_title, awardNumber = fu$grant_number,
                               stringsAsFactors = FALSE)
    }
  }
  out
}
