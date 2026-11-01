# Build the question dictionary from Almendra's harmonised table and the
# reviewed unit sheet. The dictionary maps every ClimMob question code to a
# canonical question with an English description, a unit and a basis.

# every sheet is read as text, so readxl does not guess types and turn
# entries like "not found" into NA, and duplicated column names are kept
read_sheet = function(path, sheet) {
  d = readxl::read_excel(path, sheet = sheet, col_types = "text", .name_repair = "minimal")
  as.data.frame(d, stringsAsFactors = FALSE)
}

build_dictionary = function(questions_xlsx,
                            units_xlsx = NULL,
                            fixes = c("1808" = 2632L)) {
  
  q = read_sheet(questions_xlsx, "questions")
  q$question_id = suppressWarnings(as.integer(q$question_id))
  q = q[!is.na(q$question_id), ]
  q$final_id_num = suppressWarnings(as.integer(q$final_id))
  
  # known errors in final_id, applied before resolving
  # 1808 qst_livestockfeeding pointed to 2631 (leaf to stem ratio)
  for (k in names(fixes)) q$final_id_num[q$question_id == as.integer(k)] = fixes[[k]]
  
  fmap = stats::setNames(q$final_id_num, q$question_id)
  
  resolve_one = function(i) {
    seen = integer(0)
    cur = i
    for (k in 1:25) {
      nxt = unname(fmap[as.character(cur)])
      if (length(nxt) == 0 || is.na(nxt)) return(NA_integer_)
      if (nxt == cur) return(as.integer(cur))
      if (nxt %in% seen) return(NA_integer_)
      seen = c(seen, cur)
      cur = nxt
    }
    NA_integer_
  }
  
  q$canonical_id = vapply(q$question_id, resolve_one, integer(1))
  q$code_lc = tolower(trimws(q$question_code))
  
  issues = q[is.na(q$canonical_id), c("question_id", "question_code", "final_id")]
  
  # one canonical id per code, NA when the code is ambiguous
  by_code = split(q$canonical_id, q$code_lc)
  code_map = data.frame(
    code_lc = names(by_code),
    canonical_id = vapply(by_code, function(v) {
      u = unique(stats::na.omit(v))
      if (length(u) == 1) as.integer(u) else NA_integer_
    }, integer(1)),
    n_candidates = vapply(by_code, function(v) length(unique(stats::na.omit(v))), integer(1)),
    stringsAsFactors = FALSE)
  
  # attributes of the canonical questions
  canon = q[!is.na(q$canonical_id) & q$question_id == q$canonical_id, ]
  canon = data.frame(
    canonical_id = canon$question_id,
    canonical_code = canon$question_code,
    description = trimws(as.character(canon$question_desc)),
    question_name = trimws(as.character(col_or_na(canon, "question_name"))),
    dtype = canon$question_dtype,
    lang = canon$question_lang,
    unit_original = canon$question_unit,
    stringsAsFactors = FALSE)
  
  # descriptions that are only "-" fall back to the question name
  bad_desc = is_blank(canon$description) | canon$description == "-"
  canon$description[bad_desc] = canon$question_name[bad_desc]
  
  # published variable name, unique across the dictionary
  canon$variable = clean_code(canon$canonical_code)
  dup = duplicated(canon$variable) | duplicated(canon$variable, fromLast = TRUE)
  canon$variable[dup] = paste(canon$variable[dup], canon$canonical_id[dup], sep = "_")
  
  canon$unit = NA_character_
  canon$basis = NA_character_
  canon$variable_class = NA_character_
  canon$action = NA_character_
  
  if (!is.null(units_xlsx)) {
    u = read_sheet(units_xlsx, "numeric_units")
    u$question_id = suppressWarnings(as.integer(u$question_id))
    pick = function(reviewed, proposed) {
      ifelse(is_blank(reviewed), as.character(proposed), as.character(reviewed))
    }
    u$unit = pick(col_or_na(u, "reviewer_unit"), u$unit_norm)
    u$basis = pick(col_or_na(u, "reviewer_basis"), u$basis)
    u$action = pick(col_or_na(u, "reviewer_action"), u$proposed_action)
    i = match(canon$canonical_id, u$question_id)
    has = !is.na(i)
    canon$unit[has] = u$unit[i[has]]
    canon$basis[has] = u$basis[i[has]]
    canon$variable_class[has] = u$variable_class[i[has]]
    canon$action[has] = u$action[i[has]]
  }
  
  # crop ontology terms per canonical question and crop
  onto = try(read_sheet(questions_xlsx, "Crop_Ontologies"), silent = TRUE)
  if (inherits(onto, "try-error")) {
    onto = data.frame(canonical_id = integer(0), crop = character(0), ontology_id = character(0))
  } else {
    # the sheet has two columns named Crop_ontology, take the first filled one
    oc = which(grepl("^Crop_ontology", names(onto)))
    valid = function(v) grepl("^CO_[0-9]+:[0-9]{7}$", v) & !grepl(":0000000$", v)
    term = rep(NA_character_, nrow(onto))
    for (k in oc) {
      v = gsub("[[:space:]\u00a0]", "", as.character(onto[[k]]))
      take = !valid(term) & valid(v)
      term[take] = v[take]
    }
    ok = valid(term)
    onto = data.frame(canonical_id = q$canonical_id[match(as.integer(onto$question_id), q$question_id)],
                      crop = tolower(trimws(onto$crop)),
                      ontology_id = term,
                      stringsAsFactors = FALSE)[ok, ]
    onto = onto[!is.na(onto$canonical_id) & !duplicated(onto[, c("canonical_id", "crop")]), ]
  }
  
  list(code_map = code_map, canonical = canon, issues = issues, ontology = onto)
}

# look up canonical attributes for a vector of project question codes
lookup_codes = function(codes, dict) {
  i = match(tolower(codes), dict$code_map$code_lc)
  cid = dict$code_map$canonical_id[i]
  j = match(cid, dict$canonical$canonical_id)
  out = dict$canonical[j, c("canonical_id", "variable", "description", "unit",
                            "basis", "variable_class", "action", "lang")]
  out$canonical_id = cid
  rownames(out) = NULL
  out
}
