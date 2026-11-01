# Plackett-Luce log-worth per collection moment x trait. Labels are attached
# inside each model, so traits with different sets of genotypes cannot shift.

rank_analysis = function(plot_data, genotypes, min_sites = 10) {

  r = plot_data[plot_data$value_type == "rank", ]
  empty = data.frame(collection_moment = character(0), trait = character(0),
                     genotype_name = character(0), log_worth = numeric(0),
                     se = numeric(0), quasi_se = numeric(0), quasi_var = numeric(0),
                     reference = character(0), n_sites = integer(0))
  if (nrow(r) == 0) return(empty)

  checks = genotypes$genotype_name[genotypes$role %in% "check"]
  keys = unique(r[, c("collection_moment", "trait")])
  out = list()

  for (k in seq_len(nrow(keys))) {
    dk = r[r$collection_moment == keys$collection_moment[k] & r$trait == keys$trait[k], ]
    n_sites = length(unique(dk$site_id))
    if (n_sites < min_sites) next
    dk$value = as.integer(dk$value)

    res = tryCatch({
      R = suppressWarnings(gosset::rank_tricot2(dk, id = "site_id",
                                                items = "genotype_name", input = "value"))
      m = PlackettLuce::PlackettLuce(R)
      items = colnames(unclass(R))
      freq = table(dk$genotype_name)
      ref = intersect(checks, items)[1]
      if (is.na(ref)) ref = names(freq)[which.max(freq)]
      qv = qvcalc::qvcalc(m, ref = match(ref, items))$qvframe
      data.frame(collection_moment = keys$collection_moment[k],
                 trait = keys$trait[k],
                 genotype_name = rownames(qv),
                 log_worth = round(qv$estimate, 4),
                 se = round(qv$SE, 4),
                 quasi_se = round(qv$quasiSE, 4),
                 quasi_var = round(qv$quasiVar, 6),
                 reference = ref,
                 n_sites = n_sites,
                 stringsAsFactors = FALSE)
    }, error = function(e) {
      message("  rank model failed for ", keys$trait[k], " (", keys$collection_moment[k],
              "): ", conditionMessage(e))
      NULL
    })
    out[[length(out) + 1]] = res
  }

  out = do.call(rbind, out)
  if (is.null(out)) return(empty)
  rownames(out) = NULL
  out
}
