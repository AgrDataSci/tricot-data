# .............................................
# Write one request workbook per partner for genotype, author and funder
# metadata. Run after config/projects.csv has include = TRUE and the
# institute filled for each trial. Returned workbooks go to
# metadata/partners/ and their path into the partner_file column.
# .............................................
for (f in list.files("tricot-export/R", pattern = "\\.R$", full.names = TRUE)) source(f)

env = new.env()
load("raw/trial-data.rda", envir = env)
trials = env$cmdata

config = utils::read.csv("config/projects.csv", stringsAsFactors = FALSE, na.strings = c("", "NA"))

files = make_partner_requests(trials, config, dir = "requests",
                              deadline = Sys.Date() + 15, group_by = "institute")
print(files)
