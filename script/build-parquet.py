# /// script
# requires-python = ">=3.12"
# dependencies = ["duckdb>=1.4"]
# ///
"""Build plots, sites and variables parquet in build/parquet from the validated trial JSON.

    uv run script/build-parquet.py

Types come from the release schema, not from sampling the data. Exits non-zero if any
trial's row counts differ from datasets.csv."""

import os
from pathlib import Path

import duckdb

# Paths below are relative to the repo root
os.chdir(Path(__file__).resolve().parent.parent)
OUT = "build/parquet"

# Columns of tricot-trial.schema.json; keys not listed are ignored
PLOT = """STRUCT(site_id VARCHAR, plot VARCHAR, genotype_name VARCHAR, collection_moment VARCHAR,
  moment_days INTEGER, trait VARCHAR, trait_id INTEGER, trait_original VARCHAR, value VARCHAR,
  value_type VARCHAR, unit VARCHAR, flag VARCHAR, rep INTEGER, block INTEGER, "row" INTEGER,
  col INTEGER, entry_type VARCHAR)[]"""
VARIABLE = """STRUCT(level VARCHAR, variable_name VARCHAR, description VARCHAR,
  description_language VARCHAR, description_original VARCHAR, original_language VARCHAR,
  harmonised_id INTEGER, ontology_id VARCHAR, value_type VARCHAR, unit VARCHAR, basis VARCHAR,
  controlled_vocabulary VARCHAR)[]"""


con = duckdb.connect()
Path(OUT).mkdir(parents=True, exist_ok=True)


def write(table: str, records: str, **columns: str) -> None:
    """Write one row per record of a JSON array, with the trial id as the first column.
    `columns` types the top-level keys the query reads besides metadata."""
    extra = "".join(f", '{key}': '{type}'" for key, type in columns.items())
    # unnest(list, recursive := true) gives one row per record and one column per field
    con.execute(f"""
        COPY (
            SELECT metadata.study.id AS trial_id, unnest({records}, recursive := true)
            FROM read_json(
                'data/**/*.json',
                maximum_object_size = 67108864,  -- 64 MB; the default 16 MB is close to the largest file
                columns = {{'metadata': 'STRUCT(study STRUCT(id VARCHAR), variables {VARIABLE})'{extra}}}
            )
        ) TO '{OUT}/{table}.parquet' (FORMAT parquet, COMPRESSION zstd)""")


write("plots", "plot_data", plot_data=PLOT)
write("variables", "metadata.variables")

# Site columns differ by trial: take every site variable any trial defines, in order of first use.
# A column is numeric only if every trial says so; the rest is text, as in the JSON.
site_variables = con.execute(f"""
    SELECT variable_name, bool_and(value_type = 'numeric') AS is_numeric
    FROM read_parquet('{OUT}/variables.parquet', file_row_number = true)
    WHERE level = 'site' AND variable_name != 'trial_id'
    GROUP BY variable_name
    ORDER BY min(file_row_number)""").fetchall()
fields = ", ".join(
    f'"{name}" {"DOUBLE" if is_numeric else "VARCHAR"}'
    for name, is_numeric in site_variables
)
write("sites", "site_data", site_data=f"STRUCT({fields})[]")

# Safeguard: per-trial row counts must match the published index
bad = con.execute(f"""
    SELECT trial_id
    FROM read_csv('datasets.csv')
    LEFT JOIN (SELECT trial_id, count(*) AS built_plot_rows FROM '{OUT}/plots.parquet' GROUP BY trial_id) USING (trial_id)
    LEFT JOIN (SELECT trial_id, count(*) AS built_sites FROM '{OUT}/sites.parquet' GROUP BY trial_id) USING (trial_id)
    WHERE built_plot_rows IS DISTINCT FROM n_plot_rows OR built_sites IS DISTINCT FROM n_sites""").fetchall()
if bad:
    raise SystemExit(f"{len(bad)} trials differ from datasets.csv: {bad[:5]}")
print("row counts match datasets.csv")
