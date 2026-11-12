# /// script
# requires-python = ">=3.12"
# dependencies = ["duckdb>=1.4"]
# ///
"""Build plots, sites and variables parquet from the validated trial JSON.

    uv run script/build-parquet.py [OUT_DIR] [REPO]

Types come from the release schema, not from sampling the data. Exits non-zero if any
trial's row counts differ from datasets.csv."""

import sys
from pathlib import Path

import duckdb

OUT = Path(sys.argv[1]) if len(sys.argv) > 1 else Path("build/parquet")
REPO = Path(sys.argv[2]) if len(sys.argv) > 2 else Path(__file__).resolve().parent.parent
COPY = "(FORMAT parquet, COMPRESSION zstd, ROW_GROUP_SIZE 100000)"

# Columns of tricot-trial.schema.json; keys not listed are ignored
PLOT = """STRUCT(site_id VARCHAR, plot VARCHAR, genotype_name VARCHAR, collection_moment VARCHAR,
  moment_days INTEGER, trait VARCHAR, trait_id INTEGER, trait_original VARCHAR, value VARCHAR,
  value_type VARCHAR, unit VARCHAR, flag VARCHAR, rep INTEGER, block INTEGER, "row" INTEGER,
  col INTEGER, entry_type VARCHAR)[]"""
VARIABLE = """STRUCT(level VARCHAR, variable_name VARCHAR, description VARCHAR,
  description_language VARCHAR, description_original VARCHAR, original_language VARCHAR,
  harmonised_id INTEGER, ontology_id VARCHAR, value_type VARCHAR, unit VARCHAR, basis VARCHAR,
  controlled_vocabulary VARCHAR)[]"""


def trials(**columns: str) -> str:
    # Big arrays a query does not use stay raw JSON: leaving them undeclared runs out of memory
    cols = ", ".join(f"'{k}': '{v}'" for k, v in {"plot_data": "JSON", "site_data": "JSON", **columns}.items())
    return (f"read_json('{REPO}/data/**/*.json', maximum_object_size=67108864, "
            f"columns={{'metadata': 'STRUCT(study STRUCT(id VARCHAR), variables {VARIABLE})', {cols}}})")


con = duckdb.connect()
con.execute("SET memory_limit='4GB'; SET threads=4; SET preserve_insertion_order=false")  # peaks under 2 GB
OUT.mkdir(parents=True, exist_ok=True)

con.execute(f"""COPY (SELECT metadata.study.id AS trial_id, unnest(plot_data, recursive := true)
                     FROM {trials(plot_data=PLOT)}) TO '{OUT}/plots.parquet' {COPY}""")
con.execute(f"""COPY (SELECT metadata.study.id AS trial_id, unnest(metadata.variables, recursive := true)
                     FROM {trials()}) TO '{OUT}/variables.parquet' {COPY}""")

# Site columns differ by trial: declare the union of every trial's site variables.
# A column is numeric only if every trial says so; the rest is text, as in the JSON.
fields = con.execute(f"""SELECT string_agg(format('"{{}}" {{}}', variable_name, type), ', ' ORDER BY first)
    FROM (SELECT variable_name, min(file_row_number) AS first,
                 if(bool_and(value_type = 'numeric'), 'DOUBLE', 'VARCHAR') AS type
          FROM read_parquet('{OUT}/variables.parquet', file_row_number := true)
          WHERE level = 'site' AND variable_name != 'trial_id' GROUP BY variable_name)""").fetchone()[0]
con.execute(f"""CREATE TABLE sites AS SELECT metadata.study.id AS trial_id, unnest(site_data, recursive := true)
                FROM {trials(site_data=f'STRUCT({fields})[]')}""")

# 'numeric' does not say whole or decimal: store whole-number-only columns as integers
doubles = [n for n, t, *_ in con.execute("DESCRIBE sites").fetchall() if t == "DOUBLE"]
whole = con.execute("SELECT " + ", ".join(f'bool_and("{n}" = round("{n}"))' for n in doubles) + " FROM sites").fetchone()
ints = ", ".join(f'"{n}"::BIGINT AS "{n}"' for n, w in zip(doubles, whole) if w)
con.execute(f"COPY (SELECT * {f'REPLACE ({ints})' if ints else ''} FROM sites) TO '{OUT}/sites.parquet' {COPY}")

# Safeguard: per-trial row counts must match the published index
bad = con.execute(f"""
    SELECT i.trial_id FROM read_csv('{REPO}/datasets.csv') i
    LEFT JOIN (SELECT trial_id, count(*) n FROM '{OUT}/plots.parquet' GROUP BY ALL) p USING (trial_id)
    LEFT JOIN (SELECT trial_id, count(*) n FROM '{OUT}/sites.parquet' GROUP BY ALL) s USING (trial_id)
    WHERE p.n IS DISTINCT FROM i.n_plot_rows OR s.n IS DISTINCT FROM i.n_sites""").fetchall()
print(f"{len(bad)} trials differ from datasets.csv: {bad[:5]}" if bad else "row counts match datasets.csv")
sys.exit(1 if bad else 0)
