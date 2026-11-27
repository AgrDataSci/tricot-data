# /// script
# requires-python = ">=3.12"
# dependencies = ["duckdb>=1.4"]
# ///
"""Build plots, sites and variables parquet in build/parquet from the validated trial JSON.

    uv run script/build-parquet.py

Columns and types come from metadata/tricot-trial.schema.json, not from sampling the data.
Exits non-zero if any trial's row counts differ from datasets.csv."""

import json
import os
from pathlib import Path

import duckdb

# Paths below are relative to the repo root
os.chdir(Path(__file__).resolve().parent.parent)
OUT = "build/parquet"

# Records of the release schema, read from the schema file so new columns follow it
SCHEMA = json.loads(Path("metadata/tricot-trial.schema.json").read_text())["$defs"]


def kind(prop: dict) -> str:
    """DuckDB type of a schema property: integer and number as the schema says, text for the rest."""
    types = prop.get("type", [])
    types = ({types} if isinstance(types, str) else set(types)) - {"null"}
    return "INTEGER" if types == {"integer"} else "DOUBLE" if types == {"number"} else "VARCHAR"


def types(record: str) -> dict[str, str]:
    """DuckDB type of each column of a schema record."""
    return {name: kind(prop) for name, prop in SCHEMA[record]["properties"].items()}


def struct(columns: dict[str, str]) -> str:
    """DuckDB type of a list of records with these columns."""
    return "STRUCT(" + ", ".join(f'"{name}" {type}' for name, type in columns.items()) + ")[]"


PLOT = struct(types("plot"))
VARIABLE = struct(types("variable"))

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
# Fixed columns take their type from the schema. A survey column is numeric only if every
# trial says so; the rest is text, as in the JSON.
site_variables = con.execute(f"""
    SELECT variable_name, bool_and(value_type = 'numeric') AS is_numeric
    FROM read_parquet('{OUT}/variables.parquet', file_row_number = true)
    WHERE level = 'site' AND variable_name != 'trial_id'
    GROUP BY variable_name
    ORDER BY min(file_row_number)""").fetchall()
fixed = types("site")
sites = {name: fixed.get(name, "DOUBLE" if is_numeric else "VARCHAR") for name, is_numeric in site_variables}
write("sites", "site_data", site_data=struct(sites))

# Safeguard: per-trial row counts must match the published index
bad = con.execute(f"""
    SELECT trial_id
    -- strict_mode=false: tolerate mixed CRLF/LF line endings
    FROM read_csv('datasets.csv', strict_mode=false)
    LEFT JOIN (SELECT trial_id, count(*) AS built_plot_rows FROM '{OUT}/plots.parquet' GROUP BY trial_id) USING (trial_id)
    LEFT JOIN (SELECT trial_id, count(*) AS built_sites FROM '{OUT}/sites.parquet' GROUP BY trial_id) USING (trial_id)
    WHERE built_plot_rows IS DISTINCT FROM n_plot_rows OR built_sites IS DISTINCT FROM n_sites""").fetchall()
if bad:
    raise SystemExit(f"{len(bad)} trials differ from datasets.csv: {bad[:5]}")
for table in ("plots", "sites", "variables"):
    rows, cols = con.execute(f"""
        SELECT (SELECT count(*) FROM '{OUT}/{table}.parquet'),
               (SELECT count(*) FROM (DESCRIBE SELECT * FROM '{OUT}/{table}.parquet'))""").fetchone()
    size = Path(f"{OUT}/{table}.parquet").stat().st_size / 1e6
    print(f"{table}: {rows} rows, {cols} columns, {size:.1f} MB")
print("row counts match datasets.csv")
