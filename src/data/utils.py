import pandas as pd
import numpy as np
import datetime
import yaml
import re

from sqlalchemy import create_engine

def load_credentials(group):
    with open("../../credentials.yaml") as file:
        credentials = yaml.safe_load(file)[group]
    return credentials

def connect_to_database(credentials):
    user = credentials["user"]
    password = credentials["password"]
    host = credentials["host"]
    name = credentials["database"]

    engine = create_engine(f"mysql+mysqlconnector://{user}:{password}@{host}/{name}")
    connection = engine.connect()
    return connection

def load_config():
    with open("../../config.yaml") as file:
        config = yaml.safe_load(file)
    return config

def parse_dates(df):
    date_cols = [col for col in df.columns if "date" in col]
    for col in date_cols:
        df[col] = pd.to_datetime(df[col]).dt.normalize()
    return df

def get_matching_cols(df, prefix, excluded_string=None):
    if excluded_string is None:
        return [col for col in df.columns if col.startswith(prefix)]
    else:
        return [col for col in df.columns if (col.startswith(prefix) & (excluded_string not in col))]

def pretty_join(x, prefix=None, sort=False, append_other=False):
    if prefix is not None:
        prefix_regex = f"^{prefix}_"
        x = [re.sub(prefix_regex, "", x) for x in x]
    if sort:
        x.sort()
    if append_other:
        other_regex = "Other$|other$"
        x = [s for s in x if not re.search(other_regex, s)] + [s for s in x if re.search(other_regex, s)]
    
    return f'{", ".join(x[:-2])}{", " if len(x[:-2]) > 0 else ""}{" & ".join(x[-2:])}'

def flatten_columns(df, sep='_'):
    def _remove_empty(column_name):
        return tuple(element for element in column_name if element)
    def _join(column_name):
        return sep.join(column_name)

    new_columns = [_join(_remove_empty(column)) for column in df.columns.values]
    df.columns = new_columns
    return df