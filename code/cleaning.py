import pandas as pd
from datetime import datetime, timedelta
import os
import re
from tqdm import tqdm

def clean_data(countrydir, filename, startdate, enddate):
    pathname = f"/Users/ziyuhe/Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond/{countrydir}/{filename}"
    df_raw = pd.read_csv(pathname)

    if df_raw.shape[1] > 2:
        return

    df_raw['Date'] = pd.to_datetime(df_raw['Date'], format='%d-%b-%Y')
    df_raw.iloc[:, 1] = pd.to_numeric(df_raw.iloc[:, 1], errors='coerce')

    startdate = pd.to_datetime(startdate, format = "%Y%m%d")
    enddate = pd.to_datetime(enddate, format = "%Y%m%d")

    alldates = pd.date_range(min(df_raw['Date']), max(df_raw['Date']), freq='D')
    df_clean = pd.DataFrame({'Date': alldates})
    df_clean = pd.merge(df_clean, df_raw, on='Date', how='left')
    df_clean.iloc[:, 1] = df_clean.iloc[:, 1].fillna(method='ffill')

    df_clean['start'] = (df_clean['Date'] - startdate).dt.days
    df_clean['end'] = (enddate - df_clean['Date']).dt.days

    df_clean.to_csv(pathname, index=False)

# Example usage:
#clean_data("your_country_dir", "your_filename.csv", "start_date", "end_date")


def collect_csv_file_info(folder_path):
    csv_file_info = []

    for root, dirs, files in os.walk(folder_path):
        for file in files:
            if file.endswith(".csv"):
                folder_name = os.path.basename(root)
                date_pattern = re.compile(r'_(\d{8})_(\d{8})_')
                # Use the findall method to extract matched date pairs
                date_matches = date_pattern.findall(file)

                # Check if any date pairs are found
                if date_matches:
                    start_date, end_date = date_matches[0]
                    csv_file_info.append((folder_name, file, start_date, end_date))

    return csv_file_info

folder_path = "/Users/ziyuhe/Library/CloudStorage/OneDrive-UCSanDiego/Data/Financial Data/Green Bond"
csv_file_names = collect_csv_file_info(folder_path)

for file in tqdm(csv_file_names):
    clean_data(file[0], file[1], file[2], file[3])

