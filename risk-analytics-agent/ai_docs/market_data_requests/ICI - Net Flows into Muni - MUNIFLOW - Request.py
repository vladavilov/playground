import pandas as pd
import requests

# Replace with the actual ICI URL (updated weekly/monthly)
url = 'https://www.ici.org/system/files/2023-10/weeklyflows_10_11_23.xls'

# Download the Excel file
response = request.get(url)
with open('weeklyflows.xls', 'wb') as f:
    f.write(response.content)

# Parse Excel file (assuming municipal bond fund flow data is on a known sheet)
df = pd.read_excel('weeklyflows.xls', sheet_name='Municipal Bonds', skiprows=5)

# Display data sample
print(df.head())