import pandas as pd
import os
import datetime as dt

file_names = os.listdir("../Data/Raw")
file_names.sort()

dfs = []
start_szn = 2007
for i in range(len(file_names)):
    if file_names[i][0] == ".":
        continue
    sheet = pd.ExcelFile("../Data/Raw/" + file_names[i]).parse("Sheet1")

    sheet.loc[:, "Date"] = sheet.loc[:, "Date"].astype(int)
    sheet.loc[sheet.Date > 1000, "Date"] = sheet.loc[sheet.Date > 1000, "Date"].astype(str) + str(start_szn)
    sheet.loc[:, "Date"] = sheet.loc[:, "Date"].astype(int)
    sheet.loc[sheet.Date == 229, "Date"] = 228
    sheet.loc[:, "Date"] = sheet.loc[:, "Date"].astype(int)
    sheet.loc[sheet.Date < 1000, "Date"] = "0" + sheet.loc[sheet.Date < 1000, "Date"].astype(str) + str(start_szn + 1)

    sheet.loc[:, "Date"] = sheet.loc[:, "Date"].astype(str)
    sheet['Date'] = sheet['Date'].apply(lambda x: dt.datetime.strptime(x, "%m%d%Y"))

    sheet.rename(columns={"Rot": "Season"}, inplace=True)
    sheet.Season = start_szn
    start_szn += 1
    dfs.append(sheet)

combined = pd.concat(dfs)
combined.to_csv("../Data/combined.csv")

