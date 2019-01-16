# This code has already been run to generate the clean and tidy csv file: "tidy.csv"
# which can be found in the "Data" folder, so you don't need to run it again. If you
# see something that needs to be cleaned further, you can add it to this one or run
# a separate script (this one takes a bit of time).

import pandas as pd
import numpy as np
import seaborn as sns

df = pd.read_csv("../Data/combined.csv")
df = df.reset_index(drop=True).drop(columns=["Unnamed: 0"])

# Make spelling consistent; replace NewJersey --> Brooklyn
df = df.replace(to_replace="NewJersey", value="Brooklyn")
df = df.replace(to_replace="Oklahoma City", value="OklahomaCity")
df = df.replace(to_replace="LA Clippers", value="LAClippers")

# fix detroit v. phoenix game scores; a few bad entry fixes
df.loc[df.index[2916:2918], ["1st", "2nd"]] = [[23, 23], [31, 30]]

df.loc[df.index[1974], "Open"] = 197.5
df.loc[df.index[11192], "2H"] = np.nan
df.loc[df.index[23520], 'Open'] = 195.5

# replace all pk odds (i.e. 50/50 outcomes) with 0 so we can make last four columns integers
df = df.replace(to_replace=["pk", "PK"], value=0)
df = df.replace(to_replace="NL", value=np.nan)

# make last few columns numeric
df[["Open", "Close", "ML", "2H"]] = df[["Open", "Close", "ML", "2H"]].astype(float)

# MAKE TIDY DATA FRAME
# columns of new data frame that has one game per row
tidy = pd.DataFrame(columns=["Date", "SZN", "V", "H", "V1", "V2", "V3", "V4", "H1", "H2", "H3", "H4", "VF", "HF",
                             "OUOpen", "OUClose", "VSpreadOpen", "HSpreadOpen", "VSpreadClose", "HSpreadClose",
                             "VMoney", "HMoney", "OU2H", "VSpread2H", "HSpread2H"])

for i in range(int(len(df)/2)-1):
    # only fills in until HF
    row = [df['Date'][i*2], df['Season'][i*2], df['Team'][i*2], df['Team'][i*2+1], df['1st'][i*2], df['2nd'][i*2],
           df['3rd'][i*2], df['4th'][i*2], df['1st'][i*2+1], df['2nd'][i*2+1], df['3rd'][i*2+1], df['4th'][i*2+1],
           df['Final'][i*2], df['Final'][i*2+1]]

    # Adds OUOpen
    if df["Open"][i*2] > df["Open"][i*2+1]:
        row.append(df["Open"][i*2])
    else:
        row.append(df["Open"][i*2+1])

    # Adds OUClose
    if df["Close"][i*2] > df["Close"][i*2+1]:
        row.append(df["Close"][i*2])
    else:
        row.append(df["Close"][i*2+1])

    # Adds VSpreadOpen and HSpreadOpen
    if df["Open"][i*2] > df["Open"][i*2+1]:
        if df["ML"][i*2] < 0:
            row.append(df["Open"][i*2+1]*(-1))
            row.append(df["Open"][i*2+1])
        else:
            row.append(df["Open"][i*2+1])
            row.append(df["Open"][i*2+1]*(-1))
    else:
        if df["ML"][i*2] < 0:
            row.append(df["Open"][i*2]*(-1))
            row.append(df["Open"][i*2])
        else:
            row.append(df["Open"][i*2])
            row.append(df["Open"][i*2]*(-1))

    # Adds VSpreadClose and HSpreadClose
    if df["Close"][i*2] > df["Close"][i*2+1]:
        if df["ML"][i*2] < 0:
            row.append(df["Close"][i*2+1]*(-1))
            row.append(df["Close"][i*2+1])
        else:
            row.append(df["Close"][i*2+1])
            row.append(df["Close"][i*2+1]*(-1))
    else:
        if df["ML"][i*2] < 0:
            row.append(df["Close"][i*2]*(-1))
            row.append(df["Close"][i*2])
        else:
            row.append(df["Close"][i*2])
            row.append(df["Close"][i*2]*(-1))

    # VMoney and HMoney
    row.append(df["ML"][i*2])
    row.append(df["ML"][i*2+1])

    # OU2H
    if df["2H"][i*2] > df["2H"][i*2+1]:
        row.append(df["2H"][i*2])
        row.append(df["2H"][i*2+1])
        row.append(df["2H"][i*2+1]*(-1))
    else:
        row.append(df["2H"][i*2+1])
        row.append(df["2H"][i*2]*(-1))
        row.append(df["2H"][i*2])

    tidy.loc[i] = row

intermediate = tidy.copy()
intermediate.to_csv("../Data/intermediate.csv", index_label="Index")

# Removes/fixes a few outliers
tidy.iloc[:, 4:12] = tidy.iloc[:, 4:12][tidy.iloc[:, 4:12] < 70]
tidy.iloc[:, 4:12] = tidy.iloc[:, 4:12][tidy.iloc[:, 4:12] > 0]
tidy.OUOpen = tidy.OUOpen[tidy.OUOpen > 100]
tidy = tidy.reset_index(drop=True)
tidy.iloc[:, 4:] = tidy.iloc[:, 4:].astype(float)
tidy.loc[tidy.OUOpen == tidy.OUOpen.min(), "OUOpen"] = tidy.loc[tidy.OUOpen == tidy.OUOpen.min(), "OUClose"]
tidy.loc[df.index[5598], 'OU2H'] = round(tidy[tidy.OUClose.between(197, 198)].OU2H.mean()*2)/2
tidy.dropna(axis=0, how='any', inplace=True)

tidy.loc[df.index[11958], 'VF'] = tidy.loc[df.index[11958], ['V1', 'V2', 'V3', 'V4']].sum()
tidy.loc[df.index[11958], 'HF'] = tidy.loc[df.index[11958], ['H1', 'H2', 'H3', 'H4']].sum()

tidy.to_csv("../Data/tidy.csv", index_label="Index")

'''
# Quick graphs to check for outliers:
tidy.ix[:, 4:12].plot(kind='box')  # Quarter Scores
tidy.ix[:, 12:14].plot(kind='box')  # Final Scores
tidy.ix[:, 14:16].plot(kind='box')  # OUs
tidy.ix[:, 16:20].plot(kind='box')  # Spreads
tidy.ix[:, 20:22].plot(kind='box')  # Money lines
tidy.ix[:, 22].plot(kind='box')  # OU second half
tidy.ix[:, 23:].plot(kind='box')  # Spreads second half
'''

