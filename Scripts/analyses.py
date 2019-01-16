# Basically nothing has been done on this page so feel free to test some relationships here, or on a separate
# script - whatever you guys prefer.

import pandas as pd
import numpy as np
import statsmodels.formula.api as sm
import seaborn as sns

df = pd.read_csv("../Data/tidy.csv")
dfpos = df[df.VMoney > 0]
dfpos["HProb"] = pd.Series((dfpos.ix[:, 'HMoney'] / (dfpos.ix[:, 'HMoney'] - 100))*100)

dfpos["BetSpread"] = pd.Series()
dfpos.loc[:, "BetSpread"][(dfpos.ix[:, 'HF'] - dfpos.ix[:, 'VF']) > dfpos.ix[:, 'HSpreadClose']] = "win"
dfpos.loc[:, "BetSpread"][(dfpos.ix[:, 'HF'] - dfpos.ix[:, 'VF']) == dfpos.ix[:, 'HSpreadClose']] = "tie"
dfpos.loc[:, "BetSpread"][(dfpos.ix[:, 'HF'] - dfpos.ix[:, 'VF']) < dfpos.ix[:, 'HSpreadClose']] = "lose"


result = sm.ols(formula="HSpreadClose ~ HProb", data=dfpos).fit()

sns.regplot(x="HProb", y="HSpreadClose", data=dfpos)
sns.regplot(x="HSpreadClose", y="HProb", data=dfpos)
sns.lmplot(data=dfpos, x="HSpreadClose", y="HProb", hue="BetSpread")

unequal = df[(df.V1 + df.V2 + df.V3 + df.V4) != df.VF]
unequal = unequal.loc[:, ["Date", "V", "H", "V1", "V2", "V3", "V4", "VF"]]

