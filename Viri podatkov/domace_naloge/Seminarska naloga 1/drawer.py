# -*- coding: utf-8 -*-
"""
Created on Tue May  4 08:27:37 2021

@author: LaZyM
"""

import seaborn as sns
import pandas as pd
import numpy as np
import random as rnd

age_bucket_lwr = [0,
5,
10,
15,
20,
25,
30,
35,
40,
45,
50,
55,
60,
65,
70,
75,
80
]

age_bucket_upr = [0+5,
5+5,
10+5,
15+5,
20+5,
25+5,
30+5,
35+5,
40+5,
45+5,
50+5,
55+5,
60+5,
65+5,
70+5,
75+5,
80+5
]

inc_rate_wmn_1967 = [0,
0,
0,
0,
1,
2,
3,
5,
12,
5,
22,
30,
31,
54,
92,
74,
70]

inc_rate_men_1967 = [0,
0,
1,
3,
1,
3,
5,
5,
7,
14,
36,
43,
78,
107,
140,
124,
114]

inc_rate_men_1977 = [0,
0,
0,
0,
3,
4,
1,
7,
10,
12,
34,
30,
91,
165,
155,
186,
115]

inc_rate_wmn_1977 = [0,
0,
0,
0,
1,
0,
0,
7,
18,
18,
26,
38,
74,
87,
131,
130,
129]

inc_rate_men_1987 = [0,
0,
0,
0,
0,
0,
7,
5,
21,
18,
54,
76,
97,
181,
263,
336,
283]

inc_rate_wmn_1987 = [0,
0,
0,
0,
1,
3,
2,
7,
7,
25,
38,
58,
78,
112,
125,
176,
160
]

inc_rate_men_1997 = [0,
0,
0,
1,
0,
0,
0,
6,
13,
36,
53,
109,
191,
245,
353,
398,
394]

inc_rate_wmn_1997 = [0,
0,
2,
0,
0,
1,
4,
9,
19,
19,
41,
83,
94,
127,
183,
219,
279]


inc_rate_men_2007 = [0,
0,
0,
0,
0,
3,
3,
17,
11,
33,
62,
107,
202,
294,
444,
500,
547]

inc_rate_wmn_2007 = [0,
0,
0,
0,
0,
1,
4,
3,
8,
15,
43,
79,
127,
146,
249,
199,
302]


inc_rate_men_2017 = [0,
0,
0,
0,
0,
3,
0,
6,
21,
21,
60,
91,
148,
209,
305,
337,
512]

inc_rate_wmn_2017 = [0,
0,
0,
2,
0,
0,
6,
10,
14,
30,
50,
38,
75,
89,
160,
180,
214]



data_1967_wmn = []
data_1977_wmn = []
data_1987_wmn = []
data_1997_wmn = []
data_2007_wmn = []
data_2017_wmn = []

data_1967_men = []
data_1977_men = []
data_1987_men = []
data_1997_men = []
data_2007_men = []
data_2017_men = []

np.random.seed(8)

for i in range(len(age_bucket_upr)):
    data_1967_wmn.append(np.random.randint(age_bucket_lwr[i], age_bucket_upr[i], inc_rate_wmn_1967[i]))
    data_1977_wmn.append(np.random.randint(age_bucket_lwr[i], age_bucket_upr[i], inc_rate_wmn_1977[i]))
    data_1987_wmn.append(np.random.randint(age_bucket_lwr[i], age_bucket_upr[i], inc_rate_wmn_1987[i]))
    data_1997_wmn.append(np.random.randint(age_bucket_lwr[i], age_bucket_upr[i], inc_rate_wmn_1997[i]))
    data_2007_wmn.append(np.random.randint(age_bucket_lwr[i], age_bucket_upr[i], inc_rate_wmn_2007[i]))
    data_2017_wmn.append(np.random.randint(age_bucket_lwr[i], age_bucket_upr[i], inc_rate_wmn_2017[i]))
    
    data_1967_men.append(np.random.randint(age_bucket_lwr[i], age_bucket_upr[i], inc_rate_men_1967[i]))
    data_1977_men.append(np.random.randint(age_bucket_lwr[i], age_bucket_upr[i], inc_rate_men_1977[i]))
    data_1987_men.append(np.random.randint(age_bucket_lwr[i], age_bucket_upr[i], inc_rate_men_1987[i]))
    data_1997_men.append(np.random.randint(age_bucket_lwr[i], age_bucket_upr[i], inc_rate_men_1997[i]))
    data_2007_men.append(np.random.randint(age_bucket_lwr[i], age_bucket_upr[i], inc_rate_men_2007[i]))
    data_2017_men.append(np.random.randint(age_bucket_lwr[i], age_bucket_upr[i], inc_rate_men_2017[i]))


data_1967_wmn = np.concatenate(data_1967_wmn).ravel().tolist()
data_1977_wmn = np.concatenate(data_1977_wmn).ravel().tolist()
data_1987_wmn = np.concatenate(data_1987_wmn).ravel().tolist()
data_1997_wmn = np.concatenate(data_1997_wmn).ravel().tolist()
data_2007_wmn = np.concatenate(data_2007_wmn).ravel().tolist()
data_2017_wmn = np.concatenate(data_2017_wmn).ravel().tolist()

data_1967_men = np.concatenate(data_1967_men).ravel().tolist()
data_1977_men = np.concatenate(data_1977_men).ravel().tolist()
data_1987_men = np.concatenate(data_1987_men).ravel().tolist()
data_1997_men = np.concatenate(data_1997_men).ravel().tolist()
data_2007_men = np.concatenate(data_2007_men).ravel().tolist()
data_2017_men = np.concatenate(data_2017_men).ravel().tolist()


data_2017_wmn_dict = {'Starost': data_2017_wmn,
        'Leto': np.repeat("2017", len(data_2017_wmn)).tolist(),
        'Spol': np.repeat("Zenski", len(data_2017_wmn)).tolist()
        }

data_2017_men_dict = {'Starost': data_2017_men,
        'Leto': np.repeat("2017", len(data_2017_men)).tolist(),
        'Spol': np.repeat("Moski", len(data_2017_men)).tolist()
        }

data_2007_wmn_dict = {'Starost': data_2007_wmn,
        'Leto': np.repeat("2007", len(data_2007_wmn)).tolist(),
        'Spol': np.repeat("Zenski", len(data_2007_wmn)).tolist()
        }

data_2007_men_dict = {'Starost': data_2007_men,
        'Leto': np.repeat("2007", len(data_2007_men)).tolist(),
        'Spol': np.repeat("Moski", len(data_2007_men)).tolist()
        }

data_1997_wmn_dict = {'Starost': data_1997_wmn,
        'Leto': np.repeat("1997", len(data_1997_wmn)).tolist(),
        'Spol': np.repeat("Zenski", len(data_1997_wmn)).tolist()
        }

data_1997_men_dict = {'Starost': data_1997_men,
        'Leto': np.repeat("1997", len(data_1997_men)).tolist(),
        'Spol': np.repeat("Moski", len(data_1997_men)).tolist()
        }

data_1987_wmn_dict = {'Starost': data_1987_wmn,
        'Leto': np.repeat("1987", len(data_1987_wmn)).tolist(),
        'Spol': np.repeat("Zenski", len(data_1987_wmn)).tolist()
        }

data_1987_men_dict = {'Starost': data_1987_men,
        'Leto': np.repeat("1987", len(data_1987_men)).tolist(),
        'Spol': np.repeat("Moski", len(data_1987_men)).tolist()
        }

data_1977_wmn_dict = {'Starost': data_1977_wmn,
        'Leto': np.repeat("1977", len(data_1977_wmn)).tolist(),
        'Spol': np.repeat("Zenski", len(data_1977_wmn)).tolist()
        }

data_1977_men_dict = {'Starost': data_1977_men,
        'Leto': np.repeat("1977", len(data_1977_men)).tolist(),
        'Spol': np.repeat("Moski", len(data_1977_men)).tolist()
        }

data_1967_wmn_dict = {'Starost': data_1967_wmn,
        'Leto': np.repeat("1967", len(data_1967_wmn)).tolist(),
        'Spol': np.repeat("Zenski", len(data_1967_wmn)).tolist()
        }

data_1967_men_dict = {'Starost': data_1967_men,
        'Leto': np.repeat("1967", len(data_1967_men)).tolist(),
        'Spol': np.repeat("Moski", len(data_1967_men)).tolist()
        }

data_1967_men_df = pd.DataFrame.from_dict(data_1967_men_dict)
data_1967_wmn_df = pd.DataFrame.from_dict(data_1967_wmn_dict)
data_1977_men_df = pd.DataFrame.from_dict(data_1977_men_dict)
data_1977_wmn_df = pd.DataFrame.from_dict(data_1977_wmn_dict)
data_1987_men_df = pd.DataFrame.from_dict(data_1987_men_dict)
data_1987_wmn_df = pd.DataFrame.from_dict(data_1987_wmn_dict)
data_1997_men_df = pd.DataFrame.from_dict(data_1997_men_dict)
data_1997_wmn_df = pd.DataFrame.from_dict(data_1997_wmn_dict)
data_2007_men_df = pd.DataFrame.from_dict(data_2007_men_dict)
data_2007_wmn_df = pd.DataFrame.from_dict(data_2007_wmn_dict)
data_2017_men_df = pd.DataFrame.from_dict(data_2017_men_dict)
data_2017_wmn_df = pd.DataFrame.from_dict(data_2017_wmn_dict)

data = data_1967_men_df.append(data_1967_wmn_df)
data = data.append(data_1977_wmn_df)
data = data.append(data_1977_men_df)
data = data.append(data_1987_wmn_df)
data = data.append(data_1987_men_df)
data = data.append(data_1997_wmn_df)
data = data.append(data_1997_men_df)
data = data.append(data_2007_wmn_df)
data = data.append(data_2007_men_df)
data = data.append(data_2017_wmn_df)
data = data.append(data_2017_men_df)



ax = sns.violinplot(x="Leto", 
                    y="Starost", 
                    data=data, 
                    hue="Spol",
                    palette="Set2")

ax = sns.violinplot(x="Leto", 
                    y="Starost", 
                    data=data, 
                    hue="Spol",
                    palette="Set2", 
                    split=True)

ax = sns.violinplot(x="Leto", 
                    y="Starost", 
                    data=data, 
                    hue="Spol",
                    palette="Set2", 
                    split=True,                    
                    scale="count")

ax = sns.violinplot(x="Leto", 
                    y="Starost", 
                    data=data, 
                    hue="Spol",
                    palette="Set2", 
                    split=True,                    
                    scale="count",
                    scale_hue=False)




