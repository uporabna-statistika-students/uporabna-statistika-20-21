import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from sklearn.linear_model import LinearRegression
from sklearn.linear_model import LogisticRegression
from sklearn.neighbors import KNeighborsRegressor
from sklearn.neighbors import KNeighborsClassifier


def prepare_country_stats(oecd_bli, gdp_per_capita):
    oecd_bli = oecd_bli[oecd_bli["INEQUALITY"]=="TOT"]
    oecd_bli = oecd_bli.pivot(index="Country", columns="Indicator", values="Value")
    gdp_per_capita.rename(columns={"2015": "GDP per capita"}, inplace=True)
    gdp_per_capita.set_index("Country", inplace=True)
    full_country_stats = pd.merge(left=oecd_bli, right=gdp_per_capita,
                                  left_index=True, right_index=True)
    full_country_stats.sort_values(by="GDP per capita", inplace=True)
    remove_indices = [0, 1, 6, 8, 33, 34, 35]
    keep_indices = list(set(range(36)) - set(remove_indices))
    return full_country_stats[["GDP per capita", 'Life satisfaction']].iloc[keep_indices]

# Load the data
oecd_bli = pd.read_csv("oecd_bli_2015.csv", thousands=',')
gdp_per_capita = pd.read_csv("gdp_per_capita.csv",thousands=',',delimiter='\t', encoding='latin1', na_values="n/a")

# Prepare the data
country_stats = prepare_country_stats(oecd_bli, gdp_per_capita)
X = np.c_[country_stats["GDP per capita"]]
#X = 0.01*X
y = np.c_[country_stats["Life satisfaction"]]
# Visualize the data

#df = pd.DataFrame(np.column_stack((X,y)), columns=['x','y'])
#print(df)
#exit()

#plt.show()
# Select a linear model
lin_reg_model = LinearRegression()
kNNcont = KNeighborsRegressor(n_neighbors=5)
# Train the model
lin_reg_model.fit(X, y)
kNNcont.fit(X, y)

# test data to draw a nice regresion line
X_new = [[8], [60]]
y_predict = lin_reg_model.predict(X_new)
xknn = [[10], [20], [30], [40], [50]]
yknn = kNNcont.predict(xknn)
print("kNN regression {}".format(yknn.ravel()))

# new plot; take one of the lines and plot the errors
f, ax = plt.subplots()
plt.plot(X_new, y_predict, "r-") # plot best fit line
plt.plot(X, y, "b.")
plt.plot(xknn, yknn, "go")
#country_stats.plot(kind='scatter', x="GDP per capita", y='Life satisfaction')
plt.xlabel("GDP per capita [1000 USD]")
plt.ylabel("Life satisfaction")
f.savefig("oecd_continuous.jpg", bbox_inches='tight')

exit()


# now make satisfaction discrete - 0 or 1 (threshold 6)
yd = np.where(y>6.5, 1, 0)

LR1 = LinearRegression()
knnClass = KNeighborsClassifier(n_neighbors=3)

LR1.fit(X, yd)
knnClass.fit(X, yd.ravel())
X_new = [[8], [50]]
y_predict = LR1.predict(X_new)
xknnClass = [[10], [20], [30], [40], [50]]
yknnClass = knnClass.predict(xknnClass)

f, ax = plt.subplots()
plt.plot(X_new, y_predict, "r-") # plot best fit line
plt.plot(X, yd, "b.")
plt.plot(xknnClass, yknnClass.ravel(), "go")
plt.xlabel("GDP per capita [1000 USD]")
plt.ylabel("Life satisfaction")
f.savefig("oecd_discrete.jpg", bbox_inches='tight')


"""
LOGISTIC REGRESSION
"""

# telesna temperatura, razred "0=zdrav, 1=bolan"
#X = np.array([36.5, 36.6, 36.8, 36.9, 37., 37.2, 37.5, 37.6, 39.5])
#yd = np.array([0, 0, 0, 1, 0, 1, 1, 1, 1])
#X = X.reshape(-1, 1)

LogReg = LogisticRegression(solver='lbfgs', max_iter=1000) #, C=100000000000)
LogReg.fit(X, yd.ravel())
print(LogReg.coef_, LogReg.intercept_)

Xnew = np.linspace(X.min(), X.max(), 1000).reshape(-1,1)
yp = LogReg.predict_proba(Xnew)

f, ax = plt.subplots()
plt.plot(X, yd, "b.")
plt.xlabel("GDP per capita [1000 USD]")
plt.ylabel("Life satisfaction")
#loss = model(X * LogReg.coef_ + LogReg.intercept_).ravel()
plt.plot(Xnew, yp[:,1], color='red', linewidth=1)
f.savefig("oecd_logreg.jpg", bbox_inches='tight')
