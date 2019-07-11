
import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn import metrics

from matplotlib import pyplot as plt
import sklearn
from sklearn.metrics import roc_curve, auc
from sklearn.model_selection import train_test_split
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import KFold
from sklearn.linear_model import LogisticRegression
import statsmodels.api as sm
from sklearn.metrics import confusion_matrix
from sklearn.metrics import classification_report
import seaborn as sns


#Load Data
file_loc = 'C:\\Users\\TJ\\Documents\\GitHub\\TheZoen\\Data\\trial_last\\all.xlsx'
df = pd.read_excel(file_loc)
df.info()

df['accident'] = df['accident'].astype(bool)

df.info()

X = df.iloc[:,:6]
Y = df.accident
# X.head()
# Y.head()

#Splitting Data
#split dataset into training set and test set
X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size = 0.3, random_state=1)

model = LogisticRegression()
name = 'Logistic Regression'
model.fit(X_train, Y_train)
logit_model = sm.Logit(Y_train, X_train)
result = logit_model.fit()
print(result.summary2())

pred = model.predict(X_test)
score = sklearn.metrics.accuracy_score(Y_test, pred)
# 열마다 T, F값 저장
preds = model.predict_proba(X_test)
pred = pd.Series(preds[:, 1])
fpr, tpr, thresholds = roc_curve(Y_test, pred)
auc_score = auc(fpr, tpr)
label = '%s: auc=%f' % (name, auc_score)
plt.plot(fpr, tpr, '-', linewidth=.5, label=label)

# K-fold cross validation
cv = KFold(5, shuffle=True, random_state=0)
scores = cross_val_score(model, X_train, Y_train, cv=cv)
print(name)
print("Accuracy :", score)
print("Score of K-fold Cross Validation :\n", scores, "\nMean: {:.2f}".format(scores.mean()), "\n")

print("Confusion Matrix : \n", confusion_matrix(Y_train, model.predict(X_train)))

print("\nClassification Report")
print(classification_report(Y_train, model.predict(X_train)))

plt.legend(loc="lower right")
plt.title("Performance comparison ROC curve")

plt.plot([0, 1], [0, 1], 'k--')
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.05])
plt.xlabel('False positive rate')
plt.ylabel('True positive rate')
plt.show()


print(result.summary()) # 분석결과 출력
print("* Odds Ratio :\n", np.exp(result.params))# 오즈 비(Odds Ratio) 출력
df['predict'] = result.predict(X)
y_hat = df['predict']

from sklearn.metrics import log_loss
#모형의 로그 손실
print("log_loss: ", log_loss(Y, df['predict'], normalize=False))
# 귀무 모형의 모수값
mu_null = np.sum(Y) / len(Y)
print('mu_null :', mu_null)
y_null = np.ones_like(Y) * mu_null
print("log_loss: ",log_loss(Y, y_null, normalize=False))
#McFadden pseudo R square
McRsq = 1 - (log_loss(Y, y_hat) / log_loss(Y, y_null))

print("McFadden pseudo R square :", McRsq)

sns.boxplot(x="accident", y="predict", data=df)
plt.show()

ax = sns.regplot(x= 'rain', y= 'accident', data= df, logistic= True).set_title("Rain Log Odds Linear Plot")
#gre.figure.savefig("gre log lin.png")
plt.show()

ax = sns.regplot(x= 'temp', y= 'accident', data= df, logistic= True).set_title("Temp Log Odds Linear Plot")
#gpa.figure.savefig("gpa log lin.png")
plt.show()
#
#
# data['type'] = data['Cause'].replace(['산악기타', '일반조난', '개인질환','실족추락', '암벽등반',  '낙석낙빙', [0,1,2,3,4,5]])
# data['type'].value_counts().plot(kind = 'bar')
#
#
# from sklearn.model_selection import train_test_split
# X = df.iloc[:,:6]
# Y = df.accident
#
# X_train, X_test, y_train, y_test = train_test_split(X, Y, test_size = 0.3, random_state=1)
#
# from keras.utils import np_utils
# y_train = np_utils.to_categorical(y_train)
# y_test = np_utils.to_categorical(y_test)
#
# #softmax
# from keras.models import Sequential # 케라스의 Sequential()을 임포트
# from keras.layers import Dense # 케라스의 Dense()를 임포트
# from keras import optimizers # 케라스의 옵티마이저를 임포트
# model=Sequential()
# model.add(Dense(3, input_dim=4, activation='softmax'))
# sgd=optimizers.SGD(lr=0.01)
# # 학습률(learning rate, lr)은 0.01로 합니다.
# model.compile(loss='categorical_crossentropy', optimizer='adam',metrics=['accuracy'])
# # 옵티마이저는 경사하강법의 일종인 adam을 사용합니다.
# # 손실 함수(Loss function)은 평균제곱오차 크로스 엔트로피 함수를 사용합니다.
# history=model.fit(X_train,y_train, batch_size=1, epochs=200, validation_data=(X_test, y_test))
#
# epochs = range(1, len(history.history['acc']) + 1)
# plt.plot(epochs, history.history['loss'])
# plt.plot(epochs, history.history['val_loss'])
# plt.title('model loss')
# plt.ylabel('loss')
# plt.xlabel('epoch')
# plt.legend(['train', 'val'], loc='upper left')
# plt.show()
#
# print("\n 테스트 정확도: %.4f" % (model.evaluate(X_test, y_test)[1]))