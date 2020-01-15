# ARMA {#arma}



>Достигнем просветления в анализе временных рядов вместе с нашими друзьями, stata, r и python!
В качестве анализируемых наблюдений используем данные по стоимости акций коммапнии `Apple` c 2015 - 01 - 01 по 2015 - 12 - 31: цена открытия/ закрытия, минимальная/ максимальная цены, объём и скорректиованная цена.

## r

Традиционно начнём в **r**.


Загрузим необходимые пакеты:

```r
library(xts) # работа с временными рядами
library(dplyr) # манипуляции с данными
library(ggplot2) # построение графиков
library(aTSA) # тест Дики-Фуллера
library(forecast) # прогнозирование ARMA-моделей
library(quantmod) # импортирование dataset
library(lmtest) # проверка гипотез
```
Импортируем dataset `AAPL` прямо из пакета `quantmod`. Будем анализировать одномерный временной ряд от переменной `AAPL. Close`.

```r
getSymbols("AAPL",from="2015-01-01",to="2015-12-31")
```

```
[1] "AAPL"
```
Обозначим наш dataframe как `apple_df`. 

```r
apple_df = AAPL$AAPL.Close
```
Визуализируем исследуемый временной ряд, его автокорреляционную и частную автокорреляционную функции.

```r
ggtsdisplay(apple_df)
```

<img src="08-arma_files/figure-html/unnamed-chunk-5-1.png" width="672" style="display: block; margin: auto;" />
По графику видим, что процесс напоминает случайное блуждание: медленно убывает автокорреляционная функция, первый лаг частной автокорреляционной функции не входит в доверительный интервал, остальные - входят.

Проверим стационарность ряда тестом Дики-Фуллера.

```r
adf.test(apple_df)
```

```
Augmented Dickey-Fuller Test 
alternative: stationary 
 
Type 1: no drift no trend 
     lag    ADF p.value
[1,]   0 0.0708   0.664
[2,]   1 0.1616   0.690
[3,]   2 0.1670   0.692
[4,]   3 0.1321   0.682
[5,]   4 0.0120   0.647
Type 2: with drift no trend 
     lag  ADF p.value
[1,]   0 2.15    0.99
[2,]   1 2.06    0.99
[3,]   2 2.24    0.99
[4,]   3 2.35    0.99
[5,]   4 2.65    0.99
Type 3: with drift and trend 
     lag  ADF p.value
[1,]   0 1.75    0.99
[2,]   1 1.53    0.99
[3,]   2 1.64    0.99
[4,]   3 1.77    0.99
[5,]   4 2.17    0.99
---- 
Note: in fact, p.value = 0.01 means p.value <= 0.01 
```
Тест выявил нестационарность на 5% уровне значимости (основная гипотеза – о нестационарности ряда).

Возьмём первую разность от ряда, чтобы сделать его стационарным (ведь только стационарные процессы могут быть описаны моделью `ARMA (p, q)` ) и снова построим автокорреляционную и частную автокорреляционную функции.

```r
apple_diff = diff(apple_df)
ggtsdisplay(apple_diff)
```

<img src="08-arma_files/figure-html/unnamed-chunk-7-1.png" width="672" style="display: block; margin: auto;" />

```r
summary(apple_diff)
```

```
     Index              AAPL.Close      
 Min.   :2015-01-02   Min.   :-6.89000  
 1st Qu.:2015-04-04   1st Qu.:-1.02500  
 Median :2015-07-02   Median :-0.07500  
 Mean   :2015-07-02   Mean   :-0.00804  
 3rd Qu.:2015-09-30   3rd Qu.: 1.14499  
 Max.   :2015-12-30   Max.   : 6.17000  
                      NA's   :1         
```
Ряд похож на стационарный. Теперь построим несколько моделей, которые потенциально могут описать данный ряд, хотя уже заранее ожидается, что ряд в разностях будет описан `ARIMA (0, 0, 0)`, что равносильно `ARMA(0, 0)`, но всё же...

`ARIMA (0, 0, 0)`:

```r
arima_000 = arima(apple_diff, order=c(0, 0, 0))
summary(arima_000)
```

```

Call:
arima(x = apple_diff, order = c(0, 0, 0))

Coefficients:
      intercept
        -0.0080
s.e.     0.1244

sigma^2 estimated as 3.867:  log likelihood = -523.79,  aic = 1051.58

Training set error measures:
                       ME     RMSE      MAE      MPE     MAPE      MASE
Training set 8.078552e-15 1.966425 1.495158 99.55996 99.55996 0.6825331
                    ACF1
Training set -0.02936922
```
Построим также модель `ARIMA (1, 0, 0)` , что равносильно `ARMA (1, 0)`, для сравнения.

```r
arima_100 = arima(apple_diff, order=c(1, 0, 0))
summary(arima_100)
```

```

Call:
arima(x = apple_diff, order = c(1, 0, 0))

Coefficients:
          ar1  intercept
      -0.0296    -0.0075
s.e.   0.0635     0.1208

sigma^2 estimated as 3.863:  log likelihood = -523.68,  aic = 1053.36

Training set error measures:
                        ME     RMSE      MAE      MPE     MAPE      MASE
Training set -0.0003728078 1.965566 1.491983 94.09101 105.0814 0.6810838
                     ACF1
Training set -0.002372191
```

```r
coeftest(arima_100)
```

```

z test of coefficients:

            Estimate Std. Error z value Pr(>|z|)
ar1       -0.0296313  0.0634712 -0.4668   0.6406
intercept -0.0075101  0.1207545 -0.0622   0.9504
```
По информационному критерию Акаике первая модель лучше (AIC меньше), а также во второй модели коэффициент перед ar(1) незначим. 

Получается, что (как и ожидалось) первая модель лучше.
Можно схитрить и использовать функцию автоподбора коэффициентов модели ARIMA.

```r
arima_auto_model = auto.arima(apple_diff)
summary(arima_auto_model)
```

```
Series: apple_diff 
ARIMA(0,0,0) with zero mean 

sigma^2 estimated as 3.867:  log likelihood=-523.79
AIC=1049.58   AICc=1049.6   BIC=1053.1

Training set error measures:
                       ME     RMSE     MAE       MPE     MAPE      MASE
Training set -0.008040008 1.966441 1.49548 -37.76185 445.4945 0.6841274
                    ACF1
Training set -0.02936922
```
Такая функция автоматически минимизирует критерий Акаике. Заметим, что автоподбор выдал модель `ARIMA (0, 0, 0)` для первой разности.

Теперь проверим остатки модели `ARIMA (0, 0, 0)` на белошумность. Сохраним остатки и проделаем тест Льюнг-Бокса, в котором основная гипотеза - остатки независимы. 

Сохраним остатки модели `ARIMA (0, 0, 0)` и построим тест Льюнг-Бокса (если наблюдений мало, то используем опцию `Box-Pierce`).

```r
res_arima_000 = resid(arima_000)
Box.test(res_arima_000, lag=10, type="Ljung-Box")
```

```

	Box-Ljung test

data:  res_arima_000
X-squared = 4.2362, df = 10, p-value = 0.9361
```
Основная гипотеза об отсутствии автокорреляции остатков отвергается, следовательно, модель корректно описывает структуру автокорреляции.

Время небольших фактов: Льюнг - это женщина-статистик! Поэтому правильно склонять "Льюнг-Бокса", а не "Льюнга-Бокса"!


![](images/Greta.jpg)

Можно ещё также научиться оценивать визуально, где лежат корни AR и MA (`unit root test`). Так как для построенной модели нет AR и MA частей (`ARIMA (0, 0, 0)`), то можно применить команду к, например, `ARIMA (1, 0, 0)`:

```r
autoplot(arima_100)
```

<img src="08-arma_files/figure-html/unnamed-chunk-12-1.png" width="672" style="display: block; margin: auto;" />
Построим прогноз на 3 периода вперёд для модели `arima_000`. Визуализируем прогноз, границы 80% и 95% доверительного интервалов.

```r
forecast(arima_000, h=10) %>%
autoplot()
```

<img src="08-arma_files/figure-html/unnamed-chunk-13-1.png" width="672" style="display: block; margin: auto;" />

## python

Настало время **python**!

Импортируем необходимые пакеты.

```python
import quandl # импортирование данных из Сети
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): ModuleNotFoundError: No module named 'quandl'

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
import datetime # работа с форматами даты и времени
import matplotlib.pyplot as plt # построение графиков
from pandas import Series # работа с временными рядами
import statsmodels
from statsmodels.tsa.arima_model import ARMA # ARMA-модели
from statsmodels.graphics.tsaplots import plot_acf # построение графиков acf и pacf
from statsmodels.graphics.tsaplots import plot_pacf
import statsmodels.api as sm
from statsmodels.stats import diagnostic as diag # тесты
import pmdarima as pm
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): ModuleNotFoundError: No module named 'pmdarima'

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
from pmdarima.arima import auto_arima # автоподбор коэффициентов модели ARIMA
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): ModuleNotFoundError: No module named 'pmdarima'

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
from statsmodels.tsa.stattools import adfuller # тест Дики-Фуллера
```
Загрузим dataset:

```python
start = datetime.datetime(2015, 1, 1)
end = datetime.datetime(2015, 12, 31)
apple = quandl.get("WIKI/" + "AAPL", start_date=start, end_date=end)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'quandl' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```
Проверим загрузку данных. Установим dataset как цену закрытия.

```python
apple.head()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'apple' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
apple_df = apple["Close"]
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'apple' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```
Посмотрим на структуру временного ряда, автокорреляционную и частную автокорреляционную функции.

```python
apple_df.plot(grid=True)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'apple_df' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
plt.title("Структурa временного ряда")
plt.show()
```

<img src="08-arma_files/figure-html/unnamed-chunk-18-1.png" width="672" style="display: block; margin: auto;" />

```python
plot_acf(apple_df, lags=20)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'apple_df' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
plt.show()
```

<img src="08-arma_files/figure-html/unnamed-chunk-18-2.png" width="672" style="display: block; margin: auto;" />

```python
plot_pacf(apple_df, lags=20)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'apple_df' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
plt.show()
```

<img src="08-arma_files/figure-html/unnamed-chunk-18-3.png" width="672" style="display: block; margin: auto;" />
Появились очень знакомые (и красивые) графики. Важно отметить, что на графиках есть 0 - лаг, он равен единице, в предыдущих графиках его не было.

Проверим стационарность ряда тестом Дики-Фуллера.

```python
res = sm.tsa.adfuller(apple_df, regression='ct')
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'apple_df' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
'p-value:{}'.format(res[1])
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'res' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```
Возьмём первую разность.

```python
apple_diff = apple_df.diff(periods=1).dropna()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'apple_df' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```
И визуализируем структуру нового ряда.

```python
apple_diff.plot(grid=True)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'apple_diff' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
plt.title("Структурa временного ряда")
plt.show()
```

<img src="08-arma_files/figure-html/unnamed-chunk-21-1.png" width="672" style="display: block; margin: auto;" />

```python
plot_acf(apple_diff, lags=50)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'apple_diff' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
plt.show()
```

<img src="08-arma_files/figure-html/unnamed-chunk-21-2.png" width="672" style="display: block; margin: auto;" />

```python
plot_pacf(apple_diff, lags=50)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'apple_diff' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
plt.show()
```

<img src="08-arma_files/figure-html/unnamed-chunk-21-3.png" width="672" style="display: block; margin: auto;" />
Аналогично операциям в **r**, смоделируем данный ряд как `ARMA (0, 0)`.

```python
arma_00 = ARMA(apple_diff, order=(0, 0))
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'apple_diff' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
arma_00_fit = arma_00.fit(disp=False)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'arma_00' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
arma_00_fit.summary()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'arma_00_fit' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```
Смоделируем ряд как `ARMA (1, 0)`:

```python
arma_10 = ARMA(apple_diff, order=(1, 0))
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'apple_diff' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
arma_10_fit = arma_10.fit(disp=False)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'arma_10' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
arma_10_fit.summary()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'arma_10_fit' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```
Вторая модель имеет более высокое значение критерия Акаике и незначимый коэффициент перед ar(1).

Отдельно можно выделить значения AIC и BIC для построенных моделей.

```python
np.round(arma_00_fit.aic, 2)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'np' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
np.round(arma_10_fit.aic, 2)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'np' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
np.round(arma_00_fit.bic, 2)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'np' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
np.round(arma_10_fit.bic, 2)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'np' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```
Как и в **r**, **python** имеет опцию автоподбора коэффициентов модели `ARIMA`.

```python
auto_arima_python = pm.auto_arima(apple_diff)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'pm' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
auto_arima_python.summary()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'auto_arima_python' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```
В строчке `SARIMAX` нет рядом коэффициентов. Это означает, что они нулевые, как и предполагалось, то есть модель описывается `ARMA (0, 0)`.Эта функция также удобна тем, что выводит статистики.

Проверим белошумность остатков тестом Льюнг - Бокса.

Сначала сохраним остатки как `residuals`.

```python
residuals = pd.DataFrame(arma_00_fit.resid)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'pd' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
diag.acorr_ljungbox(residuals, lags=10, boxpierce=False)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'residuals' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```
Посмотрим на прогноз на 10 дней вперёд.

```python
forecast = arma_00_fit.forecast(steps=10)[0]
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'arma_00_fit' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
forecast
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'forecast' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```
И визуализируем прогнозные значения на исходном графике.

```python
arma_00_fit.plot_predict(len(apple_diff)-250, len(apple_diff)+10)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'arma_00_fit' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
plt.xlabel('Лаги')
plt.ylabel('Изменение цены')
plt.title('Изменение цены закрытия AAPL')
plt.show()
```

<img src="08-arma_files/figure-html/unnamed-chunk-29-1.png" width="672" style="display: block; margin: auto;" />

## stata



Теперь научимся анализировать временные ряды в **stata**.
Импортируем dataset.

```stata
use data/apple.dta
```
Установим временной формат переменной `Date` и визуализируем исследуемый временной ряд, его автокорреляционную и частную автокорреляционную функции.


```stata
tsset Date
```

```
        time variable:  Date, 1/2/2015 to 12/30/2015, but with gaps
                delta:  1 day
```


```stata
tsline Close
```
![](ts.png)

```stata
ac Close
```
![](ac.png)

```stata
pac Close
```
![](pac.png)


Проверим стационарность ряда тестом Дики-Фуллера.

```stata
dfuller Close, trend lags(0)
```

```
 translator Graph2png not found
r(111);



Dickey-Fuller test for unit root                   Number of obs   =       197

                               ---------- Interpolated Dickey-Fuller ---------
                  Test         1% Critical       5% Critical      10% Critical
               Statistic           Value             Value             Value
------------------------------------------------------------------------------
 Z(t)             -3.161            -4.008            -3.437            -3.137
------------------------------------------------------------------------------
MacKinnon approximate p-value for Z(t) = 0.0925
```
Тест выявил нестационарность на 5% уровне значимости (основная гипотеза - о нестационарности).
Возьмём первую разность от ряда, чтобы сделать его стационарным и снова построим графики ACF и PACF.

```stata
gen Close_1 = Close[_n]-Close[_n-1]
```
И визуализируем его, вместе с автокорреляционной и частной автокорреляционной функциями.

```stata
tsline Close_1
```
![](ts_1.png)

```stata
ac Close_1
```
![](acc_1.png)

```stata
pac Close_1
```
![](pacc_1.png)


Теперь построим несколько моделей, которые потенциально могут описать данный ряд, хотя уже заранее ожидается, что ряд в разностях будет описан `ARIMA (0, 0, 0)`, что равносильно `ARMA (0, 0)`, но всё же...
`ARIMA (0, 0, 0)`.Можно также отдельно вывести AIC и BIC для построенной модели. Построим также модель `ARIMA (1, 0, 0)` для сравнения.

```stata
arima Close_1, arima(0, 0, 0)
estat ic
arima Close_1, arima(1, 0, 0)
estat ic
```

```
 translator Graph2png not found
r(111);



Number of gaps in sample:  52
(note: filtering over missing observations)

(setting optimization to BHHH)
Iteration 0:   log likelihood = -523.78892  
Iteration 1:   log likelihood = -523.78892  

ARIMA regression

Sample:  1/5/2015 - 12/30/2015, but with gaps   Number of obs     =        250
                                                Wald chi2(.)      =          .
Log likelihood = -523.7889                      Prob > chi2       =          .

------------------------------------------------------------------------------
             |                 OPG
     Close_1 |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
Close_1      |
       _cons |    -.00804   .1245604    -0.06   0.949     -.252174     .236094
-------------+----------------------------------------------------------------
      /sigma |   1.966425   .0725772    27.09   0.000     1.824176    2.108674
------------------------------------------------------------------------------
Note: The test of the variance against zero is one sided, and the two-sided
      confidence interval is truncated at zero.


Akaike's information criterion and Bayesian information criterion

-----------------------------------------------------------------------------
       Model |        Obs  ll(null)  ll(model)      df         AIC        BIC
-------------+---------------------------------------------------------------
           . |        250         .  -523.7889       2    1051.578   1058.621
-----------------------------------------------------------------------------
               Note: N=Obs used in calculating BIC; see [R] BIC note.

(note:  insufficient memory or observations to estimate usual
starting values [2])

Number of gaps in sample:  52
(note: filtering over missing observations)

(setting optimization to BHHH)
Iteration 0:   log likelihood = -523.76806  
Iteration 1:   log likelihood = -523.73894  
Iteration 2:   log likelihood = -523.73866  
Iteration 3:   log likelihood = -523.73865  

ARIMA regression

Sample:  1/5/2015 - 12/30/2015, but with gaps   Number of obs     =        250
                                                Wald chi2(1)      =       0.15
Log likelihood = -523.7386                      Prob > chi2       =     0.6990

------------------------------------------------------------------------------
             |                 OPG
     Close_1 |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
Close_1      |
       _cons |  -.0086177   .1234091    -0.07   0.944    -.2504952    .2332598
-------------+----------------------------------------------------------------
ARMA         |
          ar |
         L1. |  -.0220611   .0570619    -0.39   0.699    -.1339004    .0897782
-------------+----------------------------------------------------------------
      /sigma |   1.965944   .0727877    27.01   0.000     1.823283    2.108606
------------------------------------------------------------------------------
Note: The test of the variance against zero is one sided, and the two-sided
      confidence interval is truncated at zero.


Akaike's information criterion and Bayesian information criterion

-----------------------------------------------------------------------------
       Model |        Obs  ll(null)  ll(model)      df         AIC        BIC
-------------+---------------------------------------------------------------
           . |        250         .  -523.7386       3    1053.477   1064.042
-----------------------------------------------------------------------------
               Note: N=Obs used in calculating BIC; see [R] BIC note.
```
По информационному критерию Акаике первая модель лучше (AIC меньше), а также во второй модели коэффициент перед ar(1) незначим. 
Проверим остатки модели `ARIMA (0, 0, 0)` на белошумность. Сохраним остатки модели и проверим тестом Льюнг-Бокса. Основная гипотеза - остатки независимы.

```stata
arima Close_1, arima(0, 0, 0)
predict res, resid
wntestq res
```

```
 translator Graph2png not found
r(111);



Number of gaps in sample:  52
(note: filtering over missing observations)

(setting optimization to BHHH)
Iteration 0:   log likelihood = -523.78892  
Iteration 1:   log likelihood = -523.78892  

ARIMA regression

Sample:  1/5/2015 - 12/30/2015, but with gaps   Number of obs     =        250
                                                Wald chi2(.)      =          .
Log likelihood = -523.7889                      Prob > chi2       =          .

------------------------------------------------------------------------------
             |                 OPG
     Close_1 |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
Close_1      |
       _cons |    -.00804   .1245604    -0.06   0.949     -.252174     .236094
-------------+----------------------------------------------------------------
      /sigma |   1.966425   .0725772    27.09   0.000     1.824176    2.108674
------------------------------------------------------------------------------
Note: The test of the variance against zero is one sided, and the two-sided
      confidence interval is truncated at zero.

(1 missing value generated)

(note: time series has 52 gaps)

Portmanteau test for white noise
---------------------------------------
 Portmanteau (Q) statistic =    26.8342
 Prob > chi2(40)           =     0.9449
```
Теперь попробуем построить прогноз по модели `ARIMA (0, 0, 0)`:

```stata
arima Close_1, arima(0, 0, 0)
predict prognoz
display prognoz
```

```
 translator Graph2png not found
r(111);



Number of gaps in sample:  52
(note: filtering over missing observations)

(setting optimization to BHHH)
Iteration 0:   log likelihood = -523.78892  
Iteration 1:   log likelihood = -523.78892  

ARIMA regression

Sample:  1/5/2015 - 12/30/2015, but with gaps   Number of obs     =        250
                                                Wald chi2(.)      =          .
Log likelihood = -523.7889                      Prob > chi2       =          .

------------------------------------------------------------------------------
             |                 OPG
     Close_1 |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
Close_1      |
       _cons |    -.00804   .1245604    -0.06   0.949     -.252174     .236094
-------------+----------------------------------------------------------------
      /sigma |   1.966425   .0725772    27.09   0.000     1.824176    2.108674
------------------------------------------------------------------------------
Note: The test of the variance against zero is one sided, and the two-sided
      confidence interval is truncated at zero.

(option xb assumed; predicted values)

-.00804
```










