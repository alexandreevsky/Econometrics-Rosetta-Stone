# Модели множественного выбора {#multchoice}




Загрузим необходимые пакеты.

```r
library(tidyverse) # для манипуляций с данными и построения графиков
library(skimr) # для красивого summary
library(rio) # для чтения .dta файлов
library(margins) # для расчета предельных эффектов
library(mlogit)
library(skimr)
library(nnet)
library(questionr)
library(MASS)
library(survival)
library(lattice)
```

## r

Импортируем датасет. В нем находятся данные по клиентам пенсионных фондов. Нас интересует переменная `pctstck`, которая принимает три значения: 0, 50, 100 - в зависимоcти от ответа респондента на вопрос о предпочтительном способе инвестирования пенсионных накоплений - в облигации, смешанный способ и в акции.   

```r
df = rio::import("data/pension.dta")
```

Начнем с пристального взгляда на описательные статистки. 

```r
skim_with(numeric = list(p25 = NULL, p75 = NULL)) 
skim(df)
```

```
Skim summary statistics
 n obs: 226 
 n variables: 19 

── Variable type:numeric ─────────────────────────────────────────────────────────────────────────────────────────────────────────
 variable missing complete   n     mean      sd   p0     p50 p100     hist
      age       0      226 226   60.7      4.29   53   60      73 ▃▇▅▆▅▂▁▁
    black       0      226 226    0.12     0.33    0    0       1 ▇▁▁▁▁▁▁▁
   choice       0      226 226    0.62     0.49    0    1       1 ▅▁▁▁▁▁▁▇
     educ       7      219 226   13.52     2.55    8   12      18 ▁▁▁▇▁▁▂▂
   female       0      226 226    0.6      0.49    0    1       1 ▅▁▁▁▁▁▁▇
  finc100      10      216 226    0.12     0.33    0    0       1 ▇▁▁▁▁▁▁▁
  finc101      10      216 226    0.065    0.25    0    0       1 ▇▁▁▁▁▁▁▁
   finc25      10      216 226    0.21     0.41    0    0       1 ▇▁▁▁▁▁▁▂
   finc35      10      216 226    0.19     0.39    0    0       1 ▇▁▁▁▁▁▁▂
   finc50      10      216 226    0.25     0.43    0    0       1 ▇▁▁▁▁▁▁▂
   finc75      10      216 226    0.12     0.33    0    0       1 ▇▁▁▁▁▁▁▁
       id       0      226 226 2445.09  1371.27   38 2377.5  5014 ▆▅▆▆▃▅▇▃
  irain89       0      226 226    0.5      0.5     0    0.5     1 ▇▁▁▁▁▁▁▇
  married       0      226 226    0.73     0.44    0    1       1 ▃▁▁▁▁▁▁▇
  pctstck       0      226 226   46.68    39.44    0   50     100 ▇▁▁▇▁▁▁▆
  prftshr      20      206 226    0.21     0.41    0    0       1 ▇▁▁▁▁▁▁▂
   pyears       8      218 226   11.39     9.61    0    9      45 ▇▇▃▂▂▁▁▁
 stckin89       0      226 226    0.32     0.47    0    0       1 ▇▁▁▁▁▁▁▃
 wealth89       0      226 226  197.91   242.09 -580  127.85 1485 ▁▁▇▃▁▁▁▁
```

Отсюда несложно заметить, что переменная `choice` - бинарная. И принимает значение `1`, если индивид в выборке имел право выбора схемы инвестирования. Переменнная `wealth98` - чистое богатство пенсионеров на 1989 год. Остальные переменные нас пока что не интересуют :)


Для начала разберемся с объясняемой переменной. Превратим её в факторную и упорядочим категории. 

```r
df = mutate(df, y = factor(pctstck)) # факторная переменная
df = mutate(df, y = relevel(y, ref = 2)) # сменить базовую категорию
levels(df$y)
```

```
[1] "50"  "0"   "100"
```

Можно взглянуть на значения объясняемой переменной в разрезе какой-то другой переменной. 

```r
table(df$y, df$educ)
```

```
     
       8  9 10 11 12 13 14 15 16 17 18
  50   1  1  0  3 34  4  6  2 14  5 14
  0    5  3  0  3 31  4  7  0 11  1  7
  100  0  2  1  1 36  1  5  4  5  4  4
```

Построим модель множественного выбора (лог-линейная модель). 


```r
multmodel = multinom(y ~ choice+age+educ+wealth89+prftshr, data = df, reflevel = '0')
```

```
# weights:  21 (12 variable)
initial  value 220.821070 
iter  10 value 208.003694
iter  20 value 204.508245
final  value 204.507779 
converged
```

```r
summary(multmodel)
```

```
Call:
multinom(formula = y ~ choice + age + educ + wealth89 + prftshr, 
    data = df, reflevel = "0")

Coefficients:
    (Intercept)       choice        age       educ     wealth89   prftshr
0    -3.7778032 -0.626950541 0.10621896 -0.1851817 3.716695e-04 0.2718066
100   0.7155771 -0.002461399 0.01139191 -0.1387427 1.684637e-05 1.2527158

Std. Errors:
    (Intercept)    choice        age       educ     wealth89   prftshr
0      1.577870 0.3701450 0.02706225 0.06827224 0.0007353231 0.4980872
100    1.368662 0.3876881 0.02549556 0.06954044 0.0007948194 0.4622377

Residual Deviance: 409.0156 
AIC: 433.0156 
```

При необходимости можем построить модельку для подвыборки, например, только для замужних/женатых.

```r
multmodel_married = multinom(y ~ choice+age+educ+wealth89+prftshr, subset = married == 1, data = df, reflevel = '0')
```

```
# weights:  21 (12 variable)
initial  value 165.890456 
iter  10 value 152.140783
iter  20 value 149.611135
final  value 149.611069 
converged
```

```r
summary(multmodel_married)
```

```
Call:
multinom(formula = y ~ choice + age + educ + wealth89 + prftshr, 
    data = df, subset = married == 1, reflevel = "0")

Coefficients:
    (Intercept)     choice       age       educ     wealth89    prftshr
0    -4.9076771 -1.0040922 0.1279089 -0.1905437 0.0006204087 -0.1901239
100   0.2276417 -0.5382183 0.0133528 -0.1000726 0.0004076403  1.0692773

Std. Errors:
    (Intercept)    choice        age       educ     wealth89   prftshr
0      1.850782 0.4463254 0.03189365 0.07925101 0.0008454956 0.5621150
100    1.603713 0.4540907 0.02965149 0.07925338 0.0008825075 0.4947089

Residual Deviance: 299.2221 
AIC: 323.2221 
```

Быстренько прикинули значимость коэффициентов.

```r
coef(multmodel)/summary(multmodel)$standard.errors
```

```
    (Intercept)       choice       age      educ   wealth89   prftshr
0    -2.3942416 -1.693797014 3.9249859 -2.712401 0.50545058 0.5457008
100   0.5228296 -0.006348915 0.4468195 -1.995136 0.02119521 2.7101114
```

Сохраним прогнозы.

```r
fit_values = fitted(multmodel)
```

И посчитать относительное изменение отношения шансов:

\[
\frac{P(y_{i} = j)}{P(y_{i} = 1)} = exp(x_{i}\beta)
\] - показывает изменение отношения шансов при выборе альтернативы j вместо базовой альтернативы 1, если x изменился на единицу


```r
odds.ratio(multmodel) 
```

```
                       OR     2.5 %  97.5 %         p    
0/(Intercept)   0.0228729 0.0010381  0.5040  0.016655 *  
0/choice        0.5342184 0.2586133  1.1035  0.090304 .  
0/age           1.1120653 1.0546173  1.1726 8.673e-05 ***
0/educ          0.8309533 0.7268808  0.9499  0.006680 ** 
0/wealth89      1.0003717 0.9989310  1.0018  0.613242    
0/prftshr       1.3123331 0.4943921  3.4835  0.585272    
100/(Intercept) 2.0453667 0.1398827 29.9074  0.601093    
100/choice      0.9975416 0.4665845  2.1327  0.994934    
100/age         1.0114570 0.9621562  1.0633  0.655005    
100/educ        0.8704520 0.7595422  0.9976  0.046028 *  
100/wealth89    1.0000168 0.9984602  1.0016  0.983090    
100/prftshr     3.4998349 1.4144581  8.6597  0.006726 ** 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


Можем посчитать предельные эффекты в различных квартилях.

```r
summary(marginal_effects(multmodel))
```

```
  dydx_choice         dydx_age           dydx_educ      
 Min.   :0.02341   Min.   :-0.019966   Min.   :0.01298  
 1st Qu.:0.05924   1st Qu.:-0.015237   1st Qu.:0.03141  
 Median :0.07324   Median :-0.013644   Median :0.03791  
 Mean   :0.06945   Mean   :-0.012893   Mean   :0.03468  
 3rd Qu.:0.08402   3rd Qu.:-0.011486   3rd Qu.:0.03905  
 Max.   :0.11414   Max.   :-0.005686   Max.   :0.04223  
 dydx_wealth89         dydx_prftshr     
 Min.   :-6.855e-05   Min.   :-0.25602  
 1st Qu.:-5.119e-05   1st Qu.:-0.19363  
 Median :-4.509e-05   Median :-0.16877  
 Mean   :-4.275e-05   Mean   :-0.15863  
 3rd Qu.:-3.729e-05   3rd Qu.:-0.13454  
 Max.   :-1.675e-05   Max.   :-0.03909  
```

Или при заданном значении объясняемых переменных.

```r
margins(multmodel,at = list(age = 69, choice = 1))
```

```
 at(age) at(choice)  choice      age    educ   wealth89 prftshr
      69          1 0.08024 -0.01431 0.03263 -4.857e-05 -0.1158
```


Теперь посмотрим на модель упорядоченного выбора :) Для нее возьмем другие данные. Выборку позаимствуем из опроса NLSY (National Longitudinal Survey of Youth). В ней представлены данные о 3705 молодых белых женщинах из США.
Зависимая переменная tradrole – степень согласия с утверждением «Место женщины дома, а не на работе» по четырехбалльной шкале (1 – категорически не согласна, 2 – не согласна, 3 – согласна, 4 – совершенно согласна).


```r
data_nlsy = import('data/tradrole.dta')
```

```
Error in import("data/tradrole.dta"): No such file
```

```r
#skim(data_nlsy)
```


```r
ggplot(data_nlsy, aes(x = tradrole)) + 
  geom_histogram() + 
  xlab('Ответы респонденток') +
  ggtitle('Вот такие дела, джентельмены :)')
```

```
Error in ggplot(data_nlsy, aes(x = tradrole)): object 'data_nlsy' not found
```

Посмотрим, как влияет религиозное воспитание (`cath` - католичество и `fpro` - протестанство), число лет образования матери - `meduc` и проживание в крупном городе `urb` на объясняемую переменную.

```r
oprobit <- polr(as.factor(tradrole) ~  as.factor(cath) + as.factor(fpro) + meduc + as.factor(urb), data = data_nlsy, method="probit", na.action = na.omit)
```

```
Error in eval(expr, p): object 'data_nlsy' not found
```

```r
summary(oprobit)
```

```
Error in summary(oprobit): object 'oprobit' not found
```

В summary видим коэффициенты при регрессорах и коэффициенты при константах для каждой из упорядоченных альтернатив.

```r
summary(oprobit)
```

```
Error in summary(oprobit): object 'oprobit' not found
```

## python



```python
import numpy as np
import pandas as pd
import statsmodels.api as st
import matplotlib.pyplot as plt

plt.style.use('ggplot')
```



```python
df = pd.read_stata('data/pension.dta')
```


```python
df.describe()
```

```
                id      pyears     prftshr  ...    stckin89    irain89     pctstck
count   226.000000  218.000000  206.000000  ...  226.000000  226.00000  226.000000
mean   2445.092920   11.385321    0.208738  ...    0.318584    0.50000   46.681416
std    1371.270511    9.605498    0.407397  ...    0.466962    0.50111   39.441155
min      38.000000    0.000000    0.000000  ...    0.000000    0.00000    0.000000
25%    1312.500000    4.000000    0.000000  ...    0.000000    0.00000    0.000000
50%    2377.500000    9.000000    0.000000  ...    0.000000    0.50000   50.000000
75%    3804.250000   16.000000    0.000000  ...    1.000000    1.00000  100.000000
max    5014.000000   45.000000    1.000000  ...    1.000000    1.00000  100.000000

[8 rows x 19 columns]
```



```python
df.rename(columns = {'pctstck':'y'}, inplace = True)
```

Подготовим данные для построения модели множественного выбора. Избавимся от пропусков в интересующих нас переменных и добавим вектор констант. 


```python
sub = df1[['y', 'choice', 'age', 'wealth89', 'prftshr', 'married']].dropna()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'df1' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
y = sub['y']
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'sub' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
X = sub[['choice', 'age', 'wealth89', 'prftshr']]
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'sub' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
X = st.add_constant(X, prepend = False)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'X' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Кросс - табличка для объясняемой переменной и числа лет образования.

```python
pd.crosstab(sub['y'], sub['educ'])
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'sub' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Строим модель.

```python
multmodel = st.MNLogit(y, X, )
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'y' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
mm_fit = multmodel.fit()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'multmodel' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
mm_fit.summary() ### сразу же можно проверить значимость коэффициентов
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'mm_fit' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```


```python
fitted_values = mm_fit.predict()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'mm_fit' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Отдельно можем извлечь параметры.

```python
mm_fit.params
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'mm_fit' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Для того, чтобы построить модельку по подвыборке, её (подвыборку) нужно создать :)

```python
data_m = sub[(sub.married == 1)]
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'sub' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
y_m = data_m['y']
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'data_m' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
X_m = data_m[['choice', 'age', 'wealth89', 'prftshr']]
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'data_m' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
X_m = st.add_constant(X_m, prepend = False)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'X_m' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

Дальше всё аналогично :)

```python
multmodel_m = st.MNLogit(y_m, X_m)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'y_m' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
mm_fit_m = multmodel_m.fit()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'multmodel_m' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
mm_fit_m.summary()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'mm_fit_m' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

C пределными эффектами в питоне беда!!!!

```python
margeff = mm_fit.get_margeff()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'mm_fit' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
np.round(margeff.margeff, 3)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'margeff' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```
 
Или все-таки беда с отношением шансов?

```python
y50_data = sub[sub['y'] == 50][sub.columns.difference(['y', 'married'])]
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'sub' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
y100_data = sub[sub['y'] == 100][sub.columns.difference(['y', 'married'])]
#np.exp(mm_fit.params[0]*y100_data) # кажется, это придется считать вручную :(
#np.exp(mm_fit.params[0]*y100_data) # не уверена, что так, но пусть пока будет
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'sub' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

И вернемся к ~~сильным и независимым~~ моделькам упорядоченного выбора :) 

```python
data_nlsy = pd.read_stata('data/tradrole.dta')
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): FileNotFoundError: [Errno 2] No such file or directory: 'data/tradrole.dta'

Detailed traceback: 
  File "<string>", line 1, in <module>
  File "/home/boris/anaconda3/lib/python3.7/site-packages/pandas/util/_decorators.py", line 188, in wrapper
    return func(*args, **kwargs)
  File "/home/boris/anaconda3/lib/python3.7/site-packages/pandas/util/_decorators.py", line 188, in wrapper
    return func(*args, **kwargs)
  File "/home/boris/anaconda3/lib/python3.7/site-packages/pandas/io/stata.py", line 186, in read_stata
    chunksize=chunksize)
  File "/home/boris/anaconda3/lib/python3.7/site-packages/pandas/util/_decorators.py", line 188, in wrapper
    return func(*args, **kwargs)
  File "/home/boris/anaconda3/lib/python3.7/site-packages/pandas/util/_decorators.py", line 188, in wrapper
    return func(*args, **kwargs)
  File "/home/boris/anaconda3/lib/python3.7/site-packages/pandas/io/stata.py", line 994, in __init__
    self.path_or_buf = open(path_or_buf, 'rb')
```


```python
plt.hist(data_nlsy['tradrole'])
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'data_nlsy' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
plt.title('')
plt.xlabel('Ответы респонденток')
plt.show('Вот такие дела, джентельмены :)')
```

<img src="04-multinom_choice_files/figure-html/hist tradrole py-1.png" width="672" />

Дальше тоже пока печаль :(

## stata




```stata
use data/pension.dta
```

``````


```stata
sum
```

```
    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
          id |        226    2445.093    1371.271         38       5014
      pyears |        218    11.38532    9.605498          0         45
     prftshr |        206    .2087379    .4073967          0          1
      choice |        226    .6150442     .487665          0          1
      female |        226    .6017699      .49062          0          1
-------------+---------------------------------------------------------
     married |        226    .7345133    .4425723          0          1
         age |        226    60.70354    4.287002         53         73
        educ |        219    13.51598    2.554627          8         18
      finc25 |        216    .2083333    .4070598          0          1
      finc35 |        216    .1851852      .38935          0          1
-------------+---------------------------------------------------------
      finc50 |        216    .2453704    .4313061          0          1
      finc75 |        216        .125    .3314871          0          1
     finc100 |        216    .1203704      .32615          0          1
     finc101 |        216    .0648148    .2467707          0          1
    wealth89 |        226    197.9057    242.0919   -579.997   1484.997
-------------+---------------------------------------------------------
       black |        226     .119469    .3250596          0          1
    stckin89 |        226    .3185841    .4669616          0          1
     irain89 |        226          .5    .5011099          0          1
     pctstck |        226    46.68142    39.44116          0        100
```



```stata
ren pctstck y
```

``````

Построим модель множественного выбора (лог-линейная модель). 

```stata
mlogit y choice age educ wealth89 prftshr,  baseoutcome(0) 
```

```
Iteration 0:   log likelihood = -219.86356  
Iteration 1:   log likelihood = -204.58172  
Iteration 2:   log likelihood =  -204.5078  
Iteration 3:   log likelihood = -204.50778  
Iteration 4:   log likelihood = -204.50778  

Multinomial logistic regression                 Number of obs     =        201
                                                LR chi2(10)       =      30.71
                                                Prob > chi2       =     0.0007
Log likelihood = -204.50778                     Pseudo R2         =     0.0698

------------------------------------------------------------------------------
           y |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
0            |  (base outcome)
-------------+----------------------------------------------------------------
50           |
      choice |   .6269473   .3706065     1.69   0.091    -.0994281    1.353323
         age |  -.1062189   .0434194    -2.45   0.014    -.1913193   -.0211185
        educ |   .1851821    .070641     2.62   0.009     .0467283    .3236359
    wealth89 |  -.0003717   .0007432    -0.50   0.617    -.0018283     .001085
     prftshr |  -.2718087   .4988312    -0.54   0.586      -1.2495    .7058825
       _cons |   3.777798   2.790118     1.35   0.176    -1.690732    9.246328
-------------+----------------------------------------------------------------
100          |
      choice |   .6244907   .3859169     1.62   0.106    -.1318925    1.380874
         age |  -.0948282   .0450488    -2.11   0.035    -.1831222   -.0065341
        educ |   .0464378   .0767858     0.60   0.545    -.1040595    .1969352
    wealth89 |  -.0003548    .000797    -0.45   0.656     -.001917    .0012074
     prftshr |   .9809114   .4396226     2.23   0.026      .119267    1.842556
       _cons |   4.493463   2.967396     1.51   0.130    -1.322526    10.30945
------------------------------------------------------------------------------
```

Кросс - табличка для объясняемой переменной и числа лет образования.

```stata
table y educ
```

```
0=mstbnds |
,50=mixed |
,100=msts |                     highest grade completed                     
tcks      |    8     9    10    11    12    13    14    15    16    17    18
----------+-----------------------------------------------------------------
        0 |    5     3           3    31     4     7          11     1     7
       50 |    1     1           3    34     4     6     2    14     5    14
      100 |          2     1     1    36     1     5     4     5     4     4
----------------------------------------------------------------------------
```

Можем посмотреть на прогнозы.

```stata
predict p1 p2 p3, p
```

```
(25 missing values generated)
```

И посчитать относительное изменение отношения шансов:

\[
\frac{P(y_{i} = j)}{P(y_{i} = 1)} = exp(x_{i}\beta)
\] - показывает изменение отношения шансов при выборе альтернативы j вместо альтернативы 0, если x изменился на единицу.
В stata, в отличие от R, отношение шансов называется relative-risk ratio.


```stata
mlogit, rrr
```

```
Multinomial logistic regression                 Number of obs     =        201
                                                LR chi2(10)       =      30.71
                                                Prob > chi2       =     0.0007
Log likelihood = -204.50778                     Pseudo R2         =     0.0698

------------------------------------------------------------------------------
           y |        RRR   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
0            |  (base outcome)
-------------+----------------------------------------------------------------
50           |
      choice |   1.871888   .6937337     1.69   0.091     .9053551    3.870264
         age |   .8992278   .0390439    -2.45   0.014     .8258688     .979103
        educ |   1.203438    .085012     2.62   0.009     1.047837    1.382144
    wealth89 |   .9996284   .0007429    -0.50   0.617     .9981733    1.001086
     prftshr |       .762   .3801094    -0.54   0.586     .2866481    2.025633
       _cons |   43.71966    121.983     1.35   0.176     .1843845    10366.43
-------------+----------------------------------------------------------------
100          |
      choice |   1.867295   .7206205     1.62   0.106     .8764352    3.978377
         age |   .9095292   .0409732    -2.11   0.035     .8326664    .9934872
        educ |   1.047533   .0804356     0.60   0.545     .9011717    1.217665
    wealth89 |   .9996452   .0007968    -0.45   0.656     .9980848    1.001208
     prftshr |   2.666886   1.172423     2.23   0.026     1.126671    6.312652
       _cons |   89.43064   265.3761     1.51   0.130     .2664612    30015.02
------------------------------------------------------------------------------
```


Можем посчитать предельные эффекты в разных точках.

```stata
margins, predict(outcome(0)) dydx(choice age educ wealth89 prftshr) atmeans 

margins, predict(outcome(0)) dydx(choice age educ wealth89 prftshr) at((p25) *)

margins, predict(outcome(0)) dydx(choice age educ wealth89 prftshr) at(age = 69, choice = 1)
```

```
Conditional marginal effects                    Number of obs     =        201
Model VCE    : OIM

Expression   : Pr(y==0), predict(outcome(0))
dy/dx w.r.t. : choice age educ wealth89 prftshr
at           : choice          =    .6069652 (mean)
               age             =    60.52736 (mean)
               educ            =    13.56219 (mean)
               wealth89        =    205.5467 (mean)
               prftshr         =    .2089552 (mean)

------------------------------------------------------------------------------
             |            Delta-method
             |      dy/dx   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
      choice |  -.1387657   .0717581    -1.93   0.053     -.279409    .0018776
         age |   .0224223   .0083071     2.70   0.007     .0061407     .038704
        educ |  -.0273084    .014011    -1.95   0.051    -.0547696    .0001527
    wealth89 |   .0000807   .0001452     0.56   0.578    -.0002039    .0003654
     prftshr |  -.0638897   .0905915    -0.71   0.481    -.2414458    .1136664
------------------------------------------------------------------------------


Conditional marginal effects                    Number of obs     =        201
Model VCE    : OIM

Expression   : Pr(y==0), predict(outcome(0))
dy/dx w.r.t. : choice age educ wealth89 prftshr
at           : choice          =           0 (p25)
               age             =          57 (p25)
               educ            =          12 (p25)
               wealth89        =        65.1 (p25)
               prftshr         =           0 (p25)

------------------------------------------------------------------------------
             |            Delta-method
             |      dy/dx   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
      choice |  -.1479315   .0837538    -1.77   0.077     -.312086    .0162229
         age |   .0239584   .0083078     2.88   0.004     .0076754    .0402413
        educ |  -.0298039   .0158695    -1.88   0.060    -.0609076    .0012998
    wealth89 |   .0000861   .0001542     0.56   0.576     -.000216    .0003883
     prftshr |   -.061837   .1037161    -0.60   0.551    -.2651169    .1414428
------------------------------------------------------------------------------

invalid 'asobserved' 
r(198);

end of do-file
r(198);
```



И вернемся к ~~сильным и независимым~~ моделькам упорядоченного выбора :) 

```stata
use data/tradrole.dta

sum
```

```
 invalid 'asobserved' 
r(198);


no; data in memory would be lost
r(4);

end of do-file
r(4);
```


```stata
hist tradrole 'Вот такие дела, джентельмены :)')
subtitle("Вот такие дела, джентельмены :)")
```

```
 invalid 'asobserved' 
r(198);


variable tradrole not found
r(111);

end of do-file
r(111);
```

Посмотрим, как влияет религиозное воспитание (`cath` - католичество и `fpro` - протестанство), число лет образования матери - `meduc` и проживание в крупном городе `urb` на объясняемую переменную.


```stata
oprobit tradrole i.cath i.fpro meduc i.urb
```

```
 invalid 'asobserved' 
r(198);


variable tradrole not found
r(111);

end of do-file
r(111);
```

