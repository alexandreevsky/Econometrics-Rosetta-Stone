# Коан об инcтрументах для простой регрессии {#instruments}
---
<style>
pre.r {
    background-color: #FEF9E7 !important;
}
pre.stata {
    background-color: #BDBDBD !important;
}
pre.python {
    background-color: #FDF2E9 !important;
}
</style> 

> Инструментальные переменные решают проблему эндогенности регрессоров. Так как эндогенность приводит к смещенности и несостоятельности МНК-оценок, коэффициенты регрессоров являются неточными. Вводя в анализ инструментальные переменные, коррелирующие с регрессором, эндогенность часто удается устранить.


Рассмотрим, как метод инструментальных переменных реализуется на r, python и в stata.

## r

Загружаем нужные пакеты:

```r
library(ggplot2) # Для построения графиков
library(gridExtra) # Для визуализации
library(dplyr) # Для работы с данными
library(skimr) # Для изучения данных
library(Ecdat) #  Для работы с IV
```

```
Error in library(Ecdat): there is no package called 'Ecdat'
```

```r
library(AER) #  Для работы с IV
library(ggcorrplot) # Для построения коррелограмы. Для установки: devtools::install_github("kassambara/ggcorrplot")
```

```
Error in library(ggcorrplot): there is no package called 'ggcorrplot'
```
Загружаем данные по продаже недвижимости **Housing**, изучаем их строение, визуализируем:

```r
skim(Housing)
```

```
Error in skim(Housing): object 'Housing' not found
```

```r
theme_set(theme_classic())

ggplot(data = Housing) + 
  scale_fill_brewer(palette = "Spectral") + 
  geom_histogram(mapping = aes(x = price), color = "gray2") +
  labs(title = "Распределение цены", y = "Количество наблюдений", x = "Цена дома (в $)")  
```

```
Error in ggplot(data = Housing): object 'Housing' not found
```

```r
ggplot(data = Housing) + 
  scale_fill_brewer(palette = "Spectral") + 
  geom_histogram(mapping = aes(x = bedrooms), fill = "tomato1") +
  labs(title = "Распределение количества кроватей", y = "Количество наблюдений", 
  x = "Количество кроватей")  
```

```
Error in ggplot(data = Housing): object 'Housing' not found
```

```r
ggplot(data = Housing) + 
  scale_fill_brewer(palette = "Spectral") + 
  geom_histogram(mapping = aes(x = lotsize), fill = "#FFDB6D", color = "#C4961A") +
  labs(title = "Распределение размера дома", y = "Количество наблюдений", 
  x = "Размер дома (в квадратных метрах)") 
```

```
Error in ggplot(data = Housing): object 'Housing' not found
```

Используем для предсказания цены дома `price` количество кроватей в нем `bedrooms`, а  площадь дома `lotsize` будем использовать как инструмент.

Убедимся, что между ними есть связь, посчитав корреляцию. Заодно построим нехитрую корреляционную матрицу для всех численных переменных:

```r
cor(Housing$lotsize, Housing$bedrooms) # Считаем связь между переменной и инструментом
```

```
Error in is.data.frame(y): object 'Housing' not found
```

```r
housing_numeric = select_if(Housing, is.numeric) # Оставляем только численные переменные
```

```
Error in tbl_vars_dispatch(x): object 'Housing' not found
```

```r
cor_housing = cor(housing_numeric)
```

```
Error in is.data.frame(x): object 'housing_numeric' not found
```

```r
ggcorrplot(cor_housing, hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           method = "circle",
           colors = c("tomato2", "white", "springgreen3"),
           title = "Корреляция переменных",
           ggtheme = theme_bw)
```

```
Error in ggcorrplot(cor_housing, hc.order = TRUE, type = "lower", lab = TRUE, : could not find function "ggcorrplot"
```

Действительно, между выбранными нами переменной и инструментом наблюдается корреляция, которая составляет 0.15.

Построим IV-регрессию, реализующую двухшаговый МНК, с помощью функции *ivreg*:

```r
iv1 = ivreg(price ~ bedrooms | lotsize, data = Housing)
```

```
Error in terms.formula(form, ...): object 'Housing' not found
```

```r
summary(iv1)
```

```
Error in summary(iv1): object 'iv1' not found
```
Обратите внимание на синтаксис функции - он схож с простой регрессией. Первая переменная - зависимая, после знака `~` указываются регрессоры. После вертикальной черты `|` указываются инструментальные переменные. В поле *data* мы ссылаемся на данные, которые анализируем.

Мы видим, что влияние размера дома на цену значительно снизилось после использования инструментальной переменной. Значит, размер дома и количество кроватей в нем тесно связаны.

Чтобы полностью в этом убедиться, проведем тесты.

```r
summary(iv1, vcov = sandwich, diagnostics = TRUE)
```

```
Error in summary(iv1, vcov = sandwich, diagnostics = TRUE): object 'iv1' not found
```
Смотрим на раздел *Diagnostic tests*. Нас интересует тест на слабость инструментов, *Weak instruments*. Нулевая гипотеза заключается в том, что выбранный инструмент слабый. На любом разумном уровне значимости мы можем говорить о том, что гипотеза отвергается, а значит, выбранный инструмент неслабый. Следующий тест, *Wu-Hausman*, проверяет основную гипотезу о постоянстве МНК-оценок. Так как мы отвергаем основную гипотезу на любом разумном уровне значимости, оценки МНК не постоянны, то есть, присутствует эндогенность и МНК-оценки смещены, поэтому использование инструментов оправданно. *Тест Sargan*, проверяющий экзогенность инструментов, может применяться только в том случае, если количество инструментов превышает количество эндогенных переменных. В нашем случае данный тест не применим, так как мы выбрали только одну инструментальную переменную для одного регрессора. 

Добавим еще одну инструментальную переменную, `bathrms`, содержащую информацию о количестве ванных комнат, и проведем *тест Sargan*.

```r
iv2 = ivreg(price ~ bedrooms | lotsize + bathrms, data = Housing)
```

```
Error in terms.formula(form, ...): object 'Housing' not found
```

```r
summary(iv2)
```

```
Error in summary(iv2): object 'iv2' not found
```

```r
summary(iv2, vcov = sandwich, diagnostics = TRUE)
```

```
Error in summary(iv2, vcov = sandwich, diagnostics = TRUE): object 'iv2' not found
```
Нулевая гипотеза в *тесте Sargan* отвергается тогда, когда хотя бы один из инструментов не подходит. Так как нулевая гипотеза не отвергается на любом разумном уровне значимости, наши инструменты были подобраны хорошо.

## stata
Будем работать с набором данных, содержащих информацию о зарплатах. Загрузим данные, посмотрим их строение.

```stata
webuse educwages
describe
```

```
Contains data from http://www.stata-press.com/data/r14/educwages.dta
  obs:         1,000                          
 vars:             5                          11 Sep 2014 13:36
 size:        20,000                          
-------------------------------------------------------------------------------
              storage   display    value
variable name   type    format     label      variable label
-------------------------------------------------------------------------------
wages           float   %9.0g                 Annual wages (USD)
union           float   %9.0g      union      Union membership
education       float   %9.0g                 Education (years)
meducation      float   %9.0g                 Mother's education (years)
feducation      float   %9.0g                 Father's education (years)
-------------------------------------------------------------------------------
Sorted by: 
```
Проведем двухшаговый МНК с помощью функции *ivregress*. В качестве зависимой переменной djpmмем зарплату (`wages`), в качестве регрессора - образование (`education`). В качестве инструментов для образования используем образование отца (`feducation`), образование матери (`meducation`) и участие в профсоюзе (`union`).

```stata
ivregress 2sls wages union (education = meducation feducation) 
```

```
Instrumental variables (2SLS) regression          Number of obs   =      1,000
                                                  Wald chi2(2)    =    3738.34
                                                  Prob > chi2     =     0.0000
                                                  R-squared       =     0.8599
                                                  Root MSE        =      1.018

------------------------------------------------------------------------------
       wages |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
   education |   .9700481   .0177091    54.78   0.000     .9353389    1.004757
       union |   1.930183   .0644746    29.94   0.000     1.803815    2.056551
       _cons |   30.55263   .2882409   106.00   0.000     29.98769    31.11757
------------------------------------------------------------------------------
Instrumented:  education
Instruments:   union meducation feducation
```
Проведем тесты на качество подбора инструментов.

```stata
estat endog
```

```
  Tests of endogeneity
  Ho: variables are exogenous

  Durbin (score) chi2(1)          =  460.681  (p = 0.0000)
  Wu-Hausman F(1,996)             =  850.772  (p = 0.0000)
```
Основная гипотеза об экзогенности отвергается, что означает, что выбранные инструменты эндогенны. *Тест Durbin* и *тест Wu-Hausman* также показывают, что выбранные инструменты подходят.

Проведем *тесты Sargan и Basmann*.

```stata
estat overid
```

```
  Tests of overidentifying restrictions:

  Sargan (score) chi2(1) =  .127213  (p = 0.7213)
  Basmann chi2(1)        =  .126721  (p = 0.7219)
```
Нулевая гипотеза в *тесте Sargan* и *тесте Basmann на сверхидентифицирующие ограничения* отвергается тогда, когда хотя бы один из инструментов не подходит. Так как нулевая гипотеза не отвергается на любом разумном уровне значимости, наши инструменты были подобраны хорошо.

## python

Разберем применение метода инструментальных переменных на python.
Загружаем необходимые библиотеки:

```python
import pandas as pd # Библиотека pandas для работы с данными
import seaborn as sns # Библиотека seaborn для визуализации
import matplotlib.pyplot as plt # Библиотека matplotlib для визуализации
import statsmodels.api as sm # Для двухшагового МНК
```
Загружаем данные и исследуем их строение:

```python
url = "https://raw.githubusercontent.com/arunk13/MSDA-Assignments/master/IS607Fall2015/Assignment3/student-mat.csv"
df = pd.read_csv(url, 
                header=0,
                sep = ";") 
print(df.head(2))
```

```
  school sex  age address famsize Pstatus  ...  Walc  health absences G1 G2 G3
0     GP   F   18       U     GT3       A  ...     1       3        6  5  6  6
1     GP   F   17       U     GT3       T  ...     1       3        4  5  5  6

[2 rows x 33 columns]
```
Изучим зависимость успеваемости ученика `G3` от времени, которое он тратит на учебу `studytime` и его свободного времени `freetime`, а в качестве инструмента для свободного времени используем `traveltime`, время, которое он тратит на поездку до учебы. Логика здесь такая: чем больше времени ученик тратит на дорогу, тем меньше у него времени остается на учебу, а значит, меньше вероятность получить хорошую оценку. Преобразуем выбранные переменные:

```python
grade = df.loc[:, 'G3']
freetime = df.loc[:, 'freetime']
traveltime = df.loc[:, 'traveltime']
studtytime = df.loc[:, 'studytime']
```

Проверим корреляцию переменной `freetime` и подобранного инструмента `traveltime`, построив диаграмму корреляции всех признаков:

```python
colormap = plt.cm.viridis 
corr = df.corr()
plt.figure(figsize=(12,12))
plt.title('Корреляционная матрица признаков', y=1.02, size=15)
sns.heatmap(corr,linewidths=0.1,vmax=1.0, square=True, cmap=colormap, linecolor='white', annot=True)
```

<img src="07-instruments_files/figure-html/unnamed-chunk-14-1.png" width="1152" />

Корреляция между переменными составляет 0.1, а корреляция инструмента с целевой переменной -0.12, что, в принципе, нас вполне устраивает.

Проведем двухшаговый МНК при помощи функции IV2SLS из пакета **linearmodels**. Чтобы использовать ее, надо установить пакет **linearmodels**. Для этого, если он еще не установлен, нужно выполнить следующую команду в командной строке: `pip install linearmodels`. Убедившись, что она установлена, импортируем нужные библиотеки и приступаем к двухшаговому МНК:

```python
from linearmodels import IV2SLS
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): ModuleNotFoundError: No module named 'linearmodels'

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
ivmod = IV2SLS.from_formula('grade ~ 1 + studytime  + [freetime ~ traveltime]', df)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'IV2SLS' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
res_2sls = ivmod.fit()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'ivmod' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```

```python
print(res_2sls.summary)
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'res_2sls' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```
Проверим качество подбора инструментов с помощью тестов.
*Durbin тест*:

```python
res_2sls.durbin()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'res_2sls' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```
*Wu-Hausman тест*:

```python
res_2sls.wu_hausman()
```

```
Error in py_call_impl(callable, dots$args, dots$keywords): NameError: name 'res_2sls' is not defined

Detailed traceback: 
  File "<string>", line 1, in <module>
```
Основная гипотеза об экзогенности инструментов в обоих тестах отвергается, значит, мы выбрали подходящие инструменты.
