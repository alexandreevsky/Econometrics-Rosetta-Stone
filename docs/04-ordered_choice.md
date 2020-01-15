# Модели упорядоченного выбора и условный логит {#ordchoice}




Загрузим необходимые пакеты.

```r
library(tidyverse) # для манипуляций с данными и построения графиков
library(skimr) #для красивого summary
library(rio) # для чтения .dta файлов
library(margins)
library(mlogit)
library(nnet)
library(questionr)
library(MASS)
library(survival)

# log(6)
```

Импортируем датасет. В нем находятся данные по клиентам пенсионных фондов. Нас интересует переменная `pctstck`, которая принимает три значения: 0, 50, 100 - в зависимоcти от ответа респондента на вопрос о предпочтительном способе инвестирования пенсионных накоплений.   

```r
df = rio::import("pension.dta")
```


```r
skim_with(numeric = list(hist = NULL, p25 = NULL, p75 = NULL)) #посмотрим на данные
#skim(df)
```


Создадим факторную перменную и упорядочим категории. 


```r
df = rename(df,  alloc = pctstck) # переименуем 
df = mutate(df, alloc_factor = factor(alloc)) # факторная переменная
df = mutate(df, y = relevel(df$alloc_factor, ref = 1)) # сменить базовую категорию
levels(df$y)
```

```
[1] "0"   "50"  "100"
```

Построим модель множественного выбора (лог-линейная модель). 

```r
multmodel = multinom(y ~ choice+age+educ+wealth89+prftshr, data = df)
```

```
# weights:  21 (12 variable)
initial  value 220.821070 
iter  10 value 207.012642
iter  20 value 204.507792
final  value 204.507779 
converged
```

```r
summary(multmodel)
```

```
Call:
multinom(formula = y ~ choice + age + educ + wealth89 + prftshr, 
    data = df)

Coefficients:
    (Intercept)    choice         age       educ      wealth89    prftshr
50     3.777686 0.6269410 -0.10621691 0.18518113 -0.0003716626 -0.2717872
100    4.492971 0.6244954 -0.09482129 0.04644315 -0.0003548369  0.9809245

Std. Errors:
    (Intercept)    choice        age       educ     wealth89   prftshr
50     1.581691 0.3701263 0.02826469 0.06725443 0.0007365833 0.4988234
100    1.385291 0.3851273 0.02530600 0.07203058 0.0007896235 0.4396202

Residual Deviance: 409.0156 
AIC: 433.0156 
```

Сохраним прогнозы.

```r
fit_values = fitted(multmodel)
head(fit_values)
```

```
          0        50       100
1 0.4040703 0.3308134 0.2651163
2 0.1534943 0.2619464 0.5845593
3 0.1651913 0.2342525 0.6005562
4 0.4300671 0.1504960 0.4194370
5 0.4878942 0.2797337 0.2323721
6 0.4642700 0.1265789 0.4091510
```

И посчитать относительное изменение отношения шансов:

\[
\frac{P(y_{i} = j)}{P(y_{i} = 1)} = exp(x_{i}\beta)
\] 
показывает изменение отношения шансов при выборе альтернативы j вместо альтернативы 0, если x изменился на единицу

```r
odds.ratio(multmodel) # отношение шансов в stata называется relative-risk ratio
```

```
                      OR    2.5 %    97.5 %         p    
50/(Intercept)  43.71476  1.96920  970.4342 0.0169227 *  
50/choice        1.87188  0.90620    3.8666 0.0902925 .  
50/age           0.89923  0.85077    0.9505 0.0001713 ***
50/educ          1.20344  1.05481    1.3730 0.0058972 ** 
50/wealth89      0.99963  0.99819    1.0011 0.6138563    
50/prftshr       0.76202  0.28666    2.0256 0.5858522    
100/(Intercept) 89.38659  5.91713 1350.3111 0.0011814 ** 
100/choice       1.86730  0.87780    3.9722 0.1049041    
100/age          0.90954  0.86552    0.9558 0.0001790 ***
100/educ         1.04754  0.90961    1.2064 0.5190763    
100/wealth89     0.99965  0.99810    1.0012 0.6531613    
100/prftshr      2.66692  1.12669    6.3127 0.0256613 *  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


Можем посчитать предельные эффекты в различных квартилях. 

```r
summary(marginal_effects(multmodel)) # mean как в стате
```

```
  dydx_choice          dydx_age         dydx_educ        
 Min.   :-0.15646   Min.   :0.00887   Min.   :-0.036761  
 1st Qu.:-0.15043   1st Qu.:0.01777   1st Qu.:-0.029252  
 Median :-0.12909   Median :0.02075   Median :-0.025701  
 Mean   :-0.12697   Mean   :0.02049   Mean   :-0.024735  
 3rd Qu.:-0.10976   3rd Qu.:0.02411   3rd Qu.:-0.020634  
 Max.   :-0.05576   Max.   :0.02562   Max.   :-0.009214  
 dydx_wealth89        dydx_prftshr      
 Min.   :3.225e-05   Min.   :-0.177629  
 1st Qu.:6.389e-05   1st Qu.:-0.075981  
 Median :7.515e-05   Median :-0.056485  
 Mean   :7.385e-05   Mean   :-0.060746  
 3rd Qu.:8.726e-05   3rd Qu.:-0.023855  
 Max.   :9.123e-05   Max.   :-0.002558  
```



Допустим, мы можем упорядочить наши альтернативы (например, от более рискованного способа распределения ресурсов до менее)

```r
ordered_logit = polr(y ~ choice+age+educ+wealth89+prftshr , data = df)
ordered_probit = polr(y ~ choice+age+educ+wealth89+prftshr , data = df, method = 'probit') 

fit_prob = fitted(ordered_probit)
fit_log = fitted(ordered_logit)
ordered_probit
```

```
Call:
polr(formula = y ~ choice + age + educ + wealth89 + prftshr, 
    data = df, method = "probit")

Coefficients:
       choice           age          educ      wealth89       prftshr 
 0.2932276690 -0.0453064786  0.0269376562 -0.0001693805  0.4864824791 

Intercepts:
     0|50    50|100 
-2.578050 -1.561799 

Residual Deviance: 425.7763 
AIC: 439.7763 
(25 observations deleted due to missingness)
```

```r
ln(5)
```

```
Error in ln(5): could not find function "ln"
```



```r
cond_logit = clogit(y ~ choice+age+strata(educ)+wealth89+prftshr , data = df)
```

```
Error in coxph(formula = Surv(rep(1, 226L), y) ~ choice + age + strata(educ) + : Cox model doesn't support "mright" survival data
```

### То же самое в стате



```stata
use pension.dta
```

```
end of do-file
```


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
ren pctstck alloc
```

``````

Построим модель множественного выбора (лог-линейная модель). 

```stata
mlogit alloc choice age educ wealth89 prftshr,  baseoutcome(0) #маленькое отличие с R
```

```
> ичие с R
option # not allowed
r(198);

end of do-file
r(198);
```

Можем посмотреть на прогнозы.

```stata
predict p1 p2 p3, p
```

```
 option # not allowed
r(198);


last estimates not found
r(301);

end of do-file
r(301);
```

И посчитать относительное изменение отношения шансов:

\[
\frac{P(y_{i} = j)}{P(y_{i} = 1)} = exp(x_{i}\beta)
\] - показывает изменение отношения шансов при выборе альтернативы j вместо альтернативы 0, если x изменился на единицу


```stata
mlogit, rrr #relative-risk ratio
```

```
 option # not allowed
r(198);


last estimates not found
r(301);

end of do-file
r(301);
```


Можем посчитать предельные эффекты в разных точках.

```stata
margins, predict(outcome(50)) dydx( choice age educ wealth89 prftshr) atmeans 

margins, predict(outcome(50)) dydx( choice age educ wealth89 prftshr) at((p25) *)
```

```
 option # not allowed
r(198);


last estimates not found
r(301);

end of do-file
r(301);
```


```stata
oprobit alloc choice age educ wealth89 prftshr

ologit alloc choice age educ wealth89 prftshr
```

```
 option # not allowed
r(198);



Iteration 0:   log likelihood = -219.86356  
Iteration 1:   log likelihood = -212.89234  
Iteration 2:   log likelihood = -212.88817  
Iteration 3:   log likelihood = -212.88817  

Ordered probit regression                       Number of obs     =        201
                                                LR chi2(5)        =      13.95
                                                Prob > chi2       =     0.0159
Log likelihood = -212.88817                     Pseudo R2         =     0.0317

------------------------------------------------------------------------------
       alloc |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
      choice |   .2932272    .167064     1.76   0.079    -.0342122    .6206666
         age |  -.0453065   .0195009    -2.32   0.020    -.0835275   -.0070854
        educ |   .0269375   .0315643     0.85   0.393    -.0349273    .0888024
    wealth89 |  -.0001694   .0003431    -0.49   0.622    -.0008419    .0005031
     prftshr |   .4864833   .2030406     2.40   0.017      .088531    .8844355
-------------+----------------------------------------------------------------
       /cut1 |  -2.578052   1.277878                     -5.082648   -.0734562
       /cut2 |  -1.561798   1.272756                     -4.056353    .9327576
------------------------------------------------------------------------------


Iteration 0:   log likelihood = -219.86356  
Iteration 1:   log likelihood = -212.75117  
Iteration 2:   log likelihood = -212.72813  
Iteration 3:   log likelihood = -212.72813  

Ordered logistic regression                     Number of obs     =        201
                                                LR chi2(5)        =      14.27
                                                Prob > chi2       =     0.0140
Log likelihood = -212.72813                     Pseudo R2         =     0.0325

------------------------------------------------------------------------------
       alloc |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
      choice |   .4720438   .2757545     1.71   0.087     -.068425    1.012513
         age |  -.0776337   .0328659    -2.36   0.018    -.1420497   -.0132177
        educ |   .0475714   .0514763     0.92   0.355    -.0533203    .1484631
    wealth89 |   -.000277    .000561    -0.49   0.621    -.0013765    .0008224
     prftshr |   .8312158   .3506528     2.37   0.018     .1439489    1.518483
-------------+----------------------------------------------------------------
       /cut1 |  -4.376271   2.144494                     -8.579402   -.1731395
       /cut2 |  -2.714186   2.129423                     -6.887779    1.459407
------------------------------------------------------------------------------
```



Посмотрим на conditional logit

ПОКА ЗАБИЛА


```stata

use crackers.dta


egen resp = group(id occ)

tabulate brand, generate(br)
rename br1 Sunshine
rename br2 Keebler
rename br3 Nabisco

clogit choice Sunshine Keebler Nabisco display feature price, group(resp)

```

```
 option # not allowed
r(198);


no; data in memory would be lost
r(4);

end of do-file
r(4);
```
