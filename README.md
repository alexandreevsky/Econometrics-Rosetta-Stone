# Книжка будет! :)

https://bdemeshev.github.io/Econometrics-Rosetta-Stone/


# Рабочий процесс

* Соавторы редактируют каждый свою подпапку.
* Готовые .Rmd переносятся копируем на одну папку выше
* Чистим руками готовые .Rmd. Пока, увы, так. Нужна чистка, чтобы компилировалась книжка на данной оси.
Чистая кросс-платформенная компиляция — дело грядущего :)
* Книжка должна рендериться в docs сама
* Коммитим на гитхаб и ура!
* Вся книжка рендерится в pdf/html/epub командой `rmarkdown::render_site(encoding = 'UTF-8')`

# Для настроек компиляции на трёх осях:

* Сделать три настроечных чанка кода в отдельной папке
* Определять тип оси или имя пользователя и загружать нужный чанк!


# Правила Виноделов!

1. После ### в заголовках разделов должен быть пробел.

### Глава о Главном — верно
###Глава о Главном — неверно

И в комментах к коду аналогично, после креста Cum Deo # — пробел!

2. Пути к файлам — только относительные.

Скажем, в вашей negodyay_work есть подпапка Images.
Тогда ссылайтейсь на неё как на Images/best_picture.png. 
По дефолту R все пути будет измерять от папки, в которой находится главный .Rmd
А не полным путём /Users/Negodyay/Desktop/...

3. Подпапки и файлы — без веских причин лучше заглавных букв не делать. 

Например, images лучше, чем Images. 
Давайте договоримся, что у каждого могут быть подпапки:

images — с готовыми (не создаваемыми из r/python/stata) картинками.
data — с файлами данных

Если нужны ещё какие, заявку в трёх экземплярах :) За три дня, сначала у Букина одобрить! :)

4. Каждый коан должен начинаться с заголовка первого уровня

# Коан о Дружбе Енотов и Поползней

Внутри коана должны быть заголовки второго уровня:

## r
## python
## stata

5. В рамках борьбе с дискриминацией названия всех программ пишем с маленькой буквы :)

6. В формулах не должно быть переноса строки \\. Кроме систем уравнений или матриц.

\[
a^2 + b^2 = c^2,
\]
где $a$ — коэффициент Величия!

Пример системы:
\[
\begin{cases}
a^2 = b \\
c^2 = d \\
\end{cases}
\]








# Прежние заметки

### Темы
1. Простая регрессия
2. Модели бинарного выбора
3. Модели упорядоченного выбора
4. Пуассоновская регрессия
5. Модель неупорядоченного выбора
6. Инструменты для простой регрессии
7. ARMA
8. Простые модели панельных данных (Random effect, fixed effect, pooled, difference/indiffence)
9. Гетероскедастичность в простой регрессии
10. МГК

**Продвинутые сюжеты**

1. Динамические панели (Arellano-Bond, Blundell-Bond)
2. TOBIT, HECKIT
3. Treatment effects

**И еще кое-что:**

+ раздел "Шишки и грабли"

+ Коан про русских панд (русские переменные, чтобы работали и на линухе, и на маке, и на винде). По-разному сохранять эксель, чтобы везде открылось

+ **Словарик!!!** с командами из статы в питон и в r + ссылки на примеры использования в буке

___

### План

1 шаг: сотавить схему хаоса, список статовских команд, в каких они файлах, что предложено в виде решения

2 шаг: разнести команды по коанам

3 шаг: начать писать коаны

**Прикольно называем коаны!**. Например, Притча о простой регрессии/коан

**Пишем стильно!** В R поможет *styler.*


Нужные пакеты в питоне - statsmodels, linearmodels.


___

### Хотим

1. Книжечка (букдаун)

2. Самый лучший коан сделать в формате плаката a1 и повесить в качестве агитки


