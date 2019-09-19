## タイトル

```{R}

library(plotly)
library(psych)
library(plotly)
library(skimr)



# 出力したCSVデータを読み込めます
bank_marketing_train <- read.csv("bank_marketing_train.csv")

table(bank_marketing_train$marital)
p = plot_ly(x = bank_marketing_train$y, type="histogram")
describe(bank_marketing_train)
skimr::skim(bank_marketing_train)
hist(bank_marketing_train$age)
plot_ly(x = bank_marketing_train$age, type = "histogram", color = bank_marketing_train$y)


```

## 課題1

```{R}


```

## 課題2

```{R}


```