## Basic Step Statistics Project Based Learning
## Bank Marketing

# 前準備（データ読み込み、ライブラリのインポート） ------------------------------------------------

library("psych")
library("skimr")
library("plotly")

# 出力したCSVデータを読み込めます
bank_marketing_train <- read.csv("bank_marketing_train.csv")

head(bank_marketing_train)


# 1.ターゲットのペルソナを検討する -------------------------------------------------------


# y=yes/noのデータを抽出してみる
bank_marketing_train_y <- bank_marketing_train[bank_marketing_train$y=="yes",]
bank_marketing_train_n <- bank_marketing_train[bank_marketing_train$y=="no",]
summary(bank_marketing_train_y)
summary(bank_marketing_train_n)

# データ数
num_yes = 3796
num_no = 29948

# ヒストグラム

# 年齢
plot_ly(x = bank_marketing_train_y$age, type="histogram")
plot_ly(x = bank_marketing_train_n$age, type="histogram")
plot_ly(x = bank_marketing_train$age, type="box", color = bank_marketing_train$y)
# => yesの方が、60以上が多い

# 職業
#pl_job = plot_ly(x = bank_marketing_train_y$job, type="histogram")
#plot_ly(x = bank_marketing_train_n$job, type="histogram")
# add_trace(p = pl_job, x = bank_marketing_train_n$job, type="histogram")
plot_ly(x = bank_marketing_train$job, type="box", color = bank_marketing_train$y)
plot_ly(x = bank_marketing_train$job, type="histogram", color = bank_marketing_train$y)
# 割合をみてみる
summary(bank_marketing_train_y$job)/num_yes
summary(bank_marketing_train_n$job)/num_no
# => yesの方が、retired/studentが多く、blue-colorが少ない

# 婚姻状況
plot_ly(x = bank_marketing_train$marital, type="histogram", color = bank_marketing_train$y)
plot_ly(x = bank_marketing_train$marital, type="box", color = bank_marketing_train$y)
# => 差はなさそう

# クレジットの支払遅延
plot_ly(x = bank_marketing_train$default, type="histogram", color = bank_marketing_train$y)
plot_ly(x = bank_marketing_train$default, type="box", color = bank_marketing_train$y)
skimr::skim(bank_marketing_train_y$default)
skimr::skim(bank_marketing_train_n$default)
# => 差はなさそう

# 最終学歴
plot_ly(x = bank_marketing_train$education, type="histogram", color = bank_marketing_train$y)
plot_ly(x = bank_marketing_train$education, type="box", color = bank_marketing_train$y)
# 割合をみてみる
summary(bank_marketing_train_y$education)/num_yes
summary(bank_marketing_train_n$education)/num_no
# => yesはuniversity.degreeが多い

# 不動産ローンの有無
plot_ly(x = bank_marketing_train$housing, type="histogram", color = bank_marketing_train$y)
plot_ly(x = bank_marketing_train$housing, type="box", color = bank_marketing_train$y)
# 割合をみてみる
summary(bank_marketing_train_y$housing)/num_yes
summary(bank_marketing_train_n$housing)/num_no
# => 差はなさそう

# 個人ローンの有無
plot_ly(x = bank_marketing_train$loan, type="histogram", color = bank_marketing_train$y)
plot_ly(x = bank_marketing_train$loan, type="box", color = bank_marketing_train$y)
# 割合をみてみる
summary(bank_marketing_train_y$loan)/num_yes
summary(bank_marketing_train_n$loan)/num_no
# => 差はなさそう

# 連絡デバイス
plot_ly(x = bank_marketing_train$contact, type="histogram", color = bank_marketing_train$y)
plot_ly(x = bank_marketing_train$contact, type="box", color = bank_marketing_train$y)
# 割合をみてみる
summary(bank_marketing_train_y$contact)/num_yes
summary(bank_marketing_train_n$contact)/num_no
# => yesはcellularが多い

# 前回の接触からの経過日数
plot_ly(x = bank_marketing_train$pdays, type="histogram", color = bank_marketing_train$y)
plot_ly(x = bank_marketing_train$pdays, type="box", color = bank_marketing_train$y)
# 割合をみてみる
summary(bank_marketing_train_y$pdays)
summary(bank_marketing_train_n$pdays)
# => 差はなさそう

# 以前のキャンペーン結果
plot_ly(x = bank_marketing_train$poutcome, type="histogram", color = bank_marketing_train$y)
plot_ly(x = bank_marketing_train$poutcome, type="box", color = bank_marketing_train$y)
# 割合をみてみる
summary(bank_marketing_train_y$poutcome)/num_yes
summary(bank_marketing_train_n$poutcome)/num_no
# => yesはsuccessが多い(全体の割合としては2割だが、noは0.1割くらいなのでyesとnoの差はある)

# 以前のキャンペーンの接触回数
plot_ly(x = bank_marketing_train$previous, type="histogram", color = bank_marketing_train$y)
plot_ly(x = bank_marketing_train$previous, type="box", color = bank_marketing_train$y)
# 割合をみてみる
cut(bank_marketing_train_y$previous, breaks = c(0,1,2,3,4,5,6))
summary(bank_marketing_train_y$previous)
summary(bank_marketing_train_n$previous)
# => yesは平均値が大きい(yes:0.48, no:0.13)しかし、この説明変数がどれだけ有効なのかは想像つかない

# 定性的な仮説
# -年齢：入社後の22歳ごろと退職後の60歳ごろはyesが増えそう→60歳は合っている
# -職業：student、unemployedはyesが少なそう→外れている。studentは逆。
# -婚姻状況：divorced（離婚）はyesが少なそう→外れ。傾向なし
# -クレジットの支払遅延：無しはyesが多そう→外れ。傾向なし
# -最終学歴：調べられなかった
# -不動産ローンの有無：無しはyesが多そう→外れ。傾向なし
# -個人ローンの有無：無しはyesが多そう→外れ。傾向なし
# -連絡デバイス：関係なさそう→外れ。yesはcellularが多い
# -前回の接触からの経過日数：短い方がyesが多そう（担当者を覚えている）→外れ。傾向なし
# -以前のキャンペーン結果：successがyesが多そう（継続してくれるのでは）→当たり
# -以前のキャンペーンの接触回数：数が多い方がyesが多そう（担当者を覚えている）→当たり


# ロジスティック回帰で各説明変数を見る

#学習データとテストデータに分割しておく（あとで予測のデモのため）
#train_idx<-sample(c(1:dim(bank_marketing_train)[1]), size = dim(bank_marketing_train)[1]*0.7)
#train<-bank_marketing_train[train_idx, ]
#test<-bank_marketing_train[-train_idx, ]

## ロジスティック回帰
## 一般化線形モデルなのでGeneral Legression Model(glm)
lr<-glm(y~age+job+marital+default+education+housing+
          loan+contact+day_of_week+pdays+poutcome+previous,
        data=bank_marketing_train, family="binomial")

## 線形回帰と同じようにsummaryで各種統計値が見れます。
summary(lr)

## step関数
lr2 <- step(lr)
AIC(lr2)
summary(lr2)

# ペルソナの推定 -----------------------------------------------------------------

# Age:60以上?
# Job:retired
# => Job:retiredの条件で、データを絞ってみてみる
bank_marketing_train_job_retired <- bank_marketing_train[bank_marketing_train$job == "retired",]
summary(bank_marketing_train_job_retired)


# y=yes/noのデータを抽出してみる
bank_marketing_train_job_retired_y <- bank_marketing_train_job_retired[bank_marketing_train_job_retired$y=="yes",]
bank_marketing_train_job_retired_n <- bank_marketing_train_job_retired[bank_marketing_train_job_retired$y=="no",]
summary(bank_marketing_train_job_retired_y)
summary(bank_marketing_train_job_retired_n)

# データ数
num_retired_yes = 358
num_retired_no = 1074

# ヒストグラム

# 年齢
plot_ly(x = bank_marketing_train_job_retired_y$age, type="histogram")
plot_ly(x = bank_marketing_train_job_retired_n$age, type="histogram")
plot_ly(x = bank_marketing_train_job_retired$age, type="histogram", color = bank_marketing_train_job_retired$y)
plot_ly(x = bank_marketing_train_job_retired$age, type="box", color = bank_marketing_train_job_retired$y)
# => yesの方が、60以上が多い

# 婚姻状況
plot_ly(x = bank_marketing_train_job_retired$marital, type="histogram", color = bank_marketing_train_job_retired$y)
plot_ly(x = bank_marketing_train_job_retired$marital, type="box", color = bank_marketing_train_job_retired$y)
# 割合をみてみる
summary(bank_marketing_train_job_retired_y$marital)/num_retired_yes
summary(bank_marketing_train_job_retired_n$marital)/num_retired_no
# => yesはsingleが少ない

# クレジットの支払遅延
plot_ly(x = bank_marketing_train_job_retired$default, type="histogram", color = bank_marketing_train_job_retired$y)
plot_ly(x = bank_marketing_train_job_retired$default, type="box", color = bank_marketing_train_job_retired$y)
# 割合をみてみる
summary(bank_marketing_train_job_retired_y$default)/num_retired_yes
summary(bank_marketing_train_job_retired_n$default)/num_retired_no
# => yesはunknownが少なく、9割が"no"

# 最終学歴
#plot_ly(x = bank_marketing_train_job_retired$education, type="histogram", color = bank_marketing_train_job_retired$y)
#plot_ly(x = bank_marketing_train_job_retired$education, type="box", color = bank_marketing_train_job_retired$y)
pl_yes <- plot_ly(x = bank_marketing_train_job_retired_y$education, type="histogram", name = "yes")
pl_no <- plot_ly(x = bank_marketing_train_job_retired_n$education, type="histogram", name = "no")
subplot(pl_yes, pl_no)
# 割合をみてみる
summary(bank_marketing_train_job_retired_y$education)/num_retired_yes
summary(bank_marketing_train_job_retired_n$education)/num_retired_no
# => yesはbasic.4y, illiterate(学歴が高くない)が多い 

# 不動産ローンの有無
pl_yes <- plot_ly(x = bank_marketing_train_job_retired_y$housing, type="histogram", name = "yes")
pl_no <- plot_ly(x = bank_marketing_train_job_retired_n$housing, type="histogram", name = "no")
subplot(pl_yes, pl_no)
# 割合をみてみる
summary(bank_marketing_train_job_retired_y$housing)/num_retired_yes
summary(bank_marketing_train_job_retired_n$housing)/num_retired_no
# => 大きな差はない（yesは少しローン有が多い）

# 個人ローンの有無
plot_ly(x = bank_marketing_train_job_retired$loan, type="histogram", color = bank_marketing_train_job_retired$y)
pl_yes <- plot_ly(x = bank_marketing_train_job_retired_y$loan, type="histogram", name = "yes")
pl_no <- plot_ly(x = bank_marketing_train_job_retired_n$loan, type="histogram", name = "no")
subplot(pl_yes, pl_no)
# 割合をみてみる
summary(bank_marketing_train_job_retired_y$loan)/num_retired_yes
summary(bank_marketing_train_job_retired_n$loan)/num_retired_no
# => 差はなさそう

# 連絡デバイス
plot_ly(x = bank_marketing_train_job_retired$contact, type="histogram", color = bank_marketing_train_job_retired$y)
pl_yes <- plot_ly(x = bank_marketing_train_job_retired_y$contact, type="histogram", name = "yes")
pl_no <- plot_ly(x = bank_marketing_train_job_retired_n$contact, type="histogram", name = "no")
subplot(pl_yes, pl_no)
# 割合をみてみる
summary(bank_marketing_train_job_retired_y$contact)/num_retired_yes
summary(bank_marketing_train_job_retired_n$contact)/num_retired_no
# => yesはcellularが多い

# 前回の接触からの経過日数
#plot_ly(x = bank_marketing_train_job_retired$pdays, type="histogram", color = bank_marketing_train_job_retired$y)
pl_yes <- plot_ly(x = bank_marketing_train_job_retired_y$pdays, type="histogram", name = "yes")
pl_no <- plot_ly(x = bank_marketing_train_job_retired_n$pdays, type="histogram", name = "no")
subplot(pl_yes, pl_no)
# 割合をみてみる
summary(bank_marketing_train_job_retired_y$pdays)
summary(bank_marketing_train_job_retired_n$pdays)
bank_marketing_train_job_retired_y
# => yesは日数が短い人の割合が大きい

# 以前のキャンペーン結果
#plot_ly(x = bank_marketing_train_job_retired$poutcome, type="histogram", color = bank_marketing_train_job_retired$y)
pl_yes <- plot_ly(x = bank_marketing_train_job_retired_y$poutcome, type="histogram", name = "yes")
pl_no <- plot_ly(x = bank_marketing_train_job_retired_n$poutcome, type="histogram", name = "no")
subplot(pl_yes, pl_no)
# 割合をみてみる
summary(bank_marketing_train_job_retired_y$poutcome)/num_retired_yes
summary(bank_marketing_train_job_retired_n$poutcome)/num_retired_no
# => yesはfailure, successが多い

# 以前のキャンペーンの接触回数
pl_yes <- plot_ly(x = bank_marketing_train_job_retired_y$previous, type="histogram", name = "yes")
pl_no <- plot_ly(x = bank_marketing_train_job_retired_n$previous, type="histogram", name = "no")
subplot(pl_yes, pl_no)
# 割合をみてみる
summary(bank_marketing_train_job_retired_y$previous)
summary(bank_marketing_train_job_retired_n$previous)
# => yesは平均値が大きい(yes:0.65, no:0.20)しかし、この説明変数がどれだけ有効なのかは想像つかない

# 最終的なペルソナ
# age:60以上
# job:retired
# marital：結婚経験あり
# default(クレジットの支払い遅延)：なし
# education(最終学歴)：basic.4y
# contact(連絡デバイス)：cellular
# pdays（前回の接触からの経過日数）：少ない
# poutcome（以前のキャンペーン結果）：あり（初めての客でない）

# TODO やってみたいこと
# 特徴量の作成
# ローンの有無（不動産ローン、個人ローンを合わせたもの）

# 2.予測モデルを用いたアタックリストを作成する

# 人に依存しない説明変数も含め、すべての変数に対して統計値を確認しておく
# ただしday_of_week, duration, campaignは架電後の説明変数と考え、除去する
skimr::skim(bank_marketing_train)


lr3<-glm(y~.-day_of_week-duration-campaign,
        data=bank_marketing_train, family="binomial")

summary(lr3)

## step関数
lr4 <- step(lr3)
AIC(lr4)
summary(lr4)
#summary(lr2)
#summary(lr3)

# ここで、ageなどの説明変数の重要性が減ってしまうのは、
# emp.var.rateなどの説明変数の影響が大きいためと思われる
# ペルソナを定義するのに使用した説明変数と、それ以外で重要な説明変数を用いて
# モデリングをおこなう方針とする
# (emp.var.rate, cons.price.idx, cons.conf.idx を追加する)

# 続きはPythonで行う

