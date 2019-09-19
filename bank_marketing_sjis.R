## Basic Step Statistics Project Based Learning
## Bank Marketing

# �O�����i�f�[�^�ǂݍ��݁A���C�u�����̃C���|�[�g�j ------------------------------------------------

library("psych")
library("skimr")
library("plotly")

# �o�͂���CSV�f�[�^��ǂݍ��߂܂�
bank_marketing_train <- read.csv("bank_marketing_train.csv")

head(bank_marketing_train)


# 1.�^�[�Q�b�g�̃y���\�i���������� -------------------------------------------------------


# y=yes/no�̃f�[�^�𒊏o���Ă݂�
bank_marketing_train_y <- bank_marketing_train[bank_marketing_train$y=="yes",]
bank_marketing_train_n <- bank_marketing_train[bank_marketing_train$y=="no",]
summary(bank_marketing_train_y)
summary(bank_marketing_train_n)

# �f�[�^��
num_yes = 3796
num_no = 29948

# �q�X�g�O����

# �N��
plot_ly(x = bank_marketing_train_y$age, type="histogram")
plot_ly(x = bank_marketing_train_n$age, type="histogram")
plot_ly(x = bank_marketing_train$age, type="box", color = bank_marketing_train$y)
# => yes�̕����A60�ȏオ����

# �E��
#pl_job = plot_ly(x = bank_marketing_train_y$job, type="histogram")
#plot_ly(x = bank_marketing_train_n$job, type="histogram")
# add_trace(p = pl_job, x = bank_marketing_train_n$job, type="histogram")
plot_ly(x = bank_marketing_train$job, type="box", color = bank_marketing_train$y)
plot_ly(x = bank_marketing_train$job, type="histogram", color = bank_marketing_train$y)
# �������݂Ă݂�
summary(bank_marketing_train_y$job)/num_yes
summary(bank_marketing_train_n$job)/num_no
# => yes�̕����Aretired/student�������Ablue-color�����Ȃ�

# ������
plot_ly(x = bank_marketing_train$marital, type="histogram", color = bank_marketing_train$y)
plot_ly(x = bank_marketing_train$marital, type="box", color = bank_marketing_train$y)
# => ���͂Ȃ�����

# �N���W�b�g�̎x���x��
plot_ly(x = bank_marketing_train$default, type="histogram", color = bank_marketing_train$y)
plot_ly(x = bank_marketing_train$default, type="box", color = bank_marketing_train$y)
skimr::skim(bank_marketing_train_y$default)
skimr::skim(bank_marketing_train_n$default)
# => ���͂Ȃ�����

# �ŏI�w��
plot_ly(x = bank_marketing_train$education, type="histogram", color = bank_marketing_train$y)
plot_ly(x = bank_marketing_train$education, type="box", color = bank_marketing_train$y)
# �������݂Ă݂�
summary(bank_marketing_train_y$education)/num_yes
summary(bank_marketing_train_n$education)/num_no
# => yes��university.degree������

# �s���Y���[���̗L��
plot_ly(x = bank_marketing_train$housing, type="histogram", color = bank_marketing_train$y)
plot_ly(x = bank_marketing_train$housing, type="box", color = bank_marketing_train$y)
# �������݂Ă݂�
summary(bank_marketing_train_y$housing)/num_yes
summary(bank_marketing_train_n$housing)/num_no
# => ���͂Ȃ�����

# �l���[���̗L��
plot_ly(x = bank_marketing_train$loan, type="histogram", color = bank_marketing_train$y)
plot_ly(x = bank_marketing_train$loan, type="box", color = bank_marketing_train$y)
# �������݂Ă݂�
summary(bank_marketing_train_y$loan)/num_yes
summary(bank_marketing_train_n$loan)/num_no
# => ���͂Ȃ�����

# �A���f�o�C�X
plot_ly(x = bank_marketing_train$contact, type="histogram", color = bank_marketing_train$y)
plot_ly(x = bank_marketing_train$contact, type="box", color = bank_marketing_train$y)
# �������݂Ă݂�
summary(bank_marketing_train_y$contact)/num_yes
summary(bank_marketing_train_n$contact)/num_no
# => yes��cellular������

# �O��̐ڐG����̌o�ߓ���
plot_ly(x = bank_marketing_train$pdays, type="histogram", color = bank_marketing_train$y)
plot_ly(x = bank_marketing_train$pdays, type="box", color = bank_marketing_train$y)
# �������݂Ă݂�
summary(bank_marketing_train_y$pdays)
summary(bank_marketing_train_n$pdays)
# => ���͂Ȃ�����

# �ȑO�̃L�����y�[������
plot_ly(x = bank_marketing_train$poutcome, type="histogram", color = bank_marketing_train$y)
plot_ly(x = bank_marketing_train$poutcome, type="box", color = bank_marketing_train$y)
# �������݂Ă݂�
summary(bank_marketing_train_y$poutcome)/num_yes
summary(bank_marketing_train_n$poutcome)/num_no
# => yes��success������(�S�̂̊����Ƃ��Ă�2�������Ano��0.1�����炢�Ȃ̂�yes��no�̍��͂���)

# �ȑO�̃L�����y�[���̐ڐG��
plot_ly(x = bank_marketing_train$previous, type="histogram", color = bank_marketing_train$y)
plot_ly(x = bank_marketing_train$previous, type="box", color = bank_marketing_train$y)
# �������݂Ă݂�
cut(bank_marketing_train_y$previous, breaks = c(0,1,2,3,4,5,6))
summary(bank_marketing_train_y$previous)
summary(bank_marketing_train_n$previous)
# => yes�͕��ϒl���傫��(yes:0.48, no:0.13)�������A���̐����ϐ����ǂꂾ���L���Ȃ̂��͑z�����Ȃ�

# �萫�I�ȉ���
# -�N��F���Ќ��22�΂���ƑސE���60�΂����yes������������60�΂͍����Ă���
# -�E�ƁFstudent�Aunemployed��yes�����Ȃ������O��Ă���Bstudent�͋t�B
# -�����󋵁Fdivorced�i�����j��yes�����Ȃ������O��B�X���Ȃ�
# -�N���W�b�g�̎x���x���F������yes�����������O��B�X���Ȃ�
# -�ŏI�w���F���ׂ��Ȃ�����
# -�s���Y���[���̗L���F������yes�����������O��B�X���Ȃ�
# -�l���[���̗L���F������yes�����������O��B�X���Ȃ�
# -�A���f�o�C�X�F�֌W�Ȃ��������O��Byes��cellular������
# -�O��̐ڐG����̌o�ߓ����F�Z������yes���������i�S���҂��o���Ă���j���O��B�X���Ȃ�
# -�ȑO�̃L�����y�[�����ʁFsuccess��yes���������i�p�����Ă����̂ł́j��������
# -�ȑO�̃L�����y�[���̐ڐG�񐔁F������������yes���������i�S���҂��o���Ă���j��������


# ���W�X�e�B�b�N��A�Ŋe�����ϐ�������

#�w�K�f�[�^�ƃe�X�g�f�[�^�ɕ������Ă����i���Ƃŗ\���̃f���̂��߁j
#train_idx<-sample(c(1:dim(bank_marketing_train)[1]), size = dim(bank_marketing_train)[1]*0.7)
#train<-bank_marketing_train[train_idx, ]
#test<-bank_marketing_train[-train_idx, ]

## ���W�X�e�B�b�N��A
## ��ʉ����`���f���Ȃ̂�General Legression Model(glm)
lr<-glm(y~age+job+marital+default+education+housing+
          loan+contact+day_of_week+pdays+poutcome+previous,
        data=bank_marketing_train, family="binomial")

## ���`��A�Ɠ����悤��summary�Ŋe�퓝�v�l������܂��B
summary(lr)

## step�֐�
lr2 <- step(lr)
AIC(lr2)
summary(lr2)

# �y���\�i�̐��� -----------------------------------------------------------------

# Age:60�ȏ�?
# Job:retired
# => Job:retired�̏����ŁA�f�[�^���i���Ă݂Ă݂�
bank_marketing_train_job_retired <- bank_marketing_train[bank_marketing_train$job == "retired",]
summary(bank_marketing_train_job_retired)


# y=yes/no�̃f�[�^�𒊏o���Ă݂�
bank_marketing_train_job_retired_y <- bank_marketing_train_job_retired[bank_marketing_train_job_retired$y=="yes",]
bank_marketing_train_job_retired_n <- bank_marketing_train_job_retired[bank_marketing_train_job_retired$y=="no",]
summary(bank_marketing_train_job_retired_y)
summary(bank_marketing_train_job_retired_n)

# �f�[�^��
num_retired_yes = 358
num_retired_no = 1074

# �q�X�g�O����

# �N��
plot_ly(x = bank_marketing_train_job_retired_y$age, type="histogram")
plot_ly(x = bank_marketing_train_job_retired_n$age, type="histogram")
plot_ly(x = bank_marketing_train_job_retired$age, type="histogram", color = bank_marketing_train_job_retired$y)
plot_ly(x = bank_marketing_train_job_retired$age, type="box", color = bank_marketing_train_job_retired$y)
# => yes�̕����A60�ȏオ����

# ������
plot_ly(x = bank_marketing_train_job_retired$marital, type="histogram", color = bank_marketing_train_job_retired$y)
plot_ly(x = bank_marketing_train_job_retired$marital, type="box", color = bank_marketing_train_job_retired$y)
# �������݂Ă݂�
summary(bank_marketing_train_job_retired_y$marital)/num_retired_yes
summary(bank_marketing_train_job_retired_n$marital)/num_retired_no
# => yes��single�����Ȃ�

# �N���W�b�g�̎x���x��
plot_ly(x = bank_marketing_train_job_retired$default, type="histogram", color = bank_marketing_train_job_retired$y)
plot_ly(x = bank_marketing_train_job_retired$default, type="box", color = bank_marketing_train_job_retired$y)
# �������݂Ă݂�
summary(bank_marketing_train_job_retired_y$default)/num_retired_yes
summary(bank_marketing_train_job_retired_n$default)/num_retired_no
# => yes��unknown�����Ȃ��A9����"no"

# �ŏI�w��
#plot_ly(x = bank_marketing_train_job_retired$education, type="histogram", color = bank_marketing_train_job_retired$y)
#plot_ly(x = bank_marketing_train_job_retired$education, type="box", color = bank_marketing_train_job_retired$y)
pl_yes <- plot_ly(x = bank_marketing_train_job_retired_y$education, type="histogram", name = "yes")
pl_no <- plot_ly(x = bank_marketing_train_job_retired_n$education, type="histogram", name = "no")
subplot(pl_yes, pl_no)
# �������݂Ă݂�
summary(bank_marketing_train_job_retired_y$education)/num_retired_yes
summary(bank_marketing_train_job_retired_n$education)/num_retired_no
# => yes��basic.4y, illiterate(�w���������Ȃ�)������ 

# �s���Y���[���̗L��
pl_yes <- plot_ly(x = bank_marketing_train_job_retired_y$housing, type="histogram", name = "yes")
pl_no <- plot_ly(x = bank_marketing_train_job_retired_n$housing, type="histogram", name = "no")
subplot(pl_yes, pl_no)
# �������݂Ă݂�
summary(bank_marketing_train_job_retired_y$housing)/num_retired_yes
summary(bank_marketing_train_job_retired_n$housing)/num_retired_no
# => �傫�ȍ��͂Ȃ��iyes�͏������[���L�������j

# �l���[���̗L��
plot_ly(x = bank_marketing_train_job_retired$loan, type="histogram", color = bank_marketing_train_job_retired$y)
pl_yes <- plot_ly(x = bank_marketing_train_job_retired_y$loan, type="histogram", name = "yes")
pl_no <- plot_ly(x = bank_marketing_train_job_retired_n$loan, type="histogram", name = "no")
subplot(pl_yes, pl_no)
# �������݂Ă݂�
summary(bank_marketing_train_job_retired_y$loan)/num_retired_yes
summary(bank_marketing_train_job_retired_n$loan)/num_retired_no
# => ���͂Ȃ�����

# �A���f�o�C�X
plot_ly(x = bank_marketing_train_job_retired$contact, type="histogram", color = bank_marketing_train_job_retired$y)
pl_yes <- plot_ly(x = bank_marketing_train_job_retired_y$contact, type="histogram", name = "yes")
pl_no <- plot_ly(x = bank_marketing_train_job_retired_n$contact, type="histogram", name = "no")
subplot(pl_yes, pl_no)
# �������݂Ă݂�
summary(bank_marketing_train_job_retired_y$contact)/num_retired_yes
summary(bank_marketing_train_job_retired_n$contact)/num_retired_no
# => yes��cellular������

# �O��̐ڐG����̌o�ߓ���
#plot_ly(x = bank_marketing_train_job_retired$pdays, type="histogram", color = bank_marketing_train_job_retired$y)
pl_yes <- plot_ly(x = bank_marketing_train_job_retired_y$pdays, type="histogram", name = "yes")
pl_no <- plot_ly(x = bank_marketing_train_job_retired_n$pdays, type="histogram", name = "no")
subplot(pl_yes, pl_no)
# �������݂Ă݂�
summary(bank_marketing_train_job_retired_y$pdays)
summary(bank_marketing_train_job_retired_n$pdays)
bank_marketing_train_job_retired_y
# => yes�͓������Z���l�̊������傫��

# �ȑO�̃L�����y�[������
#plot_ly(x = bank_marketing_train_job_retired$poutcome, type="histogram", color = bank_marketing_train_job_retired$y)
pl_yes <- plot_ly(x = bank_marketing_train_job_retired_y$poutcome, type="histogram", name = "yes")
pl_no <- plot_ly(x = bank_marketing_train_job_retired_n$poutcome, type="histogram", name = "no")
subplot(pl_yes, pl_no)
# �������݂Ă݂�
summary(bank_marketing_train_job_retired_y$poutcome)/num_retired_yes
summary(bank_marketing_train_job_retired_n$poutcome)/num_retired_no
# => yes��failure, success������

# �ȑO�̃L�����y�[���̐ڐG��
pl_yes <- plot_ly(x = bank_marketing_train_job_retired_y$previous, type="histogram", name = "yes")
pl_no <- plot_ly(x = bank_marketing_train_job_retired_n$previous, type="histogram", name = "no")
subplot(pl_yes, pl_no)
# �������݂Ă݂�
summary(bank_marketing_train_job_retired_y$previous)
summary(bank_marketing_train_job_retired_n$previous)
# => yes�͕��ϒl���傫��(yes:0.65, no:0.20)�������A���̐����ϐ����ǂꂾ���L���Ȃ̂��͑z�����Ȃ�

# �ŏI�I�ȃy���\�i
# age:60�ȏ�
# job:retired
# marital�F�����o������
# default(�N���W�b�g�̎x�����x��)�F�Ȃ�
# education(�ŏI�w��)�Fbasic.4y
# contact(�A���f�o�C�X)�Fcellular
# pdays�i�O��̐ڐG����̌o�ߓ����j�F���Ȃ�
# poutcome�i�ȑO�̃L�����y�[�����ʁj�F����i���߂Ă̋q�łȂ��j

# TODO ����Ă݂�������
# �����ʂ̍쐬
# ���[���̗L���i�s���Y���[���A�l���[�������킹�����́j

# 2.�\�����f����p�����A�^�b�N���X�g���쐬����

# �l�Ɉˑ����Ȃ������ϐ����܂߁A���ׂĂ̕ϐ��ɑ΂��ē��v�l���m�F���Ă���
# ������day_of_week, duration, campaign�͉˓d��̐����ϐ��ƍl���A��������
skimr::skim(bank_marketing_train)


lr3<-glm(y~.-day_of_week-duration-campaign,
        data=bank_marketing_train, family="binomial")

summary(lr3)

## step�֐�
lr4 <- step(lr3)
AIC(lr4)
summary(lr4)
#summary(lr2)
#summary(lr3)

# �����ŁAage�Ȃǂ̐����ϐ��̏d�v���������Ă��܂��̂́A
# emp.var.rate�Ȃǂ̐����ϐ��̉e�����傫�����߂Ǝv����
# �y���\�i���`����̂Ɏg�p���������ϐ��ƁA����ȊO�ŏd�v�Ȑ����ϐ���p����
# ���f�����O�������Ȃ����j�Ƃ���
# (emp.var.rate, cons.price.idx, cons.conf.idx ��ǉ�����)

# ������Python�ōs��
