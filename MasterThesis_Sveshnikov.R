library(readxl)
library(stargazer)
library(dplyr)
library(corrplot)
library(ggstatsplot)
library(viridis)
library(lmtest)
library(tseries)
library(regclass)
library(sandwich)
library(plm)
library(tidyr)
library(ggplot2)
library(car)
library(viridis)

# Скачиваем данные из файла
Data_final <- read_excel("dataset.xlsx") %>% as.data.frame()
str(Data_final)

# Описательные статистики 
summary(Data_final)
stargazer(Data_final, summary = TRUE, type = "html", out = "Описательная_статистика.html")

# Изменение спредов по годам (SPREAD)
names(Data_final)
t1 <- Data_final %>% group_by(Year_Quarter) %>% summarise(mean_spread = mean(SPREAD)) %>% as.data.frame()
t2 <- Data_final %>% group_by(Year_Quarter, Year, TERM) %>% summarise(mean_spread = mean(SPREAD)) %>% as.data.frame()

# График спредов №1
ggplot(data = t1, aes(x = Year_Quarter, y = mean_spread)) + geom_col(fill = "slateblue") + theme_classic() + theme(axis.text.x = element_text(angle = 25, hjust = 1)) + xlab("Год") + ylab("Спред") + 
  scale_y_continuous(labels = scales::percent) + ggtitle("Средние спреды облигаций по кварталам", subtitle = "Построено по 28 облигациям")

# График спредов №2
ggplot(data = t2, aes(x = Year_Quarter, y = mean_spread, fill = as.character(Year))) + geom_col() + theme_classic() + theme(axis.text.x = element_text(size = 4, angle = 90, hjust = 1)) + xlab("Год") + ylab("Спред") + 
  scale_y_continuous(labels = scales::percent) + ggtitle("Средние спреды облигаций по кварталам", subtitle = "Построено по 28 облигациям") + facet_grid(~as.character(TERM)) + scale_fill_viridis_d()

## Тест Геймса-Ховелла 
ggbetweenstats(
  data = Data_final, 
  x     = TERM,
  y     = SPREAD,
  title = "Результаты теста Геймса-Ховелла", 
  caption = "Тест используется в случае наличия 3 и больше независимых выборок", 
  xlab = "Срок погашения", 
  ylab = "Спред", 
  p.adjust.method = "none",
  bf.message = FALSE) + scale_color_manual(values = c("#a16db7", "#ccb4c3", "#5d3277", "#e1ebec", "#aacfe2"))

## Корреляционная матрица
names(Data_final)
Data_final[,c(8,10,11,12,13,14,15,16,17,18,19,22,23,26)]

ggcorrmat(data = Data_final[,c(8,10,11,12,13,14,15,16,17,18,19,22,23,26)], 
          colors = c("darkred", "white", "darkblue"))

## Проверка на стационарность
data_vs <- Data_final %>% dplyr::select(Year_Quarter, BRENT, RUBUSD, IMOEX, mGDP, REF)
dim(data_vs)
data_vs1 <- unique(data_vs)
dim(data_vs1)

adf.test(Data_final$SPREAD) # Ряд стационарен

adf.test(data_vs1$BRENT) # Ряд нестационарен
adf.test(data_vs1$BRENT %>% diff()) # Ряд стационарен

adf.test(data_vs1$RUBUSD) # Ряд нестационарен
adf.test(data_vs1$RUBUSD %>% diff()) # Ряд стационарен

adf.test(data_vs1$IMOEX) # Ряд стационарен

adf.test(data_vs1$mGDP) # Ряд нестационарен
adf.test(data_vs1$mGDP %>% diff()) # Ряд стационарен

adf.test(data_vs1$REF) # Ряд нестационарен
adf.test(data_vs1$REF %>% diff()) # Ряд стационарен

# Считаем разности и доходности
data_vs1$BRENT_diff <- 0
data_vs1$RUBUSD_diff <- 0
data_vs1$mGDP_diff <- 0
data_vs1$REF_diff <- 0
data_vs1$BRENT_returns <- 0
data_vs1$RUBUSD_returns <- 0

data_vs1$BRENT_lag <- 0
data_vs1$RUBUSD_lag <- 0

dim(data_vs1)
names(data_vs1)

for (i in 1:313){
  data_vs1[i+1,7] = diff(data_vs1$BRENT)[i]
  data_vs1[i+1,8] = diff(data_vs1$RUBUSD)[i]
  data_vs1[i+1,9] = diff(data_vs1$mGDP)[i]
  data_vs1[i+1,10] = diff(data_vs1$REF)[i]
  data_vs1[i+1,11] = data_vs1$BRENT[i+1]/data_vs1$BRENT[i]-1
  data_vs1[i+1,12] = data_vs1$RUBUSD[i+1]/data_vs1$RUBUSD[i]-1
  data_vs1[i+1,13] = data_vs1$BRENT_returns[i]
  data_vs1[i+1,14] = data_vs1$RUBUSD_returns[i]
}

# Удаляем 1ю строку (-28 в данных)
data_vs1 <- data_vs1[-1,]

Data_final1 <- merge(Data_final, data_vs1)
dim(Data_final)
dim(Data_final1)

# ЛАГ Brent и RUBUSD в 1 месяц
data_vs2 <- unique(data_vs1 %>% dplyr::select(Year_Quarter, RUBUSD_returns, BRENT_returns))
dim(data_vs2)

data_vs2$RUBUSD_returns_lag <- 0
data_vs2$BRENT_returns_lag <- 0

names(data_vs2)

for (i in 1:313){
  data_vs2[i+4,4] = data_vs2[i,2]
  data_vs2[i+4,5] = data_vs2[i,3]
}

# Удаляем месяц 
data_vs3 <- data_vs2[-c(1:4),]
dim(data_vs3)

# Соединяем (-112 в данных)
dim(Data_final1)
Data_lag <- merge(Data_final1, data_vs3)
dim(Data_lag)  
dim(Data_final1)

##### Регрессии #####

# Гетероскедастичность
cse <- function(model) {
  A <- sqrt(diag(vcovHC(model,type = "HC0")))
  return(A)
}

Data_final1$CRISIS_2 <- ifelse(Data_final1$Year %in% c(2022,2023),1,0)
Data_lag$CRISIS_2 <- ifelse(Data_lag$Year %in% c(2022,2023),1,0)
model1 <- lm(data = Data_final1, SPREAD ~ COUP + COUP_PER + VOLUME + as.character(TERM) + TIME + RATING + REF_diff + BRENT_returns + RUBUSD_returns + BIDASK + PUT + CRISIS_1 + CRISIS_2)
summary(model1)

##### Добавление контрольных для проверки устойчивости #####
model2 <- lm(data = Data_final1, SPREAD ~ COUP + COUP_PER + VOLUME + as.character(TERM) + TIME + RATING + REF_diff + BRENT_returns + RUBUSD_returns + BIDASK + PUT + CRISIS_1 + CRISIS_2 + IMOEX)
model3 <- lm(data = Data_final1, SPREAD ~ COUP + COUP_PER + VOLUME + as.character(TERM) + TIME + RATING + REF_diff + BRENT_returns + RUBUSD_returns + BIDASK + PUT + CRISIS_1 + CRISIS_2 + IMOEX + mGDP_diff)
model4 <- lm(data = Data_final1, SPREAD ~ COUP + COUP_PER + VOLUME + as.character(TERM) + TIME + RATING + REF_diff + BRENT_returns + RUBUSD_returns + BIDASK + PUT + CRISIS_1 + CRISIS_2 + IMOEX + mGDP_diff + INFL_EXP)
model5 <- lm(data = Data_final1, SPREAD ~ COUP + COUP_PER + VOLUME + as.character(TERM) + TIME + RATING + REF_diff + BRENT_returns + RUBUSD_returns + BIDASK + PUT + CRISIS_1 + CRISIS_2 + mGDP_diff + INFL_EXP + as.character(INDUSTRY))
model6 <- lm(data = Data_final1, SPREAD ~ COUP + COUP_PER + VOLUME + as.character(TERM) + TIME + RATING + REF_diff + BRENT_returns + RUBUSD_returns + BIDASK + PUT + CRISIS_1 + CRISIS_2 + IMOEX + mGDP_diff + INFL_EXP + as.character(Year))

VIF(model1)
VIF(model2)
VIF(model3)
VIF(model4)
VIF(model5)
VIF(model6)

stargazer(model1, model2, model3, model4, model5, model6, type = "html", se = list(cse(model1), cse(model2), cse(model3), cse(model4), cse(model5), cse(model6)), out = "Модели.html")

##### РЕГРЕССИЯ С ЛАГОМ #####
names(Data_lag)
model_lag <- lm(data = Data_lag, SPREAD ~ COUP + COUP_PER + VOLUME + as.character(TERM) + TIME + RATING + REF_diff + BRENT_returns + BRENT_returns_lag + RUBUSD_returns + RUBUSD_returns_lag + BIDASK + PUT + CRISIS_1 + CRISIS_2)
summary(model_lag)
stargazer(model_lag, type = "html", se = list(cse(model_lag)), out = "Модель с лагом.html")



### Выбор модели ### (FE, RE или pooled)

#Модель pooled-regression на основе model1
SPREAD_pool = plm(SPREAD ~ COUP + COUP_PER + VOLUME + as.character(TERM) + TIME + RATING + REF_diff + BRENT_returns + RUBUSD_returns + BIDASK + PUT + CRISIS_1 + CRISIS_2,
                  data = Data_final1, index = c("BOND","DATE"), model="pooling")
summary(SPREAD_pool)

#Модель с фиксированными эффектами
SPREAD_fix = plm(SPREAD ~ COUP + COUP_PER + VOLUME + as.character(TERM) + TIME + RATING + REF_diff + BRENT_returns + RUBUSD_returns + BIDASK + PUT + CRISIS_1 + CRISIS_2,
                 data = Data_final1, index = c("BOND","DATE"), model="within")
summary(SPREAD_fix)

#Модель со случайными эффектами
SPREAD_rndm = plm(SPREAD ~ COUP + COUP_PER + VOLUME + as.character(TERM) + TIME + RATING + REF_diff + BRENT_returns + RUBUSD_returns + BIDASK + PUT + CRISIS_1 + CRISIS_2,
                  data = Data_final1, index = c("BOND","DATE"), 
                  effect="twoways", model="random", random.method = "walhus")
summary(SPREAD_rndm)

### Выбор модели ###

#Тест на линейное ограничение для сравнения pooled и FE
plmtest(SPREAD_pool, effect="twoways",type="ghm") #Pooled

#Тест Бреуша-Пагана для сравнения pooled и RE
bptest(SPREAD_pool, SPREAD_rndm) #RE

#Тест Хаусмана для сравнения FE и RE
phtest(SPREAD_fix, SPREAD_rndm) #RE
# Следовательно выбираем модель со случайными эффектами



### Построение моделей со случайными эффектами ###
model11 = plm(SPREAD ~ COUP + COUP_PER + VOLUME + as.character(TERM) + TIME + RATING + REF_diff + BRENT_returns + RUBUSD_returns + BIDASK + PUT + CRISIS_1 + CRISIS_2,
                  data = Data_final1, index = c("BOND","DATE"), 
                  effect="twoways", model="random", random.method = "walhus")

model22 = plm(SPREAD ~ COUP + COUP_PER + VOLUME + as.character(TERM) + TIME + RATING + REF_diff + BRENT_returns + RUBUSD_returns + BIDASK + PUT + CRISIS_1 + CRISIS_2 + IMOEX,
              data = Data_final1, index = c("BOND","DATE"), 
              effect="twoways", model="random", random.method = "walhus")

model33 = plm(SPREAD ~ COUP + COUP_PER + VOLUME + as.character(TERM) + TIME + RATING + REF_diff + BRENT_returns + RUBUSD_returns + BIDASK + PUT + CRISIS_1 + CRISIS_2 + IMOEX + mGDP_diff,
              data = Data_final1, index = c("BOND","DATE"), 
              effect="twoways", model="random", random.method = "walhus")
model44 = plm(SPREAD ~ COUP + COUP_PER + VOLUME + as.character(TERM) + TIME + RATING + REF_diff + BRENT_returns + RUBUSD_returns + BIDASK + PUT + CRISIS_1 + CRISIS_2 + mGDP_diff + INFL_EXP,
              data = Data_final1, index = c("BOND","DATE"), 
              effect="twoways", model="random", random.method = "walhus")
model55 = plm(SPREAD ~ COUP + COUP_PER + VOLUME + as.character(TERM) + TIME + RATING + REF_diff + BRENT_returns + RUBUSD_returns + BIDASK + PUT + CRISIS_1 + CRISIS_2 + mGDP_diff + INFL_EXP + as.character(INDUSTRY),
              data = Data_final1, index = c("BOND","DATE"), 
              effect="twoways", model="random", random.method = "walhus")
model66 = plm(SPREAD ~ COUP + COUP + COUP_PER + VOLUME + as.character(TERM) + TIME + RATING + REF_diff + BRENT_returns + RUBUSD_returns + BIDASK + PUT + CRISIS_1 + CRISIS_2 + IMOEX + mGDP_diff + INFL_EXP + as.character(Year),
              data = Data_final1, index = c("BOND","DATE"), 
              effect="twoways", model="random", random.method = "walhus")

VIF(model11)
VIF(model22)
VIF(model33)
VIF(model44)
VIF(model55)
VIF(model66)

stargazer(model11, model22, model33, model44, model55, model66, type = "html", se = list(cse(model1), cse(model2), cse(model3), cse(model4), cse(model5), cse(model6)), out = "Модели2.html")

##### РЕГРЕССИЯ С ЛАГОМ #####
model_lag2 = plm(SPREAD ~ COUP + COUP_PER + VOLUME + as.character(TERM) + TIME + RATING + REF_diff + BRENT_returns + BRENT_returns_lag + RUBUSD_returns + RUBUSD_returns_lag + BIDASK + PUT + CRISIS_1 + CRISIS_2,
              data = Data_lag, index = c("BOND","DATE"), 
              effect="twoways", model="random", random.method = "walhus")
stargazer(model_lag, type = "html", se = list(cse(model_lag)), out = "Модель с лагом2.html")


