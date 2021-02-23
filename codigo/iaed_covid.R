
covid_19_bauru_mortes <- read.csv("./dados/covid_19_bauru_mortes.csv", sep=";", encoding = "UTF-8")
head(covid_19_bauru_mortes)

covid_19_bauru_casos_geral <- read.csv("./dados/covid_19_bauru_casos_geral.csv",
                                       sep=";")
head(covid_19_bauru_casos_geral)

if (!require(readxl)) install.packages("readxl")

df.mortes <- read_excel("./dados/covid_19_bauru_mortes.xlsx")
head(df.mortes) 

df.geral <- read_excel("./dados/covid_19_bauru_casos_geral.xlsx")
head(df.geral) 

temp.sexo <- df.mortes$sexo[!is.na(df.mortes$sexo)]
table(temp.sexo)

unname(table(temp.sexo))

unname(table(temp.sexo))[1]

unname(table(temp.sexo))[2]

sum(unname(table(temp.sexo)))


# Grafico de mortos por genero
pct.sexo = round(unname(table(temp.sexo)) / sum(unname(table(temp.sexo)))*100,0)
pct.sexo

pct.sexo = paste0(pct.sexo, "%")
pct.sexo

cbind("fr" = addmargins(prop.table(table(temp.sexo))))

graph.sex <- barplot(table(temp.sexo), main = "Grafico 1 - Genero",
                     ylab = "Numero de mortes", ylim = c(0,sum(unname(table(temp.sexo)))),
                     names.arg = c("Feminino", "Masculino"))
text(x = graph.sex, y = table(temp.sexo), label = pct.sexo, pos = 3, cex = 1.25, col = "blue")
axis(1, at=graph.sex, labels=table(temp.sexo), tick=F, las=1, line=-4.5, cex.axis=1.25)


# Grafico de Tipo de hospitalicazao
temp.hosp <- df.mortes$tipo_hosp[!is.na(df.mortes$tipo_hosp)]
temp.hosp
pct.hosp = round(unname(table(temp.hosp)) / sum(unname(table(temp.hosp)))*100,0)
pct.hosp

pct.hosp = paste0(pct.hosp, "%")
pct.hosp

graph.hosp<- barplot(table(temp.hosp), main = "Grafico 2 - Tipo de Hospitalizacao",
                     ylab = "Numero de óbitos", ylim = c(0,sum(unname(table(temp.hosp)))),
                     names.arg = c("Privado", "Publico"))
text(x = graph.hosp, y = table(temp.hosp), label = pct.hosp, pos = 3, cex = 1.25, col = "red")
axis(1, at=graph.hosp, labels=table(temp.hosp), tick=F, las=1, line=-4.5, cex.axis=1.25)


# Grafico de mortes por idade
temp.idade <- df.mortes$idade[!is.na(df.mortes$idade)]

idade.tb <- table(temp.idade)
cbind("f" = idade.tb)

cbind("f" = addmargins(idade.tb))

maior.freq.idade = max(idade.tb)
maior.freq.idade

round(cbind("fr" = prop.table(idade.tb))*100,1)

cbind("fr" = addmargins(prop.table(idade.tb)))

summary(temp.idade)
 
indice.1<-which.max(idade.tb > 5)
indice_2<-which.max(idade.tb > 10)
idade.tb[indice.1:indice_2]

sum(idade.tb[indice.1:indice_2])

pct.idade<-round(idade.tb / sum(idade.tb)*100,1)
pct.idade

graph.age <- barplot(table(temp.idade), 
                     main = "Grafico 3 - Mortes por idade",
                     ylab = "Numero de mortes",
                     xlab = "Idade dos mortos",
                     ylim = c(0,maior.freq.idade + 5))


# Grafico Comorbidades mais frequentes
if (!require(stringr, quietly = TRUE)) install.packages("stringr")

pangram <- df.mortes$comorbidade
texto.1 <- strsplit(pangram, " e ")
texto.2 = unlist(texto.1, use.names=FALSE)
doencas.vi = table(trimws(texto.2))
doencas.vi

maior.freq.dv = max(doencas.vi)
maior.freq.dv
maior.freq.dv.name = names(which.max(doencas.vi))
maior.freq.dv.name
maior.freq.dv.i = unname(which.max(doencas.vi))
maior.freq.dv.i

# Pega as maiores comorbidades
indice.vi.1<-which.max(doencas.vi > 5)
indice.vi.1
indice.vi.2<-which.max(doencas.vi < 84)
indice.vi.2
doencas.vi[indice.vi.1:indice.vi.2]
which(doencas.vi >= 5)
pct.doencas<-sort(round((doencas.vi[which(doencas.vi >= 5)] / sum(doencas.vi))*100,1))
pct.doencas          

pct.doencas.p = paste0(pct.doencas, "%")
pct.doencas.p
#Gráfico das principais comorbidades dos óbitos. Versão #2 do professor
graph.diseases = barplot(sort(doencas.vi[which(doencas.vi >= 5)]), 
                    main = "Grafico 4 - Principais Comorbidades",
                    ylab = "Numero de Óbitos",
                    xlab = "Comorbidade",
                    ylim = c(0,maior.freq.dv + 5),
                    names.arg = names(sort(doencas.vi[which(doencas.vi >= 5)])),
                    axisnames = T)