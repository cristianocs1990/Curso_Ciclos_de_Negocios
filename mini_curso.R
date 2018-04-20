###Decomposição do PIB Brasil via métodos de filtragem####

###Instalando os pacotes necessários para realizar o processo de decomposição
install.packages(readxl) ##Pacote que permite a leitura de arquivos .xls e .xlsx
install.packages(ggplot2) ##Pacote para construção de gráficos
install.packages(mFilter) ##Pacote onde os filtros de extração de tendências e ciclos estão disponíveis

###Após instalar os pacotes, os mesmos ficam salvos na sua "biblioteca".
##Para usá-los é necessário informar ao R a sua intenção através do comando "library".
library(readxl)
library(ggplot2)
library(mFilter)

##Realizando a importação dos dados referentes ao PIB do Brasil, EUA, Reino Unido, Alemanha, Japão e México
database <- read_excel("C:/Users/crist/Desktop/Mini Curso - Business Cycle/Dados/database.xlsx")
View(database) ##Comando para visualizar a base de dados
attach(database) #Comando para informar ao R qual base de dados em questão você irá trabalhar

##Transformando as variáveis em séries temporais 
##Comando "ts"(x,start=c(), end=c(), frequency="")
#start -> data inicial, end -> data final. Em ambos casos se deve colocar o ano e o período(mês, trimestre, semestre)
#frequency -> designa a periodicidade dos dados (Mensal=12, Trimestral=4, Semestral=2, Anual=1)
pib_brasil <- ts(pib_bra, start = c(1980,1), end = c(2014,3), frequency = 4)
pib_alemanha <- ts(pib_ale, start = c(1980,1), end = c(2014,3), frequency = 4)
pib_japao <- ts(pib_jap, start = c(1980,1), end = c(2014,3), frequency = 4)
pib_mexico <- ts(pib_mex, start = c(1980,1), end = c(2014,3), frequency = 4)
pib_reino_unido <- ts(pib_uk, start = c(1980,1), end = c(2014,3), frequency = 4)
pib_eua <- ts(pib_usa, start = c(1980,1), end = c(2014,3), frequency = 4)

##############################################################################
##              Verificando se as séries são não estacionárias              ## 
##############################################################################

#     INSPECIONANDO VISUALMENTE E ATRAVÉS DAS FUNÇÕES DE AUTOCORRELAÇÃO 
                       install.packages("forecast")
                           library(forecast)

##############################################################################
#Plotando as séries temporais
par(mfrow=c(2,3)) ##Constrói uma matriz 2x3 para a plotagem das séries
plot(pib_alemanha)
plot(pib_brasil)
plot(pib_eua)
plot(pib_japao)
plot(pib_mexico)
plot(pib_reino_unido)

##Visualizando a função autocorrelação (FAC)
par(mfrow=c(2,3))
Acf(pib_alemanha,lag.max=36, main="Alemanha")
Acf(pib_brasil,lag.max=36, main="Brasil")
Acf(pib_eua,lag.max=36, main="Estados Unidos")
Acf(pib_japao,lag.max=36, main="Japão")
Acf(pib_mexico,lag.max=36, main="México")
Acf(pib_reino_unido,lag.max=36, main="Reino Unido")


#######################################################################################
#                                       MÉTODO FORMAL
##                               Teste de Raiz Unitária ADF
#                              H0: Série possui raiz unitária
###                               HA: Série é estacionária

###                                   Pacote Necessário
                                    install.packages(urca)
                                        library(urca)
#######################################################################################

#                             Rotina para a realização do teste: 
##ur.df(y, type = c("none", "drift", "trend"), lags = 1,selectlags = c("Fixed", "AIC", "BIC")) 

##                                    Argumentos
## y: vetor a ser testado (série temporal); type: Especifica se a série possui intercepto e tendência
## selectlags: critérios de seleção de defasagens

#########################################################################################

##Realizando o teste para os países em análise

adf.bra <- ur.df(pib_bra, type='trend', selectlags = 'AIC')
summary(adf.bra)

adf.ale <- ur.df(pib_alemanha, type='trend', selectlags='AIC')
summary(adf.ale)

##################################################################################


#####                    Métodos de Decomposição das Séries 

##################################################################################
##Filtro Hodrick-Prescott - Comando hpfilter
pib_bra.hp1 <- hpfilter(pib_brasil, freq=1600)
pib_bra.hp2 <- hpfilter(pib_brasil, freq=0.8)
pib_bra.hp3 <- hpfilter(pib_brasil, freq=800000)

###Plotando e comparando os resultados
plot(pib_bra.hp1)
plot(pib_bra.hp2)
plot(pib_bra.hp3)



###Filtro Baxter-King
pib_bra.bk1 <- bkfilter(pib_bra, pl=4, pu=32)
pib_usa.bk1 <- bkfilter(pib_usa, pl=4, pu=32)
pib_ale.bk1 <- bkfilter(pib_ale, pl=4, pu=32)
pib_uk.bk1 <- bkfilter(pib_uk, pl=4, pu=32)
pib_mex.bk1 <- bkfilter(pib_mex, pl=4, pu=32)
pib_jap.bk1 <- bkfilter(pib_jap, pl=4, pu=32)

###Plotando e comparando os resultados
plot(pib_bra.bk1)
plot(pib_usa.bk1)
plot(pib_ale.bk1)
plot(pib_uk.bk1)
plot(pib_mex.bk1)
plot(pib_jap.bk1)



###Gerando os ciclos
ciclo_bra <- ts(pib_bra.bk1$cycle, start=c(1980,1), end=c(2014,3), frequency = 4)
ciclo_ale <- ts(pib_ale.bk1$cycle, start=c(1980,1), end=c(2014,3), frequency = 4)
ciclo_uk <- ts(pib_uk.bk1$cycle, start=c(1980,1), end=c(2014,3), frequency = 4)
ciclo_mex <- ts(pib_mex.bk1$cycle, start=c(1980,1), end=c(2014,3), frequency = 4)
ciclo_jap <- ts(pib_jap.bk1$cycle, start=c(1980,1), end=c(2014,3), frequency = 4)
ciclo_usa <- ts(pib_usa.bk1$cycle, start=c(1980,1), end=c(2014,3), frequency = 4)

##Multiplot dos Ciclos
par(mfrow=c(2,3))
plot(ciclo_bra, main="Brasil")
plot(ciclo_mex, main="México")
plot(ciclo_uk, main="Reino Unido")
plot(ciclo_usa, main="Estados Unidos")
plot(ciclo_ale, main="Alemanha")
plot(ciclo_jap, main="Japão")

###Análise dos Ciclos de Negócios
#Instalando e chamando na biblioteca o pacote pastec
install.packages("pastecs")
library(pastecs)
stat.desc(ciclo_bra[4:136], basic=TRUE, desc=TRUE, norm=TRUE)
stat.desc(ciclo_usa[4:136], basic=TRUE, desc=TRUE, norm=TRUE)
stat.desc(ciclo_mex[4:136], basic=TRUE, desc=TRUE, norm=TRUE)
stat.desc(ciclo_uk[4:136], basic=TRUE, desc=TRUE, norm=TRUE)
stat.desc(ciclo_ale[4:136], basic=TRUE, desc=TRUE, norm=TRUE)
stat.desc(ciclo_jap[4:136], basic=TRUE, desc=TRUE, norm=TRUE)


##Realizando as datações dos ciclos de negócios 
##Instalando e chamando na biblioteca o pacote BCDating
install.packages("BCDating")
library(BCDating)
dat.bra <- BBQ(pib_brasil,mincycle=5, minphase=2, name="Datação dos ciclos de Negócios do Brasil")
dat.usa <- BBQ(pib_eua,mincycle=5, minphase=2, name="Datação dos ciclos de Negócios do Brasil")
dat.ale <- BBQ(pib_alemanha,mincycle=5, minphase=2, name="Datação dos ciclos de Negócios do Brasil")
dat.mex <- BBQ(pib_mexico,mincycle=5, minphase=2, name="Datação dos ciclos de Negócios do Brasil")
dat.jap <- BBQ(pib_japao,mincycle=5, minphase=2, name="Datação dos ciclos de Negócios do Brasil")
dat.uk <- BBQ(pib_reino_unido,mincycle=5, minphase=2, name="Datação dos ciclos de Negócios do Brasil")

##Estatisticas Descritivas das datações no Brasil
show(dat.bra)
summary(dat.bra)

##Analisando a adequabilidade dos ciclos a partir das datações para cada um dos países
##Brasil
par(mfrow=c(2,1))
plot(dat.bra,pib_brasil, main="Datações dos Ciclos do Brasil e PIB Brasil" )
plot(dat.bra, ciclo_bra, main="Datações dos Ciclos do Brasil e o Ciclo de Negócios Brasil")

##Estados Unidos
plot(dat.usa,pib_eua, main="Datações e PIB EUA" )
plot(dat.usa, ciclo_usa, main="Datações e os Ciclos de Negócios EUA")

##Alemanha
plot(dat.ale,pib_alemanha, main="Datações e PIB Alemanha" )
plot(dat.ale, ciclo_ale, main="Datações e os Ciclos de Negócios Alemanha")

##Reino Unido
plot(dat.uk,pib_reino_unido, main="Datações e PIB Reino Unido" )
plot(dat.uk, ciclo_uk, main="Datações e os Ciclos de Negócios do Reino Unido")

#Japão
plot(dat.jap,pib_japao, main="Datações e PIB Japão" )
plot(dat.jap, ciclo_jap, main="Datações e os Ciclos de Negócios do Japão")

#México
plot(dat.mex,pib_mexico, main="Datações e PIB México" )
plot(dat.mex, ciclo_mex, main="Datações e os Ciclos de Negócios do México")











