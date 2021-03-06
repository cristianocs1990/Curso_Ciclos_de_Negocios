###Decomposi��o do PIB Brasil via m�todos de filtragem####

###Instalando os pacotes necess�rios para realizar o processo de decomposi��o
install.packages("readxl") ##Pacote que permite a leitura de arquivos .xls e .xlsx
install.packages("ggplot2") ##Pacote para constru��o de gr�ficos
install.packages("mFilter") ##Pacote onde os filtros de extra��o de tend�ncias e ciclos est�o dispon�veis

###Ap�s instalar os pacotes, os mesmos ficam salvos na sua "biblioteca".
##Para us�-los � necess�rio informar ao R a sua inten��o atrav�s do comando "library".
library("readxl")
library("ggplot2")
library("mFilter")

##Realizando a importa��o dos dados referentes ao PIB do Brasil, EUA, Reino Unido, Alemanha, Jap�o e M�xico
database <- read_excel("C:/Users/crist/Desktop/Mini Curso - Business Cycle/Dados/database.xlsx")
View(database) ##Comando para visualizar a base de dados
attach(database) #Comando para informar ao R qual base de dados em quest�o voc� ir� trabalhar

##Transformando as vari�veis em s�ries temporais 
##Comando "ts"(x,start=c(), end=c(), frequency="")
#start -> data inicial, end -> data final. Em ambos casos se deve colocar o ano e o per�odo(m�s, trimestre, semestre)
#frequency -> designa a periodicidade dos dados (Mensal=12, Trimestral=4, Semestral=2, Anual=1)
pib_brasil <- ts(pib_bra, start = c(1980,1), end = c(2014,3), frequency = 4)
pib_alemanha <- ts(pib_ale, start = c(1980,1), end = c(2014,3), frequency = 4)
pib_japao <- ts(pib_jap, start = c(1980,1), end = c(2014,3), frequency = 4)
pib_mexico <- ts(pib_mex, start = c(1980,1), end = c(2014,3), frequency = 4)
pib_reino_unido <- ts(pib_uk, start = c(1980,1), end = c(2014,3), frequency = 4)
pib_eua <- ts(pib_usa, start = c(1980,1), end = c(2014,3), frequency = 4)

p##############################################################################
##              Verificando se as s�ries s�o n�o estacion�rias              ## 
##############################################################################

#     INSPECIONANDO VISUALMENTE E ATRAV�S DAS FUN��ES DE AUTOCORRELA��O 
                       install.packages("forecast")
                           library(forecast)

##############################################################################
#Plotando as s�ries temporais
par(mfrow=c(2,3)) ##Constr�i uma matriz 2x3 para a plotagem das s�ries
plot(pib_alemanha)
plot(pib_brasil)
plot(pib_eua)
plot(pib_japao)
plot(pib_mexico)
plot(pib_reino_unido)

##Visualizando a fun��o autocorrela��o (FAC)
par(mfrow=c(2,3))
Acf(pib_alemanha,lag.max=36, main="Alemanha")
Acf(pib_brasil,lag.max=36, main="Brasil")
Acf(pib_eua,lag.max=36, main="Estados Unidos")
Acf(pib_japao,lag.max=36, main="Jap�o")
Acf(pib_mexico,lag.max=36, main="M�xico")
Acf(pib_reino_unido,lag.max=36, main="Reino Unido")


#######################################################################################
#                                       M�TODO FORMAL
##                               Teste de Raiz Unit�ria ADF
#                              H0: S�rie possui raiz unit�ria
###                               HA: S�rie � estacion�ria

###                                   Pacote Necess�rio
                                    install.packages(urca)
                                        library(urca)
#######################################################################################

#                             Rotina para a realiza��o do teste: 
##ur.df(y, type = c("none", "drift", "trend"), lags = 1,selectlags = c("Fixed", "AIC", "BIC")) 

##                                    Argumentos
## y: vetor a ser testado (s�rie temporal); type: Especifica se a s�rie possui intercepto e tend�ncia
## selectlags: crit�rios de sele��o de defasagens

#########################################################################################

##Realizando o teste para os pa�ses em an�lise

adf.bra <- ur.df(pib_bra, type='trend', selectlags = 'AIC')
summary(adf.bra)

adf.ale <- ur.df(pib_alemanha, type='trend', selectlags='AIC')
summary(adf.ale)

##################################################################################


#####                    M�todos de Decomposi��o das S�ries 

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
plot(ciclo_mex, main="M�xico")
plot(ciclo_uk, main="Reino Unido")
plot(ciclo_usa, main="Estados Unidos")
plot(ciclo_ale, main="Alemanha")
plot(ciclo_jap, main="Jap�o")

###An�lise dos Ciclos de Neg�cios
#Instalando e chamando na biblioteca o pacote pastec
install.packages("pastecs")
library("pastecs")
stat.desc(ciclo_bra[4:136], basic=TRUE, desc=TRUE, norm=TRUE)
stat.desc(ciclo_usa[4:136], basic=TRUE, desc=TRUE, norm=TRUE)
stat.desc(ciclo_mex[4:136], basic=TRUE, desc=TRUE, norm=TRUE)
stat.desc(ciclo_uk[4:136], basic=TRUE, desc=TRUE, norm=TRUE)
stat.desc(ciclo_ale[4:136], basic=TRUE, desc=TRUE, norm=TRUE)
stat.desc(ciclo_jap[4:136], basic=TRUE, desc=TRUE, norm=TRUE)


##Realizando as data��es dos ciclos de neg�cios 
##Instalando e chamando na biblioteca o pacote BCDating
install.packages("BCDating")
library(BCDating)
dat.bra <- BBQ(pib_brasil,mincycle=5, minphase=2, name="Data��o dos ciclos de Neg�cios do Brasil")
dat.usa <- BBQ(pib_eua,mincycle=5, minphase=2, name="Data��o dos ciclos de Neg�cios do Brasil")
dat.ale <- BBQ(pib_alemanha,mincycle=5, minphase=2, name="Data��o dos ciclos de Neg�cios do Brasil")
dat.mex <- BBQ(pib_mexico,mincycle=5, minphase=2, name="Data��o dos ciclos de Neg�cios do Brasil")
dat.jap <- BBQ(pib_japao,mincycle=5, minphase=2, name="Data��o dos ciclos de Neg�cios do Brasil")
dat.uk <- BBQ(pib_reino_unido,mincycle=5, minphase=2, name="Data��o dos ciclos de Neg�cios do Brasil")

##Estatisticas Descritivas das data��es no Brasil
show(dat.bra)
summary(dat.bra)

##Analisando a adequabilidade dos ciclos a partir das data��es para cada um dos pa�ses
##Brasil
par(mfrow=c(2,1))
plot(dat.bra,pib_brasil, main="Data��es dos Ciclos do Brasil e PIB Brasil" )
plot(dat.bra, ciclo_bra, main="Data��es dos Ciclos do Brasil e o Ciclo de Neg�cios Brasil")

##Estados Unidos
plot(dat.usa,pib_eua, main="Data��es e PIB EUA" )
plot(dat.usa, ciclo_usa, main="Data��es e os Ciclos de Neg�cios EUA")

##Alemanha
plot(dat.ale,pib_alemanha, main="Data��es e PIB Alemanha" )
plot(dat.ale, ciclo_ale, main="Data��es e os Ciclos de Neg�cios Alemanha")

##Reino Unido
plot(dat.uk,pib_reino_unido, main="Data��es e PIB Reino Unido" )
plot(dat.uk, ciclo_uk, main="Data��es e os Ciclos de Neg�cios do Reino Unido")

#Jap�o
plot(dat.jap,pib_japao, main="Data��es e PIB Jap�o" )
plot(dat.jap, ciclo_jap, main="Data��es e os Ciclos de Neg�cios do Jap�o")

#M�xico
plot(dat.mex,pib_mexico, main="Data��es e PIB M�xico" )
plot(dat.mex, ciclo_mex, main="Data��es e os Ciclos de Neg�cios do M�xico")





###############################################################################################
#
#                            Causalidade de Granger
                           install.packages("lmtest")
                                 library("lmtest")
#
##############################################################################################

granger_bra_usa <- grangertest(ciclo_bra ~ ciclo_usa, order = 2)
granger_bra_ale <- grangertest(ciclo_bra ~ ciclo_ale, order = 2)
granger_bra_mex <- grangertest(ciclo_bra ~ ciclo_mex, order = 2)
granger_bra_jap <- grangertest(ciclo_bra ~ ciclo_jap, order = 2)
granger_bra_uk <- grangertest(ciclo_bra ~ ciclo_uk, order = 2)

granger_ale_bra <- grangertest(ciclo_ale ~ ciclo_bra, order = 2)
granger_jap_bra <- grangertest(ciclo_jap ~ ciclo_bra, order = 2)
granger_mex_bra <- grangertest(ciclo_mex ~ ciclo_bra, order = 2)
granger_uk_bra <- grangertest(ciclo_uk ~ ciclo_bra, order = 2)
granger_usa_bra <- grangertest(ciclo_usa ~ ciclo_bra, order = 2)

##############################################################################################





