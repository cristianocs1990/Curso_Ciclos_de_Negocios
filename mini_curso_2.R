###################################################################################################
##
##                                       Decomposição

                                          library(readxl)
                                          library(mFilter)
##################################################################################################

mY <- read_excel("C:/Users/crist/Desktop/Mini Curso - Business Cycle/Dados/base_2.xlsx")
attach(mY)

pib_bra <- ts(pib_bra, start=2002, frequency = 12)
juros_brasil <-  ts(juros_brasil, start=2002, frequency = 12)
ipca <-  ts(IPCA, start=2002, frequency = 12)
juros_eua <-  ts(Juros_EUA, start=2002, frequency = 12)
juros_euro <- ts(Juros_Euro, start=2002, frequency = 12)
BM <-  ts(BM, start=2002, frequency = 12)
divida_bruta <-  ts(Dívida_Bruta, start=2002, frequency = 12)

des_ale <-  ts(Des_Alemanha, start=2002, frequency = 12)
des_eua <-  ts(Des_EUA, start=2002, frequency = 12)

################################################################################################

pib.bra_bk <- bkfilter(pib_bra, pl=12, pu=96, nfix=6)
plot(pib.bra_bk)

ciclo_bra <- ts(pib.bra_bk$cycle, start=2002, frequency = 12)

mqo <- lm(ciclo_bra ~ juros_brasil + juros_eua + juros_euro + ipca + divida_bruta + BM)
