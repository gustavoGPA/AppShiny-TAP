library(shiny)
library(shinydashboard)
library(ggplot2)
require(forecast)
require(lmtest)
library(readr)
require(lubridate)
require(dplyr)
require(ggthemes)

# Função para criar os gráficos
create_plot_TMCE <- function() {
  
  Base.Completa <- read_csv2("Base Completa.csv", col_names = TRUE)
  
  Valor_Futuro = matrix(0,84,1)
  
  ########################################################################
  #1 Previsão
  
  valores <- Base.Completa$`Ticket Médio - CE`
  valores = ts(valores,start=2012,frequency=12)
  n = length(valores)
  H=12*7
  
  plot(valores)
  
  MRed=arima(valores, order = c(1, 0, 0),seasonal = list(order = c(1, 2, 1)))
  coeftest(MRed)
  MRed
  
  # Calculando previsoes
  Prev=forecast(MRed, H, level=c(95))
  
  fit <- ets(valores)
  
  
  simulacao<-list()
  for (i in 1:1000) {
    simulacao[[i]]<-simulate(fit, 84, seed = round(1000*runif(1),0)) 
  }
  Dados=c(valores,rep(NA,H))
  VPrev=ts(Dados,start=2012,frequency=12)
  
  plot(VPrev,type='l',xlab='Ano',ylab='Valores',ylim=c(20,1000))
  
  for (j in 1:1000) {
    lines(simulacao[[j]], col = "gray90")
  }
  
  Prev=forecast(fit, 84, level=c(95))
  n = length(valores)
  H = length(simulacao[[1]])
  Previsto <- ts(rep(NA,n+H),start=2012,frequency=12)
  LI <- ts(rep(NA,n+H),start=2012,frequency=12)
  LS <- ts(rep(NA,n+H),start=2012,frequency=12)
  
  Prev=forecast(MRed, H, level=c(95))
  
  for(i in 1:H){
    Previsto[n+H-H+i] <- Prev$mean[i]
    LI[n+H-H+i] <- Prev$lower[i]
    LS[n+H-H+i] <- Prev$upper[i]
    
    Valor_Futuro[i,1] = Previsto[n+H-H+i]
  }
  lines(Previsto, col='blue', lwd=2)
  lines(LI, col='blue', lwd=2)
  lines(LS, col='blue', lwd=2)
  
  Prev_TM_CE <- c(Valor_Futuro)
}

create_plot_PCCE <- function() {
  
  Base.Completa <- read_csv2("Base Completa.csv", col_names = TRUE)
  
  
  Valor_Futuro = matrix(0,84,1)
  
  ########################################################################
  #2 Previsão
  valores <- Base.Completa$`Custo Per Capita - CE`
  valores = ts(valores,start=2012,frequency=12)
  n = length(valores)
  H=12*7
  
  plot(valores)
  
  MRed=arima(valores, order = c(1, 0, 0),seasonal = list(order = c(1, 2, 1)))
  coeftest(MRed)
  MRed
  
  # Calculando previsoes
  Prev=forecast(MRed, H, level=c(95))
  
  fit <- ets(valores)
  
  
  simulacao<-list()
  for (i in 1:1000) {
    simulacao[[i]]<-simulate(fit, 84, seed = round(1000*runif(1),0)) 
  }
  Dados=c(valores,rep(NA,H))
  VPrev=ts(Dados,start=2012,frequency=12)
  
  plot(VPrev,type='l',xlab='Ano',ylab='Valores',ylim=c(70,750))
  
  for (j in 1:1000) {
    lines(simulacao[[j]], col = "gray90")
  }
  
  Prev=forecast(fit, 84, level=c(95))
  
  n = length(valores)
  H = length(simulacao[[1]])
  Previsto <- ts(rep(NA,n+H),start=2012,frequency=12)
  LI <- ts(rep(NA,n+H),start=2012,frequency=12)
  LS <- ts(rep(NA,n+H),start=2012,frequency=12)
  
  Prev=forecast(MRed, H, level=c(95))
  
  for(i in 1:H){
    Previsto[n+H-H+i] <- Prev$mean[i]
    LI[n+H-H+i] <- Prev$lower[i]
    LS[n+H-H+i] <- Prev$upper[i]
    
    Valor_Futuro[i,1] = Previsto[n+H-H+i]
  }
  lines(Previsto, col='blue', lwd=2)
  lines(LI, col='blue', lwd=2)
  lines(LS, col='blue', lwd=2)
  
  Prev_CPC_CE <- c(Valor_Futuro)
  
}  
  
create_plot_TMCA <- function() {
  
  Base.Completa <- read_csv2("Base Completa.csv", col_names = TRUE)
  
  
  Valor_Futuro = matrix(0,84,1)
  ########################################################################
  #3 Previsão
  
  valores <- Base.Completa$`Ticket Médio - CA`
  valores = ts(valores,start=2012,frequency=12)
  n = length(valores)
  H=12*7
  
  plot(valores)
  
  MRed=arima(valores, order = c(1, 0, 0),seasonal = list(order = c(1, 2, 1)))
  coeftest(MRed)
  MRed
  
  # Calculando previsoes
  Prev=forecast(MRed, H, level=c(95))
  
  fit <- ets(valores)
  
  
  simulacao<-list()
  for (i in 1:1000) {
    simulacao[[i]]<-simulate(fit, 84, seed = round(1000*runif(1),0)) 
  }
  Dados=c(valores,rep(NA,H))
  VPrev=ts(Dados,start=2012,frequency=12)
  
  plot(VPrev,type='l',xlab='Ano',ylab='Valores',ylim=c(70,800))
  
  for (j in 1:1000) {
    lines(simulacao[[j]], col = "gray90")
  }
  
  Prev=forecast(fit, 84, level=c(95))
  n = length(valores)
  H = length(simulacao[[1]])
  Previsto <- ts(rep(NA,n+H),start=2012,frequency=12)
  LI <- ts(rep(NA,n+H),start=2012,frequency=12)
  LS <- ts(rep(NA,n+H),start=2012,frequency=12)
  
  Prev=forecast(MRed, H, level=c(95))
  
  for(i in 1:H){
    Previsto[n+H-H+i] <- Prev$mean[i]
    LI[n+H-H+i] <- Prev$lower[i]
    LS[n+H-H+i] <- Prev$upper[i]
    
    Valor_Futuro[i,1] = Previsto[n+H-H+i]
  }
  lines(Previsto, col='blue', lwd=2)
  lines(LI, col='blue', lwd=2)
  lines(LS, col='blue', lwd=2)
  
  Prev_TM_CA <- c(Valor_Futuro)
}

create_plot_PCCA <- function() {
  
  Base.Completa <- read_csv2("Base Completa.csv", col_names = TRUE)
  
  
  Valor_Futuro = matrix(0,84,1)
  ########################################################################
  #4 Previsão
valores <- Base.Completa$`Custo Per Capita - CA`
valores = ts(valores,start=2012,frequency=12)
n = length(valores)
H=12*7

plot(valores)

MRed=arima(valores, order = c(1, 0, 1),seasonal = list(order = c(1, 2, 1)))
coeftest(MRed)
MRed

# Calculando previsoes
Prev=forecast(MRed, H, level=c(95))

fit <- ets(valores)


simulacao<-list()
for (i in 1:1000) {
  simulacao[[i]]<-simulate(fit, 84, seed = round(1000*runif(1),0)) 
}
Dados=c(valores,rep(NA,H))
VPrev=ts(Dados,start=2012,frequency=12)

plot(VPrev,type='l',xlab='Ano',ylab='Valores',ylim=c(25,700))

for (j in 1:1000) {
  lines(simulacao[[j]], col = "gray90")
}

Prev=forecast(fit, 84, level=c(95))
n = length(valores)
H = length(simulacao[[1]])
Previsto <- ts(rep(NA,n+H),start=2012,frequency=12)
LI <- ts(rep(NA,n+H),start=2012,frequency=12)
LS <- ts(rep(NA,n+H),start=2012,frequency=12)

Prev=forecast(MRed, H, level=c(95))

for(i in 1:H){
  Previsto[n+H-H+i] <- Prev$mean[i]
  LI[n+H-H+i] <- Prev$lower[i]
  LS[n+H-H+i] <- Prev$upper[i]
  
  Valor_Futuro[i,1] = Previsto[n+H-H+i]
}
lines(Previsto, col='blue', lwd=2)
lines(LI, col='blue', lwd=2)
lines(LS, col='blue', lwd=2)

Prev_CPC_CA <- c(Valor_Futuro)
}

create_plot_TMIND <- function() {
  
  Base.Completa <- read_csv2("Base Completa.csv", col_names = TRUE)
  
  
  Valor_Futuro = matrix(0,84,1)
  
  ########################################################################
#################################################################
#5 Previsão

valores <- Base.Completa$`Ticket Médio - IND`
valores = ts(valores,start=2012,frequency=12)
n = length(valores)
H=12*7

plot(valores)

MRed=arima(valores, order = c(1, 0, 0),seasonal = list(order = c(1, 2, 1)))
coeftest(MRed)
MRed

# Calculando previsoes
Prev=forecast(MRed, H, level=c(95))

fit <- ets(valores)


simulacao<-list()
for (i in 1:1000) {
  simulacao[[i]]<-simulate(fit, 84, seed = round(1000*runif(1),0)) 
}
Dados=c(valores,rep(NA,H))
VPrev=ts(Dados,start=2012,frequency=12)

plot(VPrev,type='l',xlab='Ano',ylab='Valores',ylim=c(100,920))

for (j in 1:1000) {
  lines(simulacao[[j]], col = "gray90")
}

Prev=forecast(fit, 84, level=c(95))
n = length(valores)
H = length(simulacao[[1]])
Previsto <- ts(rep(NA,n+H),start=2012,frequency=12)
LI <- ts(rep(NA,n+H),start=2012,frequency=12)
LS <- ts(rep(NA,n+H),start=2012,frequency=12)

Prev=forecast(MRed, H, level=c(95))

for(i in 1:H){
  Previsto[n+H-H+i] <- Prev$mean[i]
  LI[n+H-H+i] <- Prev$lower[i]
  LS[n+H-H+i] <- Prev$upper[i]
  
  Valor_Futuro[i,1] = Previsto[n+H-H+i]
}
lines(Previsto, col='blue', lwd=2)
lines(LI, col='blue', lwd=2)
lines(LS, col='blue', lwd=2)

Prev_TM_IND <- c(Valor_Futuro)
}

create_plot_PCIND <- function() {
  
  Base.Completa <- read_csv2("Base Completa.csv", col_names = TRUE)
  
  
  Valor_Futuro = matrix(0,84,1)
  
  ########################################################################
#####################################################################
#6 Previsão

valores <- Base.Completa$`Custo Per Capita - IND`
valores = ts(valores,start=2012,frequency=12)
n = length(valores)
H=12*7

plot(valores)

MRed=arima(valores, order = c(1, 0, 0),seasonal = list(order = c(1, 2, 1)))
coeftest(MRed)
MRed

# Calculando previsoes
Prev=forecast(MRed, H, level=c(95))

fit <- ets(valores)


simulacao<-list()
for (i in 1:1000) {
  simulacao[[i]]<-simulate(fit, 84, seed = round(1000*runif(1),0)) 
}
Dados=c(valores,rep(NA,H))
VPrev=ts(Dados,start=2012,frequency=12)

plot(VPrev,type='l',xlab='Ano',ylab='Valores',ylim=c(70,800))
for (j in 1:1000) {
  lines(simulacao[[j]], col = "gray90")
}

Prev=forecast(fit, 84, level=c(95))
n = length(valores)
H = length(simulacao[[1]])
Previsto <- ts(rep(NA,n+H),start=2012,frequency=12)
LI <- ts(rep(NA,n+H),start=2012,frequency=12)
LS <- ts(rep(NA,n+H),start=2012,frequency=12)

Prev=forecast(MRed, H, level=c(95))

for(i in 1:H){
  Previsto[n+H-H+i] <- Prev$mean[i]
  LI[n+H-H+i] <- Prev$lower[i]
  LS[n+H-H+i] <- Prev$upper[i]
  
  Valor_Futuro[i,1] = Previsto[n+H-H+i]
}
lines(Previsto, col='blue', lwd=2)
lines(LI, col='blue', lwd=2)
lines(LS, col='blue', lwd=2)

Prev_CPC_IND <- c(Valor_Futuro)
}

create_plot_BCE <- function() {
  
  Base.Completa <- read_csv2("Base Completa.csv", col_names = TRUE)
  
  
  Valor_Futuro = matrix(0,84,1)
  
  ########################################################################
#####################################################################
#7 Previsão

valores <- Base.Completa$Coletivo.Empresarial
valores = ts(valores,start=2012,frequency=12)
n = length(valores)
H=12*7

plot(valores)

MRed=arima(valores, order = c(1, 1, 0),seasonal = list(order = c(2, 1, 0)))
coeftest(MRed)
MRed

# Calculando previsoes
Prev=forecast(MRed, H, level=c(95))

fit <- ets(valores)


simulacao<-list()
for (i in 1:1000) {
  simulacao[[i]]<-simulate(fit, 84, seed = round(1000*runif(1),0)) 
}
Dados=c(valores,rep(NA,H))
VPrev=ts(Dados,start=2012,frequency=12)

plot(VPrev,type='l',xlab='Ano',ylab='Valores', ylim=c(40000,150000))

for (j in 1:1000) {
  lines(simulacao[[j]], col = "gray90")
}

Prev=forecast(fit, 84, level=c(95))
n = length(valores)
H = length(simulacao[[1]])
Previsto <- ts(rep(NA,n+H),start=2012,frequency=12)
LI <- ts(rep(NA,n+H),start=2012,frequency=12)
LS <- ts(rep(NA,n+H),start=2012,frequency=12)

Prev=forecast(MRed, H, level=c(95))

for(i in 1:H){
  Previsto[n+H-H+i] <- Prev$mean[i]
  LI[n+H-H+i] <- Prev$lower[i]
  LS[n+H-H+i] <- Prev$upper[i]
  
  Valor_Futuro[i,1] = Previsto[n+H-H+i]
}
lines(Previsto, col='blue', lwd=2)
lines(LI, col='blue', lwd=2)
lines(LS, col='blue', lwd=2)

Prev_BENEF_CE <- c(round(Valor_Futuro))
}

create_plot_BCA <- function() {
  
  Base.Completa <- read_csv2("Base Completa.csv", col_names = TRUE)
  
  
  Valor_Futuro = matrix(0,84,1)
  
  ########################################################################
#####################################################################
#8 Previsão

valores <- Base.Completa$Coletivo.por.Adesão
valores = ts(valores,start=2012,frequency=12)
n = length(valores)
H=12*7

plot(valores)

MRed=arima(valores, order = c(1, 1, 0),seasonal = list(order = c(1, 0, 0)))
coeftest(MRed)
MRed

# Calculando previsoes
Prev=forecast(MRed, H, level=c(95))

fit <- ets(valores)


simulacao<-list()
for (i in 1:1000) {
  simulacao[[i]]<-simulate(fit, 84, seed = round(1000*runif(1),0)) 
}
Dados=c(valores,rep(NA,H))
VPrev=ts(Dados,start=2012,frequency=12)

plot(VPrev,type='l',xlab='Ano',ylab='Valores',ylim=c(11000,30000))
for (j in 1:1000) {
  lines(simulacao[[j]], col = "gray90")
}

Prev=forecast(fit, 84, level=c(95))
n = length(valores)
H = length(simulacao[[1]])
Previsto <- ts(rep(NA,n+H),start=2012,frequency=12)
LI <- ts(rep(NA,n+H),start=2012,frequency=12)
LS <- ts(rep(NA,n+H),start=2012,frequency=12)

Prev=forecast(MRed, H, level=c(95))

for(i in 1:H){
  Previsto[n+H-H+i] <- Prev$mean[i]
  LI[n+H-H+i] <- Prev$lower[i]
  LS[n+H-H+i] <- Prev$upper[i]
  
  Valor_Futuro[i,1] = Previsto[n+H-H+i]
}
lines(Previsto, col='blue', lwd=2)
lines(LI, col='blue', lwd=2)
lines(LS, col='blue', lwd=2)

Prev_BENEF_CA <- c(round(Valor_Futuro))
}

create_plot_BIND <- function() {
  
  Base.Completa <- read_csv2("Base Completa.csv", col_names = TRUE)
  
  
  Valor_Futuro = matrix(0,84,1)
  
  ########################################################################
#####################################################################
#9 Previsão

valores <- Base.Completa$Individual.ou.Familiar
valores = ts(valores,start=2012,frequency=12)
n = length(valores)
H=12*7

plot(valores)

MRed=arima(valores, order = c(0, 1, 2), seasonal = list(order = c(0, 1, 1)))
coeftest(MRed)
MRed

# Calculando previsoes
Prev=forecast(MRed, H, level=c(95))

fit <- ets(valores)


simulacao<-list()
for (i in 1:1000) {
  simulacao[[i]]<-simulate(fit, 84, seed = round(1000*runif(1),0)) 
}
Dados=c(valores,rep(NA,H))
VPrev=ts(Dados,start=2012,frequency=12)

plot(VPrev,type='l',xlab='Ano',ylab='Valores',ylim=c(17000,26000))

for (j in 1:1000) {
  lines(simulacao[[j]], col = "gray90")
}

Prev=forecast(fit, 84, level=c(95))
n = length(valores)
H = length(simulacao[[1]])
Previsto <- ts(rep(NA,n+H),start=2012,frequency=12)
LI <- ts(rep(NA,n+H),start=2012,frequency=12)
LS <- ts(rep(NA,n+H),start=2012,frequency=12)

Prev=forecast(MRed, H, level=c(95))

for(i in 1:H){
  Previsto[n+H-H+i] <- Prev$mean[i]
  LI[n+H-H+i] <- Prev$lower[i]
  LS[n+H-H+i] <- Prev$upper[i]
  
  Valor_Futuro[i,1] = Previsto[n+H-H+i]
}
lines(Previsto, col='blue', lwd=2)
lines(LI, col='blue', lwd=2)
lines(LS, col='blue', lwd=2)

Prev_BENEF_IND <- c(round(Valor_Futuro))

}

create_plot_JUROS <- function() {
  Serie_SELIC <- read_table("Percentual.Diario.txt")
  Serie_SELIC <- Serie_SELIC[-2]
  
  Serie_SELIC$Date <- format(Serie_SELIC$Date, "%Y-%m")
  média <- rowMeans(Serie_SELIC[-1])
  
  Taxa_SELIC <- cbind.data.frame(Serie_SELIC$Date, média)
  
  ggplot(Taxa_SELIC) +
    aes(x = Taxa_SELIC$`Serie_SELIC$Date`, y = Taxa_SELIC$média) +
    geom_boxplot(fill = "#113346") +
    labs(x = " ", y = " ") +
    ggthemes::theme_stata() +
    theme(axis.text.x = element_text(angle = 90L))
}

create_plot_benef <- function() {
  
  Base.Completa <- read_csv2("Base Completa.csv", col_names = TRUE)
  
  ggplot(Base.Completa) +
    aes(x = Competência) +
    geom_line(aes(y = Base.Completa$Individual.ou.Familiar, colour = "Individual.ou.Familiar")) +
    geom_line(aes(y = Base.Completa$Coletivo.Empresarial, colour = "Coletivo.Empresarial")) +
    geom_line(aes(y = Base.Completa$Coletivo.por.Adesão, colour = "Coletivo.por.Adesão"))+
    scale_colour_manual("", 
                        breaks = c("Individual.ou.Familiar", "Coletivo.Empresarial", "Coletivo.por.Adesão"),
                        values = c("red", "black", "blue")) +
    labs(x = " ", 
         y = " ") +
    ggthemes::theme_stata(base_size = 16) + 
    theme(plot.title = element_text(face = "bold"), axis.title.y = element_text(face = "bold", hjust = 2), 
          axis.title.x = element_text(face = "bold", hjust = 2)) +
    ylim(1, 100000)
}

ui <- dashboardPage(
  dashboardHeader(title = "TAP"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Contextualização", tabName = "Contextualização"),
      menuItem("Juros", tabName = "Juros"),
      menuItem("Beneficiários", tabName = "Beneficiários"),
      menuItem("Simulações", tabName = "Simulações", icon = icon("list"), 
               menuSubItem("Coletivo Empresarial", tabName = "Coletivo_Empresarial"),
               menuSubItem("Coletivo por Adesão", tabName = "Coletivo_por_Adesão"),
               menuSubItem("Individual (Familiar)", tabName = "Individual_Familiar")),
      menuItem("Resultados", tabName = "Resultados"),
      menuItem("Referências", tabName = "Referências")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Contextualização",
              h1(tags$b("Contextualização da Saúde Suplementar no país e do Teste de Adequação do Passivo (TAP)")),
              h2("O conceito de saúde suplementar começou a partir da década de 1960,
                ver (ANS, 2024), momento em que as empresas começaram a oferecer planos de
                assistência médica aos empregados. Foi uma época marcada pelo crescimento
                econômico do Brasil e pelo avanço do trabalho formal.",
                tags$br()," ",tags$br(),
                "Já a regulação da área, segundo a (ANS, 2024) teve início somente em 1999,
                com a entrada em vigor do CONSU por meio da da lei 9.656/98 que dispõe sobre
                os planos de saúde. No ano seguinte, a Agência Nacional de Saúde Suplementar
                (ANS) foi criada para atuar na regulamentação, criação e implementação de
                normas, controle e fiscalização das atividades do segmento.",
                tags$br()," ",tags$br(),
                "Uma mudança que é discutida em (Lara, 2024) quando analisa a saúde suplementar no país é que os procedimentos que antes eram para uma população
                abrangente estão mudando para um público específico com a adição de medicamentos e procedimento para pessoas com doenças raras, por esse motivo é
                necessário que as operadoras busquem por análises e estudos para assegurar de
                riscos decorrentes dessas novas obrigações assumidas.",
                tags$br()," ",tags$br(),
                "O Teste de Adequação do Passivo (TAP) foi estabelecido à princípio pela
                International Financial Reporting Standards (IFRS) em 2005, ver (Prospera,
                2022). Porém, só em 2010 foi instituída a obrigação do TAP no Brasil pela
                SUSEP (Superintendência de Seguros Privados). Por fim, a ANS (Agência
                Nacional de Saúde Suplementar) em 2020, passou a exigir o TAP das Operadoras
                com mais de 100.000 (cem mil) beneficiários.",
                tags$br()," ",tags$br(),
                "O TAP segundo (Prospera, 2022) é um teste que possui o objetivo de analisar, por meio de estimativas correntes de fluxo de caixa futuro, se o passivo está
                adequado, sendo uma ferramenta importante para avaliar a saúde financeira de
                empresas que assumem riscos.")),
      
      tabItem(tabName = "Juros",
              h1(tags$b("Previsão da Taxa de Juros")),
              h2("Com relação à SELIC, a previsão foi realizada utilizando o pacote yuimaGUI do R. O Projeto YUIMA foi desenvolvido principalmente por estatísticos que 
                 publicam ativamente na área de inferência e simulação para equações diferenciais estocásticas.",
                 tags$br()," ",tags$br(),
                 "Para modelar os dados da taxa SELIC foi utilizado o modelo de Cox-Ingersoll-Ross (CIR) para representar a evolução das taxas de juros. É um tipo de 
                 'modelo de um fator', pois descreve os movimentos das taxas de juros como impulsionados por apenas uma fonte de risco de mercado.",
                 tags$br()," ",tags$br()),
              h1(tags$b("Previsão Média da Taxa de Juros SELIC")),
              fluidRow(column(width = 2),column(width = 12, plotOutput("plot10")))),
      
      tabItem(tabName = "Beneficiários",
              h1(tags$b("Análise de Beneficiários")),
              h2("A variação do número de beneficiários por tipo de contratação ao longo do tempo que a operadora possui está representado no gráfico abaixo:"),
              fluidRow(column(width = 12),column(width = 12, plotOutput("plot11",height = "600px")))),
      
      tabItem(tabName = "Coletivo_Empresarial",
              h2("Previsão Esperada do Ticket Médio de planos Coletivos Empresariais"),
              fluidRow(column(width = 12),column(width = 12, plotOutput("plot1"))),
              h2("Previsão Esperada do Custo per Capita de planos Coletivos Empresariais"),
              fluidRow(column(width = 12),column(width = 12, plotOutput("plot2"))),
              h2("Previsão Esperada de Beneficiários em planos Coletivos Empresarial"),
              fluidRow(column(width = 12),column(width = 12, plotOutput("plot7")))),
      
      tabItem(tabName = "Coletivo_por_Adesão",
              h2("Previsão Esperada do Ticket Médio dos planos Coletivos por Adesão"),
              fluidRow(column(width = 12),column(width = 12, plotOutput("plot3"))),
              h2("Previsão Esperada do Custo per Capita de planos Coletivos por Adesão"),
              fluidRow(column(width = 12),column(width = 12, plotOutput("plot4"))),
              h2("Previsão Esperada de Beneficiários em planos Coletivos por Adesão"),
              fluidRow(column(width = 12),column(width = 12, plotOutput("plot8")))),
      
      tabItem(tabName = "Individual_Familiar",
              h2("Previsão Esperada do Ticket Médio dos planos Individuais (Familiares)"),
              fluidRow(column(width = 12), column(width = 12, plotOutput("plot5"))),
              h2("Previsão Esperada do Custo per Capita de planos Individuais (Familiares)"),
              fluidRow(column(width = 12),column(width = 12, plotOutput("plot6"))),
              h2("Previsão Esperada do de Beneficiários em planos Individuais (Familiares)"),
              fluidRow(column(width = 12),column(width = 12, plotOutput("plot9")))),
    
      tabItem(tabName = "Resultados",
              h1(tags$b("Resultados Obtidos")),
              h2("Percebe-se que a operadora escolhida apresenta solvência quando se estima o Teste de Adequação 
                 do Passivo (TAP) de maneira estocástica, pois toda a reserva de quantil está abaixo de zero, 
                 demonstrando que não ocorreu nenhum cenário de ruína observado."),
              tags$br()," ",tags$br(),
              titlePanel(tags$b("Histograma do TAP")),

              sidebarLayout(
                sidebarPanel(
                  sliderInput("confianca",
                              "Nível de confiança:",
                              min = 90,
                              max = 99,
                              value = 95)
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                  plotOutput("distPlot")
                )
              )),
      tabItem(tabName = "Referências",
              h1(tags$b("Referências Bibliográficas")),
              h3(tags$em("A. MILANÉS. Uma introdução ao cálculo estocástico e às equações diferenciais
              estocásticas. relatório técnico. Technical report, RTE-03/2004-Série Ensino,
              2004.",
                         tags$br()," ",tags$br(),
                         "ANS. Histórico: Agência nacional de saúde suplementar, 2024.
              https://www.gov.br/ans/pt-br/acesso-a-informacao/institucional/
              quem-somos-1/historico Accesso: 07/06/2024.",
                         tags$br()," ",tags$br(),
                         "ANS. Resolução normativa - rn no 465 de 24 de fevereiro de 2021. https:
                //www.unimed.coop.br/site/documents/1952326/3448463/RN+465.
              2021.pdf/5d4a4b5a-0ad2-096a-c253-18d9095fd52a?t=1627493247653,
              2021.",
                         tags$br()," ",tags$br(),
                         "C. G. Rodrigues. Envelhecimento populacional e os desafios para o sistema de
              saúde. https://www.iess.org.br/sites/default/files/2021-04/TD33.
              pdf, 2021.",
                         tags$br()," ",tags$br(),
                         "J. C. Cox, J. E. Ingersoll, and S. A. Ross. A theory of the term structure
              ofinterest rates. Research Paper, Graduate School of Business, Stanford Univ,
              1978.",
                         tags$br()," ",tags$br(),
                         "J. Peng. Value at risk and tail value at risk in uncertain environment. In Proceedings of the 8th international conference on information and management
              sciences, pages 787–793, 2009.",
                         tags$br()," ",tags$br(),
                         "L. V. O. Antonio Aurelio Duarte, Aldy Fernandes da Silva. The term structure
              of interest rates and its impact on the liability adequacy test for insurance
              companies in brazil. 2015.",
                         tags$br()," ",tags$br(),
                         "N. Lara. Texto para discussão n° 101 – 2024: Doenças raras: Panorama dos
              gastos com internaÇÕes nos planos de saÚde do brasil (2021 e 2022), 2024.",
                         tags$br()," ",tags$br(),
                         "Nazareno junior, josé (2016) uma sugestão introdutória de modelagem estocástica do cbr de subscrição para operadoras médico-hospitalares.,
              https://pt.linkedin.com/pulse/uma-sugest2016.",
                         tags$br()," ",tags$br(),
                         "Nazareno. Nazareno junior, josé (2019) teste de adequação de passivo
              (tap) – aspectos técnicos, cenários, desafios e um exemplo hipotético.,
              https://pt.linkedin.com/pulse/teste-de-adequa2019.",
                         tags$br()," ",tags$br(),
                         "Prospera. Uma abordagem sobre o teste de adequação de
              passivos - tap. https://www.prosperabr.com/analises/
              uma-abordagem-sobre-o-teste-de-adequacao-de-passivos-tap, 2022.",
                         tags$br()," ",tags$br(),        
                         "R. Cocozza, E. Di Lorenzo, A. Orlando, and M. Sibillo. A liability adequacy
              test for mathematical provision. In Mathematical and Statistical Methods in
              Insurance and Finance, pages 75–81. Springer, 2008.",
                         tags$br()," ",tags$br(),
                         "R. Korn, E. Korn, and G. Kroisandt. Monte Carlo methods and models in
              finance and insurance. CRC press, 2010.",
                         tags$br()," ",tags$br(),
                         "S. M. Iacus and N. Yoshida. Simulation and inference for stochastic processes
              with yuima. A comprehensive R framework for SDEs and other stochastic
              processes. Use R, 2018.",
                         tags$br()," ",tags$br(),
                         "Susep. Nota técnica sobre o teste de adequação de passivos (tap). Manual
              de Orientações Sobre Teste de Adequação de Passivos (TAP) e Ajuste
              Econômico do PLA relacionado ao TAP, 2022. URL https://www.gov.br/
              susep/pt-br/arquivos/arquivos-solvencia-supervisao-prudencial/
              capital-minimo-requerido-e-tap/teste-de-adequacao-de-passivos_
              versao-maio-2024.pdf.",
                         tags$br()," ",tags$br(),
                         "Wikipédia. Taxa selic — wikipédia, a enciclopédia livre, 2023. URL https:
              //pt.wikipedia.org/w/index.php?title=Taxa_Selic&oldid=66988745.
              [Online; accessed 19-novembro-2023].",
                         tags$br()," ",tags$br(),
                         "Y. Hyndman, Rob J e Khandakar. Automatic time series forecasting: the forecast package for r. Journal of statistical software, 27:1–22, 2008.")))
    )
  )
)
server <- function(input, output) {
  output$plot1 <- renderPlot({
    create_plot_TMCE()
  })
  output$plot2 <- renderPlot({
    create_plot_PCCE()
  })
  output$plot3 <- renderPlot({
    create_plot_TMCA()
  })
  output$plot4 <- renderPlot({
    create_plot_PCCA()
  })
  output$plot5 <- renderPlot({
    create_plot_TMIND()
  })
  output$plot6 <- renderPlot({
    create_plot_PCIND()
  })
  output$plot7 <- renderPlot({
    create_plot_BCE()
  })
  output$plot8 <- renderPlot({
    create_plot_BCA()
  })
  output$plot9 <- renderPlot({
    create_plot_BIND()
  })
  output$plot10 <- renderPlot({
    create_plot_JUROS()
  })
  output$plot11 <- renderPlot({
    create_plot_benef()
  })
  output$distPlot <- renderPlot({
    x    <- seq(75,99)
    confianca <- seq(min(x), max(x), length.out = input$confianca + 1)
    
    {
      Base.Completa <- read_csv2("Base Completa.csv", col_names = TRUE)
      
      
      Valor_Futuro = matrix(0,84,1)
      
      ########################################################################
      #1 Previsão
      
      valores <- Base.Completa$`Ticket Médio - CE`
      valores = ts(valores,start=2012,frequency=12)
      n = length(valores)
      H=12*7
      
      plot(valores)
      
      MRed=arima(valores, order = c(1, 0, 0),seasonal = list(order = c(1, 2, 1)))
      coeftest(MRed)
      MRed
      
      # Calculando previsoes
      Prev=forecast(MRed, H, level=c(95))
      # Grafico da serie com previsoes e intervalos de previsao
      Dados=c(valores,rep(NA,H))
      VPrev=ts(Dados,start=2012,frequency=12)
      Previsto <- ts(rep(NA,n+H),start=2012,frequency=12)
      LI <- ts(rep(NA,n+H),start=2012,frequency=12)
      LS <- ts(rep(NA,n+H),start=2012,frequency=12)
      for(i in 1:H){
        Previsto[n+H-H+i] <- Prev$mean[i]
        LI[n+H-H+i] <- Prev$lower[i]
        LS[n+H-H+i] <- Prev$upper[i]
        
        Valor_Futuro[i,1] = Previsto[n+H-H+i]
      }
      plot(VPrev,type='l',xlab='Ano',ylab='Valores',ylim=c(70,750))
      lines(Previsto, col='blue')
      lines(LI, col='red')
      lines(LS, col='red')
      
      Prev_TM_CE <- c(Valor_Futuro)
      
      #####################################################################
      #2 Previsão
      valores <- Base.Completa$`Custo Per Capita - CE`
      valores = ts(valores,start=2012,frequency=12)
      n = length(valores)
      H=12*7
      
      plot(valores)
      
      MRed=arima(valores, order = c(1, 0, 0),seasonal = list(order = c(1, 2, 1)))
      coeftest(MRed)
      MRed
      
      # Calculando previsoes
      Prev=forecast(MRed, H, level=c(95))
      # Grafico da serie com previsoes e intervalos de previsao
      Dados=c(valores,rep(NA,H))
      VPrev=ts(Dados,start=2012,frequency=12)
      Previsto <- ts(rep(NA,n+H),start=2012,frequency=12)
      LI <- ts(rep(NA,n+H),start=2012,frequency=12)
      LS <- ts(rep(NA,n+H),start=2012,frequency=12)
      for(i in 1:H){
        Previsto[n+H-H+i] <- Prev$mean[i]
        LI[n+H-H+i] <- Prev$lower[i]
        LS[n+H-H+i] <- Prev$upper[i]
        
        Valor_Futuro[i,1] = Previsto[n+H-H+i]
      }
      plot(VPrev,type='l',xlab='Ano',ylab='Valores',ylim=c(70,650))
      lines(Previsto, col='blue')
      lines(LI, col='red')
      lines(LS, col='red')
      
      Prev_CPC_CE <- c(Valor_Futuro)
      
      ########################################################################
      #3 Previsão
      
      valores <- Base.Completa$`Ticket Médio - CA`
      valores = ts(valores,start=2012,frequency=12)
      n = length(valores)
      H=12*7
      
      plot(valores)
      
      MRed=arima(valores, order = c(1, 0, 0),seasonal = list(order = c(1, 2, 1)))
      coeftest(MRed)
      MRed
      
      # Calculando previsoes
      Prev=forecast(MRed, H, level=c(95))
      # Grafico da serie com previsoes e intervalos de previsao
      Dados=c(valores,rep(NA,H))
      VPrev=ts(Dados,start=2012,frequency=12)
      Previsto <- ts(rep(NA,n+H),start=2012,frequency=12)
      LI <- ts(rep(NA,n+H),start=2012,frequency=12)
      LS <- ts(rep(NA,n+H),start=2012,frequency=12)
      for(i in 1:H){
        Previsto[n+H-H+i] <- Prev$mean[i]
        LI[n+H-H+i] <- Prev$lower[i]
        LS[n+H-H+i] <- Prev$upper[i]
        
        Valor_Futuro[i,1] = Previsto[n+H-H+i]
      }
      plot(VPrev,type='l',xlab='Ano',ylab='Valores',ylim=c(70,650))
      lines(Previsto, col='blue')
      lines(LI, col='red')
      lines(LS, col='red')
      
      Prev_TM_CA <- c(Valor_Futuro)
      
      ######################################################################
      #4 Previsão
      
      valores <- Base.Completa$`Custo Per Capita - CA`
      valores = ts(valores,start=2012,frequency=12)
      n = length(valores)
      H=12*7
      
      plot(valores)
      
      MRed=arima(valores, order = c(1, 0, 1),seasonal = list(order = c(1, 2, 1)))
      coeftest(MRed)
      MRed
      
      # Calculando previsoes
      Prev=forecast(MRed, H, level=c(95))
      # Grafico da serie com previsoes e intervalos de previsao
      Dados=c(valores,rep(NA,H))
      VPrev=ts(Dados,start=2012,frequency=12)
      Previsto <- ts(rep(NA,n+H),start=2012,frequency=12)
      LI <- ts(rep(NA,n+H),start=2012,frequency=12)
      LS <- ts(rep(NA,n+H),start=2012,frequency=12)
      for(i in 1:H){
        Previsto[n+H-H+i] <- Prev$mean[i]
        LI[n+H-H+i] <- Prev$lower[i]
        LS[n+H-H+i] <- Prev$upper[i]
        
        Valor_Futuro[i,1] = Previsto[n+H-H+i]
      }
      plot(VPrev,type='l',xlab='Ano',ylab='Valores',ylim=c(70,500))
      lines(Previsto, col='blue')
      lines(LI, col='red')
      lines(LS, col='red')
      
      Prev_CPC_CA <- c(Valor_Futuro)
      
      #################################################################
      #5 Previsão
      
      valores <- Base.Completa$`Ticket Médio - IND`
      valores = ts(valores,start=2012,frequency=12)
      n = length(valores)
      H=12*7
      
      plot(valores)
      
      MRed=arima(valores, order = c(1, 0, 0),seasonal = list(order = c(1, 2, 1)))
      coeftest(MRed)
      MRed
      
      # Calculando previsoes
      Prev=forecast(MRed, H, level=c(95))
      # Grafico da serie com previsoes e intervalos de previsao
      Dados=c(valores,rep(NA,H))
      VPrev=ts(Dados,start=2012,frequency=12)
      Previsto <- ts(rep(NA,n+H),start=2012,frequency=12)
      LI <- ts(rep(NA,n+H),start=2012,frequency=12)
      LS <- ts(rep(NA,n+H),start=2012,frequency=12)
      for(i in 1:H){
        Previsto[n+H-H+i] <- Prev$mean[i]
        LI[n+H-H+i] <- Prev$lower[i]
        LS[n+H-H+i] <- Prev$upper[i]
        
        Valor_Futuro[i,1] = Previsto[n+H-H+i]
      }
      plot(VPrev,type='l',xlab='Ano',ylab='Valores',ylim=c(70,800))
      lines(Previsto, col='blue')
      lines(LI, col='red')
      lines(LS, col='red')
      
      Prev_TM_IND <- c(Valor_Futuro)
      
      #####################################################################
      #6 Previsão
      
      valores <- Base.Completa$`Custo Per Capita - IND`
      valores = ts(valores,start=2012,frequency=12)
      n = length(valores)
      H=12*7
      
      plot(valores)
      
      MRed=arima(valores, order = c(1, 0, 0),seasonal = list(order = c(1, 2, 1)))
      coeftest(MRed)
      MRed
      
      # Calculando previsoes
      Prev=forecast(MRed, H, level=c(95))
      # Grafico da serie com previsoes e intervalos de previsao
      Dados=c(valores,rep(NA,H))
      VPrev=ts(Dados,start=2012,frequency=12)
      Previsto <- ts(rep(NA,n+H),start=2012,frequency=12)
      LI <- ts(rep(NA,n+H),start=2012,frequency=12)
      LS <- ts(rep(NA,n+H),start=2012,frequency=12)
      for(i in 1:H){
        Previsto[n+H-H+i] <- Prev$mean[i]
        LI[n+H-H+i] <- Prev$lower[i]
        LS[n+H-H+i] <- Prev$upper[i]
        
        Valor_Futuro[i,1] = Previsto[n+H-H+i]
      }
      plot(VPrev,type='l',xlab='Ano',ylab='Valores',ylim=c(70,600))
      lines(Previsto, col='blue')
      lines(LI, col='red')
      lines(LS, col='red')
      
      Prev_CPC_IND <- c(Valor_Futuro)
      
      #####################################################################
      #7 Previsão
      
      valores <- Base.Completa$Coletivo.Empresarial
      valores = ts(valores,start=2012,frequency=12)
      n = length(valores)
      H=12*7
      
      plot(valores)
      
      MRed=arima(valores, order = c(1, 1, 0),seasonal = list(order = c(2, 1, 0)))
      coeftest(MRed)
      MRed
      
      # Calculando previsoes
      Prev=forecast(MRed, H, level=c(95))
      # Grafico da serie com previsoes e intervalos de previsao
      Dados=c(valores,rep(NA,H))
      VPrev=ts(Dados,start=2012,frequency=12)
      Previsto <- ts(rep(NA,n+H),start=2012,frequency=12)
      LI <- ts(rep(NA,n+H),start=2012,frequency=12)
      LS <- ts(rep(NA,n+H),start=2012,frequency=12)
      for(i in 1:H){
        Previsto[n+H-H+i] <- Prev$mean[i]
        LI[n+H-H+i] <- Prev$lower[i]
        LS[n+H-H+i] <- Prev$upper[i]
        
        Valor_Futuro[i,1] = Previsto[n+H-H+i]
      }
      plot(VPrev,type='l',xlab='Ano',ylab='Valores', ylim=c(65000,91000))
      lines(Previsto, col='blue')
      lines(LI, col='red')
      lines(LS, col='red')
      
      Prev_BENEF_CE <- c(round(Valor_Futuro))
      
      #####################################################################
      #8 Previsão
      
      valores <- Base.Completa$Coletivo.por.Adesão
      valores = ts(valores,start=2012,frequency=12)
      n = length(valores)
      H=12*7
      
      plot(valores)
      
      MRed=arima(valores, order = c(1, 1, 0),seasonal = list(order = c(1, 0, 0)))
      coeftest(MRed)
      MRed
      
      # Calculando previsoes
      Prev=forecast(MRed, H, level=c(95))
      # Grafico da serie com previsoes e intervalos de previsao
      Dados=c(valores,rep(NA,H))
      VPrev=ts(Dados,start=2012,frequency=12)
      Previsto <- ts(rep(NA,n+H),start=2012,frequency=12)
      LI <- ts(rep(NA,n+H),start=2012,frequency=12)
      LS <- ts(rep(NA,n+H),start=2012,frequency=12)
      for(i in 1:H){
        Previsto[n+H-H+i] <- Prev$mean[i]
        LI[n+H-H+i] <- Prev$lower[i]
        LS[n+H-H+i] <- Prev$upper[i]
        
        Valor_Futuro[i,1] = Previsto[n+H-H+i]
      }
      plot(VPrev,type='l',xlab='Ano',ylab='Valores',ylim=c(10000,23000))
      lines(Previsto, col='blue')
      lines(LI, col='red')
      lines(LS, col='red')
      
      Prev_BENEF_CA <- c(round(Valor_Futuro))
      
      #####################################################################
      #9 Previsão
      
      valores <- Base.Completa$Individual.ou.Familiar
      valores = ts(valores,start=2012,frequency=12)
      n = length(valores)
      H=12*7
      
      plot(valores)
      
      MRed=arima(valores, order = c(0, 1, 2), seasonal = list(order = c(0, 1, 1)))
      coeftest(MRed)
      MRed
      
      # Calculando previsoes
      Prev=forecast(MRed, H, level=c(95))
      # Grafico da serie com previsoes e intervalos de previsao
      Dados=c(valores,rep(NA,H))
      VPrev=ts(Dados,start=2012,frequency=12)
      Previsto <- ts(rep(NA,n+H),start=2012,frequency=12)
      LI <- ts(rep(NA,n+H),start=2012,frequency=12)
      LS <- ts(rep(NA,n+H),start=2012,frequency=12)
      for(i in 1:H){
        Previsto[n+H-H+i] <- Prev$mean[i]
        LI[n+H-H+i] <- Prev$lower[i]
        LS[n+H-H+i] <- Prev$upper[i]
        
        Valor_Futuro[i,1] = Previsto[n+H-H+i]
      }
      plot(VPrev,type='l',xlab='Ano',ylab='Valores',ylim=c(18000,26000))
      lines(Previsto, col='blue')
      lines(LI, col='red')
      lines(LS, col='red')
      
      Prev_BENEF_IND <- c(round(Valor_Futuro))
      
      #########################################################################
      
      Serie_SELIC <- read_table("Percentual.Diario.txt")
      Serie_SELIC <- Serie_SELIC[-2]
      
      Serie_SELIC$Date <- format(Serie_SELIC$Date, "%Y-%m")
      média <- rowMeans(Serie_SELIC[-1])
      
      Taxa_SELIC <- cbind.data.frame(Serie_SELIC$Date, média)
      
      Taxa_SELIC <- Taxa_SELIC %>%
        group_by(`Serie_SELIC$Date`) %>%
        summarise(sum(as.numeric(média))) %>%
        filter(`Serie_SELIC$Date` >= "2023-01") %>%
        filter(`Serie_SELIC$Date` <= "2029-12")
      
      Taxa_SELIC <- Taxa_SELIC$`sum(as.numeric(média))`
      
      ########################################################################
      
      Base_Previsões <- as.data.frame(cbind(Prev_BENEF_CE,Prev_TM_CE,Prev_CPC_CE, 
                                            Prev_BENEF_CA,Prev_TM_CA,Prev_CPC_CA, 
                                            Prev_BENEF_IND,Prev_TM_IND, Prev_CPC_IND,
                                            Taxa_SELIC))
      
      rm(MRed, Prev, Valor_Futuro, Dados, H, i, LI, LS, n, Prev_CPC_CA, Taxa_SELIC, 
         Prev_CPC_CE, Prev_CPC_IND, Prev_TM_CA, Prev_TM_CE, Prev_TM_IND, média,
         Previsto, valores, VPrev, Prev_BENEF_CA, Prev_BENEF_CE, Prev_BENEF_IND)
    }
    
    require(actuar)
    
    set.seed(255)
    
    N<-5000
    tmax<-84
    #Numero de afiliados por plano S ao longo do tempo ate tmax
    #Numero de planos np
    np<-3
    S<-array(NA, dim=c(tmax,np,N))
    for (n in 1:N) {
      S[,,n]<-round(c(runif(tmax*np/3,min = min(Base_Previsões$Prev_BENEF_CE),max=max(Base_Previsões$Prev_BENEF_CE)),
                      runif(tmax*np/3,min = min(Base_Previsões$Prev_BENEF_CA),max=max(Base_Previsões$Prev_BENEF_CA)),
                      runif(tmax*np/3,min = min(Base_Previsões$Prev_BENEF_IND),max=max(Base_Previsões$Prev_BENEF_IND))))
    }
    head(S,2)
    #Tiquets por plano ao longo do tempo ate tmax
    T<-array(NA, dim=c(tmax,np,N))
    for (n in 1:N) {
      T[,,n]<-c(runif(tmax*np/3,min = min(Base_Previsões$Prev_TM_CE),max=max(Base_Previsões$Prev_TM_CE)),
                runif(tmax*np/3,min = min(Base_Previsões$Prev_TM_CA),max=max(Base_Previsões$Prev_TM_CA)),
                runif(tmax*np/3,min = min(Base_Previsões$Prev_TM_IND),max=max(Base_Previsões$Prev_TM_IND)))
    }
    head(T,2)
    #Premios por plano ao longo do tempo ate tmax
    P<-array(NA, dim=c(tmax,np,N))
    for (n in 1:N) {
      P[,,n]<-c(runif(tmax*np/3,min = min(Base_Previsões$Prev_CPC_CE),max=max(Base_Previsões$Prev_CPC_CE)),
                runif(tmax*np/3,min = min(Base_Previsões$Prev_CPC_CA),max=max(Base_Previsões$Prev_CPC_CA)),
                runif(tmax*np/3,min = min(Base_Previsões$Prev_CPC_IND),max=max(Base_Previsões$Prev_CPC_IND)))
    }
    head(P,2)
    #Valor presente correspondente a taxa r ao longo do tempo ate tmax
    
    
    r<-array(NA, dim=c(tmax,N))
    for (n in 1:N) {
      r[,n]<- c(runif(tmax,min = min(Base_Previsões$Taxa_SELIC),max=max(Base_Previsões$Taxa_SELIC)))
    }
    head(r,2)
    
    VP<-array(NA, dim=c(tmax,N))
    for (n in 1:N) {
      for (i in 1:tmax) {
        VP[i,n]<-1/((1+r[i,n])^(i))
      }
    }
    head(VP,2)
    #Simulação, N simulações
    h<-array(NA,dim =c(N,tmax))
    for (n in 1:N) {
      for (i in 1:np) {
        for (t in 1:tmax){
          h[n,t] <-(S[t,i,n]*(P[t,i,n]-T[t,i,n]))*VP[t,n]
        }
      }
    }  
    V<-c()
    for (n in 1:N) {
      ini<-0
      V[n]<-ini
      for (t in 1:tmax) {
        V[n]<-V[n]+h[n,t]
      }
    }  
    plot(h[1,],type="l")
    hist(V, (breaks=20))
    abline(v=0, col="red", lwd = 2)
    
    rm(Base_Previsões, Base.Completa)
    
    MonteCarlo<-function(TAP,alpha,a) {
      #Calculo do VaR_alpha , com inervalo de confiança de (1-a)% 
      #para ter intervalos de confiançca Montecarlo vamos dividir a amostra em 50 amostras de tamanho 100
      n<-50
      TAP_amostra<-matrix(TAP, nrow = n)
      #### Estimador Monte Carlo do VaR alpha
      VaR_alpha<-mean(apply(TAP_amostra,c(1),quantile,probs=c(1-alpha)))
      VaR_alpha_sd<-sd(apply(TAP_amostra,c(1),quantile,probs=c(1-alpha)))
      ##Intervalo de confiança Monte Carlo com (1-a)x100% de confiança para VaR
      a<-a
      VaR_alpha_IC_sup<-VaR_alpha+(qt(1-a, n-1)*VaR_alpha_sd/sqrt(n))
      VaR_alpha_IC_inf<-VaR_alpha-(qt(1-a, n-1)*VaR_alpha_sd/sqrt(n))
      VaR<-t(as.matrix(list(alpha=alpha,VaR=VaR_alpha,VaRInf=VaR_alpha_IC_inf,VaRSup=VaR_alpha_IC_sup)))
      #### Estimador Monte Carlo do VaR alpha
      Q<-apply(TAP_amostra,c(1),quantile,probs=c(1-alpha))
      TVR<-NULL
      for (i in 1:n) {
        q<-which(TAP_amostra[i,]>=Q[i])
        TVR[i]<-mean(TAP_amostra[i,q])
      }
      TVaR_alpha<-mean(TVR)
      TVaR_alpha_sd<-sd(TVR)
      ##Intervalo de confiança Monte Carlo com (1-a)x100% de confiança para TVaR
      a<- 1- c(input$confianca/100)
      TVaR_alpha_IC_sup<-TVaR_alpha+(qt(1-a, n-1)*TVaR_alpha_sd/sqrt(n))
      TVaR_alpha_IC_inf<-TVaR_alpha-(qt(1-a, n-1)*TVaR_alpha_sd/sqrt(n))
      TVaR<-t(as.matrix(list(alpha=alpha,TVaR=TVaR_alpha,TVaRInf=TVaR_alpha_IC_inf,TVaRsup=TVaR_alpha_IC_sup)))
      RESULTADO<-rbind(VaR, TVaR)
      colnames(RESULTADO)<-c("alfa",  "Média",    "Inf" ,"Sup")
      rownames(RESULTADO)<-c("Var","TVaR")
      return(RESULTADO)
    }
    
    TAP<-V
    hist(TAP)
    alpha<-1 - c(input$confianca/100)
    confianca<-1- c(input$confianca/100) #corresponde ao nivel de confiança do intervalo de (0,99) ou 99% 
    Vares<-MonteCarlo(TAP,1- c(input$confianca/100),1- c(input$confianca/100))
    knitr::kable(Vares, "simple",row.names = TRUE)
    hist(TAP, main = 'Histograma de TAP', (breaks=20))
    abline(v = Vares[,2], col = c('red', 'blue'), lwd = 2, lty = 'dashed')
    text(Vares[1,2],0, "VaR", col = "red", adj = c(1.1, -12.5))
    text(Vares[2,2],0, "TVaR", col = "blue", adj = c(-0.1, -12.5))
  })
}
shinyApp(ui = ui, server = server)