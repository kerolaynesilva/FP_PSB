library(pracma) #package para usar a função findpeaks
library(htmltools)
library(openxlsx) #carrega a biblioteca "openxlsx"
library(ggplot2)
library(dygraphs)
library(signal)
library(REdaS)
library(fftwtools)


# Definição do diretório de trabalho

setwd("C:/Users/samsung/Documents/Universidade/PSB/Projeto_PSB/")


# Abrindo arquivo a partir do Excel (formato .xlsx)

df1 <- read.xlsx("Coleta_Bruno_Protocolo_2_181206_141843.xlsx", sheet = 1, skipEmptyRows = FALSE)

dt <- df1$time[2] - df1$time[1] # cálculo da resolução temporal em segundos

Fs <- 1/dt

df1$pulse <- 60*df1$pulse

#Questão 3:---------------------------------------------------------------------------------------------------
plotar_graf <- function(c){
  
  return(dygraph(data.frame(time=df1$time, p = df1$pulse, channel = c)) %>% 
           dyEvent("20.574", "Fim", labelLoc = "bottom") %>%
           dyEvent("21.367", "Início", labelLoc = "top") %>%
           dyEvent("50.924", "Fim", labelLoc = "bottom") %>%
           dyEvent("51.32", "Início", labelLoc = "top") %>%
           dyEvent("70.829", "Fim", labelLoc = "bottom") %>%
           dyEvent("71.14", "Início", labelLoc = "top") %>%
           dyEvent("100.73", "Fim", labelLoc = "bottom") %>%
           dyEvent("101.21", "Início", labelLoc = "top") %>%
           dyEvent("121.68", "Fim", labelLoc = "top") %>%
           dyEvent("122.0", "Início", labelLoc = "bottom") %>%
           dyEvent("130.82", "Fim", labelLoc = "top") %>%
           dyEvent("131.1", "Início", labelLoc = "bottom") %>%
           dyEvent("147.76", "Fim", labelLoc = "top") %>%
           dyEvent("148.1", "Início", labelLoc = "bottom") %>%
           dyEvent("159.792", "Fim", labelLoc = "top") %>%
           dyEvent("160.134", "Início", labelLoc = "bottom") %>%
           dyEvent("187.756", "Fim", labelLoc = "top") %>%
           dyEvent("188.054", "Início", labelLoc = "bottom") %>%
           dyEvent("208.789", "Fim", labelLoc = "top") %>%
           dyEvent("209.081", "Início", labelLoc = "bottom") %>% dyRangeSelector())
  } 

plotar_graf(df1$chan.1)
plotar_graf(df1$chan.2)
plotar_graf(df1$chan.3)
plotar_graf(df1$chan.4)
plotar_graf(df1$chan.5)
plotar_graf(df1$chan.6)
plotar_graf(df1$chan.7)
plotar_graf(df1$chan.8)
plotar_graf(df1$chan.9)
plotar_graf(df1$chan.10)
plotar_graf(df1$chan.11)
plotar_graf(df1$chan.12)
plotar_graf(df1$chan.13)
plotar_graf(df1$chan.14)
plotar_graf(df1$chan.15)
plotar_graf(df1$chan.16)


#Questão 4:---------------------------------------------------------------------------------------------------
#Filtro Butterworth-------------------------------------------------
#0.5 - 4 Hz Delta
#4 - 7 Hz Teta
#7 - 13 Hz Alfa
#13 - 30 Hz Beta
#30 - 70 Hz Gama

#Delta-------------------------------------------------------------

plotar_delta <- function(d){
  n <- 2 # ordem do filtro
  
  Fs <- 1000
  
  Fc_ld <- 0.5 # frequência de corte inferior em Hz
  
  Fc_ud <- 4 # frequência de corte superior em Hz
  
  #  For digital filters, W must be between 0 and 1 where 1 is the
  # Nyquist frequency
  W <- c(Fc_ld, Fc_ud)/(Fs/2)
  
  bt <- butter(n, W, type = "pass") # cálculo dos coeficientes do fitro
  freqz(filt = bt, Fs = Fs)
  
  y_filt <- filtfilt(filt = bt, d)
  
  # plotando sinal original (y1), sinal corrompido (y2) e sinal filtrado (yf)
  # observe a defasagem entre o sinal original e o sinal filtrado
  d_delta <- data.frame(time = df1$time, d, yf = y_filt)
  return(d_delta)
}

dygraph(plotar_delta(df1$chan.1))
dygraph(plotar_delta(df1$chan.2))
dygraph(plotar_delta(df1$chan.3))
dygraph(plotar_delta(df1$chan.4))
dygraph(plotar_delta(df1$chan.5))
dygraph(plotar_delta(df1$chan.6))
dygraph(plotar_delta(df1$chan.7))
dygraph(plotar_delta(df1$chan.8))
dygraph(plotar_delta(df1$chan.9))
dygraph(plotar_delta(df1$chan.10))
dygraph(plotar_delta(df1$chan.11))
dygraph(plotar_delta(df1$chan.12))
dygraph(plotar_delta(df1$chan.13))
dygraph(plotar_delta(df1$chan.14))
dygraph(plotar_delta(df1$chan.15))
dygraph(plotar_delta(df1$chan.16))

#Teta-------------------------------------------------------------------

plotar_teta <- function(d){
  n <- 2 # ordem do filtro
  
  Fs <- 1000
  
  Fc_lt <- 4 # frequência de corte inferior em Hz
  
  Fc_ut <- 7 # frequência de corte superior em Hz
  
  #  For digital filters, W must be between 0 and 1 where 1 is the
  # Nyquist frequency
  W <- c(Fc_lt, Fc_ut)/(Fs/2)
  
  bt <- butter(n, W, type = "pass") # cálculo dos coeficientes do fitro
  freqz(filt = bt, Fs = Fs)
  
  y_filt <- filtfilt(filt = bt, d)
  
  # plotando sinal original (y1), sinal corrompido (y2) e sinal filtrado (yf)
  # observe a defasagem entre o sinal original e o sinal filtrado
  d_teta <- data.frame(time = df1$time, d, yf = y_filt)
  
  return(d_teta)
}

dygraph(plotar_teta(df1$chan.1))
dygraph(plotar_teta(df1$chan.2))
dygraph(plotar_teta(df1$chan.3))
dygraph(plotar_teta(df1$chan.4))
dygraph(plotar_teta(df1$chan.5))
dygraph(plotar_teta(df1$chan.6))
dygraph(plotar_teta(df1$chan.7))
dygraph(plotar_teta(df1$chan.8))
dygraph(plotar_teta(df1$chan.9))
dygraph(plotar_teta(df1$chan.10))
dygraph(plotar_teta(df1$chan.11))
dygraph(plotar_teta(df1$chan.12))
dygraph(plotar_teta(df1$chan.13))
dygraph(plotar_teta(df1$chan.14))
dygraph(plotar_teta(df1$chan.15))
dygraph(plotar_teta(df1$chan.16))



#Alfa-------------------------------------------------------
plotar_alfa <- function(d){
  n <- 2 # ordem do filtro
  
  Fs <- 1000
  
  Fc_la <- 7 # frequência de corte inferior em Hz
  
  Fc_ua <- 13 # frequência de corte superior em Hz
  
  #  For digital filters, W must be between 0 and 1 where 1 is the
  # Nyquist frequency
  W <- c(Fc_la, Fc_ua)/(Fs/2)
  
  bt <- butter(n, W, type = "pass") # cálculo dos coeficientes do fitro
  freqz(filt = bt, Fs = Fs)
  
  y_filt <- filtfilt(filt = bt, d)
  
  # plotando sinal original (y1), sinal corrompido (y2) e sinal filtrado (yf)
  # observe a defasagem entre o sinal original e o sinal filtrado
  d_alfa <- data.frame(time = df1$time, d, yf = y_filt)
  
  return(d_alfa)
}

dygraph(plotar_alfa(df1$chan.1))
dygraph(plotar_alfa(df1$chan.2))
dygraph(plotar_alfa(df1$chan.3))
dygraph(plotar_alfa(df1$chan.4))
dygraph(plotar_alfa(df1$chan.5))
dygraph(plotar_alfa(df1$chan.6))
dygraph(plotar_alfa(df1$chan.7))
dygraph(plotar_alfa(df1$chan.8))
dygraph(plotar_alfa(df1$chan.9))
dygraph(plotar_alfa(df1$chan.10))
dygraph(plotar_alfa(df1$chan.11))
dygraph(plotar_alfa(df1$chan.12))
dygraph(plotar_alfa(df1$chan.13))
dygraph(plotar_alfa(df1$chan.14))
dygraph(plotar_alfa(df1$chan.15))
dygraph(plotar_alfa(df1$chan.16))

#Beta---------------------------------------------------------
plotar_beta <- function(d){
  n <- 3 # ordem do filtro
  
  Fs <- 1000
  
  Fc_lb <- 13 # frequência de corte inferior em Hz
  
  Fc_ub <- 30 # frequência de corte superior em Hz
  
  #  For digital filters, W must be between 0 and 1 where 1 is the
  # Nyquist frequency
  W <- c(Fc_lb, Fc_ub)/(Fs/2)
  
  bt <- butter(n, W, type = "pass") # cálculo dos coeficientes do fitro
  freqz(filt = bt, Fs = Fs)
  
  y_filt <- filtfilt(filt = bt, d)
  
  # plotando sinal original (y1), sinal corrompido (y2) e sinal filtrado (yf)
  # observe a defasagem entre o sinal original e o sinal filtrado
  d_beta <- data.frame(time = df1$time, d, yf = y_filt)
  
  return(d_beta)
}

dygraph(plotar_beta(df1$chan.1))
dygraph(plotar_beta(df1$chan.2))
dygraph(plotar_beta(df1$chan.3))
dygraph(plotar_beta(df1$chan.4))
dygraph(plotar_beta(df1$chan.5))
dygraph(plotar_beta(df1$chan.6))
dygraph(plotar_beta(df1$chan.7))
dygraph(plotar_beta(df1$chan.8))
dygraph(plotar_beta(df1$chan.9))
dygraph(plotar_beta(df1$chan.10))
dygraph(plotar_beta(df1$chan.11))
dygraph(plotar_beta(df1$chan.12))
dygraph(plotar_beta(df1$chan.13))
dygraph(plotar_beta(df1$chan.14))
dygraph(plotar_beta(df1$chan.15))
dygraph(plotar_beta(df1$chan.16))


#Gama-----------------------------------------------------------------
plotar_gama <- function(d){
  n <- 3 # ordem do filtro
  
  Fs <- 1000
  
  Fc_lg <- 30 # frequência de corte inferior em Hz
  
  Fc_ug <- 70 # frequência de corte superior em Hz
  
  #  For digital filters, W must be between 0 and 1 where 1 is the
  # Nyquist frequency
  W <- c(Fc_lg, Fc_ug)/(Fs/2)
  
  bt <- butter(n, W, type = "pass") # cálculo dos coeficientes do fitro
  freqz(filt = bt, Fs = Fs)
  
  y_filt <- filtfilt(filt = bt, d)
  
  # plotando sinal original (y1), sinal corrompido (y2) e sinal filtrado (yf)
  # observe a defasagem entre o sinal original e o sinal filtrado
  d_gama <- data.frame(time = df1$time, d, yf = y_filt)
  
  return(d_gama)
}

dygraph(plotar_gama(df1$chan.1))
dygraph(plotar_gama(df1$chan.2))
dygraph(plotar_gama(df1$chan.3))
dygraph(plotar_gama(df1$chan.4))
dygraph(plotar_gama(df1$chan.5))
dygraph(plotar_gama(df1$chan.6))
dygraph(plotar_gama(df1$chan.7))
dygraph(plotar_gama(df1$chan.8))
dygraph(plotar_gama(df1$chan.9))
dygraph(plotar_gama(df1$chan.10))
dygraph(plotar_gama(df1$chan.11))
dygraph(plotar_gama(df1$chan.12))
dygraph(plotar_gama(df1$chan.13))
dygraph(plotar_gama(df1$chan.14))
dygraph(plotar_gama(df1$chan.15))
dygraph(plotar_gama(df1$chan.16))

#Questão 5:---------------------------------------------------------------------------------------------------
espectro_amplitude <- function(df){

  #Criando lista para armazenar os espectros de amplitude de cada burst
  ff <- rep(NA, length(df1$time))
  
  for(i in 1:length(df1$time)){
    ff[i] <- fftw(df$yf[i], inverse=0, HermConj=1, n=NULL)
  }
  
  # dygraph(data.frame(time=l2[[1]], espectro=abs(ff[[1]])))
  espectro <- vector("list", 2)
  espectro[[1]] <- df1$time
  espectro[[2]] <- ff
  
  return(espectro) #Retorna lista que contém o tempo e espectro do sinal analisado
}

#Espectro delta
ed1 <- espectro_amplitude(plotar_delta(df1$chan.1))
ed2 <- espectro_amplitude(plotar_delta(df1$chan.2))
ed3 <- espectro_amplitude(plotar_delta(df1$chan.3))
ed4 <- espectro_amplitude(plotar_delta(df1$chan.4))
ed5 <- espectro_amplitude(plotar_delta(df1$chan.5))
ed6 <- espectro_amplitude(plotar_delta(df1$chan.6))
ed7 <- espectro_amplitude(plotar_delta(df1$chan.7))
ed8 <- espectro_amplitude(plotar_delta(df1$chan.8))
ed9 <- espectro_amplitude(plotar_delta(df1$chan.9))
ed10 <- espectro_amplitude(plotar_delta(df1$chan.10))
ed11 <- espectro_amplitude(plotar_delta(df1$chan.11))
ed12 <- espectro_amplitude(plotar_delta(df1$chan.12))
ed13 <- espectro_amplitude(plotar_delta(df1$chan.13))
ed14 <- espectro_amplitude(plotar_delta(df1$chan.14))
ed15 <- espectro_amplitude(plotar_delta(df1$chan.15))
ed16 <- espectro_amplitude(plotar_delta(df1$chan.16))


#Espectro teta
et1 <- espectro_amplitude(plotar_teta(df1$chan.1))
et2 <- espectro_amplitude(plotar_teta(df1$chan.2))
et3 <- espectro_amplitude(plotar_teta(df1$chan.3))
et4 <- espectro_amplitude(plotar_teta(df1$chan.4))
et5 <- espectro_amplitude(plotar_teta(df1$chan.5))
et6 <- espectro_amplitude(plotar_teta(df1$chan.6))
et7 <- espectro_amplitude(plotar_teta(df1$chan.7))
et8 <- espectro_amplitude(plotar_teta(df1$chan.8))
et9 <- espectro_amplitude(plotar_teta(df1$chan.9))
et10 <- espectro_amplitude(plotar_teta(df1$chan.10))
et11 <- espectro_amplitude(plotar_teta(df1$chan.11))
et12 <- espectro_amplitude(plotar_teta(df1$chan.12))
et13 <- espectro_amplitude(plotar_teta(df1$chan.13))
et14 <- espectro_amplitude(plotar_teta(df1$chan.14))
et15 <- espectro_amplitude(plotar_teta(df1$chan.15))
et16 <- espectro_amplitude(plotar_teta(df1$chan.16))

#Espectro alfa
ea1 <- espectro_amplitude(plotar_alfa(df1$chan.1))
ea2 <- espectro_amplitude(plotar_alfa(df1$chan.2))
ea3 <- espectro_amplitude(plotar_alfa(df1$chan.3))
ea4 <- espectro_amplitude(plotar_alfa(df1$chan.4))
ea5 <- espectro_amplitude(plotar_alfa(df1$chan.5))
ea6 <- espectro_amplitude(plotar_alfa(df1$chan.6))
ea7 <- espectro_amplitude(plotar_alfa(df1$chan.7))
ea8 <- espectro_amplitude(plotar_alfa(df1$chan.8))
ea9 <- espectro_amplitude(plotar_alfa(df1$chan.9))
ea10 <- espectro_amplitude(plotar_alfa(df1$chan.10))
ea11 <- espectro_amplitude(plotar_alfa(df1$chan.11))
ea12 <- espectro_amplitude(plotar_alfa(df1$chan.12))
ea13 <- espectro_amplitude(plotar_alfa(df1$chan.13))
ea14 <- espectro_amplitude(plotar_alfa(df1$chan.14))
ea15 <- espectro_amplitude(plotar_alfa(df1$chan.15))
ea16 <- espectro_amplitude(plotar_alfa(df1$chan.16))

#Espectro beta
eb1 <- espectro_amplitude(plotar_beta(df1$chan.1))
eb2 <- espectro_amplitude(plotar_beta(df1$chan.2))
eb3 <- espectro_amplitude(plotar_beta(df1$chan.3))
eb4 <- espectro_amplitude(plotar_beta(df1$chan.4))
eb5 <- espectro_amplitude(plotar_beta(df1$chan.5))
eb6 <- espectro_amplitude(plotar_beta(df1$chan.6))
eb7 <- espectro_amplitude(plotar_beta(df1$chan.7))
eb8 <- espectro_amplitude(plotar_beta(df1$chan.8))
eb9 <- espectro_amplitude(plotar_beta(df1$chan.9))
eb10 <- espectro_amplitude(plotar_beta(df1$chan.10))
eb11 <- espectro_amplitude(plotar_beta(df1$chan.11))
eb12 <- espectro_amplitude(plotar_beta(df1$chan.12))
eb13 <- espectro_amplitude(plotar_beta(df1$chan.13))
eb14 <- espectro_amplitude(plotar_beta(df1$chan.14))
eb15 <- espectro_amplitude(plotar_beta(df1$chan.15))
eb16 <- espectro_amplitude(plotar_beta(df1$chan.16))

#Espectro gama
eg1 <- espectro_amplitude(plotar_gama(df1$chan.1))
eg2 <- espectro_amplitude(plotar_gama(df1$chan.2))
eg3 <- espectro_amplitude(plotar_gama(df1$chan.3))
eg4 <- espectro_amplitude(plotar_gama(df1$chan.4))
eg5 <- espectro_amplitude(plotar_gama(df1$chan.5))
eg6 <- espectro_amplitude(plotar_gama(df1$chan.6))
eg7 <- espectro_amplitude(plotar_gama(df1$chan.7))
eg8 <- espectro_amplitude(plotar_gama(df1$chan.8))
eg9 <- espectro_amplitude(plotar_gama(df1$chan.9))
eg10 <- espectro_amplitude(plotar_gama(df1$chan.10))
eg11 <- espectro_amplitude(plotar_gama(df1$chan.11))
eg12 <- espectro_amplitude(plotar_gama(df1$chan.12))
eg13 <- espectro_amplitude(plotar_gama(df1$chan.13))
eg14 <- espectro_amplitude(plotar_gama(df1$chan.14))
eg15 <- espectro_amplitude(plotar_gama(df1$chan.15))
eg16 <- espectro_amplitude(plotar_gama(df1$chan.16))

#Para cada evento:
evento <- c(0, 20.574, 21.367, 50.924, 51.32, 70.829, 71.14, 100.73, 101.21, 121.68, 122.0, 130.82, 131.1,
            147.76, 148.1, 159.792, 160.134, 187.756, 188.054, 208.789, 209.081)

     
espectro_amplitude_evento <- function(df, e){
  
  intervalo <- vector('numeric')
  
  for(i in e[1]:e[2]){
    intervalo[i] <- df1$time[i]
  }
  
  #Criando lista para armazenar os espectros de amplitude de cada burst
  ff <- rep(NA, length(intervalo))
  
  for(i in 1:length(intervalo)){
    ff[i] <- fftw(df[i], inverse=0, HermConj=1, n=NULL)
  }
  
  # dygraph(data.frame(time=l2[[1]], espectro=abs(ff[[1]])))
  espectro <- vector("list", 2)
  espectro[[1]] <- intervalo
  espectro[[2]] <- ff
  
  return(espectro) #Retorna lista que contém o tempo e espectro do sinal analisado
}

#Evento 1:
Ev1.1 <- espectro_amplitude_evento(df1$chan.1, evento[1:2])
Ev1.2 <- espectro_amplitude_evento(df1$chan.2, evento[1:2])
Ev1.3 <- espectro_amplitude_evento(df1$chan.3, evento[1:2])
Ev1.4 <- espectro_amplitude_evento(df1$chan.4, evento[1:2])
Ev1.5 <- espectro_amplitude_evento(df1$chan.5, evento[1:2])
Ev1.6 <- espectro_amplitude_evento(df1$chan.6, evento[1:2])
Ev1.7 <- espectro_amplitude_evento(df1$chan.7, evento[1:2])
Ev1.8 <- espectro_amplitude_evento(df1$chan.8, evento[1:2])
Ev1.9 <- espectro_amplitude_evento(df1$chan.9, evento[1:2])
Ev1.10 <- espectro_amplitude_evento(df1$chan.10, evento[1:2])
Ev1.11 <- espectro_amplitude_evento(df1$chan.11, evento[1:2])
Ev1.12 <- espectro_amplitude_evento(df1$chan.12, evento[1:2])
Ev1.13 <- espectro_amplitude_evento(df1$chan.13, evento[1:2])
Ev1.14 <- espectro_amplitude_evento(df1$chan.14, evento[1:2])
Ev1.15 <- espectro_amplitude_evento(df1$chan.15, evento[1:2])
Ev1.16 <- espectro_amplitude_evento(df1$chan.16, evento[1:2])

#Evento 2:
Ev2.1 <- espectro_amplitude_evento(df1$chan.1, evento[3:4])
Ev2.2 <- espectro_amplitude_evento(df1$chan.2, evento[3:4])
Ev2.3 <- espectro_amplitude_evento(df1$chan.3, evento[3:4])
Ev2.4 <- espectro_amplitude_evento(df1$chan.4, evento[3:4])
Ev2.5 <- espectro_amplitude_evento(df1$chan.5, evento[3:4])
Ev2.6 <- espectro_amplitude_evento(df1$chan.6, evento[3:4])
Ev2.7 <- espectro_amplitude_evento(df1$chan.7, evento[3:4])
Ev2.8 <- espectro_amplitude_evento(df1$chan.8, evento[3:4])
Ev2.9 <- espectro_amplitude_evento(df1$chan.9, evento[3:4])
Ev2.10 <- espectro_amplitude_evento(df1$chan.10, evento[3:4])
Ev2.11 <- espectro_amplitude_evento(df1$chan.11, evento[3:4])
Ev2.12 <- espectro_amplitude_evento(df1$chan.12, evento[3:4])
Ev2.13 <- espectro_amplitude_evento(df1$chan.13, evento[3:4])
Ev2.14 <- espectro_amplitude_evento(df1$chan.14, evento[3:4])
Ev2.15 <- espectro_amplitude_evento(df1$chan.15, evento[3:4])
Ev2.16 <- espectro_amplitude_evento(df1$chan.16, evento[3:4])


#Evento 3:
Ev3.1 <- espectro_amplitude_evento(df1$chan.1, evento[5:6])
Ev3.2 <- espectro_amplitude_evento(df1$chan.2, evento[5:6])
Ev3.3 <- espectro_amplitude_evento(df1$chan.3, evento[5:6])
Ev3.4 <- espectro_amplitude_evento(df1$chan.4, evento[5:6])
Ev3.5 <- espectro_amplitude_evento(df1$chan.5, evento[5:6])
Ev3.6 <- espectro_amplitude_evento(df1$chan.6, evento[5:6])
Ev3.7 <- espectro_amplitude_evento(df1$chan.7, evento[5:6])
Ev3.8 <- espectro_amplitude_evento(df1$chan.8, evento[5:6])
Ev3.9 <- espectro_amplitude_evento(df1$chan.9, evento[5:6])
Ev3.10 <- espectro_amplitude_evento(df1$chan.10, evento[5:6])
Ev3.11 <- espectro_amplitude_evento(df1$chan.11, evento[5:6])
Ev3.12 <- espectro_amplitude_evento(df1$chan.12, evento[5:6])
Ev3.13 <- espectro_amplitude_evento(df1$chan.13, evento[5:6])
Ev3.14 <- espectro_amplitude_evento(df1$chan.14, evento[5:6])
Ev3.15 <- espectro_amplitude_evento(df1$chan.15, evento[5:6])
Ev3.16 <- espectro_amplitude_evento(df1$chan.16, evento[5:6])


#Evento 4:
Ev4.1 <- espectro_amplitude_evento(df1$chan.1, evento[7:8])
Ev4.2 <- espectro_amplitude_evento(df1$chan.2, evento[7:8])
Ev4.3 <- espectro_amplitude_evento(df1$chan.3, evento[7:8])
Ev4.4 <- espectro_amplitude_evento(df1$chan.4, evento[7:8])
Ev4.5 <- espectro_amplitude_evento(df1$chan.5, evento[7:8])
Ev4.6 <- espectro_amplitude_evento(df1$chan.6, evento[7:8])
Ev4.7 <- espectro_amplitude_evento(df1$chan.7, evento[7:8])
Ev4.8 <- espectro_amplitude_evento(df1$chan.8, evento[7:8])
Ev4.9 <- espectro_amplitude_evento(df1$chan.9, evento[7:8])
Ev4.10 <- espectro_amplitude_evento(df1$chan.10, evento[7:8])
Ev4.11 <- espectro_amplitude_evento(df1$chan.11, evento[7:8])
Ev4.12 <- espectro_amplitude_evento(df1$chan.12, evento[7:8])
Ev4.13 <- espectro_amplitude_evento(df1$chan.13, evento[7:8])
Ev4.14 <- espectro_amplitude_evento(df1$chan.14, evento[7:8])
Ev4.15 <- espectro_amplitude_evento(df1$chan.15, evento[7:8])
Ev4.16 <- espectro_amplitude_evento(df1$chan.16, evento[7:8])

#Evento 5:
Ev5.1 <- espectro_amplitude_evento(df1$chan.1, evento[9:10])
Ev5.2 <- espectro_amplitude_evento(df1$chan.2, evento[9:10])
Ev5.3 <- espectro_amplitude_evento(df1$chan.3, evento[9:10])
Ev5.4 <- espectro_amplitude_evento(df1$chan.4, evento[9:10])
Ev5.5 <- espectro_amplitude_evento(df1$chan.5, evento[9:10])
Ev5.6 <- espectro_amplitude_evento(df1$chan.6, evento[9:10])
Ev5.7 <- espectro_amplitude_evento(df1$chan.7, evento[9:10])
Ev5.8 <- espectro_amplitude_evento(df1$chan.8, evento[9:10])
Ev5.9 <- espectro_amplitude_evento(df1$chan.9, evento[9:10])
Ev5.10 <- espectro_amplitude_evento(df1$chan.10, evento[9:10])
Ev5.11 <- espectro_amplitude_evento(df1$chan.11, evento[9:10])
Ev5.12 <- espectro_amplitude_evento(df1$chan.12, evento[9:10])
Ev5.13 <- espectro_amplitude_evento(df1$chan.13, evento[9:10])
Ev5.14 <- espectro_amplitude_evento(df1$chan.14, evento[9:10])
Ev5.15 <- espectro_amplitude_evento(df1$chan.15, evento[9:10])
Ev5.16 <- espectro_amplitude_evento(df1$chan.16, evento[9:10])

#Evento 6:
Ev6.1 <- espectro_amplitude_evento(df1$chan.1, evento[11:12])
Ev6.2 <- espectro_amplitude_evento(df1$chan.2, evento[11:12])
Ev6.3 <- espectro_amplitude_evento(df1$chan.3, evento[11:12])
Ev6.4 <- espectro_amplitude_evento(df1$chan.4, evento[11:12])
Ev6.5 <- espectro_amplitude_evento(df1$chan.5, evento[11:12])
Ev6.6 <- espectro_amplitude_evento(df1$chan.6, evento[11:12])
Ev6.7 <- espectro_amplitude_evento(df1$chan.7, evento[11:12])
Ev6.8 <- espectro_amplitude_evento(df1$chan.8, evento[11:12])
Ev6.9 <- espectro_amplitude_evento(df1$chan.9, evento[11:12])
Ev6.10 <- espectro_amplitude_evento(df1$chan.10, evento[11:12])
Ev6.11 <- espectro_amplitude_evento(df1$chan.11, evento[11:12])
Ev6.12 <- espectro_amplitude_evento(df1$chan.12, evento[11:12])
Ev6.13 <- espectro_amplitude_evento(df1$chan.13, evento[11:12])
Ev6.14 <- espectro_amplitude_evento(df1$chan.14, evento[11:12])
Ev6.15 <- espectro_amplitude_evento(df1$chan.15, evento[11:12])
Ev6.16 <- espectro_amplitude_evento(df1$chan.16, evento[11:12])

#Evento 7:
Ev7.1 <- espectro_amplitude_evento(df1$chan.1, evento[13:14])
Ev7.2 <- espectro_amplitude_evento(df1$chan.2, evento[13:14])
Ev7.3 <- espectro_amplitude_evento(df1$chan.3, evento[13:14])
Ev7.4 <- espectro_amplitude_evento(df1$chan.4, evento[13:14])
Ev7.5 <- espectro_amplitude_evento(df1$chan.5, evento[13:14])
Ev7.6 <- espectro_amplitude_evento(df1$chan.6, evento[13:14])
Ev7.7 <- espectro_amplitude_evento(df1$chan.7, evento[13:14])
Ev7.8 <- espectro_amplitude_evento(df1$chan.8, evento[13:14])
Ev7.9 <- espectro_amplitude_evento(df1$chan.9, evento[13:14])
Ev7.10 <- espectro_amplitude_evento(df1$chan.10, evento[13:14])
Ev7.11 <- espectro_amplitude_evento(df1$chan.11, evento[13:14])
Ev7.12 <- espectro_amplitude_evento(df1$chan.12, evento[13:14])
Ev7.13 <- espectro_amplitude_evento(df1$chan.13, evento[13:14])
Ev7.14 <- espectro_amplitude_evento(df1$chan.14, evento[13:14])
Ev7.15 <- espectro_amplitude_evento(df1$chan.15, evento[13:14])
Ev7.16 <- espectro_amplitude_evento(df1$chan.16, evento[13:14])

#Evento 8:
Ev8.1 <- espectro_amplitude_evento(df1$chan.1, evento[15:16])
Ev8.2 <- espectro_amplitude_evento(df1$chan.2, evento[15:16])
Ev8.3 <- espectro_amplitude_evento(df1$chan.3, evento[15:16])
Ev8.4 <- espectro_amplitude_evento(df1$chan.4, evento[15:16])
Ev8.5 <- espectro_amplitude_evento(df1$chan.5, evento[15:16])
Ev8.6 <- espectro_amplitude_evento(df1$chan.6, evento[15:16])
Ev8.7 <- espectro_amplitude_evento(df1$chan.7, evento[15:16])
Ev8.8 <- espectro_amplitude_evento(df1$chan.8, evento[15:16])
Ev8.9 <- espectro_amplitude_evento(df1$chan.9, evento[15:16])
Ev8.10 <- espectro_amplitude_evento(df1$chan.10, evento[15:16])
Ev8.11 <- espectro_amplitude_evento(df1$chan.11, evento[15:16])
Ev8.12 <- espectro_amplitude_evento(df1$chan.12, evento[15:16])
Ev8.13 <- espectro_amplitude_evento(df1$chan.13, evento[15:16])
Ev8.14 <- espectro_amplitude_evento(df1$chan.14, evento[15:16])
Ev8.15 <- espectro_amplitude_evento(df1$chan.15, evento[15:16])
Ev8.16 <- espectro_amplitude_evento(df1$chan.16, evento[15:16])

#Evento 9:
Ev9.1 <- espectro_amplitude_evento(df1$chan.1, evento[17:18])
Ev9.2 <- espectro_amplitude_evento(df1$chan.2, evento[17:18])
Ev9.3 <- espectro_amplitude_evento(df1$chan.3, evento[17:18])
Ev9.4 <- espectro_amplitude_evento(df1$chan.4, evento[17:18])
Ev9.5 <- espectro_amplitude_evento(df1$chan.5, evento[17:18])
Ev9.6 <- espectro_amplitude_evento(df1$chan.6, evento[17:18])
Ev9.7 <- espectro_amplitude_evento(df1$chan.7, evento[17:18])
Ev9.8 <- espectro_amplitude_evento(df1$chan.8, evento[17:18])
Ev9.9 <- espectro_amplitude_evento(df1$chan.9, evento[17:18])
Ev9.10 <- espectro_amplitude_evento(df1$chan.10, evento[17:18])
Ev9.11 <- espectro_amplitude_evento(df1$chan.11, evento[17:18])
Ev9.12 <- espectro_amplitude_evento(df1$chan.12, evento[17:18])
Ev9.13 <- espectro_amplitude_evento(df1$chan.13, evento[17:18])
Ev9.14 <- espectro_amplitude_evento(df1$chan.14, evento[17:18])
Ev9.15 <- espectro_amplitude_evento(df1$chan.15, evento[17:18])
Ev9.16 <- espectro_amplitude_evento(df1$chan.16, evento[17:18])

#Evento 10:
Ev10.1 <- espectro_amplitude_evento(df1$chan.1, evento[19:20])
Ev10.2 <- espectro_amplitude_evento(df1$chan.2, evento[19:20])
Ev10.3 <- espectro_amplitude_evento(df1$chan.3, evento[19:20])
Ev10.4 <- espectro_amplitude_evento(df1$chan.4, evento[19:20])
Ev10.5 <- espectro_amplitude_evento(df1$chan.5, evento[19:20])
Ev10.6 <- espectro_amplitude_evento(df1$chan.6, evento[19:20])
Ev10.7 <- espectro_amplitude_evento(df1$chan.7, evento[19:20])
Ev10.8 <- espectro_amplitude_evento(df1$chan.8, evento[19:20])
Ev10.9 <- espectro_amplitude_evento(df1$chan.9, evento[19:20])
Ev10.10 <- espectro_amplitude_evento(df1$chan.10, evento[19:20])
Ev10.11 <- espectro_amplitude_evento(df1$chan.11, evento[19:20])
Ev10.12 <- espectro_amplitude_evento(df1$chan.12, evento[19:20])
Ev10.13 <- espectro_amplitude_evento(df1$chan.13, evento[19:20])
Ev10.14 <- espectro_amplitude_evento(df1$chan.14, evento[19:20])
Ev10.15 <- espectro_amplitude_evento(df1$chan.15, evento[19:20])
Ev10.16 <- espectro_amplitude_evento(df1$chan.16, evento[19:20])


#Questão 6:---------------------------------------------------------------------------------------------------
frequencia_mediana <- function(A){
  
  # Intensidade cumulativa - Somatório da amplitude atual mais a anterior
  # Intensidade do sinal - Somatório de todas amplitudes dividido por dois
  # Frequencia é selecionada na qual a intensidade cumulativa 
  # (ou seja, todos os valores de intensidade para frequências mais baixas e incluindo a intensidade focal) 
  # primeiro excede o valor calculado no passo 1.

  A1 <- A[[1]] #Recebendo lista com vetores tempo
  A2 <- A[[2]] #Recebendo lista com vetores espectro de amplitude
  
  #Criando lista para armazenar as amplitudes cumulativas e intensidade total do sinal 
  intensidade_acumulativa <- vector("list", length(A2))

  intensidade_sinal <- sum(A2)/2

  

    for(j in 1:length(A2)){
      if(j-1 == 0){
        intensidade_acumulativa[[j]] <- A2[[j]]
      }
      if(j-1 > 0){
        intensidade_acumulativa[[j]] <- intensidade_acumulativa[[j-1]] + A2[[j]]
      }
    }

  
  posicao <- NA
  
  for(i in 1:length(intensidade_acumulativa)){
    for(j in 1:length(intensidade_acumulativa[[i]])){
      if(abs(intensidade_acumulativa[[i]][[j]]) > abs(intensidade_sinal)){
        posicao <- A[[2]][[i]]
        break
      }
    }
  }
  
  return(posicao)
}

#Evento 1:
Med_canal1.1 <- frequencia_mediana(Ev1.1)
Med_canal1.2 <- frequencia_mediana(Ev1.2)
Med_canal1.3 <- frequencia_mediana(Ev1.3)
Med_canal1.4 <- frequencia_mediana(Ev1.4)
Med_canal1.5 <- frequencia_mediana(Ev1.5)
Med_canal1.6 <- frequencia_mediana(Ev1.6)
Med_canal1.7 <- frequencia_mediana(Ev1.7)
Med_canal1.8 <- frequencia_mediana(Ev1.8)
Med_canal1.9 <- frequencia_mediana(Ev1.9)
Med_canal1.10 <- frequencia_mediana(Ev1.10)
Med_canal1.11 <- frequencia_mediana(Ev1.11)
Med_canal1.12 <- frequencia_mediana(Ev1.12)
Med_canal1.13 <- frequencia_mediana(Ev1.13)
Med_canal1.14 <- frequencia_mediana(Ev1.14)
Med_canal1.15 <- frequencia_mediana(Ev1.15)
Med_canal1.16 <- frequencia_mediana(Ev1.16)

#Evento 2:
Med_canal2.1 <- frequencia_mediana(Ev2.1)
Med_canal2.2 <- frequencia_mediana(Ev2.2)
Med_canal2.3 <- frequencia_mediana(Ev2.3)
Med_canal2.4 <- frequencia_mediana(Ev2.4)
Med_canal2.5 <- frequencia_mediana(Ev2.5)
Med_canal2.6 <- frequencia_mediana(Ev2.6)
Med_canal2.7 <- frequencia_mediana(Ev2.7)
Med_canal2.8 <- frequencia_mediana(Ev2.8)
Med_canal2.9 <- frequencia_mediana(Ev2.9)
Med_canal2.10 <- frequencia_mediana(Ev2.10)
Med_canal2.11 <- frequencia_mediana(Ev2.11)
Med_canal2.12 <- frequencia_mediana(Ev2.12)
Med_canal2.13 <- frequencia_mediana(Ev2.13)
Med_canal2.14 <- frequencia_mediana(Ev2.14)
Med_canal2.15 <- frequencia_mediana(Ev2.15)
Med_canal2.16 <- frequencia_mediana(Ev2.16)


#Evento 3:
Med_canal3.1 <- frequencia_mediana(Ev3.1)
Med_canal3.2 <- frequencia_mediana(Ev3.2)
Med_canal3.3 <- frequencia_mediana(Ev3.3)
Med_canal3.4 <- frequencia_mediana(Ev3.4)
Med_canal3.5 <- frequencia_mediana(Ev3.5)
Med_canal3.6 <- frequencia_mediana(Ev3.6)
Med_canal3.7 <- frequencia_mediana(Ev3.7)
Med_canal3.8 <- frequencia_mediana(Ev3.8)
Med_canal3.9 <- frequencia_mediana(Ev3.9)
Med_canal3.10 <- frequencia_mediana(Ev3.10)
Med_canal3.11 <- frequencia_mediana(Ev3.11)
Med_canal3.12 <- frequencia_mediana(Ev3.12)
Med_canal3.13 <- frequencia_mediana(Ev3.13)
Med_canal3.14 <- frequencia_mediana(Ev3.14)
Med_canal3.15 <- frequencia_mediana(Ev3.15)
Med_canal3.16 <- frequencia_mediana(Ev3.16)


#Evento 4:
Med_canal4.1 <- frequencia_mediana(Ev4.1)
Med_canal4.2 <- frequencia_mediana(Ev4.2)
Med_canal4.3 <- frequencia_mediana(Ev4.3)
Med_canal4.4 <- frequencia_mediana(Ev4.4)
Med_canal4.5 <- frequencia_mediana(Ev4.5)
Med_canal4.6 <- frequencia_mediana(Ev4.6)
Med_canal4.7 <- frequencia_mediana(Ev4.7)
Med_canal4.8 <- frequencia_mediana(Ev4.8)
Med_canal4.9 <- frequencia_mediana(Ev4.9)
Med_canal4.10 <- frequencia_mediana(Ev4.10)
Med_canal4.11 <- frequencia_mediana(Ev4.11)
Med_canal4.12 <- frequencia_mediana(Ev4.12)
Med_canal4.13 <- frequencia_mediana(Ev4.13)
Med_canal4.14 <- frequencia_mediana(Ev4.14)
Med_canal4.15 <- frequencia_mediana(Ev4.15)
Med_canal4.16 <- frequencia_mediana(Ev4.16)


#Evento 5:
Med_canal5.1 <- frequencia_mediana(Ev5.1)
Med_canal5.2 <- frequencia_mediana(Ev5.2)
Med_canal5.3 <- frequencia_mediana(Ev5.3)
Med_canal5.4 <- frequencia_mediana(Ev5.4)
Med_canal5.5 <- frequencia_mediana(Ev5.5)
Med_canal5.6 <- frequencia_mediana(Ev5.6)
Med_canal5.7 <- frequencia_mediana(Ev5.7)
Med_canal5.8 <- frequencia_mediana(Ev5.8)
Med_canal5.9 <- frequencia_mediana(Ev5.9)
Med_canal5.10 <- frequencia_mediana(Ev5.10)
Med_canal5.11 <- frequencia_mediana(Ev5.11)
Med_canal5.12 <- frequencia_mediana(Ev5.12)
Med_canal5.13 <- frequencia_mediana(Ev5.13)
Med_canal5.14 <- frequencia_mediana(Ev5.14)
Med_canal5.15 <- frequencia_mediana(Ev5.15)
Med_canal5.16 <- frequencia_mediana(Ev5.16)


#Evento 6:
Med_canal6.1 <- frequencia_mediana(Ev6.1)
Med_canal6.2 <- frequencia_mediana(Ev6.2)
Med_canal6.3 <- frequencia_mediana(Ev6.3)
Med_canal6.4 <- frequencia_mediana(Ev6.4)
Med_canal6.5 <- frequencia_mediana(Ev6.5)
Med_canal6.6 <- frequencia_mediana(Ev6.6)
Med_canal6.7 <- frequencia_mediana(Ev6.7)
Med_canal6.8 <- frequencia_mediana(Ev6.8)
Med_canal6.9 <- frequencia_mediana(Ev6.9)
Med_canal6.10 <- frequencia_mediana(Ev6.10)
Med_canal6.11 <- frequencia_mediana(Ev6.11)
Med_canal6.12 <- frequencia_mediana(Ev6.12)
Med_canal6.13 <- frequencia_mediana(Ev6.13)
Med_canal6.14 <- frequencia_mediana(Ev6.14)
Med_canal6.15 <- frequencia_mediana(Ev6.15)
Med_canal6.16 <- frequencia_mediana(Ev6.16)


#Evento 7:
Med_canal7.1 <- frequencia_mediana(Ev7.1)
Med_canal7.2 <- frequencia_mediana(Ev7.2)
Med_canal7.3 <- frequencia_mediana(Ev7.3)
Med_canal7.4 <- frequencia_mediana(Ev7.4)
Med_canal7.5 <- frequencia_mediana(Ev7.5)
Med_canal7.6 <- frequencia_mediana(Ev7.6)
Med_canal7.7 <- frequencia_mediana(Ev7.7)
Med_canal7.8 <- frequencia_mediana(Ev7.8)
Med_canal7.9 <- frequencia_mediana(Ev7.9)
Med_canal7.10 <- frequencia_mediana(Ev7.10)
Med_canal7.11 <- frequencia_mediana(Ev7.11)
Med_canal7.12 <- frequencia_mediana(Ev7.12)
Med_canal7.13 <- frequencia_mediana(Ev7.13)
Med_canal7.14 <- frequencia_mediana(Ev7.14)
Med_canal7.15 <- frequencia_mediana(Ev7.15)
Med_canal7.16 <- frequencia_mediana(Ev7.16)


#Evento 8:
Med_canal8.1 <- frequencia_mediana(Ev8.1)
Med_canal8.2 <- frequencia_mediana(Ev8.2)
Med_canal8.3 <- frequencia_mediana(Ev8.3)
Med_canal8.4 <- frequencia_mediana(Ev8.4)
Med_canal8.5 <- frequencia_mediana(Ev8.5)
Med_canal8.6 <- frequencia_mediana(Ev8.6)
Med_canal8.7 <- frequencia_mediana(Ev8.7)
Med_canal8.8 <- frequencia_mediana(Ev8.8)
Med_canal8.9 <- frequencia_mediana(Ev8.9)
Med_canal8.10 <- frequencia_mediana(Ev8.10)
Med_canal8.11 <- frequencia_mediana(Ev8.11)
Med_canal8.12 <- frequencia_mediana(Ev8.12)
Med_canal8.13 <- frequencia_mediana(Ev8.13)
Med_canal8.14 <- frequencia_mediana(Ev8.14)
Med_canal8.15 <- frequencia_mediana(Ev8.15)
Med_canal8.16 <- frequencia_mediana(Ev8.16)


#Evento 9:
Med_canal9.1 <- frequencia_mediana(Ev9.1)
Med_canal9.2 <- frequencia_mediana(Ev9.2)
Med_canal9.3 <- frequencia_mediana(Ev9.3)
Med_canal9.4 <- frequencia_mediana(Ev9.4)
Med_canal9.5 <- frequencia_mediana(Ev9.5)
Med_canal9.6 <- frequencia_mediana(Ev9.6)
Med_canal9.7 <- frequencia_mediana(Ev9.7)
Med_canal9.8 <- frequencia_mediana(Ev9.8)
Med_canal9.9 <- frequencia_mediana(Ev9.9)
Med_canal9.10 <- frequencia_mediana(Ev9.10)
Med_canal9.11 <- frequencia_mediana(Ev9.11)
Med_canal9.12 <- frequencia_mediana(Ev9.12)
Med_canal9.13 <- frequencia_mediana(Ev9.13)
Med_canal9.14 <- frequencia_mediana(Ev9.14)
Med_canal9.15 <- frequencia_mediana(Ev9.15)
Med_canal9.16 <- frequencia_mediana(Ev9.16)


#Evento 10:
Med_canal10.1 <- frequencia_mediana(Ev10.1)
Med_canal10.2 <- frequencia_mediana(Ev10.2)
Med_canal10.3 <- frequencia_mediana(Ev10.3)
Med_canal10.4 <- frequencia_mediana(Ev10.4)
Med_canal10.5 <- frequencia_mediana(Ev10.5)
Med_canal10.6 <- frequencia_mediana(Ev10.6)
Med_canal10.7 <- frequencia_mediana(Ev10.7)
Med_canal10.8 <- frequencia_mediana(Ev10.8)
Med_canal10.9 <- frequencia_mediana(Ev10.9)
Med_canal10.10 <- frequencia_mediana(Ev10.10)
Med_canal10.11 <- frequencia_mediana(Ev10.11)
Med_canal10.12 <- frequencia_mediana(Ev10.12)
Med_canal10.13 <- frequencia_mediana(Ev10.13)
Med_canal10.14 <- frequencia_mediana(Ev10.14)
Med_canal10.15 <- frequencia_mediana(Ev10.15)
Med_canal10.16 <- frequencia_mediana(Ev10.16)

#Listas com canais de 1 a 16:
F_med_canal1 <- list(Med_canal1.1, Med_canal2.1, Med_canal3.1, Med_canal4.1, Med_canal5.1, Med_canal6.1,
                Med_canal7.1, Med_canal8.1, Med_canal9.1, Med_canal10.1)
Canal1 <- list(Ev1.1, Ev2.1, Ev3.1, Ev4.1, Ev5.1, Ev6.1, Ev7.1, Ev8.1, Ev9.1, Ev10.1)


F_med_canal2 <- list(Med_canal1.2, Med_canal2.2, Med_canal3.2, Med_canal4.2, Med_canal5.2, Med_canal6.2,
                     Med_canal7.2, Med_canal8.2, Med_canal9.2, Med_canal10.2)
Canal2 <- list(Ev1.2, Ev2.2, Ev3.2, Ev4.2, Ev5.2, Ev6.2, Ev7.2, Ev8.2, Ev9.2, Ev10.2)


F_med_canal3 <- list(Med_canal1.3, Med_canal2.3, Med_canal3.3, Med_canal4.3, Med_canal5.3, Med_canal6.3,
                     Med_canal7.3, Med_canal8.3, Med_canal9.3, Med_canal10.3)
Canal3 <- list(Ev1.3, Ev2.3, Ev3.3, Ev4.3, Ev5.3, Ev6.3, Ev7.3, Ev8.3, Ev9.3, Ev10.3)


F_med_canal4 <- list(Med_canal1.4, Med_canal2.4, Med_canal3.4, Med_canal4.4, Med_canal5.4, Med_canal6.4,
                     Med_canal7.4, Med_canal8.4, Med_canal9.4, Med_canal10.4)
Canal4 <- list(Ev1.4, Ev2.4, Ev3.4, Ev4.4, Ev5.4, Ev6.4, Ev7.4, Ev8.4, Ev9.4, Ev10.4)


F_med_canal5 <- list(Med_canal1.5, Med_canal2.5, Med_canal3.5, Med_canal4.5, Med_canal5.5, Med_canal6.5,
                     Med_canal7.5, Med_canal8.5, Med_canal9.5, Med_canal10.5)
Canal5 <- list(Ev1.5, Ev2.5, Ev3.5, Ev4.5, Ev5.5, Ev6.5, Ev7.5, Ev8.5, Ev9.5, Ev10.5)


F_med_canal6 <- list(Med_canal1.6, Med_canal2.6, Med_canal3.6, Med_canal4.6, Med_canal5.6, Med_canal6.6,
                     Med_canal7.6, Med_canal8.6, Med_canal9.6, Med_canal10.6)
Canal6 <- list(Ev1.6, Ev2.6, Ev3.6, Ev4.6, Ev5.6, Ev6.6, Ev7.6, Ev8.6, Ev9.6, Ev10.6)


F_med_canal7 <- list(Med_canal1.7, Med_canal2.7, Med_canal3.7, Med_canal4.7, Med_canal5.7, Med_canal6.7,
                     Med_canal7.7, Med_canal8.7, Med_canal9.7, Med_canal10.7)
Canal7 <- list(Ev1.7, Ev2.7, Ev3.7, Ev4.7, Ev5.7, Ev6.7, Ev7.7, Ev8.7, Ev9.7, Ev10.7)


F_med_canal8 <- list(Med_canal1.8, Med_canal2.8, Med_canal3.8, Med_canal4.8, Med_canal5.8, Med_canal6.8,
                     Med_canal7.8, Med_canal8.8, Med_canal9.8, Med_canal10.8)
Canal8 <- list(Ev1.8, Ev2.8, Ev3.8, Ev4.8, Ev5.8, Ev6.8, Ev7.8, Ev8.8, Ev9.8, Ev10.8)


F_med_canal9 <- list(Med_canal1.9, Med_canal2.9, Med_canal3.9, Med_canal4.9, Med_canal5.9, Med_canal6.9,
                     Med_canal7.9, Med_canal8.9, Med_canal9.9, Med_canal10.9)
Canal9 <- list(Ev1.9, Ev2.9, Ev3.9, Ev4.9, Ev5.9, Ev6.9, Ev7.9, Ev8.9, Ev9.9, Ev10.9)


F_med_canal10 <- list(Med_canal1.10, Med_canal2.10, Med_canal3.10, Med_canal4.10, Med_canal5.10, Med_canal6.10,
                     Med_canal7.10, Med_canal8.10, Med_canal9.10, Med_canal10.10)
Canal10 <- list(Ev1.10, Ev2.10, Ev3.10, Ev4.10, Ev5.10, Ev6.10, Ev7.10, Ev8.10, Ev9.10, Ev10.10)


F_med_canal11 <- list(Med_canal1.11, Med_canal2.11, Med_canal3.11, Med_canal4.11, Med_canal5.11, Med_canal6.11,
                     Med_canal7.11, Med_canal8.11, Med_canal9.11, Med_canal10.11)
Canal11 <- list(Ev1.11, Ev2.11, Ev3.11, Ev4.11, Ev5.11, Ev6.11, Ev7.11, Ev8.11, Ev9.11, Ev10.11)


F_med_canal12 <- list(Med_canal1.12, Med_canal2.12, Med_canal3.12, Med_canal4.12, Med_canal5.12, Med_canal6.12,
                     Med_canal7.12, Med_canal8.12, Med_canal9.12, Med_canal10.12)
Canal12 <- list(Ev1.12, Ev2.12, Ev3.12, Ev4.12, Ev5.12, Ev6.12, Ev7.12, Ev8.12, Ev9.12, Ev10.12)


F_med_canal13 <- list(Med_canal1.13, Med_canal2.13, Med_canal3.13, Med_canal4.13, Med_canal5.13, Med_canal6.13,
                     Med_canal7.13, Med_canal8.13, Med_canal9.13, Med_canal10.13)
Canal13 <- list(Ev1.13, Ev2.13, Ev3.13, Ev4.13, Ev5.13, Ev6.13, Ev7.13, Ev8.13, Ev9.13, Ev10.13)


F_med_canal14 <- list(Med_canal1.14, Med_canal2.14, Med_canal3.14, Med_canal4.14, Med_canal5.14, Med_canal6.14,
                     Med_canal7.14, Med_canal8.14, Med_canal9.14, Med_canal10.14)
Canal14 <- list(Ev1.14, Ev2.14, Ev3.14, Ev4.14, Ev5.14, Ev6.14, Ev7.14, Ev8.14, Ev9.14, Ev10.14)


F_med_canal15 <- list(Med_canal1.15, Med_canal2.15, Med_canal3.15, Med_canal4.15, Med_canal5.15, Med_canal6.15,
                     Med_canal7.15, Med_canal8.15, Med_canal9.15, Med_canal10.15)
Canal15 <- list(Ev1.15, Ev2.15, Ev3.15, Ev4.15, Ev5.15, Ev6.15, Ev7.15, Ev8.15, Ev9.15, Ev10.15)


F_med_canal16 <- list(Med_canal1.16, Med_canal2.16, Med_canal3.16, Med_canal4.16, Med_canal5.16, Med_canal6.16,
                     Med_canal7.16, Med_canal8.16, Med_canal9.16, Med_canal10.16)
Canal16 <- list(Ev1.16, Ev2.16, Ev3.16, Ev4.16, Ev5.16, Ev6.16, Ev7.16, Ev8.16, Ev9.16, Ev10.16)

#Listas com eventos de 1 a 10:
F_evento1 <- list(Med_canal1.1, Med_canal1.2, Med_canal1.3, Med_canal1.4,
                  Med_canal1.5, Med_canal1.6, Med_canal1.7, Med_canal1.8, 
                  Med_canal1.9, Med_canal1.10, Med_canal1.11, Med_canal1.12,
                  Med_canal1.13, Med_canal1.14, Med_canal1.15, Med_canal1.16)

evento1 <- list(Ev1.1, Ev1.2, Ev1.3, Ev1.4, Ev1.5, Ev1.6, Ev1.7, Ev1.8, 
                Ev1.9, Ev1.10, Ev1.11, Ev1.12, Ev1.13,Ev1.14, Ev1.15, Ev1.16)


F_evento2 <- list(Med_canal2.1, Med_canal2.2, Med_canal2.3, Med_canal2.4,
                  Med_canal2.5, Med_canal2.6, Med_canal2.7, Med_canal2.8, 
                  Med_canal2.9, Med_canal2.10, Med_canal2.11, Med_canal2.12,
                  Med_canal2.13, Med_canal2.14, Med_canal2.15, Med_canal2.16)

evento2 <- list(Ev2.1, Ev2.2, Ev2.3, Ev2.4, Ev2.5, Ev2.6, Ev2.7, Ev2.8, 
                Ev2.9, Ev2.10, Ev2.11, Ev2.12, Ev2.13,Ev2.14, Ev2.15, Ev2.16)


F_evento3 <- list(Med_canal3.1, Med_canal3.2, Med_canal3.3, Med_canal3.4,
                  Med_canal3.5, Med_canal3.6, Med_canal3.7, Med_canal3.8, 
                  Med_canal3.9, Med_canal3.10, Med_canal3.11, Med_canal3.12,
                  Med_canal3.13, Med_canal3.14, Med_canal3.15, Med_canal3.16)

evento3 <- list(Ev3.1, Ev3.2, Ev3.3, Ev3.4, Ev3.5, Ev3.6, Ev3.7, Ev3.8, 
                Ev3.9, Ev3.10, Ev3.11, Ev3.12, Ev3.13,Ev3.14, Ev3.15, Ev3.16)

F_evento4 <- list(Med_canal4.1, Med_canal4.2, Med_canal4.3, Med_canal4.4,
                  Med_canal4.5, Med_canal4.6, Med_canal4.7, Med_canal4.8, 
                  Med_canal4.9, Med_canal4.10, Med_canal4.11, Med_canal4.12,
                  Med_canal4.13, Med_canal4.14, Med_canal4.15, Med_canal4.16)

evento4 <- list(Ev4.1, Ev4.2, Ev4.3, Ev4.4, Ev4.5, Ev4.6, Ev4.7, Ev4.8, 
                Ev4.9, Ev4.10, Ev4.11, Ev4.12, Ev4.13,Ev4.14, Ev4.15, Ev4.16)


F_evento5 <- list(Med_canal5.1, Med_canal5.2, Med_canal5.3, Med_canal5.4,
                  Med_canal5.5, Med_canal5.6, Med_canal5.7, Med_canal5.8, 
                  Med_canal5.9, Med_canal5.10, Med_canal5.11, Med_canal5.12,
                  Med_canal5.13, Med_canal5.14, Med_canal5.15, Med_canal5.16)

evento5 <- list(Ev5.1, Ev5.2, Ev5.3, Ev5.4, Ev5.5, Ev5.6, Ev5.7, Ev5.8, 
                Ev5.9, Ev5.10, Ev5.11, Ev5.12, Ev5.13,Ev5.14, Ev5.15, Ev5.16)


F_evento6 <- list(Med_canal6.1, Med_canal6.2, Med_canal6.3, Med_canal6.4,
                  Med_canal6.5, Med_canal6.6, Med_canal6.7, Med_canal6.8, 
                  Med_canal6.9, Med_canal6.10, Med_canal6.11, Med_canal6.12,
                  Med_canal6.13, Med_canal6.14, Med_canal6.15, Med_canal6.16)

evento6 <- list(Ev6.1, Ev6.2, Ev6.3, Ev6.4, Ev6.5, Ev6.6, Ev6.7, Ev6.8, 
                Ev6.9, Ev6.10, Ev6.11, Ev6.12, Ev6.13,Ev6.14, Ev6.15, Ev6.16)

F_evento7 <- list(Med_canal7.1, Med_canal7.2, Med_canal7.3, Med_canal7.4,
                  Med_canal7.5, Med_canal7.6, Med_canal7.7, Med_canal7.8, 
                  Med_canal7.9, Med_canal7.10, Med_canal7.11, Med_canal7.12,
                  Med_canal7.13, Med_canal7.14, Med_canal7.15, Med_canal7.16)

evento7 <- list(Ev7.1, Ev7.2, Ev7.3, Ev7.4, Ev7.5, Ev7.6, Ev7.7, Ev7.8, 
                Ev7.9, Ev7.10, Ev7.11, Ev7.12, Ev7.13,Ev7.14, Ev7.15, Ev7.16)

F_evento8 <- list(Med_canal8.1, Med_canal8.2, Med_canal8.3, Med_canal8.4,
                  Med_canal8.5, Med_canal8.6, Med_canal8.7, Med_canal8.8, 
                  Med_canal8.9, Med_canal8.10, Med_canal8.11, Med_canal8.12,
                  Med_canal8.13, Med_canal8.14, Med_canal8.15, Med_canal8.16)

evento8 <- list(Ev8.1, Ev8.2, Ev8.3, Ev8.4, Ev8.5, Ev8.6, Ev8.7, Ev8.8, 
                Ev8.9, Ev8.10, Ev8.11, Ev8.12, Ev8.13,Ev8.14, Ev8.15, Ev8.16)


F_evento9 <- list(Med_canal9.1, Med_canal9.2, Med_canal9.3, Med_canal9.4,
                  Med_canal9.5, Med_canal9.6, Med_canal9.7, Med_canal9.8, 
                  Med_canal9.9, Med_canal9.10, Med_canal9.11, Med_canal9.12,
                  Med_canal9.13, Med_canal9.14, Med_canal9.15, Med_canal9.16)

evento9 <- list(Ev9.1, Ev9.2, Ev9.3, Ev9.4, Ev9.5, Ev9.6, Ev9.7, Ev9.8, 
                Ev9.9, Ev9.10, Ev9.11, Ev9.12, Ev9.13,Ev9.14, Ev9.15, Ev9.16)


F_evento10 <- list(Med_canal10.1, Med_canal10.2, Med_canal10.3, Med_canal10.4,
                  Med_canal10.5, Med_canal10.6, Med_canal10.7, Med_canal10.8, 
                  Med_canal10.9, Med_canal10.10, Med_canal10.11, Med_canal10.12,
                  Med_canal10.13, Med_canal10.14, Med_canal10.15, Med_canal10.16)

evento10 <- list(Ev10.1, Ev10.2, Ev10.3, Ev10.4, Ev10.5, Ev10.6, Ev10.7, Ev10.8, 
                Ev10.9, Ev10.10, Ev10.11, Ev10.12, Ev10.13,Ev10.14, Ev10.15, Ev10.16)

#Plotando gráficos da frequência mediana ao longo do tempo

graf_freq_Mediana <- function(A, B){

  tempo <- rep(NA, length(A))
  
  for(i in 1:length(A)){
    for(j in 1:length(B[[i]][[1]])){
      if(abs(B[[i]][[2]][[j]]) == abs(A[[i]])){
        tempo[i] <- B[[i]][[1]][[j]]
      }
      
    }
  }
  
  FREQ_MEDIANA <- rep(NA, length(A))
  
  for(i in 1:length(A)){
    FREQ_MEDIANA[i] <- abs(A[[i]])
  }
 
  return(qplot(tempo, FREQ_MEDIANA, geom=c("point", "line")))
}

#EVENTOS 1 A 10:
graf_freq_Mediana(F_evento1, evento1)
graf_freq_Mediana(F_evento2, evento2)
graf_freq_Mediana(F_evento3, evento3)
graf_freq_Mediana(F_evento4, evento4)
graf_freq_Mediana(F_evento5, evento5)
graf_freq_Mediana(F_evento6, evento6)
graf_freq_Mediana(F_evento7, evento7)
graf_freq_Mediana(F_evento8, evento8)
graf_freq_Mediana(F_evento9, evento9)
graf_freq_Mediana(F_evento10, evento10)



graf_freq_Mediana(F_med_canal1, Canal1)
graf_freq_Mediana(F_med_canal2, Canal2)
graf_freq_Mediana(F_med_canal3, Canal3)
graf_freq_Mediana(F_med_canal4, Canal4)
graf_freq_Mediana(F_med_canal5, Canal5)
graf_freq_Mediana(F_med_canal6, Canal6)
graf_freq_Mediana(F_med_canal7, Canal7)
graf_freq_Mediana(F_med_canal8, Canal8)
graf_freq_Mediana(F_med_canal9, Canal9)
graf_freq_Mediana(F_med_canal10, Canal10)
graf_freq_Mediana(F_med_canal11, Canal11)
graf_freq_Mediana(F_med_canal12, Canal12)
graf_freq_Mediana(F_med_canal13, Canal13)
graf_freq_Mediana(F_med_canal14, Canal14)
graf_freq_Mediana(F_med_canal15, Canal15)
graf_freq_Mediana(F_med_canal16, Canal16)




















