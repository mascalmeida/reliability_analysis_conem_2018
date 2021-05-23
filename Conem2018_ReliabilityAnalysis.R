## CONEM

## Clear all ----

# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())

# Bibliotecas----

if(!require("readxl")) install.packages("readxl") ; library(readxl)
if(!require("dplyr")) install.packages("dplyr") ; library(dplyr)
if(!require("survminer")) install.packages("survminer") ; library(survminer)    # Usar funcao surv_fit
if(!require("survival")) install.packages("survival") ; library(survival)       # Usar a funcao surv
if(!require("ggthemes")) install.packages("ggthemes") ; library(ggthemes)       # Usar temas nos graficos
if(!require("ggplot2")) install.packages("ggplot2") ; library(ggplot2) 
if(!require("reshape2")) install.packages("reshape2") ; library(reshape2) 
if(!require("gridExtra")) install.packages("gridExtra") ; library(gridExtra)
if(!require("stringr")) install.packages("stringr") ; library(stringr)
if(!require("lubridate")) install.packages("lubridate") ; library(lubridate)

# importando banco de dados ----

setwd("C:/Users/lukas/Desktop/compilado/Conem2018_ReliabilityAnalysis")

dados_conem <- read_excel("data/dados.xlsx", 
                          col_types = c("text", "numeric", "skip", 
                                        "date", "date", "date", "date", 
                                        "skip", "skip", "skip", "text", 
                                        "numeric", "text", "skip", "text", "numeric", 
                                        "skip", "skip", "skip", "skip", 
                                        "skip", "text", "skip", "skip"))

# filtrando dados da zona 42 ----

dados_conem <- dados_conem %>% 
  filter(BAIRRO %in% c("BOCA DO RIO", 
                       "IMBUÍ", "PATAMARES", 
                       "PIATÃ", "PITUAÇU", "LUCAS10",
                       "LUCAS16", "LUCAS5", "JAGUARIBE")) %>% 
  mutate(MES = substr(DATA_SOLICITACAO, 6, 7), 
         DIA = substr(DATA_SOLICITACAO, 9, 10),
         HORA = substr(HORA_SOLICITACAO, 12, 19)) %>% 
  select(ANO, MES, DIA, HORA, BAIRRO, DISCRIMINACAO) %>% 
  mutate(MINUTO = substr(HORA, 4, 5), HORA = substr(HORA, 1, 2))



# Analise exploratoria----

d_exp <- dados_conem

gg.ano <- d_exp %>%
  ggplot(mapping = aes(x = as.factor(ANO))) +
  geom_bar(aes(fill = MES), color = "gray20") +
  geom_text(aes(label = scales::percent(round((..count..)/3116, 3))), 
            stat= "count", vjust = -.1, size = 6.5) +
  labs(title = "Solicitações de serviço por Ano\n", 
       x = "Ano", y = "Solicitações\n", 
       fill = "Mês") +
  theme_stata() +
  scale_fill_manual(values = c("cadetblue3", "chocolate2", "chartreuse3", "coral1",
                               "cyan2", "darkgoldenrod3", "brown2", "darkgreen",
                               "blueviolet", "khaki1", "hotpink", "tan3",
                               "white"), label = c("Jan","Fev","Mar","Abr",
                                                   "Mai", "Jun", "Jul", "Ago",
                                                   "Set", "Out", "Nov", "Dez",
                                                   "NA"))

gg.ano <- ggpar(p = gg.ano,
      font.title    = c(26, "bold", "gray23"),        
      font.x        = c(25, "plain", "gray23"),          
      font.y        = c(25, "plain", "gray23"),      
      font.xtickslab = c(21, "bold", "gray23"),
      font.ytickslab = c(21, "bold", "gray23"),
      font.legend = c(23, "plain", "gray23"))

gg.ano

g00 = 1

if(g00 == 0){
  
  ggsave(filename = "gg.ano.png", gg.ano,                                                    # Salvando grafico
         width = 13, height = 8.75, dpi = 250)
  
}

# Inserindo Censura Intervalar no Banco de Dados----

# Criando objetos para identificar mudanca de mes e ano bissexto
# Criando variavel com unidade de tempo em dias (tempo.R)
# Criando variavel admitindo tzero o tempo inicial do estudo
# Criando varivael tempo.L

meses = as.numeric(c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334))
meses_b = as.numeric(c(0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335))
bissexto = as.numeric(c(2008, 2012))

dados_conem <- dados_conem %>% 
  mutate(t.R = as.numeric(MINUTO)*(1/1440) + as.numeric(HORA)*(1/24) + as.numeric(DIA) + 
           (ifelse(ANO == bissexto[1] | ANO == bissexto[2], meses_b[as.numeric(MES)], meses[as.numeric(MES)])) + 
           as.numeric(ANO)*365) %>%
  arrange(ANO, MES, DIA, HORA, MINUTO) %>%
  mutate(t.R = t.R - t.R[1]) %>% 
  mutate(t.L = ifelse(HORA >= 12, t.R, -Inf)) %>% 
  mutate(turno = ifelse(HORA >= 5 | HORA <= 12, "Manhã",
                        ifelse(HORA <= 17, "Tarde", "Noite")))

# Criando intervalos

dados_conem$t.R[1] = 0.0000001

for (i in c(2:length(dados_conem$t.L))) {
  dados_conem$t.L[i] <- ifelse(dados_conem$t.L[i] == -Inf, 
                              ifelse(dados_conem$DIA[i] == dados_conem$DIA[i-1], 
                                     dados_conem$t.L[i-1], 
                                     dados_conem$t.R[i-1]), 
                              dados_conem$t.L[i])
}

dados_conem <- dados_conem %>% 
  mutate(t.R = c(t.R[2:length(t.R)], t.R[length(t.R)]), 
         t.L = c(t.L[2:length(t.L)], t.L[length(t.L)]))

# Criando intervalo dos tempos entre falhas

dados_conem <- dados_conem %>% 
  mutate(t.failure.L = (c(t.R[1], diff(t.R)))) %>% 
  mutate(t.failure.R = (t.failure.L + (t.R - t.L))) %>% 
  mutate(t.medio = ifelse(t.failure.R == t.failure.L, t.failure.R, (t.failure.R+t.failure.L)/2)) %>% 
  mutate(status = 1) %>% 
  select(t.failure.L, t.failure.R, BAIRRO, DISCRIMINACAO, t.medio, turno, status)

dados_conem$t.failure.R[1] <- dados_conem$t.failure.L[1]

# Separando os bancos de dados----
d_total <- dados_conem %>% 
  select(t.medio, turno, status)

d_registro <- dados_conem %>% 
  filter(grepl("HIDROMETRO", DISCRIMINACAO) == FALSE & 
           grepl("metro", DISCRIMINACAO) == FALSE &
           grepl("LIMPEZA", DISCRIMINACAO) == FALSE &
           grepl("DESCARGA", DISCRIMINACAO) == FALSE &
           grepl("impeza", DISCRIMINACAO) == FALSE &
           grepl("SOLIC.TRANSF", DISCRIMINACAO) == FALSE &
           grepl("HIDRÔMETRO", DISCRIMINACAO) == FALSE ) %>% 
  select(t.failure.L, t.failure.R, BAIRRO, DISCRIMINACAO)

d_hidrometro <- dados_conem %>%
  filter(grepl("LIMPEZA", DISCRIMINACAO) == FALSE &
           grepl("DESCARGA", DISCRIMINACAO) == FALSE &
           grepl("SOLIC.TRANSF", DISCRIMINACAO) == FALSE &
           grepl("REGISTRO", DISCRIMINACAO) == FALSE &
           grepl("egistro", DISCRIMINACAO) == FALSE &
           grepl("hidrante", DISCRIMINACAO) == FALSE &
           grepl("impeza", DISCRIMINACAO) == FALSE &
           grepl("EXEC. RAMAL", DISCRIMINACAO) == FALSE) %>% 
  select(t.failure.L, t.failure.R, BAIRRO, DISCRIMINACAO)

# Graficos dos turnos----

d_t <- d_total %>% 
  mutate(turno = ifelse(is.na(turno) == T, "Sem informação", turno))

gg.turno <- ggplot(data = d_t, mapping = aes(x = as.factor(turno))) + 
  theme_stata() +
  geom_bar(aes(fill = turno), color = "black") +
  labs(title = "Solicitações de Serviço por Turno\n", 
       x = "Turno", y = "Solicitações\n", 
       fill = "Turno") + 
  scale_x_discrete(limits=c("Manhã", "Tarde", "Noite", "Sem informação")) + 
  theme(legend.position='none') +
  geom_text(aes(label = scales::percent(round((..count..)/3116, 3))), 
            stat= "count", vjust = -.1, size = 6.5)

gg.turno <- ggpar(p = gg.turno,
                         font.title    = c(26, "bold", "gray23"),        
                         font.x        = c(25, "plain", "gray23"),          
                         font.y        = c(25, "plain", "gray23"),      
                         font.xtickslab = c(21, "bold", "gray23"),
                         font.ytickslab = c(21, "bold", "gray23"),
                         font.legend = c(23, "plain", "gray23"))

gg.turno

km.total <- survfit(Surv(t.medio, status)~turno, data = d_total)

sum.km.total <- surv_summary(km.total)

gg.km.total <- ggsurvplot(km.total, 
                      risk.table = T, 
                      conf.int = F, 
                      surv.median.line = "hv",
                      xlim = c(0, 25), 
                      break.time.by = 5) +
  labs(title = "Estimador Kaplan-Meier para Válvulas\n", 
       x = "Tempo (dias)", 
       y = "Probabilidade")

g01 = 1

if(g01 == 0){
  
  ggsave(filename = "gg.turno.png", (gg.turno),                                                    # Salvando grafico
         width = 13, height = 7, dpi = 250)
  
  ggsave(filename = "gg.km.total.png", print(gg.km.total),                                                    # Salvando grafico
         width = 13, height = 7.7, dpi = 250)
  
}

# Nao-Parametrica para Registros----

# Criando objeto utilizando o metodo Turnbull (Intervalar)

# Sem covariar
regis.np <- survfit(Surv(time = t.failure.L, time2 = t.failure.R, type="interval2")~1, data=d_registro)

resumo.regis.np <- surv_summary(regis.np)


# Grafico - Criando curva NP sem covariar

regis.np$n.event <- round(regis.np$n.event)     # Arredondando o numero de evento topo

gg.regis.np <- ggsurvplot(fit = regis.np, 
                          size = 1.75,
                          palette = "dodgerblue4", 
                          conf.int = T, 
                          surv.median.line = "hv", 
                          risk.table = "percentage", 
                          cumevents = T, 
                          tables.height = 1/6, 
                          ggtheme = theme_stata(), 
                          tables.theme = theme_stata(), 
                          title = "Curva de Sobrevivência Não-Paramétrica\n", 
                          xlab = "\nDias",
                          ylab = "Probabilidade de S(t)",
                          legend.title = "Indivíduo",
                          legend.labs = "Válvulas",
                          break.time.by = 10,
                          surv.scale = "percent",
                          #xlim = c(0,40),
                          conf.int.style = "step",
                          risk.table.y.text = F,
                          cumevents.y.text = F,
                          fontsize = 7,
                          tables.y.text.col = T,
                          risk.table.title = "Porcetagem em Risco (%)", 
                          cumevents.title = "Quantidade de Eventos Acumulados")

gg.regis.np <- ggpar(p = gg.regis.np,
                     font.title    = c(26, "bold"),       
                     font.x        = c(25),          
                     font.y        = c(25),      
                     font.xtickslab = c(21, "bold"),
                     font.ytickslab = c(21, "bold"),
                     font.legend = c(23, "plain", "dodgerblue4"))

gg.regis.np

# Dando um zoom em xlim = 0 ate 3.5

gg.zoomregis.np <- ggsurvplot(fit = regis.np, 
                              size = 1.75,
                              palette = "dodgerblue4", 
                              conf.int = T, 
                              surv.median.line = "hv", 
                              risk.table = "percentage", 
                              cumevents = T, 
                              tables.height = 1/6, 
                              ggtheme = theme_stata(), 
                              tables.theme = theme_stata(), 
                              title = "Curva de Sobrevivência Não-Paramétrica\n", 
                              xlab = "\nDias",
                              ylab = "Probabilidade de S(t)\n",
                              legend.title = "Indivíduo",
                              legend.labs = "Válvulas",
                              break.time.by = 0.15,
                              surv.scale = "percent",
                              xlim = c(0,1.5),
                              conf.int.style = "step",
                              risk.table.y.text = F,
                              cumevents.y.text = F,
                              fontsize = 7,
                              tables.y.text.col = T,
                              risk.table.title = "Porcetagem em Risco (%)", 
                              cumevents.title = "Quantidade de Eventos Acumulados")

gg.zoomregis.np <- ggpar(p = gg.zoomregis.np,
                         font.title    = c(26, "bold"),       
                         font.x        = c(25),          
                         font.y        = c(25),      
                         font.xtickslab = c(21, "bold"),
                         font.ytickslab = c(21, "bold"),
                         font.legend = c(23, "plain", "dodgerblue4"))

gg.zoomregis.np

# Acumulada

regisacc.np <- regis.np %>% 
  surv_summary()

gg.accregis.np <- ggplot(regisacc.np, aes(x = time)) +
  geom_line(aes(y = 1-surv), color = "dodgerblue4", size = 1.75) +
  theme_stata() +
  labs(title = "Função de Distribuição Acumulada\n",
       x = "\nDias",
       y = "FDC (t)\n",
       subtitle = "Tempo Mediano de Sobrevivência (1,12 Dias)") +
  scale_x_continuous(breaks = seq(0, 2.24, by = 0.2), limits = c(0,2.24)) +
  geom_segment(aes(x = 1.12, y = 0, xend = 1.12, yend = 0.5), linetype="dashed") +
  geom_segment(aes(x = 0, y = 0.5, xend = 1.12, yend = 0.5), linetype="dashed")

gg.accregis.np <- ggpar(p = gg.accregis.np,
                         font.title    = c(26, "bold"),       
                         font.x        = c(25),          
                         font.y        = c(25),      
                         font.xtickslab = c(21, "bold"),
                         font.ytickslab = c(21, "bold"),
                         font.legend = c(23, "plain", "dodgerblue4"),
                         font.subtitle = c(21, "bold"))

gg.accregis.np

g1 = 1
if(g1 == 0){
  ggsave(filename = "gg.regis.np.png", print(gg.regis.np),                                                    # Salvando grafico
         width = 13, height = 7+(7/3), dpi = 250)
  ggsave(filename = "gg.zoomregis.np.png", print(gg.zoomregis.np),                                                    # Salvando grafico
         width = 13, height = 7+(7/3), dpi = 250)
  ggsave(filename = "gg.accregis.np.png", (gg.accregis.np),                                                    # Salvando grafico
         width = 13, height = 7, dpi = 250)
}
# Nao-Parametrica para Hidrometro----

# Criando objeto utilizando o metodo Turnbull (Intervalar)

# Sem covariar
hidro.np <- survfit(Surv(time = t.failure.L, time2 = t.failure.R, type="interval2")~1, data=d_hidrometro)

resumo.hidro.np <- surv_summary(hidro.np)


# Grafico - Criando curva NP sem covariar

hidro.np$n.event <- round(hidro.np$n.event)     # Arredondando o numero de evento topo

gg.hidro.np <- ggsurvplot(fit = hidro.np, 
                          size = 1.75,
                          palette = "firebrick", 
                          conf.int = T, 
                          surv.median.line = "hv", 
                          risk.table = "percentage", 
                          cumevents = T, 
                          tables.height = 1/6, 
                          ggtheme = theme_stata(), 
                          tables.theme = theme_stata(), 
                          title = "Curva de Sobrevivência Não-Paramétrica\n", 
                          xlab = "\nDias",
                          ylab = "Probabilidade de S(t)",
                          legend.title = "Indivíduo",
                          legend.labs = "IMV's",
                          break.time.by = 10,
                          surv.scale = "percent",
                          #xlim = c(0,40),
                          conf.int.style = "step",
                          risk.table.y.text = F,
                          cumevents.y.text = F,
                          fontsize = 7,
                          tables.y.text.col = T,
                          risk.table.title = "Porcetagem em Risco (%)", 
                          cumevents.title = "Quantidade de Eventos Acumulados")

gg.hidro.np <- ggpar(p = gg.hidro.np,
                     font.title    = c(26, "bold", "firebrick"),        
                     font.x        = c(25),          
                     font.y        = c(25),      
                     font.xtickslab = c(21, "bold"),
                     font.ytickslab = c(21, "bold"),
                     font.legend = c(23, "plain", "firebrick"))

gg.hidro.np

# Dando um zoom em xlim = 0 ate 3.5

gg.zoomhidro.np <- ggsurvplot(fit = hidro.np, 
                              size = 1.75,
                              palette = "firebrick", 
                              conf.int = T, 
                              surv.median.line = "hv", 
                              risk.table = "percentage", 
                              cumevents = T, 
                              tables.height = 1/6, 
                              ggtheme = theme_stata(), 
                              tables.theme = theme_stata(), 
                              title = "Curva de Sobrevivência Não-Paramétrica\n", 
                              xlab = "\nDias",
                              ylab = "Probabilidade de S(t)",
                              legend.title = "Indivíduo",
                              legend.labs = "IMV's",
                              break.time.by = 0.1,
                              surv.scale = "percent",
                              xlim = c(0,1),
                              conf.int.style = "step",
                              risk.table.y.text = F,
                              cumevents.y.text = F,
                              fontsize = 7,
                              tables.y.text.col = T,
                              risk.table.title = "Porcetagem em Risco (%)", 
                              cumevents.title = "Quantidade de Eventos Acumulados")

gg.zoomhidro.np <- ggpar(p = gg.zoomhidro.np,
                         font.title    = c(26, "bold", "firebrick"),        
                         font.x        = c(25),          
                         font.y        = c(25),      
                         font.xtickslab = c(21, "bold"),
                         font.ytickslab = c(21, "bold"),
                         font.legend = c(23, "plain", "firebrick"))

gg.zoomhidro.np

# Acumulada

hidroacc.np <- hidro.np %>% 
  surv_summary()

gg.acchidro.np <- ggplot(hidroacc.np, aes(x = time)) +
  geom_line(aes(y = 1-surv), color = "firebrick", size = 1.75) +
  theme_stata() +
  labs(title = "Função de Distribuição Acumulada\n",
       x = "\nDias",
       y = "FDC (t)\n",
       subtitle = "Tempo Mediano de Sobrevivência (0,32 Dias)") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0,1)) +
  geom_segment(aes(x = 0.32, y = 0, xend = 0.32, yend = 0.5), linetype="dashed") +
  geom_segment(aes(x = 0, y = 0.5, xend = 0.32, yend = 0.5), linetype="dashed")

gg.acchidro.np <- ggpar(p = gg.acchidro.np,
                        font.title    = c(26, "bold", "firebrick"),       
                        font.x        = c(25),          
                        font.y        = c(25),      
                        font.xtickslab = c(21, "bold"),
                        font.ytickslab = c(21, "bold"),
                        font.legend = c(23, "plain", "firebrick"),
                        font.subtitle = c(21, "bold", "firebrick"))

gg.acchidro.np

g2 = 0
if(g2 == 1){
  ggsave(filename = "gg.hidro.np.png", print(gg.hidro.np),                                                    # Salvando grafico
         width = 13, height = 7+(7/3), dpi = 250)
  ggsave(filename = "gg.zoomhidro.np.png", print(gg.zoomhidro.np),                                                    # Salvando grafico
         width = 13, height = 7+(7/3), dpi = 250)
  ggsave(filename = "gg.acchidro.np.png", (gg.acchidro.np),                                                    # Salvando grafico
         width = 13, height = 7, dpi = 250)
}
# Comparacao hidro e regis----

d_hidrometro <- d_hidrometro %>% 
  mutate(tipo = "Hidrometro")

d_registro <- d_registro %>% 
  mutate(tipo = "Valvulas")

d_exp_comp <- rbind(d_registro, d_hidrometro) %>% 
  select(tipo)

gg.comp <- ggplot(data = d_exp_comp, mapping = aes(x = tipo)) +
  theme_stata() +
  geom_bar(aes(fill = tipo)) +
  labs(title = "Solicitações de Serviço por Componente\n", 
       x = "Componente", y = "Solicitações\n", 
       fill = "Componente") +
  geom_text(aes(label = scales::percent(round((..count..)/3112, 2))), 
            stat= "count", vjust = -.2, size = 6.5)  + 
  theme(legend.position='none')

gg.comp <- ggpar(p = gg.comp,
                  font.title    = c(26, "bold", "gray23"),        
                  font.x        = c(25, "plain", "gray23"),          
                  font.y        = c(25, "plain", "gray23"),      
                  font.xtickslab = c(21, "bold", "gray23"),
                  font.ytickslab = c(21, "bold", "gray23"),
                  font.legend = c(23, "plain", "gray23"))

gg.comp

g02 = 1

if(g02 == 0){
  
  ggsave(filename = "gg.comp.png", gg.comp,                                                    # Salvando grafico
         width = 13, height = 7, dpi = 250)
  
}
