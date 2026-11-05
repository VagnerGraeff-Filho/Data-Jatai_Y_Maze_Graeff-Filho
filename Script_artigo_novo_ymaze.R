# Bibliotecas -------------------------------------------------------------
library(readr)
library(tidyverse)
library(glmmTMB)
library(magrittr) # Pacote necessário para o %>%
library(DHARMa)
library(ggridges)
library(patchwork)
library(sjPlot)
library(readxl)
library(MuMIn)
library(sjPlot)
library(tidyverse)
library(tidyr)
library(dplyr)
library(patchwork)
library(glmmTMB)
library(DHARMa)
library(performance)
library(DHARMa)
library(emmeans)
library(sjPlot)
library(ggeffects)
library(multcomp)
library(car)

# ############# Análise inicial 2024 - Teste de treinamento ###### --------
df_2024 <- read_excel("Teste_y_escolha.xlsx", sheet = 'uniao_planilhas')

df_2024 <- df_2024 %>% 
  filter (!is.na(Choice)) %>% #Escolhendo apenas as que fizeram alguma decisão
  mutate(across(c(ID, Scent, Side, Choice, Block), as.factor))

df_2024$Scent <- factor(df_2024$Scent, levels = c("Unconditioned","Rosemary","Geranium"))
df_2024$Choice <- factor(df_2024$Choice, levels = c("R","G"))
# df_2024$Scent <- factor(df_2024$Scent, levels = c("Geranium","Rosemary","Unconditioned"))
df_2024$ID <- interaction (df_2024$ID, df_2024$Scent, df_2024$Block)
str(df_2024)

#Resumo
df_2024 %>%
  group_by(Scent) %>% 
  summarise(
    total = n(),                               # Conta o número de linhas (N total)
    n_valid = sum(!is.na(Choice))               # Conta apenas onde Scent NÃO é NA
  )
  
  

##        Resumo de dados

 df_2024 %>%
  group_by(Scent, Choice) %>%
  summarise(Total = n(), .groups = "drop") %>%
  # Transforma G e R em colunas
  pivot_wider(names_from = Choice, values_from = Total, values_fill = 0) %>%
  # Calcula as métricas de comparação
  mutate(
    Diferenca_G_R = G - R,
    Proporcao_G = G / (G + R),
    # Calcula a razão (Quantas vezes G é maior que R)
    Razao_G_R = round(G / R, 2) 
  )

# Visualizar o resultado
print(df_2024)


#Exploração 

ggplot(df_2024, aes(x = Scent, fill = Choice)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) 

### Modelos para teste inciial - Seleção
df_2024$Scent <- factor(df_2024$Scent, levels = c("Rosemary","Unconditioned","Geranium"))

model_Choice <- glmmTMB(Choice ~ Scent + (1|ID) + (1|Block) + (1|Date), 
                         data = df_2024, 
                         family = binomial())

model_Choice.1 <- glmmTMB(Choice ~ Scent + (1|ID) + (1|Block), 
                         data = df_2024, 
                         family = binomial())

model_Choice.2 <- glmmTMB(Choice ~ Scent + (1|ID), 
                         data = df_2024, 
                         family = binomial())

model_Choice.3 <- glmmTMB(Choice ~ Scent, 
                         data = df_2024, 
                         family = binomial())

model.sel(model_Choice, model_Choice.1,model_Choice.2,model_Choice.3) # Modelo mais simples é melhor
plot(simulateResiduals(model_Choice.3)) # Resíduos Ok

summary(model_Choice.3) # HÁ APENAS DIFERENÇA QUANDO HÁ TREINAMENTO COM GERÂNIO
tab_model(model_Choice.3)

### Modelo nulos
df_2024.null <- df_2024 %>% 
  filter (Scent == "Unconditioned")

model_Choice.3.null <- glmmTMB(Choice ~ 1, 
                          data = df_2024.null, 
                          family = binomial())

summary(model_Choice.3.null)
tab_model (model_Choice.3.null)
df_2024.null <- df_2024 %>% 
  filter (Scent == "Unconditioned")

model_Choice.3.null <- glmmTMB(Choice ~ 1, 
                          data = df_2024.null, 
                          family = binomial())

summary(model_Choice.3.null)
tab_model (model_Choice.3.null)

df_2024.null <- df_2024 %>% 
  filter (Scent == "Geranium")

model_Choice.3.null <- glmmTMB(Choice ~ 1, 
                          data = df_2024.null, 
                          family = binomial())

summary(model_Choice.3.null)
tab_model (model_Choice.3.null)

df_2024.null <- df_2024 %>% 
  filter (Scent == "Rosemary")

model_Choice.3.null <- glmmTMB(Choice ~ 1, 
                          data = df_2024.null, 
                          family = binomial())

summary(model_Choice.3.null)
tab_model (model_Choice.3.null)
#### Grafrico Oficial

df_plot_first <- df_2024 %>%
  filter(Choice != 'NA') %>% 
  group_by(Scent) %>%
  count(Choice) %>%
  mutate(prop = n / sum(n))

df_plot_first$prop_corrigida <- ifelse(df_plot_first$Choice == "R", -df_plot_first$prop, df_plot_first$prop)

ggplot(df_plot_first, aes(x = Scent, y = prop_corrigida, fill = Choice )) +
  geom_bar ( stat = 'identity') +
  coord_flip()+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  xlab('First Choice')+
  ylab(' Proportion (0,1)')+
  scale_fill_manual(values = c("#8C1E3E", "#1E8C6C")) +
  scale_y_continuous(labels = abs)+
  theme(
    legend.position = c(""),
    plot.title = element_text(hjust = 0.5, face = "plain", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(face = "plain", color = "black"),
    axis.title.y = element_text(face = "bold", color = "black", size = 12),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12, color = 'black'),
    axis.text.y = element_text(angle = 0, hjust = 0.5, size = 12, color = 'black'),
    legend.title = element_blank(),
    legend.text = element_text(size = 12, face = 'plain'),
    panel.border = element_blank(),  # Remove todas as bordas
    panel.grid.major = element_line(color = "white", linetype = "dashed"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.line.y.left = element_line(color = 'black'),  # Remove a borda do eixo y esquerda
    axis.line.x = element_line(color = 'black'),  
    axis.line.y = element_line(color = 'red'),  
    axis.line.x.top = element_line(color = "red")
  )

# Não centralizado
plot.1 <- ggplot(df_2024, aes(x = Scent, fill = Choice)) +
  geom_bar(position = 'fill') +
  coord_flip() +
  # Linha central que agora representa o 0
  geom_hline(yintercept = 0.5, linewidth = 1, linetype = "dashed", color = "black") +
  xlab('Conditioned scent') +
  ylab('Proportion') +
  scale_fill_manual(values = c("#1E8C6C", "#8C1E3E")) +
  # Ajuste da escala: define os pontos e subtrai 0.5 de cada um para centralizar no zero
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.25), 
    labels = c("0.5", "0.25", "0", "0.25", "0.5")
  ) +
  scale_x_discrete(labels = c("Unconditioned (49)", "Rosemary (41)", "Geranium (68)")) +
  labs (legend = 'Side')+
  theme(
    legend.position = "right", # Ajustado para aparecer, ou c() se quiser posição exata
    plot.title = element_text(hjust = 0.5, face = "plain", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(face = "plain", color = "black", size = 14),
    axis.title.y = element_text(face = "plain", color = "black", size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14, color = 'black', face = 'plain'),
    axis.text.y = element_text(angle = 0, hjust = 0.5, size = 14, color = 'black'),
    legend.title = element_text(size = 14, face = 'plain'),
    legend.text = element_text(size = 14, face = 'plain'),
    panel.border = element_blank(),
    panel.grid.major = element_line(color = "gray90", linetype = "dashed"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.line.y.left = element_line(color = 'black'),
    axis.line.x = element_line(color = 'black'),
    base_size = 14
  )

###    Aluvial
# Instalar se necessário: install.packages("ggalluvial")
library(tidyverse)
library(ggalluvial)

# 1. Preparar os dados (contagem das conexões)
df_alluvial <- df_2024 %>%
  group_by(Scent, Choice) %>%
  summarise(Freq = n(), .groups = "drop")

# 2. Criar o Alluvial Plot
ggplot(df_alluvial,
       aes(y = Freq, axis1 = Scent, axis2 = Choice)) +
  geom_alluvium(aes(fill = Choice), width = 0.1, alpha = 0.9) +
  geom_stratum(width = 1/6, fill = "grey90", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
  scale_x_discrete(limits = c("Treinamento (Scent)", "Escolha (Choice)"), 
                   expand = c(.05, .05)) +
  scale_fill_manual(values = c("#1E8C6C", "#8C1E3E")) +
  labs(title = "Fluxo de Decisão das Abelhas",
       y = "Número de Indivíduos (n)") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    legend.position = "none"
  )

# Preparando os dados no formato 'lodes' para manter os grupos separados
library(ggalluvial)

df_lodes <- df_alluvial %>%
  mutate(Scent = factor(Scent, levels = c("Unconditioned", "Rosemary", "Geranium"))) %>%
  to_lodes_form(key = "Passo", axes = c("Scent", "Choice"))

plot.2 <- ggplot(df_lodes,
       aes(x = Passo, stratum = stratum, alluvium = alluvium, y = Freq)) +
  # As linhas de fluxo (conexões)
  geom_alluvium(aes(fill = stratum), width = 1/4, alpha = 1, color = "black") +
  # As caixas (estratos) - agora elas ficam separadas por Scent no lado esquerdo
  geom_stratum(width = 1/4, fill = "grey100", color = "black") +
  # Rótulos dentro das caixas
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3.5) +
  # Escalas de cores (ajustadas para incluir os nomes dos Aromas e as Escolhas)
  scale_fill_manual(values = c("G" = "#1E8C6C", "R" = "#8C1E3E", 
                               "Geranium" = "#8C1E3E", "Rosemary" = "#1E8C6C", "Unconditioned" = "gray70")) +
  scale_x_discrete(limits = c("Scent", "Choice"), expand = c(.1, .1)) +
  labs(title = "",
       y = "Bees (n)",
       x = "") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )+
  coord_flip()

library(patchwork)

plot.1 / plot.2


############ Validação do método, tempo observando ################ -----------------------------------
df_tempo <- read_excel("Planilha_teste_y.xlsx")

df_tempo$ID <- as.factor (df_tempo$ID)

df_tempo_long <- df_tempo %>%
  pivot_longer(
    cols = c(Tempo_Geranio, Tempo_Alecrim), # Colunas que serão "empilhadas"
    names_to = "Scent",                     # Nome da nova coluna de categoria
    values_to = "Time"          # Nome da nova coluna de valores (Variável Resposta)
  )

df_tempo_long$Teste <- factor(df_tempo_long$Teste, levels = c("GR","RG","NN"))
df_tempo_long$Scent <- factor(df_tempo_long$Scent, levels = c("Tempo_Geranio","Tempo_Alecrim"))

####### Grafico
ggplot (df_tempo_long, aes (y = Time, x = Teste, fill = Scent ))+
  geom_boxplot()+
  geom_jitter()

## Modelos

mod_tempo.2 <- glmmTMB( Time ~ Scent * Teste,
                        family = tweedie (),
                        data = df_tempo_long)

mod_tempo.3 <- glmmTMB( Time ~ Scent * Teste,
                        family = gaussian (),
                        data = df_tempo_long)

AIC(mod_tempo.2, mod_tempo.3)

plot(simulateResiduals(mod_tempo.3))

summary(mod_tempo.2)
tab_model (mod_tempo.2)
# Graficos
library(ggplot2)
library(dplyr)

df_sum <- df_tempo_long %>%
  group_by(Scent, Teste) %>%
  summarise(
    mean_time = mean(Time, na.rm = TRUE),
    se_time = sd(Time, na.rm = TRUE)/sqrt(n())
  )

plot.barras.1 <- ggplot(df_tempo_long, aes(x = Teste, y = Time, fill = Scent))+
  geom_bar(data = df_sum,
           aes(x = Teste, y = mean_time, fill = Scent),
           position = position_dodge(width = .75),
           stat = "identity",
           width = 0.75,
           color = NA,
           inherit.aes = FALSE,
           show.legend = F)+
  geom_segment(data = df_sig,
               aes(x = xmin, xend = xmax,
                   y = y + 18, yend = y+18),
               inherit.aes = FALSE,
               linewidth = 0.6,
               label = 'j') +
  geom_segment(data = df_sig,
               aes(x = xmin, xend = xmin ,
                   y = y + 18, yend = y +18  - 0.05 * max(df_resumo$media)),
               inherit.aes = FALSE,
               linewidth = 0.6) +
  geom_segment(data = df_sig,
               aes(x = xmax, xend = xmax,
                   y = y + 18, yend = y +18 - 0.05 * max(df_resumo$media)),
               inherit.aes = FALSE,
               linewidth = 0.6) +
  
  geom_errorbar(data = df_sum,
                aes(x = Teste, fill = Scent,
                    ymin = mean_time - se_time,
                    ymax = mean_time + se_time),
                position = position_dodge(width = .75),
                width = 0.2,
                linewidth = 0.7,
                inherit.aes = FALSE)+
  scale_x_discrete(labels = c("GR" = "Geranium \n conditioned \n(17)", 
                              "NN" = "Un\nconditioned \n(16)", 
                              "RG" = "Rosemary \n conditioned \n(16)")) +
  geom_text(data = df_sig,
            aes(x = (xmin + xmax) / 2,
                y = y + 18 + 0.05  * max(df_resumo$media),
                label = c('*')),
            inherit.aes = FALSE,
            size = 6) +
  
  # geom_point(position = position_jitter(width = 0.08, height = 0),
  #            size = 2.5,
  #            alpha = 0.5,
  #            shape = 21,
  #            color = "black",
  #            fill = "black")+
  
  scale_fill_manual(values = c("#8C1E3E","#1E8C6C")) +
  
  labs(x = "",
       y = "Exploration time (s)")+
  theme_classic(base_size = 14) +
  theme(
    axis.title = element_text(size = 14, face = "plain"), # Títulos dos eixos X e Y
    axis.text = element_text(size = 14)                  # Nomes das categorias
  )
  
library(cowplot)

plot_grid(plot.1, plot.barras.1, 
          labels = c("a", "b"),        # Adiciona as tags desejadas
          label_size = 15,             # Define o tamanho da fonte
          label_fontface = "plain" ,
rel_widths = c(5.5, 4.5))# Remove o negrito (plain)

# Análise do tempo de observação ------------------------------------------
df_obs <- read_excel("Planilha_teste_y.xlsx", sheet = 'Página1')
View(df_obs)
str(df_obs)


# Correção colunas --------------------------------------------------------
df_obs$Tempo <- as.numeric(df_obs$Tempo)
df_obs$ID <- as.factor(df_obs$ID)

df_obs <- df_obs %>% 
  mutate (local_b = if_else(Local == "n", 0 , 1 ))

df_obs <- df_obs %>%
  mutate(local_b = if_else(Local == "n", 0L, 1L))

#Exploração -------------------
df_fil <- df_obs %>% 
  filter (ID %in% c('a1','a2')) %>% 
  filter ( Tempo < 90)

ggplot(df_obs, aes(y = local_b, x = Tempo)) +
  geom_point(alpha = 0.5, shape = 21, color = "black") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  theme_minimal()

# Modelagem
mod_obs.1 <- glmmTMB(local_b ~ Tempo + (1|ID),
                     data = df_obs,
                     family = binomial (link = 'logit'))

mod_obs.2 <- glmmTMB(local_b ~ Tempo,
                     data = df_obs,
                     family = binomial ())

mod_obs.3 <- glmmTMB(local_b ~ poly(Tempo, 2) + (1|ID),
                     data = df_obs,
                     family = binomial(link = "logit"))


model.sel(mod_obs.1,mod_obs.2,mod_obs.3)  #
plot(simulateResiduals(mod_obs.3))
tab_model(mod_obs.3)
summary(mod_obs.3)


plot_model(mod_obs.3, type="pred", terms="Tempo")



#       ###### Grafico
# Extração das estimativas marginais
plot_data <- plot_model(mod_obs.3,
                        type = "pred",
                        terms = "Tempo",
                        show.data = FALSE,
                        ci.lvl = 0.95,
                        transform = NULL)   # já retorna na escala da resposta
df_plot <- plot_data$data
tempo_max <- plot_data$data$x[which.max(plot_data$data$predicted)]

ggplot(df_plot, aes(x = x, y = predicted)) +
  geom_jitter(size = 3,alpha = 0.6, height = 0.02)+ 
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.3) +
  geom_vline(xintercept = tempo_max, linetype = "dashed", color = "black", size = 0.8) +
  labs(x = "Time",
       y = "Exploring probability") +
  theme_classic()+
  theme(
    legend.position = "right", # Ajustado para aparecer, ou c() se quiser posição exata
    plot.title = element_text(hjust = 0.5, face = "plain", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(face = "plain", color = "black"),
    axis.title.y = element_text(face = "bold", color = "black", size = 12),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12, color = 'black'),
    axis.text.y = element_text(angle = 0, hjust = 0.5, size = 12, color = 'black'),
    legend.title = element_blank(),
    legend.text = element_text(size = 12, face = 'plain'),
    panel.border = element_blank(),
    panel.grid.major = element_line(color = "gray90", linetype = "dashed"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.line.y.left = element_line(color = 'black'),
    axis.line.x = element_line(color = 'black')
  )


# ################## ## Análise de inseticidas ####################### --------------------------------------------------

df <- read.csv("Teste_y_inseticidas - Planilha_limpa.csv", sep = ';')

df$Group <- factor(df$Group, levels = c('CONTROL','DEL','IMI','COM'))
df$Id <- factor(df$Id)
df$Choice <-  factor(df$Choice)
df$Escolha_total <- (df$Rosemary + df$Geranium)


df |> 
  group_by(Group) |> 
  summarise (n = n())

# Conversão da tabela Longa -----------------------------------------------------
df_long <- df %>%
  pivot_longer(
    cols = c(Rosemary, Geranium, Neutral_zone, Decision_time, Escolha_total), 
    names_to = "Aroma",                                       
    values_to = "Tempo_gasto"                                
  )
df_long$Id.uni <- interaction(df_long$Id, df_long$Group) 


df_long$Aroma <- factor(df_long$Aroma, levels = c('Geranium','Rosemary','Decision_time','Neutral_zone','Escolha_total'))



# Agrupamento -------------------------------------------------------------
df_agrup <- df_long %>% 
  group_by (Group, Aroma) %>% 
  summarise (med_tempo = mean (Tempo_gasto),
             sd_tempo = sd (Tempo_gasto),
             n = n(),
             se_tempo = sd_tempo/sqrt (n))

# Filtrar o df_long para manter apenas as duas colunas de escolhas
df_filtrado <- df_long %>% 
  filter (Aroma %in% c("Rosemay","Geranium")) %>% 
  filter (!is.na (Choice) & Choice != "morta")

ggplot(df_filtrado, aes(x = Group, fill = Choice)) +
  # O ggplot vai contar quantas vezes cada 'Choice' aparece por 'Group'
  geom_bar(position = "dodge") + 
  scale_fill_brewer(palette = "Set1") + # Sugestão de cores acadêmicas
  labs(x = "Tratamento", y = "Número de Abelhas", fill = "Escolha") +
  theme_classic()

### Modelos INSETICIDAS para escolha de aroma ------------
df_long.esc <- df_long %>% 
  filter (Aroma %in% c("Rosemary","Geranium")) %>% 
  filter ( Choice != c("morta")) %>% 
  filter ( Choice != is.na(Choice))

ggplot(df_long.esc, aes(x = Group, y = Tempo_gasto, fill = Aroma)) +
  geom_boxplot()+
  geom_jitter()+
  scale_fill_brewer(palette = "Set1") + # Sugestão de cores acadêmicas
  labs(x = "Tratamento", y = "Número de Abelhas", fill = "Escolha") +
  theme_classic()

modelo_nbin1.1 <- glmmTMB(Tempo_gasto ~ Group * Aroma, 
                          family = tweedie(), 
                          data = df_long.esc)

modelo_nbin1.2 <- glmmTMB(Tempo_gasto ~ Group * Aroma + (1|Id.uni), 
                          family = tweedie(), 
                          data = df_long.esc)


AIC(modelo_nbin1.1, modelo_nbin1.2) # MOdelo 6 é o melhor

plot(simulacoes <- simulateResiduals(modelo_nbin1.1))
summary(modelo_nbin1.1)
tab_model(modelo_nbin1.1)

emm <- emmeans(modelo_nbin1.1,
               ~ Group * Aroma,
               type = "response")
emm

contrast(emm,
         method = "pairwise",
         by = "Group",
         adjust = "tukey")

library(emmeans)


# Resumo estatístico
df_resumo <- df_long.esc %>%
  group_by(Group, Aroma) %>%
  summarise(
    media = mean(Tempo_gasto, na.rm = TRUE),
    sd = sd(Tempo_gasto, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    .groups = "drop"
  )

df_resumo

library(dplyr)

# calcular altura máxima por grupo
df_sig <- df_resumo %>%
  group_by(Group) %>%
  summarise(y_pos = max(media + se)) %>%
  ungroup()

# manter apenas grupos com diferença significativa
df_sig <- df_sig %>%
  filter(Group == "CONTROL") %>%   # edite aqui se quiser outros grupos
  mutate(
    xmin = as.numeric(factor(Group)) - 0.2,
    xmax = as.numeric(factor(Group)) + 0.2,
    y = y_pos + 0.05 * max(df_resumo$media),
    label = "†"   # símbolo manual
  )

plot.insect.2 <- ggplot(df_resumo,
       aes(x = Group,
           y = media,
           fill = Aroma)) +
  geom_col(position = position_dodge(width = 0.8),
           width = 0.7) +
  geom_errorbar(aes(ymin = media - se,
                    ymax = media + se),
                position = position_dodge(width = 0.8),
                width = 0.2) +
  scale_fill_manual(values = c("Rosemary" = "#1E8C6C",
                               "Geranium" = "#8C1E3E")) +
  
  geom_segment(data = df_sig,
               aes(x = xmin, xend = xmax,
                   y = y, yend = y),
               inherit.aes = FALSE,
               linewidth = 0.6,
               label = 'j') +
  geom_segment(data = df_sig,
               aes(x = xmin, xend = xmin,
                   y = y, yend = y - 0.03 * max(df_resumo$media)),
               inherit.aes = FALSE,
               linewidth = 0.6) +
  geom_segment(data = df_sig,
               aes(x = xmax, xend = xmax,
                   y = y, yend = y - 0.03 * max(df_resumo$media)),
               inherit.aes = FALSE,
               linewidth = 0.6) +
  geom_text(data = df_sig,
            aes(x = (xmin + xmax) / 2,
                y = y + 0.05 * max(df_resumo$media),
                label = c('*')),
            inherit.aes = FALSE,
            size = 6) +
  labs(x = "",
       y = "Mean time of exploration (s)",
       fill = "Scent")+
  theme_classic()+
  theme(
    legend.position = c(0.85, 0.9),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.title.y = element_text(color = "black", size = 12),
    legend.text = element_text(color = "black", size = 12),
    legend.title = element_text(color = "black", size = 12)
  )


ggplot(df_resumo,
       aes(x = Group,
           y = media,
           fill = Aroma)) +
  geom_col(position = position_dodge(width = 0.8),
           width = 0.7) +
  geom_errorbar(aes(ymin = media - se,
                    ymax = media + se),
                position = position_dodge(width = 0.8),
                width = 0.2) +
  scale_fill_manual(values = c("Rosemary" = "#1B9E77",
                               "Geranium" = "#D95F02")) +
  labs(x = "Tratamento",
       y = "Tempo médio observado",
       fill = "Aroma") +
  theme_classic()


# Nova modelagem 2026 (cbind) ---------------------------------------------
# A sintaxe cbind(sucessos, fracassos) diz ao R para modelar a proporção de geranium vs rosemary
df$Choice <- factor(df$Choice)
df_fil <- df |> 
  filter(Choice %in% c("Rosemary", "Geranium"))

modelo_glm <- glm(cbind(Geranium, Rosemary) ~ Group, 
                  data = df, 
                  family = quasibinomial(link = "logit"))

# 3. Visualizar o resultado do modelo
summary(modelo_glm)





# TEMPO DE DECISÃO ---------------------------------------------------------
df_decisao <- df_long %>% 
  subset (Aroma == c('Decision_time')) %>% 
  filter (Choice_1 == 1)


## modelagem de tempo de decisão

mod.2 <- glmmTMB(Tempo_gasto ~ Group + (1|Id.uni), 
                          family = gaussian (), 
                          data = df_decisao)

mod.1 <- glmmTMB(Tempo_gasto ~ Group + (1|Id.uni), 
                 family = tweedie (), 
                 data = df_decisao)

mod.1.1 <- glmmTMB(Tempo_gasto ~ Group, 
                 family = tweedie (), 
                 data = df_decisao)


AIC(mod.1, mod.2, mod.1.1) # mod.1.1 é o melhor

plot(simulacoes <- simulateResiduals(mod.1.1))
summary(mod.1.1)
tab_model(mod.1.1)

emm <- emmeans(mod.1.1, ~ Group, type = "response")
print(emm)
pairs(emm, adjust = "tukey")

especificos <- contrast(emm, method = list(
  "DEL - IMI" = c(0, 1, -1, 0),  # CTR=0, DEL=1, IMI=-1, COM=0
  "DEL - COM" = c(0, 1, 0, -1),  # CTR=0, DEL=1, IMI=0, COM=-1
  "IMI - COM" = c(0, 0, 1, -1)   # CTR=0, DEL=0, IMI=1, COM=-1
))
especificos

df_resumo.2 <- df_decisao %>%
  group_by(Group) %>%
  summarise(
    media = mean(Tempo_gasto, na.rm = TRUE),
    sd = sd(Tempo_gasto, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    .groups = "drop"
  )
df_resumo.2

df_sig.2 <- df_resumo.2 %>%
  group_by(Group) %>%
  summarise(y_pos = max(media + se)) %>%
  ungroup()

# manter apenas grupos com diferença significativa
df_sig.2 <- df_sig.2 %>%
  filter(Group == "CONTROL") %>%   # edite aqui se quiser outros grupos
  mutate(
    xmin = as.numeric(factor(Group)) - 0.2,
    xmax = as.numeric(factor(Group)) + 0.2,
    y = y_pos + 0.05 * max(df_resumo$media),
    label = "†"   # símbolo manual
  )

plot.desicion <- ggplot(df_resumo.2,
       aes(x = Group,
           y = media,
           fill = Group)) +
  geom_col(position = position_dodge(width = 0.8),
           width = 0.7,
           fill = 'grey',
           color = 'black',
           show.legend = F) +
  geom_errorbar(aes(ymin = media - se,
                    ymax = media + se),
                position = position_dodge(width = 0.8),
                width = 0.2,
                show.legend = F) +
  geom_text(aes(y = media + se + 0.05 * max(media),
                label = c('b','a','c','a')),
            position = position_dodge(width = 0.8),
            size = 6,
            show.legend = F) +
  scale_fill_manual(values = c("Rosemary" = "#1B9E77",
                               "Geranium" = "#D95F02")) +
  # scale_fill_brewer(palette = "Dark2")+
  labs(x = "",
       y = "Decision time (s)",
       fill = "Scent") +
  scale_x_discrete(labels = c(
    "CONTROL" = "CTRL \n(n = 19)",
    "DEL" = "DEL \n(n = 14)",
    "IMI" = "IMI \n(n = 14)",
    "COM" = "COM \n(n = 15)"
  ))+
  theme_classic() +
  theme(
    legend.position = c(0.85, 0.9),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.title.y = element_text(color = "black", size = 12),
    legend.text = element_text(color = "black", size = 12),
    legend.title = element_text(color = "black", size = 12)
  )




# Histograma dos dados
ggplot(df_decisao, aes(x = Tempo_gasto)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribuição de Tempo_gasto", x = "Tempo gasto", y = "Frequência")




modelo_nbin1.4 <- glmmTMB(Tempo_gasto ~ Group, 
                          family = nbinom1(link = 'log'), 
                          data = df_decisao)

modelo_nbin1.5 <- glmmTMB(Tempo_gasto ~ Group, 
                          family = poisson(link = 'log'), 
                          # dispformula = ~Group,
                          data = df_decisao)

modelo_nbin1.6 <- glmmTMB(Tempo_gasto ~ Group, 
                          family = nbinom2(link = 'log'),
                          dispformula = ~ Group,
                          data = df_decisao)
diagnose(modelo_nbin1.4)
AIC(modelo_nbin1.5, modelo_nbin1.4, modelo_nbin1.6) # MOdelo 6 é o melhor
Anova(modelo_nbin1.4, type = "III")

plot(simulacoes <- simulateResiduals(modelo_nbin1.6))
summary(modelo_nbin1.6)
tab_model(modelo_nbin1.6)

#Extração dos efeitos
predic_prop.d <-ggpredict(modelo_nbin1.6, type ="fixed" , terms = c("Group"), ci_level = 0.95)
predic_prop.d$predicted
predic_prop.d$conf.low
predic_prop.d$conf.high 
predic_prop.d$std.error


# Grafico de Decisão PRETO E BRANCO
plot_td <- ggplot(df_decisao, aes(y = Tempo_gasto, x = Group, color = Group))+
  geom_violin(width = 1,
              fill = NA,
              linewidth = 0.6,
              position = position_dodge(width = 0.75),
              color = 'black',
              show.legend = F)+
  geom_boxplot(width = 0.15,
               # fill = ,
               linewidth = 0.6,
               position = position_dodge(width = 0.45),
               color = 'black',
               show.legend = F,
               whiskers = F)+
  geom_jitter(size = 3.5,
              position = position_jitterdodge(jitter.width = 0.3, 
                                              dodge.width = 0.45),
              alpha = 0.6,
              show.legend = T)+
  geom_segment(data = predic_prop.d, aes(x = as.numeric(x),
                                         xend = as.numeric(x) + 0.25,
                                         y = predicted, yend = predicted),
               size = 1, color = "black",
               linetype = 'dotted',
               inherit.aes = F,
               show.legend = F) +
  geom_label(data = predic_prop.d, aes(x = x, 
                                       y = predicted,
                                       label = paste("µ̂ = ", round(predicted, 2))),
             fill = 'white', 
             color = "black",
             size = 4.5, 
             fontface = "bold",
             label.padding = unit(0.3, "lines"),
             label.size = 0.5,
             position = position_nudge(x = 0.35),
             show.legend = F) +
  geom_errorbar(data = predic_prop.d, aes(x = x,
                                          ymax = conf.high,
                                          ymin = conf.low),
                inherit.aes = F,
                width = 0.07,
                position = position_dodge(width = 1.2),
                show.legend =F,
                color = '#700',
                size = 1)+
  geom_point (data = predic_prop.d, 
              aes(y = predicted, x = x),
              size = 4.5,
              color = '#700',
              inherit.aes = F,
              position = position_dodge (width = 1.2),
              # position = position_nudge (x = 1.2),
              show.legend = F)+
  geom_text(data = predic_prop.d, aes(x = x,
                                      y = (- 1),
                                      # fill = group,
                                      label = c('r','*','*','**')),
            color = "black",
            size = 7,
            fontface = "plain",
            # label.padding = unit(0.3, "lines"), label.size = 0.5,
            position = position_dodge(width = 1.2),
            inherit.aes = F,
            show.legend = F) +
  theme_bw()+
  scale_color_manual(values = c('#000','#000','#000','#000','#928039','#928010'))+
  scale_fill_manual(values = c('white','white','#b65DdF','#892829','#928039','#928010'))+
  labs (x = '')+
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(face = "bold", color = "black", size = 14),
    axis.title.y = element_text(face = "bold", color = "black",size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14, color = 'black', face = 'plain'),
    axis.text.y = element_text(size = 14, color = 'black'),
    legend.position = 'none',
    legend.title = element_text(size = 12, face = "bold"),
    panel.border = element_blank(),  
    panel.grid.major.y = element_line(color = "grey", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.line.y.left = element_line(color = 'black'),  
    axis.line.x = element_line(color = 'black'),  
    axis.line.y = element_line(color = 'red'),  
    axis.line.x.top = element_line(color = "white"))+
  scale_x_discrete(labels = c("CONTROL" = "Control \n n = (19)", "DEL" = "Deltamethrin \n n = (14)", "IMI" = "Imidacloprid \n n = (14)", 'COM' = 'Combination \n n = (15)'))+
  labs (y = 'Time (s)')

# Neutral zone ------------------------------------------------------------
df_neutral <- df_long %>% 
  subset (Aroma == c('Neutral_zone'))


## modelagem de tempo de decisão

mod.2 <- glmmTMB(Tempo_gasto ~ Group + (1|Id.uni), 
                 family = gaussian (), 
                 data = df_neutral)

mod.1 <- glmmTMB(Tempo_gasto ~ Group + (1|Id.uni), 
                 family = tweedie (), 
                 data = df_neutral)

mod.1.1 <- glmmTMB(Tempo_gasto ~ Group, 
                   family = tweedie (), 
                   data = df_neutral)


AIC(mod.1, mod.2, mod.1.1) # mod.1.1 é o melhor

plot(simulacoes <- simulateResiduals(mod.1.1))
summary(mod.1.1)
tab_model(mod.1.1)

emm <- emmeans(mod.1.1, ~ Group, type = "response")
print(emm)
pairs(emm, adjust = "tukey")

especificos <- contrast(emm, method = list(
  "DEL - IMI" = c(0, 1, -1, 0),  # CTR=0, DEL=1, IMI=-1, COM=0
  "DEL - COM" = c(0, 1, 0, -1),  # CTR=0, DEL=1, IMI=0, COM=-1
  "IMI - COM" = c(0, 0, 1, -1)   # CTR=0, DEL=0, IMI=1, COM=-1
))
especificos

df_resumo.2 <- df_neutral %>%
  group_by(Group) %>%
  summarise(
    media = mean(Tempo_gasto, na.rm = TRUE),
    sd = sd(Tempo_gasto, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    .groups = "drop"
  )
df_resumo.2

df_sig.2 <- df_resumo.2 %>%
  group_by(Group) %>%
  summarise(y_pos = max(media + se)) %>%
  ungroup()

# manter apenas grupos com diferença significativa
df_sig.2 <- df_sig.2 %>%
  filter(Group == "CONTROL") %>%   # edite aqui se quiser outros grupos
  mutate(
    xmin = as.numeric(factor(Group)) - 0.2,
    xmax = as.numeric(factor(Group)) + 0.2,
    y = y_pos + 0.05 * max(df_resumo$media),
    label = "†"   # símbolo manual
  )

plot.neutral <- ggplot(df_resumo.2,
       aes(x = Group,
           y = media,
           fill = Group)) +
  geom_col(position = position_dodge(width = 0.8),
           width = 0.7,
           fill = 'grey',
           color = 'black',
           show.legend = F) +
  geom_errorbar(aes(ymin = media - se,
                    ymax = media + se),
                position = position_dodge(width = 0.8),
                width = 0.2,
                show.legend = F) +
  geom_text(aes(y = media + se + 0.05 * max(media),
                label = c('b','a','a','ab')),
            position = position_dodge(width = 0.8),
            size = 6,
            show.legend = F) +
  scale_fill_manual(values = c("Rosemary" = "#1B9E77",
                               "Geranium" = "#D95F02")) +
  # scale_fill_brewer(palette = "Dark2")+
  labs(x = "",
       y = "Time in neutral zone (s)",
       fill = "Scent") +
  scale_x_discrete(labels = c(
    "CONTROL" = "CTRL \n(n = 20)",
    "DEL" = "DEL \n(n = 20)",
    "IMI" = "IMI \n(n = 20)",
    "COM" = "COM \n(n = 20)"
  ))+
  theme_classic() +
  theme(
    legend.position = c(0.85, 0.9),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.title.y = element_text(color = "black", size = 12),
    legend.text = element_text(color = "black", size = 12),
    legend.title = element_text(color = "black", size = 12)
  )


plot_grid(plot.desicion, plot.neutral, 
          labels = c("a", "b"),        # Adiciona as tags desejadas
          label_size = 15,             # Define o tamanho da fonte
          label_fontface = "plain" ,
          rel_widths = c(5,5))# Remove o negrito (plain)

# Histograma dos dados
ggplot(df_decisao, aes(x = Tempo_gasto)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribuição de Tempo_gasto", x = "Tempo gasto", y = "Frequência")




modelo_nbin1.4 <- glmmTMB(Tempo_gasto ~ Group, 
                          family = nbinom1(link = 'log'), 
                          data = df_decisao)

modelo_nbin1.5 <- glmmTMB(Tempo_gasto ~ Group, 
                          family = poisson(link = 'log'), 
                          # dispformula = ~Group,
                          data = df_decisao)

modelo_nbin1.6 <- glmmTMB(Tempo_gasto ~ Group, 
                          family = nbinom2(link = 'log'),
                          dispformula = ~ Group,
                          data = df_decisao)
diagnose(modelo_nbin1.4)
AIC(modelo_nbin1.5, modelo_nbin1.4, modelo_nbin1.6) # MOdelo 6 é o melhor
Anova(modelo_nbin1.4, type = "III")

plot(simulacoes <- simulateResiduals(modelo_nbin1.6))
summary(modelo_nbin1.6)
tab_model(modelo_nbin1.6)

#Extração dos efeitos
predic_prop.d <-ggpredict(modelo_nbin1.6, type ="fixed" , terms = c("Group"), ci_level = 0.95)
predic_prop.d$predicted
predic_prop.d$conf.low
predic_prop.d$conf.high 
predic_prop.d$std.error


# Grafico de Decisão PRETO E BRANCO
plot_td <- ggplot(df_decisao, aes(y = Tempo_gasto, x = Group, color = Group))+
  geom_violin(width = 1,
              fill = NA,
              linewidth = 0.6,
              position = position_dodge(width = 0.75),
              color = 'black',
              show.legend = F)+
  geom_boxplot(width = 0.15,
               # fill = ,
               linewidth = 0.6,
               position = position_dodge(width = 0.45),
               color = 'black',
               show.legend = F,
               whiskers = F)+
  geom_jitter(size = 3.5,
              position = position_jitterdodge(jitter.width = 0.3, 
                                              dodge.width = 0.45),
              alpha = 0.6,
              show.legend = T)+
  geom_segment(data = predic_prop.d, aes(x = as.numeric(x),
                                         xend = as.numeric(x) + 0.25,
                                         y = predicted, yend = predicted),
               size = 1, color = "black",
               linetype = 'dotted',
               inherit.aes = F,
               show.legend = F) +
  geom_label(data = predic_prop.d, aes(x = x, 
                                       y = predicted,
                                       label = paste("µ̂ = ", round(predicted, 2))),
             fill = 'white', 
             color = "black",
             size = 4.5, 
             fontface = "bold",
             label.padding = unit(0.3, "lines"),
             label.size = 0.5,
             position = position_nudge(x = 0.35),
             show.legend = F) +
  geom_errorbar(data = predic_prop.d, aes(x = x,
                                          ymax = conf.high,
                                          ymin = conf.low),
                inherit.aes = F,
                width = 0.07,
                position = position_dodge(width = 1.2),
                show.legend =F,
                color = '#700',
                size = 1)+
  geom_point (data = predic_prop.d, 
              aes(y = predicted, x = x),
              size = 4.5,
              color = '#700',
              inherit.aes = F,
              position = position_dodge (width = 1.2),
              # position = position_nudge (x = 1.2),
              show.legend = F)+
  geom_text(data = predic_prop.d, aes(x = x,
                                      y = (- 1),
                                      # fill = group,
                                      label = c('r','*','*','**')),
            color = "black",
            size = 7,
            fontface = "plain",
            # label.padding = unit(0.3, "lines"), label.size = 0.5,
            position = position_dodge(width = 1.2),
            inherit.aes = F,
            show.legend = F) +
  theme_bw()+
  scale_color_manual(values = c('#000','#000','#000','#000','#928039','#928010'))+
  scale_fill_manual(values = c('white','white','#b65DdF','#892829','#928039','#928010'))+
  labs (x = '')+
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(face = "bold", color = "black", size = 14),
    axis.title.y = element_text(face = "bold", color = "black",size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14, color = 'black', face = 'plain'),
    axis.text.y = element_text(size = 14, color = 'black'),
    legend.position = 'none',
    legend.title = element_text(size = 12, face = "bold"),
    panel.border = element_blank(),  
    panel.grid.major.y = element_line(color = "grey", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.line.y.left = element_line(color = 'black'),  
    axis.line.x = element_line(color = 'black'),  
    axis.line.y = element_line(color = 'red'),  
    axis.line.x.top = element_line(color = "white"))+
  scale_x_discrete(labels = c("CONTROL" = "Control \n n = (19)", "DEL" = "Deltamethrin \n n = (14)", "IMI" = "Imidacloprid \n n = (14)", 'COM' = 'Combination \n n = (15)'))+
  labs (y = 'Time (s)')


# Proporção de escolhas GERANIUM/ROSEMARY ---------------------------------------------------
df_escolhas <- df_long %>% 
  filter (!is.na(Choice)) %>%
  subset (Aroma %in% c('Geranium','Rosemary')) 

ggplot(df_escolhas, aes( x = Group, y = Tempo_gasto, fill = Aroma))+
  geom_boxplot()+
  geom_violin()+
  geom_point()


modelo_zi <- glmmTMB(Tempo_gasto ~ Group * Aroma + 
                       (1 | Id.uni), 
                     family = poisson(link = 'log'), 
                     zi = ~ Group + Aroma,  # Parte zero-inflada
                     data = df_escolhas)

modelo_gau <- glmmTMB(Tempo_gasto ~ Group * Aroma + 
                        (1 | Id.uni), 
                      family = gaussian(), 
                      zi = ~ Group + Aroma,  # Parte zero-inflada
                      data = df_escolhas)

modelo_nbin1 <- glmmTMB(Tempo_gasto ~ Group * Aroma + 
                          (1 | Id.uni), 
                        family = nbinom1(), 
                        zi = ~ Group + Aroma,  # Parte zero-inflada
                        data = df_escolhas)

modelo_nbin2 <- glmmTMB(Tempo_gasto ~ Group * Aroma + 
                          (1 | Id.uni) , 
                        family = nbinom2(), 
                        zi = ~ Group + Aroma,  # Parte zero-inflada
                        data = df_escolhas)

# Seleção por AIC
AIC(modelo_zi,modelo_nbin1,modelo_nbin2, modelo_gau) # Melhor modelo é o nbinom1


# Seleção das variaveis aleatórias

modelo_nbin1.1 <- glmmTMB(Tempo_gasto ~ Group * Aroma, 
                          family = nbinom1(), 
                          zi = ~ Group + Aroma,  # Parte zero-inflada
                          data = df_escolhas)

modelo_nbin1.2 <- glmmTMB(Tempo_gasto ~ Group * Aroma + 
                            (1 | Id.uni), 
                          family = nbinom1(), 
                          zi = ~ Group + Aroma,  # Parte zero-inflada
                          data = df_escolhas)


modelo_nbin1.3 <- glmmTMB(Tempo_gasto ~ Group * Aroma,
                          family = nbinom1(), 
                          data = df_escolhas)

# Modelo mais simples é o melhor AIC: 717.165
AIC (modelo_nbin1, modelo_nbin1.1, modelo_nbin1.2, modelo_nbin1.3)


df_escolhas$Group <- factor(df_escolhas$Group, levels = c('CONTROL','DEL','IMI','COM'))
df_escolhas$Aroma <- factor(df_escolhas$Aroma, levels = c('Geranium','Rosemary'))
#Finalmente Extração dos efeitos
summary(modelo_nbin1.1)
tab_model(modelo_nbin1.1)
plot_model (modelo_nbin1.1)

emmeans::emmeans(modelo_nbin1.1, pairwise ~  Aroma | Group, adjust = 'tukey')


# Extração de tamanhos de efeito e intervalos de confiança 
predic_prop <-ggpredict(modelo_nbin1.1, type ="fixed" , terms = c("Group",'Aroma'), ci_level = 0.95)
predic_prop$predicted
predic_prop$conf.low
predic_prop$conf.high 
predic_prop$std.error
predic_prop$group <- factor(predic_prop$group, levels = c('Geranium','Rosemary'))


# Plots 
agrup_escolhas <- df_agrup %>% 
  subset(!(Aroma %in% c('Neutral_zone', 'Decision_time')))


ggplot(df_escolhas, aes (y = Tempo_gasto, x = Group, fill = Aroma))+
  geom_bar(stat = 'identity')

ggplot(df_escolhas, aes(y = Tempo_gasto, x = Group, fill = Aroma, color = Aroma))+
  # geom_violin(width = 1,
  #             # fill = ,
  #             linewidth = 0.5,
  #             position = position_dodge(width = 0.75),
  #             color = 'black')+
  geom_boxplot(width = 0.35,
               # fill = ,
               linewidth = 0.5,
               position = position_dodge(width = 0.40),
               color = 'black',
               notch = F,
               show.legend = T)+
  geom_jitter(size = 3.5,
              position = position_jitterdodge(jitter.width = 0.1, 
                                              dodge.width = 0.40),
              alpha = 0.6,
              show.legend = F)+
  # geom_segment(data = agrup_escolhas, aes(x = as.numeric(Group), 
  #                                   xend = as.numeric(Group) + 0.25, 
  #                                   y = med_tempo, yend = med_tempo), 
  #              size = 1, color = "black",
  #              linetype = 'dotted') +
  geom_label(data = predic_prop, aes(x = x,
                                     y = (conf.low - 0.45),
                                     fill = group,
                                     label = paste("µ̂=", round(predicted, 1))),
             # fill = 'white', 
             color = "black",
             size = 4, 
             fontface = "plain",
             label.padding = unit(0.3, "lines"), label.size = 0.5,
             position = position_dodge(width = 1.2),
             inherit.aes = F,
             show.legend = F) +
  geom_text(data = predic_prop, aes(x = x,
                                    y = (- 1),
                                    # fill = group,
                                    label = c('***','','ns','ns','','ns','','ns')),
            color = "black",
            size = 4.5,
            fontface = "plain",
            # label.padding = unit(0.3, "lines"), label.size = 0.5,
            position = position_dodge(width = 1.2),
            inherit.aes = F,
            show.legend = F) +
  geom_errorbar(data = predic_prop, aes(x = x,
                                        ymax = conf.high,
                                        ymin = conf.low,
                                        fill = group),
                inherit.aes = F,
                width = 0.2,
                position = position_dodge(width = 1.1),
                color = '#700',
                show.legend = F, 
                size = 1)+
  geom_point (data = predic_prop, aes(y = predicted, x = x, fill = group),
              size = 4.5,
              color = '#700',
              inherit.aes = F,
              position = position_dodge (width = 1.1),
              show.legend = F)+
  theme_bw()+
  scale_color_manual(values = c('black','black','#b65DdF','#6DA34D','#928039','#928010'))+
  scale_fill_manual(values = c('#bbb','#fff','#b65DdF','#892829','#928039','#928010'))+
  labs (x = '')+
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(face = "bold", color = "black", size = 14),
    axis.title.y = element_text(face = "bold", color = "black",size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14, color = 'black', face = 'plain'),
    axis.text.y = element_text(size = 14, color = 'black'),
    legend.position = 'right',
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 14, face = 'plain'),
    panel.border = element_blank(),  
    panel.grid.major.y = element_line(color = "grey", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.line.y.left = element_line(color = 'black'),  
    axis.line.x = element_line(color = 'black'),  
    axis.line.y = element_line(color = 'red'),  
    axis.line.x.top = element_line(color = "white"))+
  scale_x_discrete(labels = c("CONTROL" = "Control \n n = (20)", "DEL" = "Deltamethrin \n n = (20)", "IMI" = "Imidacloprid \n n = (20)", 'COM' = 'Combination \n n = (20)'))+
  labs (y = 'Time (s)')


# Diferença entre motivação de decidir - CHOICE1---------------------------------------------
df_choice1 <- df %>% 
  group_by(Group) %>% 
  summarise(media = mean(Choice_1))

# Modelo para ver se tem diferença na probabilidade de escolher ou não
modelo.choice1 <- glmmTMB(Choice_1 ~ Group, 
                          family = binomial(), 
                          data = df)

plot(simulateResiduals(modelo.choice1))
summary(modelo.choice1)
tab_model(modelo.choice1)

predic_prop_c1 <-ggpredict(modelo.choice1, type ="fixed" , terms = c("Group"), ci_level = 0.95)
predic_prop_c1$predicted
predic_prop_c1$conf.low
predic_prop_c1$conf.high 
predic_prop_c1$std.error
predic_prop_c1$n_total <- 1
predic_prop_c1$n_tot <- c(20)
predic_prop_c1$n_decisao <- c(19,14,14,15)

plot_proportion <-  ggplot(predic_prop_c1, aes(x =x, y = predicted))+
  geom_bar(data = predic_prop_c1, 
           aes(x =x , y = n_total),
           stat = 'identity',
           fill = '#fff',
           color = 'black',
           width = 0.45,
           inherit.aes = F)+
  geom_bar(stat = 'identity', fill = '#aaa', color = 'black',
           width = 0.45)+
  geom_text(stat = 'identity',
            aes(label = paste('n = (',n_decisao,')')),
            y = 0.09,
            size = 5)+
  # geom_text(stat = 'identity',
  #           aes(label = paste('n = ',n_tot)),
  #               y = 1,
  #           size = 5)+
  geom_errorbar(data = predic_prop_c1, aes(x = x,
                                           ymax = conf.high,
                                           ymin = conf.low),
                inherit.aes = F,
                width = 0.25,
                position = position_dodge(width = 1.2),
                show.legend =F,
                color = 'black',
                size = 1)+
  geom_segment(data = predic_prop_c1, aes(x = as.numeric(x),
                                          xend = as.numeric(x) + 0.25,
                                          y = predicted, yend = predicted),
               size = 1, color = "black",
               linetype = 2,
               inherit.aes = F,
               show.legend = F) +
  geom_label(data = predic_prop_c1, aes(x = x, 
                                        y = predicted,
                                        label = paste("P = ", round(predicted, 2))),
             fill = 'white', 
             color = "black",
             size = 4.5, 
             fontface = "bold",
             label.padding = unit(0.3, "lines"),
             label.size = 0.5,
             position = position_nudge(x = 0.35),
             show.legend = F) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold", color = "black",size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14, color = 'black', face = 'plain'),
    axis.text.y = element_text(size = 14, color = 'black'),
    legend.position = 'right',
    legend.title = element_text(size = 14, face = "bold"),
    panel.border = element_blank(),  
    panel.grid.major.y = element_line(color = "grey", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.line.y.left = element_line(color = 'black'),  
    axis.line.x = element_line(color = 'black'),  
    axis.line.y = element_line(color = 'red'),  
    axis.line.x.top = element_line(color = "white"))+
  scale_x_discrete(labels = c("CONTROL" = "Control \n n = (20)", "DEL" = "Deltamethrin \n n = (20)", "IMI" = "Imidacloprid \n n = (20)", 'COM' = 'Combination \n n = (20)'))+
  labs (y = 'Proportion')



# Diferenças entre lados escolhidos GERANIUM /ROSEMARY ---------------------------------------
df_choice <- df %>% 
  filter (Choice %in% c('Geranium','Rosemary'))
str(df_choice)



modelo.choice.nulo <- df_choice %>% 
  filter(Group == "CONTROL") %>% 
  glmmTMB(Choice ~ 1, 
          data = .,             # O ponto indica que o df filtrado entra aqui
          family = binomial())

summary(modelo.choice.nulo)

modelo.choice.nulo <- df_choice %>% 
  filter(Group == "IMI") %>% 
  glmmTMB(Choice ~ 1, 
          data = .,             # O ponto indica que o df filtrado entra aqui
          family = binomial())

summary(modelo.choice.nulo)


modelo.choice.nulo <- df_choice %>% 
  filter(Group == "DEL") %>% 
  glmmTMB(Choice ~ 1, 
          data = .,             # O ponto indica que o df filtrado entra aqui
          family = binomial())
summary(modelo.choice.nulo)


modelo.choice.nulo <- df_choice %>% 
  filter(Group == "COM") %>% 
  glmmTMB(Choice ~ 1, 
          data = .,             # O ponto indica que o df filtrado entra aqui
          family = binomial())
summary(modelo.choice.nulo)




modelo.choice <- glmmTMB(Choice ~ Group, 
                         family = binomial(),
                         data = df_choice)

plot(simulateResiduals(modelo.choice))
summary(modelo.choice)
summary(modelo.choice.nulo)



library(dplyr)

# Filtrar e agrupar os dados
df_choice_grup <- df %>%
  filter (Choice %in% c("Geranium","Rosemary")) %>%
  filter(!is.na(Choice_1)) %>%  # Remove NAs
  group_by(Group, Choice) %>%  # Agrupa por Group e Choice
  summarise(escolha = n(), .groups = "drop") %>%  # Conta as escolhas
  group_by(Group) %>%  # Reagrupa por Group
  mutate(Proporcao = escolha / sum(escolha)) %>%  # Calcula a proporção
  ungroup()  # Desagrupa para continuar manipulando os dados

df_choice_grup$Choice <- factor(df_choice_grup$Choice, levels = c("Rosemary", "Geranium"))
df_choice_grup$Choice <- factor(df_choice_grup$Choice, levels = c("Geranium","Rosemary"))

plot.insect.1 <- ggplot(df_choice_grup, aes(y = Proporcao, x = Group, fill = Choice)) +
  geom_bar(stat = 'identity', color = 'black') +
  theme_bw()+
  theme_bw()+
  scale_color_manual(values = c('black','black','#b65DdF','#6DA34D','#928039','#928010'))+
  scale_fill_manual(values = c("#1E8C6C","#8C1E3E")) +
  labs (x = '')+
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black")+
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(face = "plain", color = "black", size = 14),
    axis.title.y = element_text(face = "bold", color = "black",size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14, color = 'black', face = 'plain'),
    axis.text.y = element_text(size = 14, color = 'black'),
    legend.position = 'right',
    legend.title = element_text(size = 14, face = "plain"),
    legend.text = element_text (size = 14, face = 'plain'),
    panel.border = element_blank(),  
    panel.grid.major.y = element_line(color = "grey", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.line.y.left = element_line(color = 'black'),  
    axis.line.x = element_line(color = 'black'),  
    axis.line.y = element_line(color = 'red'),  
    axis.line.x.top = element_line(color = "white"))+
  scale_x_discrete(labels = c("CONTROL" = "Control \n n = (19)", "DEL" = "Deltamethrin \n n = (14)", "IMI" = "Imidacloprid \n n = (14)", 'COM' = 'Combination \n n = (15)'))+
  labs (y = 'Proportion')  +
  coord_flip()


plot_grid(plot.insect.1, plot.insect.2, 
          labels = c("a", "b"),        # Adiciona as tags desejadas
          label_size = 15,             # Define o tamanho da fonte
          label_fontface = "plain" ,
          rel_widths = c(5.5, 4.5))# Remove o negrito (plain)



ggplot(df_choice_grup, aes(y = Proporcao, x = Group, fill = Choice)) +
  geom_bar(position = 'fill') +
  coord_flip() +
  # Linha central que agora representa o 0
  geom_hline(yintercept = 0.5, linewidth = 1, linetype = "dashed", color = "black") +
  xlab('Conditioned scent') +
  ylab('Proportion') +
  scale_fill_manual(values = c("#1E8C6C", "#8C1E3E")) +
  # Ajuste da escala: define os pontos e subtrai 0.5 de cada um para centralizar no zero
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.25), 
    labels = c("0.5", "0.25", "0", "0.25", "0.5")
  ) +
   scale_x_discrete(labels = c("CONTROL" = "Control \n n = (19)", "DEL" = "Deltamethrin \n n = (14)", "IMI" = "Imidacloprid \n n = (14)", 'COM' = 'Combination \n n = (15)'))+
  labs (legend = 'Side')+
  theme(
    legend.position = "right", # Ajustado para aparecer, ou c() se quiser posição exata
    plot.title = element_text(hjust = 0.5, face = "plain", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(face = "plain", color = "black", size = 14),
    axis.title.y = element_text(face = "plain", color = "black", size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14, color = 'black', face = 'plain'),
    axis.text.y = element_text(angle = 0, hjust = 0.5, size = 14, color = 'black'),
    legend.title = element_text(size = 14, face = 'plain'),
    legend.text = element_text(size = 14, face = 'plain'),
    panel.border = element_blank(),
    panel.grid.major = element_line(color = "gray90", linetype = "dashed"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.line.y.left = element_line(color = 'black'),
    axis.line.x = element_line(color = 'black'),
    base_size = 14
  )

# Análise do tempo neutro - Interesse na fonte de alimento - NEUTRAL_ZONE ----------------

df_neutro <- df_long %>% 
  subset (Aroma == 'Neutral_zone') 

modelo_neutro.1 <- glmmTMB(Tempo_gasto ~ Group, 
                           family = nbinom2(link = 'log'),
                           data = df_neutro)

modelo_neutro.2 <- glmmTMB(Tempo_gasto ~ Group, 
                           family = truncated_nbinom2(),
                           # dispformula = ~Group,
                           data = df_neutro)

modelo_neutro.3 <- glmmTMB(Tempo_gasto ~ Group, 
                           family = nbinom1(link = 'log'),
                           dispformula = ~Group,
                           data = df_neutro)

modelo_neutro.4 <- glmmTMB(Tempo_gasto ~ Group, 
                           family = nbinom12(link = 'log'),
                           dispformula = ~Group,
                           data = df_neutro)

modelo_neutro.5 <- glmmTMB(Tempo_gasto ~ Group, 
                           family = truncated_poisson(),
                           # dispformula = ~Group,
                           data = df_neutro)

modelo_neutro.6 <- glmmTMB(Tempo_gasto ~ Group, 
                           family = gaussian(),
                           # dispformula = ~Group,
                           data = df_neutro)

modelo_neutro.7 <- glmmTMB(Tempo_gasto ~ Group, 
                           family = gaussian(link = 'identity'),
                           dispformula = ~Group,
                           data = df_neutro)

AIC(modelo_neutro.1, modelo_neutro.2, modelo_neutro.3, modelo_neutro.4, modelo_neutro.5,modelo_neutro.6,modelo_neutro.7)
plot(simulateResiduals(modelo_neutro.7))

Anova(modelo_neutro.1)
check_model(modelo_neutro.6)


summary(modelo_neutro.6)
tab_model(modelo_neutro.6)

predic_prop_neu <-ggpredict(modelo_neutro.6, type ="fixed" , terms = c("Group"), ci_level = 0.95)
predic_prop_neu$predicted
predic_prop_neu$conf.low
predic_prop_neu$conf.high 
predic_prop_neu$std.error



# Histograma dos dados
ggplot(df_neutro, aes(x = Group,y = Tempo_gasto)) +
  geom_boxplot()+
  theme_minimal() +
  labs(title = "Distribuição de Tempo_gasto", x = "Tempo gasto", y = "Frequência")

plot_neutro <- ggplot(df_neutro, aes(y = Tempo_gasto, x = Group, color = Group))+
  geom_violin(width = 1,
              fill = NA,
              linewidth = 0.6,
              position = position_dodge(width = 0.75),
              color = 'black',
              show.legend = F)+
  geom_boxplot(width = 0.15,
               # fill = ,
               linewidth = 0.6,
               position = position_dodge(width = 0.45),
               color = 'black',
               show.legend = F,
               whiskers = F)+
  geom_jitter(size = 3.5,
              position = position_jitterdodge(jitter.width = 0.3, 
                                              dodge.width = 0.45),
              alpha = 0.6,
              show.legend = T)+
  geom_segment(data = predic_prop_neu, aes(x = as.numeric(x),
                                           xend = as.numeric(x) + 0.25,
                                           y = predicted, yend = predicted),
               size = 1, color = "black",
               linetype = 'dotted',
               inherit.aes = F,
               show.legend = F) +
  geom_label(data = predic_prop_neu, aes(x = x, 
                                         y = predicted,
                                         label = paste("µ̂ = ", round(predicted, 2))),
             fill = 'white', 
             color = "black",
             size = 4.5, 
             fontface = "bold",
             label.padding = unit(0.3, "lines"),
             label.size = 0.5,
             position = position_nudge(x = 0.25),
             show.legend = F) +
  geom_errorbar(data = predic_prop_neu, aes(x = x,
                                            ymax = conf.high,
                                            ymin = conf.low),
                inherit.aes = F,
                width = 0.07,
                position = position_dodge(width = 1.2),
                show.legend =F,
                color = '#700',
                size = 1)+
  geom_point (data = predic_prop_neu, 
              aes(y = predicted, x = x),
              size = 4.5,
              color = '#700',
              inherit.aes = F,
              position = position_dodge (width = 1.2),
              # position = position_nudge (x = 1.2),
              show.legend = F)+
  geom_text(data = predic_prop_neu, aes(x = x,
                                        y = (- 1),
                                        # fill = group,
                                        label = c('r','*','*','')),
            color = "black",
            size = 7,
            fontface = "plain",
            # label.padding = unit(0.3, "lines"), label.size = 0.5,
            position = position_dodge(width = 1.2),
            inherit.aes = F,
            show.legend = F) +
  theme_bw()+
  scale_color_manual(values = c('#000','#000','#000','#000','#928039','#928010'))+
  scale_fill_manual(values = c('white','white','#b65DdF','#892829','#928039','#928010'))+
  labs (x = '')+
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(face = "bold", color = "black", size = 14),
    axis.title.y = element_text(face = "bold", color = "black",size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14, color = 'black', face = 'plain'),
    axis.text.y = element_text(size = 14, color = 'black'),
    legend.position = 'none',
    legend.title = element_text(size = 14, face = "bold"),
    panel.border = element_blank(),  
    panel.grid.major.y = element_line(color = "grey", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.line.y.left = element_line(color = 'black'),  
    axis.line.x = element_line(color = 'black'),  
    axis.line.y = element_line(color = 'red'),  
    axis.line.x.top = element_line(color = "white"))+
  scale_x_discrete(labels = c("CONTROL" = "Control \n n = (20)", "DEL" = "Deltamethrin \n n = (20)", "IMI" = "Imidacloprid \n n = (20)", 'COM' = 'Combination \n n = (20)'))+
  labs (y = 'Time (s)')

plot_neutro + plot_proportion+
  plot_layout (heights = c(5.5,4.5))+
  plot_annotation(tag_levels = 'a', )




