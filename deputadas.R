getwd()
setwd("C:\\Users\\User\\Documents\\perfil")

a <- read.csv("deputadas_r.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)


vlab <- names(a)
names(a) <- paste0("b", 1:ncol(a))

a$idade <- a$b2
attr(a$idade, "label") <- "Idade"

a$estado <- a$b3
attr(a$estado, "label") <- "Estado"

a$partido <- a$b4
attr(a$partido, "label") <- "Partido"

a$classificação <- a$b5
attr(a$classificação, "label") <- "Classificação"

a$raça <- a$b6
attr(a$raça, "label") <- "Raça"

a$profissão <- a$b7
attr(a$profissão, "label") <- "Profissão"

a$escolaridade <- a$b8
attr(a$escolaridade, "label") <- "Escolaridade"

a$estadocivil <- a$b9
attr(a$estadocivil, "label") <- "Estado civil"

a$classesocial <- a$b10
attr(a$classesocial, "label") <- "Classe social"

a$tempo <- a$b11
attr(a$tempo, "label") <- "Tempo na política"



#idade
media <- mean(a$b2)
print(media)

mediana <- median(a$b2)
print(mediana)


ggplot(a, aes(x = b2)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  geom_vline(aes(xintercept = media), color = "red", linetype = "dashed", size = 1, label = "Média") +
  geom_vline(aes(xintercept = mediana), color = "blue", linetype = "dashed", size = 1, label = "Mediana") +
  labs(
    title = "Distribuição das Idades",
    x = "Idade",
    y = "Frequência"
  ) +
  theme_minimal() +
  annotate("text", x = media, y = max(table(a$b2)) * 0.9, label = paste("Média:", round(media, 2)), color = "red", hjust = -0.1) +
  annotate("text", x = mediana, y = max(table(a$b2)) * 0.8, label = paste("Mediana:", round(mediana, 2)), color = "blue", hjust = -0.1)

#partido
fpartido <- table(a$partido)
print(fpartido)

fpartido <- table(a$partido)
ppartido <- prop.table(fpartido) * 100
print(ppartido)


partidos <- c("PL","PSDB", "PSD", "MDB","REPUBLICANOS", "PP", "UNIÃO", "PODE", "PATRIOTA", "SDD")
porcentagens_partido <- c(26.66, 16.66, 10, 10, 10, 6.66, 6.66, 6.66, 3.33, 3.33  )
largura_barras <- 0.9
barplot(porcentagens_partido, 
        width = largura_barras, 
        names.arg = partidos, 
        main = "Partidos", xlab = "Partido", ylab = "Porcentagem",
        ylim = c(0, max(porcentagens_partido +5 )))




#raça
a$raça[a$raça == "Sem informação"] <- NA

fraça <- table(a$raça)
print(fraça)

fraça <- table(a$raça)
praça <- prop.table(fraça) * 100
print(praça)

write.csv(a, "C:\\Users\\User\\Documents\\deputadas_r.csv", row.names = FALSE)
barplot(praça, main = "Raça", xlab = "Raça", ylab = "Porcentagem", ylim = c(0, max(praça) * 1.2))

#estado

festado <- table(a$estado)
print(festado)

pestado <- prop.table(festado) * 100
print(pestado)

estados <- c("São Paulo", "Rio Grande do Sul", "Maranhão", "Amapá", "Distrito Federal")
porcentagens_estados <- c(43.33, 20, 16.66, 13.33, 6.66)
largura_barras <- 0.8
barplot(porcentagens_estados, 
        width = largura_barras, 
        names.arg = estados, 
        main = "Estados", xlab = "Estado", ylab = "Porcentagem",
        ylim = c(0, max(porcentagens_estados +10 )))


#profissão
fprofissão <- table(a$profissão)
print(fprofissão)

fprofissão <- table(a$profissão)
pprofissão <- prop.table(fprofissão) * 100
print(pprofissão)


profissão <- c("Deputada", "Empresária", "Servidora pública", "Administradora", "Advogada")
porcentagens_profissão <- c(33.33, 16.66, 13.33, 6.66, 6.66)
largura_barras <- 0.8
barplot(porcentagens_profissão, 
        width = largura_barras, 
        names.arg = profissão, 
        main = "Profissões preponderantes", xlab = "Profissão", ylab = "Porcentagem",
        ylim = c(0, max(porcentagens_profissão *1.5 )))

# Verificar os valores únicos na variável escolaridade
print(unique(a$escolaridade))
print(a$escolaridade)

# Redefinir a ordem dos fatores da escolaridade
a$escolaridade <- factor(a$escolaridade, 
                         levels = c("Ensino fundamental completo", 
                                    "Ensino médio completo", 
                                    "Superior incompleto", 
                                    "Superior completo"))

# Calcular a frequência e as porcentagens de escolaridade
fescolaridade <- table(a$escolaridade)
print(fescolaridade)

pescolaridade <- prop.table(fescolaridade) * 100
print(pescolaridade)

# Ordena os dados em ordem decrescente
pescolaridade_ordenada <- pescolaridade[order(pescolaridade, decreasing = TRUE)]

# Cria o gráfico com os dados ordenados
barplot(pescolaridade_ordenada, 
        main = "Escolaridade", 
        xlab = "Escolaridade", 
        ylab = "Porcentagem", 
        ylim = c(0, max(pescolaridade) * 1.2))

#estado civil

festadocivil <- table(a$estadocivil)
print(festadocivil)

festadocivil <- table(a$estadocivil)
pestadocivil <- prop.table(festadocivil) * 100
print(pestadocivil)

barplot(pestadocivil, main = "Estado Civil", xlab = "Estado Civil", ylab = "Porcentagem", ylim = c(0, 100))

#classe social de acordo com campos e machado (2020)

# Redefinir a ordem dos fatores da classe social
a$classesocial <- factor(a$classesocial, 
                         levels = c("Alta", 
                                    "Média alta", 
                                    "Média baixa", 
                                    "Baixa"))

# Calcular a frequência e as porcentagens de classe social
fclassesocial <- table(a$classesocial)
print(fclassesocial)

pclassesocial <- prop.table(fclassesocial) * 100
print(pclassesocial)

# Criar o gráfico de barras mantendo a ordem dos níveis definidos
barplot(pclassesocial, 
        main = "Classe social", 
        xlab = "Classe social", 
        ylab = "Porcentagem", 
        ylim = c(0, max(pclassesocial) * 1.2),
        col = "gray")


#tempo na política
media <- mean(a$tempo)
print(media)
mediana <- median(a$tempo)
print(mediana)

# Cria o histograma com linhas verticais para a média e a mediana
hist(a$tempo, 
     main = "Distribuição do Tempo na Política", 
     xlab = "Tempo na Política (anos)", 
     ylab = "Frequência", 
     col = "lightblue", 
     border = "black")

# Adiciona linhas para a média e a mediana
abline(v = media, col = "red", lwd = 2, lty = 2)     # Linha pontilhada vermelha para a média
abline(v = mediana, col = "blue", lwd = 2, lty = 2)   # Linha pontilhada azul para a mediana

# Adiciona uma legenda para as linhas
legend("topright", legend = c("Média", "Mediana"), 
       col = c("red", "blue"), lty = 2, lwd = 2)


#cruzamentos e distribuições


# Converter as variáveis para fator, se ainda não estiverem
a$partido <- as.factor(a$partido)
a$raça <- as.factor(a$raça)

# Filtrar os dados para remover valores NA
dados_filtrados <- a %>%
  filter(!is.na(partido), !is.na(raça))


# Criar um gráfico de barras agrupadas
ggplot(dados_filtrados, aes(x = partido, fill = raça)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribuição de Raça por Partido",
       x = "Partido",
       y = "Contagem",
       fill = "Raça") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Converter as variáveis para fator, se ainda não estiverem
a$partido <- as.factor(a$partido)
a$estado <- as.factor(a$estado)

# Filtrar os dados para remover valores NA
dados_filtrados <- a %>%
  filter(!is.na(partido), !is.na(estado))

# Criar um gráfico de barras agrupadas
ggplot(dados_filtrados, aes(x = partido, fill = estado)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribuição de Estado por Partido",
       x = "Partido",
       y = "Contagem",
       fill = "Estado") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Converter as variáveis para fator, se ainda não estiverem
a$raça <- as.factor(a$raça)
a$escolaridade <- as.factor(a$escolaridade)

# Filtrar os dados para remover valores NA
dados_filtrados <- a %>%
  filter(!is.na(raça), !is.na(escolaridade))

# Criar um gráfico de barras agrupadas
ggplot(dados_filtrados, aes(x = raça, fill = escolaridade)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribuição de Escolaridade por Raça",
       x = "Raça",
       y = "Contagem",
       fill = "Escolaridade") +
  theme_minimal()

# Converter as variáveis para fator, se ainda não estiverem
a$raça <- as.factor(a$raça)
a$classesocial <- as.factor(a$classesocial)

# Filtrar os dados para remover valores NA
dados_filtrados <- a %>%
  filter(!is.na(raça), !is.na(classesocial))

# Criar um gráfico de barras agrupadas
ggplot(dados_filtrados, aes(x = raça, fill = classesocial)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribuição de Classe Social por Raça",
       x = "Raça",
       y = "Contagem",
       fill = "Classe Social") +
  theme_minimal()

# Converter as variáveis para fator, se ainda não estiverem
a$classesocial <- as.factor(a$classesocial)
a$partido <- as.factor(a$partido)

# Filtrar os dados para remover valores NA
dados_filtrados <- a %>%
  filter(!is.na(classesocial), !is.na(partido))

# Criar um gráfico de barras agrupadas
ggplot(dados_filtrados, aes(x = classesocial, fill = partido)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribuição de Partidos por Classe Social",
       x = "Classe Social",
       y = "Contagem",
       fill = "Partido") +
  theme_minimal()


# Contagem das mulheres nas duas classificações
direita_tradicional <- sum(a$classificação == "Direita tradicional")
neoconservadora <- sum(a$classificação == "Neoconservadora")

# Exibir as contagens
direita_tradicional
neoconservadora

# Criar o gráfico de barras com as porcentagens
porcentagens_classificacao <- c(
  "Direita Tradicional" = (direita_tradicional / nrow(a)) * 100,
  "Neoconservadora" = (neoconservadora / nrow(a)) * 100
)

barplot(porcentagens_classificacao,
        main = "Porcentagem de Mulheres de Direita Tradicional e Neoconservadoras",
        xlab = "Classificação",
        ylab = "Porcentagem (%)",
        col = c("lightcoral", "lightblue"),
        ylim = c(0, 100))


# Contagem de partidos por classificação direita tradicional e neoconservadora
contagem_partidos <- a %>%
  group_by(partido, classificação) %>%
  summarise(contagem = n()) %>%
  ungroup()

# Plotar o gráfico de barras
ggplot(contagem_partidos, aes(x = partido, y = contagem, fill = classificação)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Comparação dos Partidos entre Direita Tradicional e Neoconservadora",
    x = "Partidos",
    y = "Contagem",
    fill = "Classificação"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###########diferenciando os perfis

#partido
# Contagem de partidos por classificação direita tradicional e neoconservadora
contagem_partidos <- table(a$partido, a$classificação)
print(contagem_partidos)

# Realizar o teste qui-quadrado para verificar se a distribuição dos partidos é independente entre as classificações 
teste_qui_quadrado <- chisq.test(contagem_partidos)

# Exibir o resultado do teste qui-quadrado
print(teste_qui_quadrado)

# Caso o valor de p seja menor que 0.05, isso indicaria que há uma associação significativa entre partido e classificação
if(teste_qui_quadrado$p.value < 0.05) {
  print("Há uma associação significativa entre partido e a classificação")
} else {
  print("Não há uma associação significativa entre partido e a classificação")
}

# Contagem de partidos por classificação direita tradicional e neoconservadora
contagem_partidos <- a %>%
  group_by(partido, classificação) %>%
  summarise(contagem = n()) %>%
  ungroup()

# Plotar o gráfico de barras
ggplot(contagem_partidos, aes(x = partido, y = contagem, fill = classificação)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Comparação dos Partidos entre Direita Tradicional e Neoconservadora",
    x = "Partidos",
    y = "Contagem",
    fill = "Classificação"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#estado

# Contagem de estados por classificação direita tradicional e neoconservadora
contagem_estados <- table(a$estado, a$classificação)
print(contagem_estados)

# Realizar o teste qui-quadrado para verificar se a distribuição dos estados é independente entre as classificações 
teste_qui_quadrado <- chisq.test(contagem_estados)

# Exibir o resultado do teste qui-quadrado
print(teste_qui_quadrado)

# Caso o valor de p seja menor que 0.05, isso indicaria que há uma associação significativa entre estado e classificação 
if(teste_qui_quadrado$p.value < 0.05) {
  print("Há uma associação significativa entre estado e a classificação.")
} else {
  print("Não há uma associação significativa entre estado e a classificação.")
}



#raça

# Contagem de raça por classificação direita tradicional e neoconservadora
contagem_raca <- table(a$raça, a$classificação)
print(contagem_raca)

# Realizar o teste qui-quadrado para verificar se a distribuição das raças é independente entre as classificações 
teste_qui_quadrado_raca <- chisq.test(contagem_raca)

# Exibir o resultado do teste qui-quadrado
print(teste_qui_quadrado_raca)

# Caso o valor de p seja menor que 0.05, isso indicaria que há uma associação significativa entre raça e classificação 
if(teste_qui_quadrado_raca$p.value < 0.05) {
  print("Há uma associação significativa entre raça e a classificação")
} else {
  print("Não há uma associação significativa entre raça e a classificação")
}



#escolaridade

# Contagem de escolaridade por classificação direita tradicional e neoconservadora
contagem_escolaridade <- table(a$escolaridade, a$classificação)
print(contagem_escolaridade)

# Realizar o teste qui-quadrado para verificar se a distribuição de escolaridade é independente entre as classificações 
teste_qui_quadrado_escolaridade <- chisq.test(contagem_escolaridade)

# Exibir o resultado do teste qui-quadrado
print(teste_qui_quadrado_escolaridade)

# Caso o valor de p seja menor que 0.05, isso indicaria que há uma associação significativa entre escolaridade e classificação 
if(teste_qui_quadrado_escolaridade$p.value < 0.05) {
  print("Há uma associação significativa entre escolaridade e a classificação")
} else {
  print("Não há uma associação significativa entre escolaridade e a classificação")
}




# idade


direita_tradicional <- a %>% filter(classificação == "Direita Tradicional")
neoconservadora <- a %>% filter(classificação == "Neoconservadora")

# Teste de Mann-Whitney para comparar a idade entre Direita Tradicional e Neoconservadora
resultado_mann_whitney <- wilcox.test(idade ~ classificação, data = a)

# Exibir o resultado
print(resultado_mann_whitney)

# Exibir o resultado do teste
resultado_mann_whitney <- wilcox.test(idade ~ classificação, data = a)

# Verificar se o p-valor é menor que 0.05 (nível de significância típico)
if (resultado_mann_whitney$p.value < 0.05) {
  message("A diferença nas idades entre os grupos Direita Tradicional e Neoconservadora é estatisticamente significativa (p < 0.05).")
} else {
  message("A diferença nas idades entre os grupos Direita Tradicional e Neoconservadora NÃO é estatisticamente significativa (p >= 0.05).")
}



#profissão

# Teste qui-quadrado para comparar Profissão por Classificação Direita Tradicional e Neoconservadora
teste_chi_profissao <- chisq.test(table(a$profissão, a$classificação))

# Exibir o resultado do teste qui-quadrado
print(teste_chi_profissao)

# Caso o valor de p seja menor que 0.05, isso indicaria que há uma associação significativa entre profissão e classificação
if(teste_chi_profissao$p.value < 0.05) {
  print("Há uma associação significativa entre profissão e a classificação")
} else {
  print("Não há uma associação significativa entre profissão e a classificação")
}


#classe social


# Teste qui-quadrado para comparar Classe Social por Classificação Direita Tradicional e Neoconservadora
teste_chi_classesocial <- chisq.test(table(a$classesocial, a$classificação))

# Exibir o resultado do teste qui-quadrado
print(teste_chi_classesocial)

# Caso o valor de p seja menor que 0.05, isso indicaria que há uma associação significativa entre classe social e classificação
if(teste_chi_classesocial$p.value < 0.05) {
  print("Há uma associação significativa entre classe social e a classificação")
} else {
  print("Não há uma associação significativa entre classe social e a classificação")
}

#tempo na política


# Teste de Mann-Whitney para comparar Tempo na Política entre Direita Tradicional e Neoconservadora
teste_mannwhitney <- wilcox.test(tempo ~ classificação, data = a)

# Exibir o resultado do teste Mann-Whitney
print(teste_mannwhitney)

# Verificar se o p-valor é menor que 0.05 para determinar se a diferença é estatisticamente significativa
if (teste_mannwhitney$p.value < 0.05) {
  message("Há uma diferença estatisticamente significativa no Tempo na Política entre Direita Tradicional e Neoconservadora (p < 0.05).")
} else {
  message("Não há uma diferença estatisticamente significativa no Tempo na Política entre Direita Tradicional e Neoconservadora (p >= 0.05).")
}


#estado civil


# Teste de Qui-Quadrado para comparar Estado Civil entre Direita Tradicional e Neoconservadora
# Criando uma tabela de contingência
tabela_contingencia <- table(a$estadocivil, a$classificação)

# Realizando o teste de Qui-Quadrado
teste_qi_quadrado <- chisq.test(tabela_contingencia)

# Exibir o resultado do teste de Qui-Quadrado
print(teste_qi_quadrado)

# Verificar se o p-valor é menor que 0.05 para determinar se há uma associação significativa
if (teste_qi_quadrado$p.value < 0.05) {
  message("Há uma associação significativa entre Estado Civil e a Classificação (Direita Tradicional e Neoconservadora) (p < 0.05).")
} else {
  message("Não há uma associação significativa entre Estado Civil e a Classificação (Direita Tradicional e Neoconservadora) (p >= 0.05).")
}


############capital político

b <- read.csv("capitais.csv",
              stringsAsFactors = FALSE)
b <- read.csv("capitais.csv", fileEncoding = "UTF-8")

# Define novos nomes para as colunas
colnames(b) <- paste0("b", 1:ncol(b))

# Imprime o dataframe para ver o resultado
print(b)

vlab <- names(b)
names(b) <- paste0("b", 1:ncol(b))


# Calcular as frequências e proporções de capital político
fcapitalpolítico <- table(b$b1)
pcapitalpolítico <- prop.table(fcapitalpolítico) * 100

# Imprimir os resultados
print(fcapitalpolítico)
print(pcapitalpolítico)

# Definir os dados para o gráfico de barras
capitalpolítico <- c("Capital político do próprio campo", "Capital familiar", "Capital midiático", "Capital religioso", "Capital associativo", "Sindicatos", "Sem informações")
porcentagens_capitalpolítico <- c(50, 35.41, 4.16, 4.16, 2.08, 2.08, 2.08 )
largura_barras <- 0.8

# Aumentar a margem esquerda para evitar sobreposição
par(mar = c(5, 7, 4, 2) + 0.1)  

# Criar o gráfico de barras sem rótulos no eixo X
barplot_positions <- barplot(porcentagens_capitalpolítico, 
                             width = largura_barras, 
                             names.arg = "",  # Remove os rótulos temporariamente
                             main = "Capital Político", 
                             xlab = "Capital Político", 
                             ylab = "Porcentagem", 
                             ylim = c(0, 60))

# Adicionar os rótulos manualmente na horizontal
text(x = barplot_positions, 
     y = par("usr")[3] - 2,  # Ajuste de posição abaixo do eixo X
     labels = capitalpolítico, 
     srt = 10,  # Inclinação de 45 graus para melhorar a legibilidade
     adj = 1, 
     xpd = TRUE, 
     cex = 0.8)  # Ajuste o tamanho da fonte conforme necessário




# Carregar os dados
capitais <- read.csv("classificacao_capitais.csv", stringsAsFactors = FALSE)

# Carregar os dados com a codificação UTF-8
capitais <- read.csv("classificacao_capitais.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")


# Verificar os dados para garantir a estrutura
print(capitais)

# Renomear as colunas para facilitar o uso
colnames(capitais) <- c("Direita Tradicional", "Neoconservadora")

# Transformar os dados para o formato longo usando tidyr
library(tidyr)
capitais_long <- pivot_longer(capitais, 
                              cols = everything(), 
                              names_to = "Classificacao",   
                              values_to = "Capital")

# Remover linhas com valores NA ou vazios na coluna "Capital"
capitais_long <- capitais_long[!is.na(capitais_long$Capital) & capitais_long$Capital != "", ]

# Gerar o gráfico comparativo

ggplot(capitais_long, aes(x = Capital, fill = Classificacao)) + 
  geom_bar(position = "dodge") +
  labs(title = "Comparação de Capitais Políticos: Direita Tradicional vs Neoconservadora",
       x = "Tipo de Capital Político",
       y = "Contagem",
       fill = "Classificação") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Criar a tabela de contingência
tabela_contingencia <- table(capitais_long$Capital, capitais_long$Classificacao)  

# Aplicar o teste qui-quadrado
resultado_qc <- chisq.test(tabela_contingencia)

# Exibir o resultado
print(resultado_qc)

