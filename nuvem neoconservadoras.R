# Definir o diretório de trabalho
getwd()
setwd("C:\\Users\\User\\Documents\\perfil")

# Instale os pacotes necessários, se ainda não estiverem instalados
install.packages("wordcloud")
install.packages("RColorBrewer") # Pacote adicional para paleta de cores

# Carregar os pacotes
library(wordcloud)
library(RColorBrewer)

# Dados com os temas e frequências especificados
temas <- data.frame(
  palavra = c("Saúde", "Moradia", "Adoção", "Inclusão social", "Sexualidade", 
              "Proteção à criança/adolescente", "Educação", "Meio ambiente", 
              "Violência", "Religião", "Cultura", "Assistência social", "Economia"),
  frequencia = c(17, 2, 1, 15, 8, 2, 3, 1, 9, 2, 1, 2, 1)
)

# Verificar os dados
print(temas)

# Salvar a nuvem de palavras em um arquivo PNG com melhor resolução
png(filename = "nuvem_de_palavras2.png", width = 2400, height = 1800, res = 300)  # Alta resolução (300 DPI)

# Criar a nuvem de palavras com 'wordcloud'
wordcloud(words = temas$palavra, 
          freq = temas$frequencia, 
          min.freq = 1,           # Frequência mínima para aparecer
          max.words = 100,        # Limite de palavras a serem exibidas
          random.order = FALSE,   # Manter palavras mais frequentes no centro
          colors = brewer.pal(8, "Dark2"), # Cores aleatórias
          scale = c(4, 0.5))      # Escala de tamanhos de palavras

# Fechar o arquivo PNG após a geração da imagem
dev.off()
