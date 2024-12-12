getwd()
setwd("C:\\Users\\User\\Documents\\perfil")

# Instale o pacote wordcloud, se ainda não estiver instalado
install.packages("wordcloud")
install.packages("RColorBrewer") # Pacote adicional para paleta de cores

# Carregar os pacotes
library(wordcloud)
library(RColorBrewer)

# Dados de exemplo com frequências
temas <- data.frame(
  palavra = c("Saúde", "Violência", "Inclusão Social", "Educação", "Esporte", "Segurança", "Emprego", "Religião", "Adoção", 
              "Proteção animal", "Proteção à criança/adolescente", "Assistência social", "Empoderamento", "Trabalho", 
              "Moradia", "Proteção ao idoso", "Meio ambiente", "Participação feminina na política", "Agricultura", 
              "Cultura", "Migração", "Cuidado", "Economia", "Igualdade de gênero"),
  frequencia = c(16, 32, 20, 8, 6, 3, 1, 2, 1, 1, 3, 2, 2, 5, 1, 1, 2, 1, 3, 2, 1, 1, 2, 1)
)

# Verifique se a palavra "Violência" está corretamente inserida
print(temas)

# Salvar a nuvem de palavras em um arquivo PNG com melhor resolução
png(filename = "nuvem_de_palavras.png", width = 2400, height = 1800, res = 300)  # Alta resolução (300 DPI)

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
