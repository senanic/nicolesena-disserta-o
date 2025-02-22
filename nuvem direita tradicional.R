getwd()
setwd("C:\\Users\\User\\Documents\\perfil")

# Instale o pacote wordcloud, se ainda n�o estiver instalado
install.packages("wordcloud")
install.packages("RColorBrewer") # Pacote adicional para paleta de cores

# Carregar os pacotes
library(wordcloud)
library(RColorBrewer)

# Dados de exemplo com frequ�ncias
temas <- data.frame(
  palavra = c("Sa�de", "Viol�ncia", "Inclus�o Social", "Educa��o", "Esporte", "Seguran�a", "Emprego", "Religi�o", "Ado��o", 
              "Prote��o animal", "Prote��o � crian�a/adolescente", "Assist�ncia social", "Empoderamento", "Trabalho", 
              "Moradia", "Prote��o ao idoso", "Meio ambiente", "Participa��o feminina na pol�tica", "Agricultura", 
              "Cultura", "Migra��o", "Cuidado", "Economia", "Igualdade de g�nero"),
  frequencia = c(16, 32, 20, 8, 6, 3, 1, 2, 1, 1, 3, 2, 2, 5, 1, 1, 2, 1, 3, 2, 1, 1, 2, 1)
)

# Verifique se a palavra "Viol�ncia" est� corretamente inserida
print(temas)

# Salvar a nuvem de palavras em um arquivo PNG com melhor resolu��o
png(filename = "nuvem_de_palavras.png", width = 2400, height = 1800, res = 300)  # Alta resolu��o (300 DPI)

# Criar a nuvem de palavras com 'wordcloud'
wordcloud(words = temas$palavra, 
          freq = temas$frequencia, 
          min.freq = 1,           # Frequ�ncia m�nima para aparecer
          max.words = 100,        # Limite de palavras a serem exibidas
          random.order = FALSE,   # Manter palavras mais frequentes no centro
          colors = brewer.pal(8, "Dark2"), # Cores aleat�rias
          scale = c(4, 0.5))      # Escala de tamanhos de palavras

# Fechar o arquivo PNG ap�s a gera��o da imagem
dev.off()
