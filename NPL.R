#LOAD LIBRARIES
library(tm)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(utf8)
library(spacyr)
library(quanteda)
library(wordcloud)
library(wordcloud2)
library(syuzhet)

#--------------LECTURA DEL FICHERO-----------------------------------------

# Leemos el texto y lo almacenamos en lines
lines <- readLines("/Users/luciafmolleda/NPL/AQP5A.txt", encoding = "UTF-8") 

# Guardamos los personajes
personajes <- lines[3:16]

# Fijamos el inicio y el fin del texto
inicio <- grep(pattern = "Acto primero", lines,  fixed = TRUE)
fin <- grep(pattern = "(El joven muere.", lines,  fixed = TRUE) 
lines <- lines[inicio:fin]


# Comprobamos la codificacion correcta
# linesQ_NFC <- utf8_normalize(lines)
# sum(linesQ_NFC != lines) # res = 0

# Quitamos las acotaciones, ya que nos centraremos en dialogo
lines <- gsub('(?=\\().*(?<=\\))', "" , lines,perl=TRUE) 

# Quitamos las lineas en blanco
index_lines <- which(lines=="")
lines <- lines[-c(index_lines)]

#-------------- FIN LECTURA DEL FICHERO-----------------------------------------


#--------------ANALISIS DEL TEXTO -----------------------------------------

# Gestion de la libreria
if(!require("spacyr")) install.packages("spacyr")
spacy_install(prompt = FALSE)
spacy_download_langmodel("es")
spacy_initialize(model = "es_core_news_sm")

# NOTA: debido a la cantidad de personajes y por simplificar, se muestra una vez paso a paso que se hace en cada sentencia

# Dividimos el texto entre los personajes y eliminamos la etiqueta que hace referencia al personaje
# Pasos:
    #lineas_joven <- lines[c(grep(pattern = "JOVEN.", lines,  fixed = TRUE))] *Se buscan las l??neas dichas por el joven
    #lineas_joven <- gsub("JOVEN. ", "" , lineas_joven ,perl=TRUE) * Se elimina la indicacion de que habla el joven al principio de la linea
lineas_joven <- gsub("JOVEN. ", "" , lines[c(grep(pattern = "JOVEN.", lines,  fixed = TRUE) )],perl=TRUE) 
lineas_viejo <- gsub("VIEJO. ", "" , lines[c(grep(pattern = "VIEJO.", lines,  fixed = TRUE) )],perl=TRUE) 
lineas_nino <- gsub("NI??O. ", "" , lines[c(grep(pattern = "NI??O.", lines,  fixed = TRUE) )],perl=TRUE) 
lineas_criado <- gsub("CRIADO. ", "" , lines[c(grep(pattern = "CRIADO.", lines,  fixed = TRUE) )],perl=TRUE) 
lineas_criada <- gsub("CRIADA. ", "" , lines[c(grep(pattern = "CRIADA. ", lines,  fixed = TRUE) )],perl=TRUE) 
lineas_amigo <- gsub("AMIGO. ", "" , lines[c(grep(pattern = "AMIGO. ", lines,  fixed = TRUE) )],perl=TRUE) 
lineas_gato <- gsub("GATO. ", "" , lines[c(grep(pattern = "GATO. ", lines,  fixed = TRUE) )],perl=TRUE) 
lineas_mecanografa <- gsub("MECAN??GRAFA. ", "" , lines[c(grep(pattern = "MECAN??GRAFA. ", lines,  fixed = TRUE) )],perl=TRUE) 
lineas_novia <- gsub("NOVIA. ", "" , lines[c(grep(pattern = "NOVIA. ", lines,  fixed = TRUE) )],perl=TRUE) 
lineas_maniqui <- gsub("MANIQU??. ", "" , lines[c(grep(pattern = "MANIQU??. ", lines,  fixed = TRUE) )],perl=TRUE) 
lineas_padre <- gsub("PADRE. ", "" , lines[c(grep(pattern = "PADRE. ", lines,  fixed = TRUE) )],perl=TRUE) 
lineas_payaso <- gsub("PAYASO. ", "" , lines[c(grep(pattern = "PAYASO.", lines,  fixed = TRUE) )],perl=TRUE) 
lineas_arlequin <- gsub("ARLEQU??N. ", "" , lines[c(grep(pattern = "ARLEQU??N. ", lines,  fixed = TRUE) )],perl=TRUE) 
lineas_muchacha <- gsub("MUCHACHA. ", "" , lines[c(grep(pattern = "MUCHACHA. ", lines,  fixed = TRUE) )],perl=TRUE) 

# Ponemos todo en minusculas
lineas_joven <- tolower(lineas_joven)
lineas_viejo <- tolower(lineas_viejo)
lineas_nino <- tolower(lineas_nino)
lineas_criado <- tolower(lineas_criado)
lineas_criada <- tolower(lineas_criada)
lineas_amigo <- tolower(lineas_amigo)
lineas_gato <- tolower(lineas_gato)
lineas_mecanografa <- tolower(lineas_mecanografa)
lineas_novia <- tolower(lineas_novia)
lineas_maniqui <- tolower(lineas_maniqui)
lineas_padre <- tolower(lineas_padre)
lineas_payaso <- tolower(lineas_payaso)
lineas_arlequin <- tolower(lineas_arlequin)
lineas_muchacha <- tolower(lineas_muchacha)
lineas <- tolower(lines)

# Mostramos graficamente la cantidad de intervenciones por personaje
valor <- c(length(lineas_joven), length(lineas_viejo), length(lineas_nino),length(lineas_criado), length(lineas_criada), length(lineas_amigo), length(lineas_gato), length(lineas_mecanografa), length(lineas_novia), length(lineas_maniqui), length(lineas_padre), length(lineas_payaso), length(lineas_arlequin), length(lineas_payaso))
barplot(height=valor, names=personajes, xlab="Intervenciones", space=0, horiz = TRUE, las = 1)

# Dividimos el dialogo en frases 
frases_joven <- spacy_tokenize(lineas_joven, what = "sentence")
frases_viejo <- spacy_tokenize(lineas_viejo, what = "sentence")
frases_nino <- spacy_tokenize(lineas_nino, what = "sentence")
frases_criado <- spacy_tokenize(lineas_criado, what = "sentence")
frases_criada <- spacy_tokenize(lineas_criado, what = "sentence")
frases_amigo <- spacy_tokenize(lineas_amigo, what = "sentence")
frases_gato <- spacy_tokenize(lineas_gato, what = "sentence")
frases_mecanografa <- spacy_tokenize(lineas_mecanografa, what = "sentence")
frases_novia <- spacy_tokenize(lineas_novia, what = "sentence")
frases_maniqui <- spacy_tokenize(lineas_maniqui, what = "sentence")
frases_padre <- spacy_tokenize(lineas_padre, what = "sentence")
frases_payaso <- spacy_tokenize(lineas_payaso, what = "sentence")
frases_arlequin <- spacy_tokenize(lineas_arlequin, what = "sentence")
frases_muchacha <- spacy_tokenize(lineas_muchacha, what = "sentence")

# Mostramos la longitud de las frases
hist(nchar(frases_joven), main = "Longitud de las frases del joven", xlab = "Longitud", ylab = "Intervenciones")
hist(nchar(frases_novia), main = "Longitud de las frases de la novia", xlab = "Longitud", ylab = "Intervenciones")
hist(nchar(frases_viejo), main = "Longitud de las frases del viejo", xlab = "Longitud", ylab = "Intervenciones")
hist(nchar(frases_mecanografa), main = "Longitud de las frases de la mecan??grafa", xlab = "Longitud", ylab = "Intervenciones")
hist(nchar(frases_criada), main = "Longitud de las frases de la criada", xlab = "Longitud", ylab = "Intervenciones")
hist(nchar(frases_arlequin), main = "Longitud de las frases del arlequ??n", xlab = "Longitud", ylab = "Intervenciones")
hist(nchar(frases_nino), main = "Longitud de las frases del ni??o", xlab = "Longitud", ylab = "Intervenciones")
hist(nchar(frases_gato), main = "Longitud de las frases del gato", xlab = "Longitud", ylab = "Intervenciones")
hist(nchar(frases_padre), main = "Longitud de las frases del padre", xlab = "Longitud", ylab = "Intervenciones")
hist(nchar(frases_criado), main = "Longitud de las frases del criado", xlab = "Longitud", ylab = "Intervenciones")
hist(nchar(frases_muchacha), main = "Longitud de las frases de la muchacha", xlab = "Longitud", ylab = "Intervenciones")
hist(nchar(frases_maniqui), main = "Longitud de las frases del maniqu??", xlab = "Longitud", ylab = "Intervenciones")
hist(nchar(frases_amigo), main = "Longitud de las frases del amigo", xlab = "Longitud", ylab = "Intervenciones")

# Ahora dividimos por palabras y eliminamos signos de puntuacion
palabras_joven <- unlist(spacy_tokenize(lineas_joven, what = "word", remove_punct = TRUE))
palabras_viejo <- unlist(spacy_tokenize(lineas_viejo, what = "word", remove_punct = TRUE))
palabras_mecanografa <- unlist(spacy_tokenize(lineas_mecanografa, what = "word", remove_punct = TRUE))
palabras_novia <- unlist(spacy_tokenize(lineas_novia, what = "word", remove_punct = TRUE))
palabras_criada <- unlist(spacy_tokenize(lineas_criada, what = "word", remove_punct = TRUE))
palabras_arlequin <- unlist(spacy_tokenize(lineas_arlequin, what = "word", remove_punct = TRUE))
palabras_nino <- unlist(spacy_tokenize(lineas_nino, what = "word", remove_punct = TRUE))
palabras_gato <- unlist(spacy_tokenize(lineas_gato, what = "word", remove_punct = TRUE))
palabras_padre <- unlist(spacy_tokenize(lineas_padre, what = "word", remove_punct = TRUE))
palabras_payaso <- unlist(spacy_tokenize(lineas_payaso, what = "word", remove_punct = TRUE))
palabras_criado <- unlist(spacy_tokenize(lineas_criado, what = "word", remove_punct = TRUE))
palabras_muchacha <- unlist(spacy_tokenize(lineas_muchacha, what = "word", remove_punct = TRUE))
palabras_maniqui <- unlist(spacy_tokenize(lineas_maniqui, what = "word", remove_punct = TRUE))
palabras_amigo <- unlist(spacy_tokenize(lineas_amigo, what = "word", remove_punct = TRUE))

# Mostramos graficamente la cantidad de palabras por personaje
valor <- c(length(palabras_joven), length(palabras_viejo), length(palabras_nino),length(palabras_criado), length(palabras_criada), length(palabras_amigo), length(palabras_gato), length(palabras_mecanografa), length(palabras_novia), length(palabras_maniqui), length(palabras_padre), length(palabras_payaso), length(palabras_arlequin), length(palabras_payaso))
barplot(height=valor, names=personajes, xlab="Palabras",col = brewer.pal(10, "Set3"), main="Palabras por personaje", space=0, horiz = TRUE, las = 1)

# Mostramos graficamente la cantidad de palabras diferentes por personaje
valor <- c(length(unique(palabras_joven)), length(unique(palabras_viejo)), length(unique(palabras_nino)),length(unique(palabras_criado)), length(unique(palabras_criada)), length(unique(palabras_amigo)), length(unique(palabras_gato)), length(unique(palabras_mecanografa)), length(unique(palabras_novia)), length(unique(palabras_maniqui)), length(unique(palabras_padre)), length(unique(palabras_payaso)), length(unique(palabras_arlequin)), length(unique(palabras_payaso)))
barplot(height=valor, names=personajes, xlab="Palabras", main="Palabras por personaje", space=0, horiz = TRUE, las = 1)

# Nos centramos solo en los personajes con dialogos m??s complejos y vemos sus palabras m??s frecuentes (joven, viejo, novia y mecan??grafa) y buscamos sus palabras m??s comunes
barplot(head(sort(table(palabras_viejo), decreasing = TRUE), n = 15), xlab = "Veces que aparece", main="Palabras m??s comunes viejo", horiz = TRUE, las=1)
barplot(head(sort(table(palabras_joven), decreasing = TRUE), n = 15), xlab = "Veces que aparece", main="Palabras m??s comunes joven", horiz = TRUE, las=1)
barplot(head(sort(table(palabras_novia), decreasing = TRUE), n = 15), xlab = "Veces que aparece", main="Palabras m??s comunes novia", horiz = TRUE, las=1)
barplot(head(sort(table(palabras_mecanografa), decreasing = TRUE), n = 15), xlab = "Veces que aparece", main="Palabras m??s comunes mecanografa", horiz = TRUE, las=1)

# ahora vemos las m??s frecuentes de todo el texto
palabras_texto <- unlist(spacy_tokenize(lineas, what = "word", remove_punct = TRUE))
barplot(head(sort(table(palabras_texto), decreasing = TRUE), n = 15), xlab = "Veces que aparece",col = brewer.pal(10, "Set3"), main="Palabras m??s comunes de todo el texto", horiz = TRUE, las=1)
head(sort(table(palabras_texto), decreasing = FALSE), n = 15)

spacy_finalize()

# Palabras clave por personaje
corpus_joven <- Corpus(VectorSource(palabras_joven))
corpus_viejo <- Corpus(VectorSource(palabras_viejo))
corpus_novia <- Corpus(VectorSource(palabras_novia))
corpus_mecanografa <- Corpus(VectorSource(palabras_mecanografa))
    # Quitamos palabras m??s comunes
    clave_joven <- tm_map(corpus_joven, removeWords, stopwords('spanish'))
    dtm <- TermDocumentMatrix(clave_joven) 
    matrix <- as.matrix(dtm) 
    words <- sort(rowSums(matrix),decreasing=TRUE) 
    df <- data.frame(word = names(words),freq=words)
    wordcloud2(data=df, size=3, color='random-dark')

# Version corta para el resto
corpus_viejo <- tm_map(corpus_joven, removeWords, stopwords('spanish'))
words <- sort(rowSums(as.matrix(TermDocumentMatrix(tm_map(corpus_viejo, removeWords, stopwords('spanish'))))),decreasing=TRUE) 
wordcloud2(data= data.frame(word = names(words),freq=words), size=3, color='random-dark')

corpus_novia <- tm_map(corpus_novia, removeWords, stopwords('spanish'))
words <- sort(rowSums(as.matrix(TermDocumentMatrix(tm_map(corpus_novia, removeWords, stopwords('spanish'))))),decreasing=TRUE) 
wordcloud2(data= data.frame(word = names(words),freq=words), size=3, color='random-dark')

corpus_mecanografa <- tm_map(corpus_mecanografa, removeWords, stopwords('spanish'))
words <- sort(rowSums(as.matrix(TermDocumentMatrix(tm_map(corpus_mecanografa, removeWords, stopwords('spanish'))))),decreasing=TRUE) 
wordcloud2(data= data.frame(word = names(words),freq=words), size=3, color='random-dark')

#-------------- FIN NALISIS DEL TEXTO -----------------------------------------


#-------------- ANALISIS DE SENTIMIENTOS -----------------------------------------

barplot(colSums(get_nrc_sentiment(lineas_viejo)), las = 2, col = brewer.pal(10, "Set3"), main = 'Sentimientos del viejo', border = "black")
barplot(colSums(get_nrc_sentiment(lineas_joven)), las = 2, col = brewer.pal(10, "Set3"), main = 'Sentimientos del joven', border = "black")
barplot(colSums(get_nrc_sentiment(lineas_criada)), las = 2, col = brewer.pal(10, "Set3"), main = 'Sentimientos de la criada', border = "black")
barplot(colSums(get_nrc_sentiment(lineas_novia)), las = 2, col = brewer.pal(10, "Set3"), main = 'Sentimientos del novia', border = "black")
barplot(colSums(get_nrc_sentiment(lineas_mecanografa)), las = 2, col = brewer.pal(10, "Set3"), main = 'Sentimientos de la mecanografa', border = "black")

#-------------- FIN ANALISIS DE SENTIMIENTOS -----------------------------------------

