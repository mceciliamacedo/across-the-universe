library(tidyverse)
library(rvest)

url <- c("https://www.letras.mus.br")
urls <- c("https://www.letras.mus.br/the-beatles/discografia/please-please-me-1963/",
          "https://www.letras.mus.br/the-beatles/discografia/with-the-beatles-1963/",
          "https://www.letras.mus.br/the-beatles/discografia/a-hard-days-night-1964/",
          "https://www.letras.mus.br/the-beatles/discografia/beatles-for-sale-1964/",
          "https://www.letras.mus.br/the-beatles/discografia/help-1965/",
          "https://www.letras.mus.br/the-beatles/discografia/rubber-soul-1965/",
          "https://www.letras.mus.br/the-beatles/discografia/revolver-1966/",
          "https://www.letras.mus.br/the-beatles/discografia/sgt-peppers-lonely-hearts-club-band-1967/",
          "https://www.letras.mus.br/the-beatles/discografia/magical-mystery-tour-1967/",
          "https://www.letras.mus.br/the-beatles/discografia/the-beatles-white-album-1968/",
          "https://www.letras.mus.br/the-beatles/discografia/yellow-submarine-1969/",
          "https://www.letras.mus.br/the-beatles/discografia/abbey-road-1969/",
          "https://www.letras.mus.br/the-beatles/discografia/let-it-be-1970/")

album <- c()
song <- c()
links <- c()
dados <- data.frame()
for (i in 1:length(urls)) {
  html <- read_html(urls[i])
  
  album_name <- html|>
    html_elements("h1")|>
    html_text2()
  album <- c(album, album_name)
  
  songs <- html|>
    html_elements("div.songList-table")|>
    html_elements("li.songList-table-row")|>
    html_text2()
  song <- c(song, songs)
  
  links_musicas <- html|>
    html_elements("a.songList-table-playButton")|>
    html_attr("href")
  links <- c(links, links_musicas)
  
  
  url_musicas <- paste0(url, links_musicas)
  
  lyrics <- c()
  composer <- c()
  for(j in 1:length(url_musicas)){
    html_musicas <- read_html(url_musicas[j])
    
    letras <- html_musicas|>
      html_elements("div.lyric-original")|>
      html_text2()|>
      str_replace_all("\n", " ")|>
      str_replace_all("\\s\\s", " ")
    lyrics <- c(lyrics, letras)
    
    compositor <- html_musicas|>
      html_elements("div.lyric-info-composition")|>
      html_text2()|>
      str_remove_all(pattern = ".+: ")|>
      str_remove_all(pattern = "\\. .+")
    composer <- c(composer, compositor)
  }
  
  df_temporario <- data.frame(song = songs,
                              lyrics,
                              album = rep(album_name, length(songs)),
                              composer)
  
  dados <- rbind(dados, df_temporario)
}

View(dados)

dados <- tibble(dados)

#write.csv(dados, "letras_beatles.csv", row.names = FALSE)
