library(rvest)
library(plyr)
library(dplyr)

top50key <- read_html("https://getsongkey.com/charts/top")
top50bpm <- read_html("https://getsongbpm.com/charts/top")

song_rank <- 1:50
artist_name <- html_text(html_nodes(top50key, '.artist'))
song_name <- html_text(html_nodes(top50key, '.track'))
scraped_key <- html_text(html_nodes(top50key, '.camelot'))
song_bpm <- html_text(html_nodes(top50bpm, '.bpm'))
song_bpm <- as.integer(substr(song_bpm, 1, nchar(as.character(song_bpm))-4))
song_bpm <- ifelse(song_bpm > 140, song_bpm/2, song_bpm)

camelot_conversion <- data.frame("scraped_key" = c("1d", "2d", "3d", "4d", "5d", "6d", "7d", "8d", "9d", "10d", "11d", "12d",
                                                        "1m", "2m", "3m", "4m", "5m", "6m", "7m", "8m", "9m", "10m", "11m", "12m"),
                                 "song_key" = c("C", "G", "D", "A", "E", "B", "F#", "Db", "Ab", "Eb", "Bb", "F",
                                           "Am", "Em", "Bm", "F#m", "C#m", "G#m", "Ebm", "Bbm", "Fm", "Cm", "Gm", "Dm"))

chart_df <- data.frame(song_rank, artist_name, song_name, song_bpm, scraped_key)
my.df <- join(chart_df, camelot_conversion, by = "scraped_key")
rm(chart_df, artist_name, scraped_key, song_bpm, song_name, song_rank, camelot_conversion, top50key, top50bpm)
my.df <- subset(my.df, select=-c(scraped_key))
 

camelot_table <- data.frame("song_key" = c("C", "G", "D", "A", "E", "B", "F#", "Db", "Ab", "Eb", "Bb", "F",
                                      "Am", "Em", "Bm", "F#m", "C#m", "G#m", "Ebm", "Bbm", "Fm", "Cm", "Gm", "Dm"),
                            "IV" = c("F","C", "G", "D", "A", "E", "B", "F#", "Db", "Ab", "Eb", "Bb", 
                                       "Dm", "Am", "Em", "Bm", "F#m", "C#m", "G#m", "Ebm", "Bbm", "Fm", "Cm", "Gm"),
                            "V" = c("G", "D", "A", "E", "B", "F#", "Db", "Ab", "Eb", "Bb", "F", "C",
                                        "Em", "Bm", "F#m", "C#m", "G#m", "Ebm", "Bbm", "Fm", "Cm", "Gm", "Dm", "Am"),
                            "vi" = c("Am", "Em", "Bm", "F#m", "C#m", "G#m", "Ebm", "Bbm", "Fm", "Cm", "Gm", "Dm",
                                     "C", "G", "D", "A", "E", "B", "F#", "Db", "Ab", "Eb", "Bb", "F"))

my.df <- join(my.df, camelot_table, by = "song_key")
rm(camelot_table)

current_bpm <- as.integer(readline("Enter BPM: "))
current_key <- readline("Enter Key: ")

my.df <- filter(my.df, abs(current_bpm-my.df$song_bpm)< 10 | abs((current_bpm/2) - my.df$song_bpm) < 5)

key.df <- filter(my.df, current_key == my.df$song_key)
IV.df <- filter(my.df, current_key == my.df$IV)
V.df <- filter(my.df, current_key == my.df$V)
vi.df <- filter(my.df, current_key == my.df$vi)
none.df <- filter(my.df, current_key != my.df$song_key & current_key != my.df$IV & 
                    current_key != my.df$V & current_key != my.df$vi)
recommend.df <- rbind(key.df, IV.df, V.df, vi.df, none.df)
cat("Current BPM is: ", current_bpm, "\n")
cat("Current Key is: ", current_key, "\n", "\n")
rm(my.df, key.df, IV.df, V.df, vi.df, none.df, current_bpm, current_key)

print(recommend.df)