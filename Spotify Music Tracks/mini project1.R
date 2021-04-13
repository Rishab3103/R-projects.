installed.packages("tidyverse")
install.packages("reshape2")
library(reshape2)
library(tidyverse)
library(ggplot2)
spotify<-read.csv("spotify_songs.csv")
summary(spotify$track_popularity)

popularity_relationship<-spotify%>%
  select(track_artist,track_popularity,danceability,energy,key,loudness)

summary(popularity_relationship)
refined_popularity<-popularity_relationship%>%
  filter(track_popularity>median(track_popularity),danceability>median(danceability),energy>median(energy),key>median(key),loudness>median(loudness))
refined_popularity
cor(refined_popularity$track_popularity,refined_popularity$danceability)
cor(refined_popularity$track_popularity,refined_popularity$energy)
cor(refined_popularity$track_popularity,refined_popularity$loudness)
spotify
cor(spotify$danceability,spotify$liveness)
cor(spotify$danceability,spotify$tempo)
cor(spotify$track_popularity,spotify$duration_ms)
ggplot(spotify, aes(track_popularity, duration_ms, color=playlist_genre))+ geom_point(alpha=)
cor(spotify$danceability, spotify$energy)
cor(spotify$track_popularity,spotify$energy)
cor(spotify$track_popularity,spotify$duration_ms)
cor(spotify$track_popularity,spotify$acousticness)
cor(spotify$track_popularity,spotify$danceability)
cor(spotify$track_popularity,spotify$speechiness)
cor(spotify$liveness,spotify$acousticness)
cor(spotify$liveness,spotify$instrumentalness)
ggplot(spotify, aes(liveness,energy))+geom_smooth(method="lm")
ggplot(spotify, aes(liveness,instrumentalness))+geom_smooth(method="lm")
ggplot(spotify, aes(danceability,loudness))+geom_smooth(method="lm")
ggplot(spotify, aes(danceability,liveness))+geom_smooth(method="lm")
ggplot(spotify, aes(danceability,liveness, color=playlist_genre))+geom_point()
ggplot(spotify, aes(danceability,liveness, color=playlist_genre))+geom_smooth(method="lm")+facet_wrap(~playlist_genre)
ggplot(spotify, aes(danceability,energy, color=playlist_genre))+geom_smooth(method="lm")+facet_wrap(~playlist_genre)
ggplot(spotify, aes(danceability,energy, color=playlist_subgenre))+geom_smooth(method="lm")+facet_wrap(~playlist_genre)
ggplot(spotify, aes(danceability, color=playlist_genre))+geom_bar()+facet_wrap(~playlist_genre)
ggplot(spotify, aes(track_popularity,color=playlist_genre))+geom_boxplot()+facet_wrap(~playlist_genre)
ggplot(spotify, aes(track_popularity,energy,color=playlist_genre))+geom_smooth(method="lm")+facet_wrap(~playlist_genre)
ggplot(spotify, aes(track_popularity,danceability,color=playlist_genre))+geom_smooth(method="lm")+facet_wrap(~playlist_genre)+ coord_flip()
ggplot(spotify, aes(track_popularity,liveness,color=playlist_genre))+geom_smooth(method="lm")+facet_wrap(~playlist_genre)+coord_flip()
ggplot(spotify, aes(acousticness,track_popularity,color=playlist_genre))+geom_smooth(method="lm")+facet_wrap(~playlist_genre)
ggplot(spotify, aes(danceability,color=playlist_genre))+geom_density()+facet_wrap(~playlist_genre)
ggplot(spotify, aes(loudness, track_popularity,color=playlist_genre))+geom_smooth(method="lm")+facet_wrap(~playlist_genre)
ggplot(spotify, aes(loudness,color=playlist_genre))+geom_density()+facet_wrap(~playlist_genre)
ggplot(spotify, aes(energy,track_popularity,color=playlist_genre))+geom_smooth(method="lm")+facet_wrap(~playlist_genre)

ggplot(spotify, aes(track_popularity,acousticness,color=playlist_genre))+geom_smooth(method="lm")+facet_wrap(~playlist_genre)
ggplot(spotify, aes(tempo,track_popularity,color=playlist_genre))+geom_smooth(method="lm")+facet_wrap(~playlist_genre)
ggplot(spotify, aes(instrumentalness,track_popularity))+geom_smooth(method="lm")+facet_wrap(~playlist_genre["edm"])
#ggplot(spotify, aes(track_popularity,color=playlist_genre))+geom_density()+facet_wrap(~track_album_release_date["1998-"])
#ggplot(spotify, aes(track_popularity, energy,size=playlist_genre, color=playlist_genre))+geom_point()
ggplot(spotify, aes(energy,color=playlist_genre))+geom_density()
ggplot(spotify, aes(track_popularity,energy,color=playlist_genre))+geom_point()+labs(title="Track Popularity Distribution")
ggplot(spotify, aes(acousticness,color=playlist_genre))+geom_boxplot()
date<- spotify%>%
  select(track_album_release_date,track_popularity)%>%
  arrange(desc(track_popularity))
ggplot(spotify,aes(track_popularity,energy,color=playlist_genre))+geom_violin()+facet_wrap(~playlist_genre)
ggplot(spotify,aes(track_popularity,danceability,color=playlist_genre))+geom_violin()+facet_wrap(~playlist_genre)
ggplot(spotify,aes(track_popularity,acousticness,color=playlist_genre))+geom_violin()+facet_wrap(~playlist_genre)
spotify_general<-spotify%>%
  select(track_popularity, playlist_genre)
ggplot(spotify_general,aes(track_popularity,color=playlist_genre))+geom_bar()+facet_wrap(~playlist_genre)



pop_2010_2020<-c()
pop_2010_2020_genre<-c()
c=0
for(i in 1:length(date$track_album_release_date)){
  if(grepl("2020",date$track_album_release_date[i])|grepl("2019",date$track_album_release_date[i])|grepl("2018",date$track_album_release_date[i])|grepl("2013",date$track_album_release_date[i])
     |grepl("2017",date$track_album_release_date[i])|grepl("2016",date$track_album_release_date[i])&grepl("2015",date$track_album_release_date[i])|grepl("2014",date$track_album_release_date[i])
     | grepl("2012",date$track_album_release_date[i])|grepl("2011",date$track_album_release_date[i])|grepl("2010",date$track_album_release_date[i])){
    
    pop_2010_2020[c]= date$track_popularity[i]
    pop_2010_2020_genre[c]=date$playlist_genre[i]
    c=c+1
  }
}
pop_2000_2010<-c()
pop_2000_2010_genre<-c()
c=0
for(i in 1:length(date$track_album_release_date)){
  if(grepl("2010",date$track_album_release_date[i])|grepl("2009",date$track_album_release_date[i])|grepl("2008",date$track_album_release_date[i])|grepl("2003",date$track_album_release_date[i])
     |grepl("2007",date$track_album_release_date[i])|grepl("2006",date$track_album_release_date[i])&grepl("2005",date$track_album_release_date[i])|grepl("2004",date$track_album_release_date[i])
     | grepl("2002",date$track_album_release_date[i])|grepl("2001",date$track_album_release_date[i])|grepl("2000",date$track_album_release_date[i])){
    
    pop_2000_2010[c]= date$track_popularity[i]
    pop_2000_2010_genre[c]=date$playlist_genre[i]
    c=c+1
  }
}


pop_1990_2000<-c()
pop_1990_2000_genre<-c()
c=0
for(i in 1:length(date$track_album_release_date)){
  if(grepl("2000",date$track_album_release_date[i])|grepl("1999",date$track_album_release_date[i])|grepl("1998",date$track_album_release_date[i])|grepl("1993",date$track_album_release_date[i])
     |grepl("1997",date$track_album_release_date[i])|grepl("1996",date$track_album_release_date[i])&grepl("1995",date$track_album_release_date[i])|grepl("1994",date$track_album_release_date[i])
     | grepl("1992",date$track_album_release_date[i])|grepl("1991",date$track_album_release_date[i])|grepl("1990",date$track_album_release_date[i])){
    
    pop_1990_2000[c]= date$track_popularity[i]
    pop_1990_2000_genre[c]=date$playlist_genre[i]
    c=c+1
  }
}
pop_1980_1990<-c()
pop_1980_1990_genre<-c()
date$track_popularity[i]
c=0
for(i in 1:length(date$track_album_release_date)){
  if(grepl("1990",date$track_album_release_date[i])|grepl("1989",date$track_album_release_date[i])|grepl("1988",date$track_album_release_date[i])|grepl("1983",date$track_album_release_date[i])
     |grepl("1987",date$track_album_release_date[i])|grepl("1986",date$track_album_release_date[i])&grepl("1985",date$track_album_release_date[i])|grepl("1984",date$track_album_release_date[i])
     | grepl("1982",date$track_album_release_date[i])|grepl("1981",date$track_album_release_date[i])|grepl("1980",date$track_album_release_date[i])){
    
    pop_1980_1990[c]= date$track_popularity[i]
    pop_1980_1990_genre[c]=date$playlist_genre[i]
    c=c+1
  }
}
pop_1980_1990_df<-data.frame(pop_1980_1990_genre,pop_1980_1990)
pop_1990_2000_df<-data.frame(pop_1990_2000_genre,pop_1990_2000)
pop_2000_2010_df<-data.frame(pop_2000_2010_genre,pop_2000_2010)
pop_2010_2020_df<-data.frame(pop_2010_2020_genre,pop_2010_2020)
mean_1980_1990<-mean(pop_1980_1990_df$pop_1980_1990)
ggplot(pop_1980_1990_df,aes(pop_1980_1990,color=pop_1980_1990_genre))+ geom_boxplot()+labs(title="Distribution of track popularity over genre from 1980-1990")
ggplot(pop_1990_2000_df,aes(pop_1990_2000,color=pop_1990_2000_genre))+ geom_boxplot()+labs(title="Distribution of track popularity over genre from 1990-2000 ")
ggplot(pop_2000_2010_df,aes(pop_2000_2010,color=pop_2000_2010_genre))+geom_boxplot()+labs(title="Distribution of track popularity over genre from 2000-2010 ")
ggplot(pop_2010_2020_df,aes(pop_2010_2020,color=pop_2010_2020_genre))+geom_boxplot()+labs(title="Distribution of track popularity over genre from 2010-2020 ")


pop_1980_1990_genre
pop_2010_2020_factor<-as.factor(pop_2010_2020)
pop_2010_2020_factor
qplot(pop_2010_2020_factor)+ geom_density()+stat_bin(bins = 30)
da<-as.factor(spotify$track_album_release_date)
da
summary(pop_2015)
summary(pop_2016)
summary(pop_2017)
summary(pop_2018)
summary(pop_2019)
summary(pop_2020)
qplot(pop_2019)+ geom_density()+stat_bin(bins = 30)
qplot(pop_2020)+ geom_density()+stat_bin(bins = 30)
ggplot(aes(pop_2015))+
class(spotify$track_album_release_date)
summary(spotify)
genre_popularity_above_mean<-as.factor(spotify$playlist_genre[which(spotify$track_popularity>mean(spotify$track_popularity))])
summary(genre_popularity_above_mean)
genre_spotify<-as.factor(spotify$playlist_genre)
summary(genre_spotify)
qplot(genre_popularity_above_mean, fill="red")+ geom_density()+ labs(title = "Track Popularity over mean")
genre_loudness_above_median<-as.factor(spotify$playlist_genre[which(spotify$loudness>median(spotify$loudness))])
qplot(genre_loudness_above_median,fill="red")+ geom_density()+labs(title = "Genre Loudness over median ")
genre_tempo_above_median<-as.factor(spotify$playlist_genre[which(spotify$tempo>median(spotify$tempo))])
qplot(genre_tempo_above_median, fill="red")+ geom_density()+labs(title = "Genre Tempo over median ")
spotify
genre_energy_above_median<-as.factor(spotify$playlist_genre[which(spotify$energy>median(spotify$energy))])
qplot(genre_energy_above_median, fill="red")+ geom_density()+labs(title = "Genre Energy over median ")
genre_liveness_above_median<-as.factor(spotify$playlist_genre[which(spotify$liveness>median(spotify$liveness))])
qplot(genre_liveness_above_median,fill="red")+ geom_density()+labs(title = "Genre Liveness over median ")
genre_acousticness_above_median<-as.factor(spotify$playlist_genre[which(spotify$acousticness>median(spotify$acousticness))])
qplot(genre_acousticness_above_median,fill="red")+ geom_density()+labs(title = "Genre Acousticness over median ")
genre_speechiness_above_median<-as.factor(spotify$playlist_genre[which(spotify$speechiness>median(spotify$speechiness))])
qplot(genre_speechiness_above_median, fill="red")+ geom_density()+labs(title = "Genre Speechiness over median ")

spotify%>%
  ggplot(aes(track_popularity, speechiness, color=playlist_genre))+ geom_smooth(method="lm")+ facet_wrap(~playlist_genre)+ coord_flip()
spotify_sorted<-spotify%>%
  group_by(track_id,track_popularity)%>%
  count(n_distinct(track_id))

spotify_sorted_artists<-spotify%>%
  group_by(track_artist,track_popularity)%>%
  count(n_distinct(track_artist))
spotify_sorted_artists

ggplot(data = spotify, aes(x=track_popularity, y=energy, fill="value")) + geom_tile()
library(reshape2)
library(ggplot2)

features <- c("track_popularity","danceability", "loudness", "energy", "speechiness", "valence", "duration_ms")
cormat <- round(cor(spotify[c("track_popularity", features)]), 2)

melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
cormat
ggplot(spotify, aes(energy, track_popularity,color=playlist_genre))+geom_point(size=0.4)+labs(title="Energy v Track Popularity")+facet_wrap(~playlist_genre)
ggplot(spotify, aes(energy, track_popularity,color=playlist_genre))+geom_point(size=0.4)+labs(title="Energy v Track Popularity")
ggplot(spotify, aes(energy,track_popularity,color=playlist_genre))+geom_smooth(method="lm")+labs(title="Energy v Track Popularity")+facet_wrap(~playlist_genre)
ggplot(spotify, aes(duration_ms,track_popularity,color=playlist_genre))+geom_point(size=0.4)+labs(title="Track Duration v Track Popularity")+facet_wrap(~playlist_genre)
ggplot(spotify, aes(duration_ms,track_popularity,color=playlist_genre))+geom_point(size=0.4)+labs(title="Track Duration v Track Popularity")
ggplot(spotify, aes(duration_ms,track_popularity,color=playlist_genre))+geom_point(size=0.4)+labs(title="Track Duration v Track Popularity")+facet_wrap(~playlist_genre)
ggplot(spotify, aes(track_popularity,danceability,color=playlist_genre))+geom_point(size=0.2)+facet_wrap(~playlist_genre)+labs(title="Danceability v Track Popularity")
ggplot(spotify, aes(track_popularity,liveness,color=playlist_genre))+geom_point(size=0.2)+facet_wrap(~playlist_genre)+labs(title="Liveness v Track Popularity")
ggplot(spotify, aes(track_popularity,energy,color=playlist_genre))+geom_smooth(method="lm")+facet_wrap(~playlist_genre)+labs(title="Energy v Track Popularity")
ggplot(spotify, aes(track_popularity,acousticness,color=playlist_genre))+geom_smooth(method="lm")+facet_wrap(~playlist_genre)+labs(title="Acousticness v Track Popularity")

?heights
heights
