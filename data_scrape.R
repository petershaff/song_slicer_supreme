library(tidyverse)
library(httr)
library(jsonlite)

#PRELIMINARIES/GET API TOKEN
app_id <- 'song_slicer_supreme_CHOP_UP_YOUR_BEETS_IN_A_FLASH'
client_id <- '567748bac9a24379a85e8d7bd1c0ef93'
client_secret <- 'b619c711e61f42cb8d0b23d5411135e1'

spotify_endpoint <- oauth_endpoint(NULL, "https://accounts.spotify.com/authorize", "https://accounts.spotify.com/api/token")
spotify_app <- oauth_app("spotify", client_id, client_secret)
spotify_token <- oauth2.0_token(spotify_endpoint, spotify_app, scope = 'user-library-read')

user <- GET('https://api.spotify.com/v1/me', spotify_token) %>% content
user_id <- user$id


#JUST LIKE A MESS OF HANDY FUNCTIONS TO DO THINGS I PROBABLY DON'T NEED FUNCTIONS FOR
get_playlists <- function(user_id){
    playlists_url <- paste('https://api.spotify.com/v1/users/', user_id, '/playlists', sep='')
    playlists <- GET(playlists_url,spotify_token) %>% content

    names <- map_chr(playlists$items, function(x) { x$name } )
    ids <- map_chr(playlists$items, function(x) { x$id } ) 

    return(as_data_frame(cbind(names,ids)))
    
}

get_playlist_tracks <- function(user_id, playlist_id){
    playlist_url <- paste('https://api.spotify.com/v1/users/', user_id , '/playlists/', playlist_id, '/tracks', sep='')
    tracks <- GET(playlist_url, spotify_token) %>% content

    names <- map_chr(tracks$items, function(x) { x$track$name })
    ids <- map_chr(tracks$items, function(x) { x$track$id })

    return(as_data_frame(cbind(names,ids)))
    
}

get_track_features <- function(track_id){
    track_features_url <- paste('https://api.spotify.com/v1/audio-features/', track_id, sep='')
    features <- GET(track_features_url, spotify_token) %>% content

    return(features)
}

get_track_analysis <- function(track_id){
    track_features_url <- paste('https://api.spotify.com/v1/audio-analysis/', track_id, sep='')
    analysis <- GET(track_features_url, spotify_token) %>% content

    #return(analysis)
    
    pitches <- as.numeric(analysis[7]$segment[length(analysis$segments)][[1]]$pitches)
    timbre <- as.numeric(analysis[7]$segment[length(analysis$segments)][[1]]$timbre)
    return(c(mean(pitches),mean(timbre)))

    #return(features)
}

get_my_albums <- function(){
    names <- c()
    ids <- c()
    for (i in 1:10){
        print(i)
        my_albums <- GET('https://api.spotify.com/v1/me/albums', query = list(limit=43, offset=43*i), spotify_token) %>% content
        names <- c(names, unlist(map(my_albums$items, function(x) {return(x$album$name)})) )
        ids <- c(ids, unlist(map(my_albums$items, function(x) {return(x$album$id)})) )
        Sys.sleep(30)
    }
    return( data.frame(names, ids) )
}

get_album_tracks <- function(album_id){
    tracks <- GET(paste0('https://api.spotify.com/v1/albums/',album_id,'/tracks'), spotify_token) %>% content

    name <- unlist(lapply(tracks$items, function(x) {return(x$name)}))
    ids <-unlist(lapply(tracks$items, function(x) {return(x$id)}))
    return( data.frame(name,ids) )

}

get_rand_album_tracks <- function(album_id){
    tracks <- get_album_tracks(album_id)
    return( tracks[ sample( nrow(tracks), 1), ] )

}

#IMPORT AND CLEAN PLAYLIST TRACK DATA

#Get ids for relevant playlists
playlist.ids <- get_playlists(user_id)

bleep_id <- subset(playlists,playlists$names == 'BL33P C O R E')$ids
beats_id <- subset(playlists,playlists$names == 'chewy beats')$ids
bangz_id <- subset(playlists,playlists$names == 'bangers from beyond the stars')$ids

#Pull track lists and then track features
bleep_tracks <- get_playlist_tracks(user_id, bleep_id)
beats_tracks <- get_playlist_tracks(user_id, beats_id)
bangz_tracks <- get_playlist_tracks(user_id, bangz_id)

bleep <- map_df(bleep_tracks$ids, get_track_features)
beats <- map_df(beats_tracks$ids, get_track_features)
bangz <- map_df(beats_tracks$ids, get_track_features)

bleep$name <- bleep_tracks$names
bleep$id <- bleep_tracks$ids
bleep$label <- rep('bleeps', dim(bleep_tracks)[1])
bleep <- subset(bleep, select = -c(type, uri, track_href, analysis_url, duration_ms, time_signature) )

beats$name <- beats_tracks$names
beats$id <- beats_tracks$ids
beats$label <- rep('beats', dim(beats_tracks)[1])
beats <- subset(beats, select = -c(type, uri, track_href, analysis_url, duration_ms, time_signature) )

bangz$name <- beats_tracks$names
bangz$id <- beats_tracks$ids
bangz$label <- rep('bangerz', dim(beats_tracks)[1])
bangz <- subset(bangz, select = -c(type, uri, track_href, analysis_url, duration_ms, time_signature) )

#IMPORT AND CLEAN RANDO TRACKS
my_albums <- my_albs
#my_albums <- get_my_albums()
my_albums <- my_albums[ sample( nrow(my_albums), 100 ), ]

rando_tracks <- map_df(my_albums[,2], get_rand_album_tracks)

rando <- map_df(rando_tracks$ids, get_track_features)

rando$name <- rando_tracks$names
rando$id <- rando_tracks$ids
rando$label <- rep('rando', dim(rando_tracks)[1])
rando <- subset(rando, select = -c(type, uri, track_href, analysis_url, duration_ms, time_signature) )

write.csv(beats, './data/beats.csv')
write.csv(bleep, './data/bleep.csv')
write.csv(bangz, './data/bangz.csv')
write.csv(rando, './data/rando.csv')
