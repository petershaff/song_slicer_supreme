library(tidyverse)
library(httr)
library(jsonlite)
library(glmnet)

#PRELIMINARIES/GET API TOKEN
app_id <- 'song_slicer_supreme_CHOP_UP_YOUR_BEETS_IN_A_FLASH'
client_id <- '567748bac9a24379a85e8d7bd1c0ef93'
client_secret <- 'b619c711e61f42cb8d0b23d5411135e1'

spotify_endpoint <- oauth_endpoint(NULL, "https://accounts.spotify.com/authorize", "https://accounts.spotify.com/api/token")
spotify_app <- oauth_app("spotify", client_id, client_secret)
spotify_token <- oauth2.0_token(spotify_endpoint, spotify_app, scope = 'user-library-read')

user <- GET('https://api.spotify.com/v1/me', spotify_token) %>% content
user_id <- user$id


#JUST LIKE A LOT OF HANDY FUNCTIONS TO DO THINGS I PROBABLY DON'T NEED FUNCTIONS FOR
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
    N = 86*4
    albums <- as.data.frame(matrix(0, nrow=N, ncol=2))
    names(albums) <- c('name', 'id')
    for (i in 1:7){
        print(i)
        my_albums <- GET('https://api.spotify.com/v1/me/albums', query = list(limit=43, offset=43*i), spotify_token) %>% content
        albums[(43*(i-1)+1):(43*i),1] <- unlist(map(my_albums$items, function(x) {return(x$album$name)}))
        albums[(43*(i-1)+1):(43*i),2] <- unlist(map(my_albums$items, function(x) {return(x$album$id)}))
        Sys.sleep(30)
    }
    return(albums[1:(N-43),])
}

get_album_tracks <- function(album_id){
    tracks <- GET(paste0('https://api.spotify.com/v1/albums/',album_id,'/tracks'), spotify_token) %>% content

    name <- unlist(lapply(tracks$items, function(x) {return(x$name)}))
    ids <-unlist(lapply(tracks$items, function(x) {return(x$id)}))
    return(as.data.frame(cbind(name,ids)))

}

#IMPORT AND CLEAN PLAYLIST TRACK DATA
playlists <- get_playlists(user_id)
bleep_id <- subset(playlists,playlists$names == 'BL33P C O R E')$ids
beats_id <- subset(playlists,playlists$names == 'chewable beats')$ids

bleep_tracks <- get_playlist_tracks(user_id, bleep_id)
beats_tracks <- get_playlist_tracks(user_id, beats_id)
    
bleep_feats <- map_df(bleep_tracks$ids, get_track_features)
beats_feats <- map_df(beats_tracks$ids, get_track_features)

bleep_an <- as_data_frame(do.call(rbind, map(bleep_tracks$ids, get_track_analysis)))
names(bleep_an) <- c('pitch', 'timbre')
beats_an <- as_data_frame(do.call(rbind, map(beats_tracks$ids, get_track_analysis)))
names(beats_an) <- c('pitch', 'timbre')

bleep_feats <- cbind(bleep_feats, bleep_an)
beats_feats <- cbind(beats_feats, beats_an)

bleep_feats$name <- bleep_tracks$names
bleep_feats$id <- bleep_tracks$ids
bleep_feats$plylst <- rep('bleep',dim(bleep_tracks)[1])

beats_feats$name <- beats_tracks$names
beats_feats$id <- beats_tracks$names
beats_feats$plylst <- rep('beats',dim(beats_tracks)[1])

all_tracks <- rbind(beats_feats,bleep_feats)
all_tracks_feats <- all_tracks[,c(1:11,19:22)]

all_tracks_feats$'plylst_lbl' <- rep(0,dim(all_tracks_feats)[1])
all_tracks_feats[all_tracks_feats$plylst=='beats',]$plylst_lbl <- 1

all_tracks_feats$'lbl_col' <- rep('blue',dim(all_tracks_feats)[1])
all_tracks_feats[all_tracks_feats$plylst=='beats',]$lbl_col <- 'red'

png('feature_plots.png')
plot(all_tracks_feats[,1:13], col = all_tracks_feats$lbl_col)
dev.off()


#IMPORT AND CLEAN "YOUR TRACKS" DATA
my_albums <- get_my_albums()

hold1 <- map_df(my_albums[1:100,2],get_album_tracks)
hold2 <- map_df(my_albums[100:200,2],get_album_tracks)
hold3 <- map_df(my_albums[200:301,2],get_album_tracks)

my_songs <- rbind(hold1,hold2)
my_songs <- rbind(my_songs,hold3)

#Import and clean OOS track dat
all_song_feats <- map_df(my_songs$id, get_track_features)

song_mat <- t(as.matrix(all_song_feats[[1]]))
for (song in all_song_feats) {
    if (length(song) == 18) {
        song_mat <- rbind(song_mat, t(as.matrix(song)))
    }
}

song_mat <- cbind(song_mat,my_songs$name[1:4680])

new_dat <- song_mat[,c(1:11,20)]
new_dat <- new_dat[!map_lgl(new_dat[,1], is.null),]
new_dat <- new_dat[-1993,]
dat_names <- map_chr(new_dat[,12], as.character)
new_dat <- apply(new_dat[,1:11], 2, function(x) {return(map_dbl(x, as.numeric))})

#\BEGIN{STATISTICS}
dat <- all_tracks_feats[,c(1:13,16)]
logit_fit <- glm(plylst_lbl ~., data=dat, family=binomial)

miss_rate <- sum(abs(round(fitted(logit_fit))-dat$plylst_lbl))/37
misses <- all_tracks[dat$plylst_lbl != round(fitted(logit_fit)),21]

predicted <- as.data.frame(cbind(all_tracks$name, as.numeric(100*fitted(logit_fit))))
predicted <- predicted[order(fitted(logit_fit)),]
names(predicted) <- c('Trck_Nm','Score')

cv_fit <- cv.glmnet( as.matrix(dat[,1:11]), dat[,14], family = 'binomial', type.measure = 'class', alpha=1)

l0 <- cv_fit$'lambda.min'
l2 <- cv_fit$'lambda.1se$'
l1 <- mean(c(l0,l2))

var_select <- map(c(l0,l1,l2), function(x) {return(coef(cv_fit, s=x))})

oos_pred <- predict(cv_fit, newx = new_dat, type='response', s=l0)

best_recs <- abs(oos_pred - .5) > .49
rec_tracks <- as_data_frame(dat_names[best_recs])
rec_tracks$score <- round(oos_pred[best_recs],4)
names(rec_tracks) <- c('name','score')

#Fun fact: when I swap Geotic's "Sunspell" for "Billionth Remnant I get perfect within-sample prediction, whereas otherwise I get about a 13% miss rate
#test_id <- bleep_tracks$ids[1]
#test_an <- get_track_analysis(test_id)

