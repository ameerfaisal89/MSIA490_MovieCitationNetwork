library( ergm );
library( sna );
library( network );

setwd( "C:/Users/Ameer Asif Khan/Documents/Academic/Northwestern/MSIA 490 Social Networks Analysis/" );

filmAttributes = read.csv( "movieAttributes.csv", header = T,
                           stringsAsFactors = F, na.strings = c( "", "N/A", "NA", "<NA>" ) );
citationsFull  = read.csv( "MovieCitation.csv",   header = T, stringsAsFactors = F );

outYear = as.integer( gsub( "/.*", "", citationsFull$OutFilm_Year ) );
inYear  = as.integer( gsub( "/.*", "", citationsFull$InFilm_Year ) );

citationsFull$OutFilm = paste( citationsFull$OutFilm, outYear, sep = "" );
citationsFull$InFilm  = paste( citationsFull$InFilm,  inYear,  sep = "" );

# citations = citationsFull[ ( outYear > 1996 & outYear < 2000 ) | ( inYear > 1996 & inYear < 2000 ), ];
citations = citationsFull[ outYear > 2008, ];

# index = !duplicated( citations$OutFilm );
# filmList = as.data.frame( cbind( Film = citations$OutFilm[ index ],
#                                  Year = citations$OutFilm_Year[ index ] ) );
# 
# index = !duplicated(  citations$InFilm );
# filmList = rbind( filmList, cbind( Film =  citations$InFilm[ index ],
#                                    Year =  citations$InFilm_Year[ index ] ) );
# filmList$Year = gsub( "/.*", "", filmList$Year );
# filmList = unique( filmList );
# write.csv( filmList, "uniqueMovies.csv" );

uniqueOutFilms = unique( citations$OutFilm );
uniqueInFilms  = unique( citations$InFilm  );

outYear = citations$OutFilm_Year;
outYear = as.integer( gsub( "/.*", "", outYear ) );

inYear  = as.character( citations$InFilm_Year );
inYear  = as.integer( gsub( "/.*", "", inYear ) );

allFilms = unique( c( uniqueOutFilms, uniqueInFilms ) );

citationsIndex = data.frame( OutIndex = sapply( citations$OutFilm, function( x ) match( x, allFilms ) ),
                             InIndex  = sapply( citations$InFilm,  function( x ) match( x, allFilms ) ) );
# 
# citationsData = data.frame( citations$OutFilm, citations$OutFilm_Year, citationsIndex$OutIndex,
#                             citations$InFilm,  citations$InFilm_Year,  citationsIndex$InIndex );
# write.csv( citationsData, "citationsWithIndex.csv" );

filmAttributes$imdb_votes = as.integer( gsub( ",", "", filmAttributes$imdb_votes ) );
filmAttributes$imdb_rating[  is.na( filmAttributes$imdb_rating  ) ] = 0;
filmAttributes$imdb_votes[   is.na( filmAttributes$imdb_votes   ) ] = 0;
filmAttributes$tomato_meter[ is.na( filmAttributes$tomato_meter ) ] = 0;

topProducers = c( "Warner Bros. Pictures", "Universal Pictures", "Paramount Pictures", 
				  "20th Century Fox", "Sony Pictures Home Entertainment", "Warner Home Video",
				  "MGM", "Columbia Pictures", "MGM Home Entertainment", "MCA Universal Home Video");
filmAttributes$production[ !( filmAttributes$production %in% topProducers ) ] = "Other";

filmAttributes$action = 0;
filmAttributes$action[ grep( ".*action.*", filmAttributes$genre, ignore.case = T ) ] = 1;
filmAttributes$comedy = 0;
filmAttributes$comedy[ grep( ".*comedy.*", filmAttributes$genre, ignore.case = T ) ] = 1;
filmAttributes$crime  = 0;
filmAttributes$crime[  grep( ".*crime.*",  filmAttributes$genre, ignore.case = T ) ] = 1;
filmAttributes$doc    = 0;
filmAttributes$doc[    grep( ".*documentary.*", filmAttributes$genre, ignore.case = T ) ] = 1;
filmAttributes$drama  = 0;
filmAttributes$drama[  grep( ".*drama.*",  filmAttributes$genre, ignore.case = T ) ] = 1;
filmAttributes$horror = 0;
filmAttributes$horror[ grep( ".*horror.*", filmAttributes$genre, ignore.case = T ) ] = 1;
filmAttributes$romance = 0;
filmAttributes$romance[ grep( ".*romance.*", filmAttributes$genre, ignore.case = T ) ] = 1;
filmAttributes$scifi  = 0;
filmAttributes$scifi[  grep( ".*sci-fi.*", filmAttributes$genre, ignore.case = T ) ] = 1;

citationsMatrix = matrix( 0, length( allFilms ), length( allFilms ) );
citationsMatrix[ as.matrix( citationsIndex[ , 1:2 ] ) ] = 1;

citationsNetwork = network( citationsMatrix, directed = T );

##### Giant Component ####
# library( intergraph );
# library( igraph );
# 
# graph = asIgraph( citationsNetwork );
# clust = clusters( graph );
# giantComponent = induced.subgraph( graph, which( clust$membership == which.max( clust$csize ) ) );
# citationsNetwork = asNetwork( giantComponent );
# 
# index = c( );
# 
# for ( i in 1:citationsNetwork$gal$n ) {
#   index = c( index, citationsNetwork$val[[ i ]]$vertex.name );
# }
# 
# filmAttributes = filmAttributes[ index, ];
#####

# set.vertex.attribute( citationsNetwork, "vertex.names", filmAttributes$Film );
set.vertex.attribute( citationsNetwork, "imdb_rating",   filmAttributes$imdb_rating  );
set.vertex.attribute( citationsNetwork, "imdb_votes",    filmAttributes$imdb_votes   );
set.vertex.attribute( citationsNetwork, "tomato_meter",  filmAttributes$tomato_meter );

set.vertex.attribute( citationsNetwork, "year",  filmAttributes$Year  );
set.vertex.attribute( citationsNetwork, "rated", filmAttributes$rated );
set.vertex.attribute( citationsNetwork, "production", filmAttributes$production );

set.vertex.attribute( citationsNetwork, "action",  filmAttributes$action  );
set.vertex.attribute( citationsNetwork, "comedy",  filmAttributes$comedy  );
set.vertex.attribute( citationsNetwork, "crime",   filmAttributes$crime   );
set.vertex.attribute( citationsNetwork, "doc",     filmAttributes$doc     );
set.vertex.attribute( citationsNetwork, "drama",   filmAttributes$drama   );
set.vertex.attribute( citationsNetwork, "horror",  filmAttributes$horror  );
set.vertex.attribute( citationsNetwork, "romance", filmAttributes$romance );
set.vertex.attribute( citationsNetwork, "scifi",   filmAttributes$scifi   );

control = control.ergm( seed = 1, MCMLE.maxit = 100 );

fit1 = ergm( citationsNetwork ~ edges + transitive +
               nodeicov( "imdb_rating" ) + nodeicov( "imdb_votes" ) + absdiff( "year" ) +
               nodeicov( "tomato_meter" ) + nodeifactor( "production", base = 6 ),
             control = control );
summary( fit1 );

fit2 = ergm( citationsNetwork ~ edges + transitive +
               nodeicov( "imdb_rating" ) + nodeicov( "imdb_votes" ) +
               nodeicov( "tomato_meter" ) + nodematch( "production", keep = c( 1:5, 7:11 ) ) +
               absdiff( "year" ),
             control = control );
summary( fit2 );

fit3 = ergm( citationsNetwork ~ edges + transitive +
               nodeicov( "imdb_rating" ) + nodeicov( "imdb_votes" ) +
               nodeicov( "tomato_meter" ) + nodematch( "production", keep = c( 1:5, 7:11 ) ) +
               nodeifactor( "production", base = c( 2:6, 8, 11 ) ) + absdiff( "year" ),
             control = control );
summary( fit3 );

fit4 = ergm( citationsNetwork ~ edges + transitive +
               nodeicov( "imdb_rating" ) + nodeicov( "imdb_votes" ) +
               nodeicov( "tomato_meter" ) + nodematch( "production", keep = c( 1:5, 7:11 ) ) +
               nodeifactor( "production", base = c( 2:6, 8, 11 ) ) +
               nodematch( "comedy", keep = 1 ) + nodematch( "crime",   keep = 1 ) +
               nodematch( "doc",    keep = 1 ) + nodematch( "romance", keep = 1 ) +
               nodematch( "scifi",  keep = 1 ),
             control = control );
summary( fit4 );

fit5 = ergm( citationsNetwork ~ edges + transitive +
               nodeicov( "imdb_rating" ) + nodeicov( "imdb_votes" ) +
               nodeicov( "tomato_meter" ) + nodematch( "production", keep = c( 1:5, 7:11 ) ) +
               nodeifactor( "production", base = 6 ) +
               nodematch( "action", keep = 1 ) + nodematch( "comedy",  keep = 1 ) +
               nodematch( "crime",  keep = 1 ) + nodematch( "doc",     keep = 1 ) +
               nodematch( "horror", keep = 1 ) + nodematch( "romance", keep = 1 ) +
               nodematch( "scifi",  keep = 1 ),
             control = control );
summary( fit5 );

fit_gof_degree = gof( fit5 ~ idegree );
fit_gof_degree
plot( fit_gof_degree );

fit_gof_distance = gof( fit5 ~ distance );
fit_gof_distance
plot( fit_gof_distance );

pdf( "diagnostics.pdf" );
mcmc.diagnostics( fit5 );
dev.off( );



