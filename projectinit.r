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

# citationsData = data.frame( citations$OutFilm, citations$OutFilm_Year, citationsIndex$OutIndex,
#                             citations$InFilm,  citations$InFilm_Year,  citationsIndex$InIndex );
# write.csv( citationsData, "citationsWithIndex.csv" );

filmAttributes$imdb_votes = as.integer( gsub( ",", "", filmAttributes$imdb_votes ) );
filmAttributes$imdb_rating[ is.na( filmAttributes$imdb_rating ) ] = 0;
filmAttributes$imdb_votes[  is.na( filmAttributes$imdb_votes  ) ] = 0;

citationsMatrix = matrix( 0, length( allFilms ), length( allFilms ) );
citationsMatrix[ as.matrix( citationsIndex[ , 1:2 ] ) ] = 1;

citationsNetwork = network( citationsMatrix, directed = T );
# set.vertex.attribute( citationsNetwork, "vertex.names", filmAttributes$Film );
set.vertex.attribute( citationsNetwork, "imdb_rating", filmAttributes$imdb_rating );
set.vertex.attribute( citationsNetwork, "imdb_votes",  filmAttributes$imdb_votes  );
set.vertex.attribute( citationsNetwork, "year",  filmAttributes$Year  );

fit = ergm( citationsNetwork ~ edges + transitive +
            nodeicov( "imdb_rating" ) + nodeicov( "imdb_votes" ) + nodeicov( "year" ), seed = 1 );
summary( fit );



