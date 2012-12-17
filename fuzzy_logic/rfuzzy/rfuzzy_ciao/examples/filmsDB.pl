% Rfuzzy approach for a film database.
% CopyLeft

:-module(filmsDB,_,[rfuzzy, clpr]).
% Compilation time debug can be activated  by adding to the packages list [rfuzzy, clpr] the package pkgs_output_debug.
% Running time debug can be activated removing the comment marker % at the beginning of the following line.
% :- activate_rfuzzy_debug.

% Define the films database format.
rfuzzy_define_database(film/7, 
	[(film_name, rfuzzy_string_type), 
	  (release_year, rfuzzy_integer_type), 
	   (duration_in_minutes, rfuzzy_integer_type),
	    (genre, rfuzzy_enum_type), 
	     (original_language, rfuzzy_integer_type), 
	      (directed_by, rfuzzy_integer_type), 
	       (distributed_by, rfuzzy_enum_type)]).

film('The Godfather', 1972, 207, drama, english, 'Francis Ford Coppola', 'Paramount Pictures').
film('Casablanca', 1946, 172, romance, english, 'Michael Curtiz', 'Warner Bros').
film('Cera una volta il West', 1968, 165, western, italian, 'Sergio Leone', 'MYMONETRO').
film('El laberinto del fauno', 2006, 107, drama, spanish, 'Guillermo del Toro', 'Esperanto Films').
film('Il buono, il brutto, il cattivo', 1967, 141, adventure, italian, 'Sergio Leone', 'United Artists').
film('Finding Nemo', 2003, 112, comedy, english, 'Andrew Stanton and Lee Unkrich', 'Disney - PIXAR').


rfuzzy_fuzzification(modern(film), release_year(film)) :- function([ (1970, 0), (2000, 1), (2010, 1)]).
rfuzzy_fuzzification(long_duration(film), duration_in_minutes(film)) :- function([ (120, 0), (180, 1), (600, 1)]) .

% similarity over genres
% rfuzzy_similarity_between(film, genre(drama), genre(drama), 1).
rfuzzy_similarity_between(film, genre(drama), genre(romance), 0.6).
rfuzzy_similarity_between(film, genre(drama), genre(western), 0.1).
rfuzzy_similarity_between(film, genre(drama), genre(adventure), 0.1).
rfuzzy_similarity_between(film, genre(drama), genre(comedy), 0).
rfuzzy_similarity_between(film, genre(romance), genre(drama), 0.6).
%rfuzzy_similarity_between(film, genre(romance), genre(romance), 1).
rfuzzy_similarity_between(film, genre(romance), genre(western), 0.4).
rfuzzy_similarity_between(film, genre(romance), genre(adventure), 0.3).
rfuzzy_similarity_between(film, genre(romance), genre(comedy), 0.3).
rfuzzy_similarity_between(film, genre(western), genre(drama), 0.1).
rfuzzy_similarity_between(film, genre(western), genre(romance), 0.4).
%rfuzzy_similarity_between(film, genre(western), genre(western), 1).
rfuzzy_similarity_between(film, genre(western), genre(adventure), 0.8).
rfuzzy_similarity_between(film, genre(western), genre(comedy), 0.1).
rfuzzy_similarity_between(film, genre(adventure), genre(drama), 0.1).
rfuzzy_similarity_between(film, genre(adventure), genre(romance), 0.3).
rfuzzy_similarity_between(film, genre(adventure), genre(western), 0.8).
%rfuzzy_similarity_between(film, genre(adventure), genre(adventure), 1).
rfuzzy_similarity_between(film, genre(adventure), genre(comedy), 0.2).
rfuzzy_similarity_between(film, genre(comedy), genre(drama), 0).
rfuzzy_similarity_between(film, genre(comedy), genre(romance), 0.3).
rfuzzy_similarity_between(film, genre(comedy), genre(western), 0.1).
rfuzzy_similarity_between(film, genre(comedy), genre(adventure), 0.2).
%rfuzzy_similarity_between(film, genre(comedy), genre(comedy), 1.

% similarity over languages
% rfuzzy_similarity_between(film, original_language(english), original_language(english), 1).
rfuzzy_similarity_between(film, original_language(english), original_language(spanish), 0.2).
rfuzzy_similarity_between(film, original_language(english), original_language(italian), 0.2).
rfuzzy_similarity_between(film, original_language(spanish), original_language(english), 0.2).
% rfuzzy_similarity_between(film, original_language(spanish), original_language(spanish), 1).
rfuzzy_similarity_between(film, original_language(spanish), original_language(italian), 0.7).
rfuzzy_similarity_between(film, original_language(italian), original_language(english), 0.2).
rfuzzy_similarity_between(film, original_language(italian), original_language(spanish), 0.7).
% rfuzzy_similarity_between(film, original_language(italian), original_language(italian), 1).

% funny is an example of a discrete attribute
%rfuzzy_type_for(funny/1, [film]).
%funny(genre('drama')) value 0 .
%funny('romance') value 0.4 .
%funny('western') value 0.2 .
%funny('adventure') value 0.2 .
%funny('comedy') value 1 .




