:- use_package(assertions).

:- entry oracion(A, B) : ( list(A, atm), var(B) ).

:- push_prolog_flag(single_var_warnings,off).

%%  Determinantes. 

detE(detvacio, N, G, S/S).
detE(X, N, G, S):- 
        det(X, N, G, S).
det(X, N, G, [X|S]/S):- 
        edet(E, N, G), 
        pertenece(X, E).

edet([el, lo, un, uno, eso, ese, este, esto, aquello], sing, masc).
edet([la, una, esa, esta, aquella], sing, fem).
edet([los, unos, esos, estos, aquellos], plu, masc).
edet([las, unas, esas, estas, aquellas], plu, fem).
edet([mi, tu, su, nuestro, nuestra, vuestro, vuestra], sing, X).
edet([mis, tus, sus, nuestros, nuestras, vuestros, vuestras], plu, X).

 %%  Pronombres. No tenemos en cuenta la concordancia se - lo ( no
 %% vale "le lo dí", hay que decir "se lo dí").  

pronE(S/S1):- 
        pronciE(S/S0), 
        proncdE(S0/S1).

pronciE([X|S]/S):- 
        epronci(E), 
        pertenece(X, E).
pronciE(S/S).

proncdE([X|S]/S):- 
        eproncd(E), 
        pertenece(X, E).
proncdE(S/S).

eproncd([lo, la, los, las]).
epronci([me, te, se, le, nos, os, les]).

%%  Preposiciones. 
prepE(prepvacia, S/S).
prepE(P,S):- prep(P, S).

prep(X, [X|S]/S):- 
        eprep(E), 
        pertenece(X, E).
eprep([a, ante, bajo, con, contra, de, desde, en, entre, hacia, hasta,
        para, por, segun, sin, so, sobre, tras]).


 %% Pronombres relativos, para subordinadas adjetivas. Las sustantivas
 %% las construímos siempre con "que". A veces también sirven de
 %% nexo adverbios relativos o el relativo posesivo "cuyo".  

pronrel(X):- epronrel(E), pertenece(X, E).
epronrel([que, cual, cuales, quien, quienes, donde, como, cuando,
        cuyo, cuya, cuyos, cuyas]).

%% Conjunciones adverbiales. 

%% tiempo. 
conjadv( tiempo, [cuando|S]/S).
conjadv( tiempo, [tan,pronto,como|S]/S).
conjadv( tiempo, [no, bien|S]/S).
conjadv( tiempo, [asi|S]/S).		%% puede ser de tiempo o de modo 
conjadv( tiempo, [apenas|S]/S).
conjadv( tiempo, [en,cuanto|S]/S).
conjadv( tiempo, [despues, de|S]/S).
conjadv( tiempo, [antes, de|S]/S).

%% modo 
conjadv( modo, [como|S]/S).
conjadv( modo, [segun|S]/S).
conjadv( modo, [segun, que|S]/S).

%% causa 
conjadv( causa, [porque|S]/S).
conjadv( causa, [pues|S]/S).
conjadv( causa, [puesto, que|S]/S).

%% condicion 
conjadv( condicion, [si|S]/S).

%% concesion 
conjadv( concesion, [aunque|S]/S).
conjadv( concesion, [a, pesar, de|S]/S).
conjadv( concesion, [a, pesar, de, que|S]/S).

%% finales 
conjadv( final, [a, que|S]/S).
conjadv( final, [para, que|S]/S).
conjadv( final, [con, el , objeto, de, que|S]/S).

%% Un intensificador (adverbio que puede no estar presente). 
intE(intvacio, S/S).
intE(X, S):- adverbio(X, S).

%%Cositas
pertenece(X, [X|S]):-!.
pertenece(X, [Y|S]):- pertenece(X, S).

prondef(n1s, sing, X, 1).
prondef(n2s, sing, X, 2).
prondef(n3s, sing, X, 3).
prondef(n1p ,plu , X,1).
prondef(n2p ,plu , X,2).
prondef(n3p , plu, X, 3).

%% Nombres. 
nombre(X, N, G, 3, [X|S]/S):- 
        esust(E, N, G), 
        pertenece(X, E).
nombre(X, N, G, P, [X|S]/S):- 
        nomp(X, N, P).

 %% nombres personales. 
nomp(yo, sing, 1).
nomp(tu, sing, 2).
nomp(usted, sing, 3).			 %% Para la concordancia con el verbo. 
nomp(nosotros, plu, 1).
nomp(vosotros,plu, 2).
nomp(ustedes,plu, 3).

 %% Sustantivos y pronombres personales. 

esust([el, hombre, lobo, cerdo, corte, muro, anyo, mes, dia, edificio,
        gato, perro, rapidez, consuelo, martillo], sing, masc).
esust([ella, mujer, loba, cerda, muralla, gata, perra, iguana,
        aranya, escolopendra, tenia, pocima, pulga, noche], sing, fem).
esust([ellos, hombres, lobos, cerdos, cortes, muros, anyos, meses,
        dias, edificios, gatos, perros, consuelos, martillos], plu, masc).
esust([ellas, mujeres, lobas, cerdas, murallas, gatas, perras,
        iguanas, aranyas, escolopendras, tenias, pocimas,
        pulgas, noches], plu, fem).

%% Adjetivos. 

adjetivo(X, N, G, [X|S]/S):- 
        eadj(E, N, G), 
        pertenece(X, E).

eadj([blanco, pequenyo, alto, tonto, largo, asombroso, estupido], sing, masc).
eadj([blanca, pequenya, alta, tonta, larga, asombrosa, estupida], sing, fem).
eadj([blancos, pequenyos, altos, tontos, largos, asombrosos,
        estupidos], plu, masc).
eadj([blancas, pequenyas, altas, tontas, largas, asombrosas, 
        estupidas], plu, fem).
eadj([imponente, gran, grande, imbecil, inmovil, importante], sing, X).
eadj([imponentes, grandes, imbeciles, inmoviles, importantes], plu, X).

%% Adverbios. 
adverbio(X, [X|S]/S):- eadv(E), pertenece(X, E).

eadv([muy, ahora, hoy, ayer, manyana, pronto, tarde, aqui, alli, alla,
        asi, solamente, unicamente, habilmente, poco, mucho,
        bastante]). 

%% Verbos no copulativos. 

verbo(X, N, P, [X|S]/S):- 
        everb(E, N, P), 
        pertenece(X, E).

everb([quiero, tengo, se, digo, pienso, ando, corro, como, bebo, voy,
        veo, doy, pongo, pego], sing, 1). 
everb([quieres, tienes, sabes, dices, piensas, andas, corres, comes,
        bebes, vas, ves, das, pones, pegas], sing, 2). 
everb([quiere, tiene, sabe, dice, piensa, anda, corre, come, bebe, va,
        ve, da, pone, pega], sing, 3). 
everb([queremos, tenemos, sabemos, decimos, pensamos, andamos,
        corremos, comemos, bebemos, vamos, vemos, damos, ponemos,
        pegamos], plu, 1). 
everb([quereis, teneis, sabeis, decis, pensais, andais, correis,
        comeis, bebeis, vais, veis, dais, poneis, pegais], plu, 2). 
everb([quieren, tienen, saben, dicen, piensan, andan, corren, comen,
        beben, van, ven, dan, ponen, pegan], plu, 3). 

%%  Verbos copulativos. 

copula(X, N, P, [X|S]/S):- 
        ecop(E, N, P), 
        pertenece(X, E).

ecop([soy, estoy, sea, fui, estuve, parezco, pongo], sing, 1).
ecop([eres, estas, seas, fuiste, estuviste, pareces, pones], sing, 2).
ecop([es, sea, fue, estuvo, parece, pone], sing, 3).
ecop([somos, estamos, seamos, estemos, fuimos, estuvimos, parecemos,
        ponemos], plu, 1). 
ecop([sois, esteis, seais, fuisteis, estuvisteis, pareceis, poneis],
        plu, 2). 
ecop([son, sean, estan, fueron, estuvieron, parecen, ponen], plu, 3).

%% Sintagma verbal
%% SV -> (CC)^n + (Copula) + SN + (CC)^n 
%% SV -> (CC)^n + (Copula) + gAdj + (CC)^n 
%% SV -> (CC)^n + Ver + (CD) + (CI) + (CC)^n 
%% SV -> (CC)^n + CI + Ver + (CD) + (CC)^n 
%% Ver -> (Pron) + (Pron) + V 

sintagverb(SV, N, G, P, S/S1):-
	circunstanciales(C1, S/S0), 
        sintagverb1(SV, C1, N, G, P, S0/S1).

sintagverb1(nominal(Cop, CCx/CCy, At), CCx/Z, N, G, P, S/S1):-
	vercop(Cop, N, P, S/S0), 
        sintagverb2(Cop, At, Z/CCy, N, G, P, S0/S1).
sintagverb1(predicativo(V, Cd, Ci, CCx/CCy), CCx/Z, N, G, P, S/S3):-
	verbal(V, N, P, S/S0), 
        compdirE(Cd, S0/S1),
	compindE(Ci, S1/S2),
        circunstanciales(Z/CCy, S2/S3).
sintagverb1(predicativo(V, Cd, Ci, CCx/CCy), CCx/Z, N, G, P, S/S3):-
	compind(Ci, S/S0), 
        verbal(V, N, P, S0/S1),
	compdirE(Cd, S1/S2),
        circunstanciales(CC, S2/S3).

sintagverb2(Cop, At, CC, N, G, P, S/S1):-
	sintagnom(At, N, G, P, S/S0), 
        circunstanciales(CC, S0/S1).
sintagverb2(Cop, At, CC, N, G, P, S/S1):-
	gadj(At, N, G, S/S0), 
        circunstanciales(CC, S0/S1). 


verbal(Ve, N, P, S/S1):- 
        pronE(S/S0),
        verbo(Ve, N, P, S0/S1).
vercop(Cop, N, P, S/S1):- 
        pronE(S/S0), 
        copula(Cop, N, P, S0/S1).

%%  Lista de complementos circunstanciales. 

circunstanciales(C, S):- 
        circunstanciales_(C, s(s(s(0))), S).   %% Hasta 3. 

circunstanciales_(Z/Z, X, S/S).
circunstanciales_([T|V]/Z, s(X), S/S1):-
	circunstancial(T, S/S0),
        circunstanciales_(V/Z, X, S0/S1).

%%  CC -> conjadv + O | SPrep | (prep) + (int) + Adv 

circunstancial(cc(tipo(T), O), S/S1):- 
        conjadv(T, S/S0), 
        oracion1(O, S0/S1).
circunstancial(cc(SP), S):- 
        sprep(SP, S).
circunstancial(cc(Prep, Int, Adv), S/S2):-
	prepE(Prep, S/S0), 
        intE(Int, S0/S1),
        adverbio(Adv, S1/S2).

%%  CI -> a + SN 

compindE(nvacio, S/S).
compindE(C, S):- compind(C, S).
compind(SN, [a|S]/S0):- sintagnom(SN, N, G, P, S/S0).

%%  CD -> SN 

compdirE(nvacio, S/S).
compdirE(CD, S):-  sintagnom(CD, N, G, P, S).

%%  SPrep -> prep + SN 

sprepE(nvacio, S/S).
sprepE(Sp, S):- sprep(Sp, S).
sprep(sprep(Prep, SN), S/S1):-
	prep(Prep, S/S0),
        sintagnom(SN, N, G, P, S0/S1).

%% SN -> (det) + (prep) + que + O 
%% SN -> NucSN + (SPrep) + (RelAdj) 

sintagnom(propsus(O), N, G, 3, S/S2):-
	detE(D, N, G, S/S0), 
        prepE(X, S0/[que|S1]), !, 
        oracion1(O, S1/S2).
sintagnom(sn(Nuc, Comp, RelAdj), N, G, P, S/S2):-
	nucleoSN(Nuc, N, G, P, S/S0), 
        sprepE(Comp, S0/S1),
	reladjE(RelAdj, N, G, S1/S2).


%% NucSN -> det | det + gAdj | det + nuc2 | nuc2 
%% Nuc2 -> N | N + gAdj | gAdj + N 

nucleoSN(nucleo(Adj, nvacio), N, G, P, S/S1):-
	det(Det, N, G, S/S0),
        gadjE(Adj, N, G, S0/S1).
nucleoSN(nucleo(Adj, Nombre), N, G, P, S/S1):-
	detE(Det, N, G, S/S0),
        nucleoSN2(Adj, Nombre, N, G, P, S0/S1).

nucleoSN2(Adj, Nombre, N, G, P, S/S1):-
	nombre(Nombre, N, G, P, S/S0),
        gadjE(Adj, N, G, S0/S1).
nucleoSN2(Adj, Nombre, N, G, P, S/S1):-
	gadj(Adj, N, G, S/S0), 
        nombre(Nombre, N, G, P, S0/S1).

%% reladj -> (det) + (prep) + pronrel + SV 

reladjE(nvacio, N, G, S/S).
reladjE(R, N, G, S):- reladj(R, N, G, S).
reladj(propadje(SV), N, G, S/S2):-
	detE(Det, N, G, S/S0),
        prepE(X, S0/[Y|S1]),
	pronrel(Y), 
        sintagverb(SV, N, G, 3, S1/S2).

%% Adj -> (int) + adj | reladj

gadjE(nvacio, N, G, S/S).
gadjE(A, N, G, S):- gadj(A, N, G, S).
gadj(A, N, G, S):- reladj(A, N, G, S).
gadj(adjetivo(Adv, Adj), N, G, S/S1):-
	intE(Adv, S/S0),
        adjetivo(Adj, N, G, S0/S1).

%% O -> SN + SV | SV + (SN) 

oracion(Frase, Estructura):- oracion1(Estructura, Frase/[]).
oracion1(oracion(Suj, Pred), S/S1):-
	sintagnom(Suj, N, G, P, S/S0),
        sintagverb(Pred, N, G, P, S0/S1).
oracion1(oracion(Suj, Pred), S/S1):-
	sintagverb(Pred, N, G, P, S/S0), 
        oracion2(Suj, N, G, P, S0/S1).
oracion2(Suj, N, G, P, S/S0):- sintagnom(Suj, N, G, P, S/S0).
oracion2(Suj, N, G, P, S/S):- prondef(Suj, N, G, P).

:- pop_prolog_flag(single_var_warnings).
