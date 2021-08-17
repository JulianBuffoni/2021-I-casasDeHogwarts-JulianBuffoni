mago(Mago):- sangre(Mago, _).

caracteristica(harry, coraje).
caracteristica(harry, amistad).
caracteristica(harry, orgullo).
caracteristica(harry, inteligencia).
caracteristica(ron, amistad).
caracteristica(ron, diversion).
caracteristica(ron, coraje).
caracteristica(hermione, inteligencia).
caracteristica(hermione, coraje).
caracteristica(hermione, responsabilidad).
caracteristica(hermione, amistad).
caracteristica(hermione, orgullo).
caracteristica(hannahAbbott, amistad).
caracteristica(hannahAbbott, diversion).
caracteristica(draco, inteligencia).
caracteristica(draco, orgullo).
caracteristica(lunaLovegood, inteligencia).
caracteristica(lunaLovegood, responsabilidad).
caracteristica(lunaLovegood, amistad).
caracteristica(lunaLovegood, coraje).

sangre(harry, mestiza).
sangre(ron, pura).
sangre(hermione, impura).
sangre(hannahAbbott, mestiza).
sangre(draco, pura).
sangre(lunaLovegood, mestiza).

odia(harry, slytherin).
odia(draco, hufflepuff).

casa(gryffindor).
casa(hufflepuff).
casa(ravenclaw).
casa(slytherin).

caracteriza(gryffindor, amistad).
caracteriza(gryffindor, coraje).
caracteriza(slytherin, orgullo).
caracteriza(slytherin, inteligencia).
caracteriza(ravenclaw, inteligencia).
caracteriza(ravenclaw, responsabilidad).
caracteriza(hufflepuff, amistad).
caracteriza(hufflepuff, diversion).

permiteEntrar(slytherin, Mago):-
    sangre(Mago, pura).
permiteEntrar(Casa, Mago):-
    casa(Casa),
    mago(Mago),
    Casa \= slytherin.

tieneCaracter(Mago, Casa):-
    mago(Mago),
    casa(Casa),
    forall(caracteriza(Casa, Caracteristica), caracteristica(Mago, Caracteristica)).

casaPosible(Mago, Casa):-
    tieneCaracter(Mago, Casa),
    permiteEntrar(Casa, Mago),
    not(odia(Mago, Casa)).

cadenaDeAmistades(ListaMagos):-
    amistosos(ListaMagos),
    cadenaCasas(ListaMagos).

amistosos(ListaMagos):-
    forall(member(Mago, ListaMagos), caracteristica(Mago, amistad)).

cadenaCasas([_]).
cadenaCasas([Mago, OtroMago | Cola ]):-
    entranMismaCasa(Mago, OtroMago),
    cadenaCasas([ OtroMago | Cola ]).

entranMismaCasa(Mago, OtroMago):-
    casaPosible(Mago, Casa),
    casaPosible(OtroMago, Casa).

lugarProhibido(bosque,50).
lugarProhibido(seccionRestringida,10).
lugarProhibido(tercerPiso,75).

alumnoFavorito(flitwick, hermione).
alumnoFavorito(snape, draco).
alumnoOdiado(snape, harry).

hizo(ron, buenaAccion(jugarAlAjedrez, 50)).
hizo(ron, irA(bosque)).
hizo(harry, fueraDeCama).
hizo(hermione, irA(tercerPiso)).
hizo(hermione, responder("Donde se encuentra un Bezoar", 15, snape)).
hizo(hermione, responder("Wingardium Leviosa", 25, flitwick)).
hizo(draco, irA(mazmorras)).
