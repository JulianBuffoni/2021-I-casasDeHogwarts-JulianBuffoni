mago(harry, mestiza, [coraje, amistad, orgullo, inteligencia]).
mago(ron, pura, [amistad, diversion, coraje]).
mago(hermione, impura, [inteligencia, coraje, responsabilidad, amistad, orgullo]).
mago(hannahAbbott, mestiza, [amistad, diversion]).
mago(draco, pura, [inteligencia, orgullo]).
mago(lunaLovegood, mestiza, [inteligencia, responsabilidad, amistad, coraje]).

odia(harry,slytherin).
odia(draco,hufflepuff).

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
    mago(Mago, pura, _).
permiteEntrar(Casa, Mago):-
    casa(Casa),
    Casa \= slytherin,
    mago(Mago,_,_).

tieneCaracter(Mago, Casa):-
    mago(Mago, _, Caracteristicas),
    casa(Casa),
    forall(caracteriza(Casa, Caracter), member(Caracter, Caracteristicas)).

casaPosible(Mago, Casa):-
    mago(Mago, _, _),
    casa(Casa),
    not(odia(Mago, Casa)),
    tieneCaracter(Mago, Casa),
    permiteEntrar(Casa, Mago).

cadenaDeAmistades([_]).
cadenaDeAmistades(ListaMagos):-
    head(ListaMagos, Mago),
    segundoElemento(ListaMagos, OtroMago),
    tieneCaracteristica(Mago, amistad),
    casaPosible(Mago, Casa),
    casaPosible(OtroMago, Casa),
    cola(ListaMagos, ColaMagos),
    cadenaDeAmistades(ColaMagos).

head(Lista, PrimerElemento):-
    Lista = [PrimerElemento|_].

cola(Lista, Cola):-
    Lista = [_|Cola].

segundoElemento(ListaMagos, Mago):-
    cola(ListaMagos, ColaMagos),
    head(ColaMagos, Mago).

tieneCaracteristica(Mago, amistad):-
    mago(Mago,_,Caracteristicas),
    member(amistad, Caracteristicas).

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

/*casaGanadora(Casa):-
    casa(Casa),
    forall(otraCasa(Casa, OtraCasa), masPuntos(Casa, OtraCasa)).*/

otraCasa(Casa, OtraCasa):-
    casa(Casa),
    casa(OtraCasa),
    Casa \= OtraCasa.

/*masPuntos(Casa, OtraCasa):-
    puntosDeCasa(Casa, PuntosMayor),
    puntosDeCasa(OtraCasa, PuntosMenor),
    PuntosMayor > PuntosMenor.*/