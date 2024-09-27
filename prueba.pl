mujer(maria).
mujer(sofia).

varon(jaime).
varon(juan).

sevende(vestido).
sevende(sombrero).
sevende(zapatos).
sevende(traje).

costo(sombrero,200).
costo(zapatos,200).
costo(vestido,100).
costo(traje,300).

stockSombrero([mujer,varon],[2,1]). %2 sobreros de mujer y 1 de varon.
stockVestido([xs, m, l, xl],[2,30,10,5]).
%tallas de vestidos con las respectivas cantidades en la tienda
stockZapato(varon, [40, 41, 42],[20,0,10]).
%tallas de zapatos con las respectivas cantidades en la tienda
stockZapato(mujer, [36, 37, 38],[5,2,14]).
%tallas de zapatos con las respectivas cantidades en la tienda

gusta(jaime,zapatos).
gusta(maria,vestido).
gusta(maria,sombrero).
gusta(jaime,vestido).

% (a) A una mujer le interesa un artículo que se vende en la tienda,
% cuando el costo es menor a 200bs.

interesa(X,Y):-mujer(X),sevende(Y),costo(Y,C),C<200,write(X), write(" le interesa "), write(Y),nl.

% (b) Imprimir el stock de los vestidos de la forma: talla xs hay 22,
% talla m hay 30, talla L hay 10, talla xl hay 5.

imprime([],[]).
imprime(L1,L2):-L1=[C1|R1],L2=[C2|R2], write("talla "),write(C1),write(" hay "), write(C2),nl,imprime(R1,R2).

imprime_Stock:-stockVestido(L1,L2), imprime(L1,L2).

% (c) Calcular la cantidad de zapatos total (sumar la cantidad de zapatos
% de varón y mujer).
%
sum_elem([],0).
sum_elem([X|Xs], S):- sum_elem(Xs, S2), S is S2 + X.

suma_total:-stockZapato(varon,_,L1),sum_elem(L1,R1),stockZapato(mujer,_,L2),sum_elem(L2,R2), Total is R1+R2, write("cantidad de zapatos "), write(Total).

% (d) Si un varón compra un vestido talla xs, se descuenta el 20% del
% costo del vestido. Imprimir el precio y modificar la existencia en
% stock.
%
%
modificar([_|Cola],R,L1):-L1=[R|Cola].

impri_lista([]).
impri_lista(L):- L=[C|R], write(C), write(", "), impri_lista(R).

compra_varon(X,vestido,xs):-varon(X),stockVestido([C1|_],[C2|R2]),C1==xs, 0<C2, costo(vestido,Co), Descuento is Co-(Co*0.2), write(Descuento), write(" xs en stock "), R is C2-1, write(R), nl, modificar([C2|R2],R,L), impri_lista(L).

%Los clientes que compren articulos de la tienda cuyo costo en total es mayor a 200bs, tengan el descuento del 15%
compra(X,Y):- sevende(Y), costo(Y,C), C > 200, Descuento is C - (C * 0.15), write("Monto a pagar: "), write(Descuento).

impListaGusta:- gusta(X,Y), write(X), write(" gusta de "), write(Y),nl, fail.

% Predicado para encontrar la posición de un valor en una lista
posicion_valor(Valor, Lista, Posicion) :-
    posicion_valor(Valor, Lista, 1, Posicion).

% Caso base: se ha encontrado el valor
posicion_valor(Valor, [Valor|_], PosicionActual, PosicionActual).

% Caso recursivo: continuar buscando
posicion_valor(Valor, [_|Resto], PosicionActual, Posicion) :-
    NuevaPosicion is PosicionActual + 1,
    posicion_valor(Valor, Resto, NuevaPosicion, Posicion).

% Predicado para reemplazar un elemento en una lista en la posición dada
reemplazar_elemento(Indice, Lista, NuevoElemento, ListaModificada) :-
    nth1(Indice, Lista, _, Resto),  % Obtener el elemento en la posición y el resto de la lista
    append([NuevoElemento], Resto, ListaModificada).

% Predicado para acceder a un elemento en una posición específica de una lista
acceder_posicion(N, Lista, Elemento) :-
    nth1(N, Lista, Elemento).

compraZapato(Genero,Talla):- stockZapato(Genero, LTalla, LStock), posicion_valor(Talla, LTalla, X), acceder_posicion(X, LStock, Aux), Aux2 is Aux - 1,reemplazar_elemento(X,LStock,Aux2,Resp), impri_lista(Resp).