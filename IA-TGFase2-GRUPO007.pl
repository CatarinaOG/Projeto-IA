:- set_prolog_flag( singleton,off ).


%---------------------------------------------------------------------------------
%----------------------------------- ENTIDADES -----------------------------------
%---------------------------------------------------------------------------------

%veiculo(TipoDeVeiculo,ClassificaçaoEcologica)

veiculo(bicicleta,1).
veiculo(mota,2).
veiculo(carro,3).


%velocidadeMedia(TipoDeVeiculo,VelocidadeMedia)
velocidadeMedia(bicicleta,10).
velocidadeMedia(mota,35).
velocidadeMedia(carro,25).

%veiculoDecrescimo(TipoDeVeiculo,Decrescimo)

veiculoDecrescimo(bicicleta,0.7).
veiculoDecrescimo(mota,0.5).
veiculoDecrescimo(carro,0.1).


%estado Inicial e Final

inicial(armazem).


%Coordenadas(rua,latitude,longitude)
coord(armazem                 , 41.520857 , -8.444599).
coord(avenida_de_sao_lourenco , 41.510072 , -8.452233).
coord(rua_monte_carrinhos     , 41.518838 , -8.449136).
coord(avenida_trezeste        , 41.503440 , -8.456939).
coord(rua_do_outeiro          , 41.514425 , -8.452256).
coord(avenida_do_covedelo     , 41.514742 , -8.455489).


%Calcular as distancias previstas
grausParaRads(G,R) :- R is G / (180/pi).

distancia(A,B,D) :- coord(A,LatA,LongA),
                    coord(B,LatB,LongB),
                    grausParaRads(LatA , RLatA) ,
                    grausParaRads(LongA, RLongA),
                    grausParaRads(LatB , RLatB) ,
                    grausParaRads(LongB, RLongB),
                    D is (1.609344 * (3963.0 * acos((sin(RLatA) * sin(RLatB)) + cos(RLatA) * cos(RLatB) * cos(RLongB - RLongA)))).
                    

%grafo_______(grafo([vertices],[arestas]))
%aresta(origem,destino,distancia)

grafoCeleiros(grafo([armazem,avenida_de_sao_lourenco, rua_monte_carrinhos ,avenida_trezeste ,rua_do_outeiro],
                     [aresta(armazem                 , avenida_do_covedelo     ,1.4),
                      aresta(avenida_trezeste        , avenida_de_sao_lourenco ,0.9),
                      aresta(avenida_trezeste        , rua_do_outeiro          ,1.7),
                      aresta(avenida_trezeste        , avenida_do_covedelo     ,1.7),
                      aresta(rua_do_outeiro          , avenida_do_covedelo     ,0.3),
                      aresta(rua_do_outeiro          , rua_monte_carrinhos     ,0.9),
                      aresta(avenida_de_sao_lourenco , rua_monte_carrinhos     ,1.2),
                      aresta(avenida_de_sao_lourenco , rua_do_outeiro          ,0.7),
                      aresta(rua_monte_carrinhos     , avenida_do_covedelo     ,0.8)])).




adjacente(X,Y,D, grafo(_,Es)) :- member(aresta(X,Y,D),Es).
adjacente(X,Y,D, grafo(_,Es)) :- member(aresta(Y,X,D),Es).


%encomenda(Id,Peso,Volume,Classificacao,Rua,Freguesia,Veiculo,Preço,idCliente,idEstafeta).

encomenda(1 , 5 , 8 , 4, avenida_de_sao_lourenco      , "Celeirós"        , bicicleta , 10 , 123   , 987).
encomenda(2 , 14, 10, 5, rua_monte_carrinhos          , "Celeirós"        , mota      , 9  , 456   , 654).
encomenda(3 , 90, 5 , 3, rua_dos_pomares              , "Martim"          , carro     , 14 , 789   , 321).
encomenda(4 , 6 , 6 , 4, avenida_trezeste             , "Celeirós"        , carro     , 15 , 12345 , 987).
encomenda(5 , 4 , 13, 4, rua_monte_carrinhos          , "Martim"          , mota      , 15 , 123   , 654).
encomenda(6 , 12, 9 , 3, rua_paulo_fernandes          , "Santa Eugénia"   , carro     , 20 , 456   , 321).
encomenda(7 , 3 , 4 , 2, praca_arsenalistas           , "Cabreiros"       , bicicleta , 50 , 12345 , 321).
encomenda(12, 16, 10, 1, avenida_do_covedelo          , "Celeirós"        , mota      , 69 , 69420 , 720).
encomenda(13, 7 , 6 , 3, rua_dos_passaros             , "Prado"           , bicicleta , 120, 290   , 720).


%encomendaNE(Id,Peso,Volume,Classificacao,Rua,Freguesia,Preço,idCliente,idEstafeta).

encomendaNE(8 , 10, 12, 4, rua_do_souto                 , "Sao Vitor"       , 23 , 12345 , 654).
encomendaNE(9 , 9 , 3 , 2, rua_do_outeiro               , "Celeirós"        , 2  , 123   , 654).
encomendaNE(10, 25, 13, 5, rua_do_carmo                 , "Real"            , 50 , 290   , 321).
encomendaNE(11, 12, 2 , 4, rua_santa_ingracia           , "Merelim São Paio", 25 , 105   , 649).
encomendaNE(14, 23, 26, 4, rua_das_palhas               , "São João"        , 250, 789   , 649).


%dataE(IdEnc ,DataEnc, Prazo, DataEnt).

dataE(1,  20/11/21, 21/11/21, 21/11/21 ).
dataE(2,  20/11/21, 23/11/21, 22/11/21 ).
dataE(3,  25/12/21, 31/12/21, 01/01/22 ).
dataE(4,  31/12/21, 21/11/21, 21/11/21 ).
dataE(5,  27/03/21, 28/03/21, 28/03/21 ).
dataE(6,  09/12/19, 10/12/19, 11/12/19 ).
dataE(7,  03/07/18, 15/07/18, 18/07/18 ).
dataE(12, 02/10/20, 14/11/20, 12/11/20 ).
dataE(13, 08/09/20, 12/11/20, 14/11/20 ).

%dataNE(IdEnc, DataEnc, Prazo).

dataNE(8,  20/11/21, 21/11/21).
dataNE(9,  20/11/21, 23/11/21).
dataNE(10, 28/11/21, 12/01/22).
dataNE(11, 23/12/21, 23/01/22).
dataNE(14, 01/10/21, 04/10/22).


%cliente(Id,IdsEncomendas).
%IdsEncomendas -> lista dos ids

cliente(123  , [1,5,9]).
cliente(12345, [4,7,8]).
cliente(456  , [2,6]).
cliente(789  , [3,14]).
cliente(105  , [11]).
cliente(69420, [12]).
cliente(290  , [10,13]).


%estafeta(Id, NumEnc,Encomendas,NumPen).
estafeta(987, 2, [1,4]     , 0).
estafeta(654, 4, [2,5,8,9] , 0).
estafeta(321, 4, [3,6,7,10], 3).
estafeta(649, 2, [11,14]   , 0).
estafeta(720, 2, [12,13]   , 1).



%---------------------------------------------- Depth-First (Distancia) -----------------------------------------------



dfDistancia(Grafo,Dest,Solucao,C):-
	inicial(NodoInicial),
    statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
	dfDistancia(Grafo,Dest,NodoInicial,[NodoInicial],Solucao,C),
    statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
    write('Execution took '), write(ExecutionTime), write(' ms.'), nl.

dfDistancia(Grafo, Dest, Dest, Historico, [], 0):- !.

dfDistancia(Grafo, Dest, NodoAtual, Historico, [Novo|Solucao], C):-
	adjacente(NodoAtual, Novo, C1, Grafo),
	\+ member(Novo, Historico),
	dfDistancia(Grafo, Dest, Novo, [NodoAtual|Historico], Solucao, C2),
    C is C1 + C2.


%---------------------------------------------- Depth-First (Tempo) -----------------------------------------------

%falta funcao para determinar veiculo

dfTempo(Grafo, Id, Dest, Veiculo, Solucao, CTempo, CDistancia):-
	inicial(NodoInicial),
	dfDistancia(Grafo, Dest,NodoInicial,[NodoInicial],Solucao,CDistancia),
    encomendaNE(Id,Peso,_,_,_,_,_,_,_),
    custoFinalDec(Veiculo, Peso, CDistancia, CTempo).
    
custoFinalDec(bicicleta, Peso, CustoDist, CustoTempo) :-
    velocidadeMedia(bicicleta,X),
    CustoTempo is CustoDist/(X-(0.7*Peso)).
    
custoFinalDec(mota, Peso, CustoDist, CustoTempo) :-
    velocidadeMedia(mota,X),
    CustoTempo is CustoDist/(X-(0.5*Peso)).
    
custoFinalDec(carro, Peso, CustoDist, CustoTempo) :-
    velocidadeMedia(carro,X),
    CustoTempo is CustoDist/(X-(0.1*Peso)).


%---------------------------------------------- Breath-First (Distancia) -----------------------------------------------


bfDistancia(Grafo,Dest,Solucao/C):-
        inicial(EstadoInicial),
        bfDistancia2(Grafo,Dest,[[EstadoInicial]/0],Solucao/C).

bfDistancia2(Grafo,Dest,[[Dest|T]/C|_],Solucao/C):-
        reverse([Dest|T],Solucao).

bfDistancia2(Grafo,Dest,[EstadosA|Outros],Solucao):-
        EstadosA=[Act|Cam]/CA,
        findall([Novo|[Act|Cam]]/CN,
                (Dest\==Act,adjacente(Act, Novo, C1, Grafo),\+member(Novo,[Act|Cam]), CN is CA + C1),
                Novos),
        append(Outros,Novos,Todos),
        bfDistancia2(Grafo,Dest,Todos,Solucao).


%---------------------------------------------- Breath-First (Tempo) -----------------------------------------------

%falta funcao para determinar veiculo

bfTempo(Grafo, Id, Dest, Veiculo, Solucao, CTempo, CDistancia):-
        bfDistancia(Grafo,Dest,Solucao/CDistancia),
        encomendaNE(Id,Peso,_,_,_,_,_,_,_),
        custoFinalDec(Veiculo, Peso, CDistancia, CTempo).
        


%----------------------------------------------- Greedy (Distancia) ---------------------------------------------------

greedyDistancia(NodoFinal, Grafo, Caminho/Custo):-  inicial(NodoInicial),
                                                    distancia(NodoInicial, NodoFinal, Estima),
                                                    greedy([[NodoInicial]/0/Estima], NodoFinal, Grafo, ICaminho/Custo/SolEst),
                                                    inverso(ICaminho, Caminho).


greedy(Caminhos, NodoFinal, Grafo, [NodoFinal|Cam]/Custo/Est) :- melhorEst(Caminhos, [NodoFinal|Cam]/Custo/Est),!.


greedy(Caminhos, NodoFinal, Grafo, Caminho) :- melhorEst(Caminhos, MelhorCaminho),
                                               seleciona_caminho(MelhorCaminho, Caminhos, OutrosCaminhos),
                                               expande_gulosa(MelhorCaminho, NodoFinal, Grafo, ExpandeCaminhos),
                                               append(OutrosCaminhos, ExpandeCaminhos, NovosCaminhos),
                                               greedy(NovosCaminhos, NodoFinal, Grafo, Caminho).



melhorEst([E/Custo/Est], E/Custo/Est).
melhorEst([E1/Custo1/Est1, E2/Custo2/Est2 | Outros],BestE/Custo/BestEst):-
    Est1 >= Est2,
    melhorEst([E2/Custo2/Est2 | Outros],BestE/Custo/BestEst).

melhorEst([E1/Custo1/Est1, E2/Custo2/Est2 | Outros],BestE/Custo/BestEst):-
    Est1 =< Est2,
    melhorEst([E1/Custo1/Est1 | Outros], BestE/Custo/BestEst).


obtem_melhor_g([Caminho], Caminho) :- !.
obtem_melhor_g([Caminho1/Custo1/Est1, Caminho2/Custo2/Est2 | Caminhos], MelhorCaminho) :- 
										Est1 =< Est2, 
										!, 
										obtem_melhor_g([Caminho1/Custo1/Est1 | Caminhos], MelhorCaminho).
obtem_melhor_g([_|Caminhos],MelhorCaminho) :- obtem_melhor_g(Caminhos,MelhorCaminho).

seleciona_caminho(E, [E|XS], XS).
seleciona_caminho(E, [X|XS], [Y|YS]) :- seleciona_caminho(E,XS,YS).

expande_gulosa([NodoAtual|Cam]/Custo/Estimativa, NodoFinal, Grafo, ExpandeCaminhos) :- findall([NovoNodo, NodoAtual | Cam]/NovoCusto/NovaEstimativa,
                                                                                        (adjacente(NodoAtual, NovoNodo, CustoAresta, Grafo), 
                                                                                         \+member(NovoNodo,Cam), 
                                                                                         NovoCusto is CustoAresta + Custo, 
                                                                                         distancia(NovoNodo, NodoFinal, NovaEstimativa)), 
                                                                                        ExpandeCaminhos).


inverso(Xs, Ys):-
    inverso(Xs, [], Ys).

inverso([], Xs, Xs).
inverso([X|Xs],Ys, Zs):-
       inverso(Xs, [X|Ys], Zs).



%----------------------------------------------- Greedy (Tempo) ---------------------------------------------------

%falta predicado para descobrir veiculo 

greedyTempo(Grafo, Id, NodoFinal, Veiculo, Caminho, CTempo, CustoDistancia):- 
        greedyDistancia(NodoFinal, Grafo, Caminho/CustoDistancia),
        encomendaNE(Id,Peso,_,_,_,_,_,_,_),
        custoFinalDec(Veiculo, Peso, CustoDistancia, CTempo).


                                                                           
%------------------------------------- Depth First com profundidade limitada --------------------------------------

depthFirstLimited(Grafo, Id, NodoFinal, Veiculo, [NodoInicial | Caminho], CustoDistancia, Prof) :-
        inicial(NodoInicial),
        depthFirstLimitedAux(Grafo, NodoInicial, NodoFinal, [NodoInicial], Caminho, CustoDistancia, Prof).


depthFirstLimitedAux(Grafo, NodoAtual, NodoFinal, Historico, [], 0, 0) :- !.
depthFirstLimitedAux(Grafo, NodoFinal, NodoFinal, Historico, [], 0, Prof) :- !.

depthFirstLimitedAux(Grafo, NodoAtual, NodoFinal, Historico, [ProxNodo|Caminho], CustoDistancia, Prof) :- 
        adjacente(NodoAtual, ProxNodo, ProxCusto, Grafo),
        \+ member(ProxNodo, Historico),
        NProf is Prof - 1,
        depthFirstLimitedAux(Grafo, ProxNodo, NodoFinal, [ProxNodo | Historico], Caminho, C, NProf),
        CustoDistancia is C + ProxCusto.




