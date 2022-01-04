:- set_prolog_flag( singleton,off ).
:- discontiguous seleciona_caminho/3.
:- discontiguous expande_gulosa/4.
:- discontiguous get_date_time_value/2.

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


%estado Inicial

inicial(armazem).


%Coordenadas(rua,latitude,longitude)
%coordenadas Celeiros

coord(armazem                 , 41.520857 , -8.444599).
coord(avenida_de_sao_lourenco , 41.510072 , -8.452233).
coord(rua_monte_carrinhos     , 41.518838 , -8.449136).
coord(avenida_trezeste        , 41.503440 , -8.456939).
coord(rua_do_outeiro          , 41.514425 , -8.452256).
coord(avenida_do_covedelo     , 41.514742 , -8.455489).
coord(rua_das_andrias         , 41.512664 , -8.446336).
coord(rua_sub_carreira        , 41.508394 , -8.446849).
coord(rua_das_rosas           , 41.518919 , -8.454564).
coord(rua_de_segoes           , 41.515632 , -8.447117).
coord(rua_da_sofia            , 41.512815 , -8.449589).
coord(rua_da_pontinha         , 41.509678 , -8.450551).

%coordenadas Martim
coord(rua_dos_pomares         , 41.534702 , -8.513371).
coord(rua_do_valteiro         , 41.532211 , -8.512591).
coord(rua_da_escola           , 41.535160 , -8.518069).
coord(rua_da_pousada          , 41.532508 , -8.514566).
coord(rua_de_caldelas         , 41.532922 , -8.509372).
coord(rua_da_riquinha         , 41.531036 , -8.523354).
coord(rua_da_bolsinha         , 41.533783 , -8.522726).
coord(rua_de_vila_pouca       , 41.531336 , -8.503460).
coord(rua_da_lousa            , 41.530896 , -8.515294).
coord(travessa_da_carcova     , 41.529454 , -8.519266).
coord(rua_da_venda            , 41.537027 , -8.511801).
coord(rua_da_estrada_real     , 41.536280 , -8.515309).




%Calcular as distancias previstas
grausParaRads(G,R) :- R is G / (180/pi).

distancia(A,B,D) :- coord(A,LatA,LongA),
                    coord(B,LatB,LongB),
                    grausParaRads(LatA , RLatA) ,
                    grausParaRads(LongA, RLongA),
                    grausParaRads(LatB , RLatB) ,
                    grausParaRads(LongB, RLongB),
                    D is (1.609344 * (3963.0 * acos((sin(RLatA) * sin(RLatB)) + cos(RLatA) * cos(RLatB) * cos(RLongB - RLongA)))).
                    

%grafo_______(grafo([arestas]))
%aresta(origem,destino,distancia)

grafoCeleiros(grafo([ aresta(armazem                 , avenida_do_covedelo     ,1.4),
                      aresta(armazem                 , rua_monte_carrinhos     ,0.55),
                      aresta(armazem                 , rua_de_segoes           ,1.5),
                      aresta(avenida_trezeste        , avenida_de_sao_lourenco ,0.9),
                      aresta(avenida_trezeste        , rua_do_outeiro          ,1.7),
                      aresta(avenida_trezeste        , avenida_do_covedelo     ,1.7),
                      aresta(rua_do_outeiro          , avenida_do_covedelo     ,0.3),
                      aresta(rua_do_outeiro          , rua_monte_carrinhos     ,0.9),
                      aresta(avenida_de_sao_lourenco , rua_monte_carrinhos     ,1.2),
                      aresta(avenida_de_sao_lourenco , rua_do_outeiro          ,0.7),
                      aresta(rua_monte_carrinhos     , avenida_do_covedelo     ,0.8),
                      aresta(rua_das_rosas           , rua_monte_carrinhos     ,1.1),
                      aresta(rua_de_segoes           , rua_das_rosas           ,1.7),
                      aresta(avenida_do_covedelo     , rua_das_rosas           ,0.8),
                      aresta(rua_do_outeiro          , rua_das_rosas           ,1.1),
                      aresta(rua_de_segoes           , rua_monte_carrinhos     ,0.85),
                      aresta(rua_de_segoes           , rua_da_sofia            ,0.75),
                      aresta(rua_de_segoes           , rua_das_andrias         ,0.7),
                      aresta(rua_das_andrias         , rua_da_sofia            ,0.4),
                      aresta(rua_das_andrias         , rua_sub_carreira        ,0.7),
                      aresta(rua_das_andrias         , avenida_de_sao_lourenco ,0.85),
                      aresta(rua_sub_carreira        , avenida_de_sao_lourenco ,0.9),
                      aresta(avenida_trezeste        , rua_sub_carreira        ,1.9),
                      aresta(rua_da_pontinha         , avenida_de_sao_lourenco ,0.45),
                      aresta(rua_da_pontinha         , rua_da_sofia            ,0.3),
                      aresta(rua_da_pontinha         , rua_sub_carreira        ,0.55),
                      aresta(rua_da_pontinha         , rua_das_andrias         ,0.9)])).


grafoMartim(grafo(  [ aresta(armazem                 , rua_de_caldelas         ,7.7),
                      aresta(rua_de_caldelas         , rua_do_valteiro         ,1.4),
                      aresta(rua_de_caldelas         , rua_dos_pomares         ,0.7),
                      aresta(rua_dos_pomares         , rua_da_pousada          ,0.35),
                      aresta(rua_do_valteiro         , rua_da_pousada          ,0.85),
                      aresta(rua_dos_pomares         , rua_do_valteiro         ,0.35),
                      aresta(rua_da_lousa            , rua_da_pousada          ,0.25),
                      aresta(rua_da_lousa            , travessa_da_carcova     ,1.1),
                      aresta(travessa_da_carcova     , rua_da_riquinha         ,1.1),
                      aresta(rua_da_bolsinha         , rua_da_riquinha         ,0.45),
                      aresta(travessa_da_carcova     , rua_da_bolsinha         ,1),
                      aresta(rua_da_pousada          , rua_da_bolsinha         ,1),
                      aresta(rua_da_escola           , rua_da_bolsinha         ,0.55),
                      aresta(rua_da_estrada_real     , rua_da_escola           ,0.29),
                      aresta(rua_da_escola           , rua_da_pousada          ,0.55),
                      aresta(rua_da_estrada_real     , rua_dos_pomares         ,0.35),
                      aresta(rua_dos_pomares         , rua_da_venda            ,0.45),
                      aresta(rua_da_venda            , rua_da_estrada_real     ,0.35)])).


adjacente(grafo(Es), X,Y,D) :- member(aresta(X,Y,D),Es).
adjacente(grafo(Es), X,Y,D) :- member(aresta(Y,X,D),Es).


%encomenda(Id,Peso,Volume,Classificacao,Rua,Freguesia,Veiculo,Preço,idCliente,idEstafeta).

encomenda(1 , 5 , 8 , 4, avenida_de_sao_lourenco      , "Celeiros"        , bicicleta , 10 , 123   , 987).
encomenda(2 , 14, 10, 5, rua_monte_carrinhos          , "Celeiros"        , mota      , 9  , 456   , 654).
encomenda(3 , 90, 5 , 3, rua_dos_pomares              , "Martim"          , carro     , 14 , 789   , 321).
encomenda(4 , 6 , 6 , 4, avenida_trezeste             , "Celeiros"        , carro     , 15 , 12345 , 987).
encomenda(5 , 4 , 13, 4, rua_monte_carrinhos          , "Martim"          , mota      , 15 , 123   , 654).
encomenda(6 , 12, 9 , 3, rua_paulo_fernandes          , "Santa Eugénia"   , carro     , 20 , 456   , 321).
encomenda(7 , 3 , 4 , 2, praca_arsenalistas           , "Cabreiros"       , bicicleta , 50 , 12345 , 321).
encomenda(12, 16, 10, 1, avenida_do_covedelo          , "Celeiros"        , mota      , 69 , 69420 , 720).
encomenda(13, 7 , 6 , 3, rua_dos_passaros             , "Prado"           , bicicleta , 120, 290   , 720).


%encomendaNE(Id,Peso,Volume,Rua,Freguesia,Preço,idCliente,idEstafeta).

encomendaNE(8 , 10, 12, rua_da_riquinha              , "Martim"          , 23 , 12345 , 654).
encomendaNE(9 , 9 , 3 , rua_do_outeiro               , "Celeiros"        , 2  , 123   , 654).
encomendaNE(10, 25, 13, rua_da_escola                , "Martim"          , 50 , 290   , 321).
encomendaNE(11, 12, 2 , travessa_da_carcova          , "Martim"          , 25 , 105   , 649).
encomendaNE(14, 23, 26, rua_dos_pomares              , "Martim"          , 250, 789   , 649).
encomendaNE(15, 23, 26, rua_de_segoes                , "Celeiros"        , 15 , 12345 , 654).
encomendaNE(16, 23, 26, rua_da_pontinha              , "Celeiros"        , 8  , 456   , 321).
encomendaNE(17, 23, 26, rua_do_outeiro               , "Celeiros"        , 9  , 105   , 720).
encomendaNE(18, 5 , 27, rua_sub_carreira             , "Celeiros"        , 10 , 121   , 649).
encomendaNE(19, 10, 13, rua_das_andrias              , "Celeiros"        , 9  , 141   , 720).
encomendaNE(20,  8, 7 , rua_da_bolsinha              , "Martim"          , 21 , 69420 , 987).
encomendaNE(21, 50, 30, travessa_da_carcova          , "Martim"          , 40 , 141   , 654).


%dataE(IdEnc ,DataEnc, Prazo, DataEnt).

dataE(1,  2/20/11/2021, 2/21/11/2021, 1/21/11/2021 ).
dataE(2,  3/20/11/2021, 3/23/11/2021, 2/22/11/2021 ).
dataE(3,  4/25/12/2021, 4/31/12/2021, 3/01/01/2022 ).
dataE(4,  5/31/12/2021, 5/21/11/2021, 4/21/11/2021 ).
dataE(5,  6/27/03/2021, 6/28/03/2021, 5/28/03/2021 ).
dataE(6,  7/09/12/2019, 7/10/12/2019, 6/11/12/2019 ).
dataE(7,  8/03/07/2018, 8/15/07/2018, 7/18/07/2018 ).
dataE(12, 5/02/10/2020, 5/14/11/2020, 4/12/11/2020 ).
dataE(13, 8/08/09/2020, 8/12/11/2020, 7/14/11/2020 ).

%dataNE(IdEnc, DataEnc, Prazo).

dataNE(8,  01/01/2022, 18/03/01/2022).  % ja passou
dataNE(9,  01/01/2022, 19/03/01/2022).  % tem 30 min
dataNE(10, 02/01/2022, 17/05/01/2022).  % ja passou o prazo
dataNE(11, 02/01/2022, 19/23/01/2022).  % ainda falta muito para acabar o prazo
dataNE(14, 01/01/2022, 14/04/01/2022).  % ainda falta muito para acabar o prazo
dataNE(15, 01/01/2022, 15/04/01/2022).  % ainda falta muito para acabar o prazo
dataNE(16, 02/01/2022, 17/10/01/2022).  % ainda falta muito para acabar o prazo
dataNE(17, 01/01/2022, 10/07/01/2022).  % ainda falta muito para acabar o prazo


%cliente(Id,IdsEncomendas).
%IdsEncomendas -> lista dos ids
cliente(121  , [18]).
cliente(141  , [19,21]).
cliente(123  , [1,5,9]).
cliente(12345, [4,7,8,15]).
cliente(456  , [2,6,16]).
cliente(789  , [3,14]).
cliente(105  , [11,17]).
cliente(69420, [12,20]).
cliente(290  , [10,13]).


%estafeta(Id, NumEnc,Encomendas,NumPen).
estafeta(987, 2, [1,4,20]        , 0).
estafeta(654, 4, [2,5,8,9,15,21] , 0).
estafeta(321, 4, [3,6,7,10,16]   , 3).
estafeta(649, 2, [11,14,18]      , 0).
estafeta(720, 2, [12,13,17,19]   , 1).



%---------------------------------------------- Depth-First (Distancia) -----------------------------------------------



dfDistancia(Grafo,Dest,Solucao,C):-
	inicial(NodoInicial),
    statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
	dfDistancia(Grafo,Dest,NodoInicial,[NodoInicial],Solucao,C),
    statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
    write('Execution took '), write(ExecutionTime), write(' ms.'), nl,
    write(Solucao).

dfDistancia(Grafo, Dest, Dest, Historico, [], 0):- !.

dfDistancia(Grafo, Dest, NodoAtual, Historico, [Novo|Solucao], C):-
	adjacente(Grafo, NodoAtual, Novo, C1),
	\+ member(Novo, Historico),
	dfDistancia(Grafo, Dest, Novo, [NodoAtual|Historico], Solucao, C2),
    C is C1 + C2.


%---------------------------------------------- Depth-First (Tempo) ----------------------------------------------


dfTempo(Grafo, Id, Dest, Solucao, CTempo, CDistancia, Veiculo):-
	inicial(NodoInicial),
	dfDistancia(Grafo, Dest,NodoInicial,[NodoInicial],Solucao,CDistancia),
    encomendaNE(Id,Peso,_,_,_,_,_,_),
    dataNE(Id,_,Prazo),
    escolheVeiculo(Prazo, Peso, CDistancia, CTempo, Veiculo).



%---------------------------------------------- Breath-First (Distancia) -----------------------------------------------


bfDistancia(Grafo,Dest,Solucao/C):-
        inicial(EstadoInicial),
        bfDistancia2(Grafo,Dest,[[EstadoInicial]/0],Solucao/C).

bfDistancia2(Grafo,Dest,[[Dest|T]/C|_],Solucao/C):-
        reverse([Dest|T],Solucao).

bfDistancia2(Grafo,Dest,[EstadosA|Outros],Solucao):-
        EstadosA=[Act|Cam]/CA,
        findall([Novo|[Act|Cam]]/CN,
                (Dest\==Act,adjacente(Grafo, Act, Novo, C1),\+member(Novo,[Act|Cam]), CN is CA + C1),
                Novos),
        append(Outros,Novos,Todos),
        bfDistancia2(Grafo,Dest,Todos,Solucao).


%---------------------------------------------- Breath-First (Tempo) -----------------------------------------------

%falta funcao para determinar veiculo

bfTempo(Grafo, Id, Dest, Solucao, CTempo, CDistancia, Veiculo):-
        statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
        bfDistancia(Grafo,Dest,Solucao/CDistancia),
        encomendaNE(Id,Peso,_,_,_,_,_,_),
        dataNE(Id,_,Prazo),
        escolheVeiculo(Prazo, Peso, CDistancia, CTempo, Veiculo),
        statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
        write('Execution took '), write(ExecutionTime), write(' ms.'), nl.
        

%---------------------------------------- Depth First com aprofundamento iterativo (Distancia) --------------------------------


iterativeDeepeningDistancia(Grafo, NodoFinal, Caminho, CustoDistancia, ProfSol) :-
        inicial(NodoInicial),
        statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
        iterativeDeepeningDistanciaAux(Grafo, NodoFinal, NodoInicial, Caminho, CustoDistancia, 0, ProfSol),
        statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
        write('Execution took '), write(ExecutionTime), write(' ms.'), nl.

iterativeDeepeningDistanciaAux(Grafo, NodoFinal, NodoInicial, Solucao, CustoSol, Prof, Prof) :-
        findall([NodoInicial | Caminho]/CustoDistancia, depthFirstLimitedDistancia(Grafo, NodoFinal, [NodoInicial | Caminho], CustoDistancia, Prof), Sols),
        encontraSolucao(Sols, NodoFinal, Solucao, CustoSol).

iterativeDeepeningDistanciaAux(Grafo, NodoFinal, NodoInicial, Solucao, CustoDistancia, Prof, ProfSol) :-
        NovaProf is Prof + 1,
        iterativeDeepeningDistanciaAux(Grafo, NodoFinal, NodoInicial, Solucao, CustoDistancia, NovaProf, ProfSol).




encontraSolucao([Solucao/CS | OutrasSolucoes], NodoFinal, Solucao, CS) :- member(NodoFinal, Solucao).
encontraSolucao([Sol | Solucoes], NodoFinal, Solucao, CS) :- encontraSolucao(Solucoes, NodoFinal, Solucao, CS).


%------------------------------------- Depth First com profundidade limitada (Distancia) --------------------------

depthFirstLimitedDistancia(Grafo, NodoFinal,[NodoInicial | Caminho], CustoDistancia, Prof) :-
        inicial(NodoInicial),
        depthFirstLimitedAux(Grafo, NodoInicial, NodoFinal, [NodoInicial], Caminho, CustoDistancia, Prof).


depthFirstLimitedAux(Grafo, NodoAtual, NodoFinal, Historico, [], 0, 0) :- !.
depthFirstLimitedAux(Grafo, NodoFinal, NodoFinal, Historico, [], 0, Prof) :- !.

depthFirstLimitedAux(Grafo, NodoAtual, NodoFinal, Historico, [ProxNodo|Caminho], CustoDistancia, Prof) :- 
        adjacente(Grafo, NodoAtual, ProxNodo, ProxCusto),
        \+ member(ProxNodo, Historico),
        NProf is Prof - 1,
        depthFirstLimitedAux(Grafo, ProxNodo, NodoFinal, [ProxNodo | Historico], Caminho, C, NProf),
        CustoDistancia is C + ProxCusto.



%------------------------------------- Depth First com profundidade limitada (Tempo) ------------------------------

depthFirstLimitedTempo(Grafo, Id, NodoFinal, [NodoInicial | Caminho], CDistancia, Prof, CTempo, Veiculo) :- 
        statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
        depthFirstLimitedDistancia(Grafo, NodoFinal, [NodoInicial | Caminho], CDistancia, Prof),
        encomendaNE(Id, Peso,_,_,_,_,_,_),
        dataNE(Id,_,Prazo),
        escolheVeiculo(Prazo, Peso, CDistancia, CTempo, Veiculo),
        write([NodoInicial | Caminho]),
        statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
        write('Execution took '), write(ExecutionTime), write(' ms.'), nl.





%----------------------------------------------- Greedy (Distancia) -----------------------------------------------

greedyDistancia(Grafo, NodoFinal,  Caminho/Custo):- inicial(NodoInicial),
                                                    distancia(NodoInicial, NodoFinal, Estima),
                                                    greedy(Grafo, [[NodoInicial]/0/Estima], NodoFinal, ICaminho/Custo/SolEst),
                                                    inverso(ICaminho, Caminho).


greedy(Grafo, Caminhos, NodoFinal, [NodoFinal|Cam]/Custo/Est) :- melhorEst(Caminhos, [NodoFinal|Cam]/Custo/Est),!.


greedy(Grafo, Caminhos, NodoFinal, Caminho) :- melhorEst(Caminhos, MelhorCaminho),
                                               seleciona_caminho(MelhorCaminho, Caminhos, OutrosCaminhos),
                                               expande(Grafo, MelhorCaminho, NodoFinal, ExpandeCaminhos),
                                               append(OutrosCaminhos, ExpandeCaminhos, NovosCaminhos),
                                               greedy(Grafo, NovosCaminhos, NodoFinal, Caminho).



melhorEst([E/Custo/Est], E/Custo/Est).
melhorEst([E1/Custo1/Est1, E2/Custo2/Est2 | Outros],BestE/Custo/BestEst):-
    Est1 >= Est2,
    melhorEst([E2/Custo2/Est2 | Outros],BestE/Custo/BestEst).

melhorEst([E1/Custo1/Est1, E2/Custo2/Est2 | Outros],BestE/Custo/BestEst):-
    Est1 =< Est2,
    melhorEst([E1/Custo1/Est1 | Outros], BestE/Custo/BestEst).



%----------------------------------------------- Greedy (Tempo) ---------------------------------------------------

%falta predicado para descobrir veiculo 

greedyTempo(Grafo, Id, NodoFinal, Caminho, CTempo, CDistancia, Veiculo):- 
        statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
        greedyDistancia(Grafo, NodoFinal, Caminho/CDistancia),
        encomendaNE(Id,Peso,_,_,_,_,_,_),
        dataNE(Id,_,Prazo),
        escolheVeiculo(Prazo, Peso, CDistancia, CTempo, Veiculo),
        statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
        write('Execution took '), write(ExecutionTime), write(' ms.'), nl.




%------------------------------------------------------A* (Distancia) ---------------------------------------------

resolveAEstrelaDistancia(Grafo, NodoFinal, Caminho/Custo):-  inicial(NodoInicial),
                                                distancia(NodoInicial, NodoFinal, Estima),
                                                aEstrelaAux(Grafo, [[NodoInicial]/0/Estima], NodoFinal, ICaminho/Custo/SolEst),
                                                inverso(ICaminho, Caminho).


aEstrelaAux(Grafo, Caminhos, NodoFinal, [NodoFinal|Cam]/Custo/Est) :- melhorEstEstrela(Caminhos, [NodoFinal|Cam]/Custo/Est),!.


aEstrelaAux(Grafo, Caminhos, NodoFinal, Caminho) :- melhorEstEstrela(Caminhos, MelhorCaminho),
                                                    seleciona_caminho(MelhorCaminho, Caminhos, OutrosCaminhos),
                                                    expande(Grafo, MelhorCaminho, NodoFinal, ExpandeCaminhos),
                                                    append(OutrosCaminhos, ExpandeCaminhos, NovosCaminhos),
                                                    aEstrelaAux(Grafo, NovosCaminhos, NodoFinal, Caminho).



melhorEstEstrela([E/Custo/Est], E/Custo/Est).
melhorEstEstrela([E1/Custo1/Est1, E2/Custo2/Est2 | Outros], BestE/Custo/BestEst):-
    H1 is Est1 + Custo1,
    H2 is Est2 + Custo2,
    H1 >= H2,
    melhorEstEstrela([E2/Custo2/Est2 | Outros],BestE/Custo/BestEst).

melhorEstEstrela([E1/Custo1/Est1, E2/Custo2/Est2 | Outros],BestE/Custo/BestEst):-
    H1 is Est1 + Custo1,
    H2 is Est2 + Custo2,
    H1 =< H2,
    melhorEstEstrela([E1/Custo1/Est1 | Outros], BestE/Custo/BestEst).

seleciona_caminho(E, [E|XS], XS).
seleciona_caminho(E, [X|XS], [X|YS]) :- seleciona_caminho(E,XS,YS).


%--------------------------------------------------- A* (Tempo) ---------------------------------------------------

resolveAEstrelaTempo(Grafo, NodoFinal, Id, Caminho, CustoDist, CustoTempo, Veiculo) :-
        statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
        resolveAEstrelaDistancia(Grafo, NodoFinal, Caminho/CustoDist),
        encomendaNE(Id, Peso,_,_,_,_,_,_),
        dataNE(Id,_,Prazo),
        escolheVeiculo(Prazo, Peso, CustoDist, CustoTempo, Veiculo),
        statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
        write('Execution took '), write(ExecutionTime), write(' ms.'), nl.



%-------------------------------------------- circuito mais ecologico (tempo)  ------------------------------------

maisEco(Grafo, NodoFinal, Id, Caminho, CDistancia, CTempo, Veiculo) :- 
    resolveAEstrelaTempo(Grafo, NodoFinal, Id, Caminho, CDistancia, CTempo, Veiculo).


%--------------------------------------------- circuito mais curto (distancia) ------------------------------------

maisCurto(Grafo, NodoFinal, Caminho, Custo) :-
    resolveAEstrelaDistancia(Grafo, NodoFinal, Caminho/Custo).


%------------------------------------------------ circuito com mais entregas/maisPeso/maisVo --------------------------------------
%encomendaNE(Id,Peso,Volume,Rua,Freguesia,Preço,idCliente,idEstafeta).

maisEntregas(Grafo, Id, CaminhoMelhor/NrEntregas , CaminhoMaisPesado/Pesos, CaminhoMelhorVolume/Volumes) :-
    encomendaNE(Id,Peso,Volume, NodoFinal, _, _, _, _),
    findall(Cam , bfDistancia(Grafo,NodoFinal,Cam/CDist), Caminhos),
    calculaNumeroEnc(Caminhos, CaminhosEnc),
    calculaMelhor(CaminhosEnc,CaminhoMelhor/NrEntregas),
    calculaPesos(Caminhos,CaminhosPesados),
    calculaMelhor(CaminhosPesados, CaminhoMelhorPesado/Pesos).

maisVolume(Grafo, Id, CaminhoMelhorVolume/Volumes) :-
    encomendaNE(Id,Peso,Volume, NodoFinal, _, _, _, _),
    findall(Cam , bfDistancia(Grafo,NodoFinal,Cam/CDist), Caminhos),
    calculaVolumes(Caminhos,CaminhosVolume),
    calculaVolumeEncAux(CaminhosVolume, CaminhoMelhorVolume/Volumes).




calculaMelhor([Caminho/N1], Caminho/N1).
calculaMelhor([Caminho1/N1, Caminho2/N2 | Caminhos], MelhorCaminho) :-
        N1 >= N2, !,
        calculaMelhor([Caminho1/N1 | Caminhos], MelhorCaminho).
calculaMelhor([_| Caminho], MelhorCaminho) :-
        calculaMelhor(Caminho, MelhorCaminho).


calculaNumeroEnc([] , []).
calculaNumeroEnc([Cam| XS ] , [Cam/Encomendas | Res]) :- calculaNumeroEncAux(Cam,Encomendas), calculaNumeroEnc(XS,Res).


calculaNumeroEncAux([] , 0).
calculaNumeroEncAux([Rua | Ruas], Res) :- findall(Id , encomendaNE(Id,_,_,Rua,_,_,_,_) , L), 
                                          length(L,N), 
                                          calculaNumeroEncAux(Ruas,Temp),
                                          Res is Temp+N.



calculaPesos([] , []).
calculaPesos([Cam| XS ] , [Cam/PesoTotal | Res]) :- calculaPesoEncAux(Cam,PesoTotal), calculaPesos(XS,Res).

calculaPesoEncAux([] , 0).
calculaPesoEncAux([Rua | Ruas], Res) :- findall(Peso , encomendaNE(_,Peso,_,Rua,_,_,_,_) , L), 
                                        sumlist(L,N),
                                        calculaPesoEncAux(Ruas,Temp),
                                        Res is Temp+N.

calculaVolumes([] , []).
calculaVolumes([Cam| XS ] , [Cam/VolumeTotal | Res]) :- calculaVolumeEncAux(Cam,VolumeTotal), calculaVolumes(XS,Res).

calculaVolumeEncAux([] , 0).
calculaVolumeEncAux([Rua | Ruas], Res) :- findall(Volume , encomendaNE(_,_,Volume,Rua,_,_,_,_) , L), 
                                        sumlist(L,N),
                                        calculaVolumeEncAux(Ruas,Temp),
                                        Res is Temp+N.


%--------------------------------------------- escolha do veículo  ------------------------------------------------


escolheVeiculo(Prazo, Peso, CDistancia, CustoTempo, VeiculoSolucao):-
    findall((CTempo, Veiculo), (custoFinalDec(Veiculo, Peso, CDistancia, CTempo), suportaPeso(Veiculo, Peso), entregaDentroPrazo(Prazo, CTempo)), Solucoes),
    comparaSolucoes(Solucoes, (CustoTempo, VeiculoSolucao)).


entregaDentroPrazo( Prazo, CTempo) :- get_date_time_value(year, Year),
                                      get_date_time_value(month, Month),
                                      get_date_time_value(day, Day),
                                      get_date_time_value(hour, Hour),
                                      NewHour is Hour + CTempo,
                                      antes(NewHour/Day/Month/Year, Prazo).


antes(H1/D1/M1/A1, HI/DI/MI/AI) :- A1<AI , !;
                                   A1=:=AI ,  M1<MI, !;
                                   A1=:=AI ,  M1=:=MI , D1<DI, !;
                                   A1=:=AI ,  M1=:=MI , D1=:=DI, H1<HI.



comparaSolucoes([Solucao], Solucao).
comparaSolucoes([(Tempo1, Veiculo1), (Tempo2, Veiculo2) | Solucoes], Solucao) :-
    Tempo1 =< Tempo2, !,
    comparaSolucoes([(Tempo1, Veiculo1) | Solucoes], Solucao).
comparaSolucoes([(Tempo, Veiculo)| Solucoes], Solucao) :-
    comparaSolucoes(Solucoes, Solucao).



suportaPeso( bicicleta, Peso ) :- Peso =< 5.
suportaPeso( mota, Peso ) :- Peso =< 20.
suportaPeso( carro, Peso) :- Peso =< 100.



custoFinalDec(bicicleta, Peso, CustoDist, CustoTempo) :-
    velocidadeMedia(bicicleta,X),
    CustoTempo is CustoDist/(X-(0.7*Peso)).
    
custoFinalDec(mota, Peso, CustoDist, CustoTempo) :-
    velocidadeMedia(mota,X),
    CustoTempo is CustoDist/(X-(0.5*Peso)).
    
custoFinalDec(carro, Peso, CustoDist, CustoTempo) :-
    velocidadeMedia(carro,X),
    CustoTempo is CustoDist/(X-(0.1*Peso)).


%---------------------------------- Encontra o melhor Caminho com a BreathFirst--------------------------------------

encontraMelhorBF(Grafo, Id, NodoFinal, Veiculo, MelhorCusto, MelhorCaminho) :-
    statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
    findall(Solucao/CTempo, bfTempo(Grafo, Id, NodoFinal, Solucao, CTempo, CDistancia, Veiculo), Solucoes),
    encontraMelhor(Solucoes, MelhorCaminho/MelhorCusto),
    statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
    write('Execution took '), write(ExecutionTime), write(' ms.'), nl.




encontraMelhor([Cam/Tempo], Cam/Tempo).
encontraMelhor([Cam1/Tempo1, Cam2/Tempo2 |Cams], Cam/Tempo):- 
            Tempo1<Tempo2, !,
            encontraMelhor([Cam1/Tempo1|Cams], Cam/Tempo).
encontraMelhor([Cam1/Tempo1, Cam2/Tempo2 | Cams], Cam/Tempo):- 
            encontraMelhor([Cam2/Tempo2 | Cams], Cam/Tempo).


%------------------------------------------- predicados auxiliares ------------------------------------------------




expande(Grafo, [NodoAtual|Cam]/Custo/Estimativa, NodoFinal, ExpandeCaminhos) :- findall([NovoNodo, NodoAtual | Cam]/NovoCusto/NovaEstimativa,
                                                                                (adjacente( Grafo, NodoAtual, NovoNodo, CustoAresta), 
                                                                                \+member(NovoNodo,Cam), 
                                                                                NovoCusto is CustoAresta + Custo, 
                                                                                distancia(NovoNodo, NodoFinal, NovaEstimativa)), 
                                                                                ExpandeCaminhos).


inverso(Xs, Ys):-
    inverso(Xs, [], Ys).

inverso([], Xs, Xs).
inverso([X|Xs],Ys, Zs):-
       inverso(Xs, [X|Ys], Zs).


get_date_time_value(Key, Value) :-
    get_time(Stamp),
    stamp_date_time(Stamp, DateTime, local),
    date_time_value(Key, DateTime, Value).