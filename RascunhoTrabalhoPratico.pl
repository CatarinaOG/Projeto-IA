

veiculo(bicicleta,1).
veiculo(mota,2).
veiculo(carro,3).

%------------------------------------ENTIDADES------------------------------------

%encomenda(Id,Peso,Volume,Rua,Freguesia,Prazo,Data,Veiculo,Preço,idCliente,idEstafeta).
%preço ->


encomenda(1 ,5 , 8 , "Rua Santo António, nº420","Celeirós", "21-11-21","20-11-21",bicicleta,10, 123,987).
encomenda(2 ,14, 10, "Praça do Comércio, nº15" , "Silveiros","23-11-21","20-11-21",mota,9,456,654).
encomenda(3 ,90 , 5 , "Entrocamento de São Geraldes, nº30","Martim","31-12-21","25-12-21",carro,14,789,321).
encomenda(4 ,6 , 6 , "Rua José Sócrates, nº2","Fraião","01-01-22","31-12-21",carro,15,12345,987).
encomenda(5 , 4 , 13, "Rua Monte Carrinhos , nº45","Martim", "28-03-21","27-03-21", mota, 15,123,654).
encomenda(6 , 12, 9 , "Rua Paulo Fernandes , nº48","Santa Eugénia","10-12-19","09-12-19",carro,20,456,321).
encomenda(7 , 3 , 4 , "Praça Arsenalistas  , nº410","Cabreiros","15-07-18","03-07-18",bicicleta,50,12345,321).

%encomenda(8 , 10, 12, "Rua do Souto , nº13").
%encomenda(9 , 9 , 3 , "Rua Santa Margarida , nº60").
%encomenda(10, 3 , 2 , "Largo Sr. dos Aflitos, nº4").
%encomenda(11, 14, 3 , "Rua do Raio, nº142").
%encomenda(12, 2 , 4 , "Rua da Avenida da Liberdade, nº60").


%cliente(Id,IdsEncomendas).
%IdsEncomendas -> lista dos ids
cliente(123,[1,5]).
cliente(12345,[4,7]).

cliente(456,[2,6]).

cliente(789,[3]).


%estafeta(Id, NumEnc,Rank,Encomendas,NumPen).
estafeta(987,2,5,[1,4],0).
estafeta(654,2,3,[2,5],3).
estafeta(321,3,4,[3,6,7],2).


%-------------------------- QUERY 1 --------------------------------------------


%-> calcular pontuação ecologica para cada estafeta
%-> dizer qual é a minima e devovler esse estafeta


maxEcologico(Result) :- encomendasEstafeta(Pares),maxEcologicoAux(Pares,Result).


maxEcologicoAux([(Estafeta,_)],Estafeta):-!.
maxEcologicoAux([E1,E2|T],Result):- media(E1,P1),
                                    media(E2,P2), 
                                    P1 =< P2 ,
                                    !, 
                                    maxEcologicoAux([E1|T],Result).
maxEcologicoAux([_|T],Result):- maxEcologicoAux(T,Result).


media((_,Lista),R):- pontuacao((_,Lista),X), length(Lista,Length) , R is X/Length.


pontuacao((_,[]), 0).
pontuacao((_,[(_,_,_,_,_,_,_,bicicleta,_,_)|Y]),R) :- pontuacao((_,Y),R1), R is R1 + 1.
pontuacao((_,[(_,_,_,_,_,_,_,mota,_,_) |Y]),R) :- pontuacao((_,Y),R1), R is R1 + 2.   
pontuacao((_,[(_,_,_,_,_,_,_,carro,_,_)|Y]),R):- pontuacao((_,Y),R1), R is R1 + 3.   


%-------------------------- QUERY 2 --------------------------------------------


estafetasCliente(_,[],[]).
estafetasCliente(Cliente,[X|T],[E|Result]):- 
                        estafetasClienteAux(Cliente,X,E),
                        estafetasCliente(Cliente,T,Result),
                        \+member(E,Result).

%encomenda(Id,Peso,Volume,Rua,Freguesia,Prazo,Data,Veiculo,Preço,idCliente,idEstafeta).


estafetasClienteAux(Cliente,X,E):-encomenda(X,_,_,_,_,_,_,_,_,Cliente,E).




%-------------------------- QUERY 3 --------------------------------------------


clientesEstafetaAux(_,[],Result).
clientesEstafetaAux(Estafeta,[(Estafeta, List) | Y], Result):- clientesEstafetaAux2(List,Result).
clientesEstafetaAux(Estafeta,[(X,_) | Y], Result):- clientesEstafetaAux(Estafeta,Y,Result).


clientesEstafetaAux2([],[]).
clientesEstafetaAux2([(_,_,_,_,_,_,_,_,_,Cliente) | Y],[Cliente|Result]):- clientesEstafetaAux2(Y,Result).

clientesEstafeta(Estafeta,R):- encomendasEstafeta(Pares),
                               clientesEstafetaAux(Estafeta,Pares,R).



%-------------------------- QUERY 4 --------------------------------------------


%encomenda(Id,Peso,Volume,Rua,Freguesia,Prazo,Data,Veiculo,Preço,idCliente,idEstafeta).

valorDiario(Data,Result):- findall((Preco) , encomenda(_,_,_,_,_,_,Data,_,Preco,_,_) ,L),somatorio(L,Result).

somatorio([],0).
somatorio([X|T],Result):- somatorio(T,R), Result is R+X.



%-------------------------- QUERY 5 --------------------------------------------


maisEntregasFreg(Result):- 
                findall((Freguesia) , encomenda(_,_,_,_,Freguesia,_,_,_,_,_,_),Freguesias), 
                difsList2(Freguesias,List),
                maisEntregasAux(List,Pares),
                maiorEntregas(Pares,Result).


difsList([],P,R):- length(P,R),!.
difsList([H|T],P,R):- member(H,P),difsList(T,P,R).
difsList([H|T],P,R):- not(member(H,P)),append([P,[H]],N),difsList(T,N,R).


difsList2([],[]):-!.
difsList2([X|T],[X|Result]):- difsList2(T,Result),\+member(X,Result),!.
difsList2([X|T],Result):-  difsList2(T,Result).

maisEntregasAux([],[]).
maisEntregasAux([X|T] , [(X,Reps)|Result]) :-findall((X) , encomenda(_,_,_,_,X,_,_,_,_,_,_), ListaX), 
                                             length(ListaX,Reps), 
                                             maisEntregasAux(T,Result).

maiorEntregas([(F,_)],F):-!.
maiorEntregas([(F1,Num),(F2,Num2)|T],Result):- Num>=Num2,!, maiorEntregas([(F1,Num)|T],Result).
maiorEntregas([_|T],Result):- maiorEntregas(T,Result).



%------------------------------------------
%------------MÉTODOS AUXILIARES------------
%------------------------------------------



%---------cria a lista de pares (Estafeta,[Encomendas])------------
encomendasEstafeta(ListEncomendas):-
    findall((Estafeta, ListaNumEncomendas) , estafeta(Estafeta, _, _, ListaNumEncomendas, _) ,L),
    listEncomendas(ListEncomendas,L).
    


%--- substitui a lista de numeros de encomenda pela lista de encomendas
listEncomendas([],[]).
listEncomendas([ (Est , LE) | T] , [ (Est , LNE) | L]):-
    listEncomendasAux(LE,LNE),
    listEncomendas(T,L).

%---constroi a lista de encomendas ---
listEncomendasAux([],[]).
listEncomendasAux([(NE,A,B,C,D,E,F,G,H,I,J) | LE] , [NE | LNE] ):-
    encomenda(NE,A,B,C,D,E,F,G,H,I,J),
    listEncomendasAux(LE,LNE).






