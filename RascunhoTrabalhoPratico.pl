

veiculo(bicicleta,1).
veiculo(mota,2).
veiculo(carro,3).

%------------------------------------ENTIDADES------------------------------------

%encomenda(Id,Peso,Volume,Rua,Freguesia,Prazo,Veiculo,Preço,Data,Cliente).
%preço ->


encomenda(1 ,5 , 8 , "Rua Santo António, nº420","Celeirós", "21-11-21",bicicleta,10,"20-11-21", 123).
encomenda(2 ,14, 10, "Praça do Comércio, nº15" , "Silveiros","25-11-21",mota,9,"23-11-21",456).
encomenda(3 ,90 , 5 , "Entrocamento de São Geraldes, nº30","Escudeiros","31-12-21",carro,14,"25-12-21",789).
encomenda(4 ,6 , 6 , "Rua José Sócrates, nº2","Fraião","01-01-22",carro,15,"31-12-21",12345).
%encomenda(5 , 4 , 13, "Rua Monte Carrinhos , nº45").
%encomenda(6 , 12, 9 , "Rua Paulo Fernandes , nº48").
%encomenda(7 , 3 , 4 , "Praça Arsenalistas  , nº410").
%encomenda(8 , 10, 12, "Rua do Souto , nº13").
%encomenda(9 , 9 , 3 , "Rua Santa Margarida , nº60").
%encomenda(10, 3 , 2 , "Largo Sr. dos Aflitos, nº4").
%encomenda(11, 14, 3 , "Rua do Raio, nº142").
%encomenda(12, 2 , 4 , "Rua da Avenida da Liberdade, nº60").


%cliente(Id,IdsEncomendas).
%IdsEncomendas -> lista dos ids
cliente(123,[1]).
cliente(12345,[4]).

cliente(456,[2]).

cliente(789,[3]).


%estafeta(Id, NumEnc,Rank,Encomendas,NumPen).
estafeta(987,1,5,[1,4],0).
estafeta(654,2,3,[2],3).
estafeta(321,3,4,[3],2).


%---------------------Mais ecologico
maxEcologico(Estafeta,Encomendas):- maxEcologico1(Estafeta,Encomendas).


%-> calcular pontuação ecologica para cada estafeta
%-> dizer qual é a minima e devovler esse estafeta




%---------------------Cria lista de pares Id do Estafeta e Respetiva lista de encomendas-----------



pontuacao((_,[]), 0).
pontuacao((_,[(_,_,_,_,_,_,bicicleta,_,_,_)|Y]),R) :- pontuacao(Y,R1), R is R1 + 1.
pontuacao((_,[(_,_,_,_,_,_,mota,_,_,_) |Y]),R) :- pontuacao(Y,R1), R is R1 + 2.   
pontuacao((_,[(_,_,_,_,_,_,carro,_,_,_)|Y]),R):- pontuacao(Y,R1), R is R1 + 3.   


(987,[(1,5,8,"Rua Santo António, nº420","Celeirós","21-11-21",bicicleta,10,"20-11-21",123),(4,6,6,"Rua José Sócrates, nº2","Fraião","01-01-22",carro,15,"31-12-21",12345)])



%---------------------------QUERY 3 --------------------------------------------


clientesEstafetaAux(_,[],Result).
clientesEstafetaAux(Estafeta,[(Estafeta, List) | Y], Result):- clientesEstafetaAux2(List,Result).
clientesEstafetaAux(Estafeta,[(X,_) | Y], Result):- clientesEstafetaAux(Estafeta,Y,Result).


clientesEstafetaAux2([],[]).
clientesEstafetaAux2([(_,_,_,_,_,_,_,_,_,Cliente) | Y],[Cliente|Result]):- clientesEstafetaAux2(Y,Result).

clientesEstafeta(Estafeta,R):- encomendasEstafeta(Pares),clientesEstafetaAux(Estafeta,Pares,R).


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
listEncomendasAux([(NE,A,B,C,D,E,F,G,H,I) | LE] , [NE | LNE] ):-
    encomenda(NE,A,B,C,D,E,F,G,H,I),
    listEncomendasAux(LE,LNE).


%listEncomendasAux([],[]).
%listEncomendasAux([ V | LV] , [NE | LNE] ):-
%    encomenda(NE,_,_,_,_,_,V,_,_,_),
%    listEncomendasAux(LE,LNE).

pontuacaoTeste(Result):-  encomendasEstafeta(Pares), pontuacao(Pares,Result).    


%Encomendas -> lista

