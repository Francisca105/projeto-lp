% Francisca Almeida - 105901
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % para listas completas
:- ["dados.pl"], ["keywords.pl"]. % ficheiros a importar

% Auxiliares

/*
    semestre(+Periodo, -Semestre) :- 
        Semestre eh o semestre correspondente ao periodo Periodo.
*/

semestre(Periodo, Semestre) :- 
    ((Periodo = p1; Periodo = p2),Semestre = p1_2);
    ((Periodo = p3; Periodo = p4),Semestre = p3_4).


% ---------------------------- 3.1 ----------------------------

/*
    eventosSemSalas(-EventosSemSala) :- 
        EventosSemSala eh a lista ordenada sem elementos
        repetidos de todos os eventos sem sala.
*/

eventosSemSalas(EventosSemSala) :-
    findall(Evento, 
    (evento(Evento, _, _, _, semSala)),
    EventosSemSala).

/*
    eventosSemSalasDiaSemana(+DiaDaSemana, -EventosSemSala) :- 
        EventosSemSala eh a lista ordenada sem elementos
        repetidos de todos os eventos sem sala no dia da semana
        DiaDaSemana.
*/

eventosSemSalasDiaSemana(DiaDaSemana, EventosSemSala) :-
    findall(Evento, 
    (evento(Evento, _, _, _, semSala), 
    horario(Evento, DiaDaSemana, _,_,_,_)),
    EventosSemSala).

/*
    eventosSemSalasPeriodo(+Periodos, -EventosSemSalaPeriodo) :- 
        EventosSemSalaPeriodo eh a lista ordenada sem elementos
        repetidos de todos os eventos sem sala no periodo
        Periodos.
*/

eventosSemSalasPeriodo(Periodos, EventosSemSalaPeriodo) :-
    (
        ((member(p1, Periodos); member(p2, Periodos)),
        append(Periodos, [p1_2], ListaPeriodos));
        ((member(p3, Periodos); member(p4, Periodos)),
        append(Periodos, [p3_4], ListaPeriodos))
    ),
    % Como Periodos eh uma lista, verifica se p1 ou p2/p3 ou p4
    % estao na lista e adiciona p1_2/p3_4 a ListaPeriodos

    eventosSemSalas(S),
    findall(Evento,(
        member(Periodo, ListaPeriodos),
        member(Evento, S), % Evento pertence a EventosSemSala
        horario(Evento, _, _, _, _, Periodo)),
    EventosSemSala),

    sort(EventosSemSala, EventosSemSalaPeriodo), !;
    (EventosSemSalaPeriodo = []).


% ---------------------------- 3.2 ----------------------------


/*
    organizaEventos(+ListaEventos, +Periodo, -Res) :- 
        Res eh a lista ordenada sem elementos repetidos de todos
        os eventos da lista ListaEventos que ocorrem no periodo
        Periodo.
*/

organizaEventos([], _, []).
organizaEventos([Evento|R], Periodo, Res) :-
    (semestre(Periodo, Semestre),
    horario(Evento, _, _, _, _, P),
    member(P, [Periodo, Semestre])), % Se o evento pertence ao periodo
    organizaEventos(R, Periodo, ResAux), 
    append([Evento], ResAux, ResAux2), % Adiciona-o a ResAux2
    sort(ResAux2, Res).

organizaEventos([_|R], Periodo, Res) :-
    organizaEventos(R, Periodo, Res).

/*
    eventosMenoresQue(+Duracao, -ListaEventosMenoresQue) :- 
        ListaEventosMenoresQue eh a lista ordenada sem elementos
        repetidos de todos os eventos com duracaoo menor ou igual
        a Duracao.
*/

eventosMenoresQue(Duracao, ListaEventosMenoresQue) :-
    findall(Evento, 
        (horario(Evento, _, _,_ , DuracaoEvento, _),
        DuracaoEvento =< Duracao),
    ListaEventosMenoresQue).

/*
    eventosMenoresQueBool(+Evento, +Duracao) :- 
        Evento eh um evento com duracao menor ou igual a Duracao.
*/

eventosMenoresQueBool(Evento, Duracao) :-
    eventosMenoresQue(Duracao, ListaEventosMenoresQue),
    member(Evento, ListaEventosMenoresQue), !.

/*
    procuraDisciplinas(+Curso, -ListaDisciplinas) :- 
        ListaDisciplinas eh a lista ordenada alfabeticamente do
        nome de todas as disciplinas do curso Curso.
*/

procuraDisciplinas(Curso, ListaDisciplinas) :-
    findall(Disciplina, (
        turno(Evento, Curso, _, _),
        evento(Evento, Disciplina, _, _, _)
        ),ListaDisciplinasAux),
    sort(ListaDisciplinasAux, ListaDisciplinas). 
    % Oderna a lista e retira elementos repetidos

/*
    organizaDisciplinas(+ListaDisciplinas, +Curso, -Semestres) :-
        Semestres eh a lista que contehm os dois semestres (no
        formato de listas) com as disciplinas do curso Curso da
        lista ListaDisciplinas ordenadas de forma alfabetica.
        O predicado falha se nao existir no curso Curso uma
        disciplina da ListaDisciplinas.
*/

organizaDisciplinas(ListaDisciplinas, Curso, Semestres) :-
    procuraDisciplinas(Curso, ListaDisciplinasCurso),
    subtract(ListaDisciplinas, ListaDisciplinasCurso, ListaComparacao),
    % Disciplinas que nao pertencem ao curso
    length(ListaComparacao, 0),
    % Se a lista nao tiver elementos, entao todas as disciplinas pertencem ao curso

    organizaDisciplinasAux(ListaDisciplinas, Curso, [], [p1,p2,p1_2], Semestre1), 
    % Organiza as disciplinas do primeiro semestre
    organizaDisciplinasAux(ListaDisciplinas, Curso, [], [p3,p4,p3_4], Semestre2), 
    % Organiza as disciplinas do segundo semestre

    sort(Semestre1, Semestre1Aux),
    sort(Semestre2, Semestre2Aux),

    subtract(Semestre2Aux, Semestre1Aux, Semestre2Aux_),
    % Remove as disciplinas que estao no primeiro semestre do segundo
    % (considerando que nao ha disciplinas anuais)

    SemestresAux = [Semestre1Aux, Semestre2Aux_],

    sort(SemestresAux, Semestres).

/* 
    organizaDisciplinasAux(+ListaDisciplinas, +Curso, +Acc, +Periodos, -Semestres) :- 
        Semestres eh a lista que contem as disciplinas do curso Curso da
        lista ListaDisciplinas, do semestre com os peoriodos Periodos,
        ordenadas de forma alfabetica.
*/

organizaDisciplinasAux([], _, Acc, _, Acc).
organizaDisciplinasAux([Disciplina|R], Curso, Acc, Periodos, Semestres) :-
    % Se a disciplina pertence ao curso e ao periodo

    (evento(Evento, Disciplina, _, _, _),
    horario(Evento, _, _, _, _, P),
    member(P, Periodos),
    turno(Evento, Curso, _, _)), !,

    organizaDisciplinasAux(R, Curso, [Disciplina|Acc], Periodos, Semestres).

organizaDisciplinasAux([_|R], Curso, Acc, Periodos, Semestres) :-
        organizaDisciplinasAux(R, Curso, Acc, Periodos, Semestres).

/*
    horasCurso(+Periodo, +Curso, +Ano, -TotalHoras) :- 
        TotalHoras eh o numero total de horas do curso Curso, no
        ano Ano e periodo Periodo.
*/


horasCurso(Periodo, Curso, Ano, TotalHoras) :-
    procuraDisciplinas(Curso, ListaDisciplinas),
    semestre(Periodo, Semestre),
    Periodos = [Periodo, Semestre],

    findall(Evento, (
        member(Disciplina, ListaDisciplinas),
        % Procura dentro das disciplinas do curso

        evento(Evento, Disciplina, _, _, _),
        turno(Evento, Curso, Ano, _),
        % O evento da disciplina do ano pretendido
        horario(Evento, _, _, _, _, P),
        % O evento tem de estar no periodo pretendido
        
        member(P, Periodos)
    ), ListaEventos),

    sort(ListaEventos, ListaEventosSorted),

    findall(Duracao, (
        member(Evento, ListaEventosSorted),
        horario(Evento, _, _, _, Duracao, _)
    ), ListaDuracoes),

    sum_list(ListaDuracoes, TotalHoras), !.

/*
    evolucaoHorasCurso(+Curso, -Evolucao) :- 
        Evolucao eh a lista de tuplos com todos os anos letivos e
        periodos letivos de um determinado curso, com o numero de
        horas de cada um.
*/

evolucaoHorasCurso(Curso, Evolucao) :-
    findall((Ano, Periodo, Horas), (
        member(Ano, [1,2,3]),
        member(Periodo, [p1,p2,p3,p4]),
        horasCurso(Periodo, Curso, Ano, Horas)
    ), Evolucao).


% ---------------------------- 3.3 ----------------------------


/*
    ocupaSlot(+HoraInicioDada, +HoraFimDada, +HoraInicioEvento, +HoraFimEvento, -Horas) :- 
        Horas eh o numero de horas sobrepostas que o evento ocupa
        no slot de um determinado tempo.
        Se nao existir sobreposicao, o predicado falha.
*/

ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas) :-
    Hora_Inicio is max(HoraInicioDada, HoraInicioEvento),
    Hora_Fim is min(HoraFimDada, HoraFimEvento),
    Hora is Hora_Fim - Hora_Inicio,

    abs(Hora, Hora),
    % Se Hora for negativo, o predicado falha (nao ha sobreposicao)

    Horas is Hora.

/*
    numHorasOcupadas(+Periodo, +TipoSala, +DiaSemana, +HoraInicio, +HoraFim, -SomaHoras) :- 
        SomaHoras eh a soma de todas as horas ocupadas por eventos
        de um determinado tipo de sala, dia da semana e horario.
*/

numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras) :-
    semestre(Periodo, Semestre),
    Periodos = [Periodo, Semestre],
    salas(TipoSala, Salas),
    
    findall(Horas, (
        member(Sala, Salas),
        member(P, Periodos),

        evento(Evento, _,_,_, Sala),
        horario(Evento, DiaSemana, HoraInicioEvento, HoraFimEvento, _, P),
        ocupaSlot(HoraInicio, HoraFim, HoraInicioEvento, HoraFimEvento, Horas)
        % Horas eh o numero de horas que o evento Evento ocupa na sala Sala
    ), Horas),

    sum_list(Horas, SomaHoras), !.

/*
    ocupacaoMax(+TipoSala, +HoraInicio, +HoraFim, -Max) :- 
        Max eh o numero maximo de horas que podem ser ocupadas
        por eventos de um determinado tipo de sala e horario.
*/

ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max) :-
    salas(TipoSala, Salas),
    length(Salas, Numero_Salas),
    Max is Numero_Salas * (HoraFim - HoraInicio).

/*
    percentagem(+SomaHoras, +Max, -Percentagem) :- 
        Percentagem eh a divisao de SomaHoras por Max,
        multiplicado por 100.
*/

percentagem(SomaHoras, Max, Percentagem) :-
    Percentagem is SomaHoras / Max * 100.

/*
    ocupacaoCritica(+HoraInicio, +HoraFim, +Threshold, -Resultados) :- 
        Resultados eh a lista de todos os eventos que ultrapassam
        o Threshold de ocupacao de um determinado horario.
*/

ocupacaoCritica(HoraInicio, HoraFim, Threshold, Resultados) :-
    findall(casosCriticos(DiaSemana, TipoSala, PercentagemArredondada), (
        % Cria uma lista de casosCriticos com o dia da semana, 
        % o tipo de sala e a percentagem de ocupacao

        salas(TipoSala, Salas),
        evento(Evento, _, _, _, Sala),
        member(Sala, Salas),
        horario(Evento, DiaSemana, _, _, _, Periodo),

        member(Periodo, [p1, p2, p3, p4]),

        numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras),
        ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max),
        percentagem(SomaHoras, Max, Percentagem),
        
        Percentagem > Threshold,
        % Se a percentagem for maior que o threshold, o evento eh considerado critico
        ceiling(Percentagem, PercentagemArredondada)
        % Arredonda a percentagem da lista para o inteiro mais proximo
    ), ListaResultados),
    sort(ListaResultados, Resultados).


% ---------------------------- 3.4 ----------------------------

% Auxiliares para o predicado ocupacaoMesa

/*
    obterRestricao(+ListaRestricoes, +Restricao, -Pessoa) :- 
        Pessoa eh a pessoa que tem a restricao Restricao na lista ListaRestricoes.
*/

obterRestricao(ListaRestricoes, Restricao, Pessoa) :-
    member(Restricao, ListaRestricoes),
    arg(1, Restricao, Pessoa), !.

/*
    honra1(+ListaRestricoes, +Lista) :- 
        Verifica se a Pessoa da restricao honra(Cab1, Pessoa) senta-se ah direita
        do Cab1.
*/

honra1(ListaRestricoes, [_,_,_,Cab1,_,X6,_,_]) :-
    obterRestricao(ListaRestricoes, honra(Cab1, Pessoa), Cab1), !,
    Pessoa = X6. % Pessoa senta-se a direita do Cab1
honra1(_, _). % Se nao existir a restricao honra(Cab1, Pessoa) retorna verdadeiro

/*
    cabeceira1(+ListaRestricoes, +Lista) :- 
        Verifica se a Pessoa da restricao cab1(Pessoa) senta-se no lugar da cabeceira 1.
*/

cabeceira1(ListaRestricoes, [X1,_,_,Cab1,_,X6,_,_]) :-
    obterRestricao(ListaRestricoes, cab1(Pessoa), Pessoa), !, 
    Cab1 = Pessoa, % Pessoa senta-se no lugar da cabeceira 1
    honra1(ListaRestricoes, [X1,_,_,Cab1,_,X6,_,_]).
cabeceira1(_, _).

/*
    honra2(+ListaRestricoes, +Lista) :- 
        Verifica se a Pessoa da restricao honra(Cab2, Pessoa) senta-se ah direita
        do Cab2.
*/

honra2(ListaRestricoes, [_,_,X3,_,Cab2,_,_,_]) :-
    obterRestricao(ListaRestricoes, honra(Cab2, Pessoa), Cab2), !,
    Pessoa = X3.
honra2(_, _).

/*
    cabeceira2(+ListaRestricoes, +Lista) :- 
        Verifica se a Pessoa da restricao cab2(Pessoa) senta-se no lugar da cabeceira 2.
*/

cabeceira2(ListaRestricoes, [_,_,X3,_,Cab2,_,_,X8]) :-
    obterRestricao(ListaRestricoes, cab2(Pessoa), Pessoa), !,
    Cab2 = Pessoa,
    honra2(ListaRestricoes, [_,_,X3,_,Cab2,_,_,X8]).
cabeceira2(_, _).

/*
    lado(+ListaRestricoes, +Lista) :- 
        Verifica se a Pessoa da restricao lado(P1, P2) senta-se ao lado de P2.
*/

lado(ListaRestricoes, [X1,X2,X3,_,_,X6,X7,X8]) :-
    findall(lado(P1, P2), (
        member(Restricao, ListaRestricoes),
        Restricao =.. [lado, P1, P2]
    ), ListaLados),
    maplist(ladoAux([X1,X2,X3,_,_,X6,X7,X8]), ListaLados).
    % Lista todas as restricoes de lado e verifica se cada uma delas eh verdadeira

/*
    ladoAux(+Lista, +lado(P1, P2)) :- 
        Verifica se P1 e P2 estao lado a lado na lista Lista.
*/

ladoAux([P1,P2,_,_,_,_,_,_], lado(P1, P2)).
ladoAux([P2,P1,_,_,_,_,_,_], lado(P1, P2)).
ladoAux([_,P2,P1,_,_,_,_,_], lado(P1, P2)).
ladoAux([_,P1,P2,_,_,_,_,_], lado(P1, P2)).
ladoAux([_,_,_,_,_,P1,P2,_], lado(P1, P2)).
ladoAux([_,_,_,_,_,P2,P1,_], lado(P1, P2)).
ladoAux([_,_,_,_,_,_,P1,P2], lado(P1, P2)).
ladoAux([_,_,_,_,_,_,P2,P1], lado(P1, P2)).

/*
    naoLado(+ListaRestricoes, +Lista) :- 
        Verifica se a Pessoa da restricao naoLado(P1, P2) nao se senta ao lado de P2.
*/

naoLado(ListaRestricoes, [X1,X2,X3,_,_,X6,X7,X8]) :-
    findall(lado(P1, P2), (
        member(Restricao, ListaRestricoes),
        Restricao =.. [naoLado, P1, P2]
    ), ListaNaoLados),
    maplist(naoLadoAux([X1,X2,X3,_,_,X6,X7,X8]), ListaNaoLados).

/*
    naoLadoAux(+Lista, +lado(P1, P2)) :- 
        Verifica se P1 e P2 nao estao lado a lado na lista Lista.
*/

naoLadoAux(Mesa, lado(P1, P2)) :-
    \+ ladoAux(Mesa, lado(P1, P2)).

/*
    frente(+ListaRestricoes, +Lista) :- 
        Verifica se a Pessoa da restricao frente(P1, P2) senta-se ah frente de P2.
*/

frente(ListaRestricoes, [X1,X2,X3,_,_,X6,X7,X8]) :-
    findall(frente(P1, P2), (
        member(Restricao, ListaRestricoes),
        Restricao =.. [frente, P1, P2]
    ), ListaFrentes),
    maplist(frenteAux([X1,X2,X3,_,_,X6,X7,X8]), ListaFrentes).
    % Lista todas as restricoes de frente e verifica se cada uma delas eh verdadeira

/*
    frenteAux(+Lista, +frente(P1, P2)) :- 
        Verifica se P1 e P2 estao na frente na lista Lista.
*/

frenteAux([P1,_,_,_,_,P2,_,_], frente(P1,P2)).
frenteAux([_,P1,_,_,_,_,P2,_], frente(P1,P2)).
frenteAux([_,_,P1,_,_,_,_,P2], frente(P1,P2)).
frenteAux([P2,_,_,_,_,P1,_,_], frente(P1,P2)).
frenteAux([_,P2,_,_,_,_,P1,_], frente(P1,P2)).
frenteAux([_,_,P2,_,_,_,_,P1], frente(P1,P2)).

/*
    naoFrente(+ListaRestricoes, +Lista) :- 
        Verifica se a Pessoa da restricao naoFrente(P1, P2) nao se senta ah frente de P2.
*/

naoFrente(ListaRestricoes, [X1,X2,X3,_,_,X6,X7,X8]) :-
    findall(frente(P1, P2), (
        member(Restricao, ListaRestricoes),
        Restricao =.. [naoFrente, P1, P2]
    ), ListaNaoFrentes),
    maplist(naoFrenteAux([X1,X2,X3,_,_,X6,X7,X8]), ListaNaoFrentes).

/*
    naoFrenteAux(+Lista, +frente(P1, P2)) :- 
        Verifica se P1 e P2 nao estao frente a frente na lista Lista.
*/

naoFrenteAux(Mesa, frente(P1, P2)) :-
    \+ frenteAux(Mesa, frente(P1, P2)).

/*
    colocarNaMesa(+Lista, -Mesa) :- 
        Coloca os elementos da Lista na Mesa de acordo com o formato
        pretendido.
*/

colocarNaMesa([X1,X2,X3,X4,X5,X6,X7,X8], Mesa) :-
    Mesa = [[X1,X2,X3],[X4,X5],[X6,X7,X8]].

/*
    ocupacaoMesa(+Pessoas, +ListaRestricoes, -MesaPedida) :- 
        A partir da lista de Pessoas e das ListaRestricoes
        devolve uma MesaPedida que satisfaz as restricoes, de forma a
        todas as pessoas se sentarem uma unica vez.
*/

ocupacaoMesa(Pessoas, ListaRestricoes, MesaPedida) :-
    findall(Mesa, (
        permutation(Pessoas, Mesa),
        % Todas as permutacoes de Pessoas

        cabeceira1(ListaRestricoes, Mesa),
        cabeceira2(ListaRestricoes, Mesa),

        lado(ListaRestricoes, Mesa),
        naoLado(ListaRestricoes, Mesa),

        frente(ListaRestricoes, Mesa),
        naoFrente(ListaRestricoes, Mesa)
        
    ), MesaAux),
    flatten(MesaAux, MesaAuxLimpa),
    % Retira as listas de dentro da lista de listas
    colocarNaMesa(MesaAuxLimpa, MesaPedida).