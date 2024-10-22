% REGLAS
% Bibliotecas HTTP
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).

% Relación entre pedidos HTTP y predicados que los procesan
:- http_handler('/register_user', register_user, []).

% Creación de servidor HTTP en el puerto 'Port'					
server(Port) :-						
    http_server(http_dispatch, [port(Port)]).

% Manejador para registrar usuario
register_user(Request) :-
    http_parameters(Request,
                    [ doenca(Docenca, [list]), 
                      sex(Sex, [oneof([male, female])]),
                      birth_year(BY, [between(1850, 10000)])
                    ]),
    format('Content-type: text/plain~n~n'),
    format('User registered!~n'),
    format('Doencas: ~w~nSex: ~w~nBirth Year: ~w~n', [Docenca, Sex, BY]).


    
%--------------------------------------------------------------------------------
% Declarar el hecho como dinámico para permitir agregar y actualizar
:- dynamic atividades_permitidas_guardadas/1.
%--------------------------------------------------------------------------------

% Regla para cargar un archivo de base de conocimiento
carrega_bc :- 
    consult('/Users/Carlo/Desktop/BDC.pl'),
    write('Base de conocimiento cargada.'), nl.

%--------------------------------------------------------------------------------

% Regla para guardar las actividades permitidas en un hecho dinámico
iniciar(Doencas) :-
    atividades_permitidas(Doencas, Atividades),
    retractall(atividades_permitidas_guardadas(_)),  % Eliminar cualquier hecho previo
    assertz(atividades_permitidas_guardadas(Atividades)),
    mostrar_atividades_permitidas_guardadas.  

% Regla para recuperar lista de doenca/atividade
atividades_permitidas(Doencas, AtividadesPermitidas) :-
    findall(Atividade, 
            (atividade(Atividade),
             \+ (member(Doenca, Doencas), nao_pode(Doenca, Atividade))), 
            AtividadesPermitidas).

%--------------------------------------------------------------------------------

% Regla para guardar las actividades permitidas en un hecho dinámico
iniciar2(Condicoes) :-
    filtrar_actividades_adecuadas(Condicoes, Atividades),
    retractall(atividades_permitidas_guardadas(_)),  % Eliminar cualquier hecho previo
    assertz(atividades_permitidas_guardadas(Atividades)),
    mostrar_atividades_permitidas_guardadas.

% Regla para filtrar actividades permitidas según condiciones
filtrar_actividades_adecuadas(Condicoes, ActividadesAdecuadas) :-
    atividades_permitidas_guardadas(Atividades),
    findall(Actividad, 
            (member(Actividad, Atividades),
             \+ (member(Cond, Condicoes), no_adecuado(Cond, Actividad))), 
            ActividadesAdecuadas).

%--------------------------------------------------------------------------------

% Regla para iniciar3, que reordena la lista de actividades permitidas según los gustos dados
iniciar3(Gustos) :-
    atividades_permitidas_guardadas(Atividades),
    reordenar_atividades_por_gostos(Gustos, Atividades, Atividades_Ordenadas),
    retractall(atividades_permitidas_guardadas(_)),  % Eliminar cualquier hecho previo
    assertz(atividades_permitidas_guardadas(Atividades_Ordenadas)),
    mostrar_atividades_permitidas_guardadas.

% Regla para reordenar actividades colocando primero las que coinciden con los gustos
reordenar_atividades_por_gostos(Gustos, Atividades, Atividades_Ordenadas) :-
    % Encuentra las actividades relacionadas con los gustos, eliminando duplicados
    findall(Atividade, 
            (member(Gosto, Gustos), preferencia(Gosto, Atividade), member(Atividade, Atividades)), 
            Atividades_Preferidas_Repetidas),
    sort(Atividades_Preferidas_Repetidas, Atividades_Preferidas),  % Eliminar duplicados
    % Filtra las actividades que no son preferidas
    exclude(esta_en_lista(Atividades_Preferidas), Atividades, Otras_Atividades),
    % Combina las actividades preferidas al inicio de la lista
    append(Atividades_Preferidas, Otras_Atividades, Atividades_Ordenadas).

% Predicado auxiliar para verificar si una actividad está en una lista
esta_en_lista(Lista, Elemento) :-
    member(Elemento, Lista).

%--------------------------------------------------------------------------------

% Mostrar las actividades permitidas guardadas
mostrar_atividades_permitidas_guardadas :-
    atividades_permitidas_guardadas(Atividades),
    maplist(writeln, Atividades).

% Mostrar las actividades permitidas guardadas
mostrar_atividades_permitidas_guardadas :-
    atividades_permitidas_guardadas(Atividades),
    (   Atividades \= []
    ->  format('Actividades permitidas: ~n', []),
        maplist(writeln, Atividades)
    ;   writeln('No hay actividades permitidas registradas.')
    ).

