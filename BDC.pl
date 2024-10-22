:- dynamic evidence/2.

% FATOS
%--------------------------------------------

%DEFINIÇÃO DAS DOENÇAS:
doenca(alzheimer_inicial) :-
    evidence(diagnosis,yes),
    evidence(diagnosis_alzheimer,yes),
    evidence(diagnosis_alzheimer_stage,initial).
doenca(alzheimer_inicial) :-
    findall(yes, (
        evidence(observation_alzheimer_spacial_disorientation, yes);
        evidence(observation_alzheimer_memory_loss_frustration, yes);
        evidence(observation_alzheimer_slight_memory_loss, yes)
    ), Results),
    length(Results, Count),
    Count >= 2.
doenca(alzheimer_avancada) :-
    evidence(diagnosis,yes),
    evidence(diagnosis_alzheimer,yes),
    evidence(diagnosis_alzheimer_stage,advanced).
doenca(alzheimer_avancada) :-
    doenca(alzheimer_inicial),
    findall(yes, (
        evidence(observation_alzheimer_stare,yes);
        evidence(observation_alzheimer_needs_permanent_watch,yes);
        evidence(observation_alzheimer_cant_make_stimuli,yes);
        evidence(observation_alzheimer_fall_history,yes)
    ), Results),
    length(Results, Count),
    Count >= 1.
doenca(parkinson_inicial) :-
    evidence(diagnosis,yes),
    evidence(diagnosis_parkinson,yes),
    evidence(diagnosis_parkinson_stage,initial).
doenca(parkinson_inicial) :-
    findall(yes, (
        evidence(observation_parkinson_shaking,yes);
        evidence(observation_parkinson_bent_spine,yes);
        evidence(observation_parkinson_balance_loss,yes);
        evidence(observation_parkinson_initial_hear_loss,yes)
    ), Results),
    length(Results, Count),
    Count >= 2.
doenca(parkinson_avancada):-
    evidence(diagnosis,yes),
    evidence(diagnosis_parkinson,yes),
    evidence(diagnosis_parkinson_stage,advanced).
doenca(parkinson_avancada) :-
    doenca(parkinson_inicial),
    findall(yes, (
        evidence(observation_parkinson_locomotion_difficulties,yes);
        evidence(observation_parkinson_intense_shaking,yes);
        evidence(observation_parkinson_fine_motor_control,yes)
    ), Results),
    length(Results, Count),
    Count >= 1.
doenca(demencia_vascular_inicial):-
    evidence(diagnosis,yes),
    evidence(diagnosis_vascular_dementia,yes),
    evidence(diagnosis_vascular_dementia_stage,initial).
doenca(demencia_vascular_inicial) :-
    findall(yes, (
        evidence(observation_vascular_dementia_slight_memory_loss,yes);
        evidence(observation_vascular_dementia_depression_anxiety,yes);
        evidence(observation_vascular_dementia_thinking_problems,yes)
    ), Results),
    length(Results, Count),
    Count >= 2.
doenca(demencia_vascular_avancada):-
    evidence(diagnosis,yes),
    evidence(diagnosis_vascular_dementia,yes),
    evidence(diagnosis_vascular_dementia_stage,advanced).
doenca(demencia_vascular_avancada) :-
    doenca(demencia_vascular_inicial),
    findall(yes, (
        evidence(observation_vascular_dementia_heavy_memory_loss,yes);
        evidence(observation_vascular_dementia_people_recognition,yes);
        evidence(observation_vascular_dementia_history_aggressiveness_insomnia_agitation,yes);
        evidence(observation_vascular_dementia_body_control,yes)
    ), Results),
    length(Results, Count),
    Count >= 1.


get_doencas(Doencas) :-
    findall(X, doenca(X), DoencasList),
    sort(DoencasList, Doencas).
%--------------------------------------------


%DEFINIÇÃO DAS ATIVIDADES:
%Expressão Plástica
atividade(tecnica_construcao).
atividade(materiais_reciclados).
atividade(execucao_trabalhos_festas).
atividade(trabalhos_feltro).
atividade(pequenos_arranjos).
atividade(quadros).
atividade(trabalhos_decoracao_centro).

%Quotidiano
atividade(confeccao_sobremesas).

%Estimulação Cognitiva e Sensorial
atividade(exercicio_orientacao_temporal).
atividade(exercicios_conhecimento).
atividade(exercicios_reconhecimento_espacial).
atividade(exercicios_tarefas_verbais).  
atividade(jogos_reconhecimento_imagem).
atividade(exercicios_memoria).
atividade(jogos_destreza_manual).
atividade(exercicios_leitura_escrita).
atividade(jogos_diferencas).
atividade(exercicios_compreensao_numerica).
atividade(exercicios_reconhecimento_sentidos).
atividade(reagir_diferentes_sabores).
atividade(diferenciar_cheiros).
atividade(distinguir_vozes_animais).

%Exercício Físico
atividade(caminhadas).
atividade(exercicios_aquecimento).
atividade(fisioterapia).

%Lúdico -Musical  
atividade(karaoke).
atividade(musicograma).
atividade(utilizacao_instrumentos).
atividade(adivinhar_musicas).

%Culturais e Sociais: Jogos de Animação fisico
atividade(jogos_petanca).
atividade(jogos_bowling).
atividade(jogos_lencinho_adaptado).
atividade(jogos_mimica).

%Culturais e Sociais: Jogos de Animação 
atividade(jogos_bingo).
atividade(jogos_missako).
atividade(jogos_domino).
atividade(jogos_galo).

%Culturais e Sociais: Cuidados de Imagem
atividade(manicure).
atividade(massagens).
atividade(maquilhagem).

%Culturais e Sociais: Oficina de Teatro
atividade(leitura_textos_dramaticos).
atividade(interpretacao_personagens).


%--------------------------------------------


%RELAÇÃO DOENÇA/ATIVIDADE (NÃO PODE REALIZAR):
%DEM. VASCULAR - INICIAL
nao_pode(demencia_vascular_inicial, exercicios_memoria).
nao_pode(demencia_vascular_inicial, exercicio_orientacao_temporal).

%DEM. VASCULAR - AVANCADA
nao_pode(demencia_vascular_avancada, exercicios_memoria).
nao_pode(demencia_vascular_avancada, trabalhos_feltro).
nao_pode(demencia_vascular_avancada, pequenos_arranjos).
nao_pode(demencia_vascular_avancada, confeccao_sobremesas).
nao_pode(demencia_vascular_avancada, execucao_trabalhos_festas).
nao_pode(demencia_vascular_avancada, quadros).
nao_pode(demencia_vascular_avancada, exercicio_orientacao_temporal).
nao_pode(demencia_vascular_avancada, exercicios_reconhecimento_espacial).
nao_pode(demencia_vascular_avancada, jogos_destreza_manual).
nao_pode(demencia_vascular_avancada, exercicios_leitura_escrita).
nao_pode(demencia_vascular_avancada, jogos_diferencas).
nao_pode(demencia_vascular_avancada, jogos_petanca).
nao_pode(demencia_vascular_avancada, jogos_bowling).

%ALZHEIMER - INICIAL
nao_pode(alzheimer_inicial, leitura_textos_dramaticos).
nao_pode(alzheimer_inicial, jogos_bingo).
nao_pode(alzheimer_inicial, jogos_domino).
nao_pode(alzheimer_inicial, jogos_diferencas).

%ALZHEIMER - AVANCADA
nao_pode(alzheimer_avancada, tecnica_construcao).
nao_pode(alzheimer_avancada, materiais_reciclados).
nao_pode(alzheimer_avancada, execucao_trabalhos_festas).
nao_pode(alzheimer_avancada, trabalhos_feltro).
nao_pode(alzheimer_avancada, pequenos_arranjos).
nao_pode(alzheimer_avancada, quadros).
nao_pode(alzheimer_avancada, exercicios_tarefas_verbais).
nao_pode(alzheimer_avancada, exercicios_leitura_escrita).
nao_pode(alzheimer_avancada, jogos_diferencas).
nao_pode(alzheimer_avancada, diferenciacao_cheiros).
nao_pode(alzheimer_avancada, utilizacao_instrumentos).
nao_pode(alzheimer_avancada, adivinhar_musicas).
nao_pode(alzheimer_avancada, jogos_bingo).
nao_pode(alzheimer_avancada, jogos_missako).
nao_pode(alzheimer_avancada, jogos_domino).
nao_pode(alzheimer_avancada, jogos_galo).
nao_pode(alzheimer_avancada, leitura_textos_dramaticos).
nao_pode(parkinson_avancada, exercicios_memoria).

%PARKINSON - INICIAL
nao_pode(parkinson_inicial, pequenos_arranjos).
nao_pode(parkinson_inicial, jogos_destreza_manual).
nao_pode(parkinson_inicial, adivinhar_musicas).
nao_pode(parkinson_inicial, jogos_missako).

%PARKINSON - AVANCADA
nao_pode(parkinson_avancada, tecnica_construcao).
nao_pode(parkinson_avancada, execucao_trabalhos_festas).
nao_pode(parkinson_avancada, trabalhos_feltro).
nao_pode(parkinson_avancada, pequenos_arranjos).
nao_pode(parkinson_avancada, quadros).
nao_pode(parkinson_avancada, exercicios_memoria).
nao_pode(parkinson_avancada, jogos_destreza_manual).
nao_pode(parkinson_avancada, exercicios_leitura_escrita).
nao_pode(parkinson_avancada, caminhadas).
nao_pode(parkinson_avancada, utilizacao_instrumentos).
nao_pode(parkinson_avancada, adivinhar_musicas).
nao_pode(parkinson_avancada, jogos_petanca).
nao_pode(parkinson_avancada, jogos_bowling).
nao_pode(parkinson_avancada, jogos_mimica).
nao_pode(parkinson_avancada, jogos_missako).


%--------------------------------------------


% CONDIÇÕES (OUTROS ASPECTOS)
%1 Integracao_social
condicao(integracao_social, relacoes_sociais_boas) :-
    evidence(integracao_social, relacoes_sociais_boas). 
condicao(integracao_social, graves_problemas_de_integracao) :-
    evidence(integracao_social, graves_problemas_de_integracao). 
condicao(integracao_social, pessoa_isolada) :-
    evidence(integracao_social, pessoa_isolada).
%2 Visao
condicao(visao, boa_visao) :-
    evidence(visao, boa_visao).
condicao(visao, visao_com_dificuldade) :-
    evidence(visao, visao_com_dificuldade). 
condicao(visao, cegueira) :-
    evidence(visao, cegueira).
%3 Audicao
condicao(audicao, boa_audicao) :-
    evidence(audicao, boa_audicao). 
condicao(audicao, audicao_com_dificuldade) :-
    evidence(audicao, audicao_com_dificuldade). 
condicao(audicao, surdez) :-
    evidence(audicao, surdez).
%4 Fala
condicao(fala, fala_normalmente) :-
    evidence(fala, fala_normalmente). 
condicao(fala, expressa_se_com_dificuldade) :-
    evidence(fala, expressa_se_com_dificuldade). 
condicao(fala, nao_se_faz_compreender) :-
    evidence(fala, nao_se_faz_compreender).
%5 Olfato
condicao(olfato, olfato_funciona_normalmente) :-
    evidence(olfato, olfato_funciona_normalmente). 
condicao(olfato, olfato_tem_dificuldade) :-
    evidence(olfato, olfato_tem_dificuldade). 
condicao(olfato, olfato_perda_total_olfato) :-
    evidence(olfato, olfato_perda_total_olfato).
%6 Mobilidade_superior
condicao(mobilidade_superior, superior_funciona_normalmente) :-
    evidence(mobilidade_superior, superior_funciona_normalmente). 
condicao(mobilidade_superior, superior_tem_dificuldade) :-
    evidence(mobilidade_superior, superior_tem_dificuldade). 
condicao(mobilidade_superior, superior_no_consegue_mover) :-
    evidence(mobilidade_superior, superior_no_consegue_mover).
%7 Mobilidade_inferior
condicao(mobilidade_inferior, inferior_funciona_normalmente) :-
    evidence(mobilidade_inferior, inferior_funciona_normalmente). 
condicao(mobilidade_inferior, inferior_tem_dificuldade) :-
    evidence(mobilidade_inferior, inferior_tem_dificuldade). 
condicao(mobilidade_inferior, inferior_no_consegue_mover) :-
    evidence(mobilidade_inferior, inferior_no_consegue_mover).
%8 Manuseamento_objectos
condicao(manuseamento_objectos, manipulacao_correta) :-
    evidence(manuseamento_objectos, manipulacao_correta). 
condicao(manuseamento_objectos, so_alguns) :-
    evidence(manuseamento_objectos, so_alguns). 
condicao(manuseamento_objectos, nao_pode_faze_lo) :-
    evidence(manuseamento_objectos, nao_pode_faze_lo).
%9 Ler
condicao(ler, ler_sem_dificuldade) :-
    evidence(ler, ler_sem_dificuldade). 
condicao(ler, ler_alguma_dificultade) :-
    evidence(ler, ler_alguma_dificultade). 
condicao(ler, ler_nao_consegue) :-
    evidence(ler, ler_nao_consegue).
%10 Escrever
condicao(escrever, escrever_sem_dificuldade) :-
    evidence(escrever, escrever_sem_dificuldade). 
condicao(escrever, escrever_alguma_dificultade) :-
    evidence(escrever, escrever_alguma_dificultade). 
condicao(escrever, escrever_nao_consegue) :-
    evidence(escrever, escrever_nao_consegue).
%11 Mobilidade
condicao(mobilidade, sem_dificuldade) :-
    evidence(mobilidade, sem_dificuldade). 
condicao(mobilidade, alguma_dificuldade) :-
    evidence(mobilidade, alguma_dificuldade). 
condicao(mobilidade, dependencia_total) :-
    evidence(mobilidade, dependencia_total).

get_condicoes(Condicoes):-
    findall(X, condicao(X, _), CondicoesList),
    sort(CondicoesList, Condicoes).

%--------------------------------------------


% %RELAÇÃO CONDIÇÕES/ATIVIDADE (NÃO PODE REALIZAR):

%1 Integracao_social 
%Graves_problemas_de_integracao (GRAVIDADE: MEDIA)
no_adecuado(graves_problemas_de_integracao, execucao_trabalhos_festas).
no_adecuado(graves_problemas_de_integracao, trabalhos_decoracao_centro). 
no_adecuado(graves_problemas_de_integracao, jogos_reconhecimento_imagem).
no_adecuado(graves_problemas_de_integracao, karaoke).
no_adecuado(graves_problemas_de_integracao, jogos_lencinho_adaptado).
no_adecuado(graves_problemas_de_integracao, jogos_mimica).
no_adecuado(graves_problemas_de_integracao, jogos_bingo).
no_adecuado(graves_problemas_de_integracao, jogos_galo). 

%Pessoa_isolada (GRAVIDADE: ALTA)
no_adecuado(pessoa_isolada, execucao_trabalhos_festas).
no_adecuado(pessoa_isolada, trabalhos_decoracao_centro).
no_adecuado(pessoa_isolada, confeccao_sobremesas).
no_adecuado(pessoa_isolada, jogos_reconhecimento_imagem).
no_adecuado(pessoa_isolada, karaoke).
no_adecuado(pessoa_isolada, jogos_petanca).
no_adecuado(pessoa_isolada, jogos_bowling).
no_adecuado(pessoa_isolada, jogos_lencinho_adaptado).
no_adecuado(pessoa_isolada, jogos_mimica).
no_adecuado(pessoa_isolada, jogos_bingo).
no_adecuado(pessoa_isolada, jogos_missako).
no_adecuado(pessoa_isolada, jogos_domino).
no_adecuado(pessoa_isolada, jogos_galo).
no_adecuado(pessoa_isolada, leitura_textos_dramatimanicurecos).
no_adecuado(pessoa_isolada, massagens).
no_adecuado(pessoa_isolada, maquilhagem).
no_adecuado(pessoa_isolada, leitura_textos_dramaticos).
no_adecuado(pessoa_isolada, interpretacao_personagens).


%2 Visao
%Visao_com_dificuldade (GRAVIDADE MEDIA)
no_adecuado(visao_com_dificuldade, tecnica_construcao).
no_adecuado(visao_com_dificuldade, trabalhos_feltro).
no_adecuado(visao_com_dificuldade, pequenos_arranjos).
no_adecuado(visao_com_dificuldade, trabalhos_decoracao_centro).
no_adecuado(visao_com_dificuldade, confeccao_sobremesas).
no_adecuado(visao_com_dificuldade, jogos_reconhecimento_imagem).
no_adecuado(visao_com_dificuldade, exercicios_leitura_escrita).
no_adecuado(visao_com_dificuldade, jogos_diferencas).
no_adecuado(visao_com_dificuldade, musicograma).
no_adecuado(visao_com_dificuldade, jogos_petanca).
no_adecuado(visao_com_dificuldade, jogos_bowling).
no_adecuado(visao_com_dificuldade, jogos_bingo).
no_adecuado(visao_com_dificuldade, leitura_textos_dramaticos).

%Cegueira (GRAVIDADE ALTA)
no_adecuado(cegueira, tecnica_construcao).
no_adecuado(cegueira, materiais_reciclados).
no_adecuado(cegueira, execucao_trabalhos_festas).
no_adecuado(cegueira, trabalhos_feltro).
no_adecuado(cegueira, pequenos_arranjos).
no_adecuado(cegueira, quadros).
no_adecuado(cegueira, trabalhos_decoracao_centro).
no_adecuado(cegueira, confeccao_sobremesas).
no_adecuado(cegueira, jogos_reconhecimento_imagem).
no_adecuado(cegueira, exercicios_leitura_escrita).
no_adecuado(cegueira, jogos_diferencas).
no_adecuado(cegueira, caminhadas).
no_adecuado(cegueira, utilizacao_instrumentos).
no_adecuado(cegueira, musicograma).
no_adecuado(cegueira, jogos_petanca).
no_adecuado(cegueira, jogos_bowling).
no_adecuado(cegueira, jogos_lencinho_adaptado).
no_adecuado(cegueira, jogos_mimica).
no_adecuado(cegueira, jogos_bingo).
no_adecuado(cegueira, leitura_textos_dramaticos).
no_adecuado(cegueira, interpretacao_personagens).


%3 Audicao
%Audicao_com_dificuldade (GRAVIDADE MEDIA)
no_adecuado(audicao_com_dificuldade, karaoke).
no_adecuado(audicao_com_dificuldade, musicograma).
no_adecuado(audicao_com_dificuldade, utilizacao_instrumentos).
no_adecuado(audicao_com_dificuldade, adivinhar_musicas).
no_adecuado(audicao_com_dificuldade, distinguir_vozes_animais).
no_adecuado(audicao_com_dificuldade, jogos_bingo).
no_adecuado(audicao_com_dificuldade, leitura_textos_dramaticos).

%Surdez (GRAVIDADE ALTA)
no_adecuado(surdez, confeccao_sobremesas).
no_adecuado(surdez, karaoke).
no_adecuado(surdez, musicograma).
no_adecuado(surdez, utilizacao_instrumentos).
no_adecuado(surdez, adivinhar_musicas).
no_adecuado(surdez, distinguir_vozes_animais).
no_adecuado(surdez, exercicios_tarefas_verbais).
no_adecuado(surdez, exercicios_leitura_escrita).
no_adecuado(surdez, jogos_missako).
no_adecuado(surdez, jogos_bingo).
no_adecuado(surdez, jogos_mimica).
no_adecuado(surdez, leitura_textos_dramaticos).
no_adecuado(surdez, interpretacao_personagens).


%4 Fala
%Expressa_se_com_dificuldade (GRAVIDADE MEDIA)
no_adecuado(expressa_se_com_dificuldade, karaoke).
no_adecuado(expressa_se_com_dificuldade, interpretacao_personagens).
no_adecuado(expressa_se_com_dificuldade, exercicios_tarefas_verbais).
no_adecuado(expressa_se_com_dificuldade, leitura_textos_dramaticos).
no_adecuado(expressa_se_com_dificuldade, adivinhar_musicas).
no_adecuado(nao_se_faz_compreender, musicograma).

%Expressa_se_com_dificuldade (GRAVIDADE ALTA)
no_adecuado(nao_se_faz_compreender, exercicios_tarefas_verbais).
no_adecuado(nao_se_faz_compreender, karaoke).
no_adecuado(nao_se_faz_compreender, interpretacao_personagens).
no_adecuado(nao_se_faz_compreender, exercicios_tarefas_verbais).
no_adecuado(nao_se_faz_compreender, leitura_textos_dramaticos).
no_adecuado(nao_se_faz_compreender, adivinhar_musicas).
no_adecuado(nao_se_faz_compreender, musicograma).


%5 Olfato
%Tem_dificuldade (GRAVIDADE MEDIA)
no_adecuado(olfato_tem_dificuldade, diferenciar_cheiros).
no_adecuado(olfato_tem_dificuldade, reagir_diferentes_sabores).

%Perda_total_olfato (GRAVIDADE ALTA)
no_adecuado(olfato_perda_total_olfato, diferenciar_cheiros).
no_adecuado(olfato_perda_total_olfato, reagir_diferentes_sabores).


%6 Mobilidade parte superior
%Tem_dificuldade (GRAVIDADE MEDIA)
no_adecuado(superior_tem_dificuldade, tecnica_construcao).
no_adecuado(superior_tem_dificuldade, materiais_reciclados).
no_adecuado(superior_tem_dificuldade, trabalhos_feltro).
no_adecuado(superior_tem_dificuldade, pequenos_arranjos).
no_adecuado(superior_tem_dificuldade, quadros).
no_adecuado(superior_tem_dificuldade, trabalhos_decoracao_centro).
no_adecuado(superior_tem_dificuldade, jogos_destreza_manual).
no_adecuado(superior_tem_dificuldade, manicure).
no_adecuado(superior_tem_dificuldade, maquilhagem).
no_adecuado(superior_tem_dificuldade, utilizacao_instrumentos).
no_adecuado(superior_tem_dificuldade, jogos_petanca).
no_adecuado(superior_tem_dificuldade, jogos_bowling).
no_adecuado(superior_tem_dificuldade, jogos_galo).
no_adecuado(superior_tem_dificuldade, jogos_domino).

%No_consegue_mover (GRAVIDADE ALTA)
no_adecuado(superior_no_consegue_mover, tecnica_construcao).
no_adecuado(superior_no_consegue_mover, materiais_reciclados).
no_adecuado(superior_no_consegue_mover, execucao_trabalhos_festas).
no_adecuado(superior_no_consegue_mover, trabalhos_feltro).
no_adecuado(superior_no_consegue_mover, pequenos_arranjos).
no_adecuado(superior_no_consegue_mover, quadros).
no_adecuado(superior_no_consegue_mover, trabalhos_decoracao_centro).
no_adecuado(superior_no_consegue_mover, confeccao_sobremesas).
no_adecuado(superior_no_consegue_mover, jogos_diferencas).
no_adecuado(superior_no_consegue_mover, jogos_destreza_manual).
no_adecuado(superior_no_consegue_mover, fisioterapia).
no_adecuado(superior_no_consegue_mover, manicure).
no_adecuado(superior_no_consegue_mover, maquilhagem).
no_adecuado(superior_no_consegue_mover, musicograma).
no_adecuado(superior_no_consegue_mover, utilizacao_instrumentos).
no_adecuado(superior_no_consegue_mover, exercicios_aquecimento).
no_adecuado(superior_no_consegue_mover, jogos_petanca).
no_adecuado(superior_no_consegue_mover, jogos_bowling).
no_adecuado(superior_no_consegue_mover, jogos_lencinho_adaptado).
no_adecuado(superior_no_consegue_mover, jogos_mimica).
no_adecuado(superior_no_consegue_mover, jogos_bingo).
no_adecuado(superior_no_consegue_mover, jogos_missako).
no_adecuado(superior_no_consegue_mover, jogos_galo).
no_adecuado(superior_no_consegue_mover, jogos_domino).
no_adecuado(superior_no_consegue_mover, interpretacao_personagens).


%7 Mobilidade parte inferior
%Tem_dificuldade (GRAVIDADE MEDIA)
no_adecuado(inferior_tem_dificuldade, jogos_petanca).
no_adecuado(inferior_tem_dificuldade, jogos_bowling).
no_adecuado(inferior_tem_dificuldade, fisioterapia).

%No_consegue_mover (GRAVIDADE ALTA)
no_adecuado(inferior_no_consegue_mover, caminhadas).
no_adecuado(inferior_no_consegue_mover, exercicios_aquecimento).
no_adecuado(inferior_no_consegue_mover, fisioterapia).
no_adecuado(inferior_no_consegue_mover, jogos_petanca).
no_adecuado(inferior_no_consegue_mover, jogos_bowling).
no_adecuado(inferior_no_consegue_mover, jogos_lencinho_adaptado).
no_adecuado(inferior_no_consegue_mover, jogos_mimica).


%8 Manuseamento_objectos
%So_alguns (GRAVIDADE MEDIA)
no_adecuado(so_alguns, trabalhos_feltro).
no_adecuado(so_alguns, pequenos_arranjos).
no_adecuado(so_alguns, confeccao_sobremesas).
no_adecuado(so_alguns, jogos_destreza_manual).
no_adecuado(so_alguns, manicure).
no_adecuado(so_alguns, maquilhagem).
no_adecuado(so_alguns, utilizacao_instrumentos).
no_adecuado(so_alguns, musicograma).

%No_pode (GRAVIDADE ALTA)
no_adecuado(nao_pode_faze_lo, tecnica_construcao).
no_adecuado(nao_pode_faze_lo, materiais_reciclados).
no_adecuado(nao_pode_faze_lo, execucao_trabalhos_festas).
no_adecuado(nao_pode_faze_lo, trabalhos_feltro).
no_adecuado(nao_pode_faze_lo, pequenos_arranjos).
no_adecuado(nao_pode_faze_lo, quadros).
no_adecuado(nao_pode_faze_lo, trabalhos_decoracao_centro).
no_adecuado(nao_pode_faze_lo, confeccao_sobremesas).
no_adecuado(nao_pode_faze_lo, exercicios_conhecimento).
no_adecuado(nao_pode_faze_lo, jogos_destreza_manual).
no_adecuado(nao_pode_faze_lo, manicure).
no_adecuado(nao_pode_faze_lo, maquilhagem).
no_adecuado(nao_pode_faze_lo, utilizacao_instrumentos).
no_adecuado(nao_pode_faze_lo, musicograma).
no_adecuado(nao_pode_faze_lo, jogos_domino).
no_adecuado(nao_pode_faze_lo, jogos_missako).
no_adecuado(nao_pode_faze_lo, jogos_galo).
no_adecuado(nao_pode_faze_lo, jogos_petanca).
no_adecuado(nao_pode_faze_lo, jogos_bowling).

%9 Ler
%Alguma_dificultade (GRAVIDADE MEDIA)
no_adecuado(ler_alguma_dificultade, exercicios_leitura_escrita).
no_adecuado(ler_alguma_dificultade, leitura_textos_dramaticos).
no_adecuado(ler_alguma_dificultade, interpretacao_personagens).
no_adecuado(ler_alguma_dificultade, musicograma).
no_adecuado(ler_alguma_dificultade, exercicios_compreensao_numerica).

%Nao_consegue (GRAVIDADE ALTA)
no_adecuado(ler_nao_consegue, exercicios_leitura_escrita).
no_adecuado(ler_nao_consegue, exercicios_compreensao_numerica).
no_adecuado(ler_nao_consegue, musicograma).
no_adecuado(ler_nao_consegue, leitura_textos_dramaticos).
no_adecuado(ler_nao_consegue, interpretacao_personagens).
no_adecuado(ler_nao_consegue, exercicios_conhecimento).
no_adecuado(ler_nao_consegue, jogos_bingo).


%10 Escrever
%Alguma_dificultade (GRAVIDADE MEDIA)
no_adecuado(escrever_alguma_dificultade, exercicios_leitura_escrita).
no_adecuado(escrever_alguma_dificultade, exercicios_tarefas_verbais).
no_adecuado(escrever_alguma_dificultade, interpretacao_personagens).
no_adecuado(escrever_alguma_dificultade, musicograma).

%Nao_consegue (GRAVIDADE ALTA)
no_adecuado(escrever_nao_consegue, exercicios_leitura_escrita).
no_adecuado(escrever_nao_consegue, exercicios_tarefas_verbais).
no_adecuado(escrever_nao_consegue, interpretacao_personagens).
no_adecuado(escrever_nao_consegue, musicograma).
no_adecuado(escrever_nao_consegue, jogos_bingo).
no_adecuado(escrever_nao_consegue, exercicios_conhecimento).
no_adecuado(escrever_nao_consegue, exercicios_compreensao_numerica).


%11 Mobilidade
%Alguma_dificuldade (GRAVIDADE MEDIA)
no_adecuado(alguma_dificuldade, jogos_petanca).
no_adecuado(alguma_dificuldade, jogos_bowling).
no_adecuado(alguma_dificuldade, exercicios_aquecimento).
no_adecuado(alguma_dificuldade, fisioterapia).

%Dependencia_total (GRAVIDADE ALTA)
no_adecuado(dependencia_total, caminhadas).
no_adecuado(dependencia_total, exercicios_aquecimento).
no_adecuado(dependencia_total, fisioterapia).
no_adecuado(dependencia_total, jogos_petanca).
no_adecuado(dependencia_total, jogos_bowling).
no_adecuado(dependencia_total, jogos_lencinho_adaptado).
no_adecuado(dependencia_total, jogos_mimica).


%--------------------------------------------


% GOSTOS
gosto(teatro):-
    evidence(teatro, yes).
gosto(museu):-
    evidence(museu,yes).
gosto(musica):-
    evidence(musica,yes).
gosto(leitura):-
    evidence(leitura,yes).
gosto(grupo_recreativo):-
    evidence(grupo_recreativo,yes).
gosto(arte):-
    evidence(arte,yes).
gosto(desporto):-
    evidence(desporto,yes).
gosto(cozinha):-
    evidence(cozinha,yes).
gosto(cozinha):-
    evidence(cozinha,yes).
gosto(trabalhos_manuales):-
    evidence(trabalhos_manuales,yes).

get_gostos(Gostos) :-
    findall(X, gosto(X), GostosList),
    sort(GostosList, Gostos).

%--------------------------------------------

% GOSTOS/ACTIVIDADE (PREFERENCIAS)
%1-Teatro
preferencia(teatro, leitura_textos_dramaticos).
preferencia(teatro, interpretacao_personagens).
preferencia(teatro, jogos_mimica).
%2-Museu
preferencia(museu, quadros). 
preferencia(museu, pequenos_arranjos).
preferencia(museu, caminhadas).
%3-Musica
preferencia(musica, karaoke).
preferencia(musica, musicograma).
preferencia(musica, adivinhar_musicas).
preferencia(musica, utilizacao_instrumentos). 
%4-Leitura
preferencia(leitura, leitura_textos_dramaticos). 
preferencia(leitura, exercicios_leitura_escrita).
preferencia(leitura, exercicios_tarefas_verbais).
%5-Grupo_recreativo
preferencia(grupo_recreativo, jogos_petanca). 
preferencia(grupo_recreativo, jogos_bowling). 
preferencia(grupo_recreativo, jogos_mimica).
preferencia(grupo_recreativo, jogos_bingo).
preferencia(grupo_recreativo, jogos_lencinho_adaptado).
preferencia(grupo_recreativo, execucao_trabalhos_festas).
preferencia(grupo_recreativo, trabalhos_decoracao_centro).
%6-Arte
preferencia(arte, quadros).
preferencia(arte, trabalhos_decoracao_centro).
preferencia(arte, trabalhos_feltro).
preferencia(arte, pequenos_arranjos).
%7-Desporto
preferencia(desporto, caminhadas).
preferencia(desporto, jogos_petanca).
preferencia(desporto, jogos_bowling).
%8-Cozinha
preferencia(cozinha, confeccao_sobremesas).
%9-Trabalhos_manuales
preferencia(trabalhos_manuales, tecnica_construcao).
preferencia(trabalhos_manuales, materiais_reciclados). 
preferencia(trabalhos_manuales, trabalhos_feltro). 
preferencia(trabalhos_manuales, pequenos_arranjos).
preferencia(trabalhos_manuales, quadros). 
preferencia(trabalhos_manuales, trabalhos_decoracao_centro).
preferencia(trabalhos_manuales, manicure).
preferencia(trabalhos_manuales, maquilhagem). 
preferencia(trabalhos_manuales, massagens). 
