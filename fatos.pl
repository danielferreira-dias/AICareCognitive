%DEFINIÇÃO DAS DOENÇAS:
doenca(parkinson_inicial).
doenca(parkinson_avancada).
doenca(alzheimer_inicial).
doenca(alzheimer_avancada).
doenca(demencia_vascular_avancada).
doenca(demencia_vascular_inicial).


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
atividade(diferenciar_cheiros.).
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
nao_pode(alzheimer_inicial, leitura_textos_dramaticos).
nao_pode(parkinson_avancada, exercicios_memoria).

%PARKINSON - INICIAL
nao_pode(parkinson_inicial, pequenos_arranjos).
nao_pode(parkinson_inicial, jogos_destreza_manual).
nao_pode(parkinson_inicial, adivinhar_musicas).
nao_pode(parkinson_avancada, jogos_missako).

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
nao_pode(parkinson_avancada, jogos_petanca).
nao_pode(parkinson_avancada, jogos_bowling).
nao_pode(parkinson_avancada, jogos_mimica).
nao_pode(parkinson_avancada, jogos_missako).
nao_pode(parkinson_avancada, adivinhar_musicas).


%--------------------------------------------


% CONDIÇÕES (OUTROS ASPECTOS)
%1 Integracao_social
condicao(integracao_social, relacoes_sociais_boas). 
condicao(integracao_social, graves_problemas_de_integracao). 
condicao(integracao_social, pessoa_isolada).
%2 Visao
condicao(visao, boa_visao).
condicao(visao, visao_com_dificuldade). 
condicao(visao, cegueira).
%3 Audicao
condicao(audicao, boa_audicao). 
condicao(audicao, audicao_com_dificuldade). 
condicao(audicao, surdez).
%4 Fala
condicao(fala, fala_normalmente). 
condicao(fala, expressa_se_com_dificuldade). 
condicao(fala, nao_se_faz_compreender).
%5 Olfato
condicao(olfato, olfato_funciona_normalmente). 
condicao(olfato, olfato_tem_dificuldade). 
condicao(olfato, olfato_perda_total_olfato).
%6 Mobilidade_superior
condicao(mobilidade_superior, superior_funciona_normalmente). 
condicao(mobilidade_superior, superior_tem_dificuldade). 
condicao(mobilidade_superior, superior_nao_consegue_mover).
%7 Mobilidade_inferior
condicao(mobilidade_inferior, inferior_funciona_normalmente). 
condicao(mobilidade_inferior, inferior_tem_dificuldade). 
condicao(mobilidade_inferior, inferior_nao_consegue_mover).
%8 Manuseamento_objectos
condicao(manuseamento_objectos, manipulacao_correta). 
condicao(manuseamento_objectos, so_alguns). 
condicao(manuseamento_objectos, nao_pode_faze_lo).
%9 Ler
condicao(ler, ler_sem_dificuldade). 
condicao(ler, ler_alguma_dificultade). 
condicao(ler, ler_nao_consegue).
%10 Escrever
condicao(escrever, escrever_sem_dificuldade). 
condicao(escrever, escrever_alguma_dificultade). 
condicao(escrever, escrever_nao_consegue).
%11 Mobilidade
condicao(mobilidade, sem_dificuldade). 
condicao(mobilidade, alguma_dificuldade). 
condicao(mobilidade, dependencia_total).


%--------------------------------------------


% %RELAÇÃO CONDIÇÕES/ATIVIDADE (NÃO PODE REALIZAR):

%1 Integracao_social 
%Graves_problemas_de_integracao (GRAVIDADE: MEDIA)
nao_adecuado(graves_problemas_de_integracao, execucao_trabalhos_festas).
nao_adecuado(graves_problemas_de_integracao, trabalhos_decoracao_centro). 
nao_adecuado(graves_problemas_de_integracao, jogos_reconhecimento_imagem).
nao_adecuado(graves_problemas_de_integracao, karaoke).
nao_adecuado(graves_problemas_de_integracao, jogos_lencinho_adaptado).
nao_adecuado(graves_problemas_de_integracao, jogos_mimica).
nao_adecuado(graves_problemas_de_integracao, jogos_bingo).
nao_adecuado(graves_problemas_de_integracao, jogos_galo). 

%Pessoa_isolada (GRAVIDADE: ALTA)
nao_adecuado(pessoa_isolada, execucao_trabalhos_festas).
nao_adecuado(pessoa_isolada, trabalhos_decoracao_centro).
nao_adecuado(pessoa_isolada, confeccao_sobremesas).
nao_adecuado(pessoa_isolada, jogos_reconhecimento_imagem).
nao_adecuado(pessoa_isolada, karaoke).
nao_adecuado(pessoa_isolada, jogos_petanca).
nao_adecuado(pessoa_isolada, jogos_bowling).
nao_adecuado(pessoa_isolada, jogos_lencinho_adaptado).
nao_adecuado(pessoa_isolada, jogos_mimica).
nao_adecuado(pessoa_isolada, jogos_bingo).
nao_adecuado(pessoa_isolada, jogos_missako).
nao_adecuado(pessoa_isolada, jogos_domino).
nao_adecuado(pessoa_isolada, jogos_galo).
nao_adecuado(pessoa_isolada, leitura_textos_dramatimanicurecos).
nao_adecuado(pessoa_isolada, massagens).
nao_adecuado(pessoa_isolada, maquilhagem).
nao_adecuado(pessoa_isolada, leitura_textos_dramaticos).
nao_adecuado(pessoa_isolada, interpretacao_personagens).


%2 Visao
%Visao_com_dificuldade (GRAVIDADE MEDIA)
nao_adecuado(visao_com_dificuldade, tecnica_construcao).
nao_adecuado(cegueira, trabalhos_feltro).
nao_adecuado(cegueira, pequenos_arranjos).
nao_adecuado(visao_com_dificuldade, trabalhos_decoracao_centro).
nao_adecuado(visao_com_dificuldade, confeccao_sobremesas).
nao_adecuado(visao_com_dificuldade, jogos_reconhecimento_imagem).
nao_adecuado(visao_com_dificuldade, exercicios_leitura_escrita).
nao_adecuado(visao_com_dificuldade, jogos_diferencas).
nao_adecuado(visao_com_dificuldade, musicograma).
nao_adecuado(cegueira, jogos_petanca).
nao_adecuado(cegueira, jogos_bowling).
nao_adecuado(visao_com_dificuldade, jogos_bingo).
nao_adecuado(visao_com_dificuldade, leitura_textos_dramaticos).

%Cegueira (GRAVIDADE ALTA)
nao_adecuado(cegueira, tecnica_construcao).
nao_adecuado(cegueira, materiais_reciclados).
nao_adecuado(cegueira, execucao_trabalhos_festas).
nao_adecuado(cegueira, trabalhos_feltro).
nao_adecuado(cegueira, pequenos_arranjos).
nao_adecuado(cegueira, quadros).
nao_adecuado(cegueira, trabalhos_decoracao_centro).
nao_adecuado(cegueira, confeccao_sobremesas).
nao_adecuado(cegueira, jogos_reconhecimento_imagem).
nao_adecuado(cegueira, exercicios_leitura_escrita).
nao_adecuado(cegueira, jogos_diferencas).
nao_adecuado(cegueira, caminhadas).
nao_adecuado(cegueira, utilizacao_instrumentos).
nao_adecuado(cegueira, musicograma).
nao_adecuado(cegueira, jogos_petanca).
nao_adecuado(cegueira, jogos_bowling).
nao_adecuado(cegueira, jogos_lencinho_adaptado).
nao_adecuado(cegueira, jogos_mimica).
nao_adecuado(cegueira, jogos_bingo).
nao_adecuado(cegueira, leitura_textos_dramaticos).
nao_adecuado(cegueira, interpretacao_personagens).


%3 Audicao
%Audicao_com_dificuldade (GRAVIDADE MEDIA)
nao_adecuado(audicao_com_dificuldade, karaoke).
nao_adecuado(audicao_com_dificuldade, musicograma).
nao_adecuado(audicao_com_dificuldade, utilizacao_instrumentos).
nao_adecuado(audicao_com_dificuldade, adivinhar_musicas).
nao_adecuado(audicao_com_dificuldade, distinguir_vozes_animais).
nao_adecuado(audicao_com_dificuldade, jogos_bingo).
nao_adecuado(audicao_com_dificuldade, leitura_textos_dramaticos).

%Surdez (GRAVIDADE ALTA)
nao_adecuado(surdez, confeccao_sobremesas).
nao_adecuado(surdez, karaoke).
nao_adecuado(surdez, musicograma).
nao_adecuado(surdez, utilizacao_instrumentos).
nao_adecuado(surdez, adivinhar_musicas).
nao_adecuado(surdez, distinguir_vozes_animais).
nao_adecuado(surdez, exercicios_tarefas_verbais).
nao_adecuado(surdez, exercicios_leitura_escrita).
nao_adecuado(surdez, jogos_missako).
nao_adecuado(surdez, jogos_bingo).
nao_adecuado(surdez, jogos_mimica).
nao_adecuado(surdez, leitura_textos_dramaticos).
nao_adecuado(surdez, interpretacao_personagens).


%4 Fala
%Expressa_se_com_dificuldade (GRAVIDADE MEDIA)
nao_adecuado(expressa_se_com_dificuldade, karaoke).
nao_adecuado(expressa_se_com_dificuldade, interpretacao_personagens).
nao_adecuado(expressa_se_com_dificuldade, exercicios_tarefas_verbais).
nao_adecuado(expressa_se_com_dificuldade, leitura_textos_dramaticos).
nao_adecuado(expressa_se_com_dificuldade, adivinhar_musicas).
nao_adecuado(nao_se_faz_compreender, musicograma).

%Expressa_se_com_dificuldade (GRAVIDADE ALTA)
nao_adecuado(nao_se_faz_compreender, exercicios_tarefas_verbais).
nao_adecuado(nao_se_faz_compreender, karaoke).
nao_adecuado(nao_se_faz_compreender, interpretacao_personagens).
nao_adecuado(nao_se_faz_compreender, exercicios_tarefas_verbais).
nao_adecuado(nao_se_faz_compreender, leitura_textos_dramaticos).
nao_adecuado(nao_se_faz_compreender, adivinhar_musicas).
nao_adecuado(nao_se_faz_compreender, musicograma).


%5 Olfato
%Tem_dificuldade (GRAVIDADE MEDIA)
nao_adecuado(olfato_tem_dificuldade, diferenciar_cheiros).
nao_adecuado(olfato_tem_dificuldade, reagir_diferentes_sabores).

%Perda_total_olfato (GRAVIDADE ALTA)
nao_adecuado(olfato_perda_total_olfato, diferenciar_cheiros).
nao_adecuado(olfato_perda_total_olfato, reagir_diferentes_sabores).


%6 Mobilidade parte superior
%Tem_dificuldade (GRAVIDADE MEDIA)
nao_adecuado(superior_tem_dificuldade, tecnica_construcao).
nao_adecuado(superior_tem_dificuldade, materiais_reciclados).
nao_adecuado(superior_tem_dificuldade, trabalhos_feltro).
nao_adecuado(superior_tem_dificuldade, pequenos_arranjos).
nao_adecuado(superior_tem_dificuldade, quadros).
nao_adecuado(superior_tem_dificuldade, trabalhos_decoracao_centro).
nao_adecuado(superior_tem_dificuldade, jogos_destreza_manual).
nao_adecuado(superior_tem_dificuldade, manicure).
nao_adecuado(superior_tem_dificuldade, maquilhagem).
nao_adecuado(superior_tem_dificuldade, utilizacao_instrumentos).
nao_adecuado(superior_tem_dificuldade, jogos_petanca).
nao_adecuado(superior_tem_dificuldade, jogos_bowling).
nao_adecuado(superior_tem_dificuldade, jogos_galo).
nao_adecuado(superior_tem_dificuldade, jogos_domino).

%No_consegue_mover (GRAVIDADE ALTA)
nao_adecuado(superior_nao_consegue_mover, tecnica_construcao).
nao_adecuado(superior_nao_consegue_mover, materiais_reciclados).
nao_adecuado(superior_nao_consegue_mover, execucao_trabalhos_festas).
nao_adecuado(superior_nao_consegue_mover, trabalhos_feltro).
nao_adecuado(superior_nao_consegue_mover, pequenos_arranjos).
nao_adecuado(superior_nao_consegue_mover, quadros).
nao_adecuado(superior_nao_consegue_mover, trabalhos_decoracao_centro).
nao_adecuado(superior_nao_consegue_mover, confeccao_sobremesas).
nao_adecuado(superior_nao_consegue_mover, jogos_diferencas).
nao_adecuado(superior_nao_consegue_mover, jogos_destreza_manual).
nao_adecuado(superior_nao_consegue_mover, fisioterapia).
nao_adecuado(superior_nao_consegue_mover, manicure).
nao_adecuado(superior_nao_consegue_mover, maquilhagem).
nao_adecuado(superior_nao_consegue_mover, musicograma).
nao_adecuado(superior_nao_consegue_mover, utilizacao_instrumentos).
nao_adecuado(superior_nao_consegue_mover, exercicios_aquecimento).
nao_adecuado(superior_nao_consegue_mover, jogos_petanca).
nao_adecuado(superior_nao_consegue_mover, jogos_bowling).
nao_adecuado(superior_nao_consegue_mover, jogos_lencinho_adaptado).
nao_adecuado(superior_nao_consegue_mover, jogos_mimica).
nao_adecuado(superior_nao_consegue_mover, jogos_bingo).
nao_adecuado(superior_nao_consegue_mover, jogos_missako).
nao_adecuado(superior_nao_consegue_mover, jogos_galo).
nao_adecuado(superior_nao_consegue_mover, jogos_domino).
nao_adecuado(superior_nao_consegue_mover, interpretacao_personagens).


%7 Mobilidade parte inferior
%Tem_dificuldade (GRAVIDADE MEDIA)
nao_adecuado(inferior_tem_dificuldade, jogos_petanca).
nao_adecuado(inferior_tem_dificuldade, jogos_bowling).
nao_adecuado(inferior_tem_dificuldade, fisioterapia).

%No_consegue_mover (GRAVIDADE ALTA)
nao_adecuado(inferior_nao_consegue_mover, caminhadas).
nao_adecuado(inferior_nao_consegue_mover, exercicios_aquecimento).
nao_adecuado(inferior_nao_consegue_mover, fisioterapia).
nao_adecuado(inferior_nao_consegue_mover, jogos_petanca).
nao_adecuado(inferior_nao_consegue_mover, jogos_bowling).
nao_adecuado(inferior_nao_consegue_mover, jogos_lencinho_adaptado).
nao_adecuado(inferior_nao_consegue_mover, jogos_mimica).


%8 Manuseamento_objectos
%So_alguns (GRAVIDADE MEDIA)
nao_adecuado(so_alguns, trabalhos_feltro).
nao_adecuado(so_alguns, pequenos_arranjos).
nao_adecuado(so_alguns, confeccao_sobremesas).
nao_adecuado(so_alguns, jogos_destreza_manual).
nao_adecuado(so_alguns, manicure).
nao_adecuado(so_alguns, maquilhagem).
nao_adecuado(so_alguns, utilizacao_instrumentos).
nao_adecuado(so_alguns, musicograma).

%No_pode (GRAVIDADE ALTA)
nao_adecuado(nao_pode_faze_lo, tecnica_construcao).
nao_adecuado(nao_pode_faze_lo, materiais_reciclados).
nao_adecuado(nao_pode_faze_lo, execucao_trabalhos_festas).
nao_adecuado(nao_pode_faze_lo, trabalhos_feltro).
nao_adecuado(nao_pode_faze_lo, pequenos_arranjos).
nao_adecuado(nao_pode_faze_lo, quadros).
nao_adecuado(nao_pode_faze_lo, trabalhos_decoracao_centro).
nao_adecuado(nao_pode_faze_lo, confeccao_sobremesas).
nao_adecuado(nao_pode_faze_lo, exercicios_conhecimento).
nao_adecuado(nao_pode_faze_lo, jogos_destreza_manual).
nao_adecuado(nao_pode_faze_lo, manicure).
nao_adecuado(nao_pode_faze_lo, maquilhagem).
nao_adecuado(nao_pode_faze_lo, utilizacao_instrumentos).
nao_adecuado(nao_pode_faze_lo, musicograma).
nao_adecuado(nao_pode_faze_lo, jogos_domino).
nao_adecuado(nao_pode_faze_lo, jogos_missako).
nao_adecuado(nao_pode_faze_lo, jogos_galo).
nao_adecuado(nao_pode_faze_lo, jogos_petanca).
nao_adecuado(nao_pode_faze_lo, jogos_bowling).

%9 Ler
%Alguma_dificultade (GRAVIDADE MEDIA)
nao_adecuado(ler_alguma_dificultade, exercicios_leitura_escrita).
nao_adecuado(ler_alguma_dificultade, leitura_textos_dramaticos).
nao_adecuado(ler_alguma_dificultade, interpretacao_personagens).
nao_adecuado(ler_alguma_dificultade, musicograma).
nao_adecuado(ler_alguma_dificultade, exercicios_compreensao_numerica).

%Nao_consegue (GRAVIDADE ALTA)
nao_adecuado(ler_nao_consegue, exercicios_leitura_escrita).
nao_adecuado(ler_nao_consegue, exercicios_compreensao_numerica).
nao_adecuado(ler_nao_consegue, musicograma).
nao_adecuado(ler_nao_consegue, leitura_textos_dramaticos).
nao_adecuado(ler_nao_consegue, interpretacao_personagens).
nao_adecuado(ler_nao_consegue, exercicios_conhecimento).
nao_adecuado(ler_nao_consegue, jogos_bingo).


%10 Escrever
%Alguma_dificultade (GRAVIDADE MEDIA)
nao_adecuado(escrever_alguma_dificultade, exercicios_leitura_escrita).
nao_adecuado(escrever_alguma_dificultade, exercicios_tarefas_verbais).
nao_adecuado(escrever_alguma_dificultade, interpretacao_personagens).
nao_adecuado(escrever_alguma_dificultade, musicograma).

%Nao_consegue (GRAVIDADE ALTA)
nao_adecuado(escrever_nao_consegue, exercicios_leitura_escrita).
nao_adecuado(escrever_nao_consegue, exercicios_tarefas_verbais).
nao_adecuado(escrever_nao_consegue, interpretacao_personagens).
nao_adecuado(escrever_nao_consegue, musicograma).
nao_adecuado(escrever_nao_consegue, jogos_bingo).
nao_adecuado(escrever_nao_consegue, exercicios_conhecimento).
nao_adecuado(escrever_nao_consegue, exercicios_compreensao_numerica).


%11 Mobilidade
%Alguma_dificuldade (GRAVIDADE MEDIA)
nao_adecuado(alguma_dificuldade, jogos_petanca).
nao_adecuado(alguma_dificuldade, jogos_bowling).
nao_adecuado(alguma_dificuldade, exercicios_aquecimento).
nao_adecuado(alguma_dificuldade, fisioterapia).

%Dependencia_total (GRAVIDADE ALTA)
nao_adecuado(dependencia_total, caminhadas).
nao_adecuado(dependencia_total, exercicios_aquecimento).
nao_adecuado(dependencia_total, fisioterapia).
nao_adecuado(dependencia_total, jogos_petanca).
nao_adecuado(dependencia_total, jogos_bowling).
nao_adecuado(dependencia_total, jogos_lencinho_adaptado).
nao_adecuado(dependencia_total, jogos_mimica).


%--------------------------------------------


% GOSTOS
gosto(teatro).
gosto(museu).
gosto(musica).
gosto(leitura).
gosto(grupo_recreativo).
gosto(arte).
gosto(desporto).
gosto(cozinha).
gosto(cozinha).
gosto(trabalhos_manuales).


%--------------------------------------------


% GOSTOS/ACTIVIDADE (PREFERENCIAS)
%1-Teatro
preferencia(teatro, leitura_textos_dramaticos).
relacionado(teatro, interpretacao_personagens).
relacionado(teatro, jogos_mimica).
%2-Museu
relacionado(museu, quadros). 
relacionado(museu, pequenos_arranjos).
relacionado(museu, caminhadas).
%3-Musica
relacionado(musica, karaoke).
relacionado(musica, musicograma).
relacionado(musica, adivinhar_musicas).
relacionado(musica, utilizacao_instrumentos). 
%4-Leitura
relacionado(leitura, leitura_textos_dramaticos). 
relacionado(leitura, exercicios_leitura_escrita).
relacionado(leitura, exercicios_tarefas_verbais).
%5-Grupo_recreativo
relacionado(grupo_recreativo, jogos_petanca). 
relacionado(grupo_recreativo, jogos_bowling). 
relacionado(grupo_recreativo, jogos_mimica).
relacionado(grupo_recreativo, jogos_bingo).
relacionado(grupo_recreativo, jogos_lencinho_adaptado).
relacionado(grupo_recreativo, execucao_trabalhos_festas).
relacionado(grupo_recreativo, trabalhos_decoracao_centro).
%6-Arte
relacionado(arte, quadros).
relacionado(arte, trabalhos_decoracao_centro).
relacionado(arte, trabalhos_feltro).
relacionado(arte, pequenos_arranjos).
%7-Desporto
relacionado(desporto, caminhadas).
relacionado(desporto, jogos_petanca).
relacionado(desporto, jogos_bowling).
%8-Cozinha
relacionado(cozinha, confeccao_sobremesas).
%9-Trabalhos_manuales
relacionado(trabalhos_manuales, tecnica_construcao).
relacionado(trabalhos_manuales, materiais_reciclados). 
relacionado(trabalhos_manuales, trabalhos_feltro). 
relacionado(trabalhos_manuales, pequenos_arranjos).
relacionado(trabalhos_manuales, quadros). 
relacionado(trabalhos_manuales, trabalhos_decoracao_centro).
relacionado(trabalhos_manuales, manicure).
relacionado(trabalhos_manuales, maquilhagem). 
relacionado(trabalhos_manuales, massagens). 


% REGLAS
pode_fazer_atividade(Doenca, Atividade) :-
    doenca(Doenca),                  % Verifica que la enfermedad existe
    atividade(Atividade),            % Verifica que la actividad existe
    \+ nao_pode(Doenca, Atividade).  % Verifica que no hay restricción para la actividad

atividades_nao_podem(Doenca, AtividadesNaoPodem) :-
    findall(Atividade, nao_pode(Doenca, Atividade), AtividadesNaoPodem).

atividades_podem(Doenca, AtividadesPodem) :-
    findall(Atividade, (atividade(Atividade), \+ nao_pode(Doenca, Atividade)), AtividadesPodem).