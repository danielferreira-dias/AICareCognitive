# Projeto AICARE

Este é o projeto AICARE, uma aplicação projetada para diagnóstico e recomendação de atividades para pacientes com doenças neurodegenerativas como Alzheimer e Parkinson, utilizando algoritmos de aprendizado de máquina.

## Estrutura do Projeto

O projeto está organizado nas seguintes pastas:

## Descrição das Pastas

- **DATA**: Contém os datasets utilizados para treinar os modelos, um para Parkinson e outro para Alzheimer, juntamente com um arquivo README explicativo sobre os mesmos.
  
- **docs**: Aqui está o relatório final do projeto, onde é descrito em detalhes o enfoque, resultados e conclusões do trabalho realizado.

- **frontend**: Contém o código fonte do frontend da aplicação, desenvolvido em Angular.

- **notebooks**: Notebooks principais: CarlosParkinson e PedroAlzeimer.
                 Nesta pasta estão os notebooks usados para análise de dados, processamento, técnicas de oversampling/undersampling e treinamento dos modelos. Também inclui os resultados dos modelos finais, Random Forest para Parkinson e Alzheimer.
                 E também os cadernos de todos os membros da equipa com os seus testes de diferentes modelos e técnicas.

- **src**: Contém o código do backend, desenvolvido com FastAPI. Esta pasta inclui o código da API, modelos de dados e os serviços necessários para a interação com o frontend.

- **Dockerfile**: Arquivo de configuração para criar e rodar a aplicação no Docker.

## Como Executar o Projeto

### Usando Docker

Para rodar o backend usando Docker, siga os seguintes passos:

1. Navegue até a pasta raiz do projeto, onde o `Dockerfile` está localizado.

2. Construa a imagem Docker:
    ```bash
    docker build -t ai-care .
    ```

3. Execute o contêiner Docker:
    ```bash
    docker run -d -p 8000:8000 ai-care
    ```

Isso iniciará a API do FastAPI no contêiner Docker na porta 8000.

### Backend sem Docker

1. Navegue até a pasta `src`.
2. Instale as dependências:
    ```bash
    pip install -r requirements.txt
    ```
3. Execute o servidor FastAPI:
    ```bash
    uvicorn app.main:app --reload
    ```

### Frontend

1. Navegue até a pasta `frontend`.
2. Instale as dependências:
    ```bash
    npm install
    ```
3. Execute o servidor de desenvolvimento:
    ```bash
    ng serve
    ```

## Requisitos

- Python 3.8+
- Node.js
- Angular CLI
- Docker
