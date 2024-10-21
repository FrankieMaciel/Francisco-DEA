Bem-vindo ao **FRANCISCO**, uma ferramenta desenvolvida para auxiliar na análise de dados temporais e cálculo de dimensões fractais. Este guia fornecerá uma visão geral das principais funcionalidades do projeto.

# FAQ (Perguntas Frequentes)
## Como carregar meus dados?

Use o Botão "Browse" para selecionar um arquivo ou "Selecionar a pasta do seu dataset" para carregar uma pasta inteira.

## Quais formatos de arquivo são suportados?

* DMU do APACHE BENCH:
Dados extraídos de testes de desempenho realizados com o Apache Bench, uma ferramenta usada para medir a performance de servidores web. As DMUs (Unidades de Decisão) representam os resultados dos testes, como tempo de resposta ou requisições por segundo.

* DMU do IPERF:
Dados provenientes de testes de largura de banda de rede, realizados com o Iperf. As DMUs são geradas com base na taxa de transferência.

* DMU Numérica:
Formato genérico que aceita dados numéricos para qualquer tipo de análise, permitindo flexibilidade na criação de DMUs.

* Tabela:
Diferentemente dos outros formatos, o formato "Tabela" é utilizado especificamente para carregar tabelas de análise DEA que já foram geradas previamente pelo programa.

## Como funciona as janelas de tempo?
O tamanho de uma janela de tempo refere-se ao intervalo de dados utilizado para analisar uma série temporal. Esse valor pode variar entre 60 e o tamanho máximo da série temporal analisada. Janelas de tempo menores geram uma maior quantidade de séries temporais disponíveis para análise. No entanto, é importante notar que uma quantidade maior de séries temporais pode exigir mais tempo de processamento.

---
# Configurações da tabela
## 1. Carregamento de Dados

* **Seleção de Arquivo (Botão Browse):** Carregue um ou mais arquivos de dados para análise a partir do seu computador.
  
* **Seleção de Pasta:** Selecione uma pasta contendo vários arquivos de dados.

##  2. Configuração da Análise Temporal

* **Tamanho da Janela Temporal:** Defina a quantidade de unidades de tempo para a análise usando o controle deslizante.

* **Janela Temporal a Ser Exibida:** Especifique a parte dos dados temporais que deseja visualizar e analisar.

## 3. Tipo de Arquivo

* **Seleção do Tipo de Arquivo:** Escolha o tipo de arquivo para ajustar a análise ao formato do dataset (ex.: "DMU do APACHE BENCH").

## 4. Configuração das Variáveis

* **Inserção das Variáveis:** As váriaveis que vão ser geradas pelo programa, se nenhuma for escolhida todas serão selecionadas por padrão.

## 5. Métodos de Cálculo de Dimensão Fractal

* **Seleção do Método:** Escolha o método de cálculo de dimensão fractal a ser aplicado, como o "Madogram".

## 6. Execução da Análise

* **Gerar Tabela de Resultados:** Após configurar as opções, clique em "Gerar Tabela" para executar a análise e gerar a tabela com os resultados.

# Configurações da análise DEA
## Seleção de inputs
Escolha quais variáveis da tabela serão interpretados como entrada (input) pelo programa. Se nenhuma for escolhida, a dimensão fractal será escolhida por padrão.
## Seleção de outputs
Escolha quais variáveis da tabela serão interpretados como saida (output) pelo programa. Se nunhuma for escolhida, todas as váriaveis, menos a dimensão fractal, será escolhida por padrão.
## Modelo DEA

Escolha o modelo DEA a ser utilizado nas análises. Os modelos disponíveis são:

* **CCR:** (Charnes, Cooper, Rhodes) - Modelo clássico que assume retornos constantes à escala.
* **BCC:** (Banker, Charnes, Cooper) - Modelo que permite retornos variáveis à escala, focando na eficiência técnica pura.
* **SCCR:** Modelo de super-eficiência baseado no CCR, que avalia DMUs eficientes além da fronteira de eficiência.
* **SBM:** (Slacks-Based Measure) - Modelo que considera diretamente as folgas nos insumos e produtos, proporcionando uma medida mais robusta da eficiência.

## Orientação da Análise DEA

Defina a orientação da análise DEA, que pode ser baseada em insumos ou produtos:

* **IN:** Foca na minimização dos insumos utilizados para alcançar um determinado nível de produção.
* **OUT:** Busca maximizar a produção com os insumos disponíveis, otimizando a eficiência em termos de resultados.

