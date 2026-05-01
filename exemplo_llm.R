# =======================================================================================
# mpiPnad - Testando LLM com dados resumidos do MPI/PNAD
# Autor: Pier De Maria
# Repositório: https://github.com/dpierf/mpiPnad
#
# Pré-requisito: instalar o pacote usando `pak::pak('dpierf/mpiPnad')`
# =======================================================================================


# ── 0. Dependências ───────────────────────────────────────────────────────────
install.packages('ellmer');      require(ellmer)
install.packages('data.table');  require(data.table)
install.packages('glue');        require(glue)
install.packages('scales');      require(scales)

pak::pak('dpierf/mpiPnad', upgrade = TRUE, dependencies = TRUE)
require(mpipnad)


# ── 1. Serialização ───────────────────────────────────────────────────────────
serializar_resumo <- function(resumo, grupos, dicts = NULL) {
  
  cols_grupo   <- intersect(grupos, names(resumo))
  cols_metrica <- intersect(c('H', 'A', 'MPI', 'P0', 'P1t', 'P1c'), names(resumo))
  k_val        <- unique(resumo$k)
  
  # Quais grupos têm dicionário disponível
  grupos_com_dict <- if (!is.null(dicts))
    intersect(cols_grupo, names(dicts))
  else
    character(0)
  
  linhas <- apply(resumo, 1, function(r) {
    grupo_str <- paste(
      sapply(cols_grupo, \(g) {
        val <- r[[g]]
        # Decodifica se tiver dicionário para este grupo
        if (g %in% grupos_com_dict) {
          label <- dicts[[g]][as.character(as.integer(val))]
          label <- if (!is.na(label)) label else val
        } else {
          label <- val
        }
        paste0(g, '=', label)
      }),
      collapse = ', '
    )
    
    metricas_str <- paste(
      sapply(cols_metrica, \(m) {
        val <- as.numeric(gsub(',', '.', r[[m]]))
        if (m == 'MPI')
          paste0(m, '=', round(val, 4))
        else
          paste0(m, '=', scales::percent(val, accuracy = 0.1))
      }),
      collapse = ', '
    )
    
    paste0('[', grupo_str, '] ', metricas_str)
  })
  
  # Descrição legível dos grupos para o modelo
  grupos_labels <- sapply(cols_grupo, \(g) {
    switch(g,
           ano       = 'ano',
           regiao    = 'região',
           uf        = 'unidade federativa',
           setor_dec = 'setor (urbano/rural)',
           area_dec  = 'área (metropolitana/interior)',
           sexo_dec  = 'sexo do chefe',
           raca      = 'raça/cor',
           periodo   = 'período histórico',
           arranjo_full = 'arranjo familiar',
           g  # fallback: nome da coluna
    )
  })
  
  paste0(
    'Corte de pobreza: k = ', scales::percent(k_val, accuracy = 1), '\n',
    'Recorte analítico: ', paste(grupos_labels, collapse = ' × '), '\n',
    'Observações: ', nrow(resumo), '\n\n',
    paste(linhas, collapse = '\n')
  )
}


# ── 2. Sistema prompt especializado ──────────────────────────────────────────
sistema_mpi <- function() {
"Você é um pesquisador sênior especializado em pobreza multidimensional
no Brasil, com domínio da metodologia Alkire-Foster, das pesquisas do OPHI
e do MPI-LA (Santos et al., 2015).

MÉTRICAS — definições exatas:
- H (incidência): proporção de pessoas identificadas como pobres
- A (intensidade): média das privações entre os pobres (share de indicadores)
- k (cutoff): corte de pobreza dual (share mínimo de privações para ser considerado pobre)
- MPI: índice ajustado de pobreza multidimensional, dado por MPI = H × A
- P0: incidência de pobreza de renda (headcount monetário)
- P1t: hiato de pobreza total (Foster-Greer-Thorbecke, α=1)
- P1c: hiato de pobreza condicional (apenas entre os pobres de renda)

CONTEXTO DA BASE:
- Fonte: PNAD Anual (1981–2015) e PNAD Contínua — Visita 1 (2016–2024)
- 2020–2021 excluídos: IBGE não divulgou os microdados nestes anos
- Deflator: IPCA (série 433/BCB), valores em BRL de dezembro de 2024
- Estrutura do MPI-LA: 5 dimensões, 14 indicadores
- Indicadores: D1–D3 (Educação), B1–B4 (Habitação/Serviços),
  V1–V2 (Trabalho), E1–E3 (Saúde), P1–P2 (Renda)

INSTRUÇÕES DE ESCRITA:
- Português acadêmico brasileiro, texto corrido, sem marcadores ou listas
- Decimais com vírgula (ex: 0,42 e não 0.42)
- Percentuais com uma casa decimal (ex: 73,9%)
- Mencione o corte k ao contextualizar os resultados
- Quando houver série temporal, identifique tendências, inflexões relevantes
  e compare com contexto de políticas públicas quando pertinente
- Quando houver grupos (UF, região, sexo, arranjo), compare entre grupos
- Não invente dados; narre apenas o que estiver nos dados fornecidos"
}


# ── 3. Função de LLM ──────────────────────────────────────────────────────────
llm_mpi <- function(
    resumo,
    grupos     = 'ano',
    tipo       = 'tendencia',
    contexto   = NULL,
    dicts      = NULL,
    provider   = 'groq',
    modelo     = NULL,
    max_tokens = 1000
) {
  
  # Verificação de credenciais
  if (provider == 'groq' && Sys.getenv('GROQ_API_KEY') == '') {
    stop(
      'GROQ_API_KEY não encontrada.\n',
      'Para usar o provider "groq":\n',
      '  1. Crie uma conta gratuita em console.groq.com\n',
      '  2. Gere uma API key em API Keys > Create Key\n',
      '  3. Adicione ao seu .Renviron:\n',
      '       usethis::edit_r_environ()\n',
      '       GROQ_API_KEY=gsk_xxxxxxxxxxxx\n',
      '  4. Reinicie o R (Ctrl+Shift+F10)\n',
      call. = FALSE
    )
  }

  if (provider == 'ollama') {
    ollama_ok <- tryCatch({
      resp <- httr2::request('http://localhost:11434') |>
        httr2::req_perform()
      TRUE
    }, error = function(e) FALSE)
    
    if (!ollama_ok) {
      stop(
        'Ollama não está em execução.\n',
        'Para usar o provider "ollama":\n',
        '  1. Instale o Ollama em ollama.com\n',
        '  2. Inicie o Ollama (ele aparecerá na bandeja do sistema)\n',
        '  3. Baixe um modelo pelo terminal:\n',
        '       ollama pull llama3.1:8b\n',
        '  4. Tente novamente\n',
        call. = FALSE
      )
    }
  }
  
  modelo_default <- list(
    groq      = 'llama-3.3-70b-versatile',
    ollama    = 'llama3.1:8b'
  )
  modelo <- modelo %||% modelo_default[[provider]]
  
  chat <- switch(provider,
                 groq = chat_groq(
                   system_prompt = sistema_mpi(),
                   model         = modelo,
                   params        = params(max_tokens = max_tokens),
                   echo          = FALSE
                 ),
                 ollama = chat_ollama(
                   system_prompt = sistema_mpi(),
                   model         = modelo,
                   params        = params(max_tokens = max_tokens),
                   echo          = FALSE
                 ),
                 stop('provider deve ser "groq" ou "ollama"')
  )
  
  instrucao_tipo <- switch(tipo,
                           tendencia  = 'Narre a evolução temporal dos indicadores, identificando
                                         tendências, rupturas e períodos relevantes.',
                           snapshot   = 'Descreva o estado atual (ou do ano mais recente disponível)
                                         dos indicadores, contextualizando sua magnitude.',
                           comparacao = 'Compare os grupos presentes nos dados, destacando
                                         desigualdades e padrões entre categorias.',
                           stop('tipo deve ser "tendencia", "snapshot" ou "comparacao"')
  )
  
  contexto_str <- if (!is.null(contexto))
    paste0('\nContexto adicional fornecido pelo pesquisador:\n', contexto, '\n')
  else
    ''
  
  prompt_usuario <- glue::glue(
    '{instrucao_tipo}
     {contexto_str}
     Dados:
     {serializar_resumo(resumo, grupos, dicts)}

     Escreva 1 a 2 parágrafos de resultados adequados para um artigo acadêmico.'
  )
  
  chat$chat(prompt_usuario)
}


# ── 4. Testando o LLM ─────────────────────────────────────────────────────────

#Carregando a base (ver arquivo 'pipeline.R')
resultados <- create_mpi()

#Criando o resumo de dados
grupos <- c('ano','regiao')
k      <- 0.33

mpi_summary <- resume_mpi(
  dt       = resultados$mpi_pnad,
  grupos   = grupos,
  k_output = k
)


#Exemplo de LLM para análise de tendência para Brasil (1981-2024)
cat(llm_mpi(
  resumo   = mpi_summary[regiao == 'Total',],
  grupos   = grupos,
  tipo     = 'tendencia',
  dicts    = resultados$dicts,
  contexto = 'Marcos relevantes: processo hiperinflacionário (1986-1994), Plano Real (1994), Real forte (1995-1998),
              criação do Bolsa Família (2003), crescimento econômico do ciclo de commodities (2003–2010), crise fiscal
              e Emenda do Teto de Gastos (2015-2016), pandemia COVID-19 com suspensão do módulo de habitação (2020–2021
              excluídos da base). Mudanças substanciais de espectro político em 1990, 2003, 2016, 2023. Dados disponíveis
              a nível nacional (`Total`), por ano.'
))

#Exemplo de LLM para análise de tendência para Brasil (1981-2024)
cat(llm_mpi(
  resumo   = mpi_summary[ano == 2024,],
  grupos   = grupos,
  tipo     = 'comparacao',
  dicts    = resultados$dicts,
  contexto = 'Comparação regional do MPI-LA no Brasil em 2024, com corte k=33%. Contexto estrutural relevante: (1) Norte e Nordeste 
              historicamente apresentam os maiores índices de pobreza multidimensional do país; (2) Sul e Sudeste concentram menor 
              incidência e intensidade de privações; (3)  Centro-Oeste tem perfil intermediário, com heterogeneidade interna entre 
              áreas urbanas e rurais; (4) O processo de convergência regional acelerou entre 2003 e 2015, desacelerou com a crise 
              fiscal de 2016 e retomou parcialmente após 2022; (5) Diferenças regionais refletem desigualdades estruturais em
              educação, saneamento e mercado de trabalho'
))
