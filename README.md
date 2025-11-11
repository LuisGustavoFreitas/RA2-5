# Sistema de Inventário — Haskell

**Instituição:** PUCPR  
**Disciplina:** Resolução de Problemas Estruturados em Computação  
**Professor:** Frank Coelho de Alcantara  
**Alunos:** 
- Gabriel Calado da Silva Castro | @castruti  
- Luis Gustavo Freitas Kulzer | @LuisGustavoFreitas


---

##  Descrição do Projeto

Este projeto implementa um **Sistema de Inventário** utilizando a linguagem **Haskell**, com suporte para:

- Cadastro de itens
- Remoção e atualização de estoque
- Registro de auditoria (histórico de operações)
- Relatório de erros e análise de itens mais movimentados

O sistema persiste os dados em arquivos locais:

- `Inventario.dat` | Armazena os itens cadastrados   
- `Auditoria.log`  | Registra todas as operações e erros 

---

## Conceitos Utilizados

- **Leitura e escrita** em arquivos
- **Tratamento de erros**
- Formatação e filtragem de logs
- Processamento de listas e recursão

---

##  Funcionalidades

| Comando | Descrição | Exemplo |
|--------|----------|---------|
| `add <id> <nome> <quantidade> <categoria>` | Adiciona um item ao inventário | `add I001 Teclado 10 Perifericos` |
| `remove <id> <quantidade>` | Remove unidades de um item | `remove I001 3` |
| `update <id> <nova_quantidade>` | Atualiza a quantidade total do item | `update I001 15` |
| `list` | Lista todos os itens cadastrados | `list` |
| `report` | Mostra erros registrados e itens mais movimentados | `report` |
| `help` | Mostra todos os comandos disponíveis | `help` |
| `exit` | Encerra o programa | `exit` |

---

## Fluxo de Execução

```haskell
add I001 Teclado 10 Perifericos
Item adicionado.
```

```haskell
remove I001 15
Estoque insuficiente: possui 10 unidades.
```

```haskell
report
Erros:
(LogEntry mostrando tentativa inválida de remoção)
Itens mais movimentados:
("I001", 1)
```

---

## Como Executar no **OnlineGDB**

1. Acesse o link:
   **https://www.onlinegdb.com/online_haskell_compiler**
2. Crie um novo projeto.
3. Copie o conteúdo dos arquivos `.hs` do projeto para o editor.
4. Clique em **Run** para executar.

---

### Estrutura do Projeto

| Arquivo | Função | Detalhes |
|--------|----------|---------|
| **main.hs** | Controla o fluxo do programa | Recebe comandos, atualiza arquivos e exibe respostas |
| **Types.hs** | Define os tipos principais | Item, LogEntry, StatusLog, etc. |
| **Logic.hs** | Onde ficam as regras do sistema | Valida ações e atualiza inventário |
| **Reports.hs** | Gera relatórios e análises | Usa o log para listar erros e itens mais movimentados |
| **Inventario.dat** | Armazenamento persistente | Contém o inventário salvo (Gerado automaticamente) |
| **Auditoria.log** | Histórico de eventos | Registra sucesso ou falha de todas operações (Gerado automaticamente) |

