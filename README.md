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
- `Registro_Atividade.log`  | Registra todas as operações e erros 

---

## Conceitos Utilizados

- **Leitura e escrita** em arquivos
- **Tratamento de erros**
- Formatação e filtragem de logs
- Processamento de listas e recursão

---

## Estrutura do Projeto

```haskell
projeto/
├── main.hs                   # Recebe comandos, atualiza arquivos e exibe respostas  
├── Types.hs                  # Item, LogEntry, StatusLog, etc.  
├── Logic.hs                  # Valida ações e atualiza inventário  
├── Reports.hs                # Usa o log para listar erros e itens mais movimentados  
├── Inventario.dat            # Contém o inventário salvo (Gerado automaticamente)  
├── Registro_Atividade.log    # Registra sucesso ou falha de todas operações (Gerado automaticamente)  
└── README.md                 # Este documento  
```

---

##  Comandos 

### Adicionar Item

```haskell
add <id> <nome> <quantidade> <categoria> 
```

Exemplo: `add I001 Teclado 10 Periféricos` 

### Remover Item

```haskell
remove <id> <quantidade>
```

Exemplo: `remove I001 3`

### Atualizar Quantidade

```haskell
update <id> <nova_quantidade>
```

Exemplo: `update I001 15`

### Deletar Item

```haskell
delete <id>
```

Exemplo: `delete I001`

### Listar Inventário

```haskell
list
```

### Relatório

```haskell
report
```

### Ajuda

```haskell
help
```

### Sair

```haskell
exit
```

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

## Cenários de Teste

### Cenário 1: Persistência de Estado (Sucesso)  
**Objetivo**: Verificar se o estado do inventário é persistido corretamente entre execuções.  

**Passos:**
  1. Iniciar o programa **sem os arquivos `Inventario.dat` e `Registro_Atividade.log`**
  2. Executar os seguintes comandos:
     ```haskell
        add I001 Monitor 2 Perifericos
        add I002 Teclado 5 Perifericos
        add I003 Lapis 10 Material Escolar
     ```
  3. Sair do programa com o `exit`.
  4. Verificar se os arquivos foram criados:
   - `Inventario.dat`
   - `Registro_Atividade.log`
  5. Reiniciar o programa.
  6. Executar o comando `list` e verificar se **os mesmos 3 itens** aparecem.  
  **Resultado esperado:**   
  ✔ Arquivos Inventario.dat e Auditoria.log criados  
  ✔ Ao reiniciar, o sistema carrega os 3 itens  
  ✔ Comando list exibe os 3 itens adicionados

---

### **Cenário 2 – Erro de Lógica (Estoque Insuficiente)**  
**Objetivo**: Verificar se o sistema trata corretamente a tentativa de remover mais itens do que há em estoque.

**Passos:**
  1. Adicionar um item com **10 unidades**, por exemplo:
     ```haskell
        add I001 Lapis 10 Material Escolar
     ```
  2. Tentar remover **15 unidades**:
     ```haskell
        remove I001 15
     ```
  3. O sistema deve exibir uma mensagem clara de erro:
     ```haskell
        Estoque insuficiente: tem 10 unidades
     ```
  4. Verificar se o arquivo `Inventario.dat` ainda registra **10 unidades** do item.
  5. Verificar se o arquivo `Registro_Atividade.log` contém uma entrada com:
     - Ação: `remove I001 15`
     - Status: `Falha`
     - Detalhes informando o estoque insuficiente.  
  **Resultado esperado:**  
     ✔ Estado do inventário permanece inalterado  
     ✔ Log registra adequadamente a falha

---

### **Cenário 3 – Geração de Relatório de Erros**
**Objetivo**: Verificar se o relatório exibe corretamente as entradas de log de falhas.

**Passos:**
   1. Após concluir o Cenário 2, executar:
       ```haskell
        report
      ```
   2. Validar se o relatório exibe:
      - A lista de erros gerados (`logsDeErro`).
      - Incluindo a tentativa falha de remover 15 unidades do item  
      **Resultado esperado:**   
          ✔ O relatório mostra claramente todos os erros registrados no sistema.

---

## Como Executar no **OnlineGDB**

1. Acesse o link:
   **https://www.onlinegdb.com/online_haskell_compiler**
2. Crie um novo projeto.
3. Copie o conteúdo dos arquivos `.hs` do projeto para o editor.
4. Clique em **Run** para executar.

---
