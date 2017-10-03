# Relatório de Requests

[![N|Solid](https://wiki.scn.sap.com/wiki/download/attachments/1710/ABAP%20Development.png?version=1&modificationDate=1446673897000&api=v2)](https://www.sap.com/brazil/developer.html)

Este tem como objeto prover uma visão mais clara das requests e em qual Ambiente estão seus log's mais recentes de atualização de transporte. Como um _plus_, foi implementada a funcionalidade de atualização de forma que seja possivel acompanhar o transporte de varias request's sem ter mudanças de telas.

  - Informações de transporte
  - Colunas dinâmicas
  - Log's de transporte
  - Link com detalhes de transporte
  
## Informações de Request

As tabelas `E070` e `E07T` sãso as principais utilizadas para acessar as informações referente a request, User, Tipo de Request (Ordem de workbench, Ordem customizing, Transporte de cópias, Desenvolvimento/correção, Reparação) e etc.

A partir dessas informações, os dados de log de transportes são recuperados usando a função `TR_READ_GLOBAL_INFO_OF_REQUEST`. Infelizmente isso inibe algums filtros que poderiam ser implementados, mas me retorno log's completos para o relatório.

```abap
      call function 'TR_READ_GLOBAL_INFO_OF_REQUEST'
        exporting
          iv_trkorr   = ls_e070-trkorr
          iv_dir_type = 'T'
          is_settings = settings
        importing
          es_cofile   = cofile.
  ```
## Layout do Relatório

Muitas informações estão sendo avalidas e estou aplicando conforme as necessidades.
