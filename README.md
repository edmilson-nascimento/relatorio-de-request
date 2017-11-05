# Cores em linhas de uma Report ALV (em construção)

[![N|Solid](https://wiki.scn.sap.com/wiki/download/attachments/1710/ABAP%20Development.png?version=1&modificationDate=1446673897000&api=v2)](https://www.sap.com/brazil/developer.html)


Visto a necessidade de criar um relatório para acompanhar o transporte de request's, eu utilizei como modelo a ideia que vi em um cliente e tentei melhorar a codificação. Hoje esse relatório me traz a lista das request's de acordo com a tela de seleção e posso atualizar a visão, tendo assim uma mostra do andamento do transporte. Para isso eu criei a classe local `lcl_report` com os seguintes métodos.

* public section
	* [initial](#initial)
	* [cria_tabela](#cria_tabela)
	* [get_data](#get_data)
	* [generate_output](#generate_output)

* protected
	* [on_link_click](#on_link_click)
	* [on_added_function](#on_added_function)

* private section
	* [limpar_dados](#limpar_dados)
	* [carrega_descricao](#carrega_descricao)
	* [seleciona_dados](#seleciona_dados)
	* [cria_coluna](#cria_coluna)
	* [monta_relatorio](#monta_relatorio)
	* [atualiza_atributos](#atualiza_atributos)
	* [set_text](#set_text)
  * [set_text_output](#set_text_output)
  * [link_click](#link_click)
  * [process](#process)
  * [change_tmscsys](#change_tmscsys)
  * [assign](#assign)
  * [get_data_refresh ](#get_data_refresh )
