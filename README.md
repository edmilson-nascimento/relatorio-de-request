# Relatório de request's (em construção)

![Static Badge](https://img.shields.io/badge/development-abap-blue)
![GitHub commit activity (branch)](https://img.shields.io/github/commit-activity/t/edmilson-nascimento/abap-7.4)


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
 foi escolhida apenas por ser relacionada ao modulo que eu tratava quando desenvolvi a solução.

## public section ##
Métodos da sessão publica.
### initial ###
Este tem como objetivo configurar os ambientes que serão exibidos no inicio do relatório e automaticamente, tambem as colunas que serão exibidas.
```abap
method initial .

    data:
      ls_status   type ty_r_status,
      ls_sysnam   type ty_r_sysnam,
      ls_tmscsys  type ty_tmscsys,
      opt_list    type sscr_opt_list,
      ass         type sscr_ass,
      restriction type sscr_restrict.

    refresh:
      ambiente .

     select domnam sysnam limbo
       into table t_tmscsys
       from tmscsys.
 
     if sy-subrc eq 0 .
 
       loop at t_tmscsys into ls_tmscsys .
 
         ls_sysnam-sign = 'I' .
         ls_sysnam-option = 'EQ' .
         ls_sysnam-low = ls_tmscsys-sysnam .
         append ls_sysnam to ambiente .
         clear  ls_sysnam .
 
       endloop.
 
       sort ambiente ascending by low .
 
     endif.
     
    opt_list-name       = 'OBJECTKEY1'.
    opt_list-options-eq = abap_on .
    append opt_list to restriction-opt_list_tab.

    ass-kind    = 'S'.
    ass-name    = 'S_AMB'.
    ass-sg_main = 'I'.
    ass-sg_addy = abap_off .
    ass-op_main = 'OBJECTKEY1'.
    append ass to restriction-ass_tab.

    call function 'SELECT_OPTIONS_RESTRICT'
      exporting
*       program                =
        restriction            = restriction
*       db                     = SPACE
      exceptions
        too_late               = 1
        repeated               = 2
        selopt_without_options = 3
        selopt_without_signs   = 4
        invalid_sign           = 5
        empty_option_list      = 6
        invalid_kind           = 7
        repeated_kind_a        = 8
        others                 = 9.

    if sy-subrc eq 0.
    else .
      message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.


  endmethod .                    "initial

  method get_data.
```
### cria_tabela ###
Depois de definidos os Ambientes, agora será criada uma tabela dinâmica. Desta forma temos as colunas de acordo com os ambientes que são informados na tela de seleção. Ao inves de uma `tabela interna`, é criada uma referência que pode ser acessada vida `field-symbols`.
```abap
method cria_tabela .

    data:
      lt_fieldcat type lvc_t_fcat,
      ls_tmscsys  type ty_tmscsys,
      fieldname   type lvc_fname.

    field-symbols:
      <fs_line>  type any .

    me->change_tmscsys( ambiente = ambiente ).

    me->cria_coluna(
      exporting
        fieldname    = 'TRKORR'
        outputlen    = 20
        ref_table    = 'E071'
        ref_field    = 'TRKORR'
      changing
        fieldcatalog = lt_fieldcat
    ).
    me->cria_coluna(
      exporting
        fieldname    = 'DESCREQ'
        outputlen    = 60
        ref_table    = 'E07T'
        ref_field    = 'AS4TEXT'
      changing
        fieldcatalog = lt_fieldcat
    ).
    me->cria_coluna(
      exporting
        fieldname    = 'AS4USER'
        outputlen    = 12
        ref_table    = 'E070'
        ref_field    = 'AS4USER'
      changing
        fieldcatalog = lt_fieldcat
    ).
    me->cria_coluna(
      exporting
        fieldname    = 'AS4DATE'
        outputlen    = 8
        ref_table    = 'E070'
        ref_field    = 'AS4DATE'
      changing
        fieldcatalog = lt_fieldcat
    ).
    me->cria_coluna(
      exporting
        fieldname    = 'AS4TIME'
        outputlen    = 6
        ref_table    = 'E070'
        ref_field    = 'AS4TIME'
      changing
        fieldcatalog = lt_fieldcat
    ).
    me->cria_coluna(
      exporting
        fieldname    = 'TRFUNCTION'
        outputlen    = 42
        ref_table    = ''
        ref_field    = ''
*       ref_table    = 'E070'
*       ref_field    = 'TRFUNCTION'
      changing
        fieldcatalog = lt_fieldcat
    ).
    me->cria_coluna(
      exporting
        fieldname    = 'TRSTATUS'
        outputlen    = 1
        ref_table    = 'E070'
        ref_field    = 'TRSTATUS'
      changing
        fieldcatalog = lt_fieldcat
    ).
    me->cria_coluna(
      exporting
        fieldname    = 'KORRDEV'
        outputlen    = 4
        ref_table    = 'E070'
        ref_field    = 'KORRDEV'
      changing
        fieldcatalog = lt_fieldcat
    ).
    me->cria_coluna(
      exporting
        fieldname    = 'DTECP'
        outputlen    = 10
        ref_table    = 'SYST'
        ref_field    = 'DATUM'
      changing
        fieldcatalog = lt_fieldcat
    ).
    me->cria_coluna(
      exporting
        fieldname    = 'TMECP'
        outputlen    = 10
        ref_table    = 'SYST'
        ref_field    = 'UZEIT'
      changing
        fieldcatalog = lt_fieldcat
    ).


    loop at t_tmscsys into ls_tmscsys .

      fieldname = ls_tmscsys-sysnam .

      me->cria_coluna(
        exporting
          fieldname    = fieldname
          outputlen    = 10
          ref_table    = ''
          ref_field    = ''
        changing
          fieldcatalog = lt_fieldcat
      ).

    endloop.


    call method cl_alv_table_create=>create_dynamic_table
      exporting
*       i_style_table             =     " Add Style Table
        it_fieldcatalog           = lt_fieldcat
*       i_length_in_byte          =     " Boolean Variable (X=True, Space=False)
      importing
*       ep_table                  = new_table
        ep_table                  = table
*       e_style_fname             =     " ALV Control: Field Name of Internal Table Field
      exceptions
        generate_subpool_dir_full = 1
        others                    = 2.

    if sy-subrc eq 0 .

    else .

    endif.

  endmethod .                    "cria_tabela
  ```
