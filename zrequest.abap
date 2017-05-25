*&---------------------------------------------------------------------*
*& Report zteste
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zteste message-id >0 ..


*&---------------------------------------------------------------------*
*& Report  ZREQUEST
*&
*&---------------------------------------------------------------------*
*& 1 - Atribuição de valores para paramantro de ambiente
*& 2 - Alteração na forma de atribuição dos faróis de transpote
*&   -> Verificar se existe algum log de transporte (icon empty)
*&   -> Verificar se existe algum log de error (icon red)
*&   -> Verificar se existe algum log de warning (icon yellow)
*&   -> Verificar se existe algum log do id Exportação (ambiente de origem,
*&      supostamente o ambiente de qualidade (icon green)
*&   -> Verificar se existe algum log do id Importação (icon green)
*& 3 - Alteração nomeclatura de variaveis
*& 4 - Atualização de declarações de tabelas de tipos
*& 4 - Alteração do ALV de exibição com opção de refresh (em desenvol)



* obs: Form monta_relatorio - verificar se variaveis globais podem ser locais
* obs: Form f_cria_tabela - verificar nomeclatura
* obs: Verificar ordenação por: data, hora e request
*&---------------------------------------------------------------------*

*report  zrequest  no standard page heading line-size 200 message-id >0 .

*--------------------------------------------------------------------*
*- Includes
*--------------------------------------------------------------------*
include <icon>.

*--------------------------------------------------------------------*
*- Tipos SAP
*--------------------------------------------------------------------*
type-pools:
  vrm, slis, ctslg, kkblo, icon, sym .

*--------------------------------------------------------------------*
*- Tabelas
*--------------------------------------------------------------------*
tables:
  e070, trtarget.

*--------------------------------------------------------------------*
*- Tipos
*--------------------------------------------------------------------*
types:
  begin of ty_tmscsys,
    domnam like tmscsys-domnam,
    sysnam like tmscsys-sysnam,
    limbo  like tmscsys-limbo,
  end of ty_tmscsys,

  tmscsys_tab type table of ty_tmscsys,

  begin of ty_status,
    status type trstatus,
    descr  type c length 60,
  end of ty_status,

  begin of ty_tipo,
    tipo type trfunction,
    desc type c length 42,
  end of ty_tipo.

field-symbols:
  <fs_table> type standard table .

*--------------------------------------------------------------------*
*- Declarações Globais
*--------------------------------------------------------------------*
data:
  e_settings      type          ctslg_settings,
  gs_cofile       type          ctslg_cofile,
  e_systems       type          ctslg_systems,
  gt_alv_layout   type          slis_layout_alv,
  gt_alv_print    type          slis_print_alv,
  gt_alv_sort     type          slis_t_sortinfo_alv,
  gs_alv_sort     like line of  gt_alv_sort,
  gt_alv_fieldcat type          slis_t_fieldcat_alv,
  gs_alv_fieldcat like line of  gt_alv_fieldcat,
  gt_fieldcatalog type          lvc_t_fcat,
  gs_fieldcatalog like line of  gt_fieldcatalog,
  gt_new_table    type ref to   data,
  gs_new_line     type ref to   data,
  gs_layout       type          slis_layout_alv,
  gt_fieldcat     type          slis_t_fieldcat_alv,
  gs_fieldcat     like line of  gt_fieldcat,
  it_tmscsys      type table of ty_tmscsys,
  wa_tmscsys      type          ty_tmscsys.

*--------------------------------------------------------------------*
** Variáveis Globais do programa
*--------------------------------------------------------------------*
data:
  v_listheader  type slis_listheader, " Cabeçalho
  v_alv_spos    type n length 2,      " Posição
  wc_cont       type n length 10 .
*  v_field       type lvc_fname,
*  v_long_text   type scrtext_l,
*  v_medium_text type scrtext_m,
*  v_short_text  type scrtext_s.

data:
  gt_e070    type table of e070,
  gs_e070    type          e070,
  gt_e070_2  type table of e070,
  gs_e070_2  type          e070,
  gt_e071    type table of e071,
  gs_e071    type          e071,
  gt_e071_2  type table of e071,
  gs_e071_2  type          e071,
  gt_e07t    type table of e07t,
  gs_e07t    type          e07t,
  gt_status  type table of ty_status,
  gs_status  type          ty_status,
  gt_tipo    type table of ty_tipo,
  gs_tipo    type          ty_tipo,

  gs_steps   type ctslg_step,
  gs_actions type ctslg_action.


*----------------------------------------------------------------------*
*       CLASS lcl_report DEFINITION
*----------------------------------------------------------------------*
class lcl_report definition.

  public section.

    types:
      begin of ty_tmscsys,
        domnam type tmscsys-domnam,
        sysnam type tmscsys-sysnam,
        limbo  type tmscsys-limbo,
      end of ty_tmscsys,

      tmscsys_tab type table of ty_tmscsys.

    data:
      lo_table   type ref to cl_salv_table,
      lo_events  type ref to cl_salv_events_table,
*      lo_columns type ref to cl_salv_columns_table,
*      lo_column  type ref to cl_salv_column_list,
      lo_display type ref to cl_salv_display_settings,
      lo_sorts   type ref to cl_salv_sorts.

    methods get_data .

    methods generate_output
      importing
        !t_tmscsys type tmscsys_tab .


    class-methods on_before_salv_function         " BEFORE_SALV_FUNCTION
      for event if_salv_events_functions~before_salv_function
                  of cl_salv_events_table
      importing e_salv_function.

    class-methods on_after_salv_function          " AFTER_SALV_FUNCTION
      for event if_salv_events_functions~before_salv_function
                  of cl_salv_events_table
      importing e_salv_function.

    class-methods on_added_function               " ADDED_FUNCTION
      for event if_salv_events_functions~added_function
                  of cl_salv_events_table
      importing e_salv_function.

    class-methods on_top_of_page                  " TOP_OF_PAGE
      for event if_salv_events_list~top_of_page
                  of cl_salv_events_table
      importing r_top_of_page
                  page
                  table_index.

    class-methods on_end_of_page                  " END_OF_PAGE
      for event if_salv_events_list~end_of_page
                  of cl_salv_events_table
      importing r_end_of_page
                  page.

    class-methods on_double_click                 " DOUBLE_CLICK
      for event if_salv_events_actions_table~double_click
                  of cl_salv_events_table
      importing row
                  column.

    class-methods on_link_click                   " LINK_CLICK
      for event if_salv_events_actions_table~link_click
                  of cl_salv_events_table
      importing row
                  column.


  protected section .

  private section .

    methods set_text
      importing
        !i_field       type lvc_fname
        !i_long_text   type scrtext_l
        !i_medium_text type scrtext_m
        !i_short_text  type scrtext_s
      changing
        !c_columns     type ref to cl_salv_columns_table
        !c_column      type ref to cl_salv_column_list.

    methods set_text_output
      importing
        !t_tmscsys type tmscsys_tab
      changing
        !table     type ref to cl_salv_table .


endclass.                    "lcl_report DEFINITION


**--------------------------------------------------------------------*
**- Objetos
**--------------------------------------------------------------------*
*data: lo_report     type ref to   lcl_report .
*
*&---------------------------------------------------------------------*
*** Tela de seleção
*&---------------------------------------------------------------------*
selection-screen begin of block b1 with frame title text-001.

select-options: s_amb for trtarget-tarsystem no intervals obligatory,
                p_ordem for  e070-trkorr,
                p_tipo  for  e070-trfunction,
                p_stat  for  e070-trstatus,
                p_categ for  e070-korrdev,
                p_user  for  e070-as4user,
                p_data  for  e070-as4date,
                s_dtecp for  e070-as4date.
selection-screen end of block b1.

*--------------------------------------------------------------------*
*- Eventos
*--------------------------------------------------------------------*
initialization.

  clear: p_stat, p_stat[].
  move 'I'  to p_stat-sign.
  move 'EQ' to p_stat-option.
  move 'R'  to p_stat-low.
  append p_stat.

  clear: p_data, p_data[].
  move 'I'      to p_data-sign.
  move 'EQ'     to p_data-option.
  move sy-datum to p_data-low.
  append p_data.

  select domnam sysnam limbo
    into table it_tmscsys
    from tmscsys.
  if sy-subrc eq 0 .
    loop at it_tmscsys into wa_tmscsys .
      s_amb-sign = 'I' .
      s_amb-option = 'EQ' .
      s_amb-low = wa_tmscsys-sysnam .
      append s_amb .
    endloop.
    sort s_amb ascending by low .
  endif.


start-of-selection .

  data: lo_report type ref to lcl_report.

  create object lo_report.

  lo_report->get_data( ).

  lo_report->generate_output( t_tmscsys = it_tmscsys ).


end-of-selection.

*&---------------------------------------------------------------------*
*&      Form  f_limpa_dados
*&---------------------------------------------------------------------*
*       Limpa dados
*----------------------------------------------------------------------*
form f_limpa_dados .

  clear: gs_e070,
         gs_e070_2,
         gs_e071,
         gs_e071_2,
         gs_e07t,
         gs_status.

  refresh: gt_e070,
           gt_e070_2,
           gt_e071,
           gt_e071_2,
           gt_e07t,
           gt_status.

endform.                    " f_limpa_dados
*&---------------------------------------------------------------------*
*&      Form  f_carrega_descricao
*&---------------------------------------------------------------------*
form f_carrega_descricao .

  data: lt_t_list type table of vrm_value,
        ls_t_list type          vrm_value.

  call function 'FICO_DOMAIN_VALUES_GET'
    exporting
      i_table_name = 'E070'
      i_field_name = 'TRSTATUS'
      i_langu      = sy-langu
    importing
      e_t_list     = lt_t_list.

  loop at lt_t_list into ls_t_list.

    move: ls_t_list-key  to gs_status-status,
          ls_t_list-text to gs_status-descr.

    append gs_status to gt_status.
    clear  gs_status .
  endloop.

  sort gt_status ascending by status.

  refresh lt_t_list .
  clear   ls_t_list .

  call function 'FICO_DOMAIN_VALUES_GET'
    exporting
      i_table_name = 'E070'
      i_field_name = 'TRFUNCTION'
      i_langu      = sy-langu
    importing
      e_t_list     = lt_t_list.

  loop at lt_t_list into ls_t_list .

    move: ls_t_list-key  to gs_tipo-tipo,
          ls_t_list-text to gs_tipo-desc.

    append gs_tipo to gt_tipo .
    clear  gs_tipo .
  endloop.

  sort gt_tipo ascending by tipo.

endform.                    " f_carrega_descricao

*&---------------------------------------------------------------------*
*&      Form  f_seleciona_dados
*&---------------------------------------------------------------------*
*       Seleciona dados
*----------------------------------------------------------------------*
form f_seleciona_dados .

*** Guarda as ordens
  select *
    from e070
    into table gt_e070
   where trkorr     in p_ordem
     and trfunction in p_tipo
     and trstatus   in p_stat
     and korrdev    in p_categ
     and as4user    in p_user
     and as4date    in p_data
     and strkorr    eq space.
  if sy-subrc ne 0 .
    message s000 with 'Não há dados para seleção'.
    stop.
  endif.

* Seleciona as descrições da request
  if not gt_e070[] is initial.
    select *
      from e07t
      into table gt_e07t
       for all entries in gt_e070
     where trkorr eq gt_e070-trkorr
       and ( langu  eq 'PT' or langu  eq 'EN' ).
  endif.

endform.                    " f_seleciona_dados
*&---------------------------------------------------------------------*
*&      Form  MONTA_QUEBRA_SUBTOTAL
*&---------------------------------------------------------------------*
*       Monta quebra
*----------------------------------------------------------------------*
form monta_quebra_subtotal .

  perform seleciona_quebra_subtotal using:
        'AS4DATE'  '<fs_table>'  'X' ''  ''   'X',
        'AS4TIME'  '<fs_table>'  'X' ''  ''   'X',
        'TRKORR'   '<fs_table>'  'X' ''  ''   'X',
        'DESCREQ'  '<fs_table>'  'X' ''  ''   'X'.
endform.                    " MONTA_QUEBRA_SUBTOTAL
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_QUEBRA_SUBTOTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0433   text
*      -->P_C_TPRINT  text
*      -->P_C_X  text
*      -->P_0436   text
*      -->P_0437   text
*      -->P_C_X  text
*----------------------------------------------------------------------*
form seleciona_quebra_subtotal  using   i_fieldname   type c  " 1
                                        i_tabname     type c  " 2
                                        i_up          type c  " 3
                                        i_down        type c  " 4
                                        i_subtot      type c  " 5
                                        i_group       type c. " 6
  v_alv_spos = v_alv_spos + 1.

  gs_alv_sort-spos          =  v_alv_spos.     "
  gs_alv_sort-fieldname     =  i_fieldname.    " Campo p/ quebra
  gs_alv_sort-tabname       =  i_tabname.      "
  gs_alv_sort-up            =  i_up.           "
  gs_alv_sort-down          =  i_down.         "
  gs_alv_sort-subtot        =  i_subtot.       " Subtotal

*** '*' quebra de página - 'UL' underline
  gs_alv_sort-group         =  i_group.        " Agrupar campo

  append gs_alv_sort to gt_alv_sort.
  clear  gs_alv_sort.

endform.                    " SELECIONA_QUEBRA_SUBTOTAL
*&---------------------------------------------------------------------*
*&      Form  monta_estrutura_do_alv
*&---------------------------------------------------------------------*
*       Monta a estrutura do relatório ALV
*----------------------------------------------------------------------*
form monta_estrutura_do_alv .
  free: gt_alv_fieldcat.

  perform seleciona_campos_impressao using :
        'X' 'X' 'X' 'AS4DATE' '<fs_table>' '' text-h07 '' 'C' '' '' '' '' ''.
  perform seleciona_campos_impressao using :
        'X' 'X' 'X' 'AS4TIME' '<fs_table>' '' text-h18 '' 'C' '' '' '' '' ''.

  loop at s_amb.
    perform seleciona_campos_impressao using :
          '' '' 'X' s_amb-low '<fs_table>' '' s_amb-low '' 'C' '' '' '' '' ''.
  endloop.

  perform seleciona_campos_impressao using :
        'X' 'X' 'X' 'DTECP' '<fs_table>' '' text-h23 '' 'C' '' '' '' '' ''.
  perform seleciona_campos_impressao using :
        'X' 'X' 'X' 'TMECP' '<fs_table>' '' text-h24 '' 'C' '' '' '' '' ''.
  perform seleciona_campos_impressao using :
        'X' 'X' 'X' 'TRKORR' '<fs_table>' '' text-h01 '' 'C' '' '' '' '' 'X'.
  perform seleciona_campos_impressao using :
        '' '' 'X' 'DESCREQ' '<fs_table>' '' text-h13 '' 'L' '' '' '' '' ''.
  perform seleciona_campos_impressao using :
        '' '' 'X' 'AS4USER' '<fs_table>' '' text-h06 '' 'C' '' '' '' '' ''.
  perform seleciona_campos_impressao using :
        '' '' 'X' 'TRFUNCTION' '<fs_table>' '' text-h10 '' 'C' '' '' '' '' ''.
  perform seleciona_campos_impressao using :
        '' '' 'X' 'TRSTATUS' '<fs_table>' '' text-h11 '' 'C' '' '' '' '' ''.
  perform seleciona_campos_impressao using :
        '' '' 'X' 'KORRDEV' '<fs_table>' '' text-h12 '' 'C' '' '' '' '' ''.
endform.                    " monta_estrutura_do_alv
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_CAMPOS_IMPRESSAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0175   text
*      -->P_C_X  text
*      -->P_C_X  text
*      -->P_C_DATUM  text
*      -->P_C_TPRINT  text
*      -->P_0180   text
*      -->P_TEXT_H01  text
*      -->P_0182   text
*      -->P_C_C  text
*      -->P_C_10  text
*      -->P_0185   text
*      -->P_0186   text
*      -->P_0187   text
*----------------------------------------------------------------------*
form seleciona_campos_impressao  using  i_fixc        type c  " 1
                                        i_key         type c  " 2
                                        i_emph        type c  " 3
                                        i_field       type c  " 1
                                        i_tab         type c  " 2
                                        i_ref         type c  " 3
                                        i_text        type c  " 4
                                        i_sum         type c  " 5
                                        i_just        type c  " 6
                                        i_outputlen   type c  " 7
                                        i_datatype    type c  " 8
                                        i_no_out      type c  " 9
                                        i_mark        type c  " 10
                                        i_hotspot     type c. " 11

  gs_alv_fieldcat-fix_column      =  i_fixc.
  gs_alv_fieldcat-key             =  i_key.
  gs_alv_fieldcat-emphasize       =  i_emph.
  gs_alv_fieldcat-fieldname       =  i_field.
  gs_alv_fieldcat-tabname         =  i_tab.
  gs_alv_fieldcat-ref_tabname     =  i_ref.
  gs_alv_fieldcat-reptext_ddic    =  i_text.
  gs_alv_fieldcat-do_sum          =  i_sum.
  gs_alv_fieldcat-just            =  i_just.
  if not i_outputlen is initial.
    gs_alv_fieldcat-outputlen     =  i_outputlen.
  else.
    clear:                           gs_alv_fieldcat-outputlen.
  endif.
  gs_alv_fieldcat-datatype        =  i_datatype.
  gs_alv_fieldcat-no_out          =  i_no_out.
  gs_alv_fieldcat-checkbox        =  i_mark.
  gs_alv_fieldcat-hotspot         =  i_hotspot.

*** No_zero
  gs_alv_fieldcat-no_zero = 'X'.
  append gs_alv_fieldcat to gt_alv_fieldcat.
  clear  gs_alv_fieldcat.

endform.                    " SELECIONA_CAMPOS_IMPRESSAO
*&---------------------------------------------------------------------*
*&      Form  exibe_relatorio
*&---------------------------------------------------------------------*
*       Exibe relatório ALV
*----------------------------------------------------------------------*
form exibe_relatorio .
  data: l_repid like sy-repid,
        l_page  type  slis_formname.

  gt_alv_print-no_coverpage       = 'X'.
  l_repid                         = sy-repid.
  gt_alv_layout-colwidth_optimize = 'X'.

  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program      = l_repid
      i_callback_user_command = 'F_USER_COMMAND'
      is_layout               = gt_alv_layout
      it_fieldcat             = gt_alv_fieldcat[]
      it_sort                 = gt_alv_sort[]
      i_save                  = 'A'  "Mostra botoes de gravar layout
      i_default               = 'X'
      is_print                = gt_alv_print
      i_screen_start_column   = 0
      i_screen_start_line     = 0
      i_screen_end_column     = 0
      i_screen_end_line       = 0
    tables
      t_outtab                = <fs_table>
    exceptions
      program_error           = 1
      others                  = 2.

endform.                    " exibe_relatorio
*---------------------------------------------------------------------*
*       FORM F_STATUS_SET                                            *
*---------------------------------------------------------------------*
form f_status_set using extab type slis_t_extab.            "#EC CALLED

  clear extab.

  set pf-status 'STANDARD' excluding extab.

endform.                    " F_STATUS_SET
*
**&---------------------------------------------------------------------*
**&      Form  F_USER_COMMAND
**&---------------------------------------------------------------------*
*form f_user_command using ucomm    like sy-ucomm            "#EC *
*      selfield type slis_selfield.                          "#EC *
*
*  data: v_trkorr like e070-trkorr.
*
*  if sy-ucomm = 'REFRESH'.
*    set screen 0.
*    leave list-processing .
*    perform executar_relatorio.
*  elseif ucomm = '&IC1'.
*    if not selfield-value is initial.
*      v_trkorr = selfield-value.
*      call function 'TR_LOG_OVERVIEW_REQUEST_REMOTE'
*        EXPORTING
*          iv_trkorr = v_trkorr.
*    endif.
*  endif.
*
*endform.                    " F_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  CRIA_TABELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_cria_tabela .

  field-symbols:
    <fs_line>  type any .

* Limpando objetos
  unassign: <fs_table>, <fs_line>. "<fs_field>.

  refresh:  gt_fieldcatalog .
  clear:    gs_fieldcatalog, gt_new_table .

  perform f_cria_coluna using: 'TRKORR'      20  'E071' 'TRKORR',
*                              ''AS4POS'      6   'E071' 'AS4POS',
*                              'PGMID'       4   'E071' 'PGMID',
*                              'OBJECT'      4   'E071' 'OBJECT',
*                              'OBJNAME'     120 'E071' 'OBJ_NAME',
                               'AS4USER'     12  'E070' 'AS4USER',
                               'AS4DATE'     8   'E070' 'AS4DATE',
                               'AS4TIME'     6   'E070' 'AS4TIME',
                               'TRFUNCTION'  42  ''     '',
                               'TRSTATUS'    60  ''     '',
                               'KORRDEV'     4   'E070' 'KORRDEV',
*                              'TRKORRTASK'  20  'E070' 'TRKORR',
*                              'AS4USERTASK' 12  'E070' 'AS4USER',
                               'DESCREQ'     60  'E07T' 'AS4TEXT',
*                              'DESCTASK'    60  'E07T' 'AS4TEXT',
                               'DTECP'       10  'SYST' 'DATUM',
                               'TMECP'       6   'SYST' 'UZEIT'.

  loop at s_amb.
    perform f_cria_coluna using: s_amb-low 10 '' '' .
  endloop.

* Cria Campos
  call method cl_alv_table_create=>create_dynamic_table
    exporting
      it_fieldcatalog           = gt_fieldcatalog
    importing
      ep_table                  = gt_new_table
    exceptions
      generate_subpool_dir_full = 1
      others                    = 2.

  if sy-subrc eq 0.

*   Cria uma field-symbol como Tabela Interna
    assign gt_new_table->* to <fs_table>.
    create data gs_new_line like line of <fs_table>.

*   Cria uma field-symbol como Work Area
    assign gs_new_line->* to <fs_line>.

*    perform f_monta_relatorio.

  endif.
endform.                    " CRIA_TABELA
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_RELATORIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_monta_relatorio .

  data:
    ls_ctslg_cofile    type ctslg_system,
    lv_qtd_task        type n length 4,
    lv_qtd_cont        type n length 4,
    lv_systemid        like tstrfcofil-tarsystem,
    lv_linhas          type n length 4,
    lv_controla_transp type c,
    lv_ult_reg         type i,
    gv_nao_gera        type c length 1.

  field-symbols:
    <fs_line>  type any,
    <fs_field> type any.

  assign gs_new_line->* to <fs_line> .

  loop at gt_e070 into gs_e070 .

    clear: lv_controla_transp, gs_cofile.

    e_settings-point_to_missing_steps = 'X'.
    e_settings-detailed_depiction     = 'X'.

    call function 'TR_READ_GLOBAL_INFO_OF_REQUEST'
      exporting
        iv_trkorr   = gs_e070-trkorr
        iv_dir_type = 'T'
        is_settings = e_settings
      importing
        es_cofile   = gs_cofile.

    clear gv_nao_gera.

    loop at s_amb.


*     De acordo com a proposta de nova logica, será colocado o icone com farol apagado
*     caso ainda não tenha a opção de importação no historico da request.
*     Caso tenho ao menos 1 warning, será o farol amarelo
*     Caso tenho ao menos 1 erro, será o farol vermelho

      lv_systemid = s_amb-low.
*     Ordenando de acordo com o Sistema
      sort gs_cofile-systems ascending by systemid.
*     Iniciando variaveis
      clear: ls_ctslg_cofile, gs_actions, gs_steps.
*     Buscando log espeficico de cada Sistema
      read table gs_cofile-systems into ls_ctslg_cofile
                                   with key systemid = lv_systemid
                                   binary search.
      if  sy-subrc eq 0 .
*       Ordenando de acordo com o retorno.
*       Cada ação de importação da request tem um retorno.
        sort ls_ctslg_cofile-steps ascending by rc.

*       Verificando se houve algum erro
        read table ls_ctslg_cofile-steps into gs_steps
                                         with key rc = 8 binary search.
        if sy-subrc eq 0 .
          assign component lv_systemid of structure <fs_line> to <fs_field>.
          <fs_field> = icon_led_red.
        else.
*         Verificando se houve algum warning
          read table ls_ctslg_cofile-steps into gs_steps
                                           with key rc = 4 binary search.
          if sy-subrc eq 0 .
            assign component lv_systemid of structure <fs_line> to <fs_field>.
            <fs_field> = icon_led_yellow.
          else.
*           Verifica a opção de Exportação (significa que é sistema de origem DEV)
            read table ls_ctslg_cofile-steps into gs_steps
                                             with key stepid = 'E' .
            if sy-subrc eq 0 .
              assign component lv_systemid of structure <fs_line> to <fs_field>.
              <fs_field> = icon_led_green.
            else.
*             Verificando se a opção Importação esta aplicada
              read table ls_ctslg_cofile-steps into gs_steps
                                               with key stepid = 'I' .
              if sy-subrc eq 0 .
                assign component lv_systemid of structure <fs_line> to <fs_field>.
                <fs_field> = icon_led_green.
              else.
                assign component lv_systemid of structure <fs_line> to <fs_field>.
                <fs_field> = icon_wd_radio_button_empty.
              endif.
            endif.
          endif.
        endif.
      else.
        assign component lv_systemid of structure <fs_line> to <fs_field>.
        <fs_field> = icon_wd_radio_button_empty.
      endif.

      describe table ls_ctslg_cofile-steps lines lv_ult_reg.
      read table ls_ctslg_cofile-steps into gs_steps index lv_ult_reg.
      read table gs_steps-actions into gs_actions index 1 .

      if  sy-subrc eq 0 .
        if not gs_actions-date in s_dtecp.
          gv_nao_gera = 'X'.
          continue.
        endif.
        assign component 'DTECP' of structure <fs_line> to <fs_field>.
        <fs_field> = gs_actions-date.
        assign component 'TMECP' of structure <fs_line> to <fs_field>.
        <fs_field> = gs_actions-time.
      else.
        if not s_dtecp[] is initial.
          gv_nao_gera = 'X'.
        endif.
        assign component 'DTECP' of structure <fs_line> to <fs_field>.
        clear: <fs_field>.
        assign component 'TMECP' of structure <fs_line> to <fs_field>.
        clear: <fs_field>.
      endif.
    endloop.

    check gv_nao_gera is initial.

    clear gs_tipo.
    read table gt_tipo into gs_tipo
                       with key tipo = gs_e070-trfunction
                       binary search.

    if sy-subrc eq 0 .
      assign component 'TRFUNCTION' of structure <fs_line> to <fs_field>.
      <fs_field> = gs_tipo-desc.
    endif.

    clear gs_status .
    read table gt_status into gs_status
                         with key status = gs_e070-trstatus
                         binary search.
    if sy-subrc eq 0 .
      assign component 'TRSTATUS' of structure <fs_line> to <fs_field>.
      <fs_field> = gs_status-descr.
      assign component 'KORRDEV' of structure <fs_line> to <fs_field>.
      <fs_field> = gs_e070-korrdev.
    endif.

*** Descrição da Request
    clear gs_e07t.
    read table gt_e07t into gs_e07t
                       with key trkorr = gs_e070-trkorr .
    if sy-subrc eq 0 .
      assign component 'DESCREQ' of structure <fs_line> to <fs_field>.
      <fs_field> = gs_e07t-as4text.
    endif.

    assign component 'TRKORR' of structure <fs_line> to <fs_field>.
    <fs_field> = gs_e070-trkorr.
    assign component 'AS4USER' of structure <fs_line> to <fs_field>.
    <fs_field> = gs_e070-as4user.
    assign component 'AS4DATE' of structure <fs_line> to <fs_field>.
    <fs_field> = gs_e070-as4date.
    assign component 'AS4TIME' of structure <fs_line> to <fs_field>.
    <fs_field> = gs_e070-as4time.

    insert <fs_line> into table <fs_table>.
  endloop.

endform.                    " F_MONTA_RELATORIO


*&---------------------------------------------------------------------*
*&      Form  F_ON_LINK_CLICK
*&---------------------------------------------------------------------*
form f_on_link_click  using    p_row
                               p_column.

  data: vl_trkorr type trkorr .


  field-symbols:
    <fs_line>  type any,
    <fs_field> type any.

  read table <fs_table> assigning <fs_line> index p_row .
  if sy-subrc eq 0 .
    assign component 'TRKORR' of structure <fs_line> to <fs_field>.
    if <fs_field> is assigned .
      move <fs_field> to vl_trkorr .
      call function 'TR_LOG_OVERVIEW_REQUEST_REMOTE'
        exporting
          iv_trkorr = vl_trkorr
*         iv_dirtype             =
*         iv_without_check       = ' '
        .
    endif.
  endif.

endform.                    " F_ON_LINK_CLICK
*&---------------------------------------------------------------------*
*&      Form  F_CRIA_COLUNA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIELDNAME  text
*      -->P_OUTPUTLEN  text
*      -->P_REF_TABLE  text
*      -->P_REF_FIELD  text
*----------------------------------------------------------------------*
form f_cria_coluna  using    p_fieldname
                             p_outputlen
                             p_ref_table
                             p_ref_field.

  data: ls_fieldcatalog like line of gt_fieldcatalog .

  ls_fieldcatalog-fieldname = p_fieldname .
  ls_fieldcatalog-outputlen = p_outputlen .
  ls_fieldcatalog-ref_table = p_ref_table .
  ls_fieldcatalog-ref_field = p_ref_field .

  append ls_fieldcatalog to gt_fieldcatalog.
  clear  ls_fieldcatalog.

endform.                    " F_CRIA_COLUNA



*----------------------------------------------------------------------*
*       CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
class lcl_report implementation.

  method get_data.

    perform: f_limpa_dados,
             f_carrega_descricao,
             f_seleciona_dados,
             f_cria_tabela,
             f_monta_relatorio.

  endmethod.                    "GET_DATA

  method generate_output .

    data:
      column  type ref to cl_salv_column_list,
      columns type ref to cl_salv_columns_table.

    try.
        call method cl_salv_table=>factory
          importing
            r_salv_table = lo_table
          changing
            t_table      = <fs_table>[].


        lo_events = lo_table->get_event( ).

***       instantiate the event handler object
**        data: lo_event_handler type ref to cl_event_handler.
**        create object lo_event_handler.
**
***       set handler cl_event_handler=>on_before_salv_function for lo_events.
***       set handler cl_event_handler=>on_after_salv_function for lo_events.
**        set handler cl_event_handler=>on_added_function for lo_events.
***       set handler cl_event_handler=>on_top_of_page for lo_events.
***       set handler cl_event_handler=>on_end_of_page for lo_events.
***       set handler cl_event_handler=>on_double_click for lo_events.
**        set handler cl_event_handler=>on_link_click for lo_events.

*     ALV-Toolbar
        lo_table->set_screen_status(
*         pfstatus      = 'ZTESTE'
          pfstatus      = 'STANDARD_FULLSCREEN'
*         report        = sy-cprog
          report        = 'SAPLKKBL'
          set_functions = lo_table->c_functions_all ).

*     Set column as hotspot
        columns = lo_table->get_columns( ).

*     Otimizando largura das colunas
        columns->set_optimize( 'X' ).
        column ?= columns->get_column( 'TRKORR' ).
        column->set_cell_type( if_salv_c_cell_type=>hotspot ).

        me->set_text_output(
          exporting
            t_tmscsys = t_tmscsys
          changing
            table     = lo_table
        ).

*       Layout de Zebra
        lo_display = lo_table->get_display_settings( ) .
        lo_display->set_striped_pattern(  cl_salv_display_settings=>true ) .

*       Ordenação de campos
        lo_sorts = lo_table->get_sorts( ) .
        lo_sorts->add_sort('AS4DATE') .
        lo_sorts->add_sort('AS4TIME') .
        lo_sorts->add_sort('TRKORR') .

        lo_table->display( ).

      catch cx_salv_msg.             " cl_salv_table=>factory()
        write: / 'cx_salv_msg exception'.
      catch cx_salv_not_found.       " cl_salv_columns_table->get_column()
        write: / 'cx_salv_not_found exception'.
      catch  cx_salv_object_not_found .
    endtry.

  endmethod .                    "generate_output


  method set_text .

    try.
        c_column ?= c_columns->get_column( i_field ).
        c_column->set_short_text( i_short_text ).
        c_column->set_medium_text( i_medium_text ).
        c_column->set_long_text( i_long_text ).
      catch cx_salv_not_found .
    endtry .

  endmethod .

  method set_text_output .

    data:
      line        type ty_tmscsys,
      field       type lvc_fname,
      long_text   type scrtext_l,
      medium_text type scrtext_m,
      short_text  type scrtext_s,
      column      type ref to cl_salv_column_list,
      columns     type ref to cl_salv_columns_table.

    break abap00 .

    columns = table->get_columns( ).

    loop at t_tmscsys into line .

      field       = line-sysnam .
      long_text   = line-sysnam .
      medium_text = line-sysnam .
      short_text  = line-sysnam .

      me->set_text(
        exporting
          i_field       = field
          i_long_text   = long_text
          i_medium_text = medium_text
          i_short_text  = short_text
        changing
          c_columns     = columns
          c_column      = column
      ).

    endloop.

  endmethod.


  method on_before_salv_function.
  endmethod.                    "ON_BEFORE_SALV_FUNCTION

  method on_after_salv_function.
  endmethod.                    "ON_AFTER_SALV_FUNCTION

  method on_added_function.
    break-point .

*   lo_report->lo_table->get_data( ).
*   lo_report->lo_table->refresh( ).

  endmethod.                    "ON_ADDED_FUNCTION

  method on_top_of_page.
  endmethod.                    "ON_TOP_OF_PAGE

  method on_end_of_page.
  endmethod.                    "ON_END_OF_PAGE

  method on_double_click.
  endmethod.                    "ON_DOUBLE_CLICK

  method on_link_click.
    perform f_on_link_click using row column .
  endmethod.                    "ON_LINK_CLICK
endclass.                    "lcl_report IMPLEMENTATION
