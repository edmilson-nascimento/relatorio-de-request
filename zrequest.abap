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

types:
  begin of ty_tmscsys,
    domnam type tmscsys-domnam,
    sysnam type tmscsys-sysnam,
    limbo  type tmscsys-limbo,
  end of ty_tmscsys .

field-symbols:
  <fs_table> type standard table .

*--------------------------------------------------------------------*
*- Declarações Globais
*--------------------------------------------------------------------*
data:
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
  v_listheader type slis_listheader, " Cabeçalho
  v_alv_spos   type n length 2,      " Posição
  wc_cont      type n length 10.



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

      tmscsys_tab type table of ty_tmscsys,

      begin of ty_status,
        status type trstatus,
        descr  type c length 60,
      end of ty_status,

      status_tab type table of ty_status,

      begin of ty_tipo,
        tipo type trfunction,
        desc type c length 42,
      end of ty_tipo,

      tipo_tab type table of ty_tipo,

      begin of ty_r_sysnam,
        sign   type ddsign,
        option type ddoption,
        low    type trtarget-tarsystem,
        high   type trtarget-tarsystem,
      end of ty_r_sysnam,

      r_sysnam type table of ty_r_sysnam,

      begin of ty_r_status,
        sign   type ddsign,
        option type ddoption,
        low    type trstatus,
        high   type trstatus,
      end of ty_r_status,

      r_status type table of ty_r_status.



    class-data:
      t_tmscsys  type tmscsys_tab .

    data:
      lo_table   type ref to cl_salv_table,
      lo_events  type ref to cl_salv_events_table,
      lo_display type ref to cl_salv_display_settings,
      lo_sorts   type ref to cl_salv_sorts.

    class-methods initial
      changing
        ambiente type r_sysnam
        status   type r_status
        data     type wrf_ref_date_rtty .

    methods get_data
      importing
        !ambiente     type r_sysnam
        !ordem        type /gc1/tab_rng_trkorr
        !tipo         type trg_char1
        !status       type r_status
        !categoria    type trg_char4
        !usuario      type fip_t_uname_range
        !data         type wrf_ref_date_rtty
        !data_produca type wrf_ref_date_rtty .

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


  protected section .

    methods on_link_click
      for event if_salv_events_actions_table~link_click
                  of cl_salv_events_table
      importing row
                  column.

  private section .

    methods limpar_dados
      changing
        !e070   type e070_t
        !e071   type e071_t
        !e07t   type e07t_t
        !status type status_tab
        !tipo   type tipo_tab .

    methods carrega_descricao
      changing
        !status type status_tab
        !tipo   type tipo_tab .

    methods seleciona_dados
      importing
        !ordem     type /gc1/tab_rng_trkorr
        !tipo      type trg_char1
        !status    type r_status
        !categoria type trg_char4
        !usuario   type fip_t_uname_range
        !data      type wrf_ref_date_rtty
      changing
        !e070      type e070_t
        !e07t      type e07t_t .

    methods cria_tabela
      changing
        !table type standard table .

    methods cria_coluna
      importing
        !fieldname    type lvc_fname
        !outputlen    type lvc_outlen
        !ref_table    type lvc_rtname
        !ref_field    type lvc_rfname
      changing
        !fieldcatalog type lvc_t_fcat .

    methods monta_relatorio
      importing
        !e070   type e070_t
        !tipo   type tipo_tab
        !status type status_tab
        !e07t   type e07t_t .

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

    methods link_click
      importing
        !row    type any
        !column type any .


endclass.                    "lcl_report DEFINITION



****&---------------------------------------------------------------------*
****&      Form  MONTA_QUEBRA_SUBTOTAL
****&---------------------------------------------------------------------*
****       Monta quebra
****----------------------------------------------------------------------*
***form monta_quebra_subtotal .
***
***  perform seleciona_quebra_subtotal using:
***        'AS4DATE'  '<fs_table>'  'X' ''  ''   'X',
***        'AS4TIME'  '<fs_table>'  'X' ''  ''   'X',
***        'TRKORR'   '<fs_table>'  'X' ''  ''   'X',
***        'DESCREQ'  '<fs_table>'  'X' ''  ''   'X'.
***endform.                    " MONTA_QUEBRA_SUBTOTAL
****&---------------------------------------------------------------------*
****&      Form  SELECIONA_QUEBRA_SUBTOTAL
****&---------------------------------------------------------------------*
****       text
****----------------------------------------------------------------------*
****      -->P_0433   text
****      -->P_C_TPRINT  text
****      -->P_C_X  text
****      -->P_0436   text
****      -->P_0437   text
****      -->P_C_X  text
****----------------------------------------------------------------------*
***form seleciona_quebra_subtotal  using   i_fieldname   type c  " 1
***                                        i_tabname     type c  " 2
***                                        i_up          type c  " 3
***                                        i_down        type c  " 4
***                                        i_subtot      type c  " 5
***                                        i_group       type c. " 6
***  v_alv_spos = v_alv_spos + 1.
***
***  gs_alv_sort-spos          =  v_alv_spos.     "
***  gs_alv_sort-fieldname     =  i_fieldname.    " Campo p/ quebra
***  gs_alv_sort-tabname       =  i_tabname.      "
***  gs_alv_sort-up            =  i_up.           "
***  gs_alv_sort-down          =  i_down.         "
***  gs_alv_sort-subtot        =  i_subtot.       " Subtotal
***
****** '*' quebra de página - 'UL' underline
***  gs_alv_sort-group         =  i_group.        " Agrupar campo
***
***  append gs_alv_sort to gt_alv_sort.
***  clear  gs_alv_sort.
***
***endform.                    " SELECIONA_QUEBRA_SUBTOTAL
****&---------------------------------------------------------------------*
****&      Form  monta_estrutura_do_alv
****&---------------------------------------------------------------------*
****       Monta a estrutura do relatório ALV
****----------------------------------------------------------------------*
***form monta_estrutura_do_alv .
***  free: gt_alv_fieldcat.
***
***  perform seleciona_campos_impressao using :
***        'X' 'X' 'X' 'AS4DATE' '<fs_table>' '' text-h07 '' 'C' '' '' '' '' ''.
***  perform seleciona_campos_impressao using :
***        'X' 'X' 'X' 'AS4TIME' '<fs_table>' '' text-h18 '' 'C' '' '' '' '' ''.
***
***  loop at s_amb.
***    perform seleciona_campos_impressao using :
***          '' '' 'X' s_amb-low '<fs_table>' '' s_amb-low '' 'C' '' '' '' '' ''.
***  endloop.
***
***  perform seleciona_campos_impressao using :
***        'X' 'X' 'X' 'DTECP' '<fs_table>' '' text-h23 '' 'C' '' '' '' '' ''.
***  perform seleciona_campos_impressao using :
***        'X' 'X' 'X' 'TMECP' '<fs_table>' '' text-h24 '' 'C' '' '' '' '' ''.
***  perform seleciona_campos_impressao using :
***        'X' 'X' 'X' 'TRKORR' '<fs_table>' '' text-h01 '' 'C' '' '' '' '' 'X'.
***  perform seleciona_campos_impressao using :
***        '' '' 'X' 'DESCREQ' '<fs_table>' '' text-h13 '' 'L' '' '' '' '' ''.
***  perform seleciona_campos_impressao using :
***        '' '' 'X' 'AS4USER' '<fs_table>' '' text-h06 '' 'C' '' '' '' '' ''.
***  perform seleciona_campos_impressao using :
***        '' '' 'X' 'TRFUNCTION' '<fs_table>' '' text-h10 '' 'C' '' '' '' '' ''.
***  perform seleciona_campos_impressao using :
***        '' '' 'X' 'TRSTATUS' '<fs_table>' '' text-h11 '' 'C' '' '' '' '' ''.
***  perform seleciona_campos_impressao using :
***        '' '' 'X' 'KORRDEV' '<fs_table>' '' text-h12 '' 'C' '' '' '' '' ''.
***endform.                    " monta_estrutura_do_alv
****&---------------------------------------------------------------------*
****&      Form  SELECIONA_CAMPOS_IMPRESSAO
****&---------------------------------------------------------------------*
****       text
****----------------------------------------------------------------------*
****      -->P_0175   text
****      -->P_C_X  text
****      -->P_C_X  text
****      -->P_C_DATUM  text
****      -->P_C_TPRINT  text
****      -->P_0180   text
****      -->P_TEXT_H01  text
****      -->P_0182   text
****      -->P_C_C  text
****      -->P_C_10  text
****      -->P_0185   text
****      -->P_0186   text
****      -->P_0187   text
****----------------------------------------------------------------------*
***form seleciona_campos_impressao  using  i_fixc        type c  " 1
***                                        i_key         type c  " 2
***                                        i_emph        type c  " 3
***                                        i_field       type c  " 1
***                                        i_tab         type c  " 2
***                                        i_ref         type c  " 3
***                                        i_text        type c  " 4
***                                        i_sum         type c  " 5
***                                        i_just        type c  " 6
***                                        i_outputlen   type c  " 7
***                                        i_datatype    type c  " 8
***                                        i_no_out      type c  " 9
***                                        i_mark        type c  " 10
***                                        i_hotspot     type c. " 11
***
***  gs_alv_fieldcat-fix_column      =  i_fixc.
***  gs_alv_fieldcat-key             =  i_key.
***  gs_alv_fieldcat-emphasize       =  i_emph.
***  gs_alv_fieldcat-fieldname       =  i_field.
***  gs_alv_fieldcat-tabname         =  i_tab.
***  gs_alv_fieldcat-ref_tabname     =  i_ref.
***  gs_alv_fieldcat-reptext_ddic    =  i_text.
***  gs_alv_fieldcat-do_sum          =  i_sum.
***  gs_alv_fieldcat-just            =  i_just.
***  if not i_outputlen is initial.
***    gs_alv_fieldcat-outputlen     =  i_outputlen.
***  else.
***    clear:                           gs_alv_fieldcat-outputlen.
***  endif.
***  gs_alv_fieldcat-datatype        =  i_datatype.
***  gs_alv_fieldcat-no_out          =  i_no_out.
***  gs_alv_fieldcat-checkbox        =  i_mark.
***  gs_alv_fieldcat-hotspot         =  i_hotspot.
***
****** No_zero
***  gs_alv_fieldcat-no_zero = 'X'.
***  append gs_alv_fieldcat to gt_alv_fieldcat.
***  clear  gs_alv_fieldcat.
***
***endform.                    " SELECIONA_CAMPOS_IMPRESSAO
****&---------------------------------------------------------------------*
****&      Form  exibe_relatorio
****&---------------------------------------------------------------------*
****       Exibe relatório ALV
****----------------------------------------------------------------------*
***form exibe_relatorio .
***  data: l_repid like sy-repid,
***        l_page  type  slis_formname.
***
***  gt_alv_print-no_coverpage       = 'X'.
***  l_repid                         = sy-repid.
***  gt_alv_layout-colwidth_optimize = 'X'.
***
***  call function 'REUSE_ALV_GRID_DISPLAY'
***    exporting
***      i_callback_program      = l_repid
***      i_callback_user_command = 'F_USER_COMMAND'
***      is_layout               = gt_alv_layout
***      it_fieldcat             = gt_alv_fieldcat[]
***      it_sort                 = gt_alv_sort[]
***      i_save                  = 'A'  "Mostra botoes de gravar layout
***      i_default               = 'X'
***      is_print                = gt_alv_print
***      i_screen_start_column   = 0
***      i_screen_start_line     = 0
***      i_screen_end_column     = 0
***      i_screen_end_line       = 0
***    tables
***      t_outtab                = <fs_table>
***    exceptions
***      program_error           = 1
***      others                  = 2.
***
***endform.                    " exibe_relatorio
****---------------------------------------------------------------------*
****       FORM F_STATUS_SET                                            *
****---------------------------------------------------------------------*
***form f_status_set using extab type slis_t_extab.            "#EC CALLED
***
***  clear extab.
***
***  set pf-status 'STANDARD' excluding extab.
***
***endform.                    " F_STATUS_SET
****
*****&---------------------------------------------------------------------*
*****&      Form  F_USER_COMMAND
*****&---------------------------------------------------------------------*
****form f_user_command using ucomm    like sy-ucomm            "#EC *
****      selfield type slis_selfield.                          "#EC *
****
****  data: v_trkorr like e070-trkorr.
****
****  if sy-ucomm = 'REFRESH'.
****    set screen 0.
****    leave list-processing .
****    perform executar_relatorio.
****  elseif ucomm = '&IC1'.
****    if not selfield-value is initial.
****      v_trkorr = selfield-value.
****      call function 'TR_LOG_OVERVIEW_REQUEST_REMOTE'
****        EXPORTING
****          iv_trkorr = v_trkorr.
****    endif.
****  endif.
****
****endform.                    " F_USER_COMMAND
****&---------------------------------------------------------------------*
****&      Form  CRIA_TABELA
****&---------------------------------------------------------------------*
****       text
****----------------------------------------------------------------------*
****  -->  p1        text
****  <--  p2        text
****----------------------------------------------------------------------*
***form f_cria_tabela .
****
****  field-symbols:
****    <fs_line>  type any .
****
***** Limpando objetos
****  unassign: <fs_table>, <fs_line>. "<fs_field>.
****
****  refresh:  gt_fieldcatalog .
****  clear:    gs_fieldcatalog, gt_new_table .
****
****  perform f_cria_coluna using: 'TRKORR'      20  'E071' 'TRKORR',
*****                              ''AS4POS'      6   'E071' 'AS4POS',
*****                              'PGMID'       4   'E071' 'PGMID',
*****                              'OBJECT'      4   'E071' 'OBJECT',
*****                              'OBJNAME'     120 'E071' 'OBJ_NAME',
****                               'AS4USER'     12  'E070' 'AS4USER',
****                               'AS4DATE'     8   'E070' 'AS4DATE',
****                               'AS4TIME'     6   'E070' 'AS4TIME',
****                               'TRFUNCTION'  42  ''     '',
****                               'TRSTATUS'    60  ''     '',
****                               'KORRDEV'     4   'E070' 'KORRDEV',
*****                              'TRKORRTASK'  20  'E070' 'TRKORR',
*****                              'AS4USERTASK' 12  'E070' 'AS4USER',
****                               'DESCREQ'     60  'E07T' 'AS4TEXT',
*****                              'DESCTASK'    60  'E07T' 'AS4TEXT',
****                               'DTECP'       10  'SYST' 'DATUM',
****                               'TMECP'       6   'SYST' 'UZEIT'.
****
****  loop at s_amb.
****    perform f_cria_coluna using: s_amb-low 10 '' '' .
****  endloop.
****
***** Cria Campos
****  call method cl_alv_table_create=>create_dynamic_table
****    exporting
****      it_fieldcatalog           = gt_fieldcatalog
****    importing
****      ep_table                  = gt_new_table
****    exceptions
****      generate_subpool_dir_full = 1
****      others                    = 2.
****
****  if sy-subrc eq 0.
****
*****   Cria uma field-symbol como Tabela Interna
****    assign gt_new_table->* to <fs_table>.
****    create data gs_new_line like line of <fs_table>.
****
*****   Cria uma field-symbol como Work Area
****    assign gs_new_line->* to <fs_line>.
****
*****    perform f_monta_relatorio.
****
****  endif.
***endform.                    " CRIA_TABELA
****&---------------------------------------------------------------------*
****&      Form  F_MONTA_RELATORIO
****&---------------------------------------------------------------------*
****       text
****----------------------------------------------------------------------*
****  -->  p1        text
****  <--  p2        text
****----------------------------------------------------------------------*
***form f_monta_relatorio .
***
****  data:
****    ls_ctslg_cofile    type ctslg_system,
****    lv_qtd_task        type n length 4,
****    lv_qtd_cont        type n length 4,
****    lv_systemid        like tstrfcofil-tarsystem,
****    lv_linhas          type n length 4,
****    lv_controla_transp type c,
****    lv_ult_reg         type i,
****    gv_nao_gera        type c length 1.
****
****  field-symbols:
****    <fs_line>  type any,
****    <fs_field> type any.
****
****  assign gs_new_line->* to <fs_line> .
****
****  loop at gt_e070 into gs_e070 .
****
****    clear: lv_controla_transp, gs_cofile.
****
****    e_settings-point_to_missing_steps = 'X'.
****    e_settings-detailed_depiction     = 'X'.
****
****    call function 'TR_READ_GLOBAL_INFO_OF_REQUEST'
****      exporting
****        iv_trkorr   = gs_e070-trkorr
****        iv_dir_type = 'T'
****        is_settings = e_settings
****      importing
****        es_cofile   = gs_cofile.
****
****    clear gv_nao_gera.
****
****    loop at s_amb.
****
****
*****     De acordo com a proposta de nova logica, será colocado o icone com farol apagado
*****     caso ainda não tenha a opção de importação no historico da request.
*****     Caso tenho ao menos 1 warning, será o farol amarelo
*****     Caso tenho ao menos 1 erro, será o farol vermelho
****
****      lv_systemid = s_amb-low.
*****     Ordenando de acordo com o Sistema
****      sort gs_cofile-systems ascending by systemid.
*****     Iniciando variaveis
****      clear: ls_ctslg_cofile, gs_actions, gs_steps.
*****     Buscando log espeficico de cada Sistema
****      read table gs_cofile-systems into ls_ctslg_cofile
****                                   with key systemid = lv_systemid
****                                   binary search.
****      if  sy-subrc eq 0 .
*****       Ordenando de acordo com o retorno.
*****       Cada ação de importação da request tem um retorno.
****        sort ls_ctslg_cofile-steps ascending by rc.
****
*****       Verificando se houve algum erro
****        read table ls_ctslg_cofile-steps into gs_steps
****                                         with key rc = 8 binary search.
****        if sy-subrc eq 0 .
****          assign component lv_systemid of structure <fs_line> to <fs_field>.
****          <fs_field> = icon_led_red.
****        else.
*****         Verificando se houve algum warning
****          read table ls_ctslg_cofile-steps into gs_steps
****                                           with key rc = 4 binary search.
****          if sy-subrc eq 0 .
****            assign component lv_systemid of structure <fs_line> to <fs_field>.
****            <fs_field> = icon_led_yellow.
****          else.
*****           Verifica a opção de Exportação (significa que é sistema de origem DEV)
****            read table ls_ctslg_cofile-steps into gs_steps
****                                             with key stepid = 'E' .
****            if sy-subrc eq 0 .
****              assign component lv_systemid of structure <fs_line> to <fs_field>.
****              <fs_field> = icon_led_green.
****            else.
*****             Verificando se a opção Importação esta aplicada
****              read table ls_ctslg_cofile-steps into gs_steps
****                                               with key stepid = 'I' .
****              if sy-subrc eq 0 .
****                assign component lv_systemid of structure <fs_line> to <fs_field>.
****                <fs_field> = icon_led_green.
****              else.
****                assign component lv_systemid of structure <fs_line> to <fs_field>.
****                <fs_field> = icon_wd_radio_button_empty.
****              endif.
****            endif.
****          endif.
****        endif.
****      else.
****        assign component lv_systemid of structure <fs_line> to <fs_field>.
****        <fs_field> = icon_wd_radio_button_empty.
****      endif.
****
****      describe table ls_ctslg_cofile-steps lines lv_ult_reg.
****      read table ls_ctslg_cofile-steps into gs_steps index lv_ult_reg.
****      read table gs_steps-actions into gs_actions index 1 .
****
****      if  sy-subrc eq 0 .
****        if not gs_actions-date in s_dtecp.
****          gv_nao_gera = 'X'.
****          continue.
****        endif.
****        assign component 'DTECP' of structure <fs_line> to <fs_field>.
****        <fs_field> = gs_actions-date.
****        assign component 'TMECP' of structure <fs_line> to <fs_field>.
****        <fs_field> = gs_actions-time.
****      else.
****        if not s_dtecp[] is initial.
****          gv_nao_gera = 'X'.
****        endif.
****        assign component 'DTECP' of structure <fs_line> to <fs_field>.
****        clear: <fs_field>.
****        assign component 'TMECP' of structure <fs_line> to <fs_field>.
****        clear: <fs_field>.
****      endif.
****    endloop.
****
****    check gv_nao_gera is initial.
****
****    clear gs_tipo.
****    read table gt_tipo into gs_tipo
****                       with key tipo = gs_e070-trfunction
****                       binary search.
****
****    if sy-subrc eq 0 .
****      assign component 'TRFUNCTION' of structure <fs_line> to <fs_field>.
****      <fs_field> = gs_tipo-desc.
****    endif.
****
****    clear gs_status .
****    read table gt_status into gs_status
****                         with key status = gs_e070-trstatus
****                         binary search.
****    if sy-subrc eq 0 .
****      assign component 'TRSTATUS' of structure <fs_line> to <fs_field>.
****      <fs_field> = gs_status-descr.
****      assign component 'KORRDEV' of structure <fs_line> to <fs_field>.
****      <fs_field> = gs_e070-korrdev.
****    endif.
****
******* Descrição da Request
****    clear gs_e07t.
****    read table gt_e07t into gs_e07t
****                       with key trkorr = gs_e070-trkorr .
****    if sy-subrc eq 0 .
****      assign component 'DESCREQ' of structure <fs_line> to <fs_field>.
****      <fs_field> = gs_e07t-as4text.
****    endif.
****
****    assign component 'TRKORR' of structure <fs_line> to <fs_field>.
****    <fs_field> = gs_e070-trkorr.
****    assign component 'AS4USER' of structure <fs_line> to <fs_field>.
****    <fs_field> = gs_e070-as4user.
****    assign component 'AS4DATE' of structure <fs_line> to <fs_field>.
****    <fs_field> = gs_e070-as4date.
****    assign component 'AS4TIME' of structure <fs_line> to <fs_field>.
****    <fs_field> = gs_e070-as4time.
****
****    insert <fs_line> into table <fs_table>.
****  endloop.
***
***endform.                    " F_MONTA_RELATORIO
***
****&---------------------------------------------------------------------*
****&      Form  F_CRIA_COLUNA
****&---------------------------------------------------------------------*
****       text
****----------------------------------------------------------------------*
****      -->P_FIELDNAME  text
****      -->P_OUTPUTLEN  text
****      -->P_REF_TABLE  text
****      -->P_REF_FIELD  text
****----------------------------------------------------------------------*
***form f_cria_coluna  using    p_fieldname
***                             p_outputlen
***                             p_ref_table
***                             p_ref_field.
***
***  data: ls_fieldcatalog like line of gt_fieldcatalog .
***
***  ls_fieldcatalog-fieldname = p_fieldname .
***  ls_fieldcatalog-outputlen = p_outputlen .
***  ls_fieldcatalog-ref_table = p_ref_table .
***  ls_fieldcatalog-ref_field = p_ref_field .
***
***  append ls_fieldcatalog to gt_fieldcatalog.
***  clear  ls_fieldcatalog.
***
***endform.                    " F_CRIA_COLUNA



*----------------------------------------------------------------------*
*       CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
class lcl_report implementation.


  method initial .

    data:
      ls_status type ty_r_status,
      ls_data   type admpn_ti_mfrpn_range,
      ls_sysnam type ty_r_sysnam.

    refresh:
      ambiente, status, data .

    ls_status-sign   = 'I' .
    ls_status-option = 'EQ' .
    ls_status-low    = 'R' .
    append ls_status to status .
    clear  ls_status .

    ls_data-sign   = 'I' .
    ls_data-option = 'EQ' .
    ls_data-low    = sy-datum .
    append ls_data to data .
    clear  ls_data .


    select domnam sysnam limbo
      into table t_tmscsys
      from tmscsys.

    if sy-subrc eq 0 .

      loop at t_tmscsys into wa_tmscsys .

        ls_sysnam-sign = 'I' .
        ls_sysnam-option = 'EQ' .
        ls_sysnam-low = wa_tmscsys-sysnam .
        append ls_sysnam to ambiente .
        clear  ls_sysnam .

      endloop.

      sort ambiente ascending by low .

    endif.


  endmethod .

  method get_data.

    data:
      gt_e070   type table of e070,
*      gs_e070    type          e070,
*      gt_e070_2  type table of e070,
*      gs_e070_2  type          e070,
      gt_e071   type table of e071,
*      gs_e071    type          e071,
*      gt_e071_2  type table of e071,
*      gs_e071_2  type          e071,
      gt_e07t   type table of e07t,
*      gs_e07t    type          e07t,
      gt_status type table of ty_status,
*      gs_status  type          ty_status,
      gt_tipo   type table of ty_tipo.
*      gs_tipo    type          ty_tipo,

*      gs_steps   type ctslg_step,
*      gs_actions type ctslg_action.

*   perform f_limpa_dados .
    me->limpar_dados(
      changing
        e070    = gt_e070
        e071    = gt_e071 "#verificar essa utilização
        e07t    = gt_e07t
        status  = gt_status
        tipo    = gt_tipo
    ).


*   perform f_carrega_descricao .
    me->carrega_descricao(
      changing
        status = gt_status
        tipo   = gt_tipo
    ).

*   perform f_seleciona_dados .
    me->seleciona_dados(
      exporting
        ordem     = ordem
        tipo      = tipo
        status    = status
        categoria = categoria
        usuario   = usuario
        data      = data
      changing
        e070      = gt_e070
        e07t      = gt_e07t
    ).

**   perform f_cria_tabela .
*    me->cria_tabela(
*      changing
*        table = table
*    ).

*   perform f_monta_relatorio .
    me->monta_relatorio(
      exporting
        e070   = gt_e070
        tipo   = gt_tipo
        status = gt_status
        e07t   = gt_e07t
    ).


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



**       instantiate the event handler object
**        data: lo_event_handler type ref to cl_event_handler.
**        create object lo_event_handler.

*       set handler cl_event_handler=>on_before_salv_function for lo_events.
*       set handler cl_event_handler=>on_after_salv_function for lo_events.
        set handler lcl_report=>on_added_function for lo_events.
*       set handler cl_event_handler=>on_top_of_page for lo_events.
*       set handler cl_event_handler=>on_end_of_page for lo_events.
*       set handler cl_event_handler=>on_double_click for lo_events.
        set handler me->on_link_click for lo_events.

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

  method limpar_dados .

    free:
      e070, e071, e07t, status, tipo .

  endmethod .

  method carrega_descricao .

    data:
      lt_list   type table of vrm_value,
      ls_list   type          vrm_value,
      ls_status type ty_status,
      ls_tipo   type ty_tipo.

    call function 'FICO_DOMAIN_VALUES_GET'
      exporting
        i_table_name = 'E070'
        i_field_name = 'TRSTATUS'
        i_langu      = sy-langu
      importing
        e_t_list     = lt_list.

    loop at lt_list into ls_list.
      ls_status-status = ls_list-key .
      ls_status-descr = ls_list-text .
      append ls_status to status.
      clear  ls_status .
    endloop.

    sort status ascending by status.

    refresh lt_list .
    clear   ls_list .

    call function 'FICO_DOMAIN_VALUES_GET'
      exporting
        i_table_name = 'E070'
        i_field_name = 'TRFUNCTION'
        i_langu      = sy-langu
      importing
        e_t_list     = lt_list.

    loop at lt_list into ls_list .
      ls_tipo-tipo = ls_list-key .
      ls_tipo-desc = ls_list-text .
      append ls_tipo to tipo .
      clear  ls_tipo .
    endloop.

    sort tipo ascending by tipo.

  endmethod .


  method seleciona_dados .

    select *
      from e070
      into table e070
     where trkorr     in ordem
       and trfunction in tipo
       and trstatus   in status
       and korrdev    in categoria
       and as4user    in usuario
       and as4date    in data
       and strkorr    eq space.
    if sy-subrc ne 0 .

    endif.


    if lines( e070 ) eq 0 .

    else .

      select *
        from e07t
        into table e07t
         for all entries in e070
       where trkorr eq e070-trkorr
         and ( langu  eq 'PT' or langu  eq 'EN' ).

    endif.

  endmethod.


  method cria_tabela .

    data:
      lt_fieldcat type lvc_t_fcat,
      ls_tmscsys  type ty_tmscsys,
      fieldname   type lvc_fname.

    field-symbols:
      <fs_line>  type any .


*    unassign: <fs_table>, <fs_line>. "<fs_field>.
*
*    refresh:  gt_fieldcatalog .
*    clear:    gs_fieldcatalog, gt_new_table .

    me->cria_coluna(
      exporting
        fieldname    = 'TRKORR'
        outputlen    = 20
        ref_table    = 'E071'
        ref_field    = 'TRKORR'
      changing
        fieldcatalog = lt_fieldcat
    ).
*    me->cria_coluna(
*      exporting
*        fieldname    = 'AS4POS'
*        outputlen    = 6
*        ref_table    = 'E071'
*        ref_field    = 'AS4POS'
*      changing
*        fieldcatalog = lt_fieldcat
*    ).
*    me->cria_coluna(
*      exporting
*        fieldname    = 'PGMID'
*        outputlen    = 6
*        ref_table    = 'E071'
*        ref_field    = 'PGMID'
*      changing
*        fieldcatalog = lt_fieldcat
*    ).
*    me->cria_coluna(
*      exporting
*        fieldname    = 'OBJECT'
*        outputlen    = 4
*        ref_table    = 'E071'
*        ref_field    = 'OBJECT'
*      changing
*        fieldcatalog = lt_fieldcat
*    ).
*    me->cria_coluna(
*      exporting
*        fieldname    = 'OBJNAME'
*        outputlen    = 120
*        ref_table    = 'E071'
*        ref_field    = 'OBJ_NAME'
*      changing
*        fieldcatalog = lt_fieldcat
*    ).

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
      changing
        fieldcatalog = lt_fieldcat
    ).
    me->cria_coluna(
      exporting
        fieldname    = 'TRSTATUS'
        outputlen    = 60
        ref_table    = ''
        ref_field    = ''
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
*    me->cria_coluna(
*      exporting
*        fieldname    = 'TRKORRTASK'
*        outputlen    = 20
*        ref_table    = 'E070'
*        ref_field    = 'TRKORR'
*      changing
*        fieldcatalog = lt_fieldcat
*    ).
*    me->cria_coluna(
*      exporting
*        fieldname    = 'AS4USERTASK'
*        outputlen    = 12
*        ref_table    = 'E070'
*        ref_field    = 'AS4USER'
*      changing
*        fieldcatalog = lt_fieldcat
*    ).
    me->cria_coluna(
      exporting
        fieldname    = 'DESCREQ'
        outputlen    = 60
        ref_table    = 'E07T'
        ref_field    = 'AS4TEXT'
      changing
        fieldcatalog = lt_fieldcat
    ).
*    me->cria_coluna(
*      exporting
*        fieldname    = 'DESCTASK'
*        outputlen    = 60
*        ref_table    = 'E07T'
*        ref_field    = 'AS4TEXT'
*      changing
*        fieldcatalog = lt_fieldcat
*    ).
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
        ep_table                  = gt_new_table
*       e_style_fname             =     " ALV Control: Field Name of Internal Table Field
      exceptions
        generate_subpool_dir_full = 1
        others                    = 2.
    if sy-subrc eq 0 .

*     Cria uma field-symbol como Tabela Interna
      assign gt_new_table->* to <fs_table>.
      create data gs_new_line like line of <fs_table>.

*     Cria uma field-symbol como Work Area
      assign gs_new_line->* to <fs_line>.

    else .
*     message id sy-msgid type sy-msgty number sy-msgno
*                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.


  endmethod .


  method monta_relatorio .

    data:
      ls_tmscys          type ty_tmscsys,
      ls_e070            type e070,
      ls_tipo            type ty_tipo,
      ls_status          type ty_status,
      ls_e07t            type e07t,
      settings           type ctslg_settings,
      systemid           type tstrfcofil-tarsystem,
      cofile             type ctslg_cofile,
      ls_systems         type ctslg_system,
      ls_steps           type ctslg_step,
      ultimo_registro    type i,
      ls_action          type ctslg_action,

      lv_qtd_task        type n length 4,
      lv_qtd_cont        type n length 4,
      lv_linhas          type n length 4,
      lv_controla_transp type c,
      gv_nao_gera        type c length 1.

    field-symbols:
      <fs_line>  type any,
      <fs_field> type any.


    settings-point_to_missing_steps = abap_on .
    settings-detailed_depiction     = abap_on .

    assign gs_new_line->* to <fs_line> .

    loop at e070 into ls_e070 .

*     clear: lv_controla_transp, gs_cofile.
      clear gv_nao_gera.

      call function 'TR_READ_GLOBAL_INFO_OF_REQUEST'
        exporting
          iv_trkorr   = ls_e070-trkorr
          iv_dir_type = 'T'
          is_settings = settings
        importing
          es_cofile   = cofile.


      loop at t_tmscsys into ls_tmscys .

*     De acordo com a proposta de nova logica, será colocado o icone com farol apagado
*     caso ainda não tenha a opção de importação no historico da request.
*     Caso tenho ao menos 1 warning, será o farol amarelo
*     Caso tenho ao menos 1 erro, será o farol vermelho

        systemid = ls_tmscys-sysnam .

*       Ordenando de acordo com o Sistema
        sort cofile-systems ascending by systemid.

*       Buscando log espeficico de cada Sistema
        read table cofile-systems into ls_systems
                                  with key systemid = systemid
                                  binary search .
        if  sy-subrc eq 0 .

*         Ordenando de acordo com o retorno.
*         Cada ação de importação da request tem um retorno.
          sort ls_systems-steps ascending by rc.

*         Verificando se houve algum erro
          read table ls_systems-steps into ls_steps
                                      with key rc = 8
                                      binary search.
          if sy-subrc eq 0 .
            assign component systemid of structure <fs_line> to <fs_field>.
            <fs_field> = icon_led_red.
          else.
*         Verificando se houve algum warning
            read table ls_systems-steps into ls_steps
                                        with key rc = 4
                                        binary search .
            if sy-subrc eq 0 .
              assign component systemid of structure <fs_line> to <fs_field>.
              <fs_field> = icon_led_yellow.
            else.
*           Verifica a opção de Exportação (significa que é sistema de origem DEV)
              read table ls_systems-steps into ls_steps
                                          with key stepid = 'E' .
              if sy-subrc eq 0 .
                assign component systemid of structure <fs_line> to <fs_field>.
                <fs_field> = icon_led_green.
              else.
*             Verificando se a opção Importação esta aplicada
                read table ls_systems-steps into ls_steps
                                            with key stepid = 'I' .
                if sy-subrc eq 0 .
                  assign component systemid of structure <fs_line> to <fs_field>.
                  <fs_field> = icon_led_green.
                else.
                  assign component systemid of structure <fs_line> to <fs_field>.
                  <fs_field> = icon_wd_radio_button_empty.
                endif.
              endif.
            endif.
          endif.
        else.
          assign component systemid of structure <fs_line> to <fs_field>.
          <fs_field> = icon_wd_radio_button_empty.
        endif.

        describe table ls_systems-steps lines ultimo_registro .
        read table ls_systems-steps into ls_steps index ultimo_registro .
        read table ls_steps-actions into ls_action index 1 .

        if  sy-subrc eq 0 .

***         Verificar esse filtro de data para transporte em produção
**          if not ls_action-date in s_dtecp.
**            gv_nao_gera = 'X'.
**            continue.
**          endif.

          assign component 'DTECP' of structure <fs_line> to <fs_field>.
          <fs_field> = ls_action-date.
          assign component 'TMECP' of structure <fs_line> to <fs_field>.
          <fs_field> = ls_action-time.

        else.

**          if not s_dtecp[] is initial.
**            gv_nao_gera = 'X'.
**          endif.

          assign component 'DTECP' of structure <fs_line> to <fs_field>.
          clear: <fs_field>.
          assign component 'TMECP' of structure <fs_line> to <fs_field>.
          clear: <fs_field>.

        endif.

      endloop.

      check gv_nao_gera is initial.

      read table tipo into ls_tipo
        with key tipo = ls_e070-trfunction
        binary search.

      if sy-subrc eq 0 .
        assign component 'TRFUNCTION' of structure <fs_line> to <fs_field>.
        <fs_field> = ls_tipo-desc.
      endif.

      read table status into ls_status
        with key status = ls_e070-trstatus
        binary search.

      if sy-subrc eq 0 .
        assign component 'TRSTATUS' of structure <fs_line> to <fs_field>.
        <fs_field> = ls_status-descr.
*       assign component 'KORRDEV' of structure <fs_line> to <fs_field>.
*       <fs_field> = ls_status-korrdev.
      endif.


      read table e07t into ls_e07t
        with key trkorr = ls_e070-trkorr .

      if sy-subrc eq 0 .
        assign component 'DESCREQ' of structure <fs_line> to <fs_field>.
        <fs_field> = ls_e07t-as4text.
      endif.

      assign component 'TRKORR' of structure <fs_line> to <fs_field>.
      <fs_field> = ls_e070-trkorr.
      assign component 'AS4USER' of structure <fs_line> to <fs_field>.
      <fs_field> = ls_e070-as4user.
      assign component 'AS4DATE' of structure <fs_line> to <fs_field>.
      <fs_field> = ls_e070-as4date.
      assign component 'AS4TIME' of structure <fs_line> to <fs_field>.
      <fs_field> = ls_e070-as4time.

      insert <fs_line> into table <fs_table>.

    endloop.

  endmethod .


  method cria_coluna .

    data:
      line type lvc_s_fcat .

    line-fieldname = fieldname .
    line-outputlen = outputlen .
    line-ref_table = ref_table .
    line-ref_field = ref_field .

    append line to fieldcatalog.
    clear  line .

  endmethod.


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


  method link_click .

    data:
      vl_trkorr type trkorr .

    field-symbols:
      <fs_line>  type any,
      <fs_field> type any.

    read table <fs_table> assigning <fs_line> index row .
    if sy-subrc eq 0 .
      assign component 'TRKORR' of structure <fs_line> to <fs_field>.
      if <fs_field> is assigned .
        move <fs_field> to vl_trkorr .
        call function 'TR_LOG_OVERVIEW_REQUEST_REMOTE'
          exporting
            iv_trkorr = vl_trkorr
*           iv_dirtype             =
*           iv_without_check       = ' '
          .
      endif.
    endif.

  endmethod .


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

    me->link_click(
      exporting
        row    = row
        column = column
    ).

*    perform f_on_link_click using row column .
  endmethod.


endclass.                    "lcl_report IMPLEMENTATION



*--------------------------------------------------------------------*
*- Tela de seleção
*--------------------------------------------------------------------*
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

  lcl_report=>initial(
    changing
      ambiente = s_amb[]
      status   = p_stat[]
      data     = p_data[]
  ).


start-of-selection .

  data: lo_report type ref to lcl_report.

  create object lo_report.


  lo_report->get_data(
    exporting
      ambiente     = s_amb[]
      ordem        = p_ordem[]
      tipo         = p_tipo[]
      status       = p_stat[]
      categoria    = p_categ[]
      usuario      = p_user[]
      data         = p_data[]
      data_produca = s_dtecp[]
  ).

  lo_report->generate_output( t_tmscsys = it_tmscsys ).


end-of-selection.
