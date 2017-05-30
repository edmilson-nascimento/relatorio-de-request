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
        ambiente type r_sysnam .

    methods cria_tabela .

    methods get_data
      importing
        !ambiente     type r_sysnam
        !ordem        type /gc1/tab_rng_trkorr
        !tipo         type trg_char1
        !status       type r_status
        !categoria    type trg_char4
        !usuario      type wcft_cc_sel_range_user_tab
        !data         type trg_date
        !data_produca type trg_date .


    methods generate_output .


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

    data:
      table type ref to data .


    methods limpar_dados
      changing
        !e070   type e070_t
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
        !usuario   type wcft_cc_sel_range_user_tab
        !data      type trg_date
      changing
        !e070      type e070_t
        !e07t      type e07t_t .

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
        !e07t   type e07t_t
        !table  type ref to data
      exporting
        !outtab type standard table .

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

*----------------------------------------------------------------------*
*       CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
class lcl_report implementation.


  method initial .

    data:
      ls_status  type ty_r_status,
*     ls_data    type admpn_ti_mfrpn_range,
      ls_sysnam  type ty_r_sysnam,
      ls_tmscsys type ty_tmscsys.

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


  endmethod .

  method get_data.

    data:
      gt_e070   type table of e070,
*     gt_e071   type table of e071,
      gt_e07t   type table of e07t,
      gt_status type table of ty_status,
      gt_tipo   type table of ty_tipo,
      lt_table  type ref to data.

*   perform f_limpa_dados .
    me->limpar_dados(
      changing
        e070    = gt_e070
*       e071    = gt_e071
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
*      importing
*        table = lt_table
*    ).

*   perform f_monta_relatorio .
    me->monta_relatorio(
      exporting
        e070   = gt_e070
        tipo   = gt_tipo
        status = gt_status
        e07t   = gt_e07t
        table  = table
    ).


  endmethod.                    "GET_DATA

  method generate_output .

    data:
      column  type ref to cl_salv_column_list,
      columns type ref to cl_salv_columns_table.
    field-symbols:
      <table> type standard table .


    if table is not initial .
      assign table->* to <table>.
    endif .

    check <table> is assigned .


    try.
        call method cl_salv_table=>factory
          importing
            r_salv_table = lo_table
          changing
            t_table      = <table>.


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

      catch cx_salv_msg .
      catch cx_salv_not_found .
      catch cx_salv_existing .
      catch cx_salv_data_error .
      catch  cx_salv_object_not_found .
    endtry.

  endmethod .                    "generate_output

  method limpar_dados .

    free:
      e070, e07t, status, tipo .

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
*       ep_table                  = new_table
        ep_table                  = table
*       e_style_fname             =     " ALV Control: Field Name of Internal Table Field
      exceptions
        generate_subpool_dir_full = 1
        others                    = 2.

    if sy-subrc eq 0 .

    else .

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
      new_line           type ref to data,

      lv_qtd_task        type n length 4,
      lv_qtd_cont        type n length 4,
      lv_linhas          type n length 4,
      lv_controla_transp type c,
      gv_nao_gera        type c length 1.

    field-symbols:
      <table> type standard table,
      <line>  type any,
      <field> type any.

    if table is not initial .
      assign table->* to <table>.
      if <table> is assigned .
        create data new_line like line of <table>.
        assign new_line->* to <line>.
      endif .
    endif .

    settings-point_to_missing_steps = abap_on .
    settings-detailed_depiction     = abap_on .

    assign new_line->* to <line> .

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
            assign component systemid of structure <line> to <field>.
            <field> = icon_led_red.
          else.
*         Verificando se houve algum warning
            read table ls_systems-steps into ls_steps
                                        with key rc = 4
                                        binary search .
            if sy-subrc eq 0 .
              assign component systemid of structure <line> to <field>.
              <field> = icon_led_yellow.
            else.
*           Verifica a opção de Exportação (significa que é sistema de origem DEV)
              read table ls_systems-steps into ls_steps
                                          with key stepid = 'E' .
              if sy-subrc eq 0 .
                assign component systemid of structure <line> to <field>.
                <field> = icon_led_green.
              else.
*             Verificando se a opção Importação esta aplicada
                read table ls_systems-steps into ls_steps
                                            with key stepid = 'I' .
                if sy-subrc eq 0 .
                  assign component systemid of structure <line> to <field>.
                  <field> = icon_led_green.
                else.
                  assign component systemid of structure <line> to <field>.
                  <field> = icon_wd_radio_button_empty.
                endif.
              endif.
            endif.
          endif.
        else.
          assign component systemid of structure <line> to <field>.
          <field> = icon_wd_radio_button_empty.
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

          assign component 'DTECP' of structure <line> to <field>.
          <field> = ls_action-date.
          assign component 'TMECP' of structure <line> to <field>.
          <field> = ls_action-time.

        else.

**          if not s_dtecp[] is initial.
**            gv_nao_gera = 'X'.
**          endif.

          assign component 'DTECP' of structure <line> to <field>.
          clear: <field>.
          assign component 'TMECP' of structure <line> to <field>.
          clear: <field>.

        endif.

      endloop.

      check gv_nao_gera is initial.

      read table tipo into ls_tipo
        with key tipo = ls_e070-trfunction
        binary search.

      if sy-subrc eq 0 .
        assign component 'TRFUNCTION' of structure <line> to <field>.
        <field> = ls_tipo-desc.
      endif.

      read table status into ls_status
        with key status = ls_e070-trstatus
        binary search.

      if sy-subrc eq 0 .
        assign component 'TRSTATUS' of structure <line> to <field>.
        <field> = ls_status-descr.
*       assign component 'KORRDEV' of structure <fs_line> to <fs_field>.
*       <fs_field> = ls_status-korrdev.
      endif.


      read table e07t into ls_e07t
        with key trkorr = ls_e070-trkorr .

      if sy-subrc eq 0 .
        assign component 'DESCREQ' of structure <line> to <field>.
        <field> = ls_e07t-as4text.
      endif.

      assign component 'TRKORR' of structure <line> to <field>.
      <field> = ls_e070-trkorr.
      assign component 'AS4USER' of structure <line> to <field>.
      <field> = ls_e070-as4user.
      assign component 'AS4DATE' of structure <line> to <field>.
      <field> = ls_e070-as4date.
      assign component 'AS4TIME' of structure <line> to <field>.
      <field> = ls_e070-as4time.

      insert <line> into table <table>.

    endloop.

    if lines( <table>[] ) eq 0 .
    else .
      append lines of <table> to outtab .
    endif .

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
      trkorr type trkorr .

    field-symbols:
      <table> type standard table,
      <line>  type any,
      <field> type any.

    if table is not initial .

      assign table->* to <table>.

      if <table> is assigned .

        read table <table> assigning <line> index row .

        if sy-subrc eq 0 .

          assign component 'TRKORR' of structure <line> to <field>.

          if <field> is assigned .

            trkorr = <field>  .

            call function 'TR_LOG_OVERVIEW_REQUEST_REMOTE'
              exporting
                iv_trkorr = trkorr
*               iv_dirtype             =
*               iv_without_check       = ' '
              .
          endif.

        endif.

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
data:
  lo_report type ref to lcl_report.

* Verificar forma de mudar para entro do método
field-symbols:
  <table> type standard table .


*--------------------------------------------------------------------*
*- Tela de seleção
*--------------------------------------------------------------------*
selection-screen begin of block b1 with frame title text-001.

select-options: s_amb for trtarget-tarsystem no intervals obligatory,
                p_ordem for  e070-trkorr,
                p_tipo  for  e070-trfunction,
                p_stat  for  e070-trstatus default 'R',
                p_categ for  e070-korrdev,
                p_user  for  e070-as4user default sy-uname ,
                p_data  for  e070-as4date default sy-datum ,
                s_dtecp for  e070-as4date.
selection-screen end of block b1.

*--------------------------------------------------------------------*
*- Eventos
*--------------------------------------------------------------------*
initialization.

  lcl_report=>initial(
    changing
      ambiente = s_amb[]
  ).


start-of-selection .

  create object lo_report.

  lo_report->cria_tabela( ).

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


end-of-selection.

  lo_report->generate_output( ) .
