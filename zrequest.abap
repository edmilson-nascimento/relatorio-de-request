REPORT ZREQUEST.

*--------------------------------------------------------------------*
*- Anotações
*--------------------------------------------------------------------*
*- Coloca icone ou mesmo informação para Modificavel e etc

*--------------------------------------------------------------------*
*- Tipos SAP
*--------------------------------------------------------------------*
type-pools:
  sscr, vrm, ctslg, icon .

*--------------------------------------------------------------------*
*- Tabelas
*--------------------------------------------------------------------*
tables:
  e070, trtarget.

*----------------------------------------------------------------------*
*       CLASS lcl_report DEFINITION
*----------------------------------------------------------------------*
class class_report definition .

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

      begin of ty_categoria,
        categoria type trfunction,
        desc      type c length 60,
      end of ty_categoria,

      categoria_tab type table of ty_categoria,

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

    methods create_structure
      importing
        !ambiente type r_sysnam .

    methods get_data
      importing
        !ambiente  type r_sysnam
        !ordem     type /gc1/tab_rng_trkorr
        !tipo      type trg_char1
        !status    type r_status
        !categoria type trg_char4
        !usuario   type wcft_cc_sel_range_user_tab
        !data      type trg_date .

    methods generate_output .


  protected section .

    methods on_link_click
      for event if_salv_events_actions_table~link_click
                  of cl_salv_events_table
      importing row
                  column.

    methods on_added_function
      for event if_salv_events_functions~added_function
                  of cl_salv_events_table
      importing e_salv_function.

  private section .

    data:
      table          type ref to data,
      ambiente       type r_sysnam,
      i_ordem        type /gc1/tab_rng_trkorr,
      i_tipo         type trg_char1,
      i_status       type r_status,
      i_categoria    type trg_char4,
      i_usuario      type wcft_cc_sel_range_user_tab,
      i_data         type trg_date,
      i_data_produca type datum.


    methods limpar_dados
      changing
        !e070   type tt_e070
        !e07t   type tt_e07t
        !status type status_tab
        !tipo   type tipo_tab .

    methods carrega_descricao
      changing
        !tipo      type tipo_tab
        !status    type status_tab
        !categoria type categoria_tab .

    methods seleciona_dados
      importing
        !ordem     type /gc1/tab_rng_trkorr
        !tipo      type trg_char1
        !status    type r_status
        !categoria type trg_char4
        !usuario   type wcft_cc_sel_range_user_tab
        !data      type trg_date
      changing
        !e070      type tt_e070
        !e07t      type tt_e07t .

    methods cria_coluna
      importing
        !fieldname    type lvc_fname
        !outputlen    type lvc_outlen
        !ref_table    type lvc_rtname
        !ref_field    type lvc_rfname
        !text_l       type lvc_s_fcat-scrtext_l optional
        !text_m       type lvc_s_fcat-scrtext_m optional
        !text_s       type lvc_s_fcat-scrtext_s optional
      changing
        !fieldcatalog type lvc_t_fcat .

    methods cria_coluna_ambiente
      importing
        !ambiente     type class_report=>r_sysnam
      changing
        !fieldcatalog type lvc_t_fcat .


    methods create_date_time
      importing
        !sysnam  type sysname
      changing
        !catalog type lvc_t_fcat .

    methods monta_relatorio
      importing
        !e070      type tt_e070
        !status    type status_tab
        !tipo      type tipo_tab
        !categoria type categoria_tab
        !e07t      type tt_e07t
        !table     type ref to data
      exporting
        !outtab    type standard table .

    methods atualiza_atributos
      importing
        !ambiente  type r_sysnam
        !ordem     type /gc1/tab_rng_trkorr
        !tipo      type trg_char1
        !status    type r_status
        !categoria type trg_char4
        !usuario   type wcft_cc_sel_range_user_tab
        !data      type trg_date .

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

    methods process .

    methods change_tmscsys
      importing
        !ambiente type r_sysnam .

    methods assign
      importing
        !field type any
        !value type any
      changing
        !line  type any .

    methods assign_log
      importing
        !field type any
        !steps type ctslg_steps
      changing
        !line  type any .

    methods get_data_refresh .

endclass.                    "lcl_report DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
class class_report implementation.



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

*   Para ambiente Panpharma
    ls_sysnam-sign    = 'I' .
    ls_sysnam-option  = 'EQ' .
    ls_sysnam-low     = 'DV2' .
    append ls_sysnam to ambiente .
    ls_tmscsys-sysnam = ls_sysnam-low .
    append ls_tmscsys to t_tmscsys .
    clear:
      ls_sysnam, ls_tmscsys .


    ls_sysnam-sign   = 'I' .
    ls_sysnam-option = 'EQ' .
    ls_sysnam-low    = 'QAS' .
    append ls_sysnam to ambiente .
    ls_tmscsys-sysnam = ls_sysnam-low .
    append ls_tmscsys to t_tmscsys .
    clear:
      ls_sysnam, ls_tmscsys .

    ls_sysnam-sign   = 'I' .
    ls_sysnam-option = 'EQ' .
    ls_sysnam-low    = 'PRD' .
    append ls_sysnam to ambiente .
    ls_tmscsys-sysnam = ls_sysnam-low .
    append ls_tmscsys to t_tmscsys .
    clear:
      ls_sysnam, ls_tmscsys .

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

    if ( sy-subrc eq 0 ) .
    else .
      message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.


  endmethod .                    "initial

  method get_data.

    data:
      gt_e070      type table of e070,
      gt_e07t      type table of e07t,
      gt_status    type table of ty_status,
      gt_tipo      type table of ty_tipo,
      gt_categoria type table of ty_categoria,
      lt_table     type ref to data.

    me->limpar_dados(
      changing
        e070    = gt_e070
        e07t    = gt_e07t
        status  = gt_status
        tipo    = gt_tipo
    ).

    me->carrega_descricao(
      changing
        tipo      = gt_tipo
        status    = gt_status
        categoria = gt_categoria
    ).

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

    me->monta_relatorio(
      exporting
        e070      = gt_e070
        status    = gt_status
        tipo      = gt_tipo
        categoria = gt_categoria
        e07t      = gt_e07t
        table     = table
    ).

    me->atualiza_atributos(
      exporting
        ambiente     = ambiente
        ordem        = ordem
        tipo         = tipo
        status       = status
        categoria    = categoria
        usuario      = usuario
        data         = data
    ) .


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

        set handler me->on_link_click for lo_events.
        set handler me->on_added_function for lo_events.

        lo_table->set_screen_status(
          pfstatus      = 'STANDARD_FULLSCREEN'
          report        = sy-cprog
          set_functions = lo_table->c_functions_all ).


        columns = lo_table->get_columns( ).

        columns->set_optimize( 'X' ).
        column ?= columns->get_column( 'TRKORR' ).
        column->set_cell_type( if_salv_c_cell_type=>hotspot ).

        column ?= columns->get_column( 'TRFUNCTION' ).
        column->set_long_text( 'Tipo de request' ).

        me->set_text_output(
          exporting
            t_tmscsys = t_tmscsys
          changing
            table     = lo_table
        ).

*       Layout de Zebra
        lo_display = lo_table->get_display_settings( ) .
        lo_display->set_striped_pattern(  cl_salv_display_settings=>true ) .

**       Ordenação de campos
*        lo_sorts = lo_table->get_sorts( ) .
*        lo_sorts->add_sort('AS4DATE') .
*        lo_sorts->add_sort('AS4TIME') .
*        lo_sorts->add_sort('TRKORR') .

        lo_table->display( ).

      catch cx_salv_msg .
      catch cx_salv_not_found .
      catch cx_salv_existing .
      catch cx_salv_data_error .
      catch cx_salv_object_not_found .

    endtry.

  endmethod .                    "generate_output

  method limpar_dados .

    field-symbols:
      <table> type standard table .

    free:
      e070, e07t, status, tipo .

    if table is not initial .
      assign table->* to <table>.
      if <table> is assigned .
        refresh:
          <table> .
      endif .
    endif .

  endmethod .                    "limpar_dados

  method carrega_descricao .

    data:
      lt_list      type table of vrm_value,
      ls_list      type          vrm_value,
      ls_tipo      type ty_tipo,
      ls_status    type ty_status,
      ls_categoria type ty_categoria.

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

    refresh lt_list .
    clear   ls_list .

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
        i_field_name = 'KORRDEV'
        i_langu      = sy-langu
      importing
        e_t_list     = lt_list.

    loop at lt_list into ls_list.
      ls_categoria-categoria = ls_list-key .
      ls_categoria-desc      = ls_list-text .
      append ls_categoria to categoria .
      clear  ls_categoria .
    endloop.

    sort categoria ascending by categoria .

    refresh lt_list .
    clear   ls_list .


  endmethod .                    "carrega_descricao


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

    delete e070 where trkorr is initial .

    if lines( e070 ) eq 0 .

    else .

      select *
        from e07t
        into table e07t
         for all entries in e070
       where trkorr eq e070-trkorr
         and ( langu  eq 'PT' or langu  eq 'EN' ).

    endif.

  endmethod.                    "seleciona_dados


  method create_structure .

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

*    me->cria_coluna(
*      exporting
*        fieldname    = 'AS4DATE'
*        outputlen    = 8
*        ref_table    = 'E070'
*        ref_field    = 'AS4DATE'
*      changing
*        fieldcatalog = lt_fieldcat
*    ).
*    me->cria_coluna(
*      exporting
*        fieldname    = 'AS4TIME'
*        outputlen    = 6
*        ref_table    = 'E070'
*        ref_field    = 'AS4TIME'
*      changing
*        fieldcatalog = lt_fieldcat
*    ).

    me->cria_coluna_ambiente(
      exporting
        ambiente     = ambiente
      changing
        fieldcatalog = lt_fieldcat
    ).

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


  method monta_relatorio .

    data:
      ls_tmscys          type ty_tmscsys,
      ls_e070            type e070,
      ls_status          type ty_status,
      ls_tipo            type ty_tipo,
      ls_categoria       type ty_categoria,
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

      assign new_line->* to <line> .
      check <line> is assigned .

*     Request
      me->assign(
        exporting
          field = 'TRKORR'
          value = ls_e070-trkorr
        changing
          line  = <line>
      ).

*     Descricao da Request
      read table e07t into ls_e07t
        with key trkorr = ls_e070-trkorr .

      if ( sy-subrc eq 0 ) .

        me->assign(
          exporting
            field = 'DESCREQ'
            value = ls_e07t-as4text
          changing
            line  = <line>
        ).

      endif.

*     Usuario da Request
      me->assign(
        exporting
          field = 'AS4USER'
          value = ls_e070-as4user
        changing
          line  = <line>
      ).

*     Tipo de Request
      read table tipo into ls_tipo
        with key tipo = ls_e070-trfunction .

      if ( sy-subrc eq 0 ) .

        me->assign(
          exporting
            field = 'TRFUNCTION'
            value = ls_tipo-desc
          changing
            line  = <line>
        ).

      endif.

*     Status da Request
      read table status into ls_status
        with key status = ls_e070-trstatus .

      if ( sy-subrc eq 0 ) .

        me->assign(
          exporting
            field = 'TRSTATUS'
            value = ls_e070-trstatus
          changing
            line  = <line>
        ).

      endif.

*     Categoria da Request
      read table categoria into ls_categoria
        with key categoria = ls_e070-korrdev .

      if ( sy-subrc eq 0 ) .

        me->assign(
          exporting
            field = 'KORRDEV'
            value = ls_e070-korrdev
          changing
            line  = <line>
        ).

*        me->assign(
*          exporting
*            field = 'KORRDEV_TEXT'
*            value = ls_categoria-desc
*          changing
*            line  = <line>
*        ).

      endif .


      call function 'TR_READ_GLOBAL_INFO_OF_REQUEST'
        exporting
          iv_trkorr   = ls_e070-trkorr
          iv_dir_type = 'T'
          is_settings = settings
        importing
          es_cofile   = cofile.


      loop at t_tmscsys into ls_tmscys .

        systemid = ls_tmscys-sysnam .

*       Para DEV, o status usado sera "Exportação"
*       Para os demais ambientes, sera "Importação encerrada"

*       Buscando log espeficico de cada Sistema
        read table cofile-systems into ls_systems
                                  with key systemid = systemid .
        if ( sy-subrc eq 0 ) .

*         Verificando se houve algum erro
          read table ls_systems-steps into ls_steps
            with key rc = 8 .
          if ( sy-subrc eq 0 ) .

            me->assign(
              exporting
                field = systemid
                value = icon_led_red
              changing
                line  = <line>
            ).

            me->assign_log(
              exporting
                field  = systemid
                steps  = ls_systems-steps
              changing
                line   = <line>
            ).

          else.

*           Verificando se houve algum warning
            read table ls_systems-steps into ls_steps
              with key rc = 4 .
            if ( sy-subrc eq 0 ) .

              me->assign(
                exporting
                  field = systemid
                  value = icon_led_yellow
                changing
                  line  = <line>
              ).

              me->assign_log(
                exporting
                  field  = systemid
                  steps  = ls_systems-steps
                changing
                  line   = <line>
              ).

            else.

*             Verifica a opção de Exportação (significa que é sistema de origem DEV)
              read table ls_systems-steps into ls_steps
                with key stepid = 'E' .
              if (  sy-subrc eq 0 ) .

                me->assign(
                  exporting
                    field = systemid
                    value = icon_led_green
                  changing
                    line  = <line>
                ).

                me->assign_log(
                  exporting
                    field  = systemid
                    steps  = ls_systems-steps
                  changing
                    line   = <line>
                ).

              else.

*               Verificando se a opção Importação esta aplicada
                read table ls_systems-steps into ls_steps
                                            with key stepid = 'I' .
                if ( sy-subrc eq 0 ) .

                  me->assign(
                    exporting
                      field = systemid
                      value = icon_led_green
                    changing
                      line  = <line>
                  ) .

                  me->assign_log(
                    exporting
                      field  = systemid
                      steps  = ls_systems-steps
                    changing
                      line   = <line>
                  ).

                else.

                  me->assign(
                    exporting
                      field = systemid
                      value = icon_wd_radio_button_empty
                    changing
                      line  = <line>
                  ).

                endif.

              endif .

            endif .

          endif.

        else.

          me->assign(
            exporting
              field = systemid
              value = icon_wd_radio_button_empty
            changing
              line  = <line>
          ) .

        endif.

      endloop.


      insert <line> into table <table>.
      unassign <line> .

    endloop.

    if lines( <table>[] ) eq 0 .
    else .
      append lines of <table> to outtab .
    endif .

  endmethod .                    "monta_relatorio


  method cria_coluna .

    data:
      line type lvc_s_fcat .

    line-fieldname = fieldname .
    line-outputlen = outputlen .
    line-ref_table = ref_table .
    line-ref_field = ref_field .

    if ( text_l is not initial ) .
      line-scrtext_l = text_l .
    endif .
    if ( text_m is not initial ) .
      line-scrtext_m = text_m .
    endif .
    if ( text_s is not initial ) .
      line-scrtext_s = text_s .
    endif .

    append line to fieldcatalog.
    clear  line .

  endmethod.                    "cria_coluna


  method cria_coluna_ambiente .

    data:
      fieldname type lvc_fname .

*  Verificando os ambiente passados como parametro
    if ( lines( ambiente ) eq 0 ) .

    else .


      loop at t_tmscsys into data(line_tmscsys) .

        fieldname = line_tmscsys-sysnam .
        me->cria_coluna(
          exporting
            fieldname    = fieldname
            outputlen    = 10
            ref_table    = ''
            ref_field    = ''
          changing
            fieldcatalog = fieldcatalog
        ).

        fieldname = |DT{ line_tmscsys-sysnam }| .
        me->cria_coluna(
          exporting
            fieldname    = fieldname
            outputlen    = 10
            ref_table    = 'SYST'
            ref_field    = 'DATUM'
          changing
            fieldcatalog = fieldcatalog
        ).

        fieldname = |TM{ line_tmscsys-sysnam }| .
        me->cria_coluna(
          exporting
            fieldname    = fieldname
            outputlen    = 10
            ref_table    = 'SYST'
            ref_field    = 'UZEIT'
          changing
            fieldcatalog = fieldcatalog
        ).

      endloop .

    endif .


  endmethod .


  method create_date_time .

    data:
      text_l type lvc_s_fcat-scrtext_l,
      text_m type lvc_s_fcat-scrtext_m,
      text_s type lvc_s_fcat-scrtext_s.

    if ( sysnam is initial ) .
    else .

      text_l = text_m = text_s = sysnam .

      me->cria_coluna(
        exporting
          fieldname    = |AS4DATE_{ sysnam }|
          outputlen    = 8
          ref_table    = 'E070'
          ref_field    = 'AS4DATE'
          text_l       = text_l
          text_m       = text_m
          text_s       = text_s
        changing
          fieldcatalog = catalog
      ).


      me->cria_coluna(
        exporting
          fieldname    = |AS4TIME_{ sysnam }|
          outputlen    = 6
          ref_table    = 'E070'
          ref_field    = 'AS4TIME'
          text_l       = text_l
          text_m       = text_m
          text_s       = text_s
        changing
          fieldcatalog = catalog
      ).

    endif .

  endmethod .

  method atualiza_atributos .

    if lines( i_ordem ) eq 0 .
      if lines( ordem ) eq 0 .
      else .
        append lines of ordem to i_ordem .
      endif .
    else .
    endif .

    if lines( i_tipo ) eq 0 .
      if lines( tipo ) eq 0 .
      else .
        append lines of tipo to i_tipo .
      endif .
    else .
    endif .

    if lines( i_status ) eq 0 .
      if lines( status ) eq 0 .
      else .
        append lines of status to i_status .
      endif .
    else .
    endif .

    if lines( i_categoria ) eq 0 .
      if lines( categoria ) eq 0 .
      else .
        append lines of categoria to i_categoria .
      endif .
    else .
    endif .

    if lines( i_usuario ) eq 0 .
      if lines( usuario ) eq 0 .
      else .
        append lines of usuario to i_usuario .
      endif .
    else .
    endif .

    if lines( i_data ) eq 0 .
      if lines( data ) eq 0 .
      else .
        append lines of data to i_data .
      endif .
    else .
    endif .

  endmethod .                    "atualiza_atributos

  method set_text .

    try.
        c_column ?= c_columns->get_column( i_field ).
        c_column->set_short_text( i_short_text ).
        c_column->set_medium_text( i_medium_text ).
        c_column->set_long_text( i_long_text ).
      catch cx_salv_not_found .
    endtry .

  endmethod .                    "set_text

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

      field = |DT{ line-sysnam }| .
      long_text = medium_text = short_text = |Data({ line-sysnam })| .
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

      field = |TM{ line-sysnam }| .
      long_text = medium_text = short_text = |Hora({ line-sysnam })| .
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

  endmethod.                    "set_text_output


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

  endmethod .                    "link_click


  method process .

    data:
      value type salv_t_row,
      line  type i.

    field-symbols:
      <line> type snwd_texts .

    case sy-ucomm .

      when 'REFRESH' .

        me->get_data_refresh( ) .

        if me->lo_table is bound .

          me->lo_table->refresh( ) .

        endif .

      when others .

    endcase .

  endmethod .                    "process


  method change_tmscsys .


*   #verificar: inibir abas no parametro s_amb

    data:
      line       type ty_r_sysnam,
      lt_tmscsys type tmscsys_tab,
      ls_tmscsys type ty_tmscsys.

    if ambiente[] is not initial .

      do .

        read table ambiente into line index sy-index .

        if sy-subrc eq 0 .

          read table t_tmscsys into ls_tmscsys
            with key sysnam = line-low .

          if sy-subrc eq 0 .

            append ls_tmscsys to lt_tmscsys .
            clear  ls_tmscsys .

          endif .

        else .
          exit .
        endif.

      enddo .

      if lines( lt_tmscsys ) eq 0 .
      else .
        refresh t_tmscsys .
        append lines of lt_tmscsys to t_tmscsys .
      endif .

    endif .

    free:
      lt_tmscsys .

  endmethod.                    "change_tmscsys

  method assign .

    field-symbols:
      <field> type any .


    assign component field of structure line to <field>.

    if <field> is assigned .

      <field> = value .

      unassign
        <field> .

    endif .

  endmethod .                    "assign


  method assign_log .

    data:
      fieldname    type char10,
      steps_line   type ctslg_step,
      actions_line type ctslg_action.

    if ( lines( steps ) eq 0 ) .
    else .

      read table steps into steps_line index lines( steps ) .
      if ( sy-subrc eq 0 ) .

        read table steps_line-actions into actions_line index 1 .
        if ( sy-subrc eq 0 ) .

          fieldname = |DT{ field }|.
          me->assign(
            exporting
              field = fieldname
              value = actions_line-date
            changing
              line  = line
          ).

          fieldname = |TM{ field }|.
          me->assign(
            exporting
              field = fieldname
              value = actions_line-time
            changing
              line  = line
          ).

        endif .

      endif .

    endif .

  endmethod .

  method get_data_refresh .

    me->get_data(
      exporting
        ambiente     = ambiente
        ordem        = i_ordem
        tipo         = i_tipo
        status       = i_status
        categoria    = i_categoria
        usuario      = i_usuario
        data         = i_data
    ).

  endmethod .                    "get_data_refresh

  method on_link_click.

    me->link_click(
      exporting
        row    = row
        column = column
    ).

  endmethod.                    "on_link_click

  method on_added_function .

    me->process( ) .

  endmethod .                    "on_added_function

endclass.                    "lcl_report IMPLEMENTATION


*--------------------------------------------------------------------*
*- Tela de seleção
*--------------------------------------------------------------------*
data:
  report type ref to class_report.

*--------------------------------------------------------------------*
*- Tela de seleção
*--------------------------------------------------------------------*
selection-screen begin of block b1 with frame title text-001.

select-options:
  s_amb for trtarget-tarsystem no intervals obligatory,
  p_ordem for  e070-trkorr,
  p_tipo  for  e070-trfunction,
  p_stat  for  e070-trstatus default 'R',
  p_categ for  e070-korrdev,
  p_user  for  e070-as4user default sy-uname ,
  p_data  for  e070-as4date default sy-datum .
*parameters:
*  p_produ type e070-as4date .
selection-screen end of block b1.

*--------------------------------------------------------------------*
*- Eventos
*--------------------------------------------------------------------*
initialization.

  class_report=>initial(
    changing
      ambiente = s_amb[]
  ).


start-of-selection .

  create object report.

  report->create_structure( ambiente = s_amb[] ).

  report->get_data(
    exporting
      ambiente     = s_amb[]
      ordem        = p_ordem[]
      tipo         = p_tipo[]
      status       = p_stat[]
      categoria    = p_categ[]
      usuario      = p_user[]
      data         = p_data[]
  ).


end-of-selection.

  report->generate_output( ) .
