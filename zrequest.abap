*&---------------------------------------------------------------------*
*& Report YTESTE
*&---------------------------------------------------------------------*
REPORT yteste.


*--------------------------------------------------------------------*
*- Tipos SAP
*--------------------------------------------------------------------*
TYPE-POOLS:
  sscr, vrm, ctslg, icon .

*--------------------------------------------------------------------*
*- Tabelas
*--------------------------------------------------------------------*
TABLES:
  e070, trtarget.

*----------------------------------------------------------------------*
*       CLASS lcl_report DEFINITION
*----------------------------------------------------------------------*
CLASS class_report DEFINITION .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_tmscsys,
        domnam TYPE tmscsys-domnam,
        sysnam TYPE tmscsys-sysnam,
        limbo  TYPE tmscsys-limbo,
      END OF ty_tmscsys,

      tmscsys_tab TYPE TABLE OF ty_tmscsys,

      BEGIN OF ty_status,
        status TYPE trstatus,
        descr  TYPE c LENGTH 60,
      END OF ty_status,

      status_tab TYPE TABLE OF ty_status,

      BEGIN OF ty_tipo,
        tipo TYPE trfunction,
        desc TYPE c LENGTH 42,
      END OF ty_tipo,

      tipo_tab TYPE TABLE OF ty_tipo,

      BEGIN OF ty_categoria,
        categoria TYPE trfunction,
        desc      TYPE c LENGTH 60,
      END OF ty_categoria,

      categoria_tab TYPE TABLE OF ty_categoria,

      BEGIN OF ty_r_sysnam,
        sign   TYPE ddsign,
        option TYPE ddoption,
        low    TYPE trtarget-tarsystem,
        high   TYPE trtarget-tarsystem,
      END OF ty_r_sysnam,

      r_sysnam TYPE TABLE OF ty_r_sysnam,

      BEGIN OF ty_r_status,
        sign   TYPE ddsign,
        option TYPE ddoption,
        low    TYPE trstatus,
        high   TYPE trstatus,
      END OF ty_r_status,

      r_status TYPE TABLE OF ty_r_status.

    CLASS-DATA:
      gt_tmscsys  TYPE tmscsys_tab .

    DATA:
      lo_table   TYPE REF TO cl_salv_table,
      lo_events  TYPE REF TO cl_salv_events_table,
      lo_display TYPE REF TO cl_salv_display_settings,
      lo_sorts   TYPE REF TO cl_salv_sorts.

    CLASS-METHODS initial
      CHANGING
        ct_ambient TYPE r_sysnam .

    METHODS create_structure
      IMPORTING
        !it_ambient TYPE r_sysnam .

    METHODS get_data
      IMPORTING
        !it_ambient    TYPE r_sysnam
        !it_request    TYPE /gc1/tab_rng_trkorr
        !it_type       TYPE trg_char1
        !it_status     TYPE r_status
        !it_trfunction TYPE trg_char4
        !it_user       TYPE wcft_cc_sel_range_user_tab
        !it_date       TYPE trg_date .

    METHODS generate_output .

  PROTECTED SECTION .

    METHODS on_link_click
      FOR EVENT if_salv_events_actions_table~link_click
                OF cl_salv_events_table
      IMPORTING row
                column .

    METHODS on_added_function
      FOR EVENT if_salv_events_functions~added_function
                OF cl_salv_events_table
      IMPORTING e_salv_function .

  PRIVATE SECTION .

    DATA:
      go_data       TYPE REF TO data,
      gt_ambient    TYPE r_sysnam,
      gt_request    TYPE /gc1/tab_rng_trkorr,
      gt_type       TYPE trg_char1,
      gt_status     TYPE r_status,
      gt_trfunction TYPE trg_char4,
      gt_user       TYPE wcft_cc_sel_range_user_tab,
      gt_date       TYPE trg_date.

    METHODS clear_data
      CHANGING
        !ct_e070   TYPE tt_e070
        !gt_e07t   TYPE tt_e07t
        !ct_status TYPE status_tab
        !ct_type   TYPE tipo_tab .

    METHODS get_description
      CHANGING
        !ct_type       TYPE tipo_tab
        !ct_status     TYPE status_tab
        !ct_trfunction TYPE categoria_tab .

    METHODS search_data
      IMPORTING
        !it_request    TYPE /gc1/tab_rng_trkorr
        !it_type       TYPE trg_char1
        !it_status     TYPE r_status
        !it_trfunction TYPE trg_char4
        !it_user       TYPE wcft_cc_sel_range_user_tab
        !it_date       TYPE trg_date
      CHANGING
        !ct_e070       TYPE tt_e070
        !ct_e07t       TYPE tt_e07t .

    METHODS set_request_data
      IMPORTING
        !e070      TYPE e070
        !e07t      TYPE e07t
        !status    TYPE class_report=>ty_status
        !tipo      TYPE class_report=>ty_tipo
        !categoria TYPE class_report=>ty_categoria
      CHANGING
        !line      TYPE any .

    METHODS create_column
      IMPORTING
        !fieldname    TYPE lvc_fname
        !outputlen    TYPE lvc_outlen
        !ref_table    TYPE lvc_rtname OPTIONAL
        !ref_field    TYPE lvc_rfname OPTIONAL
        !text_l       TYPE lvc_s_fcat-scrtext_l OPTIONAL
        !text_m       TYPE lvc_s_fcat-scrtext_m OPTIONAL
        !text_s       TYPE lvc_s_fcat-scrtext_s OPTIONAL
      CHANGING
        !fieldcatalog TYPE lvc_t_fcat .

    METHODS create_ambient_column
      IMPORTING
        !ambiente     TYPE class_report=>r_sysnam
      CHANGING
        !fieldcatalog TYPE lvc_t_fcat .


    METHODS create_date_time
      IMPORTING
        !iv_sysnam  TYPE sysname
      CHANGING
        !ct_catalog TYPE lvc_t_fcat .

    METHODS create_report
      IMPORTING
        !it_e070       TYPE tt_e070
        !it_status     TYPE status_tab
        !it_type       TYPE tipo_tab
        !it_trfunction TYPE categoria_tab
        !it_e07t       TYPE tt_e07t
        !table         TYPE REF TO data
      EXPORTING
        !et_outtab     TYPE STANDARD TABLE .

    METHODS update_attributes
      IMPORTING
        !ambiente  TYPE r_sysnam
        !ordem     TYPE /gc1/tab_rng_trkorr
        !tipo      TYPE trg_char1
        !status    TYPE r_status
        !categoria TYPE trg_char4
        !usuario   TYPE wcft_cc_sel_range_user_tab
        !data      TYPE trg_date .

    METHODS set_text
      IMPORTING
        !i_field       TYPE lvc_fname
        !i_long_text   TYPE scrtext_l
        !i_medium_text TYPE scrtext_m
        !i_short_text  TYPE scrtext_s
      CHANGING
        !c_columns     TYPE REF TO cl_salv_columns_table
        !c_column      TYPE REF TO cl_salv_column_list.

    METHODS set_text_output
      IMPORTING
        !t_tmscsys TYPE tmscsys_tab
      CHANGING
        !table     TYPE REF TO cl_salv_table .

    METHODS link_click
      IMPORTING
        !row    TYPE any
        !column TYPE any .

    METHODS process .

    METHODS change_tmscsys
      IMPORTING
        !ambiente TYPE r_sysnam .

    METHODS assign
      IMPORTING
        !field TYPE any
        !value TYPE any
      CHANGING
        !line  TYPE any .

    METHODS assign_log
      IMPORTING
        !field TYPE any
        !steps TYPE ctslg_steps
      CHANGING
        !line  TYPE any .

    METHODS get_data_refresh .

ENDCLASS.                    "lcl_report DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS class_report IMPLEMENTATION.


  METHOD initial .

    DATA:
      ls_status     TYPE ty_r_status,
      ls_sysnam     TYPE ty_r_sysnam,
      ls_tmscsys    TYPE ty_tmscsys,
      opt_list      TYPE sscr_opt_list,
      ass           TYPE sscr_ass,
      restriction   TYPE sscr_restrict,

      lt_db_ambient TYPE tmscsyss.

    lt_db_ambient = me->get_ambients( ) .

    ct_ambient =
    VALUE #( LET s = rsmds_c_sign-including
                 o = rsmds_c_option-equal
             IN sign   = s
                option = o
             ( low = 'D01' )
             ( low = 'Q01' )
             ( low = 'P01' ) ) .

    gt_tmscsys =
      VALUE #( FOR a IN ct_ambient ( sysnam = a-low ) ) .

    restriction =
      VALUE #( opt_list_tab = VALUE #( ( name       = 'OBJECTKEY1'
                                         options-eq = abap_on ) )
               ass_tab      = VALUE #( ( kind    = 'S'
                                         name    = 'S_AMB'
                                         sg_main = 'I'
                                         sg_addy = abap_off
                                         op_main = 'OBJECTKEY1' ) ) ) .

    CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
      EXPORTING
*       program                =
        restriction            = restriction
*       db                     = SPACE
      EXCEPTIONS
        too_late               = 1
        repeated               = 2
        selopt_without_options = 3
        selopt_without_signs   = 4
        invalid_sign           = 5
        empty_option_list      = 6
        invalid_kind           = 7
        repeated_kind_a        = 8
        OTHERS                 = 9.

    IF ( sy-subrc EQ 0 ) .
    ELSE .
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


  ENDMETHOD .                    "initial


  METHOD get_data.

    DATA:
      gt_e070      TYPE TABLE OF e070,
      gt_e07t      TYPE TABLE OF e07t,
      gt_status    TYPE TABLE OF ty_status,
      gt_tipo      TYPE TABLE OF ty_tipo,
      gt_categoria TYPE TABLE OF ty_categoria,
      lt_table     TYPE REF TO data.

    me->clear_data(
      CHANGING
        ct_e070    = gt_e070
        gt_e07t    = gt_e07t
        ct_status  = gt_status
        ct_type    = gt_tipo
    ).

    me->get_description(
      CHANGING
        ct_type      = gt_tipo
        ct_status    = gt_status
        ct_trfunction = gt_categoria
    ).

    me->search_data(
      EXPORTING
        it_request     = it_request
        it_type      = it_type
        it_status    = it_status
        it_trfunction = it_trfunction
        it_user   = it_user
        it_date      = it_date
      CHANGING
        ct_e070      = gt_e070
        ct_e07t      = gt_e07t
    ).

    me->create_report(
      EXPORTING
        it_e070       = gt_e070
        it_status     = gt_status
        it_type       = gt_tipo
        it_trfunction = gt_categoria
        it_e07t       = gt_e07t
        table         = go_data
    ).

    me->update_attributes(
      EXPORTING
        ambiente     = it_ambient
        ordem        = it_request
        tipo         = it_type
        status       = it_status
        categoria    = it_trfunction
        usuario      = it_user
        data         = it_date
    ) .


  ENDMETHOD.                    "GET_DATA


  METHOD generate_output .

    DATA:
      column  TYPE REF TO cl_salv_column_list,
      columns TYPE REF TO cl_salv_columns_table.

    FIELD-SYMBOLS:
      <table> TYPE STANDARD TABLE .


    IF go_data IS NOT INITIAL .
      ASSIGN go_data->* TO <table>.
    ENDIF .

    CHECK <table> IS ASSIGNED .

    TRY.

        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_table
          CHANGING
            t_table      = <table>.

        lo_events = lo_table->get_event( ).

        SET HANDLER me->on_link_click FOR lo_events.
        SET HANDLER me->on_added_function FOR lo_events.

        lo_table->set_screen_status( pfstatus      = 'STANDARD_FULLSCREEN'
                                     report        = 'SAPLKKBL'
                                     set_functions = lo_table->c_functions_all ) .

        columns = lo_table->get_columns( ).

        columns->set_optimize( 'X' ).
        column ?= columns->get_column( 'TRKORR' ).
        column->set_cell_type( if_salv_c_cell_type=>hotspot ).

        column ?= columns->get_column( 'TRFUNCTION' ).
        column->set_long_text( 'Tipo de request' ).

        me->set_text_output( EXPORTING t_tmscsys = gt_tmscsys
                             CHANGING  table     = lo_table ) .

        column ?= columns->get_column( 'OBJECTS' ).
        IF ( column IS BOUND ) .
          column->set_icon( if_salv_c_bool_sap=>true ).
          column->set_cell_type( if_salv_c_cell_type=>hotspot ).
          column->set_long_text( 'Objects' ).
          column->set_symbol( if_salv_c_bool_sap=>true ).
        ENDIF .

        lo_display = lo_table->get_display_settings( ) .
        lo_display->set_striped_pattern( cl_salv_display_settings=>true ) .

        lo_table->display( ).

      CATCH cx_salv_msg .
      CATCH cx_salv_not_found .
      CATCH cx_salv_existing .
      CATCH cx_salv_data_error .
      CATCH cx_salv_object_not_found .

    ENDTRY.

  ENDMETHOD .                    "generate_output


  METHOD clear_data .

    FIELD-SYMBOLS:
      <table> TYPE STANDARD TABLE .

    FREE:
      ct_e070, gt_e07t, ct_status, ct_type .

    IF go_data IS NOT INITIAL .
      ASSIGN go_data->* TO <table>.
      IF <table> IS ASSIGNED .
        REFRESH:
          <table> .
      ENDIF .
    ENDIF .

  ENDMETHOD .                    "limpar_dados

  METHOD get_description .

    DATA:
      lt_list      TYPE TABLE OF vrm_value,
      ls_list      TYPE          vrm_value,
      ls_tipo      TYPE ty_tipo,
      ls_status    TYPE ty_status,
      ls_categoria TYPE ty_categoria.

    CALL FUNCTION 'FICO_DOMAIN_VALUES_GET'
      EXPORTING
        i_table_name = 'E070'
        i_field_name = 'TRFUNCTION'
        i_langu      = sy-langu
      IMPORTING
        e_t_list     = lt_list.

    LOOP AT lt_list INTO ls_list .
      ls_tipo-tipo = ls_list-key .
      ls_tipo-desc = ls_list-text .
      APPEND ls_tipo TO ct_type .
      CLEAR  ls_tipo .
    ENDLOOP.

    SORT ct_type ASCENDING BY tipo.

    REFRESH lt_list .
    CLEAR   ls_list .

    CALL FUNCTION 'FICO_DOMAIN_VALUES_GET'
      EXPORTING
        i_table_name = 'E070'
        i_field_name = 'TRSTATUS'
        i_langu      = sy-langu
      IMPORTING
        e_t_list     = lt_list.

    LOOP AT lt_list INTO ls_list.
      ls_status-status = ls_list-key .
      ls_status-descr = ls_list-text .
      APPEND ls_status TO ct_status.
      CLEAR  ls_status .
    ENDLOOP.

    SORT ct_status ASCENDING BY status.

    REFRESH lt_list .
    CLEAR   ls_list .

    CALL FUNCTION 'FICO_DOMAIN_VALUES_GET'
      EXPORTING
        i_table_name = 'E070'
        i_field_name = 'KORRDEV'
        i_langu      = sy-langu
      IMPORTING
        e_t_list     = lt_list.

    LOOP AT lt_list INTO ls_list.
      ls_categoria-categoria = ls_list-key .
      ls_categoria-desc      = ls_list-text .
      APPEND ls_categoria TO ct_trfunction .
      CLEAR  ls_categoria .
    ENDLOOP.

    SORT ct_trfunction ASCENDING BY categoria .

    REFRESH lt_list .
    CLEAR   ls_list .


  ENDMETHOD .                    "carrega_descricao


  METHOD search_data .

    SELECT *
      FROM e070
      INTO TABLE ct_e070
     WHERE trkorr     IN it_request
       AND trfunction IN it_type
       AND trstatus   IN it_status
       AND korrdev    IN it_trfunction
       AND as4user    IN it_user
       AND as4date    IN it_date
       AND strkorr    EQ space.
    IF sy-subrc NE 0 .

    ENDIF.

    DELETE ct_e070 WHERE trkorr IS INITIAL .

    IF lines( ct_e070 ) EQ 0 .

    ELSE .

      SELECT *
        FROM e07t
        INTO TABLE ct_e07t
         FOR ALL ENTRIES IN ct_e070
       WHERE trkorr EQ ct_e070-trkorr
         AND ( langu  EQ 'P' OR langu  EQ 'E' ).

    ENDIF.

  ENDMETHOD.                    "seleciona_dados


  METHOD create_structure .

    DATA:
      lt_fieldcat TYPE lvc_t_fcat,
      ls_tmscsys  TYPE ty_tmscsys,
      fieldname   TYPE lvc_fname.

    FIELD-SYMBOLS:
      <fs_line>  TYPE any .

    me->change_tmscsys( ambiente = it_ambient ).

    me->create_column( EXPORTING fieldname    = 'OBJECTS'
                                 outputlen    = 10
*                                ref_table    = 'E071'
*                                ref_field    = 'TRKORR'
                       CHANGING  fieldcatalog = lt_fieldcat ) .

    me->create_column(
      EXPORTING
        fieldname    = 'TRKORR'
        outputlen    = 20
        ref_table    = 'E071'
        ref_field    = 'TRKORR'
      CHANGING
        fieldcatalog = lt_fieldcat
    ).

    me->create_column(
      EXPORTING
        fieldname    = 'DESCREQ'
        outputlen    = 60
        ref_table    = 'E07T'
        ref_field    = 'AS4TEXT'
      CHANGING
        fieldcatalog = lt_fieldcat
    ).

    me->create_column(
      EXPORTING
        fieldname    = 'AS4USER'
        outputlen    = 12
        ref_table    = 'E070'
        ref_field    = 'AS4USER'
      CHANGING
        fieldcatalog = lt_fieldcat
    ).

    me->create_column(
      EXPORTING
        fieldname    = 'TRFUNCTION'
        outputlen    = 42
        ref_table    = ''
        ref_field    = ''
*       ref_table    = 'E070'
*       ref_field    = 'TRFUNCTION'
      CHANGING
        fieldcatalog = lt_fieldcat
    ).

    me->create_column(
      EXPORTING
        fieldname    = 'TRSTATUS'
        outputlen    = 1
        ref_table    = 'E070'
        ref_field    = 'TRSTATUS'
      CHANGING
        fieldcatalog = lt_fieldcat
    ).

    me->create_column(
      EXPORTING
        fieldname    = 'KORRDEV'
        outputlen    = 4
        ref_table    = 'E070'
        ref_field    = 'KORRDEV'
      CHANGING
        fieldcatalog = lt_fieldcat
    ).

    me->create_ambient_column(
      EXPORTING
        ambiente     = it_ambient
      CHANGING
        fieldcatalog = lt_fieldcat
    ).

    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
*       i_style_table             =     " Add Style Table
        it_fieldcatalog           = lt_fieldcat
*       i_length_in_byte          =     " Boolean Variable (X=True, Space=False)
      IMPORTING
*       ep_table                  = new_table
        ep_table                  = go_data
*       e_style_fname             =     " ALV Control: Field Name of Internal Table Field
      EXCEPTIONS
        generate_subpool_dir_full = 1
        OTHERS                    = 2.

    IF sy-subrc EQ 0 .

    ELSE .

    ENDIF.

  ENDMETHOD .                    "cria_tabela


  METHOD create_report .

    DATA:
      ls_tmscys    TYPE ty_tmscsys,
      ls_e070      TYPE e070,
      ls_e07t      TYPE e07t,
      ls_status    TYPE ty_status,
      ls_tipo      TYPE ty_tipo,
      ls_categoria TYPE ty_categoria,
      settings     TYPE ctslg_settings,
      systemid     TYPE tstrfcofil-tarsystem,
      cofile       TYPE ctslg_cofile,
      ls_systems   TYPE ctslg_system,
      ls_steps     TYPE ctslg_step,
      new_line     TYPE REF TO data,
      fieldname    TYPE char10.

    FIELD-SYMBOLS:
      <table> TYPE STANDARD TABLE,
      <line>  TYPE any,
      <field> TYPE any.

    IF table IS NOT INITIAL .
      ASSIGN table->* TO <table>.
    ENDIF .

    settings-point_to_missing_steps = abap_on .
    settings-detailed_depiction     = abap_on .


    LOOP AT it_e070 INTO ls_e070 .

      IF (  <table> IS ASSIGNED ) .
        CREATE DATA new_line LIKE LINE OF <table>.
        ASSIGN new_line->* TO <line>.
      ENDIF .

      IF ( <line> IS NOT ASSIGNED ) .
        EXIT .
      ENDIF .

*     Acessando as tabelas internas da request
      READ TABLE it_e07t INTO ls_e07t
        WITH KEY trkorr = ls_e070-trkorr .

      IF ( sy-subrc EQ 0 ) .
      ENDIF.

*     Tipo de Request
      READ TABLE it_type INTO ls_tipo
        WITH KEY tipo = ls_e070-trfunction .

      IF ( sy-subrc EQ 0 ) .
      ENDIF.

*     Status da Request
      READ TABLE it_status INTO ls_status
        WITH KEY status = ls_e070-trstatus .

      IF ( sy-subrc EQ 0 ) .
      ENDIF.

*     Categoria da Request
      READ TABLE it_trfunction INTO ls_categoria
        WITH KEY categoria = ls_e070-korrdev .
      IF ( sy-subrc EQ 0 ) .
      ENDIF .


      CALL FUNCTION 'TR_READ_GLOBAL_INFO_OF_REQUEST'
        EXPORTING
          iv_trkorr   = ls_e070-trkorr
          iv_dir_type = 'T'
          is_settings = settings
        IMPORTING
          es_cofile   = cofile.


      LOOP AT gt_tmscsys INTO ls_tmscys .

        ASSIGN new_line->* TO <line> .
        IF ( <line> IS NOT ASSIGNED ) .
          EXIT .
        ENDIF .

        me->set_request_data(
          EXPORTING
            e070 = ls_e070
            e07t = ls_e07t
            tipo = ls_tipo
            status = ls_status
            categoria = ls_categoria
          CHANGING
            line = <line>

        ).

        me->assign( EXPORTING field = 'OBJECTS'
                              value = '@SK@'
                    CHANGING  line  = <line> ) .

        systemid = ls_tmscys-sysnam .

*       Para DEV, o status usado sera "Exportação"
*       Para os demais ambientes, sera "Importação encerrada"

*       Buscando log espeficico de cada Sistema
        READ TABLE cofile-systems INTO ls_systems
                                  WITH KEY systemid = systemid .
        IF ( sy-subrc EQ 0 ) .

*         Verificando se houve algum erro
          READ TABLE ls_systems-steps INTO ls_steps
            WITH KEY rc = 8 .
          IF ( sy-subrc EQ 0 ) .

            me->assign(
              EXPORTING
                field = systemid
                value = icon_led_red
              CHANGING
                line  = <line>
            ).

            me->assign_log(
              EXPORTING
                field  = systemid
                steps  = ls_systems-steps
              CHANGING
                line   = <line>
            ).

          ELSE.

*           Verificando se houve algum warning
            READ TABLE ls_systems-steps INTO ls_steps
              WITH KEY rc = 4 .
            IF ( sy-subrc EQ 0 ) .

              me->assign(
                EXPORTING
                  field = systemid
                  value = icon_led_yellow
                CHANGING
                  line  = <line>
              ).

              me->assign_log(
                EXPORTING
                  field  = systemid
                  steps  = ls_systems-steps
                CHANGING
                  line   = <line>
              ).

            ELSE.

*             Verifica a opção de Exportação (significa que é sistema de origem DEV)
              READ TABLE ls_systems-steps INTO ls_steps
                WITH KEY stepid = 'E' .
              IF (  sy-subrc EQ 0 ) .

                me->assign(
                  EXPORTING
                    field = systemid
                    value = icon_led_green
                  CHANGING
                    line  = <line>
                ).

                me->assign_log(
                  EXPORTING
                    field  = systemid
                    steps  = ls_systems-steps
                  CHANGING
                    line   = <line>
                ).

              ELSE.

*               Verificando se a opção Importação esta aplicada
                READ TABLE ls_systems-steps INTO ls_steps
                                            WITH KEY stepid = 'I' .
                IF ( sy-subrc EQ 0 ) .

                  me->assign(
                    EXPORTING
                      field = systemid
                      value = icon_led_green
                    CHANGING
                      line  = <line>
                  ) .

                  me->assign_log(
                    EXPORTING
                      field  = systemid
                      steps  = ls_systems-steps
                    CHANGING
                      line   = <line>
                  ).

                ELSE.

                  me->assign(
                    EXPORTING
                      field = systemid
                      value = icon_wd_radio_button_empty
                    CHANGING
                      line  = <line>
                  ).

                ENDIF.

              ENDIF .

            ENDIF .

          ENDIF.

        ELSE.

          me->assign(
            EXPORTING
              field = systemid
              value = icon_wd_radio_button_empty
            CHANGING
              line  = <line>
          ) .

          fieldname = |DT{ systemid }| .
          me->assign(
            EXPORTING
              field = fieldname
              value = ''
            CHANGING
              line  = <line>
          ) .

          fieldname = |TM{ systemid }| .
          me->assign(
            EXPORTING
              field = fieldname
              value = ''
            CHANGING
              line  = <line>
          ) .


        ENDIF.

      ENDLOOP.

      INSERT <line> INTO TABLE <table>.
      UNASSIGN <line> .

    ENDLOOP.

    IF lines( <table>[] ) EQ 0 .
    ELSE .
      APPEND LINES OF <table> TO et_outtab .
    ENDIF .

  ENDMETHOD .                    "monta_relatorio


  METHOD create_column .

    DATA:
      line TYPE lvc_s_fcat .

    line-fieldname = fieldname .
    line-outputlen = outputlen .
    line-ref_table = ref_table .
    line-ref_field = ref_field .

    IF ( text_l IS NOT INITIAL ) .
      line-scrtext_l = text_l .
    ENDIF .

    IF ( text_m IS NOT INITIAL ) .
      line-scrtext_m = text_m .
    ENDIF .

    IF ( text_s IS NOT INITIAL ) .
      line-scrtext_s = text_s .
    ENDIF .

    APPEND line TO fieldcatalog.
    CLEAR  line .

  ENDMETHOD.                    "cria_coluna


  METHOD create_ambient_column .

    DATA:
      fieldname TYPE lvc_fname .

*  Verificando os ambiente passados como parametro
    IF ( lines( ambiente ) EQ 0 ) .
    ELSE .

*     loop at gt_tmscsys into data(line_tmscsys) .
      LOOP AT gt_tmscsys INTO DATA(line_tmscsys) .

        fieldname = line_tmscsys-sysnam .

        me->create_column( EXPORTING fieldname    = fieldname
                                   outputlen    = 10
                                   ref_table    = ''
                                   ref_field    = ''
                          CHANGING fieldcatalog = fieldcatalog ) .

        fieldname = |DT{ line_tmscsys-sysnam }| .

        me->create_column( EXPORTING fieldname    = fieldname
                                     outputlen    = 10
                                     ref_table    = 'SYST'
                                     ref_field    = 'DATUM'
                           CHANGING  fieldcatalog = fieldcatalog ) .

        fieldname = |TM{ line_tmscsys-sysnam }| .

        me->create_column( EXPORTING fieldname    = fieldname
                                     outputlen    = 10
                                     ref_table    = 'SYST'
                                     ref_field    = 'UZEIT'
                           CHANGING  fieldcatalog = fieldcatalog ) .

      ENDLOOP .

    ENDIF .

  ENDMETHOD .


  METHOD create_date_time .

    DATA:
      text_l TYPE lvc_s_fcat-scrtext_l,
      text_m TYPE lvc_s_fcat-scrtext_m,
      text_s TYPE lvc_s_fcat-scrtext_s.

    IF ( iv_sysnam IS INITIAL ) .
    ELSE .

      text_l = text_m = text_s = iv_sysnam .

      me->create_column(
        EXPORTING
          fieldname    = |AS4DATE_{ iv_sysnam }|
          outputlen    = 8
          ref_table    = 'E070'
          ref_field    = 'AS4DATE'
          text_l       = text_l
          text_m       = text_m
          text_s       = text_s
        CHANGING
          fieldcatalog = ct_catalog
      ).


      me->create_column(
        EXPORTING
          fieldname    = |AS4TIME_{ iv_sysnam }|
          outputlen    = 6
          ref_table    = 'E070'
          ref_field    = 'AS4TIME'
          text_l       = text_l
          text_m       = text_m
          text_s       = text_s
        CHANGING
          fieldcatalog = ct_catalog
      ).

    ENDIF .

  ENDMETHOD .

  METHOD update_attributes .

    IF lines( gt_request ) EQ 0 .
      IF lines( ordem ) EQ 0 .
      ELSE .
        APPEND LINES OF ordem TO gt_request .
      ENDIF .
    ELSE .
    ENDIF .

    IF lines( gt_type ) EQ 0 .
      IF lines( tipo ) EQ 0 .
      ELSE .
        APPEND LINES OF tipo TO gt_type .
      ENDIF .
    ELSE .
    ENDIF .

    IF lines( gt_status ) EQ 0 .
      IF lines( status ) EQ 0 .
      ELSE .
        APPEND LINES OF status TO gt_status .
      ENDIF .
    ELSE .
    ENDIF .

    IF lines( gt_trfunction ) EQ 0 .
      IF lines( categoria ) EQ 0 .
      ELSE .
        APPEND LINES OF categoria TO gt_trfunction .
      ENDIF .
    ELSE .
    ENDIF .

    IF lines( gt_user ) EQ 0 .
      IF lines( usuario ) EQ 0 .
      ELSE .
        APPEND LINES OF usuario TO gt_user .
      ENDIF .
    ELSE .
    ENDIF .

    IF lines( gt_date ) EQ 0 .
      IF lines( data ) EQ 0 .
      ELSE .
        APPEND LINES OF data TO gt_date .
      ENDIF .
    ELSE .
    ENDIF .

  ENDMETHOD .                    "atualiza_atributos

  METHOD set_text .

    TRY.
        c_column ?= c_columns->get_column( i_field ).
        c_column->set_short_text( i_short_text ).
        c_column->set_medium_text( i_medium_text ).
        c_column->set_long_text( i_long_text ).
      CATCH cx_salv_not_found .
    ENDTRY .

  ENDMETHOD .                    "set_text


  METHOD set_text_output .

    DATA:
      line        TYPE ty_tmscsys,
      field       TYPE lvc_fname,
      long_text   TYPE scrtext_l,
      medium_text TYPE scrtext_m,
      short_text  TYPE scrtext_s,
      column      TYPE REF TO cl_salv_column_list,
      columns     TYPE REF TO cl_salv_columns_table.

    columns = table->get_columns( ).

    LOOP AT t_tmscsys INTO line .

      field       = line-sysnam .
      long_text   = line-sysnam .
      medium_text = line-sysnam .
      short_text  = line-sysnam .

      me->set_text(
        EXPORTING
          i_field       = field
          i_long_text   = long_text
          i_medium_text = medium_text
          i_short_text  = short_text
        CHANGING
          c_columns     = columns
          c_column      = column
      ).

      field = |DT{ line-sysnam }| .
      long_text = medium_text = short_text = |Data({ line-sysnam })| .
      me->set_text(
        EXPORTING
          i_field       = field
          i_long_text   = long_text
          i_medium_text = medium_text
          i_short_text  = short_text
        CHANGING
          c_columns     = columns
          c_column      = column
      ).

      field = |TM{ line-sysnam }| .
      long_text = medium_text = short_text = |Hora({ line-sysnam })| .
      me->set_text(
        EXPORTING
          i_field       = field
          i_long_text   = long_text
          i_medium_text = medium_text
          i_short_text  = short_text
        CHANGING
          c_columns     = columns
          c_column      = column
      ).

    ENDLOOP.

  ENDMETHOD.                    "set_text_output


  METHOD set_request_data .


    IF ( e070 IS NOT INITIAL ) AND
       ( e07t IS NOT INITIAL ) .

*     Request
      me->assign(
        EXPORTING
          field = 'TRKORR'
          value = e070-trkorr
        CHANGING
          line  = line
      ).

*     Descricao da Request
      me->assign(
        EXPORTING
          field = 'DESCREQ'
          value = e07t-as4text
        CHANGING
          line  = line
      ).

*     Usuario da Request
      me->assign(
        EXPORTING
          field = 'AS4USER'
          value = e070-as4user
        CHANGING
          line  = line
      ).

*     Tipo de Request
      me->assign(
        EXPORTING
          field = 'TRFUNCTION'
          value = tipo-desc
        CHANGING
          line  = line
      ).

*     Status da Request
      me->assign(
        EXPORTING
          field = 'TRSTATUS'
          value = e070-trstatus
        CHANGING
          line  = line
      ).

*     Categoria da Request
      me->assign(
        EXPORTING
          field = 'KORRDEV'
          value = e070-korrdev
        CHANGING
          line  = line
      ).

    ENDIF .

  ENDMETHOD .


  METHOD link_click .

    DATA:
      trkorr TYPE trkorr .

    FIELD-SYMBOLS:
      <table> TYPE STANDARD TABLE,
      <line>  TYPE any,
      <field> TYPE any.

    IF go_data IS NOT INITIAL .

      ASSIGN go_data->* TO <table>.

      IF <table> IS ASSIGNED .

        READ TABLE <table> ASSIGNING <line> INDEX row .
        IF sy-subrc EQ 0 .

          ASSIGN COMPONENT 'TRKORR' OF STRUCTURE <line> TO <field>.
          IF <field> IS ASSIGNED .

            trkorr = <field>  .

            CASE column .
              WHEN 'TRKORR' .

                CALL FUNCTION 'TR_LOG_OVERVIEW_REQUEST_REMOTE'
                  EXPORTING
                    iv_trkorr = trkorr
*                   iv_dirtype             =
*                   iv_without_check       = ' '
                  .

              WHEN 'OBJECTS' .

*                call function 'TRINT_TDR_USER_COMMAND'
*                  exporting
*                    iv_object  = trkorr
*                    iv_type    = 'TASK'
*                    iv_command = 'REQUEST_SHOW'
**                  importing
**                   ev_exit    =
                .
                CALL FUNCTION 'TRINT_TDR_USER_COMMAND'
                  EXPORTING
                    iv_object  = trkorr
                    iv_type    = 'REQU'
                    iv_command = 'REQUEST_SHOW'
*                  importing
*                   ev_exit    =
                  .

              WHEN OTHERS .

            ENDCASE .

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD .                    "link_click


  METHOD process .

    DATA:
      value TYPE salv_t_row,
      line  TYPE i.

    FIELD-SYMBOLS:
      <line> TYPE snwd_texts .

    CASE sy-ucomm .

      WHEN 'REFRESH' .

        me->get_data_refresh( ) .

        IF me->lo_table IS BOUND .

          me->lo_table->refresh( ) .

        ENDIF .

      WHEN OTHERS .

    ENDCASE .

  ENDMETHOD .                    "process


  METHOD change_tmscsys .


*   #verificar: inibir abas no parametro s_amb

    DATA:
      line       TYPE ty_r_sysnam,
      lt_tmscsys TYPE tmscsys_tab,
      ls_tmscsys TYPE ty_tmscsys.

    IF ambiente[] IS NOT INITIAL .

      DO .

        READ TABLE ambiente INTO line INDEX sy-index .

        IF sy-subrc EQ 0 .

          READ TABLE gt_tmscsys INTO ls_tmscsys
            WITH KEY sysnam = line-low .

          IF sy-subrc EQ 0 .

            APPEND ls_tmscsys TO lt_tmscsys .
            CLEAR  ls_tmscsys .

          ENDIF .

        ELSE .
          EXIT .
        ENDIF.

      ENDDO .

      IF lines( lt_tmscsys ) EQ 0 .
      ELSE .
        REFRESH gt_tmscsys .
        APPEND LINES OF lt_tmscsys TO gt_tmscsys .
      ENDIF .

    ENDIF .

    FREE:
      lt_tmscsys .

  ENDMETHOD.                    "change_tmscsys

  METHOD assign .

    FIELD-SYMBOLS:
      <field> TYPE any .


    ASSIGN COMPONENT field OF STRUCTURE line TO <field>.

    IF <field> IS ASSIGNED .

      <field> = value .

      UNASSIGN
        <field> .

    ENDIF .

  ENDMETHOD .                    "assign


  METHOD assign_log .

    DATA:
      lv_fieldname    TYPE char10,
      ls_steps_line   TYPE ctslg_step,
      ls_actions_line TYPE ctslg_action.

    IF ( lines( steps ) EQ 0 ) .
    ELSE .

      READ TABLE steps INTO ls_steps_line INDEX lines( steps ) .
      IF ( sy-subrc EQ 0 ) .

        READ TABLE ls_steps_line-actions INTO ls_actions_line INDEX 1 .
        IF ( sy-subrc EQ 0 ) .

          lv_fieldname = |DT{ field }|.

          me->assign( EXPORTING field = lv_fieldname
                                value = ls_actions_line-date
                      CHANGING  line  = line ) .

          lv_fieldname = |TM{ field }|.

          me->assign( EXPORTING field = lv_fieldname
                                value = ls_actions_line-time
                      CHANGING  line  = line ) .

        ENDIF .

      ENDIF .

    ENDIF .

  ENDMETHOD .


  METHOD get_data_refresh .

    me->get_data( EXPORTING it_ambient    = gt_ambient
                            it_request    = gt_request
                            it_type       = gt_type
                            it_status     = gt_status
                            it_trfunction = gt_trfunction
                            it_user       = gt_user
                            it_date       = gt_date ) .

  ENDMETHOD .                    "get_data_refresh


  METHOD on_link_click.

    me->link_click( EXPORTING row    = row
                              column = column ) .

  ENDMETHOD.                    "on_link_click


  METHOD on_added_function .

    me->process( ) .

  ENDMETHOD .                    "on_added_function

ENDCLASS.                    "lcl_report IMPLEMENTATION


*--------------------------------------------------------------------*
*- Tela de seleção
*--------------------------------------------------------------------*
DATA:
  report TYPE REF TO class_report.

*--------------------------------------------------------------------*
*- Tela de seleção
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS:
  s_amb FOR trtarget-tarsystem NO INTERVALS OBLIGATORY,
  p_ordem FOR  e070-trkorr,
  p_tipo  FOR  e070-trfunction,
  p_stat  FOR  e070-trstatus DEFAULT 'R',
  p_categ FOR  e070-korrdev,
  p_user  FOR  e070-as4user DEFAULT sy-uname ,
  p_data  FOR  e070-as4date DEFAULT sy-datum .
PARAMETERS:
  p_rfcdes TYPE rfcdes-rfcdest .
SELECTION-SCREEN END OF BLOCK b1.

*--------------------------------------------------------------------*
*- Eventos
*--------------------------------------------------------------------*
INITIALIZATION.

  class_report=>initial(
    CHANGING
      ct_ambient = s_amb[]
  ).


START-OF-SELECTION .

  CREATE OBJECT report.

  report->create_structure( it_ambient = s_amb[] ).

  report->get_data( EXPORTING it_ambient    = s_amb[]
                              it_request    = p_ordem[]
                              it_type       = p_tipo[]
                              it_status     = p_stat[]
                              it_trfunction = p_categ[]
                              it_user       = p_user[]
                              it_date       = p_data[] ) .

end-of-selection.

  report->generate_output( ) .

*
*