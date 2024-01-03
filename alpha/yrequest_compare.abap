*&---------------------------------------------------------------------*
*& Report ZCA_R_COMPARE_OBJECTS
*&
*&---------------------------------------------------------------------*
*& Author........: Paulo Amor [EX83935]                                *
*& Creation date : 15.12.2023 15:39:38                                 *
*& Description...:                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*& Modifications                                                       *
*&---------------------------------------------------------------------*
*& Author........: [Name][Username]                                    *
*& Creation date : YYYY/MM/DD                                          *
*& Tag...........: MODXX                                               *
*& Description...:                                                     *
*&---------------------------------------------------------------------*

REPORT zca_r_compare_objects.

TYPES:
  BEGIN OF ty_output,
    status_k15e TYPE zca_status_icon,
    status_k15c TYPE zca_status_icon,
    status_p15e TYPE zca_status_icon,
    status_p15c TYPE zca_status_icon,
    trkorr      TYPE trkorr,
    trkorr_k15  TYPE zca_status_icon,
    trkorr_p15  TYPE zca_status_icon,
    as4pos      TYPE ddposition,
    trfunction  TYPE trfunction,
    trstatus    TYPE trstatus,
    as4text     TYPE as4text,
    pgmid       TYPE pgmid,
    object      TYPE trobjtype,
    object_text TYPE ddtext,
    obj_name    TYPE trobj_name,
  END OF ty_output.

DATA: t_output TYPE STANDARD TABLE OF ty_output,
      w_output TYPE ty_output.


*&-------------------------------------------------------------------*
*&    CLASS DEFINITION
*&-------------------------------------------------------------------*
CLASS cl_handle_events DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events_table
        IMPORTING e_salv_function,
      on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column.
ENDCLASS.                    " cl_handle_events DEFINITION


DATA: o_salv_table    TYPE REF TO cl_salv_table,  " ALV declaration
      o_handle_events TYPE REF TO cl_handle_events.




SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.

SELECT-OPTIONS: s_trkorr FOR w_output-trkorr MODIF ID gp1,
                s_object FOR w_output-obj_name.

SELECTION-SCREEN END OF BLOCK b1.



INITIALIZATION.


START-OF-SELECTION.

  PERFORM get_data.


End-OF-SELECTION.

  PERFORM display_alv USING t_output.






*&---------------------------------------------------------------------*
*&      Class  CL_HANDLE_EVENTS
*&---------------------------------------------------------------------*
*       Class for alv envents
*----------------------------------------------------------------------*
CLASS cl_handle_events IMPLEMENTATION.

  " added_function
  METHOD on_user_command.

    DATA: progname TYPE progname.
    DATA(o_selections) = o_salv_table->get_selections( ).
    DATA(t_rows)       = o_selections->get_selected_rows( ).

    READ TABLE t_rows INTO DATA(w_row) INDEX 1.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    READ TABLE t_output INTO DATA(w_output) INDEX w_row.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    IF e_salv_function EQ '&COMPK15' OR
       e_salv_function EQ '&COMPP15'.
      CALL FUNCTION 'SVRS_GET_OBJECT_REPORTS'
        EXPORTING
          objtype  = w_output-object
        IMPORTING
          rep_comp = progname.
      IF progname IS INITIAL.
        MESSAGE i017(ds) DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
    ENDIF.

    CASE e_salv_function.
      WHEN '&COMPK15'.
        SUBMIT (progname) WITH objname = w_output-obj_name
                          WITH objnam2 = w_output-obj_name
                          WITH objtyp1 = w_output-object
                          WITH objtyp2 = w_output-object
                          WITH log_dest = 'TMSADM@K15.DOMAIN_D26'
                          WITH rem_syst = 'K15'
                          AND RETURN.
      WHEN '&COMPP15'.
        SUBMIT (progname) WITH objname = w_output-obj_name
                          WITH objnam2 = w_output-obj_name
                          WITH objtyp1 = w_output-object
                          WITH objtyp2 = w_output-object
                          WITH log_dest = 'TMSADM@P15.DOMAIN_D26'
                          WITH rem_syst = 'P15'
                          AND RETURN.
      WHEN '&LOG'.
        CALL FUNCTION 'TR_LOG_OVERVIEW_REQUEST'
          EXPORTING
            iv_trkorr = w_output-trkorr
*           IV_DIR_TYPE                  = 'T'
*           IV_DETAILED_CHRONOLOGY       = 'X'
*           IV_TARGET_SYSTEM             = ' '
*           IS_POPUP  =
*         EXCEPTIONS
*           E_WRONG_CALL                 = 1
*           OTHERS    = 2
          .
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.



      WHEN OTHERS.
        RETURN.
    ENDCASE.

    o_salv_table->refresh( ).
  ENDMETHOD.                    "on_user_command

  " double_click
  METHOD on_double_click.

    DATA: progname TYPE progname.
    DATA(o_selections) = o_salv_table->get_selections( ).
    DATA(t_rows)       = o_selections->get_selected_rows( ).

    READ TABLE t_rows INTO DATA(w_row) INDEX 1.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    READ TABLE t_output INTO DATA(w_output) INDEX w_row.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SVRS_GET_OBJECT_REPORTS'
      EXPORTING
        objtype  = w_output-object
      IMPORTING
        rep_comp = progname.
    IF progname IS INITIAL.
      MESSAGE i017(ds) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    SUBMIT (progname) WITH objname = w_output-obj_name
                      WITH objnam2 = w_output-obj_name
                      WITH objtyp1 = w_output-object
                      WITH objtyp2 = w_output-object
                      WITH log_dest = 'TMSADM@P15.DOMAIN_D26'
                      WITH rem_syst = 'P15'
                      AND RETURN.

    o_salv_table->refresh( ).

  ENDMETHOD.                    "on_double_click

ENDCLASS.                    " lcl_handle_events IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .

  DATA: t_e071      TYPE scwb_t_e071,
        t_ko100     TYPE STANDARD TABLE OF ko100,
        t_items_k15 TYPE vrs_compare_item_tab,
        t_items_p15 TYPE vrs_compare_item_tab,
        ot_k15      TYPE zca_status_icon,
        ot_p15      TYPE zca_status_icon,
        subrc       TYPE sysubrc.


  CALL FUNCTION 'TRINT_OBJECT_TABLE'
    EXPORTING
      iv_complete  = abap_true
    TABLES
      tt_types_out = t_ko100.

  SORT t_ko100 BY pgmid object.

  SELECT e1~trkorr e1~as4pos e0~trfunction e0~trstatus t~as4text
         e1~pgmid e1~object e1~obj_name
    FROM e071 AS e1
    INNER JOIN e070 AS e0 ON e0~trkorr EQ e1~trkorr
    INNER JOIN e07t AS t ON t~trkorr EQ e1~trkorr
    INTO CORRESPONDING FIELDS OF TABLE t_output
    WHERE e1~trkorr IN s_trkorr
      AND e1~obj_name IN s_object
      AND e0~trfunction NE 'T'.

  IF t_output IS INITIAL.
    RETURN.
  ENDIF.

  SORT t_output BY trkorr pgmid object obj_name.
  DELETE ADJACENT DUPLICATES FROM t_output COMPARING trkorr pgmid object obj_name.

  SORT t_output BY object.
  DELETE t_output WHERE object EQ 'RELE'.
  DELETE t_output WHERE object EQ 'TABL'.
  DELETE t_output WHERE object EQ 'TABU'.
  DELETE t_output WHERE object EQ 'VDAT'.
  DELETE t_output WHERE object EQ 'VIEW'.
  DELETE t_output WHERE object EQ 'TOBJ'.
  DELETE t_output WHERE object EQ 'CDAT'.
  DELETE t_output WHERE object EQ 'CLAS'.
  DELETE t_output WHERE object EQ 'ACGR'.


  SORT t_output BY pgmid object obj_name.


  LOOP AT t_output ASSIGNING FIELD-SYMBOL(<fs>).

    READ TABLE t_e071 TRANSPORTING NO FIELDS WITH KEY pgmid  = <fs>-pgmid
                                                      object = <fs>-object
                                                      obj_name = <fs>-obj_name BINARY SEARCH.
    IF sy-subrc NE 0.
      INSERT INITIAL LINE INTO t_e071 ASSIGNING FIELD-SYMBOL(<fs_71>) INDEX sy-tabix.
      IF <fs_71> IS ASSIGNED.
        <fs_71>-pgmid  = <fs>-pgmid.
        <fs_71>-object = <fs>-object.
        <fs_71>-obj_name = <fs>-obj_name.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF t_e071 IS NOT INITIAL.

    " Valida Objectos em K15
    CALL FUNCTION 'SVRS_MASSCOMPARE_ACT_OBJECTS'
      EXPORTING
        it_e071               = t_e071
        iv_rfcdest_b          = 'K15'
        iv_filter_lang        = abap_true
        iv_delete_lang        = ' '
        iv_ignore_report_text = ' '
      IMPORTING
        et_compare_items      = t_items_k15
      EXCEPTIONS
        rfc_error             = 1
        not_supported         = 2
        OTHERS                = 3.
    IF t_items_p15 IS NOT INITIAL.
      SORT t_items_k15 BY fragid fragment fragname.
    ENDIF.

    " Valida Objectos em P15
    CALL FUNCTION 'SVRS_MASSCOMPARE_ACT_OBJECTS'
      EXPORTING
        it_e071               = t_e071
        iv_rfcdest_b          = 'P15'
        iv_filter_lang        = abap_true
        iv_delete_lang        = ' '
        iv_ignore_report_text = ' '
      IMPORTING
        et_compare_items      = t_items_p15
      EXCEPTIONS
        rfc_error             = 1
        not_supported         = 2
        OTHERS                = 3.
    IF t_items_p15 IS NOT INITIAL.
      SORT t_items_p15 BY fragid fragment fragname.
    ENDIF.
  ENDIF.


  SORT t_output BY trkorr pgmid object obj_name.
  DATA(t_trkorr) = t_output.
  DELETE ADJACENT DUPLICATES FROM t_trkorr COMPARING trkorr.

  DATA: cofile   TYPE ctslg_cofile,
        settings TYPE ctslg_settings.

  LOOP AT t_trkorr INTO DATA(w_trkorr).

    CLEAR: ot_k15, ot_p15.

    " Valida Ordem em K15
    PERFORM check_ot_system USING w_trkorr-trkorr
                         CHANGING ot_k15 ot_p15.

    LOOP AT t_output ASSIGNING <fs> WHERE trkorr EQ w_trkorr-trkorr.

      <fs>-trkorr_k15 = ot_k15.
      <fs>-trkorr_p15 = ot_p15.

      " Comparação com K15
      READ TABLE t_items_k15 INTO DATA(w_items) WITH KEY fragid  = <fs>-pgmid
                                                         fragment = <fs>-object
                                                         fragname = <fs>-obj_name." BINARY SEARCH.
      IF sy-subrc EQ 0.
        <fs>-status_k15e = icon_checked.
        IF w_items-equal EQ abap_true.
          <fs>-status_k15c = icon_green_light.
        ELSE.
          <fs>-status_k15c = icon_red_light.
        ENDIF.
      ELSE.
        <fs>-status_k15e = icon_message_warning.
      ENDIF.

      " Comparação com P15
      CLEAR w_items.
      READ TABLE t_items_p15 INTO w_items WITH KEY fragid  = <fs>-pgmid
                                                   fragment = <fs>-object
                                                   fragname = <fs>-obj_name." BINARY SEARCH.
      IF sy-subrc EQ 0.
        <fs>-status_p15e = icon_checked.
        IF w_items-equal EQ abap_true.
          <fs>-status_p15c = icon_green_light.
        ELSE.
          <fs>-status_p15c = icon_red_light.
        ENDIF.
      ELSE.
        <fs>-status_p15e = icon_message_warning.
      ENDIF.

      READ TABLE t_ko100 INTO DATA(w_ko100) WITH KEY pgmid = <fs>-pgmid
                                                     object = <fs>-object.
      IF sy-subrc EQ 0.
        <fs>-object_text = w_ko100-text.
      ENDIF.

      " Comparação com K15
*  PERFORM compare_object USING <fs>-obj_name <fs>-object 'K15'
*                      CHANGING <fs>-status_k15e <fs>-status_k15c.
*
*      " Comparação com P15
*      PERFORM compare_object USING <fs>-obj_name <fs>-object 'P15'
*                          CHANGING <fs>-status_p15e <fs>-status_p15c.
    ENDLOOP.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  display_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PU_T_TABLE     text
*      -->PC_SALV_TABLE  text
*----------------------------------------------------------------------*
FORM display_alv USING t_table TYPE STANDARD TABLE.

  DATA: lo_selections TYPE REF TO cl_salv_selections,
        lo_columns    TYPE REF TO cl_salv_columns_table,
        lo_functions  TYPE REF TO cl_salv_functions_list,
        lo_column     TYPE REF TO cl_salv_column,
        lo_events     TYPE REF TO cl_salv_events_table,
        lo_layout     TYPE REF TO cl_salv_layout,
        ls_key        TYPE salv_s_layout_key.

  IF t_table IS INITIAL.
    MESSAGE i325(lr) DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF o_salv_table IS NOT BOUND.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = o_salv_table
                                 CHANGING t_table      = t_table ).

      CATCH cx_salv_msg INTO DATA(lv_salv_msg)                                        ##no_handler.
    ENDTRY.

    " Activa as funções standard da ALV
    lo_functions = o_salv_table->get_functions( ).
    lo_functions->set_all( abap_true ).

    " Set Standard status GUI
    o_salv_table->set_screen_status(
      EXPORTING
        report        = sy-repid
        pfstatus      = 'STANDARD'
    ).

    " Get layout object
    lo_layout = o_salv_table->get_layout( ).

    " Set Layout save restriction
    ls_key-report = |{ sy-repid }-{ sy-uname }|.
    lo_layout->set_key( ls_key ).
    lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
    lo_layout->set_default( 'X' ).

    " Activa a selecção de linhas
    lo_selections = o_salv_table->get_selections( ).
    lo_selections->set_selection_mode( if_salv_c_selection_mode=>single ).

    " Declara Eventos do ALV
    lo_events = o_salv_table->get_event( ).
    SET HANDLER cl_handle_events=>on_user_command FOR lo_events.
    SET HANDLER cl_handle_events=>on_double_click FOR lo_events.

    lo_columns = o_salv_table->get_columns( ).
    lo_columns->set_optimize( abap_true ).

    TRY.
        lo_column ?= lo_columns->get_column( 'STATUS_K15E' ).
        lo_column->set_short_text( 'K15 Existe'(f00) ).
        lo_column->set_medium_text( 'K15 Existe'(f00) ).
        lo_column->set_long_text( 'K15 Existe'(f00) ).

        lo_column ?= lo_columns->get_column( 'STATUS_K15C' ).
        lo_column->set_short_text( 'K15 Igual'(f01) ).
        lo_column->set_medium_text( 'K15 Igual'(f01) ).
        lo_column->set_long_text( 'K15 Igual'(f01) ).

        lo_column ?= lo_columns->get_column( 'STATUS_P15E' ).
        lo_column->set_short_text( 'P15 Existe'(f02) ).
        lo_column->set_medium_text( 'P15 Existe'(f02) ).
        lo_column->set_long_text( 'P15 Existe'(f02) ).

        lo_column ?= lo_columns->get_column( 'STATUS_P15C' ).
        lo_column->set_short_text( 'P15 Igual'(f03) ).
        lo_column->set_medium_text( 'P15 Igual'(f03) ).
        lo_column->set_long_text( 'P15 Igual'(f03) ).

        lo_column ?= lo_columns->get_column( 'TRKORR_K15' ).
        lo_column->set_short_text( 'Ordem K15'(f04) ).
        lo_column->set_medium_text( 'Ordem K15'(f04) ).
        lo_column->set_long_text( 'Ordem K15'(f04) ).

        lo_column ?= lo_columns->get_column( 'TRKORR_P15' ).
        lo_column->set_short_text( 'Ordem P15'(f05) ).
        lo_column->set_medium_text( 'Ordem P15'(f05) ).
        lo_column->set_long_text( 'Ordem P15'(f05) ).
      CATCH cx_salv_not_found INTO DATA(lv_salv_not_found).
        MESSAGE lv_salv_not_found TYPE 'W'.
    ENDTRY.

  ELSE.
    o_salv_table->set_data( CHANGING t_table = t_table ).

  ENDIF.

  o_salv_table->display( ).

ENDFORM.                    "display_alv
*&---------------------------------------------------------------------*
*&      Form  compare_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PU_T_TABLE     text
*      -->PC_SALV_TABLE  text
*----------------------------------------------------------------------*
FORM compare_object USING p_objname TYPE trobj_name
                          p_objtyp1 TYPE trobjtype
                          p_dest    TYPE char3
                 CHANGING output_e  TYPE icon_d
                          output_c  TYPE icon_d.


  DATA: lt_bdcdata      TYPE STANDARD TABLE OF bdcdata,
        lt_tcode_output TYPE STANDARD TABLE OF bdcmsgcoll,
        t_return        TYPE bapiret2_t,
        ls_options      TYPE ctu_params,
        rv_subrc        TYPE sysubrc,
        lv_mode         TYPE apq_ouac.

  lv_mode = 'N'.
  rv_subrc = 0.

  IF p_dest EQ 'P15'.
    DATA(dest) = 'TMSADM@P15.DOMAIN_D26'.
    DATA(syst) = 'P15'.
  ELSEIF p_dest EQ 'K17'.
    dest = 'TMSADM@K15.DOMAIN_D26'.
    syst = 'K15'.
  ENDIF.

  lt_bdcdata = VALUE #( ( program = 'ZCA_RSVRSRS3'   dynpro = '1000' dynbegin = 'X' )
                        ( fnam    = 'BDC_CURSOR' fval   = 'REM_SYST' )
                        ( fnam    = 'BDC_OKCODE' fval   = '=ONLI' )
                        ( fnam    = 'OBJNAME' fval   = p_objname )
                        ( fnam    = 'OBJNAM2' fval   = p_objname )
                        ( fnam    = 'OBJTYP1' fval   = p_objtyp1 )
                        ( fnam    = 'OBJTYP2' fval   = p_objtyp1 )
                        ( fnam    = 'LOG_DEST' fval   = dest ) "'TMSADM@P15.DOMAIN_D26' )
                        ( fnam    = 'REM_SYST' fval   = syst ) "'P15' )

                        ( program = 'ZCA_RSVRSRS3'   dynpro = '1000' dynbegin = 'X' )
                        ( fnam    = 'BDC_OKCODE' fval   = '/EE' )
                        ( fnam    = 'BDC_CURSOR' fval   = 'OBJNAME' )
                        ( fnam    = 'BDC_SUBSCR' fval   = 'SAPMV50A                                1102SUBSCREEN_BODY' ) ).

  IF lt_bdcdata IS INITIAL.
    rv_subrc = 4.
    RETURN.
  ENDIF.

  ls_options = VALUE #( racommit = abap_true
                        dismode  = lv_mode
                        updmode  = 'S' ).

  CALL TRANSACTION 'ZCA_COMP_DDIC' USING lt_bdcdata
                            OPTIONS FROM ls_options
                           MESSAGES INTO lt_tcode_output.

  IF lt_tcode_output IS INITIAL.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
    TABLES
      imt_bdcmsgcoll = lt_tcode_output
      ext_return     = t_return.

  " Valida se existem erros
  READ TABLE t_return TRANSPORTING NO FIELDS WITH KEY number = '814'.
  IF sy-subrc EQ 0.
    output_e = icon_failure.
    output_c = icon_red_light.
  ELSE.
    output_e = icon_checked.
    READ TABLE t_return TRANSPORTING NO FIELDS WITH KEY number = '813'.
    IF sy-subrc EQ 0.
      output_c = icon_red_light.
    ELSE.
      READ TABLE t_return TRANSPORTING NO FIELDS WITH KEY number = '812'.
      IF sy-subrc EQ 0.
        output_c = icon_green_light.
      ELSE.
      ENDIF.
    ENDIF.

  ENDIF.

*  rv_subrc = zca_cl_utilities=>bapi_return_get_status( EXPORTING it_return = t_return ).
*  IF rv_subrc NE 0.
*    output = zca_cl_utilities=>bapi_return_get_status_icon( 'E' ).
*  ELSE.
*    output = zca_cl_utilities=>bapi_return_get_status_icon( 'I' ).
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_OT_SYSTEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_ot_system USING trkorr     TYPE trkorr
                  CHANGING status_k15 TYPE icon_d
                           status_p15 TYPE icon_d.

  DATA: t_log_overview TYPE scts_log_overviews.

  CALL FUNCTION 'TRINT_GET_LOG_OVERVIEW'
    EXPORTING
      iv_request                = trkorr
      iv_with_transport_targets = 'X'
    IMPORTING
      et_log_overview           = t_log_overview.
  IF t_log_overview IS INITIAL.
    status_k15 = icon_status_open.
    status_p15 = icon_status_open.
    RETURN.
  ENDIF.

  READ TABLE t_log_overview INTO DATA(w_log) WITH KEY sysnam = 'P15'.
  IF sy-subrc EQ 0.

    CONDENSE w_log-rc.
    status_k15 = icon_status_booked.

    IF w_log-rc EQ '8'.
      status_p15 = icon_status_reverse.
    ELSEIF w_log-rc EQ '0' OR
           w_log-rc EQ '1' OR
           w_log-rc EQ '4' .
      status_p15 = icon_status_booked.
    ELSE.
      status_p15 = icon_status_open.
    ENDIF.
  ELSE.

    status_p15 = icon_status_open.

    READ TABLE t_log_overview INTO w_log WITH KEY sysnam = 'K15'.
    IF sy-subrc EQ 0.
      CONDENSE w_log-rc.

      IF w_log-rc EQ '8'.
        status_k15 = icon_status_reverse.
      ELSEIF w_log-rc EQ '0' OR
             w_log-rc EQ '1' OR
             w_log-rc EQ '4' .
        status_k15 = icon_status_booked.
      ELSE.
        status_k15 = icon_status_open.
      ENDIF.
    ELSE.
      status_k15 = icon_status_open.
    ENDIF.
  ENDIF.


ENDFORM.