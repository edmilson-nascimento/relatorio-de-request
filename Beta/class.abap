CLASS /yga/cl_transp_control DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_r_ordem TYPE RANGE OF trkorr .
    TYPES:
      ty_r_text  TYPE RANGE OF as4text .
    TYPES:
      ty_r_user TYPE RANGE OF sy-uname .
    TYPES:
      ty_r_date TYPE RANGE OF as4date .
    TYPES:
      ty_r_time TYPE RANGE OF as4time .
    TYPES:
      ty_r_cd TYPE RANGE OF /yga/charm_change .
    TYPES:
      ty_r_st_cd TYPE RANGE OF /yga/status_cd .
    TYPES:
      ty_r_uname TYPE RANGE OF xubname .

    METHODS at_selection_screen_output .
    METHODS check_screen
      IMPORTING
        !iv_impac  TYPE flag
        !iv_obs_im TYPE /yga/obs_impacto_jump .
    METHODS constructor
      IMPORTING
        !i_repid TYPE syrepi2 .
    METHODS init_erdat
      CHANGING
        !cr_erdat TYPE ty_r_date .
*       !IR_CD1    type TY_R_CD
    "! <p class="shorttext synchronized" lang="pt">Retorna variant escolhiada no Search HelpF4</p>
    METHODS get_varian
      CHANGING
        ch_varian TYPE slis_vari .
    "! <p class="shorttext synchronized" lang="pt">Prepara o filtro de "Importado para Produção"</p>
    METHODS get_prod_filter
      IMPORTING
        !im_aprovado     TYPE sap_bool
        !im_nao_apro     TYPE sap_bool
        !im_ambos        TYPE sap_bool
      RETURNING
        VALUE(rv_result) TYPE hdb_t_icon .
    METHODS main
      IMPORTING
        !i_cd      TYPE /yga/charm_change
        !i_obs     TYPE /yga/obs_ot_tec
        !ir_cd3    TYPE ty_r_cd
        !ir_user   TYPE ty_r_user
        !ir_date   TYPE ty_r_date
        !ir_time   TYPE ty_r_time
        !i_aprov   TYPE abap_bool
*       !i_imp_p15 TYPE abap_bool OPTIONAL
        !i_imp_p15 TYPE hdb_t_icon
        !i_chang   TYPE abap_bool
        !i_descr   TYPE /yga/descricao_cd
        !i_variant TYPE slis_vari OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF tp_out.
        INCLUDE TYPE /yga/transp_ctrl_st.
      TYPES:
        color TYPE lvc_t_scol.
    TYPES: END OF tp_out .
    TYPES:
      ty_t_out TYPE STANDARD TABLE OF tp_out
               WITH DEFAULT KEY .
    TYPES:
      ty_t_trkorr TYPE STANDARD TABLE OF trkorr .
    TYPES:
      BEGIN OF tp_ot_cd,
        trkorr TYPE trkorr,
        cd     TYPE /yga/charm_change.
    TYPES: END OF tp_ot_cd .
    TYPES:
      ty_t_ot_cd TYPE STANDARD TABLE OF tp_ot_cd .
    TYPES:
      BEGIN OF ty_table_mail,
        line TYPE string,
      END OF ty_table_mail.

    DATA gv_cd TYPE /yga/charm_change.
    DATA gv_tcode TYPE syst_tcode .
    DATA gv_repid TYPE syrepi2 .
    CONSTANTS gc_trm01 TYPE syst_tcode VALUE '/YGA/TRANPS_CTRL_01' ##NO_TEXT.
    CONSTANTS gc_trm03 TYPE syst_tcode VALUE '/YGA/TRANPS_CTRL_03' ##NO_TEXT.
    DATA gv_obs TYPE /yga/obs_ot_tec .
    DATA gv_descri TYPE /yga/descricao_cd .
    DATA gr_ordem1 TYPE ty_r_ordem .
    DATA gr_user TYPE ty_r_user .
    DATA gr_date TYPE ty_r_date .
    DATA gr_time TYPE ty_r_time .
    DATA gr_text TYPE ty_r_text .
    DATA gr_uname TYPE ty_r_uname .
    DATA:
      gt_output TYPE TABLE OF tp_out .
    DATA o_salv TYPE REF TO cl_salv_table .
    DATA:
      gt_transp_ctrl TYPE TABLE OF /yga/transp_ctrl .
    DATA:
      gt_e070 TYPE TABLE OF e070 .
    DATA:
      gt_e070aux TYPE TABLE OF e070.
    DATA:
      gt_e07t TYPE TABLE OF e07t .
    DATA:
      gt_name TYPE TABLE OF v_usr_name .
    DATA gv_aprov TYPE abap_bool .
    DATA:
      gv_imp_p15 TYPE abap_bool, "# remover
      gt_imp_p15 TYPE hdb_t_icon.
*  data gr_cd1 type ty_r_cd .
    DATA gt_ot_cd TYPE ty_t_ot_cd .
    DATA gr_cd3 TYPE ty_r_cd .
    DATA gv_chang TYPE abap_bool .
    DATA gv_variant TYPE  slis_vari .
    CONSTANTS gc_area TYPE zca_area VALUE '09' ##NO_TEXT.
    CONSTANTS gc_process_name TYPE zprocesso VALUE '/YGA/CL_TRANSP_CONTROL' ##NO_TEXT.
    CONSTANTS gc_campo TYPE name_feld VALUE 'NUMBER_USER' ##NO_TEXT.
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="pt">Status (Aprovado / Não aprovado)</p>
      BEGIN OF gc_status_ot,
        aprovado  TYPE /yga/transp_ctrl-status_aprov VALUE '@01@',
        nao_aprov TYPE /yga/transp_ctrl-status_aprov VALUE '@02@',
      END OF gc_status_ot,
      "! <p class="shorttext synchronized" lang="pt">Status (Aprovado / Não aprovado)</p>
      BEGIN OF gc_importacao,
        importado TYPE /yga/transp_ctrl_st-trp_p15 VALUE '@01@',
        nao_impor TYPE /yga/transp_ctrl_st-trp_p15 VALUE '@02@',
      END OF gc_importacao .

    METHODS check_trfunction
      CHANGING
        !ct_ot_cd  TYPE ty_t_ot_cd OPTIONAL
        !cr_ordem1 TYPE ty_r_ordem OPTIONAL .
    METHODS control_color
      EXPORTING
        !et_color TYPE lvc_t_scol .
    METHODS data_display .
    METHODS data_get .
    METHODS data_insert
      EXCEPTIONS
        error_data_insert
        no_data .
    METHODS data_prepare .
    METHODS data_selection .
    METHODS delete_ot .
    METHODS edit_fields .
    METHODS exec_val .
    METHODS fill_system_fields
      IMPORTING
        VALUE(i_columns) TYPE REF TO cl_salv_columns_table
        !i_systemid      TYPE sysname .
    METHODS get_data_by_cd .
    METHODS get_system_info
      IMPORTING
        !i_systemid TYPE sysname
        !i_ordem    TYPE trkorr
      EXPORTING
        !e_icon     TYPE icon_d
        !e_flag     TYPE icon_d
        !e_data_imp TYPE as4date
        !e_hora_imp TYPE as4time .
    METHODS on_link_click
        FOR EVENT if_salv_events_actions_table~link_click OF cl_salv_events_table
      IMPORTING
        !row
        !column .
    METHODS on_user_command
        FOR EVENT added_function OF cl_salv_events .
    METHODS open_outlook .
    METHODS open_outlook_email
      IMPORTING
        !iv_body    TYPE string
        !iv_to_mail TYPE string
        !iv_cc_mail TYPE string
        !iv_subject TYPE string .
    METHODS refresh .
    METHODS save_data .
    METHODS set_columns .
    METHODS set_focus_ini .
    METHODS set_handler .
    METHODS set_impct_jump .
    METHODS set_layout .
    METHODS set_light .
    METHODS set_selection .
    METHODS set_status_aprov .
    METHODS set_status_aprov_p26 .
    "! <p class="shorttext synchronized" lang="pt">Atualiza a descrição exibindo a qtde de CDs listados</p>
    METHODS set_title .
    METHODS set_toolbar .
    METHODS update_chg .
    METHODS update_status_cd .
    "! <p class="shorttext synchronized" lang="pt">Exibe status de processamento</p>
    METHODS progress
      IMPORTING
        !percent  TYPE i OPTIONAL
        !total    TYPE i OPTIONAL
        !currency TYPE i OPTIONAL
        !message  TYPE char50 .
    "! <p class="shorttext synchronized" lang="pt">Retorna TRUE se o usuário for autorizado para enviar e-mail</p>
    METHODS user_has_authority
      RETURNING
        VALUE(rv_result) TYPE sap_bool .
    "! <p class="shorttext synchronized" lang="pt">Retorna TRUE se todos os CDs estiverem na lista</p>
    METHODS all_tr_included
      RETURNING
        VALUE(rv_result) TYPE sap_bool .

    "! <p class="shorttext synchronized" lang="pt">Retorna TRUE se esta de acordo com o filtro Imp.p/P15</p>
    METHODS filter_production
      IMPORTING
        !im_output       TYPE tp_out
      RETURNING
        VALUE(rv_result) TYPE sap_bool .

ENDCLASS.



CLASS /yga/cl_transp_control IMPLEMENTATION.


  METHOD at_selection_screen_output.

    LOOP AT SCREEN.

*   /YGA/TRANPS_CTRL_01 -> Criar
      IF screen-group1 = 'TR1' AND gv_tcode = gc_trm01.
        screen-active = 1.
*   /YGA/TRANPS_CTRL_01 -> Visualizar
      ELSEIF screen-group1 = 'TR3' AND gv_tcode = gc_trm03.
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.

      IF screen-name CS 'BLOCK'.
        screen-active = 1.
      ENDIF.

      MODIFY SCREEN.

    ENDLOOP.

  ENDMETHOD.


  METHOD check_screen.

*   /YGA/TRANPS_CTRL_01 -> Criar
    IF gv_tcode = gc_trm01.

*   Se existir impacto em jump, obrigatório preencher observações
      IF iv_impac = abap_true AND iv_obs_im IS INITIAL.

        MESSAGE s159(/yga/jump1) DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD check_trfunction.

    DATA lt_options     TYPE STANDARD TABLE OF rfc_db_opt.
    DATA lt_fields_e070 TYPE STANDARD TABLE OF rfc_db_fld.
    DATA lt_data        TYPE STANDARD TABLE OF tab512.
    DATA lt_e070_rfc    TYPE TABLE OF e070.
    DATA ls_data_e070   TYPE e070.

    IF cr_ordem1 IS SUPPLIED AND ct_ot_cd IS SUPPLIED.

      SELECT *
        FROM e070
        INTO TABLE @gt_e070aux
       WHERE trkorr IN @gr_ordem1.
      IF sy-subrc = 0.

        LOOP AT gt_e070aux ASSIGNING FIELD-SYMBOL(<fs_e070>) WHERE trfunction = 'T'.
          DELETE gt_ot_cd WHERE trkorr = <fs_e070>-trkorr.
          DELETE gr_ordem1 WHERE low = <fs_e070>-trkorr.
        ENDLOOP.

      ENDIF.

    ELSEIF cr_ordem1 IS NOT SUPPLIED AND ct_ot_cd IS SUPPLIED.

      FREE gt_e070aux[].
      SELECT *
        FROM e070
        INTO TABLE @gt_e070aux
        FOR ALL ENTRIES IN @gt_ot_cd
       WHERE trkorr = @gt_ot_cd-trkorr
         AND trfunction <> 'T'.
      IF sy-subrc = 0.

        DATA(gt_ot_cd_aux) = gt_ot_cd[].
        LOOP AT gt_ot_cd_aux ASSIGNING FIELD-SYMBOL(<fs_ot_cd_aux>).
          IF NOT line_exists( gt_e070aux[ trkorr = <fs_ot_cd_aux>-trkorr ] ).
            DELETE gt_ot_cd WHERE trkorr = <fs_ot_cd_aux>-trkorr.
          ENDIF.
        ENDLOOP.

      ENDIF.

    ENDIF.

*   Obter mapeamento máquina - RFC
    SELECT *
      FROM zca_rfc_mapping
      INTO TABLE @DATA(lt_rfc_map).

*   Caso a OT Toc não pertença à maquina atual, é necessário validar por RFC

    FREE: lt_options[].
    LOOP AT gr_ordem1 ASSIGNING FIELD-SYMBOL(<fs_ordem1>) WHERE low(3) <> sy-sysid(3) .
      AT NEW low(4).
        lt_options = VALUE #( BASE lt_options ( text = |TRKORR = '{ <fs_ordem1>-low }'| ) ).
        CONTINUE.
      ENDAT.
      lt_options = VALUE #( BASE lt_options ( text = |OR TRKORR = '{ <fs_ordem1>-low }'| ) ).
    ENDLOOP.

    FREE: lt_fields_e070[].
    APPEND INITIAL LINE TO lt_fields_e070 ASSIGNING FIELD-SYMBOL(<fs_fields>).
    <fs_fields>-fieldname = 'TRKORR'.
    APPEND INITIAL LINE TO lt_fields_e070 ASSIGNING <fs_fields>.
    <fs_fields>-fieldname = 'TRFUNCTION'.

    LOOP AT lt_rfc_map ASSIGNING FIELD-SYMBOL(<fs_rfc_map>).

      DATA(lt_options_aux) = lt_options[].
      DELETE lt_options_aux WHERE text NS <fs_rfc_map>-ot_prefix.

      IF lt_options_aux IS NOT INITIAL.

        FREE: lt_data[].
        CALL FUNCTION 'RFC_READ_TABLE' DESTINATION <fs_rfc_map>-rfc_dest
          EXPORTING
            query_table          = 'E070'
          TABLES
            options              = lt_options
            fields               = lt_fields_e070
            data                 = lt_data
          EXCEPTIONS
            table_not_available  = 1
            table_without_data   = 2
            option_not_valid     = 3
            field_not_valid      = 4
            not_authorized       = 5
            data_buffer_exceeded = 6
            OTHERS               = 7.
        IF sy-subrc = 0.
          CLEAR: ls_data_e070.
          LOOP AT lt_data INTO ls_data_e070.
            lt_e070_rfc = VALUE #( BASE lt_e070_rfc ( ls_data_e070 ) ).
          ENDLOOP.

          LOOP AT lt_e070_rfc ASSIGNING FIELD-SYMBOL(<fs_e070_rfc>) WHERE trfunction = 'T'.
            DELETE gt_ot_cd  WHERE trkorr = <fs_e070_rfc>-trkorr.
            DELETE gr_ordem1 WHERE low = <fs_e070_rfc>-trkorr.
          ENDLOOP.

        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    gv_repid = i_repid.
    gv_tcode = sy-tcode.

  ENDMETHOD.


  METHOD control_color.

    DATA: ls_lvc_s_scol TYPE lvc_s_scol,
          lt_dfies      TYPE ddfields,
          lo_tabdescr   TYPE REF TO cl_abap_structdescr,
          lv_data       TYPE REF TO data.

    CLEAR: ls_lvc_s_scol.
    FREE: et_color, lt_dfies.

    CREATE DATA lv_data LIKE LINE OF gt_output.

    lo_tabdescr ?= cl_abap_structdescr=>describe_by_data_ref( lv_data ).

    lt_dfies = cl_salv_data_descr=>read_structdescr( lo_tabdescr ).

    LOOP AT lt_dfies ASSIGNING FIELD-SYMBOL(<fs_dfies>).

      ls_lvc_s_scol-fname     = <fs_dfies>-fieldname.
      ls_lvc_s_scol-color-col = 3.
      ls_lvc_s_scol-color-int = 1.
      APPEND ls_lvc_s_scol TO et_color.

    ENDLOOP.

  ENDMETHOD.


  METHOD data_display.

    me->progress(
      EXPORTING
        percent  = 90
        message  = 'Visualização de dados'(m02)
    ).

    TRY.
        CALL METHOD cl_salv_table=>factory(
          IMPORTING
            r_salv_table = o_salv
          CHANGING
            t_table      = gt_output[] ).
      CATCH cx_salv_msg  .
    ENDTRY.

    set_layout( ).
    me->set_title( ).
    set_toolbar( ).
    set_columns( ).
    set_selection( ).
    set_handler( ).

    o_salv->display( ).

  ENDMETHOD.


  METHOD data_get.

    DATA: lt_e070 TYPE ty_t_trkorr.

    me->progress(
      EXPORTING
        percent  = 15
        message  = 'Determinar dados da BD'(m04)
    ).

*   Obter dados da tabela /YGA/YGA/T_TRP_INFO
    IF gv_tcode = gc_trm01.

      SELECT *
        FROM /yga/transp_ctrl
        INTO TABLE @gt_transp_ctrl
        WHERE ernam        EQ @sy-uname
          AND erdat        IN @gr_date
          AND erzet        IN @gr_time
          AND charm_change IN @gr_cd3.
    ELSE.
      SELECT *
        FROM /yga/transp_ctrl
        INTO TABLE @gt_transp_ctrl
       WHERE ernam        IN @gr_user
         AND erdat        IN @gr_date
         AND erzet        IN @gr_time
         AND charm_change IN @gr_cd3.
    ENDIF.

    IF gv_aprov IS NOT INITIAL.
      DELETE gt_transp_ctrl WHERE status_aprov <> icon_checked.
    ENDIF.

    IF gt_transp_ctrl[] IS INITIAL.
      MESSAGE s725(/yga/jump) DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
      RETURN.
    ENDIF.

*   Obter informação detalhada de cada ordem de transporte
    SELECT trkorr trfunction trstatus tarsystem korrdev as4user
           as4date as4time
      FROM e070
      INTO CORRESPONDING FIELDS OF TABLE gt_e070
       FOR ALL ENTRIES IN gt_transp_ctrl
     WHERE trkorr EQ gt_transp_ctrl-trkorr
       AND strkorr = ''.

    LOOP AT gt_transp_ctrl INTO DATA(lw_treq).
      READ TABLE gt_e070 TRANSPORTING NO FIELDS WITH KEY trkorr = lw_treq-trkorr.
      IF sy-subrc NE 0.
        APPEND INITIAL LINE TO gt_e070 ASSIGNING FIELD-SYMBOL(<fs_e070>).
        <fs_e070>-trkorr  = lw_treq-trkorr.
        <fs_e070>-as4user = lw_treq-ernam.
        lt_e070 = VALUE #( BASE lt_e070 ( lw_treq-trkorr ) ).
      ENDIF.
    ENDLOOP.

    SORT gt_e070 BY trkorr.
    DELETE ADJACENT DUPLICATES FROM gt_e070 COMPARING trkorr.

    IF gt_e070[] IS NOT INITIAL.

*   Obter textos de cada ordem transport
      SELECT trkorr langu as4text
        FROM e07t
        INTO CORRESPONDING FIELDS OF TABLE gt_e07t
     FOR ALL ENTRIES IN gt_e070
       WHERE trkorr = gt_e070-trkorr.

*   Obter nome titular OT
      SELECT DISTINCT bname name_text
        FROM v_usr_name
        INTO CORRESPONDING FIELDS OF TABLE gt_name
     FOR ALL ENTRIES IN gt_e070
       WHERE bname = gt_e070-as4user.

*   Obter nome criador registo
      SELECT DISTINCT bname name_text
        FROM v_usr_name
        APPENDING CORRESPONDING FIELDS OF TABLE gt_name
     FOR ALL ENTRIES IN gt_transp_ctrl
       WHERE bname = gt_transp_ctrl-ernam.

    ENDIF.

  ENDMETHOD.


  METHOD data_insert.

    DATA: lt_transp_ctrl TYPE TABLE OF /yga/transp_ctrl,
          lt_message_tab TYPE esp1_message_tab_type.

    FREE: lt_transp_ctrl[], lt_message_tab[].

    SORT gr_ordem1 BY low.
    DELETE ADJACENT DUPLICATES FROM gr_ordem1 COMPARING low.

*  Verificar se vao ser inseridas ordens já existentes
    SELECT trkorr
      FROM /yga/transp_ctrl
      INTO TABLE @DATA(lt_trkorr)
     WHERE trkorr IN @gr_ordem1.
    IF sy-subrc = 0.

      DATA(gr_ordem1_aux) = gr_ordem1.
      LOOP AT gr_ordem1_aux ASSIGNING FIELD-SYMBOL(<fs_ordem1_aux>).
        IF line_exists( lt_trkorr[ trkorr = <fs_ordem1_aux>-low ] ).
          DELETE gr_ordem1 WHERE low = <fs_ordem1_aux>-low.

*   Atualizar CD
          READ TABLE gt_ot_cd ASSIGNING FIELD-SYMBOL(<fs_ot_cd>) WITH KEY trkorr = <fs_ordem1_aux>-low.
          IF sy-subrc = 0.
            UPDATE /yga/transp_ctrl SET charm_change = <fs_ot_cd>-cd WHERE trkorr = <fs_ot_cd>-trkorr.
          ENDIF.

          APPEND INITIAL LINE TO lt_message_tab ASSIGNING FIELD-SYMBOL(<fs_message>).
          <fs_message>-msgno = '722'.
          <fs_message>-msgty = 'I'.
          <fs_message>-msgid = '/YGA/JUMP'.
          <fs_message>-msgv1 = <fs_ordem1_aux>-low.

        ENDIF.
      ENDLOOP.

      IF lt_message_tab[] IS NOT INITIAL.
        CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
          TABLES
            i_message_tab = lt_message_tab.
      ENDIF.

    ENDIF.

    IF gr_ordem1[] IS INITIAL.
      MESSAGE s723(/yga/jump) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

*   Obter nr sequencia actual.
    SELECT MAX( seq_nr )
      FROM /yga/transp_ctrl
      INTO @DATA(lv_seq_nr).
    IF sy-subrc IS NOT INITIAL.
      lv_seq_nr = 0.
    ENDIF.

*   Criar tabela dados a inserir
    LOOP AT gr_ordem1 ASSIGNING FIELD-SYMBOL(<fs_ordem1>).
      ADD 1 TO lv_seq_nr.

      APPEND INITIAL LINE TO lt_transp_ctrl ASSIGNING FIELD-SYMBOL(<fs_transp_ctrl>).
      <fs_transp_ctrl>-mandt        = sy-mandt.
      <fs_transp_ctrl>-seq_nr       = lv_seq_nr.
      <fs_transp_ctrl>-trkorr       = <fs_ordem1>-low.
      READ TABLE gt_e070aux ASSIGNING FIELD-SYMBOL(<fs_e070>) WITH KEY trkorr = <fs_ordem1>-low.
      IF sy-subrc = 0.
        <fs_transp_ctrl>-ernam = <fs_e070>-as4user.
      ELSE.
        <fs_transp_ctrl>-ernam        = sy-uname.
      ENDIF.
      <fs_transp_ctrl>-erdat        = sy-datum.
      <fs_transp_ctrl>-erzet        = sy-uzeit.
      <fs_transp_ctrl>-obs_tecnicas = gv_obs.
      <fs_transp_ctrl>-descricao_cd = gv_descri.

      READ TABLE gt_ot_cd ASSIGNING FIELD-SYMBOL(<fs_cd>) WITH KEY trkorr = <fs_ordem1>-low.
      IF sy-subrc = 0.
        <fs_transp_ctrl>-charm_change = <fs_cd>-cd.
      ENDIF.

      <fs_transp_ctrl>-status_aprov     = icon_incomplete.

    ENDLOOP.

    IF lt_transp_ctrl IS NOT INITIAL.

*   Adicionar entradas
      INSERT /yga/transp_ctrl FROM TABLE lt_transp_ctrl.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.
      ELSE.
        ROLLBACK WORK.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD data_prepare.

    DATA:
      ls_output LIKE LINE OF gt_output.

    me->progress(
      EXPORTING
        percent  = 25
        message  = 'Ajustar dados para output'(m05)
    ).

    control_color( IMPORTING et_color = DATA(lt_color) ).

    SORT gt_transp_ctrl BY seq_nr.

    LOOP AT gt_transp_ctrl ASSIGNING FIELD-SYMBOL(<fs_transp_ctrl>).
      CLEAR ls_output.

      DATA(lv_message) = CONV char50( |{ 'Processar'(m06) } { sy-tabix }| ) .
      lv_message = |{ lv_message } { 'de'(m07) } { lines( me->gt_transp_ctrl ) }{ '...'(m08) }| .

      me->progress(
        EXPORTING
          total    = lines( gt_transp_ctrl )
          currency = sy-tabix
          message  = lv_message
      ).

*   Ler detalhe da ordem a tratar
      READ TABLE gt_e070 ASSIGNING FIELD-SYMBOL(<fs_e070>) WITH KEY trkorr = <fs_transp_ctrl>-trkorr.

*   Preencher datas da Bo Derek e pedidos de transporte.
      MOVE-CORRESPONDING <fs_transp_ctrl> TO ls_output.

*   Preencher detalhes da ordem
      ls_output-trkorr     = <fs_e070>-trkorr.

*   Preencher nome criador do registo
      READ TABLE gt_name ASSIGNING FIELD-SYMBOL(<fs_name>) WITH KEY bname = ls_output-ernam.
      IF sy-subrc = 0.
        ls_output-name_text_ernam = <fs_name>-name_text.
      ENDIF.

*   Preencher texto da ordem transporte
      READ TABLE gt_e07t ASSIGNING FIELD-SYMBOL(<fs_e07t>) WITH KEY trkorr = ls_output-trkorr BINARY SEARCH.
      IF sy-subrc = 0.
        ls_output-as4text = <fs_e07t>-as4text.
      ENDIF.

      IF ls_output-highlight = abap_true.
        ls_output-color = lt_color.
      ENDIF.

*     P15
      me->get_system_info( EXPORTING i_systemid = 'P15'
                                     i_ordem    = <fs_transp_ctrl>-trkorr
                           IMPORTING e_icon     = ls_output-status_p15
                                     e_flag     = ls_output-trp_p15
                                     e_data_imp = ls_output-data_imp_p15
                                     e_hora_imp = ls_output-hora_imp_p15 ).

**     Mostrar apenas CDs importados em P15 com o pisco
*      IF gv_imp_p15 IS INITIAL.
*        IF ls_output-trp_p15 = icon_checked.
*          CONTINUE.
*        ENDIF.
*      ENDIF.

      " Mostrar apenas CDs importados em P15 com o pisco
      IF ( me->filter_production( ls_output ) EQ abap_false ) .
        CONTINUE.
      ENDIF.

*     K15
      me->get_system_info( EXPORTING i_systemid = 'K15'
                                     i_ordem    = <fs_transp_ctrl>-trkorr
                           IMPORTING e_icon     = ls_output-status_k15
                                     e_flag     = ls_output-trp_k15
                                     e_data_imp = ls_output-data_imp_k15
                                     e_hora_imp = ls_output-hora_imp_k15 ).

*     Q15
      me->get_system_info( EXPORTING i_systemid = 'Q15'
                                     i_ordem    = <fs_transp_ctrl>-trkorr
                           IMPORTING e_icon     = ls_output-status_q15
                                     e_flag     = ls_output-trp_q15
                                     e_data_imp = ls_output-data_imp_q15
                                     e_hora_imp = ls_output-hora_imp_q15 ).

      ls_output-log_tr = '@B_NODP@' .

      APPEND ls_output TO gt_output.

    ENDLOOP.

    IF gv_chang EQ abap_true.
      update_chg( ).
    ENDIF.

*    sort gt_output by charm_change ascending.
    SORT gt_output BY charm_change erdat erzet ASCENDING.

  ENDMETHOD.


  METHOD data_selection.

    me->progress( EXPORTING
      percent = 10
      message = 'Determinar dados...'(m01)
    ).

*   Determinar dados da BD
    data_get( ).

*   Ajustar dados para output
    data_prepare( ).

  ENDMETHOD.


  METHOD delete_ot.

    DATA: lv_answer TYPE n.

    CLEAR: lv_answer.

    DATA(lt_rows) = o_salv->get_selections( )->get_selected_rows( ).

    IF lt_rows[] IS INITIAL.
      MESSAGE i000(/yga/jump) WITH 'Marcar Linha'(007) .
      RETURN.
    ENDIF.

    MESSAGE s724(/yga/jump) INTO DATA(lv_question).
    CONDENSE lv_question.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Eliminar OT(s)'
        text_question         = lv_question
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        display_cancel_button = ''
      IMPORTING
        answer                = lv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    IF sy-subrc <> 0 OR lv_answer = 2.

      RETURN.

    ELSE.

      LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<fs_row>).

        READ TABLE gt_output ASSIGNING FIELD-SYMBOL(<fs_output>) INDEX <fs_row>.
        IF sy-subrc EQ 0.

          DELETE FROM /yga/transp_ctrl
           WHERE trkorr = <fs_output>-trkorr.

        ENDIF.

      ENDLOOP.

      me->refresh( ).

    ENDIF.

  ENDMETHOD.


  METHOD edit_fields.

    DATA: lt_fields TYPE TABLE OF /yga/transp_control_edit,
          ls_fields TYPE /yga/transp_control_edit.

    DATA(lt_rows) = o_salv->get_selections( )->get_selected_rows( ).

    IF lt_rows[] IS INITIAL.
      MESSAGE i000(/yga/jump) WITH 'Marcar Linha'(007) .
      RETURN.
    ENDIF.

    FREE: lt_fields.

    LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<fs_row>).
      READ TABLE gt_output ASSIGNING FIELD-SYMBOL(<fs_output>) INDEX <fs_row>.
      IF sy-subrc = 0 .
        MOVE-CORRESPONDING <fs_output> TO ls_fields.
        APPEND ls_fields TO lt_fields.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION '/YGA/TRANSP_CONTROL_EDIT'
      TABLES
        et_tab = lt_fields.

    LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).
      READ TABLE gt_output ASSIGNING <fs_output> WITH KEY trkorr = <fs_fields>-trkorr .
      IF sy-subrc = 0.
        <fs_output>-descricao_cd     = <fs_fields>-descricao_cd.
        <fs_output>-obs_tecnicas     = <fs_fields>-obs_tecnicas.
        <fs_output>-obs_email        = <fs_fields>-obs_email.
      ENDIF.
    ENDLOOP .

  ENDMETHOD.


  METHOD exec_val.

    DATA: lt_transp_ctrl TYPE /yga/transp_ctrl_tt.

    FREE: lt_transp_ctrl[].

    DATA(lt_rows) = o_salv->get_selections( )->get_selected_rows( ).

    IF lt_rows[] IS INITIAL.
      MESSAGE i000(/yga/jump) WITH 'Marcar Linha'(007) .
      RETURN.
    ENDIF.

    LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<fs_row>).
      READ TABLE gt_output ASSIGNING FIELD-SYMBOL(<fs_output>) INDEX <fs_row>.
      IF sy-subrc = 0 .
        APPEND INITIAL LINE TO lt_transp_ctrl ASSIGNING FIELD-SYMBOL(<fs_transp_ctrl>).
        <fs_transp_ctrl>-trkorr       = <fs_output>-trkorr.
        <fs_transp_ctrl>-charm_change = <fs_output>-charm_change.
        <fs_transp_ctrl>-status_aprov = <fs_output>-status_aprov.
        <fs_transp_ctrl>-obs_tecnicas = <fs_output>-obs_tecnicas.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION '/YGA/TRANSP_CONTROL_VAL'
      TABLES
        t_transp_ctrl = lt_transp_ctrl.

    me->refresh( ).

  ENDMETHOD.


  METHOD fill_system_fields.

    CONSTANTS: lc_status(6)  TYPE c VALUE 'Status',
               lc_st_fd(7)   TYPE c VALUE 'STATUS_',
               lc_tp_fd(4)   TYPE c VALUE 'TRP_',
               lc_data(4)    TYPE c VALUE 'Data',
               lc_data_fd(9) TYPE c VALUE 'DATA_IMP_',
               lc_hora(4)    TYPE c VALUE 'Hora',
               lc_hora_fd(9) TYPE c VALUE 'HORA_IMP_'.

    DATA: lo_column   TYPE REF TO cl_salv_column_table,
          lv_field    TYPE lvc_fname,
          lv_text(20) TYPE c,
          lv_long     TYPE scrtext_l.

    CLEAR: lv_field, lv_text, lv_long.

    lv_field = lc_tp_fd && i_systemid.
    TRY.
        lo_column ?= i_columns->get_column( lv_field ).

        lv_long = i_systemid.

        lo_column->set_long_text( lv_long ).
        lo_column->set_medium_text( ' ' ).
        lo_column->set_short_text( ' ' ).
        lo_column->set_alignment( 3 ).

      CATCH cx_salv_not_found.
    ENDTRY.

    CLEAR: lv_field, lv_text, lv_long.

    lv_field = lc_st_fd && i_systemid.
    TRY.
        lo_column ?= i_columns->get_column( lv_field ).

        CLEAR lv_text.
        CONCATENATE lc_status i_systemid INTO lv_text SEPARATED BY space.
        lv_long = lv_text.

        lo_column->set_long_text( lv_long ).
        lo_column->set_medium_text( ' ' ).
        lo_column->set_short_text( ' ' ).
        lo_column->set_alignment( 3 ).

      CATCH cx_salv_not_found.
    ENDTRY.

    lv_field = lc_data_fd && i_systemid.
    TRY.
        lo_column ?= i_columns->get_column( lv_field ).

        CLEAR lv_text.
        CONCATENATE lc_data i_systemid INTO lv_text SEPARATED BY space.
        lv_long = lv_text.

        lo_column->set_long_text( lv_long ).
        lo_column->set_medium_text( ' ' ).
        lo_column->set_short_text( ' ' ).
        lo_column->set_alignment( 3 ).

      CATCH cx_salv_not_found.
    ENDTRY.

    lv_field = lc_hora_fd && i_systemid.
    TRY.
        lo_column ?= i_columns->get_column( lv_field ).

        CLEAR lv_text.
        CONCATENATE lc_hora i_systemid INTO lv_text SEPARATED BY space.
        lv_long = lv_text.

        lo_column->set_long_text( lv_long ).
        lo_column->set_medium_text( ' ' ).
        lo_column->set_short_text( ' ' ).
        lo_column->set_alignment( 3 ).

      CATCH cx_salv_not_found.
    ENDTRY.

  ENDMETHOD.


  METHOD get_data_by_cd.

    DATA: lt_request TYPE TABLE OF trkorr,
          lt_message TYPE esp1_message_tab_type,
          lt_options TYPE STANDARD TABLE OF rfc_db_opt.

    FREE: lt_message[].

*    SORT gr_cd1[] BY low.
*    DELETE ADJACENT DUPLICATES FROM gr_cd1[] COMPARING low.

*    LOOP AT gr_cd1 ASSIGNING FIELD-SYMBOL(<fs_cd1>).
*
*      CONDENSE <fs_cd1>-low NO-GAPS.

    FREE: lt_request[].

    TRY .

        CALL FUNCTION 'ZCHARM_OT_FROM_CD' DESTINATION 'SM_SM1CLNT900_TRUSTED'
          EXPORTING
            i_change  = gv_cd "<fs_cd1>-low
          TABLES
            i_request = lt_request[].

      CATCH cx_root.

        MESSAGE s160(/yga/jump1) DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.

    ENDTRY.

    IF lt_request[] IS NOT INITIAL.

      LOOP AT lt_request[] ASSIGNING FIELD-SYMBOL(<fs_request>).

        APPEND INITIAL LINE TO gt_ot_cd ASSIGNING FIELD-SYMBOL(<fs_ot_cd>).
        <fs_ot_cd>-trkorr = <fs_request>.
        <fs_ot_cd>-cd     = gv_cd. "<fs_cd1>-low.
        UNASSIGN <fs_ot_cd>.

        APPEND INITIAL LINE TO gr_ordem1 ASSIGNING FIELD-SYMBOL(<fs_r_trkorr>).
        <fs_r_trkorr>-low    = <fs_request>.
        <fs_r_trkorr>-sign   = 'I'.
        <fs_r_trkorr>-option = 'EQ'.
        UNASSIGN <fs_r_trkorr>.

      ENDLOOP.

    ELSE.

*   Adicionar mensagem de erro
      APPEND INITIAL LINE TO lt_message ASSIGNING FIELD-SYMBOL(<fs_message>).
      <fs_message>-msgid  = '/YGA/JUMP1'.
      <fs_message>-msgty  = 'E'.
      <fs_message>-msgno  = '109'.
      <fs_message>-msgv1  = gv_cd. "<fs_cd1>-low.

    ENDIF.

*    endloop.

*   Eliminar OTs transporte de cópias
    IF gr_ordem1[] IS NOT INITIAL.

      check_trfunction( CHANGING ct_ot_cd  = gt_ot_cd
                                 cr_ordem1 = gr_ordem1 ).

    ENDIF.

    IF lt_message[] IS NOT INITIAL.

      CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
        TABLES
          i_message_tab = lt_message.

    ENDIF.

  ENDMETHOD.


  METHOD get_system_info.

    DATA: ls_request  TYPE ctslg_request_info,
          ls_systems  TYPE ctslg_system,
          ls_steps    TYPE ctslg_step,
          ls_actions  TYPE ctslg_action,
          lv_lines    TYPE i,
          ls_settings TYPE ctslg_settings,
          lt_systems  TYPE sysnames.

    CLEAR: ls_request, ls_systems, ls_steps, ls_actions, lv_lines, ls_settings.
    FREE: lt_systems.

    APPEND INITIAL LINE TO lt_systems ASSIGNING FIELD-SYMBOL(<fs_systems>).
    <fs_systems>-name = i_systemid.

    ls_settings-systems = lt_systems[].

    CALL FUNCTION 'TR_READ_GLOBAL_INFO_OF_REQUEST'
      EXPORTING
        iv_trkorr   = i_ordem
        iv_dir_type = 'T'
        is_settings = ls_settings
      IMPORTING
        es_cofile   = ls_request-cofile.

    IF sy-subrc = 0.

      CLEAR: ls_systems.
      READ TABLE ls_request-cofile-systems WITH KEY systemid = i_systemid INTO ls_systems.
      IF sy-subrc = 0.

*        LOOP AT ls_request-cofile-systems ASSIGNING FIELD-SYMBOL(<fs_rc>).

*        ENDLOOP.

*   Eliminar entradas do ChARM:
*        - Não importável localmente (central CTS)
*        - Marcação p/importação
        DELETE ls_systems-steps WHERE stepid EQ 'p' OR stepid EQ '!'.

*   Obter ultima entrada dos steps
        CLEAR: lv_lines, ls_steps.
        DESCRIBE TABLE ls_systems-steps LINES lv_lines.
        READ TABLE ls_systems-steps INTO ls_steps INDEX lv_lines.
        IF sy-subrc = 0.

          e_flag = icon_checked.

*   Obter ultima entrada das actions
          CLEAR: lv_lines, ls_actions.
          DESCRIBE TABLE ls_steps-actions LINES lv_lines.
          READ TABLE ls_steps-actions INTO ls_actions INDEX lv_lines.

*   Data e Hora de importação
          e_data_imp = ls_actions-date.
          e_hora_imp = ls_actions-time.

*   Verificar se OT foi transportada com erro (quando RC = 8)
          CASE ls_request-cofile-rc.
            WHEN '008'.
              e_icon = icon_red_light.
            WHEN '004'.
              e_icon = icon_yellow_light.
            WHEN '000' OR '001'.
              e_icon = icon_green_light.
            WHEN OTHERS.
              e_icon = icon_message_question_small.
          ENDCASE.

        ELSE.

          e_flag = icon_incomplete.

        ENDIF.

      ELSE.

        e_flag = icon_incomplete.

      ENDIF.

    ELSE.

      e_icon = icon_question.
      e_flag = icon_question.

    ENDIF.

  ENDMETHOD.


  METHOD init_erdat.

    CHECK cr_erdat[] IS INITIAL.

    APPEND INITIAL LINE TO cr_erdat ASSIGNING FIELD-SYMBOL(<fs_erdat>).
    <fs_erdat>-low    = sy-datum - 90.
    <fs_erdat>-high   = sy-datum.
    <fs_erdat>-sign   = /yga/cl_constants_intf=>ac_sign_i.
    <fs_erdat>-option = /yga/cl_constants_intf=>ac_option_bt.

  ENDMETHOD .


  METHOD get_varian .

    CONSTANTS:
      lc_a TYPE c VALUE 'A'.

    DATA(ls_variant) = VALUE disvariant(
      report     =  sy-repid
      variant    = ch_varian
    ) .

    CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
      EXPORTING
        is_variant    = ls_variant
        i_save        = lc_a
      IMPORTING
        es_variant    = ls_variant
      EXCEPTIONS
        not_found     = 1
        program_error = 2
        OTHERS        = 3.

    IF ( sy-subrc NE 0 ) .
      RETURN .
    ENDIF.

    ch_varian = ls_variant-variant.

  ENDMETHOD .


  METHOD get_prod_filter .

    IF ( im_ambos IS NOT INITIAL ) .
      rv_result = VALUE #( ( me->gc_importacao-importado )
                           ( me->gc_importacao-nao_impor ) ) .
      RETURN .
    ENDIF .

    IF ( im_aprovado IS NOT INITIAL ) .
      rv_result = VALUE #( ( me->gc_importacao-importado ) ) .
    ENDIF .

    IF ( im_nao_apro IS NOT INITIAL ) .
      rv_result = VALUE #( ( me->gc_importacao-nao_impor ) ) .
    ENDIF .

    IF ( lines( rv_result ) GT 0 ) .
      SORT rv_result ASCENDING .
    ENDIF .

  ENDMETHOD .


  METHOD main.

    CLEAR gt_ot_cd[].

    gv_obs         = i_obs.
    gv_cd          = i_cd.
    gv_descri      = i_descr.
    gr_user[]      = ir_user[].
    gr_date[]      = ir_date[].
    gr_time[]      = ir_time[].
    gv_aprov       = i_aprov.
*   gv_imp_p15     = i_imp_p15.
    me->gt_imp_p15 = i_imp_p15.
*   gr_cd1         = ir_cd1.
    gr_cd3         = ir_cd3.
    gv_chang       = i_chang.
    me->gv_variant = i_variant .

    zcl_ca_fixed_value=>get_range( EXPORTING i_zarea             = gc_area
                                             i_zprocesso         = gc_process_name
                                             i_campo             = gc_campo
                                   IMPORTING e_range_fixed_value = DATA(lr_range_fixed_value) ).

    gr_uname = lr_range_fixed_value.

*   Inserção de dados
    IF gv_tcode = gc_trm01.

*     OTs inseridas por CD
      get_data_by_cd( ).

      data_insert( EXCEPTIONS error_data_insert = 1
                              OTHERS            = 2 ).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      IF gr_date IS INITIAL.
        gr_date = VALUE #( ( sign = 'I' option = 'BT' low = sy-datum - 2 high = sy-datum ) ).
      ENDIF.

    ENDIF.

*   Determinar dados
    data_selection( ).

*   Visualização de dados
    data_display( ).

  ENDMETHOD.


  METHOD on_link_click.

    FIELD-SYMBOLS:
      <fs_output> LIKE LINE OF gt_output,
      <fs_field>  TYPE any.

    IF ( NOT line_exists( me->gt_output[ row ] ) ) .
      RETURN .
    ENDIF .

    READ TABLE gt_output ASSIGNING <fs_output> INDEX row.
    IF ( <fs_output> IS NOT ASSIGNED ) .
      RETURN .
    ENDIF .

    ASSIGN COMPONENT column OF STRUCTURE <fs_output> TO <fs_field> .
    IF ( <fs_field> IS INITIAL ) .
      RETURN.
    ENDIF.

    CASE column .

      WHEN 'TRKORR ' .
        CALL FUNCTION 'TR_PRESENT_REQUEST'
          EXPORTING
            iv_trkorr = <fs_field>.

      WHEN 'LOG_TR' .
        CALL FUNCTION 'TR_LOG_OVERVIEW_REQUEST'
          EXPORTING
            iv_trkorr = <fs_output>-trkorr.

      WHEN OTHERS .

    ENDCASE .

  ENDMETHOD.


  METHOD on_user_command.

    CONSTANTS:
      c_msgno TYPE syst_msgno VALUE '277',
      c_msgty TYPE syst_msgty VALUE 'I',
      c_msgid TYPE char10 VALUE '/YGA/JUMP3'.
    DATA:
      lt_messagem TYPE esp1_message_tab_type.

    CLEAR lt_messagem[].

    CASE sy-ucomm.
      WHEN 'REFRESH'.
        refresh( ).

      WHEN 'LIGHT'.
        set_light( ).

      WHEN 'EDIT'.
        edit_fields( ).

*      when 'DELETE_OT'.
*        delete_ot( ).

      WHEN 'APROVAR'.
        set_status_aprov( ).

      WHEN 'VALIDAR'.
        exec_val( ).

      WHEN 'UPDATE_CHG'.
        update_chg( ).

*      when 'STATUS_CD'.
*        update_status_cd( ).

      WHEN 'EMAIL'.

        IF ( me->user_has_authority( ) EQ abap_false ) .
          MESSAGE i000(/yga/jump) WITH 'Não tem permissão para a acção :)'(009).
          RETURN.
        ENDIF.

        " Validar que todas as entradas de um CD estão aprovadas para P15
        IF ( me->all_tr_included( ) EQ abap_false ) .
          MESSAGE i227(/yga/jump3) INTO DATA(lv_dummy).
          RETURN .
        ENDIF .

        open_outlook( ).

      WHEN 'BACK' OR 'EXIT'.

      WHEN OTHERS.
    ENDCASE.

    o_salv->refresh( ).

  ENDMETHOD.


  METHOD open_outlook.

    DATA: lv_to_mail    TYPE string,
          lv_cc_mail    TYPE string,
          lv_body       TYPE string,
          lv_body_aux   TYPE string,
          lv_format     TYPE string,
          lv_subject    TYPE string,
          ls_header     TYPE thead,
          lt_lines      TYPE STANDARD TABLE OF tline,
          lt_table_mail TYPE STANDARD TABLE OF ty_table_mail,
          lv_to_email   TYPE string.

    lt_table_mail = VALUE #( ( line = '<style>' )
                             ( line = 'table{font-family:calibri;font-size:10pt;border-spacing:0}' )
                             ( line = '.head{border-left:#ccc 1px solid;border-right:#fff 1px solid;border-top:#ccc 1px solid;border-bottom:#fff 1px solid;background-color:#a9a9a9;color:FFF;padding:5px;text-align:center;font-weight:bold; }' )
                             ( line = '.head_last{border-right:#ccc 1px solid;border-top:#ccc 1px solid;border-bottom:#fff 1px solid;background-color:#a9a9a9;color:FFF;padding:5px;text-align:center;font-weight:bold; }' )
                             ( line = '.head100{ width:100px;}' )
                             ( line = '.head300{ width:300px;}' )
                             ( line = '.row{border-left:#ccc 1px solid;border-bottom:#ccc 1px solid;padding:5px}' )
*                             ( line = '.row:nth-child(even){background-color:#f2f2f2}' )
                             ( line = '.row_last{ border-left:#ccc 1px solid;border-right:#ccc 1px solid;border-bottom:#ccc 1px solid;padding:5px}' )
                             ( line = '</style>' )
                             ( line = '<table>' )
                             ( line = '<tr>' )
                             ( line = |<td class="head head100">{ 'CD Charm' }</td>| )
                             ( line = |<td class="head head100">{ 'Ordem' }</td>| )
                             ( line = |<td class="head head300">{ 'Descrição CD' }</td>| )
                             ( line = |<td class="head head100">{ 'Responsável' }</td>| )
                             ( line = |<td class="head head100">{ 'Aprovação AO JUMP' }</td>| )
                             ( line = |<td class="head_last head100">{ 'Observações' }</td>| )
                             ( line = '</tr>' ) ).

    SELECT email, send_cc, send_to
      FROM /yga/adam_emails
      INTO TABLE @DATA(lt_emails).

    LOOP AT gt_output INTO DATA(ls_ot_to_import) WHERE status_aprov EQ '@01@' AND
                                                       trp_p15      EQ '@02@' AND
                                                       highlight    IS INITIAL.


      lt_table_mail = VALUE #( BASE lt_table_mail
                                    ( line = '<tr>' )
                                    ( line = |<td class="row">{ ls_ot_to_import-charm_change }</td>| )
                                    ( line = |<td class="row">{ ls_ot_to_import-trkorr }</td>| )
                                    ( line = |<td class="row">{ ls_ot_to_import-descricao_cd }</td>| )
                                    ( line = |<td class="row">{ ls_ot_to_import-name_text_ernam }</td>| )
                                    ( line = |<td class="row">{ abap_false }</td>| )
                                    ( line = |<td class="row_last">{ ls_ot_to_import-obs_email }</td>| )
                                    ( line = '</tr>' ) ).
    ENDLOOP.
    IF sy-subrc = 0.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          client                  = sy-mandt
          id                      = 'ST'
          language                = 'P'
          name                    = '/YGA/ADAMASTOR_EMAIL'
          object                  = 'TEXT'
        IMPORTING
          header                  = ls_header
        TABLES
          lines                   = lt_lines
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.

      lt_table_mail = VALUE #( BASE lt_table_mail ( line = '</table>' ) ).

      IF lt_table_mail IS NOT INITIAL.
        LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<fs_lines>).
          CLEAR: lv_body_aux, lv_format.
          IF sy-tabix EQ 1.
            lv_body = <fs_lines>-tdline.
          ELSE.
            IF <fs_lines>-tdline CS '&TABLE&'.
              LOOP AT lt_table_mail INTO DATA(ls_table_mail).
                lv_body_aux = |{ lv_body_aux }{ ls_table_mail-line }|.
              ENDLOOP.
              lv_body = |{ lv_body }{ lv_format }{ lv_body_aux }|.
            ELSE.
              IF <fs_lines>-tdformat EQ '*'.
                lv_format = '<br>'.

                lv_body = |{ lv_body }{ lv_format }{ <fs_lines>-tdline }|.
              ELSEIF <fs_lines>-tdformat EQ abap_false.
                CONCATENATE lv_body <fs_lines>-tdline INTO lv_body SEPARATED BY space.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.

      lv_subject = 'CDs JUMP GA para importação em P15'.


      LOOP AT lt_emails INTO DATA(ls_emails).

        IF ls_emails-send_cc IS NOT INITIAL.
          IF lv_cc_mail IS INITIAL.
            lv_cc_mail = ls_emails-email.
          ELSE.
            lv_cc_mail =  |{ lv_cc_mail }; { ls_emails-email }|.
          ENDIF.
        ENDIF.
        IF ls_emails-send_to IS NOT INITIAL.
          IF lv_to_email IS INITIAL.
            lv_to_email = ls_emails-email.
          ELSE.
            lv_to_email = |{ lv_to_email }; { ls_emails-email }|.
          ENDIF.
        ENDIF.

      ENDLOOP.

      me->open_outlook_email(
     EXPORTING
       iv_body    = lv_body
       iv_to_mail = lv_to_email
       iv_cc_mail = lv_cc_mail
       iv_subject = lv_subject ).

    ELSE.
      MESSAGE i000(/yga/jump) WITH 'Não existem CDs validados para aprovação.'(010) .

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          client                  = sy-mandt
          id                      = 'ST'
          language                = 'P'
          name                    = '/YGA/ADAMASTOR_EMAIL_SEM_OTS'
          object                  = 'TEXT'
        IMPORTING
          header                  = ls_header
        TABLES
          lines                   = lt_lines
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.

      LOOP AT lt_lines ASSIGNING <fs_lines>.
        CLEAR: lv_body_aux, lv_format.
        IF sy-tabix EQ 1.
          lv_body = <fs_lines>-tdline.
        ELSE.
          IF <fs_lines>-tdformat EQ '*'.
            lv_format = '<br>'.

            lv_body = |{ lv_body }{ lv_format }{ <fs_lines>-tdline }|.
          ELSEIF <fs_lines>-tdformat EQ abap_false.
            CONCATENATE lv_body <fs_lines>-tdline INTO lv_body SEPARATED BY space.
          ENDIF.
        ENDIF.
      ENDLOOP.

      lv_subject = 'CDs JUMP GA para importação em P15'.

      LOOP AT lt_emails INTO ls_emails.

        IF ls_emails-send_cc IS NOT INITIAL.
          IF lv_cc_mail IS INITIAL.
            lv_cc_mail = ls_emails-email.
          ELSE.
            lv_cc_mail =  |{ lv_cc_mail }; { ls_emails-email }|.
          ENDIF.
        ENDIF.
        IF ls_emails-send_to IS NOT INITIAL.
          IF lv_to_email IS INITIAL.
            lv_to_email = ls_emails-email.
          ELSE.
            lv_to_email = |{ lv_to_email }; { ls_emails-email }|.
          ENDIF.
        ENDIF.

      ENDLOOP.

      me->open_outlook_email(
     EXPORTING
       iv_body    = lv_body
       iv_to_mail = lv_to_email
       iv_cc_mail = lv_cc_mail
       iv_subject = lv_subject ).

    ENDIF.

  ENDMETHOD.


  METHOD open_outlook_email.

    DATA: fo_appoutlook TYPE ole2_object,
          fo_appitem    TYPE ole2_object,
          fo_namespace  TYPE ole2_object,
          fo_inspector  TYPE ole2_object.

    "Create outlook.application
    CREATE OBJECT fo_appoutlook 'outlook.application' ##NO_TEXT.

    CALL METHOD OF fo_appoutlook 'GetNameSpace' = fo_namespace EXPORTING #1 = 'MAPI'.
    "Create item
    CALL METHOD OF fo_appoutlook 'CreateItem' = fo_appitem EXPORTING #1 = '0'.

    CALL METHOD OF fo_appitem 'GetInspector' = fo_inspector.

    "Set body of an email
    SET PROPERTY OF fo_appitem 'Htmlbody' = iv_body.

    "Set recipients
    SET PROPERTY OF fo_appitem 'To' = iv_to_mail.

    "Set recipients
    SET PROPERTY OF fo_appitem 'CC' = iv_cc_mail.

    "Set subject
    SET PROPERTY OF fo_appitem 'Subject' = iv_subject.

    CALL METHOD OF fo_appitem 'Display'.

    FREE OBJECT: fo_appitem, fo_namespace, fo_appoutlook.

  ENDMETHOD.


  METHOD progress .

    DATA:
      percentage TYPE i .

    " Não sera exibido quando for em background
    IF ( sy-batch EQ abap_true ) .
      RETURN .
    ENDIF .

    IF ( percent IS INITIAL ) AND
       ( ( total IS INITIAL ) AND currency IS INITIAL ) .
      RETURN .
    ENDIF .

    IF ( percent IS NOT INITIAL ) .
      percentage = percent .
    ELSE .
      TRY .
          percentage = ( currency * 100 ) / total.
        CATCH cx_sy_zerodivide .
          percentage = 10 .
      ENDTRY .
    ENDIF .

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = percentage  " Size of Bar ( 0 <= PERCENTAGE <= 100 )
        text       = message.    " Text to be Displayed

  ENDMETHOD .


  METHOD user_has_authority .

    IF ( sy-uname NOT IN gr_uname ) .
      RETURN .
    ENDIF .

    "# Criar mensagem com link para configurar a tabela

    rv_result = abap_on .

  ENDMETHOD .


  METHOD all_tr_included .

    TYPES:
      BEGIN OF ty_cd_ordem,
        trkorr       TYPE /yga/transp_ctrl-trkorr,
        charm_change TYPE /yga/transp_ctrl-charm_change,
        status_aprov TYPE /yga/transp_ctrl-status_aprov,
      END OF ty_cd_ordem,
      tab_cd_ordem TYPE STANDARD TABLE OF ty_cd_ordem
                   WITH DEFAULT KEY .

    DATA:
      lt_nao_aprovados TYPE tab_cd_ordem .

    DATA(lt_aprovados) = VALUE me->ty_t_out(
      FOR l IN me->gt_output
    WHERE ( status_aprov EQ me->gc_status_ot-aprovado )
      ( l ) ) .

    IF ( lines( lt_aprovados ) EQ 0 ) .
      RETURN .
    ENDIF .

    " Buscando itens da lista para todos os CDs que tenham
    " pelo menos uma request marcada como 'Aprovado p/ P15"
    SELECT trkorr, charm_change, status_aprov
      FROM /yga/transp_ctrl
      INTO TABLE @lt_nao_aprovados
       FOR ALL ENTRIES IN @lt_aprovados
     WHERE charm_change EQ @lt_aprovados-charm_change .
*      AND status_aprov eq @me->gc_status_ot-aprovado .
    IF ( sy-subrc NE 0 ) .
      RETURN .
    ENDIF .

    " Lista de itens que não tem 'Aprovado p/ P15"
    lt_nao_aprovados = VALUE tab_cd_ordem(
      LET lt_temp = lt_nao_aprovados
      IN
      FOR t IN lt_temp
      WHERE ( status_aprov EQ me->gc_status_ot-nao_aprov )
      ( t ) ) .

    IF ( lines( lt_nao_aprovados ) EQ 0 ) .
      rv_result = abap_on .
      RETURN .
    ENDIF .

    "# Pedir testes da Miriam para esse cenario

    " Criando lista de CDs com TR não aprovadas
    DATA(lt_message) = VALUE bapiret2_t(
      FOR m IN lt_nao_aprovados
      ( type       = if_xo_const_message=>error
        id         = '/YGA/JUMP3'
        number     = '227'
        message_v1 = m-trkorr
        message_v2 = m-charm_change
        row        = sy-tabix )
    ).

    "# detalhar melhor essa mensagem com texto descritivo
    "# pedir teste da Miriam

    IF ( 0 EQ 1 ). MESSAGE i227(/yga/jump3). ENDIF .

    IF ( lines( lt_message ) EQ 0 ) .
      RETURN .
    ENDIF .

    CALL FUNCTION 'UDM_MESSAGE_SHOW'
      EXPORTING
        et_message = lt_message.


*    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
*      TABLES
*        i_message_tab = lt_messagem.




*          DATA(lt_aprov) = gt_output[].
*
*          DELETE lt_aprov[] WHERE status_aprov <> icon_checked.
*
*          IF lt_aprov IS NOT INITIAL.
*
*            SELECT trkorr, charm_change, status_aprov
*            FROM /yga/transp_ctrl
*            INTO TABLE @DATA(lt_otp15)
*            FOR ALL ENTRIES IN @lt_aprov[]
*            WHERE charm_change EQ @lt_aprov-charm_change.
*
*            LOOP AT lt_otp15 ASSIGNING FIELD-SYMBOL(<fs_otp15>).
*
*              IF <fs_otp15>-status_aprov <> icon_checked.
*                "Erro
*                APPEND INITIAL LINE TO lt_messagem ASSIGNING FIELD-SYMBOL(<fs_messagem>).
*                <fs_messagem>-msgno = '227'.                 "c_msgno.
*                <fs_messagem>-msgty = 'E'.                   "c_msgty.
*                <fs_messagem>-msgid = '/YGA/JUMP3'.          "c_msgid.
*                <fs_messagem>-msgv1 = <fs_otp15>-trkorr.
*                <fs_messagem>-msgv2 = <fs_otp15>-charm_change.
*
*                MESSAGE i227(/yga/jump3) INTO DATA(lv_dummy).
*
*              ENDIF.
*
*            ENDLOOP.
*
*            IF lt_messagem[] IS NOT INITIAL.
*              CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
*                TABLES
*                  i_message_tab = lt_messagem.
*
*              RETURN.
*
*            ENDIF.
*
*          ENDIF.
  ENDMETHOD .


  METHOD filter_production .

    IF ( im_output IS INITIAL ) .
      RETURN .
    ENDIF .

    rv_result = COND #(
      WHEN line_exists( me->gt_imp_p15[ table_line = im_output-trp_p15 ] )
      THEN abap_on
      ELSE abap_off ).

  ENDMETHOD .


  METHOD refresh.

    FREE: gt_output.

    data_selection( ).

    o_salv->refresh( ).

*    data_display( ).


  ENDMETHOD.


  METHOD save_data.

*    LOOP AT gt_output ASSIGNING FIELD-SYMBOL(<fs_output>).
*
*      UPDATE /yga/transp_ctrl
*        SET "obs          = <fs_output>-obs
*            status_aprov = <fs_output>-status_aprov
*            highlight    = <fs_output>-highlight
*            "charm_rfc    = <fs_output>-charm_rfc
*            "charm_change = <fs_output>-charm_change
*        WHERE trkorr = <fs_output>-trkorr.
*
*    ENDLOOP.

  ENDMETHOD.


  METHOD set_columns.

    DATA: lo_columns TYPE REF TO cl_salv_columns_table,
          lo_column  TYPE REF TO cl_salv_column_table.

    lo_columns = o_salv->get_columns( ).
    lo_columns->set_optimize( 'A' ).
    lo_columns->set_key_fixation( abap_true ).

*   Seq nr
    TRY .
        lo_column ?= lo_columns->get_column( 'SEQ_NR' ).
        lo_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.
    ENDTRY.

*   Highlight
    TRY .
        lo_column ?= lo_columns->get_column( 'HIGHLIGHT' ).
        lo_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.
    ENDTRY.

*   Ordem
    TRY .
        lo_column ?= lo_columns->get_column( 'TRKORR' ).
        lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
        lo_column->set_key( abap_true ).
      CATCH cx_salv_not_found.
    ENDTRY.

*   CHARM - CHANGE
    TRY .
        lo_column ?= lo_columns->get_column( 'CHARM_CHANGE' ).
        lo_column->set_long_text( TEXT-002 ).
        lo_column->set_medium_text( space ).
        lo_column->set_short_text( space ).
        lo_column->set_key( abap_true ).
      CATCH cx_salv_not_found.
    ENDTRY.

*   CHARM - Status CD
    TRY .
        lo_column ?= lo_columns->get_column( 'STATUS_CD' ).
        lo_column->set_long_text( TEXT-008 ).
        lo_column->set_medium_text( TEXT-008 ).
        lo_column->set_short_text( TEXT-008 ).
        lo_column->set_key( abap_true ).
        lo_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.
    ENDTRY.

*   Descrição CD
    TRY .
        lo_column ?= lo_columns->get_column( 'DESCRICAO_CD' ).
        lo_column->set_long_text( TEXT-011 ).
        lo_column->set_medium_text( TEXT-011 ).
        lo_column->set_short_text( space ).
        lo_column->set_key( abap_true ).
      CATCH cx_salv_not_found.
    ENDTRY.

*   ERNAM
    TRY .
        lo_column ?= lo_columns->get_column( 'ERNAM' ).
        lo_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.
    ENDTRY.

*   NAME_TEXT_ERNAM
    TRY .
        lo_column ?= lo_columns->get_column( 'NAME_TEXT_ERNAM' ).
        lo_column->set_long_text( TEXT-003 ).
        lo_column->set_medium_text( ' ' ).
        lo_column->set_short_text( ' ' ).
        lo_column->set_output_length( value = '50' ).
        lo_column->set_key( abap_true ).
      CATCH cx_salv_not_found.
    ENDTRY.

*   Status Aprovação
    TRY .
        lo_column ?= lo_columns->get_column( 'STATUS_APROV' ).
        lo_column->set_key( abap_true ).
        lo_column->set_alignment( value = 3 ).
      CATCH cx_salv_not_found.
    ENDTRY.

*   Obs tecnicas
    TRY .
        lo_column ?= lo_columns->get_column( 'OBS_TECNICAS' ).
        lo_column->set_long_text( TEXT-005 ).
        lo_column->set_medium_text( ' ' ).
        lo_column->set_short_text( ' ' ).
        lo_column->set_output_length( value = '100' ).
      CATCH cx_salv_not_found.
    ENDTRY.

*   TRFUNCTION
    TRY .
        lo_column ?= lo_columns->get_column( 'TRFUNCTION' ).
        lo_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.
    ENDTRY.

*   AS4USER
    TRY .
        lo_column ?= lo_columns->get_column( 'AS4USER' ).
        lo_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.
    ENDTRY.

*   NAME_TEXT_AS4USER
    TRY .
        lo_column ?= lo_columns->get_column( 'NAME_TEXT_AS4USER' ).
        lo_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.
    ENDTRY.

*   AS4DATE
    TRY .
        lo_column ?= lo_columns->get_column( 'AS4DATE' ).
        lo_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.
    ENDTRY.

*   AS4TIME
    TRY .
        lo_column ?= lo_columns->get_column( 'AS4TIME' ).
        lo_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.
    ENDTRY.

*   ERDAT
    TRY .
        lo_column ?= lo_columns->get_column( 'ERDAT' ).
        lo_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.
    ENDTRY.

*   ERZET
    TRY .
        lo_column ?= lo_columns->get_column( 'ERZET' ).
        lo_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.
    ENDTRY.

    TRY .
        lo_column ?= lo_columns->get_column( 'OBS_EMAIL' ).
        lo_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.
    ENDTRY.

*   K15
    me->fill_system_fields( i_columns  = lo_columns i_systemid = 'K15' ).

*   P15
    me->fill_system_fields( i_columns  = lo_columns i_systemid = 'P15' ).

    " Q15
*   me->fill_system_fields( i_columns  = lo_columns i_systemid = 'Q15' ).
    TRY .
        lo_column ?= lo_columns->get_column( 'TRP_Q15' ).
        lo_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.
    ENDTRY.
    TRY .
        lo_column ?= lo_columns->get_column( 'STATUS_Q15' ).
        lo_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.
    ENDTRY.
    TRY .
        lo_column ?= lo_columns->get_column( 'DATA_IMP_Q15' ).
        lo_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.
    ENDTRY.
    TRY .
        lo_column ?= lo_columns->get_column( 'HORA_IMP_Q15' ).
        lo_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.
    ENDTRY.

    TRY.
        lo_column ?= lo_columns->get_column( 'LOG_TR' ).
        lo_column->set_alignment( if_salv_c_alignment=>centered ) .
        lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ) .
      CATCH cx_salv_not_found.
    ENDTRY.

    TRY.
        lo_columns->set_color_column( 'COLOR' ).
      CATCH cx_salv_data_error.
    ENDTRY.

  ENDMETHOD.


  METHOD set_focus_ini.
  ENDMETHOD.


  METHOD set_handler.

    DATA: lo_events_table TYPE REF TO cl_salv_events_table.

    lo_events_table = o_salv->get_event( ).

    SET HANDLER on_user_command FOR lo_events_table.
    SET HANDLER on_link_click   FOR lo_events_table.

  ENDMETHOD.


  METHOD set_impct_jump.

*    DATA(lt_rows) = o_salv->get_selections( )->get_selected_rows( ).
*
*    IF lt_rows[] IS INITIAL.
*      MESSAGE i000(/yga/jump) WITH 'Marcar Linha'(007) .
*      RETURN.
*    ENDIF.
*
*    LOOP AT lt_rows INTO DATA(lw_row).
*
*      READ TABLE gt_output ASSIGNING FIELD-SYMBOL(<fs_output>) INDEX lw_row.
*      IF sy-subrc EQ 0.
*        IF <fs_output>-impacto_jump = icon_checked.
*          <fs_output>-impacto_jump = icon_incomplete.
*        ELSEIF <fs_output>-impacto_jump = icon_incomplete OR <fs_output>-impacto_jump IS INITIAL.
*          <fs_output>-impacto_jump = icon_checked.
*        ENDIF.
*
*        UPDATE /yga/transp_ctrl
*           SET impacto_jump = <fs_output>-impacto_jump
*         WHERE trkorr = <fs_output>-trkorr.
*
*      ENDIF.
*
*    ENDLOOP.

  ENDMETHOD.


  METHOD set_layout.

    DATA:
      lo_display TYPE REF TO cl_salv_display_settings,
      lo_layout  TYPE REF TO cl_salv_layout,
      ls_key     TYPE salv_s_layout_key.

    lo_display = o_salv->get_display_settings( ).                           "Titulo
    lo_display->set_striped_pattern( cl_salv_display_settings=>true ).      "Zebra

    ls_key-report = sy-repid.

    lo_layout = o_salv->get_layout( ).
    IF ( lo_layout IS NOT BOUND ) .
      RETURN .
    ENDIF .

    IF ( me->gv_variant IS NOT INITIAL ) .
      lo_layout->set_initial_layout( me->gv_variant ). " Variante de Exibição
*   lo_layout->set_initial_layout( v_vari ). " Variante de Exibição
    ENDIF .

    lo_layout->set_key( ls_key ).
    lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).     "Gravar variantes de layout

  ENDMETHOD.


  METHOD set_light.

    DATA(lt_rows) = o_salv->get_selections( )->get_selected_rows( ).

    IF lt_rows[] IS INITIAL.
      MESSAGE i000(/yga/jump) WITH 'Marcar Linha'(007) .
      RETURN.
    ENDIF.

    control_color( IMPORTING et_color = DATA(lt_color) ).

    LOOP AT lt_rows INTO DATA(lw_row).

      READ TABLE gt_output ASSIGNING FIELD-SYMBOL(<fs_output>) INDEX lw_row.
      IF sy-subrc EQ 0.

        IF <fs_output>-color IS INITIAL.
          <fs_output>-color = lt_color.
          <fs_output>-highlight = abap_true.
        ELSE.
          IF <fs_output>-highlight = abap_true.
            CLEAR: <fs_output>-color , <fs_output>-highlight.
          ENDIF.
        ENDIF.

        UPDATE /yga/transp_ctrl
           SET highlight = <fs_output>-highlight
         WHERE trkorr = <fs_output>-trkorr.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD set_selection.

    DATA: lo_selections TYPE REF TO cl_salv_selections.

    lo_selections = o_salv->get_selections( ).
    lo_selections->set_selection_mode( if_salv_c_selection_mode=>cell ).

  ENDMETHOD.


  METHOD set_status_aprov.

    DATA: lv_answer(1) TYPE c,
          lv_valid     TYPE flag.

    CLEAR: lv_answer, lv_valid.

    DATA(lt_rows) = o_salv->get_selections( )->get_selected_rows( ).

    IF lt_rows[] IS INITIAL.
      MESSAGE i000(/yga/jump) WITH 'Marcar Linha'(007) .
      RETURN.
    ENDIF.

    LOOP AT lt_rows INTO DATA(lw_row).

      READ TABLE gt_output ASSIGNING FIELD-SYMBOL(<fs_output>) INDEX lw_row.
      IF sy-subrc EQ 0.
        IF <fs_output>-status_aprov = icon_checked.
          <fs_output>-status_aprov = icon_incomplete.
        ELSEIF <fs_output>-status_aprov = icon_incomplete OR <fs_output>-status_aprov IS INITIAL.

          IF lv_valid IS INITIAL.
            "POP UP de validar
            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                titlebar       = TEXT-t01
                text_question  = TEXT-t02
                icon_button_1  = ' '
                icon_button_2  = ' '
                default_button = '1'
              IMPORTING
                answer         = lv_answer.
            IF lv_answer NE '1'.
              RETURN.
            ELSE.
              CLEAR lv_answer.
            ENDIF.
            lv_valid = abap_true.
          ENDIF.

          IF <fs_output>-obs_email IS INITIAL.
            IF lv_answer IS INITIAL.
              "POP UP validação SIM OU NÃO de observações de email
              CALL FUNCTION 'POPUP_TO_CONFIRM'
                EXPORTING
                  titlebar       = TEXT-t01
                  text_question  = TEXT-t03
                  icon_button_1  = ' '
                  icon_button_2  = ' '
                  default_button = '1'
                IMPORTING
                  answer         = lv_answer.
              IF lv_answer EQ '2'.
                <fs_output>-status_aprov = icon_checked.
              ELSE.
                MESSAGE e012(/yga/jump3).
                RETURN.
              ENDIF.
            ELSE.
              <fs_output>-status_aprov = icon_checked.
            ENDIF.
          ELSE.
            <fs_output>-status_aprov = icon_checked.
          ENDIF.

        ENDIF.

        UPDATE /yga/transp_ctrl
           SET status_aprov = <fs_output>-status_aprov
         WHERE trkorr = <fs_output>-trkorr.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD set_status_aprov_p26.

*    DATA(lt_rows) = o_salv->get_selections( )->get_selected_rows( ).
*
*    IF lt_rows[] IS INITIAL.
*      MESSAGE i000(/yga/jump) WITH 'Marcar Linha'(007) .
*      RETURN.
*    ENDIF.
*
*    LOOP AT lt_rows INTO DATA(lw_row).
*
*      READ TABLE gt_output ASSIGNING FIELD-SYMBOL(<fs_output>) INDEX lw_row.
*      IF sy-subrc EQ 0.
*        IF <fs_output>-status_aprov_p26 = icon_checked.
*          <fs_output>-status_aprov_p26 = icon_incomplete.
*        ELSEIF <fs_output>-status_aprov_p26 = icon_incomplete OR <fs_output>-status_aprov_p26 IS INITIAL.
*          <fs_output>-status_aprov_p26 = icon_checked.
*        ENDIF.
*
*        UPDATE /yga/transp_ctrl
*           SET status_aprov_p26 = <fs_output>-status_aprov_p26
*         WHERE trkorr = <fs_output>-trkorr.
*
*      ENDIF.
*    ENDLOOP.

  ENDMETHOD.


  METHOD set_title .

    TYPES:
      r_change TYPE STANDARD TABLE OF /yga/transp_ctrl_st-charm_change
               WITH DEFAULT KEY .

    IF ( me->o_salv IS NOT BOUND ) .
      RETURN .
    ENDIF .

    DATA(display) = me->o_salv->get_display_settings( ) .
    IF ( display IS NOT BOUND ) .
      RETURN .
    ENDIF .

    DATA(list_cd) = VALUE r_change(
      FOR GROUPS cd OF l IN me->gt_output
      GROUP BY l-charm_change ASCENDING
      ( cd )
    ).

    DATA(title) = CONV lvc_title( 'Adamastor'(tt1) ) .
    title = COND #(
      WHEN lines( list_cd ) GT 1
      THEN |{ title } ({ lines( list_cd ) } { 'CDs listados'(tt2) })|
      WHEN lines( list_cd ) EQ 1
      THEN |{ title } { '(1 CD listado)'(tt3) }|
      ELSE |{ title } { space }| ) .

    display->set_list_header( title ).
    display->set_striped_pattern( cl_salv_display_settings=>true ) .

  ENDMETHOD .


  METHOD set_toolbar.

*   Status Toolbar criada no relatório
    o_salv->set_screen_status(
      pfstatus      =  'STAT_GUI100'
      report        =  gv_repid
      set_functions =  o_salv->c_functions_all ).

  ENDMETHOD.


  METHOD update_chg.

    DATA: lt_request         TYPE TABLE OF trkorr,
          lt_message         TYPE esp1_message_tab_type,
          lt_transp_ctrl_ins TYPE TABLE OF /yga/transp_ctrl,
          lv_bool            TYPE abap_bool.

*   Para todos os Changes da ALV - verificar se está atualizado
    LOOP AT gt_output ASSIGNING FIELD-SYMBOL(<fs_output>).

      DATA(lv_message) = CONV char50( |{ 'Verificar atualiz.'(m03) } { sy-tabix }| ) .
      lv_message = |{ lv_message } { 'de'(m07) } { lines( me->gt_transp_ctrl ) }{ '...'(m08) }| .

      me->progress(
        EXPORTING
          total    = lines( gt_transp_ctrl )
          currency = sy-tabix
          message  = lv_message
      ).

      FREE: lt_request[].
      CALL FUNCTION 'ZCHARM_OT_FROM_CD' DESTINATION 'SM_SM1CLNT900_TRUSTED'
        EXPORTING
          i_change  = <fs_output>-charm_change
        TABLES
          i_request = lt_request[].

      IF lt_request[] IS NOT INITIAL.

        LOOP AT lt_request[] ASSIGNING FIELD-SYMBOL(<fs_request>).

          APPEND INITIAL LINE TO gt_ot_cd ASSIGNING FIELD-SYMBOL(<fs_ot_cd>).
          <fs_ot_cd>-trkorr = <fs_request>.
          <fs_ot_cd>-cd     = <fs_output>-charm_change.
          UNASSIGN <fs_ot_cd>.

        ENDLOOP.

      ELSE.

*   Adicionar mensagem de erro
        APPEND INITIAL LINE TO lt_message ASSIGNING FIELD-SYMBOL(<fs_message>).
        <fs_message>-msgid  = '/YGA/JUMP1'.
        <fs_message>-msgty  = 'E'.
        <fs_message>-msgno  = '109'.
        <fs_message>-msgv1  = <fs_output>-charm_change.
        UNASSIGN <fs_message>.

      ENDIF.

    ENDLOOP.

    SORT gt_ot_cd[] BY trkorr cd.
    DELETE ADJACENT DUPLICATES FROM gt_ot_cd COMPARING trkorr cd.

    IF gt_ot_cd[] IS NOT INITIAL.

*   Eliminar OTs transporte de cópias
      SELECT *
        FROM e070
        INTO TABLE @DATA(lt_e070)
        FOR ALL ENTRIES IN @gt_ot_cd
       WHERE trkorr = @gt_ot_cd-trkorr
         AND trfunction = 'T'.
      IF sy-subrc = 0.

        DATA(gt_ot_cd_aux) = gt_ot_cd[].
        LOOP AT gt_ot_cd_aux ASSIGNING FIELD-SYMBOL(<fs_e070>).
          IF line_exists( lt_e070[ trkorr = <fs_e070>-trkorr ] ).
            DELETE gt_ot_cd WHERE trkorr = <fs_e070>-trkorr.
          ENDIF.
        ENDLOOP.

      ENDIF.

      IF gt_ot_cd IS NOT INITIAL.

        check_trfunction( CHANGING ct_ot_cd  = gt_ot_cd ).

*   Obter dados da tabela /yga/transp_ctrl
        IF gt_ot_cd[] IS NOT INITIAL.
          SELECT trkorr, charm_change
            FROM /yga/transp_ctrl
            INTO TABLE @DATA(lt_transp_ctrl)
            FOR ALL ENTRIES IN @gt_ot_cd
           WHERE ( trkorr = @gt_ot_cd-trkorr OR charm_change = @gt_ot_cd-cd ).
        ENDIF.

*   Obter nr sequencia actual.
        SELECT MAX( seq_nr )
          FROM /yga/transp_ctrl
          INTO @DATA(lv_seq_nr).
        IF sy-subrc IS NOT INITIAL.
          lv_seq_nr = 0.
        ENDIF.

        LOOP AT gt_ot_cd[] ASSIGNING FIELD-SYMBOL(<fs_ot>).

          READ TABLE lt_transp_ctrl ASSIGNING FIELD-SYMBOL(<fs_transp_ctrl>) WITH KEY trkorr = <fs_ot>-trkorr.
          IF sy-subrc = 0.

*   OT já adicionada no adamastor; verificar se CD é diferente
            IF <fs_ot>-cd <> <fs_transp_ctrl>-charm_change.
              UPDATE /yga/transp_ctrl SET charm_change = <fs_ot>-cd WHERE trkorr = <fs_transp_ctrl>-trkorr.
              IF sy-subrc = 0.
                APPEND INITIAL LINE TO lt_message ASSIGNING <fs_message>.
                <fs_message>-msgid  = '/YGA/JUMP1'.
                <fs_message>-msgty  = 'S'.
                <fs_message>-msgno  = '113'.
                <fs_message>-msgv1  = <fs_ot>-trkorr.
                <fs_message>-msgv2  = <fs_ot>-cd.
                COMMIT WORK AND WAIT.
                lv_bool = abap_true.
              ENDIF.
            ENDIF.

          ELSE.

*   OT ainda não adicionada no adamastor
            ADD 1 TO lv_seq_nr.

            APPEND INITIAL LINE TO lt_transp_ctrl_ins ASSIGNING FIELD-SYMBOL(<fs_new>).
            <fs_new>-mandt        = sy-mandt.
            <fs_new>-seq_nr       = lv_seq_nr.
            <fs_new>-trkorr       = <fs_ot>-trkorr.
            <fs_new>-charm_change = <fs_ot>-cd.
            <fs_new>-status_aprov = icon_incomplete.
            READ TABLE gt_e070aux ASSIGNING FIELD-SYMBOL(<fs_e070aux>) WITH KEY trkorr = <fs_ot>-trkorr.
            IF sy-subrc = 0.
              <fs_new>-ernam = <fs_e070aux>-as4user.
            ELSE.
              <fs_new>-ernam        = sy-uname.
            ENDIF.
            <fs_new>-erdat        = sy-datum.
            <fs_new>-erzet        = sy-uzeit.
            READ TABLE gt_output ASSIGNING FIELD-SYMBOL(<fs_output_aux>) WITH KEY charm_change = <fs_ot>-cd.
            IF sy-subrc = 0.
              <fs_new>-descricao_cd = <fs_output_aux>-descricao_cd.
            ENDIF.
          ENDIF.

        ENDLOOP.

        LOOP AT lt_transp_ctrl_ins ASSIGNING FIELD-SYMBOL(<fs_ins>).
          INSERT /yga/transp_ctrl FROM <fs_ins>.
          IF sy-subrc = 0.
            APPEND INITIAL LINE TO lt_message ASSIGNING <fs_message>.
            <fs_message>-msgid  = '/YGA/JUMP1'.
            <fs_message>-msgty  = 'S'.
            <fs_message>-msgno  = '114'.
            <fs_message>-msgv1  = <fs_ins>-trkorr.
            <fs_message>-msgv2  = <fs_ins>-charm_change.
            COMMIT WORK AND WAIT.
            lv_bool = abap_true.
          ENDIF.
        ENDLOOP.

        LOOP AT lt_transp_ctrl[] ASSIGNING FIELD-SYMBOL(<fs_update>).

*   Validar de OT já não pertence ao CD
          IF NOT line_exists( gt_ot_cd[ trkorr = <fs_update>-trkorr cd = <fs_update>-charm_change ] ).
            DELETE FROM /yga/transp_ctrl WHERE trkorr = <fs_update>-trkorr.
            IF sy-subrc = 0.
              APPEND INITIAL LINE TO lt_message ASSIGNING <fs_message>.
              <fs_message>-msgid  = '/YGA/JUMP2'.
              <fs_message>-msgty  = 'E'.
              <fs_message>-msgno  = '911'.
              <fs_message>-msgv1  = <fs_update>-trkorr.
              <fs_message>-msgv2  = space.
              DELETE gt_output WHERE trkorr = <fs_update>-trkorr.
              COMMIT WORK AND WAIT.
              lv_bool = abap_true.
            ENDIF.
          ENDIF.

        ENDLOOP.

      ENDIF.
    ENDIF.

    LOOP AT gt_output ASSIGNING FIELD-SYMBOL(<fs_toutput>).
      IF <fs_toutput>-as4text IS INITIAL AND
         NOT line_exists( gt_ot_cd[ cd = <fs_toutput>-charm_change trkorr = <fs_toutput>-trkorr ] ).
        DELETE FROM /yga/transp_ctrl WHERE trkorr = <fs_toutput>-trkorr.
        IF sy-subrc = 0.
          APPEND INITIAL LINE TO lt_message ASSIGNING <fs_message>.
          <fs_message>-msgid  = '/YGA/JUMP2'.
          <fs_message>-msgty  = 'E'.
          <fs_message>-msgno  = '911'.
          <fs_message>-msgv1  = <fs_toutput>-trkorr.
          <fs_message>-msgv2  = space.
          DELETE gt_output WHERE trkorr = <fs_toutput>-trkorr.
          COMMIT WORK AND WAIT.
          lv_bool = abap_true.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lv_bool = abap_false.
      APPEND INITIAL LINE TO lt_message ASSIGNING <fs_message>.
      <fs_message>-msgid  = '/YGA/JUMP1'.
      <fs_message>-msgty  = 'S'.
      <fs_message>-msgno  = '115'.
    ENDIF.

    IF lt_message[] IS NOT INITIAL.
      IF NOT ( gv_chang IS NOT INITIAL AND lines( lt_message ) EQ 1 AND line_exists( lt_message[ msgty = 'S' ] ) ).
        CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
          TABLES
            i_message_tab = lt_message.
      ENDIF.
    ENDIF.

    IF gv_chang NE abap_true.
      me->refresh( ).
    ENDIF.

  ENDMETHOD.


  METHOD update_status_cd.

    DATA(lt_rows) = o_salv->get_selections( )->get_selected_rows( ).

    DATA: lt_transp_ctrl_upd TYPE STANDARD TABLE OF /yga/transp_ctrl,
          lt_sval            TYPE STANDARD TABLE OF sval,
          ls_output          LIKE LINE OF gt_output,
          lv_return          TYPE char1,
          lv_status          TYPE /yga/status_cd.

    FREE: lt_transp_ctrl_upd[], lt_sval[].
    CLEAR: ls_output, lv_return, lv_status.

    IF lt_rows[] IS INITIAL.
      MESSAGE i000(/yga/jump) WITH 'Marcar Linha'(007) .
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO lt_sval ASSIGNING FIELD-SYMBOL(<fs_sval>).
    <fs_sval>-tabname   = '/YGA/T_STATUS_CD'.
    <fs_sval>-fieldname = 'STATUS_CD'.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = 'Inserir dados'(005)
      IMPORTING
        returncode      = lv_return
      TABLES
        fields          = lt_sval
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.

    IF sy-subrc EQ 0 AND lv_return = space.

      IF lt_sval[ fieldname = 'STATUS_CD' ]-value <> space.
        lv_status = lt_sval[ fieldname = 'STATUS_CD' ]-value .
      ENDIF.

      LOOP AT lt_rows INTO DATA(lw_row).

        READ TABLE gt_output ASSIGNING FIELD-SYMBOL(<fs_output>) INDEX lw_row.
        IF sy-subrc EQ 0.

*   Atualizar CD com novo status

*   Alterar todas as entradas com o CD seleccionado - BD
          UPDATE /yga/transp_ctrl
             SET status_cd = lv_status
           WHERE charm_change = <fs_output>-charm_change.

*   Atualizar ALV (atualizar entradas que tenham esse CD mesmo que não estejam seleccionadas)
          ls_output-status_cd = lv_status.

          MODIFY gt_output
            FROM ls_output
    TRANSPORTING status_cd
           WHERE charm_change = <fs_output>-charm_change.

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.
ENDCLASS.