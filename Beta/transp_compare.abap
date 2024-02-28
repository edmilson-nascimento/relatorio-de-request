CLASS /yga/cl_transp_compare DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_out,
        item         TYPE char10,
        charm_change TYPE /yga/transp_ctrl_st-charm_change,
        descricao_cd TYPE /yga/transp_ctrl_st-descricao_cd,
        trkorr       TYPE /yga/transp_ctrl_st-trkorr,
        status_aprov TYPE /yga/transp_ctrl_st-status_aprov,
        status       TYPE char30,
        color        TYPE lvc_t_scol,
      END OF ty_out,
      tab_out TYPE STANDARD TABLE OF ty_out
               WITH DEFAULT KEY.

    "! <p class="shorttext synchronized" lang="pt">Retorna lista de item para filtro de validação</p>
    CLASS-METHODS get_items
      RETURNING
        VALUE(result) TYPE char10_t .

    "! <p class="shorttext synchronized" lang="pt">Importar a lista de referencia</p>
    METHODS constructor
      IMPORTING
        im_list TYPE  char10_t .

    "! <p class="shorttext synchronized" lang="pt">Retorna a lista de temas gerados</p>
    METHODS get_list
      RETURNING
        VALUE(result) TYPE /yga/cl_transp_compare=>tab_out .

    "! <p class="shorttext synchronized" lang="pt">Exibe a lista de temas gerados</p>
    METHODS show_list .

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_list,
        item TYPE char10,
      END OF ty_list,
      tab_list            TYPE STANDARD TABLE OF ty_list
                          WITH DEFAULT KEY,
      tab_change_document TYPE STANDARD TABLE OF /yga/transp_ctrl,
      tab_descricao       TYPE RANGE OF /yga/transp_ctrl-descricao_cd.

    DATA:
      gt_list           TYPE tab_list,
      gt_change_request TYPE tab_change_document,
      gt_out            TYPE tab_out,
      go_salv_table     TYPE REF TO cl_salv_table.

    "! <p class="shorttext synchronized" lang="pt">Busca os dados de acordo com o filtro da lista</p>
    METHODS get_data
      RETURNING
        VALUE(result) TYPE sap_bool  .

    "! <p class="shorttext synchronized" lang="pt">Organiza a lista de dados encontrados</p>
    METHODS prepare_data .

    "! <p class="shorttext synchronized" lang="pt">Retorna os itens correspondentes ao itens</p>
    METHODS get_lines_from_item
      IMPORTING
        !im_item      TYPE ty_list-item
      RETURNING
        VALUE(result) TYPE tab_out  .

ENDCLASS.



CLASS /yga/cl_transp_compare IMPLEMENTATION.


  METHOD get_items .

    TYPES:
      tab_item TYPE RANGE OF char10 .
    DATA:
      lr_item TYPE tab_item .

    DATA(ls_excluded_options) =
      VALUE rsoptions(
        bt = abap_on
        cp = abap_on
        eq = abap_off
        ge = abap_on
        gt = abap_on
        le = abap_on
        lt = abap_on
        nb = abap_on
        ne = abap_on
        np = abap_on
    ).

    CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
      EXPORTING
        title             = 'Lista de itens (AST ou INC)'
*       text              =                  " Text for Options Dialog Box
*       signed            = 'X'              " X: Sign Allowed
*       lower_case        = space            " X: Upper/Lowercase
        no_interval_check = abap_on
*       just_display      = space            " X: Display Only
*       just_incl         = space            " X: Select Only
        excluded_options  = ls_excluded_options
*       description       =                  " Field Description If Different
*       help_field        =                  " Dictionary Field Name for F1/F4
*       search_help       =
*       tab_and_field     =                  " Table Name, Field Name
      TABLES
        range             = lr_item                " Content Table
      EXCEPTIONS
        no_range_tab      = 1                " Table Not a RANGES Table
        cancelled         = 2                " Action was canceled
        internal_error    = 3                " Internal Error
        invalid_fieldname = 4                " Incorrect Field Description
        OTHERS            = 5.
    IF ( sy-subrc NE 0 ) .
      RETURN .
    ENDIF.

    result = VALUE #(
      FOR l IN lr_item
      WHERE ( sign   = if_fsbp_const_range=>sign_include AND
              option = if_fsbp_const_range=>option_equal )
        ( l-low )
    ).

  ENDMETHOD .


  METHOD constructor .

    me->gt_list = VALUE tab_list(
      FOR l IN im_list
      ( item = l )
    ).

  ENDMETHOD .


  METHOD get_list .

    IF ( lines( me->gt_list ) EQ 0 ) .
      RETURN .
    ENDIF .

    me->get_data( ) .

    me->prepare_data( ) .

  ENDMETHOD .


  METHOD show_list .

    DATA:
      lo_column  TYPE REF TO cl_salv_column_table.

    TYPES:
      BEGIN OF ty_popup,
        start_column TYPE i,
        end_column   TYPE i,
        start_line   TYPE i,
        end_line     TYPE i,
      END OF ty_popup .

    me->get_list( ) .

    IF ( lines( me->gt_out ) EQ 0 ) .
      RETURN .
    ENDIF .

    DATA(ls_popup) = VALUE ty_popup(
      start_column = 2
      end_column   = 100
      start_line   = 2
      end_line     = COND #(
                       WHEN lines( me->gt_out ) GT 25
                       THEN 25
                       ELSE ( lines( me->gt_out ) + 3 ) )
    ).

    TRY.
        CALL METHOD cl_salv_table=>factory(
          IMPORTING
            r_salv_table = me->go_salv_table
          CHANGING
            t_table      = me->gt_out ) .
      CATCH cx_salv_msg .
    ENDTRY.

    DATA(lr_selections) = me->go_salv_table->get_selections( ).
    IF ( lr_selections IS BOUND ).
      lr_selections->set_selection_mode( if_salv_c_selection_mode=>cell ).
    ENDIF.

    DATA(lo_columns) = me->go_salv_table->get_columns( ) .
    IF ( lo_columns IS NOT BOUND ) .
      RETURN .
    ENDIF .

    lo_columns->set_optimize( ) .
    lo_columns->set_key_fixation( abap_on ) .

    TRY.
        lo_columns->set_color_column( 'COLOR' ).
      CATCH cx_salv_data_error.
    ENDTRY.

    TRY .
        lo_column ?= lo_columns->get_column( 'ITEM' ).
        IF ( lo_column IS BOUND ) .
          lo_column->set_long_text( |{ 'Item verificado'(i01) }| ) .
          lo_column->set_medium_text( |{ 'Item verificado'(i01) }| ) .
          lo_column->set_short_text( |{ 'Item verif.'(i01) }| ) .
          CLEAR lo_column .
        ENDIF .
      CATCH cx_salv_not_found.
    ENDTRY.

    TRY .
        lo_column ?= lo_columns->get_column( 'STATUS' ).
        IF ( lo_column IS BOUND ) .
          lo_column->set_long_text( |{ 'Status'(i02) }| ) .
          lo_column->set_medium_text( |{ 'Status'(i02) }| ) .
          lo_column->set_short_text( |{ 'Status'(i02) }| ) .
          CLEAR lo_column .
        ENDIF .
      CATCH cx_salv_not_found.
    ENDTRY.

    me->go_salv_table->set_screen_popup(
      EXPORTING
        start_column = ls_popup-start_column
        end_column   = ls_popup-end_column
        start_line   = ls_popup-start_line
        end_line     = ls_popup-end_line
    ).

    me->go_salv_table->display( ) .

  ENDMETHOD .


  METHOD get_data .

    CLEAR me->gt_change_request .

    DATA(lr_descricao_cd) = VALUE tab_descricao(
      FOR l IN me->gt_list
        ( sign   = if_fsbp_const_range=>sign_include
          option = if_fsbp_const_range=>option_contains_pattern
          low    = |*{ l-item }*| )
    ).

    SORT lr_descricao_cd ASCENDING .

    SELECT *
      FROM /yga/transp_ctrl
     WHERE descricao_cd IN @lr_descricao_cd
      INTO TABLE @me->gt_change_request .

    result = COND #(
      WHEN sy-subrc EQ 0
      THEN abap_on
      ELSE abap_off ) .

  ENDMETHOD.


  METHOD prepare_data .

    DATA(lt_amarelo) = VALUE lvc_t_scol(
      ( color = VALUE #( col = 3
                         int = 1 ) ) ) .

    CLEAR me->gt_out .


    IF ( lines( me->gt_change_request ) EQ 0 ) .
      LOOP AT me->gt_list INTO  DATA(ls_list) .
        me->gt_out = VALUE #(
          BASE me->gt_out ( item   = ls_list-item
                            status = |{ 'Não adicionado ao Adamastor'(s01) }|
                            color  = lt_amarelo ) ) .
      ENDLOOP .
      RETURN .
    ENDIF .

    LOOP AT me->gt_list INTO ls_list .

      DATA(lines_from_item) = me->get_lines_from_item( ls_list-item ) .

      IF ( lines( lines_from_item ) EQ 0 ) .
        " append com linhas de erro
        me->gt_out = VALUE #(
          BASE me->gt_out ( item   = ls_list-item
                            status = |{ 'Não adicionado ao Adamastor'(s01) }|
                            color  = lt_amarelo ) ) .
        CONTINUE .
      ENDIF .

      me->gt_out = VALUE #(
        BASE me->gt_out ( LINES OF lines_from_item ) ) .

    ENDLOOP .

  ENDMETHOD .


  METHOD get_lines_from_item .

    IF ( im_item IS INITIAL ) .
      RETURN .
    ENDIF .

    DATA(lv_filter) = CONV string( im_item ) .

    LOOP AT me->gt_change_request INTO DATA(ls_data) .

      DATA(lv_descr) = CONV string( ls_data-descricao_cd ) .

      " Verificar se algum item tem o "termo" esperado
      SEARCH ls_data-descricao_cd FOR im_item .
      SEARCH lv_descr FOR lv_filter .
      IF ( sy-subrc EQ 0 ) .

        DATA(ls_out) = CORRESPONDING ty_out( ls_data ) .
        ls_out-item   = im_item .
        ls_out-status = |{ 'Adicionado ao Adamastor'(s02) }| .
        APPEND ls_out TO result .

      ENDIF.

    ENDLOOP .

  ENDMETHOD .

ENDCLASS.