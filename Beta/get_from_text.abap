CLASS /yga/cl_transp_compare DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_out,
        item  TYPE char10.
        INCLUDE TYPE /yga/transp_ctrl_st.
      TYPES:
        color TYPE lvc_t_scol,
      END OF ty_out,
      tab_out TYPE STANDARD TABLE OF ty_out
               WITH DEFAULT KEY.

    "! <p class="shorttext synchronized" lang="pt">Importar a lista de referencia</p>
    METHODS constructor.
    "! <p class="shorttext synchronized" lang="pt">Exibe a lista de temas gerados</p>
    METHODS get_list
      RETURNING
        VALUE(result) TYPE /yga/cl_transp_compare=>tab_out .

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
      gt_out            TYPE tab_out.

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


  METHOD constructor .

    me->gt_list = VALUE tab_list(
      ( item = 'INC2927814' )
      ( item = 'AST-99999' )
      ( item = 'AST-74088' )
      ( item = 'AST-79334' )
      ( item = 'AST-81186' )
      ( item = 'AST-84323' )
    ).

  ENDMETHOD .

  METHOD get_list .

    IF ( lines( me->gt_list ) EQ 0 ) .
      RETURN .
    ENDIF .

    IF ( me->get_data( ) EQ abap_false ) .
      RETURN .
    ENDIF .

    me->prepare_data( ) .

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
                         int = 1 ) )
    ) .

    CLEAR me->gt_out  .

    IF ( lines( me->gt_change_request ) EQ 0 ) .
      RETURN .
    ENDIF .

    LOOP AT me->gt_list INTO DATA(ls_list) .

      DATA(lines_from_item) = me->get_lines_from_item( ls_list-item ) .

      IF ( lines( lines_from_item ) EQ 0 ) .
        " append com linhas vermelhas
        me->gt_out = VALUE #(
          BASE me->gt_out ( item  = ls_list-item
                            color = lt_amarelo )
        ) .
        CONTINUE .
      ENDIF .

      me->gt_out = VALUE #(
        BASE me->gt_out ( LINES OF lines_from_item )
      ) .

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
        ls_out-item = im_item .
        APPEND ls_out TO result .

      ENDIF.

    ENDLOOP .

  ENDMETHOD .

ENDCLASS.