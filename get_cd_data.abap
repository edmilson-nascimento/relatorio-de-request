REPORT x .


TABLES /yga/transp_ctrl .

CLASS lcl_view DEFINITION CREATE PUBLIC.

  PUBLIC SECTION .

    CLASS-METHODS get_single_value
      IMPORTING
        !fieldname    TYPE sval-fieldname
        !tabname      TYPE sval-tabname
        !titel        TYPE cua_tit_tx DEFAULT 'Manutenção de CD'
      RETURNING
        VALUE(result) TYPE pvarfield .

  PROTECTED SECTION .

  PRIVATE SECTION .

ENDCLASS.



CLASS lcl_view IMPLEMENTATION.

  METHOD get_single_value .

    DATA:
      answer TYPE char1,
      value1 TYPE pvarfield.


    CALL FUNCTION 'POPUP_TO_GET_VALUE'
      EXPORTING
        fieldname           = fieldname
        tabname             = tabname
        titel               = titel
        valuein             = ''
      IMPORTING
        answer              = answer
        valueout            = value1
      EXCEPTIONS
        fieldname_not_found = 1
        OTHERS              = 2.

    result = COND #(
      WHEN sy-subrc EQ 0
      THEN value1
      ELSE abap_off ) .

  ENDMETHOD .

ENDCLASS.


CLASS lcl_get_cd DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor .

    METHODS has_valid_cd
      RETURNING
        VALUE(result) TYPE sap_bool .

    METHODS get_cd_description .

    METHODS has_valid_cd_description
      RETURNING
        VALUE(result) TYPE sap_bool .

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_data,
        cd           TYPE /yga/transp_ctrl-charm_change,
        descricao_cd TYPE /yga/transp_ctrl-descricao_cd,
      END OF ty_data .

    DATA:
      ls_data TYPE ty_data .


ENDCLASS.

CLASS lcl_get_cd IMPLEMENTATION.

  METHOD constructor .

    me->ls_data-cd =
       lcl_view=>get_single_value( fieldname = 'CHARM_CHANGE'
                                   tabname   = '/YGA/TRANSP_CTRL' ) .

  ENDMETHOD .


  METHOD has_valid_cd .

    result = COND #(
      WHEN me->ls_data-cd IS INITIAL
      THEN abap_off
      ELSE abap_on
    ) .

  ENDMETHOD .


  METHOD get_cd_description .

    DATA(lv_titel) = CONV cua_tit_tx( |Desc. CD { ls_data-cd }| ) .

    me->ls_data-descricao_cd =
      lcl_view=>get_single_value( fieldname = 'DESCRICAO_CD'
                                  tabname   = '/YGA/TRANSP_CTRL'
                                  titel     = lv_titel ) .

  ENDMETHOD .


  METHOD has_valid_cd_description .

    result = COND #(
      WHEN me->ls_data-descricao_cd IS INITIAL
      THEN abap_off
      ELSE abap_on
    ) .

  ENDMETHOD .

ENDCLASS.

INITIALIZATION .

  DATA(lo_cd_mantain) = NEW lcl_get_cd( ) .
  IF ( lo_cd_mantain IS NOT BOUND ) .
    RETURN .
  ENDIF .

  IF ( lo_cd_mantain->has_valid_cd( ) EQ abap_false ) .
    RETURN .
  ENDIF .

  lo_cd_mantain->get_cd_description( ) .

  IF ( lo_cd_mantain->has_valid_cd_description( ) EQ abap_false ) .
    RETURN .
  ENDIF .

  BREAK-POINT .




















*