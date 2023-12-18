CLASS zcl_ca_object_code_compare DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_output,
        object_name TYPE string,
        status_l_e  TYPE string, " Comparison between Internal/External version
*        status_v1_v2 TYPE string, " Comparison between V1 and V2
*        status_v1_v3 TYPE string, " Comparison between V1 and V3
*        status_v2_v3 TYPE string, " Comparison between V2 and V3
      END OF ty_output .
    TYPES:
      t_ty_output TYPE STANDARD TABLE OF ty_output .
    TYPES:
      BEGIN OF ty_output_ext,
        object_name TYPE string,
        status_l_e  TYPE string, " Comparison between Internal/External version
      END OF ty_output_ext .
    TYPES:
      t_ty_output_ext TYPE STANDARD TABLE OF ty_output_ext .
    TYPES:
      BEGIN OF ty_output_avc,
        object_name_v1(96) TYPE c,
        status_1_2     TYPE string,
        object_name_v2(96) TYPE c,
        status_2_3     TYPE string,
        object_name_v3(96) TYPE c,
        status_1_3     TYPE string,
      END OF ty_output_avc .
    TYPES: t_ty_output_avc TYPE STANDARD TABLE OF ty_output_avc.

    TYPES:
      BEGIN OF ty_avcontrol,
        class_name TYPE seoclsname,
      END OF ty_avcontrol .
    TYPES:
      t_ty_avcontrol TYPE STANDARD TABLE OF ty_avcontrol .
    TYPES:
      BEGIN OF ty_code,
        obj_name TYPE char128,
        origin   TYPE char1, " L - Local/ E - external
        code     TYPE abaptxt255_tab,
      END OF ty_code .
    TYPES:
      t_ty_code TYPE STANDARD TABLE OF ty_code .

    METHODS main
      IMPORTING
        !i_obj_type    TYPE trobjtype
        !i_obj_name    TYPE trobj_name
        !i_destination TYPE rfcdest OPTIONAL
        !i_option      TYPE char3 DEFAULT 'EXT'
        !i_container   TYPE REF TO cl_gui_custom_container
        !i_new_table   TYPE flag DEFAULT ' ' .
    METHODS remote_compare .
    METHODS f4_obj_type
      IMPORTING
        !iv_field TYPE dynfnam .
protected section.
PRIVATE SECTION.

  TYPES:
    BEGIN OF ty_add_settings,
      alv          TYPE REF TO cl_gui_alv_grid,
      layout       TYPE lvc_s_layo,
      fieldcatalog TYPE lvc_t_fcat,
      exclude      TYPE ui_functions,
    END OF ty_add_settings .

  DATA gs_add_settings TYPE ty_add_settings .
  DATA gt_output_ext TYPE t_ty_output_ext .
  DATA gt_output_avc TYPE t_ty_output_avc .
  CONSTANTS:
    BEGIN OF c_origin,
      local    TYPE char1 VALUE 'L',
      external TYPE char1 VALUE 'E',
    END OF c_origin .
  CONSTANTS:
    BEGIN OF c_tables,
      tmdir TYPE tabname VALUE 'TMDIR',
    END OF c_tables .
  CONSTANTS:
    BEGIN OF c_obj_type,
      BEGIN OF class,
        public_header    TYPE versobjtyp VALUE 'CPUB',
        protected_header TYPE versobjtyp VALUE 'CPRO',
        private_header   TYPE versobjtyp VALUE 'CPRI',
        method           TYPE versobjtyp VALUE 'METH',
      END OF class,
    END OF c_obj_type .

  METHODS get_object_code
    IMPORTING
      !i_obj_name    TYPE versobjnam
      !i_obj_type    TYPE versobjtyp
      !i_destination TYPE rfcdest OPTIONAL
      !i_origin      TYPE char1 DEFAULT 'L'
      !i_versno      TYPE versno DEFAULT '00000'
    CHANGING
      !t_code        TYPE t_ty_code
    EXCEPTIONS
      no_version .
  METHODS get_class_components
    IMPORTING
      !i_class_name       TYPE seoclsname
      !i_destination      TYPE rfcdest OPTIONAL
    EXPORTING
      VALUE(t_components) TYPE ish_nl_t_tmdir
    EXCEPTIONS
      no_data_found
      error_accessing_table
      definition_not_found .
  METHODS get_class_code
    IMPORTING
      !i_class_name       TYPE seoclsname
      !i_destination      TYPE rfcdest OPTIONAL
      !i_origin           TYPE char1 DEFAULT 'L'
    CHANGING
      VALUE(t_class_code) TYPE t_ty_code
    EXCEPTIONS
      error_components
      error_code .
  METHODS get_class_headers
    IMPORTING
      !i_class_name   TYPE seoclsname
      !i_destination  TYPE rfcdest
      !i_origin       TYPE char1
    CHANGING
      !t_headers_code TYPE t_ty_code
    EXCEPTIONS
      no_code_found .
  METHODS get_public_header
    IMPORTING
      !i_class_name  TYPE seoclsname
      !i_destination TYPE rfcdest
      !i_origin      TYPE char1
    CHANGING
      !t_cpub_code   TYPE t_ty_code
    EXCEPTIONS
      no_code_found .
  METHODS get_protected_header
    IMPORTING
      !i_class_name  TYPE seoclsname
      !i_destination TYPE rfcdest
      !i_origin      TYPE char1
    CHANGING
      !t_cpro_code   TYPE t_ty_code
    EXCEPTIONS
      no_code_found .
  METHODS get_private_header
    IMPORTING
      !i_class_name  TYPE seoclsname
      !i_destination TYPE rfcdest
      !i_origin      TYPE char1
    CHANGING
      !t_cpri_code   TYPE t_ty_code
    EXCEPTIONS
      no_code_found .
  METHODS get_class_methods
    IMPORTING
      !t_components   TYPE ish_nl_t_tmdir
      !i_destination  TYPE rfcdest
      !i_origin       TYPE char1
    CHANGING
      !t_methods_code TYPE t_ty_code
    EXCEPTIONS
      no_code_found .
  METHODS get_method
    IMPORTING
      !i_method_name TYPE versobjnam
      !i_destination TYPE rfcdest
      !i_origin      TYPE char1
    CHANGING
      !t_meth_code   TYPE t_ty_code
    EXCEPTIONS
      no_code_found .
  METHODS get_avcontrol_classes
    IMPORTING
      !i_class_name  TYPE seoclsname
      !i_new_table   TYPE flag DEFAULT abap_false
    CHANGING
      !t_avc_classes TYPE t_ty_avcontrol
    EXCEPTIONS
      not_avc_class .
  METHODS compare_ext
    IMPORTING
      !i_obj_type    TYPE trobjtype
      !i_obj_name    TYPE trobj_name
      !i_destination TYPE rfcdest
    EXPORTING
      !t_output_ext  TYPE t_ty_output_ext .
  METHODS compare_avc
    IMPORTING
      !i_class_name TYPE seoclsname
      !i_new_table  TYPE flag DEFAULT ' '
    EXPORTING
      !t_output_avc TYPE t_ty_output_avc .
  METHODS compare_code
    IMPORTING
      !t_code_a TYPE t_ty_code
      !t_code_b TYPE t_ty_code
    EXPORTING
      !t_output TYPE t_ty_output .
  METHODS display_ext
    IMPORTING
      !i_container TYPE REF TO cl_gui_custom_container .
  METHODS compare_code_ext
    IMPORTING
      !t_code_a     TYPE t_ty_code
      !t_code_b     TYPE t_ty_code
    EXPORTING
      !t_output_ext TYPE t_ty_output_ext .
  METHODS compare_code_avc
    IMPORTING
      !i_class_name   TYPE seoclsname
      !t_classes_code TYPE t_ty_code
    EXPORTING
      !t_output_avc   TYPE t_ty_output_avc .
  METHODS display_avc
    IMPORTING
      !i_container TYPE REF TO cl_gui_custom_container .
ENDCLASS.



CLASS ZCL_CA_OBJECT_CODE_COMPARE IMPLEMENTATION.


  METHOD compare_avc.

    DATA lt_avc_classes  TYPE t_ty_avcontrol.
    DATA lt_classes_code TYPE t_ty_code.


    " Get AVControl Map for class
    me->get_avcontrol_classes( EXPORTING i_class_name  = i_class_name
                                         i_new_table   = i_new_table
                                CHANGING t_avc_classes = lt_avc_classes ).

    LOOP AT lt_avc_classes INTO DATA(ls_avc_class).

      me->get_class_code( EXPORTING
                            i_class_name     = ls_avc_class-class_name
                            i_origin         = CONV #( sy-tabix )
                          CHANGING
                            t_class_code     = lt_classes_code
                          EXCEPTIONS
                            error_components = 1
                            error_code       = 2
                            OTHERS           = 3 ).

    ENDLOOP.

    me->compare_code_avc( EXPORTING
                            i_class_name   = i_class_name
                            t_classes_code = lt_classes_code
                          IMPORTING
                            t_output_avc   = DATA(lt_output_avc) ).

    IF lt_output_avc[] IS NOT INITIAL.
      t_output_avc[] = lt_output_avc[].
    ENDIF.

  ENDMETHOD.


  METHOD COMPARE_CODE.

    DATA lt_output TYPE t_ty_output.
    DATA lv_text   TYPE string.

    LOOP AT t_code_a INTO DATA(ls_code_a).

      APPEND INITIAL LINE TO lt_output ASSIGNING FIELD-SYMBOL(<fs_output>).
      <fs_output>-object_name = ls_code_a-obj_name.
      CONCATENATE icon_message_warning(3) '\QApenas na classe local@' INTO <fs_output>-status_l_e.
      UNASSIGN <fs_output>.

    ENDLOOP.

    LOOP AT t_code_b INTO DATA(ls_code_b).

      READ TABLE lt_output ASSIGNING <fs_output> WITH KEY object_name = ls_code_b-obj_name.
      IF sy-subrc IS INITIAL.
        " Objecto deve ser comparado
        READ TABLE t_code_a INTO ls_code_a WITH KEY obj_name = ls_code_b-obj_name.
        IF sy-subrc IS INITIAL.
          IF ls_code_b-code[] EQ ls_code_a-code[].
            CONCATENATE icon_system_okay(3) '\QObjetos iguais@' INTO <fs_output>-status_l_e.
          ELSE.
            CONCATENATE icon_incomplete(3) '\QObjetos diferentes@' INTO <fs_output>-status_l_e.
          ENDIF.
        ENDIF.
      ELSE.
        APPEND INITIAL LINE TO lt_output ASSIGNING <fs_output>.
        <fs_output>-object_name = ls_code_b-obj_name.
        CONCATENATE icon_led_yellow(3) '\QApenas na classe externa@' INTO <fs_output>-status_l_e.
      ENDIF.
      UNASSIGN <fs_output>.

    ENDLOOP.

    IF lt_output[] IS NOT INITIAL.
      t_output[] = lt_output[].
    ENDIF.

  ENDMETHOD.


  METHOD compare_code_avc.

    DATA lt_output_avc TYPE t_ty_output_avc.
    DATA lv_text   TYPE string.

    DATA(lt_v1_class) = t_classes_code[].
    DELETE lt_v1_class WHERE origin NE '1'.
    DATA(lt_v2_class) = t_classes_code[].
    DELETE lt_v2_class WHERE origin NE '2'.
    DATA(lt_v3_class) = t_classes_code[].
    DELETE lt_v3_class WHERE origin NE '3'.

    DATA lt_v1_code TYPE abaptxt255_tab.
    DATA lt_v2_code TYPE abaptxt255_tab.
    DATA lt_v3_code TYPE abaptxt255_tab.

    " Comparar cabeçalho público
    FREE lt_v1_code.
    FREE lt_v2_code.
    FREE lt_v3_code.
    LOOP AT lt_v1_class INTO DATA(ls_cpub_v1) WHERE obj_name(4) EQ 'CPUB'.

      lt_v1_code = ls_cpub_v1-code[].
      DELETE lt_v1_code INDEX 1.

      LOOP AT lt_v2_class INTO DATA(ls_cpub_v2) WHERE obj_name(4) EQ 'CPUB'.

        lt_v2_code = ls_cpub_v2-code[].
        DELETE lt_v2_code INDEX 1.

        LOOP AT lt_v3_class INTO DATA(ls_cpub_v3) WHERE obj_name(4) EQ 'CPUB'.

          lt_v3_code = ls_cpub_v3-code[].
          DELETE lt_v3_code INDEX 1.

        ENDLOOP.

      ENDLOOP.

    ENDLOOP.

    APPEND INITIAL LINE TO lt_output_avc ASSIGNING FIELD-SYMBOL(<fs_out_avc>).
    <fs_out_avc>-object_name_v1 = ls_cpub_v1-obj_name.
    <fs_out_avc>-object_name_v2 = ls_cpub_v2-obj_name.
    <fs_out_avc>-object_name_v3 = ls_cpub_v3-obj_name.
    " Comparar V1 com V2
    IF lt_v1_code[] EQ lt_v2_code[].
      CONCATENATE icon_system_okay(3) '\QObjetos iguais@' INTO <fs_out_avc>-status_1_2 .
    ELSE.
      CONCATENATE icon_incomplete(3) '\QObjetos diferentes@' INTO <fs_out_avc>-status_1_2 .
    ENDIF.
    " Comparar V1 com V3
    IF lt_v1_code[] EQ lt_v3_code[].
      CONCATENATE icon_system_okay(3) '\QObjetos iguais@' INTO <fs_out_avc>-status_1_3 .
    ELSE.
      CONCATENATE icon_incomplete(3) '\QObjetos diferentes@' INTO <fs_out_avc>-status_1_3 .
    ENDIF.
    " Comparar V2 com V3
    IF lt_v2_code[] EQ lt_v3_code[].
      CONCATENATE icon_system_okay(3) '\QObjetos iguais@' INTO <fs_out_avc>-status_2_3 .
    ELSE.
      CONCATENATE icon_incomplete(3) '\QObjetos diferentes@' INTO <fs_out_avc>-status_2_3 .
    ENDIF.
    UNASSIGN <fs_out_avc>.

    " Comparar cabeçalho protegido
    FREE lt_v1_code.
    FREE lt_v2_code.
    FREE lt_v3_code.
    LOOP AT lt_v1_class INTO ls_cpub_v1 WHERE obj_name(4) EQ 'CPRO'.

      lt_v1_code = ls_cpub_v1-code[].
      DELETE lt_v1_code INDEX 1.

      LOOP AT lt_v2_class INTO ls_cpub_v2 WHERE obj_name(4) EQ 'CPRO'.

        lt_v2_code = ls_cpub_v2-code[].
        DELETE lt_v2_code INDEX 1.

        LOOP AT lt_v3_class INTO ls_cpub_v3 WHERE obj_name(4) EQ 'CPRO'.

          lt_v3_code = ls_cpub_v3-code[].
          DELETE lt_v3_code INDEX 1.

        ENDLOOP.

      ENDLOOP.

    ENDLOOP.

    APPEND INITIAL LINE TO lt_output_avc ASSIGNING <fs_out_avc>.
    <fs_out_avc>-object_name_v1 = ls_cpub_v1-obj_name.
    <fs_out_avc>-object_name_v2 = ls_cpub_v2-obj_name.
    <fs_out_avc>-object_name_v3 = ls_cpub_v3-obj_name.
    " Comparar V1 com V2
    IF lt_v1_code[] EQ lt_v2_code[].
      CONCATENATE icon_system_okay(3) '\QObjetos iguais@' INTO <fs_out_avc>-status_1_2 .
    ELSE.
      CONCATENATE icon_incomplete(3) '\QObjetos diferentes@' INTO <fs_out_avc>-status_1_2 .
    ENDIF.
    " Comparar V1 com V3
    IF lt_v1_code[] EQ lt_v3_code[].
      CONCATENATE icon_system_okay(3) '\QObjetos iguais@' INTO <fs_out_avc>-status_1_3 .
    ELSE.
      CONCATENATE icon_incomplete(3) '\QObjetos diferentes@' INTO <fs_out_avc>-status_1_3 .
    ENDIF.
    " Comparar V2 com V3
    IF lt_v2_code[] EQ lt_v3_code[].
      CONCATENATE icon_system_okay(3) '\QObjetos iguais@' INTO <fs_out_avc>-status_2_3 .
    ELSE.
      CONCATENATE icon_incomplete(3) '\QObjetos diferentes@' INTO <fs_out_avc>-status_2_3 .
    ENDIF.
    UNASSIGN <fs_out_avc>.

    " Comparar cabeçalho privado
    FREE lt_v1_code.
    FREE lt_v2_code.
    FREE lt_v3_code.
    LOOP AT lt_v1_class INTO ls_cpub_v1 WHERE obj_name(4) EQ 'CPRI'.

      lt_v1_code = ls_cpub_v1-code[].
      DELETE lt_v1_code INDEX 1.

      LOOP AT lt_v2_class INTO ls_cpub_v2 WHERE obj_name(4) EQ 'CPRI'.

        lt_v2_code = ls_cpub_v2-code[].
        DELETE lt_v2_code INDEX 1.

        LOOP AT lt_v3_class INTO ls_cpub_v3 WHERE obj_name(4) EQ 'CPRI'.

          lt_v3_code = ls_cpub_v3-code[].
          DELETE lt_v3_code INDEX 1.

        ENDLOOP.

      ENDLOOP.

    ENDLOOP.

    APPEND INITIAL LINE TO lt_output_avc ASSIGNING <fs_out_avc>.
    <fs_out_avc>-object_name_v1 = ls_cpub_v1-obj_name.
    <fs_out_avc>-object_name_v2 = ls_cpub_v2-obj_name.
    <fs_out_avc>-object_name_v3 = ls_cpub_v3-obj_name.
    " Comparar V1 com V2
    IF lt_v1_code[] EQ lt_v2_code[].
      CONCATENATE icon_system_okay(3) '\QObjetos iguais@' INTO <fs_out_avc>-status_1_2 .
    ELSE.
      CONCATENATE icon_incomplete(3) '\QObjetos diferentes@' INTO <fs_out_avc>-status_1_2 .
    ENDIF.
    " Comparar V1 com V3
    IF lt_v1_code[] EQ lt_v3_code[].
      CONCATENATE icon_system_okay(3) '\QObjetos iguais@' INTO <fs_out_avc>-status_1_3 .
    ELSE.
      CONCATENATE icon_incomplete(3) '\QObjetos diferentes@' INTO <fs_out_avc>-status_1_3 .
    ENDIF.
    " Comparar V2 com V3
    IF lt_v2_code[] EQ lt_v3_code[].
      CONCATENATE icon_system_okay(3) '\QObjetos iguais@' INTO <fs_out_avc>-status_2_3 .
    ELSE.
      CONCATENATE icon_incomplete(3) '\QObjetos diferentes@' INTO <fs_out_avc>-status_2_3 .
    ENDIF.
    UNASSIGN <fs_out_avc>.

    " Comparar métodos
    " 1º - adicionar todos os métodos da classe 1
    LOOP AT lt_v1_class INTO ls_cpub_v1 WHERE obj_name(4) EQ 'METH'.

      APPEND INITIAL LINE TO lt_output_avc ASSIGNING <fs_out_avc>.
      <fs_out_avc>-object_name_v1 = ls_cpub_v1-obj_name.
      UNASSIGN <fs_out_avc>.

    ENDLOOP.

    " 2º - comparar todos os métodos da classe 2 e, caso existam mais da classe 2, adicionar os mesmos
    LOOP AT lt_v2_class INTO ls_cpub_v2 WHERE obj_name(4) EQ 'METH'.

      " Verificar se o método já está no output
      LOOP AT lt_output_avc ASSIGNING <fs_out_avc> WHERE object_name_v1+35 EQ ls_cpub_v2-obj_name+35.
        " Método já existe no output, comparar com V1
        <fs_out_avc>-object_name_v2 = ls_cpub_v2-obj_name.
        LOOP AT lt_v1_class INTO ls_cpub_v1 WHERE obj_name+35 EQ ls_cpub_v2-obj_name+35.

          lt_v2_code[] = ls_cpub_v2-code[].
          lt_v1_code[] = ls_cpub_v1-code[].
          IF lt_v1_code[] EQ lt_v2_code[].
            CONCATENATE icon_system_okay(3) '\QObjetos iguais@' INTO <fs_out_avc>-status_1_2 .
          ELSE.
            CONCATENATE icon_incomplete(3) '\QObjetos diferentes@' INTO <fs_out_avc>-status_1_2 .
          ENDIF.

        ENDLOOP.
      ENDLOOP.
      IF sy-subrc IS NOT INITIAL.
        " Método ainda não existe no output, adicionar
        APPEND INITIAL LINE TO lt_output_avc ASSIGNING <fs_out_avc>.
        <fs_out_avc>-object_name_v2 = ls_cpub_v2-obj_name.
        UNASSIGN <fs_out_avc>.
      ENDIF.

    ENDLOOP.

    " 3º - comparar todos os métodos da classe 3 e, caso existam mais da classe 3, adicionar os mesmos
    LOOP AT lt_v3_class INTO ls_cpub_v3 WHERE obj_name(4) EQ 'METH'.

      " Verificar se o método já está no output
      LOOP AT lt_output_avc ASSIGNING <fs_out_avc> WHERE object_name_v1+35 EQ ls_cpub_v3-obj_name+35.
        " Método já existe no output
        <fs_out_avc>-object_name_v3 = ls_cpub_v3-obj_name.
        lt_v3_code[] = ls_cpub_v3-code[].
        " Comparar com V1
        LOOP AT lt_v1_class INTO ls_cpub_v1 WHERE obj_name+35 EQ ls_cpub_v3-obj_name+35.

          lt_v1_code[] = ls_cpub_v1-code[].
          IF lt_v1_code[] EQ lt_v3_code[].
            CONCATENATE icon_system_okay(3) '\QObjetos iguais@' INTO <fs_out_avc>-status_1_3 .
          ELSE.
            CONCATENATE icon_incomplete(3) '\QObjetos diferentes@' INTO <fs_out_avc>-status_1_3 .
          ENDIF.

        ENDLOOP.
        " Comparar com V2
        LOOP AT lt_v2_class INTO ls_cpub_v2 WHERE obj_name+35 EQ ls_cpub_v3-obj_name+35.

          lt_v1_code[] = ls_cpub_v1-code[].
          IF lt_v2_code[] EQ lt_v3_code[].
            CONCATENATE icon_system_okay(3) '\QObjetos iguais@' INTO <fs_out_avc>-status_2_3 .
          ELSE.
            CONCATENATE icon_incomplete(3) '\QObjetos diferentes@' INTO <fs_out_avc>-status_2_3 .
          ENDIF.

        ENDLOOP.
      ENDLOOP.
      IF sy-subrc IS NOT INITIAL.
        " Método ainda não existe no output, adicionar
        APPEND INITIAL LINE TO lt_output_avc ASSIGNING <fs_out_avc>.
        <fs_out_avc>-object_name_v3 = ls_cpub_v3-obj_name.
        UNASSIGN <fs_out_avc>.
      ENDIF.

    ENDLOOP.

    IF lt_output_avc[] IS NOT INITIAL.
      t_output_avc[] = lt_output_avc[].
    ENDIF.

  ENDMETHOD.


  METHOD COMPARE_CODE_EXT.

    DATA lt_output TYPE t_ty_output_ext.

    LOOP AT t_code_a INTO DATA(ls_code_a).

      APPEND INITIAL LINE TO lt_output ASSIGNING FIELD-SYMBOL(<fs_output>).
      <fs_output>-object_name = ls_code_a-obj_name.
      CONCATENATE icon_message_warning(3) '\QApenas no ' ls_code_a-obj_name(4) ' local@' INTO <fs_output>-status_l_e.
      UNASSIGN <fs_output>.

    ENDLOOP.

    LOOP AT t_code_b INTO DATA(ls_code_b).

      READ TABLE lt_output ASSIGNING <fs_output> WITH KEY object_name = ls_code_b-obj_name.
      IF sy-subrc IS INITIAL.
        " Objecto deve ser comparado
        READ TABLE t_code_a INTO ls_code_a WITH KEY obj_name = ls_code_b-obj_name.
        IF sy-subrc IS INITIAL.
          IF ls_code_b-code[] EQ ls_code_a-code[].
            CONCATENATE icon_system_okay(3) '\QObjetos iguais@' INTO <fs_output>-status_l_e.
          ELSE.
            CONCATENATE icon_incomplete(3) '\QObjetos diferentes@' INTO <fs_output>-status_l_e.
          ENDIF.
        ENDIF.
      ELSE.
        APPEND INITIAL LINE TO lt_output ASSIGNING <fs_output>.
        <fs_output>-object_name = ls_code_b-obj_name.
        CONCATENATE icon_led_yellow(3) '\QApenas no ' ls_code_b-obj_name(4) ' externo@' INTO <fs_output>-status_l_e.
      ENDIF.
      UNASSIGN <fs_output>.

    ENDLOOP.

    IF lt_output[] IS NOT INITIAL.
      t_output_ext[] = lt_output[].
    ENDIF.

  ENDMETHOD.


  METHOD compare_ext.

    DATA lt_local_code    TYPE t_ty_code.
    DATA lt_external_code TYPE t_ty_code.

    CASE i_obj_type.
      WHEN 'CLAS'.

        " Get local Code
        me->get_class_code( EXPORTING
                              i_class_name     = CONV #( i_obj_name )
                              i_origin         = c_origin-local
                            CHANGING
                              t_class_code     = lt_local_code
                            EXCEPTIONS
                              error_components = 1
                              error_code       = 2
                              OTHERS           = 3 ).

        " Get External Code
        me->get_class_code( EXPORTING
                              i_class_name     = CONV #( i_obj_name )
                              i_destination    = i_destination
                              i_origin         = c_origin-external
                            CHANGING
                              t_class_code     = lt_external_code
                            EXCEPTIONS
                              error_components = 1
                              error_code       = 2
                              OTHERS           = 3 ).

      WHEN 'REPS'.

      WHEN 'FUNC'.

      WHEN 'METH'.

        SPLIT i_obj_name AT space INTO DATA(lv_class) DATA(lv_meth).
        CONDENSE lv_meth.
        DATA(lv_method_name) = |{ lv_class WIDTH = 30 }{ lv_meth }|.

        " Get local Code
        me->get_method( EXPORTING
                          i_method_name = CONV #( lv_method_name )
                          i_destination = ''
                          i_origin      = c_origin-local
                        CHANGING
                          t_meth_code   = lt_local_code
                        EXCEPTIONS
                          no_code_found = 1
                          OTHERS        = 2 ).

        " Get External Code
        me->get_method( EXPORTING
                          i_method_name = CONV #( lv_method_name )
                          i_destination = i_destination
                          i_origin      = c_origin-external
                        CHANGING
                          t_meth_code   = lt_external_code
                        EXCEPTIONS
                          no_code_found = 1
                          OTHERS        = 2 ).

      WHEN 'CPUB'.

        " Get local Code
        me->get_public_header( EXPORTING
                                 i_class_name  = CONV #( i_obj_name )
                                 i_destination = ''
                                 i_origin      = c_origin-local
                               CHANGING
                                 t_cpub_code   = lt_local_code
                               EXCEPTIONS
                                 no_code_found = 1
                                 OTHERS        = 2 ).
        " Get External Code
        me->get_public_header( EXPORTING
                                 i_class_name  = CONV #( i_obj_name )
                                 i_destination = i_destination
                                 i_origin      = c_origin-external
                               CHANGING
                                 t_cpub_code   = lt_external_code
                               EXCEPTIONS
                                 no_code_found = 1
                                 OTHERS        = 2 ).


      WHEN 'CPRI'.

        " Get local Code
        me->get_private_header( EXPORTING
                                  i_class_name  = CONV #( i_obj_name )
                                  i_destination = ''
                                  i_origin      = c_origin-local
                                CHANGING
                                  t_cpri_code   = lt_local_code
                                EXCEPTIONS
                                  no_code_found = 1
                                  OTHERS        = 2 ).
        " Get External Code
        me->get_private_header( EXPORTING
                                  i_class_name  = CONV #( i_obj_name )
                                  i_destination = i_destination
                                  i_origin      = c_origin-external
                                CHANGING
                                  t_cpri_code   = lt_external_code
                                EXCEPTIONS
                                  no_code_found = 1
                                  OTHERS        = 2 ).

      WHEN 'CPRO'.

        " Get local Code
        me->get_protected_header( EXPORTING
                                    i_class_name  = CONV #( i_obj_name )
                                    i_destination = ''
                                    i_origin      = c_origin-local
                                  CHANGING
                                    t_cpro_code   = lt_local_code
                                  EXCEPTIONS
                                    no_code_found = 1
                                    OTHERS        = 2 ).

        " Get External Code
        me->get_protected_header( EXPORTING
                                    i_class_name  = CONV #( i_obj_name )
                                    i_destination = i_destination
                                    i_origin      = c_origin-external
                                  CHANGING
                                    t_cpro_code   = lt_local_code
                                  EXCEPTIONS
                                    no_code_found = 1
                                    OTHERS        = 2 ).

      WHEN OTHERS.

    ENDCASE.

    me->compare_code_ext( EXPORTING
                            t_code_a = lt_local_code
                            t_code_b = lt_external_code
                          IMPORTING
                            t_output_ext = DATA(lt_output_ext) ).

    IF lt_output_ext[] IS NOT INITIAL.
      t_output_ext[] = lt_output_ext[].
    ENDIF.

  ENDMETHOD.


  METHOD display_avc.

    " Create Grid
    gs_add_settings-alv = NEW cl_gui_alv_grid( i_parent = i_container ).
    " Set Layout
    gs_add_settings-layout-sel_mode   = 'A'.
    gs_add_settings-layout-no_toolbar = 'X'.
    " Set FielCatalog
    DATA lo_tabdescr TYPE REF TO cl_abap_structdescr.
    DATA lt_fields   TYPE ddfields.
    DATA lo_struct   TYPE REF TO data.
    DATA ls_fieldcat LIKE LINE OF gs_add_settings-fieldcatalog.

    CREATE DATA lo_struct LIKE LINE OF gt_output_avc.
    lo_tabdescr ?= cl_abap_structdescr=>describe_by_data_ref( lo_struct ).
    lt_fields = cl_salv_data_descr=>read_structdescr( lo_tabdescr ).

    LOOP AT lt_fields INTO DATA(ls_field).

      CLEAR ls_fieldcat.
      MOVE-CORRESPONDING ls_field TO ls_fieldcat.
      ls_fieldcat-col_pos   = sy-tabix.
      CASE ls_field-fieldname .
        WHEN 'OBJECT_NAME_V1' .
          ls_fieldcat-scrtext_s = ls_fieldcat-scrtext_m = ls_fieldcat-scrtext_l =
            ls_fieldcat-reptext = ls_fieldcat-seltext = ls_fieldcat-tooltip = 'Name V1'.
          ls_fieldcat-edit      = abap_false.
          ls_fieldcat-outputlen = '96'.
          APPEND ls_fieldcat TO gs_add_settings-fieldcatalog.
        WHEN 'STATUS_1_2' .
          ls_fieldcat-scrtext_s = ls_fieldcat-scrtext_m = ls_fieldcat-scrtext_l =
            ls_fieldcat-reptext = ls_fieldcat-seltext = ls_fieldcat-tooltip = 'V1<->V2'.
          ls_fieldcat-edit      = abap_false.
          ls_fieldcat-outputlen = '9'.
          ls_fieldcat-just = 'C'.
          APPEND ls_fieldcat TO gs_add_settings-fieldcatalog.
        WHEN 'OBJECT_NAME_V2' .
          ls_fieldcat-scrtext_s = ls_fieldcat-scrtext_m = ls_fieldcat-scrtext_l =
            ls_fieldcat-reptext = ls_fieldcat-seltext = ls_fieldcat-tooltip = 'Name V2'.
          ls_fieldcat-edit      = abap_false.
          ls_fieldcat-outputlen = '96'.
          APPEND ls_fieldcat TO gs_add_settings-fieldcatalog.
        WHEN 'STATUS_2_3' .
          LOOP AT gt_output_avc TRANSPORTING NO FIELDS WHERE object_name_v3(4) EQ 'METH'.
            ls_fieldcat-scrtext_s = ls_fieldcat-scrtext_m = ls_fieldcat-scrtext_l =
            ls_fieldcat-reptext = ls_fieldcat-seltext = ls_fieldcat-tooltip = 'V2<->V3'.
            ls_fieldcat-edit      = abap_false.
            ls_fieldcat-outputlen = '9'.
            ls_fieldcat-just = 'C'.
            APPEND ls_fieldcat TO gs_add_settings-fieldcatalog.
            EXIT.
          ENDLOOP.
        WHEN 'OBJECT_NAME_V3' .
          LOOP AT gt_output_avc TRANSPORTING NO FIELDS WHERE object_name_v3(4) EQ 'METH'.
            ls_fieldcat-scrtext_s = ls_fieldcat-scrtext_m = ls_fieldcat-scrtext_l =
              ls_fieldcat-reptext = ls_fieldcat-seltext = ls_fieldcat-tooltip = 'Name V3'.
            ls_fieldcat-edit      = abap_false.
            ls_fieldcat-outputlen = '96'.
            APPEND ls_fieldcat TO gs_add_settings-fieldcatalog.
            EXIT.
          ENDLOOP.
        WHEN 'STATUS_1_3' .
          LOOP AT gt_output_avc TRANSPORTING NO FIELDS WHERE object_name_v3(4) EQ 'METH'.
            ls_fieldcat-scrtext_s = ls_fieldcat-scrtext_m = ls_fieldcat-scrtext_l =
              ls_fieldcat-reptext = ls_fieldcat-seltext = ls_fieldcat-tooltip = 'V1<->V3'.
            ls_fieldcat-edit      = abap_false.
            ls_fieldcat-outputlen = '9'.
            ls_fieldcat-just = 'C'.
            APPEND ls_fieldcat TO gs_add_settings-fieldcatalog.
            EXIT.
          ENDLOOP.
        WHEN OTHERS.
      ENDCASE.

    ENDLOOP.

    gs_add_settings-layout-cwidth_opt = abap_true.

    " Display
    gs_add_settings-alv->set_table_for_first_display(
      EXPORTING
        i_save                        = 'A'
        is_layout                     = gs_add_settings-layout
*        it_toolbar_excluding          = gs_add_settings-exclude
      CHANGING
        it_outtab                     = gt_output_avc
        it_fieldcatalog               = gs_add_settings-fieldcatalog
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3 ).


  ENDMETHOD.


  METHOD display_ext.

    " Create Grid
    gs_add_settings-alv = NEW cl_gui_alv_grid( i_parent = i_container ).
    " Set Layout
    gs_add_settings-layout-sel_mode   = 'A'.
    gs_add_settings-layout-no_toolbar = 'X'.
    " Set FielCatalog
    DATA lo_tabdescr TYPE REF TO cl_abap_structdescr.
    DATA lt_fields   TYPE ddfields.
    DATA lo_struct   TYPE REF TO data.

    CREATE DATA lo_struct LIKE LINE OF gt_output_ext.
    lo_tabdescr ?= cl_abap_structdescr=>describe_by_data_ref( lo_struct ).
    lt_fields = cl_salv_data_descr=>read_structdescr( lo_tabdescr ).

    LOOP AT lt_fields INTO DATA(ls_field).
      APPEND INITIAL LINE TO gs_add_settings-fieldcatalog ASSIGNING FIELD-SYMBOL(<fs_fieldcat>).
      MOVE-CORRESPONDING ls_field TO <fs_fieldcat>.
      <fs_fieldcat>-col_pos   = sy-tabix.

      CASE <fs_fieldcat>-fieldname .
        WHEN 'OBJECT_NAME' .
          <fs_fieldcat>-scrtext_s = <fs_fieldcat>-scrtext_m = <fs_fieldcat>-scrtext_l =
            <fs_fieldcat>-reptext = <fs_fieldcat>-seltext = <fs_fieldcat>-tooltip = 'Object Name'.
          <fs_fieldcat>-edit      = abap_false.
          <fs_fieldcat>-outputlen = '90'.
        WHEN 'STATUS_L_E' .
          <fs_fieldcat>-scrtext_s = <fs_fieldcat>-scrtext_m = <fs_fieldcat>-scrtext_l =
            <fs_fieldcat>-reptext = <fs_fieldcat>-seltext = <fs_fieldcat>-tooltip = 'Comparison'.
          <fs_fieldcat>-edit      = abap_false.
          <fs_fieldcat>-outputlen = '9'.
        WHEN OTHERS.
          <fs_fieldcat>-tech = '1'.
      ENDCASE.

    ENDLOOP.

    " Display
    gs_add_settings-alv->set_table_for_first_display(
      EXPORTING
        i_save                        = 'A'
        is_layout                     = gs_add_settings-layout
*        it_toolbar_excluding          = gs_add_settings-exclude
      CHANGING
        it_outtab                     = gt_output_ext
        it_fieldcatalog               = gs_add_settings-fieldcatalog
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3 ).


  ENDMETHOD.


  METHOD f4_obj_type.

    CONSTANTS lc_field_name TYPE tabname  VALUE 'OBJECT'.
    CONSTANTS lc_value_org  TYPE ddbool_d VALUE 'S'.

    DATA lt_ko100 TYPE STANDARD TABLE OF ko100.

    DATA lt_return      TYPE STANDARD TABLE OF ddshretval.

    CALL FUNCTION 'TR_OBJECT_TABLE'
      TABLES
        wt_object_text = lt_ko100.

    DELETE lt_ko100 WHERE
*                      object NE 'FUNC' AND object NE 'REPS' AND
                          object NE 'CLAS'
                      AND object NE 'CPRO'
                      AND object NE 'CPRI'
                      AND object NE 'CPUB'
                      AND object NE 'METH'.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = lc_field_name
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = iv_field
        value_org       = lc_value_org
      TABLES
        value_tab       = lt_ko100
        return_tab      = lt_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.



  ENDMETHOD.


  METHOD get_avcontrol_classes.

    DATA lv_origin_class TYPE seoclsname.

    IF i_new_table EQ abap_false. " Use /YGA/VERS_ATIV

      " Check if class is "Destiny" class
      SELECT classe_origem, classe_destino
        INTO TABLE @DATA(lt_yga_classes)
        FROM /yga/vers_ativ
       WHERE classe_destino EQ @i_class_name.

      IF sy-subrc IS INITIAL.
        " IT IS a "Destiny" class, get "Origin" class
        READ TABLE lt_yga_classes INTO DATA(ls_yga_class) INDEX 1.
        IF sy-subrc IS INITIAL.
          lv_origin_class = ls_yga_class-classe_origem.
        ENDIF.
      ELSE.
        " It is NOT a "Destiny" class, then it must be an "Origin" class
        lv_origin_class = i_class_name.
      ENDIF.

      " Get "Destiny" classes
      FREE lt_yga_classes.
      SELECT classe_origem classe_destino
        INTO TABLE lt_yga_classes
        FROM /yga/vers_ativ
       WHERE classe_origem EQ lv_origin_class.

      IF sy-subrc IS INITIAL.

        SORT lt_yga_classes by classe_destino.
        DELETE ADJACENT DUPLICATES FROM lt_yga_classes COMPARING classe_destino.

        LOOP AT lt_yga_classes INTO ls_yga_class.
          APPEND INITIAL LINE TO t_avc_classes ASSIGNING FIELD-SYMBOL(<fs_avc_class>).
          <fs_avc_class>-class_name = ls_yga_class-classe_destino.
          UNASSIGN <fs_avc_class>.
        ENDLOOP.

      ELSE.
        RAISE not_avc_class.
      ENDIF.

    ELSEIF i_new_table EQ abap_true. " Use ZCA_VERS_ATIV

      " Check if class is "Destiny" class
      SELECT source_class, dest_class
        INTO TABLE @DATA(lt_zca_classes)
        FROM zca_vers_ativ
       WHERE dest_class EQ @i_class_name.

      IF sy-subrc IS INITIAL.
        " IT IS a "Destiny" class, get "Origin" class
        READ TABLE lt_zca_classes INTO DATA(ls_zca_class) INDEX 1.
        IF sy-subrc IS INITIAL.
          lv_origin_class = ls_zca_class-source_class.
        ENDIF.
      ELSE.
        " It is NOT a "Destiny" class, then it must be an "Origin" class
        lv_origin_class = i_class_name.
      ENDIF.

      " Get "Destiny" classes
      FREE lt_zca_classes.
      SELECT source_class dest_class
        INTO TABLE lt_zca_classes
        FROM zca_vers_ativ
       WHERE source_class EQ lv_origin_class.

      IF sy-subrc IS INITIAL.

        SORT lt_zca_classes BY dest_class.
        DELETE ADJACENT DUPLICATES FROM lt_zca_classes COMPARING dest_class.

        LOOP AT lt_zca_classes INTO ls_zca_class.
          APPEND INITIAL LINE TO t_avc_classes ASSIGNING <fs_avc_class>.
          <fs_avc_class>-class_name = ls_zca_class-dest_class.
          UNASSIGN <fs_avc_class>.
        ENDLOOP.

      ELSE.
        RAISE not_avc_class.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD GET_CLASS_CODE.

    DATA lt_class_code   TYPE t_ty_code.
    DATA lt_headers_code TYPE t_ty_code.
    DATA lt_methods_code TYPE t_ty_code.

    DATA lv_obj_name   TYPE vrsd-objname.

    me->get_class_components( EXPORTING
                                i_class_name          = i_class_name
                                i_destination         = i_destination
                              IMPORTING
                                t_components          = DATA(lt_class_components)
                              EXCEPTIONS
                                definition_not_found  = 1
                                error_accessing_table = 2
                                no_data_found         = 3
                                OTHERS                = 4 ).

    IF sy-subrc IS INITIAL.

      " Get Class Headers Code
      READ TABLE lt_class_components INTO DATA(ls_class_component) INDEX 1.
      IF sy-subrc IS INITIAL.
        me->get_class_headers( EXPORTING
                                 i_class_name   = i_class_name
                                 i_destination  = i_destination
                                 i_origin       = i_origin
                               CHANGING
                                 t_headers_code = lt_headers_code
                               EXCEPTIONS
                                 no_code_found  = 1
                                 OTHERS         = 2 ).
        IF sy-subrc IS INITIAL.
          APPEND LINES OF lt_headers_code TO lt_class_code.
        ENDIF.

      ENDIF.

      " Get Class Methods Code
      me->get_class_methods( EXPORTING
                               t_components   = lt_class_components
                               i_destination  = i_destination
                               i_origin       = i_origin
                             CHANGING
                               t_methods_code = lt_methods_code
                             EXCEPTIONS
                               no_code_found  = 1
                               OTHERS         = 2 ).
      IF sy-subrc IS INITIAL.
        APPEND LINES OF lt_methods_code TO lt_class_code.
      ENDIF.

      IF lt_class_code[] IS NOT INITIAL.
        APPEND LINES OF lt_class_code TO t_class_code.
      ELSE.
        RAISE error_code.
      ENDIF.

    ELSE.
      RAISE error_components.
    ENDIF.

  ENDMETHOD.


  METHOD GET_CLASS_COMPONENTS.

    DATA lt_dfies   TYPE STANDARD TABLE OF dfies.
    DATA lt_options TYPE STANDARD TABLE OF rfc_db_opt.
    DATA lt_fields  TYPE STANDARD TABLE OF rfc_db_fld.
    DATA lt_data    TYPE STANDARD TABLE OF tab512.

    DATA lv_subrc   TYPE syst_subrc.

*    " Get TMDIR table definition
*    CALL FUNCTION 'DDIF_FIELDINFO_GET'
*      EXPORTING
*        tabname        = c_tables-tmdir
*      TABLES
*        dfies_tab      = lt_dfies
*      EXCEPTIONS
*        internal_error = 1
*        not_found      = 2
*        OTHERS         = 3.
*
*    IF sy-subrc IS INITIAL.

*      lt_nametab[] = lt_dfies[].

    " Set selection conditions
    APPEND INITIAL LINE TO lt_options ASSIGNING FIELD-SYMBOL(<fs_options>).
    <fs_options>-text = |CLASSNAME EQ '{ i_class_name }'|.
    UNASSIGN <fs_options>.

    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION i_destination
      EXPORTING
        query_table          = c_tables-tmdir
*       DELIMITER            = ' '
*       NO_DATA              = ' '
*       ROWSKIPS             = 0
*       ROWCOUNT             = 0
      TABLES
        options              = lt_options
        fields               = lt_fields
        data                 = lt_data
      EXCEPTIONS
        table_not_available  = 1
        table_without_data   = 2
        option_not_valid     = 3
        field_not_valid      = 4
        not_authorized       = 5
        data_buffer_exceeded = 6
        OTHERS               = 7.

    IF  sy-subrc IS INITIAL.

      " Convert result to TMDIR type
      LOOP AT lt_data INTO DATA(ls_data).
        APPEND INITIAL LINE TO t_components ASSIGNING FIELD-SYMBOL(<fs_tmdir>).
        <fs_tmdir>-classname  = ls_data-wa(30).
        <fs_tmdir>-methodindx = ls_data-wa+30(5).
        <fs_tmdir>-methodname = ls_data-wa+35(61).
        <fs_tmdir>-flags      = ls_data-wa+96(10).
        UNASSIGN <fs_tmdir>.
      ENDLOOP.

*      LOOP AT lt_entries INTO DATA(ls_entry).
*        APPEND INITIAL LINE TO t_components ASSIGNING FIELD-SYMBOL(<fs_tmdir>).
*        <fs_tmdir>-classname  = ls_entry-entry(30).
*        <fs_tmdir>-methodindx = ls_entry-entry+30(5).
*        <fs_tmdir>-methodname = ls_entry-entry+35(61).
*        <fs_tmdir>-flags      = ls_entry-entry+96(10).
*        UNASSIGN <fs_tmdir>.
*      ENDLOOP.

*      ELSE.
*        " No data selected
*        RAISE no_data_found.
*      ENDIF.

    ELSE.
      " Error on TABLE_ENTRIES_GET_VIA_RFC call
      RAISE error_accessing_table.
    ENDIF.

*    ELSE.
*      " Error on DDIF_FIELDINFO_GET call
*      RAISE definition_not_found.
*    ENDIF.

  ENDMETHOD.


  METHOD GET_CLASS_HEADERS.

    DATA lt_public_header    TYPE t_ty_code.
    DATA lt_protected_header TYPE t_ty_code.
    DATA lt_private_header   TYPE t_ty_code.
    DATA lt_headers_code     TYPE t_ty_code.

    " Public Header Code
    FREE lt_public_header.
    me->get_public_header( EXPORTING
                             i_class_name  = i_class_name
                             i_destination = i_destination
                             i_origin      = i_origin
                           CHANGING
                             t_cpub_code   = lt_public_header
                           EXCEPTIONS
                             no_code_found = 1
                             OTHERS        = 2 ).
    IF sy-subrc IS INITIAL.
      APPEND LINES OF lt_public_header TO lt_headers_code.
    ENDIF.

    " Protected Header Code
    FREE lt_protected_header.
    me->get_protected_header( EXPORTING
                                i_class_name  = i_class_name
                                i_destination = i_destination
                                i_origin      = i_origin
                              CHANGING
                                t_cpro_code   = lt_protected_header
                              EXCEPTIONS
                                no_code_found = 1
                                OTHERS        = 2 ).
    IF sy-subrc IS INITIAL.
      APPEND LINES OF lt_protected_header TO lt_headers_code.
    ENDIF.

    " Private Header Code
    FREE lt_private_header.
    me->get_private_header( EXPORTING
                              i_class_name  = i_class_name
                              i_destination = i_destination
                              i_origin      = i_origin
                            CHANGING
                              t_cpri_code   = lt_private_header
                            EXCEPTIONS
                              no_code_found = 1
                              OTHERS        = 2 ).
    IF sy-subrc IS INITIAL.
      APPEND LINES OF lt_private_header TO lt_headers_code.
    ENDIF.

    IF lt_headers_code[] IS NOT INITIAL.
      APPEND LINES OF lt_headers_code TO t_headers_code.
    ELSE.
      RAISE no_code_found.
    ENDIF.


  ENDMETHOD.


  METHOD GET_CLASS_METHODS.

    DATA lt_methods_code TYPE t_ty_code.
    DATA lv_method_name  TYPE versobjnam.

    FREE lt_methods_code.
    LOOP AT t_components INTO DATA(ls_class_component) WHERE methodname IS NOT INITIAL.

      CLEAR lv_method_name.
      lv_method_name = |{ ls_class_component-classname WIDTH = 30 }{ ls_class_component-methodname }|.
      me->get_method( EXPORTING
                        i_method_name = lv_method_name
                        i_destination = i_destination
                        i_origin      = i_origin
                      CHANGING
                        t_meth_code   = lt_methods_code
                      EXCEPTIONS
                        no_code_found = 1
                        OTHERS        = 2 ).
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.

    ENDLOOP.

    IF lt_methods_code[] IS NOT INITIAL.
      APPEND LINES OF lt_methods_code TO t_methods_code.
    ELSE.
      RAISE no_code_found.
    ENDIF.

  ENDMETHOD.


  METHOD GET_METHOD.

    DATA lt_method_code TYPE t_ty_code.

    " Method
    FREE lt_method_code.
    me->get_object_code( EXPORTING
                           i_obj_name    = i_method_name
                           i_obj_type    = c_obj_type-class-method
                           i_destination = i_destination
                           i_origin      = i_origin
                         CHANGING
                           t_code        = lt_method_code
                         EXCEPTIONS
                           no_version    = 1
                           OTHERS        = 2 ).

    IF sy-subrc IS INITIAL.
      APPEND LINES OF lt_method_code TO t_meth_code.
    ELSE.
      RAISE no_code_found.
    ENDIF.

  ENDMETHOD.


  METHOD GET_OBJECT_CODE.

    DATA lt_code      TYPE t_ty_code.
    DATA lt_repos_tab TYPE STANDARD TABLE OF abaptxt255.
    DATA lt_trdir_tab TYPE STANDARD TABLE OF trdir.

    FREE lt_repos_tab.
    FREE lt_trdir_tab.
    CALL FUNCTION 'SVRS_GET_REPS_FROM_OBJECT'
      EXPORTING
        object_name = i_obj_name
        object_type = i_obj_type
        versno      = i_versno
        destination = i_destination
      TABLES
        repos_tab   = lt_repos_tab
        trdir_tab   = lt_trdir_tab
      EXCEPTIONS
        no_version  = 1
        OTHERS      = 2.

    IF sy-subrc IS INITIAL.
      APPEND INITIAL LINE TO lt_code ASSIGNING FIELD-SYMBOL(<fs_code>).
      <fs_code>-obj_name = |{ i_obj_type } { i_obj_name }|.
      <fs_code>-origin   = i_origin.
      <fs_code>-code[]   = lt_repos_tab[].
      UNASSIGN <fs_code>.
    ENDIF.

    IF lt_code[] IS NOT INITIAL.
      APPEND LINES OF lt_code TO t_code.
    ELSE.
      RAISE no_version.
    ENDIF.

  ENDMETHOD.


  METHOD GET_PRIVATE_HEADER.

    DATA lt_private_header_code TYPE t_ty_code.

    DATA(lv_class_name) = CONV versobjnam( i_class_name ).

    " Public Header
    FREE lt_private_header_code.
    me->get_object_code( EXPORTING
                           i_obj_name    = lv_class_name
                           i_obj_type    = c_obj_type-class-private_header
                           i_destination = i_destination
                           i_origin      = i_origin
                         CHANGING
                           t_code        = lt_private_header_code
                         EXCEPTIONS
                           no_version    = 1
                           OTHERS        = 2 ).

    IF sy-subrc IS INITIAL.
      APPEND LINES OF lt_private_header_code TO t_cpri_code.
    ELSE.
      RAISE no_code_found.
    ENDIF.

  ENDMETHOD.


  METHOD GET_PROTECTED_HEADER.

    DATA lt_protected_header_code TYPE t_ty_code.

    DATA(lv_class_name) = CONV versobjnam( i_class_name ).

    " Public Header
    FREE lt_protected_header_code.
    me->get_object_code( EXPORTING
                           i_obj_name    = lv_class_name
                           i_obj_type    = c_obj_type-class-protected_header
                           i_destination = i_destination
                           i_origin      = i_origin
                         CHANGING
                           t_code        = lt_protected_header_code
                         EXCEPTIONS
                           no_version    = 1
                           OTHERS        = 2 ).

    IF sy-subrc IS INITIAL.
      APPEND LINES OF lt_protected_header_code TO t_cpro_code.
    ELSE.
      RAISE no_code_found.
    ENDIF.

  ENDMETHOD.


  METHOD GET_PUBLIC_HEADER.

    DATA lt_public_header_code TYPE t_ty_code.

    DATA(lv_class_name) = CONV versobjnam( i_class_name ).

    " Public Header
    FREE lt_public_header_code.
    me->get_object_code( EXPORTING
                           i_obj_name    = lv_class_name
                           i_obj_type    = c_obj_type-class-public_header
                           i_destination = i_destination
                           i_origin      = i_origin
                         CHANGING
                           t_code        = lt_public_header_code
                         EXCEPTIONS
                           no_version    = 1
                           OTHERS        = 2 ).

    IF sy-subrc IS INITIAL.
      APPEND LINES OF lt_public_header_code TO t_cpub_code.
    ELSE.
      RAISE no_code_found.
    ENDIF.

  ENDMETHOD.


  METHOD main.

    CASE i_option.
      WHEN 'EXT'. " Comparison between Local and External Object

        me->compare_ext( EXPORTING i_obj_type = i_obj_type i_obj_name = i_obj_name i_destination = i_destination
                         IMPORTING t_output_ext = gt_output_ext ).

        me->display_ext( i_container = i_container ).

      WHEN 'AVC'. " Comparison between AVC versions

        me->compare_avc( EXPORTING i_class_name = CONV #( i_obj_name )
                                   i_new_table  = i_new_table
                         IMPORTING t_output_avc = gt_output_avc  ).

        me->display_avc( i_container = i_container ).

      WHEN 'DUO'. " Compare two objects

      WHEN OTHERS.
        " Unknown OPTION
    ENDCASE.

  ENDMETHOD.


  METHOD remote_compare.

    CONSTANTS lc_report TYPE trdir-name VALUE 'RSVRSRS3'.

    DATA lt_index_rows TYPE lvc_t_row.
    DATA lt_row_no     TYPE lvc_t_roid.
    DATA lt_fields     TYPE STANDARD TABLE OF sval.
    DATA lv_wave       TYPE /yga/wave.
    DATA lv_returncode.
    DATA lv_obj_type TYPE vrsd-objtype.
    DATA lv_obj_name TYPE vrsd-objname.

    DATA ls_infoline1a   TYPE vrsinfolna.
    DATA ls_infoline1b   TYPE vrsinfolnb.
    DATA ls_infoline2a   TYPE vrsinfolna.
    DATA ls_infoline2b   TYPE vrsinfolnb.
    DATA lv_objname1_l   TYPE vrsd-objname.
    DATA lv_objname2_l   TYPE vrsd-objname.

    gs_add_settings-alv->get_selected_rows( IMPORTING
                                            et_index_rows = lt_index_rows
                                            et_row_no     = lt_row_no ).

    IF lines( lt_index_rows ) EQ 1.

      LOOP AT lt_index_rows INTO DATA(ls_row).
        READ TABLE gt_output_ext INTO DATA(ls_output) INDEX ls_row-index.
        IF sy-subrc IS INITIAL.

          SPLIT ls_output-object_name AT space INTO lv_obj_type lv_obj_name.

          SUBMIT (lc_report) AND RETURN
            WITH objname  = lv_obj_name
            WITH objnam2  = lv_obj_name
            WITH versno1  = '00000'
            WITH versno2  = '00000'
            WITH objtyp1  = lv_obj_type "METH
            WITH objtyp2  = lv_obj_type "METH
            WITH infoln1a = ls_infoline1a
            WITH infoln1b = ls_infoline1b
            WITH infoln2a = ls_infoline2a
            WITH infoln2b = ls_infoline2b
            WITH log_dest = 'TMSADM@P15.DOMAIN_D26'
            WITH rem_syst = 'P15'.

        ENDIF.
      ENDLOOP.

    ELSE.
      MESSAGE 'Por favor selecione apenas uma linha' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.




  ENDMETHOD.
ENDCLASS.