REPORT zrequest_compare.

CLASS local_class DEFINITION .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !is_object  TYPE sctsobject
        !iv_rfcdest TYPE rfcdes-rfcdest .

    METHODS check_error
      RETURNING
        VALUE(rv_value) TYPE abap_bool .


    METHODS show_compare .

  PRIVATE SECTION .

    DATA:
      gt_message     TYPE bapiret2_t,
      gs_object      TYPE sctsobject,
      gv_destination TYPE rfcdes-rfcdest.

ENDCLASS .

*----------------------------------------------------------------------*
*       CLASS lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS local_class IMPLEMENTATION.

  METHOD constructor .

    CLEAR:
      me->gt_message, me->gv_destination, me->gs_object .

    IF ( iv_rfcdest IS NOT INITIAL ) .
      me->gv_destination = iv_rfcdest .
    ENDIF .

    IF ( is_object IS NOT INITIAL ) .
      me->gs_object = is_object .
    ENDIF .


  ENDMETHOD .


  METHOD check_error .

    IF ( line_exists( me->gt_message[ type = if_xo_const_message=>error ] ) ) .
      rv_value = abap_true .
    ELSE .
      rv_value = abap_false .
    ENDIF .

  ENDMETHOD .


  METHOD show_compare .

    DATA(ls_infoln1b) =
      VALUE vrsinfolnb( korrnum = 'D00K901960'
                        " fill1   =
                        datum   = '12.07.2018'
                        " fill2   =
                        author  = 'ABAP01' ) .

    DATA(ls_infoln2b) =
      VALUE vrsinfolnb( korrnum = 'D00K901960'
                        " fill1   =
                        datum   = '10.09.2018'
                        " fill2   =
                        author  = 'ABAP01' ) .

    "SUBMIT (dir_f5_report) AND RETURN
    SUBMIT rsvrsrs3 AND RETURN
         "WITH objname  = 'ZRFFOBR_D'
          WITH objname  = me->gs_object-object
         "WITH objnam2  = 'ZRFFOBR_D'
          WITH objnam2  = me->gs_object-object
          WITH versno1  = '00000'
          WITH versno2  = '00000'
          WITH objtyp1  = 'REPS'
          WITH objtyp2  = 'REPS'
         "WITH infoln1a = 'ZRFFOBR_D'
          WITH infoln1a = me->gs_object-object
          WITH infoln1b = ls_infoln1b
         "WITH infoln2a = 'ZRFFOBR_D'
          WITH infoln2a = me->gs_object-object
          WITH infoln2b = ls_infoln2b
          WITH log_dest = me->gv_destination
          WITH rem_syst = 'QAS' .

  ENDMETHOD .

ENDCLASS .


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS:
  p_pgmid  TYPE sctsobject-pgmid  DEFAULT 'R3TR',
  p_object TYPE sctsobject-object DEFAULT 'PROG',
  p_objnam TYPE sctsobject-text   DEFAULT 'ZRFFOBR_D'.

PARAMETERS:
  p_rfcdes TYPE rfcdes-rfcdest.

SELECTION-SCREEN END OF BLOCK b1.


START-OF-SELECTION .

  DATA(go_report) =
    NEW local_class( is_object  = VALUE sctsobject( pgmid      = p_pgmid
                                                    object     = p_object
                                                    text       = p_objnam )
                     iv_rfcdest = p_rfcdes ) .

  IF ( go_report IS BOUND ) AND
     ( go_report->check_error( ) EQ abap_false ) .

    go_report->show_compare( ) .

  ENDIF .

END-OF-SELECTION.


*
*
