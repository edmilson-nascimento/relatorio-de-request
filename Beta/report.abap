*&---------------------------------------------------------------------*
*& Report /YGA/R_TRANSP_CONTROL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /yga/r_transp_control.

INCLUDE /yga/r_transp_control_top.
INCLUDE /yga/r_transp_control_scr.

INITIALIZATION.

  CREATE OBJECT go_report
    EXPORTING
      i_repid = sy-repid.

*   Inicializar datas
*  go_report->init_erdat( changing cr_erdat = s_erdat[] ).

AT SELECTION-SCREEN OUTPUT.
  go_report->at_selection_screen_output( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varian .
  IF ( go_report IS BOUND ) .
     go_report->get_varian( CHANGING ch_varian = p_varian ) .
  ENDIF .

START-OF-SELECTION.

  IF ( go_report IS NOT BOUND ) .
    RETURN .
  ENDIF .

  DATA(imp_p15) =
    go_report->get_prod_filter(
      im_aprovado = p_ip15_y
      im_nao_apro = p_ip15_n
      im_ambos    = p_ip15_b ) .

  go_report->main(
    EXPORTING i_cd      = p_cd1
*             ir_cd1    = s_cd1[]
              i_obs     = p_obs
              i_descr   = p_descr
              ir_cd3    = s_cd3[]
              ir_user   = s_usror[]
              ir_date   = s_erdat[]
              ir_time   = s_erzet[]
              i_aprov   = p_aprov
              i_imp_p15 = imp_p15
              i_chang   = p_chang
              i_variant = p_varian ) .




*