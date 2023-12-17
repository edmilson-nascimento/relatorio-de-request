*&---------------------------------------------------------------------*
*&  Include           /YGA/R_TRANSP_CONTROL_SCR
*&---------------------------------------------------------------------*

* Tela inserção de dados
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

SELECTION-SCREEN SKIP.
*  select-options: s_cd1 for /yga/transp_ctrl-charm_change modif id tr1 no intervals matchcode object /yga/ca_user_order.
  PARAMETERS: p_cd1 TYPE /yga/transp_ctrl-charm_change OBLIGATORY MODIF ID tr1 MATCHCODE OBJECT /yga/ca_user_order MEMORY ID ID_CD1.
  PARAMETERS: p_descr TYPE /yga/descricao_cd OBLIGATORY MODIF ID tr1.
  SELECTION-SCREEN SKIP.
  PARAMETERS: p_obs TYPE /yga/obs_ot_tec MODIF ID tr1.
  SELECTION-SCREEN SKIP.

SELECTION-SCREEN END OF BLOCK b1.

**   Tela visualização de dados
*SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-003.
*
*SELECTION-SCREEN SKIP.
*SELECT-OPTIONS: s_cd3    FOR /yga/transp_ctrl-charm_change MODIF ID tr3 NO INTERVALS MATCHCODE OBJECT /yga/h_charm_change MEMORY ID ID_CD3.
*SELECTION-SCREEN SKIP.
*
*SELECT-OPTIONS: s_usror  FOR e070-as4user MODIF ID tr3,
*                s_erdat  FOR e070-as4date MODIF ID tr3,
*                s_erzet  FOR e070-as4time MODIF ID tr3.
*SELECTION-SCREEN SKIP.
*
**SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-006.
** Filtros referente a importação
*  PARAMETERS: p_aprov  AS CHECKBOX MODIF ID tr3.
*  PARAMETERS: p_imp15  AS CHECKBOX MODIF ID tr3.
**SELECTION-SCREEN END OF BLOCK b3.
*
*PARAMETERS: p_chang  DEFAULT 'X' NO-DISPLAY.
*
*SELECTION-SCREEN END OF BLOCK b2.

* Visualização de dados
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-003.

  SELECT-OPTIONS:
     s_cd3 FOR /yga/transp_ctrl-charm_change MODIF ID tr3
     MATCHCODE OBJECT /yga/h_charm_change MEMORY ID id_cd3,

     s_usror  FOR e070-as4user MODIF ID tr3,
     s_erdat  FOR e070-as4date MODIF ID tr3,
     s_erzet  FOR e070-as4time MODIF ID tr3 .
  SELECTION-SCREEN SKIP.

*   Filtros referente a importação
  SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-006.
*
    PARAMETERS:
      p_aprov  AS CHECKBOX MODIF ID tr3 .
*     p_check  AS CHECKBOX MODIF ID tr3.

*   Transportado para Ambiente Produtivo P15
    SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-007.
      PARAMETERS:
        p_ip15_y  RADIOBUTTON GROUP ip15 MODIF ID tr3,
        p_ip15_n  RADIOBUTTON GROUP ip15 MODIF ID tr3,
        p_ip15_b  RADIOBUTTON GROUP ip15 MODIF ID tr3 DEFAULT 'X'.
    SELECTION-SCREEN END OF BLOCK b4.

  SELECTION-SCREEN END OF BLOCK b3.

  PARAMETERS:
    p_varian type slis_vari MODIF ID tr3,
    p_chang  DEFAULT 'X' NO-DISPLAY.

SELECTION-SCREEN END OF BLOCK b2.