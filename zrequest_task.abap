REPORT ZREQUEST_CLASS.

*--------------------------------------------------------------------*
*- Anotações
*--------------------------------------------------------------------*
*- Coloca icone ou mesmo informação para Modificavel e etc

*--------------------------------------------------------------------*
*- Tipos SAP
*--------------------------------------------------------------------*
*type-pools:
*  sscr, vrm, ctslg, icon .

*--------------------------------------------------------------------*
*- Tabelas
*--------------------------------------------------------------------*
tables:
  e070, trtarget.

*----------------------------------------------------------------------*
*       CLASS cl_report IMPLEMENTATION
*----------------------------------------------------------------------*
class cl_report definition .

  public section .
  protected section .
  private section .


endclass.

*----------------------------------------------------------------------*
*       CLASS cl_alv IMPLEMENTATION
*----------------------------------------------------------------------*
class cl_alv definition .
  public section .
  protected section .
  private section .
endclass .

*----------------------------------------------------------------------*
*       CLASS cl_report IMPLEMENTATION
*----------------------------------------------------------------------*
class cl_report implementation.

endclass.

*----------------------------------------------------------------------*
*       CLASS cl_alv IMPLEMENTATION
*----------------------------------------------------------------------*
class cl_alv implementation .
endclass .


*--------------------------------------------------------------------*
*- Tela de seleção
*--------------------------------------------------------------------*
*data:
*  report type ref to cl_relatorio.

*--------------------------------------------------------------------*
*- Tela de seleção
*--------------------------------------------------------------------*
selection-screen begin of block b1 with frame title text-001.

select-options:
  s_sysnam for trtarget-tarsystem no intervals obligatory,
  s_trkorr for e070-trkorr.
selection-screen end of block b1.

*--------------------------------------------------------------------*
*- Eventos
*--------------------------------------------------------------------*
initialization.

start-of-selection .

end-of-selection.
