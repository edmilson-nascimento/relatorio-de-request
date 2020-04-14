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

    types:
      begin of ty_trkorr,
        sign   type ddsign,
        option type ddoption,
        low    type e070-trkorr,
        high   type e070-trkorr,
      end of ty_trkorr,

      begin of ty_as4user,
        sign   type ddsign,
        option type ddoption,
        low    type e070-as4user,
        high   type e070-as4user,
      end of ty_as4user,

      begin of ty_as4date,
        sign   type ddsign,
        option type ddoption,
        low    type e070-as4date,
        high   type e070-as4date,
      end of ty_as4date,

      begin of ty_out,
        trkorr     type e071-trkorr,
        as4text    type e07t-as4text,
        as4user    type e070-as4user,
        trfunction type e070-trfunction,
        trstatus   type e070-trstatus,
        korrdev    type e070-korrdev,
      end of ty_out,
      tab_out   type table of ty_out,

      r_trkorr  type table of ty_trkorr,
      r_as4user type table of ty_as4user,
      r_as4date type table of ty_as4date.

    data:
      out_tab type tab_out .

    methods  get_data
      importing
        !trkorr  type cl_report=>r_trkorr
        !as4user type cl_report=>r_as4user
        !as4date type cl_report=>r_as4date .

  protected section .

  private section .

    methods check_search
      importing
        !trkorr      type cl_report=>r_trkorr
        !as4user     type cl_report=>r_as4user
        !as4date     type cl_report=>r_as4date
      returning
        value(value) type abap_bool .

    methods search
      importing
        !trkorr  type cl_report=>r_trkorr
        !as4user type cl_report=>r_as4user
        !as4date type cl_report=>r_as4date
      changing
        !e070    type e070_t
        !e07t    type e07t_t .

    methods organize
      importing
        !e070 type e070_t
        !e07t type e07t_t .


endclass.

*----------------------------------------------------------------------*
*       CLASS cl_alv IMPLEMENTATION
*----------------------------------------------------------------------*
class cl_alv definition .

  public section .

    methods constructor .

    methods set_data
      changing
        !data type any table .

    methods error
      returning
        value(value) type abap_bool .

    methods show .

  protected section .

  private section .

    data:

      log        type bapiret2_t,

      salv_table type ref to cl_salv_table,
      events     type ref to cl_salv_events_table,
      display    type ref to cl_salv_display_settings,
      sorts      type ref to cl_salv_sorts,
      column     type ref to cl_salv_column_list,
      columns    type ref to cl_salv_columns_table.


endclass .

*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS cl_alv IMPLEMENTATION
*----------------------------------------------------------------------*
class cl_select_options definition .

  public section .

    class-methods init
      changing
        !sysname type c
        !as4date type c .

  protected section .
  private section .
endclass .

*----------------------------------------------------------------------*
*       CLASS cl_report IMPLEMENTATION
*----------------------------------------------------------------------*
class cl_report implementation.


  method get_data .

    data:
      e070_tab type e070_t,
      e07t_tab type e07t_t.


    if ( me->check_search(
           trkorr  = trkorr
           as4user = as4user
           as4date = as4date
         ) eq abap_true ) .

      me->search(
        exporting
          trkorr  = trkorr
          as4user = as4user
          as4date = as4date
        changing
          e070    = e070_tab
          e07t    = e07t_tab
      ).

      if ( lines( e070_tab ) gt 0 ) and
         ( lines( e07t_tab ) gt 0 ) .

        me->organize(
         e070    = e070_tab
         e07t    = e07t_tab
        ).

      endif .

    endif .

  endmethod .


  method check_search .


    if ( lines( trkorr )  gt 0 ) or
       ( lines( as4user ) gt 0 ) or
       ( lines( as4date ) gt 0 ) .

      value = abap_on .

    else .

      value = abap_off .

    endif .


  endmethod .


  method search .

    data:
      filter type icl_where_condition_t .

    if ( lines( trkorr )  gt 0 ) .
      append 'trkorr in trkorr' to filter .
    endif .

    if ( lines( as4user )  gt 0 ) .
      if ( lines( filter ) gt 0 ).
        append 'and' to filter .
      endif .
      append 'as4user in as4user' to filter .
    endif .

    if ( lines( as4date )  gt 0 ) .
      if ( lines( filter ) gt 0 ).
        append 'and' to filter .
      endif .
      append 'as4date in as4date' to filter .
    endif .

    if ( lines( filter ) eq 0 ) .
    else .


      try .

          select *
            from e070
            into table e070
           where (filter) .

          if ( sy-subrc eq 0 ) .

            select *
              from e07t
              into table e07t
               for all entries in e070
             where trkorr eq e070-trkorr .

            if ( sy-subrc eq 0 ) .
            endif .

          endif .

        catch cx_sy_dynamic_osql_syntax .
      endtry .

    endif .

  endmethod .


  method organize .

    data:
      e070_line type e070,
      e07t_line type e07t,
      out_line  type cl_report=>ty_out.

    loop at e070 into e070_line .

      out_line-trkorr     = e070_line-trkorr .

      read table e07t into e07t_line
        with key trkorr = e070_line-trkorr .
      if ( sy-subrc eq 0 ) .
        out_line-as4text    = e07t_line-as4text .
      endif .

      out_line-as4user    = e070_line-as4user .
      out_line-trfunction = e070_line-trfunction .
      out_line-trstatus   = e070_line-trstatus .
      out_line-korrdev    = e070_line-korrdev .

      append out_line to me->out_tab .
      clear  out_line .

    endloop .

  endmethod .


endclass.

*----------------------------------------------------------------------*
*       CLASS cl_alv IMPLEMENTATION
*----------------------------------------------------------------------*
class cl_alv implementation .


  method constructor .
  endmethod .


  method set_data .

    if ( lines( data ) eq 0 ) .
    else .

      try .

          call method cl_salv_table=>factory
*            exporting
*              list_display   = if_salv_c_bool_sap=>false
*              r_container    =
*              container_name =
            importing
              r_salv_table = me->salv_table
            changing
              t_table      = data.

        catch cx_salv_msg .
      endtry .

    endif .

  endmethod .


  method error .

    if ( lines( me->log ) eq 0 ) .
      value = abap_off .
    else .

      if ( line_exists( me->log[ type = 'E' ] ) ) .
        value = abap_on .
      else .
        value = abap_off .
      endif .

    endif .

  endmethod .


  method show .


    if ( me->salv_table is bound ) .


*          events = me->salv_table->get_event( ).
*
*          set handler me->on_link_click for events.
*          set handler me->on_added_function for events.
*
*          me->salv_table->set_screen_status(
*            pfstatus      = 'STANDARD_FULLSCREEN'
*            report        = 'SAPLKKBL'
*            set_functions = me->salv_table->c_functions_all ).
*
*
*          columns = me->salv_table->get_columns( ).
*
*          columns->set_optimize( 'X' ).
*          column ?= columns->get_column( 'NAVTREE' ).
*          column->set_icon( if_salv_c_bool_sap=>true ).
*          column->set_cell_type( if_salv_c_cell_type=>hotspot ).
*          column->set_long_text( 'Nível' ).
*          column->set_symbol( if_salv_c_bool_sap=>true ).
*
**       Layout de Zebra
*          display = me->salv_table->get_display_settings( ) .
*          display->set_striped_pattern( cl_salv_display_settings=>true ) .
*
**       Ordenação de campos
*          sorts = me->salv_table->get_sorts( ) .
*          sorts->add_sort('CARRID') .

      me->salv_table->display( ).

*        catch cx_salv_msg .
*        catch cx_salv_not_found .
*        catch cx_salv_existing .
*        catch cx_salv_data_error .
*        catch cx_salv_object_not_found .
*
*      endtry.

    endif .


  endmethod .

endclass .


*----------------------------------------------------------------------*
*       CLASS cl_select_options IMPLEMENTATION
*----------------------------------------------------------------------*
class cl_select_options implementation .

  method init .
  endmethod .

endclass .


*--------------------------------------------------------------------*
*- Tela de seleção
*--------------------------------------------------------------------*
data:
  report type ref to cl_report,
  alv    type ref to cl_alv.

*--------------------------------------------------------------------*
*- Tela de seleção
*--------------------------------------------------------------------*
selection-screen begin of block b1 with frame title text-001.

select-options:
  s_sysnam for trtarget-tarsystem no intervals obligatory,
  s_trkorr for e070-trkorr,
  s_as4use for e070-as4user,
  s_as4dat for e070-as4date .

selection-screen end of block b1.

*--------------------------------------------------------------------*
*- Eventos
*--------------------------------------------------------------------*
initialization.

start-of-selection .

  create object report .

  if ( report is bound ) .

    create object alv .

    if ( alv is bound ) .

      alv->set_data(
        changing
          data = report->out_tab ) .


      if ( alv->error( ) eq abap_false ) .

        alv->show( ) .

      endif .

    endif .

  endif .

end-of-selection.
