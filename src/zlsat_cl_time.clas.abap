class ZLSAT_CL_TIME definition
  public
  final
  create public .


  public section.

    class-methods get_timestampl
      returning
        value(r_result) type timestampl.

    class-methods convert_timestampl
      importing
        value(iv_timestampl) type timestampl
        value(iv_timezone)   type sy-zonlo default sy-zonlo
      exporting
        ev_date              type sy-datum
        ev_time              type sy-uzeit.

    data mv_timestampl type timestampl.
    data mo_timer type ref to cl_gui_timer.


    data mv_uzeit_next_check type uzeit.
    data mv_uzeit_last_input type uzeit.
    data mv_seconds_inact_logout type i.

    methods constructor
      importing
        value(iv_timestampl) type timestampl optional.

    methods init.

    methods refresh
      importing
        value(iv_timestampl) type timestampl optional.

    methods get_diff_in_sec
      importing
        value(iv_timestampl_high) type timestampl optional
      returning
        value(r_result)           type i.

    methods on_timer_finished for event finished of cl_gui_timer
      importing
        sender.

    methods timer_run
      importing
        iv_wait_time_in_sec type i.


  private section.
    class-methods _example_how_tu_use_this_class.

endclass.

class ZLSAT_CL_TIME implementation.

  method get_timestampl.

    get time stamp field r_result.

  endmethod.

  method constructor.

    refresh( iv_timestampl ).

    mo_timer = new cl_gui_timer( ).
    set handler on_timer_finished for mo_timer.

    mv_uzeit_last_input = sy-uzeit.

  endmethod.

  method convert_timestampl.

    if iv_timestampl is initial.
      iv_timestampl = get_timestampl(  ).
    endif.

    convert time stamp iv_timestampl
    time zone iv_timezone into
    date ev_date
    time ev_time.

  endmethod.

  method get_diff_in_sec.

    if iv_timestampl_high is initial.
      iv_timestampl_high = get_timestampl(  ).
    endif.

    r_result = cl_abap_tstmp=>subtract(
         tstmp1 = iv_timestampl_high
         tstmp2 = mv_timestampl ).

  endmethod.

  method on_timer_finished.

    data: lv_date         type sy-datum,
          lv_time         type sy-uzeit,
          lv_diff_seconds type i,
          lv_uzeit        type sy-uzeit.

    "ist es nach x uhr?
    convert_timestampl(
      exporting
        iv_timestampl = get_timestampl( )
*        iv_timezone   = SY-ZONLO
      importing
        ev_date       = lv_date
        ev_time       = lv_time
    ).


    if lv_time > mv_uzeit_next_check and mv_uzeit_next_check is not initial.

*      lcl_help2=>gui_popup(
*        lcl_help2=>msg(
*          i_any  = 'Transktion wird beendet. Logout Uhrzeit &1'
*          i_v1   = mv_uzeit_next_check
*          i_type = 'I' ) ).

      leave to transaction sy-tcode.

    else.

      data(lv_sec_last_input) = lv_time - mv_uzeit_last_input.

      if lv_sec_last_input > mv_seconds_inact_logout.

*        lcl_help2=>gui_popup(
*          lcl_help2=>msg(
*            i_any  = 'Transaktion wird beendet. Inaktivität für &1 Sekunden'
*            i_v1   = lv_sec_last_input
*            i_type = 'I' ) ).

        leave to transaction sy-tcode.

      endif.
    endif.

    init( ).

  endmethod.

  method timer_run.

    mo_timer->interval = iv_wait_time_in_sec.
    mo_timer->run( ).

  endmethod.

  method refresh.

    if iv_timestampl is initial.
      iv_timestampl = get_timestampl(  ).
    endif.

    mv_timestampl = iv_timestampl.

  endmethod.

  method _example_how_tu_use_this_class.

  endmethod.

  method init.

*    select single *
*    from zstc_t001w
*    into @data(ls_t001w)
*    where werks =  @gs_0200-werks.
*
*    if sy-subrc <> 0.
*      "kein customizing
*      return.
*    endif.
*
*    mv_seconds_inact_logout = ls_t001w-seconds_check.
*
*    try. "calculate next logout time
*
*        data lt_time type standard table of uzeit.
*
*        data(lv_uzeit) = sy-uzeit.
*        insert ls_t001w-uzeit_check1 into table lt_time.
*        insert ls_t001w-uzeit_check2 into table lt_time.
*        insert ls_t001w-uzeit_check3 into table lt_time.
*        insert ls_t001w-uzeit_check4 into table lt_time.
*        insert ls_t001w-uzeit_check5 into table lt_time.
*        insert lv_uzeit into table lt_time.
*
*        delete lt_time where table_line is initial.
*        sort lt_time by table_line ascending.
*        delete adjacent duplicates from lt_time comparing all fields.
*
*        data(lv_index) = line_index( lt_time[ table_line = lv_uzeit ] ).
*        mv_uzeit_next_check = lt_time[ lv_index + 1 ].
*
*      catch cx_root.
*    endtry.

    timer_run( 300 ).
  endmethod.

endclass.


