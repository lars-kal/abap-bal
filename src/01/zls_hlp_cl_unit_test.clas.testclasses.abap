class ltcl_unit_test definition final for testing
  duration short
  risk level harmless.

  private section.
    methods:
      test_error for testing raising cx_static_check.
    methods:
      test_message for testing raising cx_static_check.

    data hlp type ref to zls_hlp_cl_main.
endclass.


class ltcl_unit_test implementation.

  method test_error.
    hlp = new #( ).
    data lx type ref to cx_root.


    try.

        raise exception lcx=>factory_sy( ).

        raise exception type lcx. "hlp->mx.
        raise exception new lcx( ) .". "hlp->mx.
* message id 'ZTEST' number '001'.

        raise exception type lcx  "hlp->mx
              message id 'ZTEST' number '001'.


      catch cx_root into lx.
        data(lt_log) = lcx=>get_log( lx ).
    endtry.

  endmethod.

  method test_message.
    hlp = new #( ).
    data(ls_t100) = value t100( msgnr = '001' arbgb = '60'  ).
    data(ls_bapi) = value bapiret2( ).


    data(lt_bapi) = value bapirettab( ( id = 'ABC' number = '234' ) ).
    data(lt_bapi2) = value bapirettab( ( id = 'ABC' number = '234' ) ).
*  clear ls_t100.


*do 10000 times.

    hlp=>msg->main(
      exporting
        input  = ls_t100
      importing
        result = ls_bapi
    ).

    hlp=>msg->main(
    exporting
      input  = ls_t100
    importing
      result = lt_bapi
  ).
*enddo.

*  do 10000 times.
    hlp=>msg->main(
    exporting
      input  = lt_bapi
    importing
      result = lt_bapi2
  ).

*  enddo.


    data(lv_text) =  hlp=>msg->get( ls_bapi )-txt.

    if  hlp=>msg->get( value bapiret2( type = 'E' ) )-type = 'E'.
      data(lv_dummy) = 'test'.
    endif.

    if  hlp=>msg->get( value bapiret2( type = 'S' ) )-type = 'E'.
*    data(lv_Dummy) = 'test'.
    endif.


*enddo.

*  data(lt_bapi2) = lcl_msg=>log_factory( )->log( ls_bapi )->log( ls_t100 )->log_get( )-t_msg.

*  if lcl_msg=>get( ls_bapi )-is_error = abap_true.

*  endif.

*  if lcl_msg=>log_factory( )->log( ls_bapi )->log( ls_t100 )->log_get( )-is_error = abap_true.

*  endif.




*  lcl_msg=>factory( )->msg->clear(  )->



    break-point.

*  select *
*    from spfli
*    where contains ( ( cityfrom, cityto), 'Tokio', fuzzy(0.8) )
*  into table @data(data)
*  .
*
*
*  data(ls_bapi) = value bapiret2(  ).
*  data(ls_balm) = value balm(  ).
*
*  lcl_msg=>mapper(
*    exporting
*      i_any = ls_bapi
*    importing
*      e_any = ls_balm
*  ).


  endmethod.

endclass.
