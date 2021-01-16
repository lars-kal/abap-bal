class ltcl_test definition final for testing
  duration short
  risk level harmless.

  private section.
    methods test_type for testing raising cx_static_check.
    methods test_element for testing raising cx_static_check.
    methods test_struct for testing raising cx_static_check.
    methods test_tab for testing raising cx_static_check.
    methods test_obj for testing raising cx_static_check.
    methods test_msg for testing raising cx_static_check.

endclass.


class ltcl_test implementation.

  method test_type.

    data(lo_msg) = new zls_hlp_cl_msg( ).

    sy-msgty = 'E'.
    if lo_msg->get( sy )-is_error = abap_false.

      cl_aunit_assert=>fail(
        msg    = 'GET: is_error flag wrong'    " Error Message
        quit = if_aunit_constants=>no  ).

    endif.

    sy-msgty = 'S'.
    if lo_msg->get( sy )-type <> 'S'.

      cl_aunit_assert=>fail(
        msg    = 'GET: type S not found'    " Error Message
        quit = if_aunit_constants=>no  ).

    endif.

    if lo_msg->get( sy )-is_error = abap_true.

      cl_aunit_assert=>fail(
        msg    = 'GET: is_error flag wrong'    " Error Message
        quit = if_aunit_constants=>no  ).

    endif.

    sy-msgty = 'E'.
    data(lt_bapi) = value bapiret2_tab( ).
    insert lo_msg->get( sy )-s_bapi into table lt_bapi.
    sy-msgty = 'S'.
    insert lo_msg->get( sy )-s_bapi into table lt_bapi.
    if lo_msg->get( lt_bapi )-is_error = abap_false.

      cl_aunit_assert=>fail(
        msg    = 'GET: is_error flag wrong'    " Error Message
        quit = if_aunit_constants=>no  ).

    endif.

    sy-msgty = 'W'.
    clear lt_bapi.
    insert lo_msg->get( sy )-s_bapi into table lt_bapi.
    sy-msgty = 'I'.
    insert lo_msg->get( sy )-s_bapi into table lt_bapi.
    if lo_msg->get( lt_bapi )-type <> 'W'.

      cl_aunit_assert=>fail(
        msg    = 'GET: type W is wrong'    " Error Message
        quit = if_aunit_constants=>no  ).

    endif.

  endmethod.

  method test_struct.
    data(lo_msg) = new zls_hlp_cl_msg( ).

    data(ls_sy_copy) = sy.
    data(ls_bapi) = lo_msg->get( ls_sy_copy )-s_bapi.

    data(ls_sy) = value sy( ).
    lo_msg->main(
      exporting
        input  = ls_bapi
      importing
        result = ls_sy
    ).
    if ls_sy-msgid <> ls_sy_copy-msgid.
      cl_aunit_assert=>fail(
         msg    = 'MAP struct: msgid wrong'    " Error Message
         quit = if_aunit_constants=>no  ).
    endif.

    ls_sy-msgid = '398'.
    if ls_sy-msgid <> lo_msg->get( ls_sy )-s_bapi-id.

      cl_aunit_assert=>fail(
      msg    = 'GET struct: msgid wrong'    " Error Message
      quit = if_aunit_constants=>no  ).

    endif.

    ls_sy-msgno = '123'.
    if ls_sy-msgno <> lo_msg->get( ls_sy )-s_bapi-number.

      cl_aunit_assert=>fail(
      msg    = 'GET struct: number wrong'    " Error Message
      quit = if_aunit_constants=>no  ).

    endif.


  endmethod.

  method test_element.

    data(lo_msg) = new zls_hlp_cl_msg( ).

    if lo_msg->get( 'das ist ein text' )-txt = 'das ist ein test'.
      cl_aunit_assert=>fail(
         msg    = 'get element: type wrong'    " Error Message
         quit = if_aunit_constants=>no  ).
    endif.

  endmethod.

  method test_msg.

    data(lo_msg) = new zls_hlp_cl_msg( ).
    data(lv_msgid) = 'MSGCLASS' .
    data(ls_msg) = lo_msg->get_by_msg( id = lv_msgid ).

    if ls_msg-s_bapi-id <> lv_msgid.

      cl_aunit_assert=>fail(
            msg    = 'get_msg: id wrong'    " Error Message
            quit = if_aunit_constants=>no  ).

    endif.

    message e398(00) with 'V1' 'V2' 'V3' 'V4' into data(lv_dummy).
    data(ls_sy) = sy.

    if lo_msg->get( ls_sy )-s_bapi-message_v1 <> 'V1'.

      cl_aunit_assert=>fail(
           msg    = 'message info wrong'    " Error Message
           quit = if_aunit_constants=>no  ).


    endif.

    if lo_msg->get( ls_sy )-s_bapi-message_v2 <> 'V2'.

      cl_aunit_assert=>fail(
         msg    = 'message info wrong'    " Error Message
         quit = if_aunit_constants=>no  ).

    endif.

    if lo_msg->get( ls_sy )-s_bapi-message_v3 <> 'V3'.

      cl_aunit_assert=>fail(
         msg    = 'message info wrong'    " Error Message
         quit = if_aunit_constants=>no  ).

    endif.

    if lo_msg->get( ls_sy )-s_bapi-message_v4 <> 'V4'.

      cl_aunit_assert=>fail(
         msg    = 'message info wrong'    " Error Message
         quit = if_aunit_constants=>no  ).

    endif.

    if lo_msg->get( ls_sy )-txt <> 'V1 V2 V3 V4'.

      cl_aunit_assert=>fail(
         msg    = 'message info wrong'    " Error Message
         quit = if_aunit_constants=>no  ).

    endif.

    if lo_msg->get( ls_sy )-type <> 'E'.

      cl_aunit_assert=>fail(
         msg    = 'message info wrong'    " Error Message
         quit = if_aunit_constants=>no  ).

    endif.

    if lo_msg->get( ls_sy )-is_error <> abap_true.

      cl_aunit_assert=>fail(
         msg    = 'message info wrong'    " Error Message
         quit = if_aunit_constants=>no  ).

    endif.

    if lo_msg->get( ls_sy )-s_bapi-id = '398'.

      cl_aunit_assert=>fail(
         msg    = 'message info wrong'    " Error Message
         quit = if_aunit_constants=>no  ).

    endif.

  endmethod.

  method test_tab.

    data lt_t100 type standard table of t100.
    data lt_bapi type bapiret2_tab.
    data lt_bapi1 type bapirettab.
    data lt_sy type standard table of sy.
    data lt_sy_copy type standard table of sy.

    message e398(00) with 'V1' 'V2' into data(lv_dummy).
    data(ls_sy) = sy.
    insert ls_sy into table lt_sy.
    insert ls_sy into table lt_sy.

    lt_sy_copy = lt_sy.

    new zls_hlp_cl_msg( )->main(
      exporting
        input           = lt_sy
      importing
        result          = lt_bapi
    ).

    new zls_hlp_cl_msg( )->main(
     exporting
       input           = lt_bapi
     importing
       result          = lt_bapi1
   ).

    new zls_hlp_cl_msg( )->main(
     exporting
       input           = lt_bapi1
     importing
       result          = lt_sy
   ).

    if lt_sy[ 2 ]-msgv1 <> lt_sy_copy[ 2 ]-msgv1 or
      lt_sy[ 2 ]-msgv2 <> lt_sy_copy[ 2 ]-msgv2 or
      lt_sy[ 2 ]-msgv3 <> lt_sy_copy[ 2 ]-msgv3 or
      lt_sy[ 2 ]-msgv4 <> lt_sy_copy[ 2 ]-msgv4 or
      lt_sy[ 2 ]-msgid <> lt_sy_copy[ 2 ]-msgid or
      lt_sy[ 2 ]-msgno <> lt_sy_copy[ 2 ]-msgno
      .

      cl_aunit_assert=>fail(
         msg    = 'message info wrong'    " Error Message
         quit = if_aunit_constants=>no  ).

    endif.

    new zls_hlp_cl_msg( )->main(
     exporting
       input           = lt_sy
     importing
       result          = lt_t100
   ).

    new zls_hlp_cl_msg( )->main(
     exporting
       input           = lt_t100
     importing
       result          = lt_sy
   ).

    if lt_sy[ 2 ]-msgid <> lt_sy[ 2 ]-msgid or
      lt_sy[ 2 ]-msgno <> lt_sy[ 2 ]-msgno
      .

      cl_aunit_assert=>fail(
         msg    = 'message info wrong'    " Error Message
         quit = if_aunit_constants=>no  ).

    endif.


  endmethod.

  method test_obj.

    try.

        raise exception new cx_t100_msg(
          t100_msgid = '00'
          t100_msgno = '398'
          t100_msgv1 = 'V1'
          t100_msgv2 = 'V2'
          t100_msgv3 = 'V3'
          t100_msgv4 = 'V4'
        ).

      catch cx_root into data(lx).

        message e398(00) with 'V1' 'V2' 'V3' 'V4' into data(lv_dummy).
        data(ls_sy) = sy.
        data(ls_sy_copy) = ls_sy.

        data(ls_bapi) = new zls_hlp_cl_msg( )->get( lx )-s_bapi.

        new zls_hlp_cl_msg( )->main(
          exporting
            input           = ls_bapi
*    io_typedescr_in =
          importing
            result          = ls_sy
        ).

        if
            ls_sy-msgid <> ls_sy_copy-msgid or
            ls_sy-msgno <> ls_sy_copy-msgno.
*            ls_sy-msgv1 <> ls_sy_copy-msgv1
            .
          cl_aunit_assert=>fail(
             msg    = 'message info wrong'    " Error Message
             quit = if_aunit_constants=>no  ).
        endif.

    endtry.

    try.


*    raise exception new cx_t100_msg(
*      t100_msgid = '00'
*      t100_msgno = '398'
*      t100_msgv1 = 'V1'
*      t100_msgv2 = 'V2'
*      t100_msgv3 = 'V3'
*      t100_msgv4 = 'V4'
*    ).

      catch cx_root into lx.

        message e398(00) with 'V1' 'V2' into lv_dummy.
        ls_sy = sy.
        ls_sy_copy = ls_sy.

        ls_bapi = new zls_hlp_cl_msg( )->get( lx )-s_bapi.

        new zls_hlp_cl_msg( )->main(
          exporting
            input           = ls_bapi
*    io_typedescr_in =
          importing
            result          = ls_sy
        ).

        if ls_sy <> ls_sy_copy.
          cl_aunit_assert=>fail(
             msg    = 'message info wrong'    " Error Message
             quit = if_aunit_constants=>no  ).
        endif.

    endtry.




  endmethod.

endclass.
