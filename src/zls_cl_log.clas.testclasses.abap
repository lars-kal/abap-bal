*"* use this source file for your ABAP unit test classes
CLASS ltcl_unit_test_log DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      test_bal FOR TESTING RAISING cx_static_check,
      test_local FOR TESTING RAISING cx_static_check,
      test_local_chain FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_unit_test_log IMPLEMENTATION.

  METHOD test_bal.


    DATA(lo_log) = NEW zls_cl_log( ).

    MESSAGE e001(/scwm/rf_de) INTO DATA(lv_msg).
    lo_log->add( sy ).

    lo_log->db_save( ).

    MESSAGE e002(/scwm/rf_de) WITH '4711' INTO lv_msg.
    lo_log->add( sy ).

    lo_log->add_tag(
      EXPORTING
        iv_ident = 'TANUM' " Anwendungs-Log: Parameter
        i_any    = '4711'  ).

    lo_log->db_save( ).


    lo_log->add_msg(
        iv_msgty     = 'E'     " Nachrichtentyp
        iv_msgid     = '/SCWM/RF_DE'
        iv_msgno     = '003'   ).

    lo_log->db_save( ).


    MESSAGE i004(/scwm/rf_de) WITH '4812' INTO lv_msg.
    lo_log->add( sy ).

    lo_log->add_tag(
      EXPORTING
        iv_ident = 'TANUM' " Anwendungs-Log: Parameter
        i_any    = '4812'  ).

    lo_log->db_save( ).


    IF lo_log->get( )-is_error = abap_false.

      cl_aunit_assert=>fail(
        msg    = 'log handling error'    " Error Message
        quit = if_aunit_constants=>no  ).

    ENDIF.

    SELECT * UP TO 10 ROWS FROM /scwm/huhdr  INTO TABLE @DATA(lt_hu).

    lo_log->add_tag(
      EXPORTING
        iv_ident = 'HUIDENT' " Anwendungs-Log: Parameter
        i_any    = lt_hu  ).

    lo_log->db_save( ).

    WAIT UP TO '0.5' SECONDS.

    DATA(lo_new_log) = zls_cl_log=>factory_by_bal( lo_log->ms_balhdr-extnumber  ).

    DATA(lt_db) = lo_new_log->get( )-t_bapi.
    DATA(lt_local) = lo_log->get( )-t_bapi.

    IF lines( lt_db ) <> 4.
      cl_aunit_assert=>fail(
        msg    = 'wrong message processing'    " Error Message
       quit = if_aunit_constants=>no  ).
    ENDIF.

    IF lines( lt_db ) <> lines( lt_local ).

      cl_aunit_assert=>fail(
          msg    = 'BAL write/read with db not working'    " Error Message
         quit = if_aunit_constants=>no  ).

    ENDIF.


    IF  lo_new_log->get( )-is_error = abap_false.

      cl_aunit_assert=>fail(
        msg    = 'log handling error'    " Error Message
        quit = if_aunit_constants=>no  ).

    ENDIF.

  ENDMETHOD.



  METHOD test_local.

    DATA(lo_log) = NEW zls_cl_log( ).

    MESSAGE e001(/scwm/rf_de) INTO DATA(lv_msg).
    lo_log->add( sy ).

    IF lo_log->get( )-type <> 'E'.
      cl_aunit_assert=>fail(
    msg    = 'get type: E not found'    " Error Message
   quit = if_aunit_constants=>no  ).
    ENDIF.


    IF lo_log->get( )-is_error = abap_false.
      cl_aunit_assert=>fail(
      msg    = 'is error: not found'    " Error Message
     quit = if_aunit_constants=>no  ).
    ENDIF.

    DATA(lt_bapi) = lo_log->get( )-t_bapi.

    IF lt_bapi[ 1 ]-number <> '001'.

      cl_aunit_assert=>fail(
  msg    = 'mt_log wrong'    " Error Message
 quit = if_aunit_constants=>no  ).

    ENDIF.


    DATA(lo_log_new) = NEW zls_cl_log( )->add( lo_log ).

    IF lines( lo_log_new->mt_log ) <> lines( lo_log->mt_log ).

      cl_aunit_assert=>fail(
  msg    = 'log copy not working'    " Error Message
 quit = if_aunit_constants=>no  ).

    ENDIF.

    MESSAGE s001(/scwm/rf_de) INTO lv_msg.
    DATA(ls_sy) = sy.

    DATA(lo_log3) = NEW zls_cl_log( )->add( ls_sy ).
    IF lo_log3->get( )-is_error = abap_true.

      cl_aunit_assert=>fail(
  msg    = 'type not working'    " Error Message
 quit = if_aunit_constants=>no  ).

    ENDIF.

    IF lo_log3->get( )-type <> 'S'.

      cl_aunit_assert=>fail(
  msg    = 'type not working'    " Error Message
 quit = if_aunit_constants=>no  ).

    ENDIF.

    DATA lt_messages TYPE bapirettab.
    INSERT VALUE #(
     id = '00'
     number =  '398'
     message_v1 = 'V1'
         message_v2 = 'V1'
             message_v3 = 'V1'
                 message_v4 = 'V1'
     ) INTO TABLE lt_messages.

    data(lo_bal) = new zls_cl_log( ).

    lo_bal->add( lt_messages ).

    if lo_bal->mt_log[ 1 ]-v1 <> 'V1'.

     cl_aunit_assert=>fail(
  msg    = 'type not working'    " Error Message
 quit = if_aunit_constants=>no  ).


    endif.

  ENDMETHOD.



  METHOD test_local_chain.

    MESSAGE e001(/scwm/rf_de) INTO DATA(lv_msg).
    DATA(ls_sy) = sy.

    DATA(lo_log) = NEW zls_cl_log(
        )->add( ls_sy
        )->add( ls_sy
        )->add_msg(
          iv_msgty  = 'E'     " Nachrichtentyp
          iv_msgid  = '/SCWM/RF_DE'
          iv_msgno  = '003'
         ).
*         ->db_save( ).

    IF lines( lo_log->mt_log ) <> 3 .

      cl_aunit_assert=>fail(
       msg  = 'log handling error'    " Error Message
       quit = if_aunit_constants=>no  ).

    ENDIF.

    IF lines( lo_log->add( ls_sy )->mt_log ) <> 4.

      cl_aunit_assert=>fail(
       msg  = 'log handling error'    " Error Message
       quit = if_aunit_constants=>no  ).

    ENDIF.

    IF lines( lo_log->add( ls_sy )->get( )-t_bapi ) <> 5.

      cl_aunit_assert=>fail(
    msg    = 'log handling error'    " Error Message
   quit = if_aunit_constants=>no  ).

    ENDIF.

    IF lo_log->mt_log[ 5 ]-msgno <> '001'.

      cl_aunit_assert=>fail(
    msg    = 'log handling error'    " Error Message
   quit = if_aunit_constants=>no  ).

    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_unit_test_msg_mapper DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test_type FOR TESTING RAISING cx_static_check.
    METHODS test_element FOR TESTING RAISING cx_static_check.
    METHODS test_struct FOR TESTING RAISING cx_static_check.
    METHODS test_tab FOR TESTING RAISING cx_static_check.
    METHODS test_obj FOR TESTING RAISING cx_static_check.
    METHODS test_msg FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_unit_test_msg_mapper IMPLEMENTATION.

  METHOD test_type.

    DATA(lo_msg) = NEW lcl_help_msg_mapper( ).

    sy-msgty = 'E'.
    IF lo_msg->get( sy )-is_error = abap_false.

      cl_aunit_assert=>fail(
        msg    = 'GET: is_error flag wrong'    " Error Message
        quit = if_aunit_constants=>no  ).

    ENDIF.

    sy-msgty = 'S'.
    IF lo_msg->get( sy )-type <> 'S'.

      cl_aunit_assert=>fail(
        msg    = 'GET: type S not found'    " Error Message
        quit = if_aunit_constants=>no  ).

    ENDIF.

    IF lo_msg->get( sy )-is_error = abap_true.

      cl_aunit_assert=>fail(
        msg    = 'GET: is_error flag wrong'    " Error Message
        quit = if_aunit_constants=>no  ).

    ENDIF.

    sy-msgty = 'E'.
    DATA(lt_bapi) = VALUE bapiret2_tab( ).
    INSERT lo_msg->get( sy )-s_bapi INTO TABLE lt_bapi.
    sy-msgty = 'S'.
    INSERT lo_msg->get( sy )-s_bapi INTO TABLE lt_bapi.
    IF lo_msg->get( lt_bapi )-is_error = abap_false.

      cl_aunit_assert=>fail(
        msg    = 'GET: is_error flag wrong'    " Error Message
        quit = if_aunit_constants=>no  ).

    ENDIF.

    sy-msgty = 'W'.
    CLEAR lt_bapi.
    INSERT lo_msg->get( sy )-s_bapi INTO TABLE lt_bapi.
    sy-msgty = 'I'.
    INSERT lo_msg->get( sy )-s_bapi INTO TABLE lt_bapi.
    IF lo_msg->get( lt_bapi )-type <> 'W'.

      cl_aunit_assert=>fail(
        msg    = 'GET: type W is wrong'    " Error Message
        quit = if_aunit_constants=>no  ).

    ENDIF.

  ENDMETHOD.

  METHOD test_struct.
    DATA(lo_msg) = NEW lcl_help_msg_mapper( ).

    DATA(ls_sy_copy) = sy.
    DATA(ls_bapi) = lo_msg->get( ls_sy_copy )-s_bapi.

    DATA(ls_sy) = VALUE sy( ).
    lo_msg->main(
      EXPORTING
        input  = ls_bapi
      IMPORTING
        result = ls_sy
    ).
    IF ls_sy-msgid <> ls_sy_copy-msgid.
      cl_aunit_assert=>fail(
         msg    = 'MAP struct: msgid wrong'    " Error Message
         quit = if_aunit_constants=>no  ).
    ENDIF.

    ls_sy-msgid = '398'.
    IF ls_sy-msgid <> lo_msg->get( ls_sy )-s_bapi-id.

      cl_aunit_assert=>fail(
      msg    = 'GET struct: msgid wrong'    " Error Message
      quit = if_aunit_constants=>no  ).

    ENDIF.

    ls_sy-msgno = '123'.
    IF ls_sy-msgno <> lo_msg->get( ls_sy )-s_bapi-number.

      cl_aunit_assert=>fail(
      msg    = 'GET struct: number wrong'    " Error Message
      quit = if_aunit_constants=>no  ).

    ENDIF.


  ENDMETHOD.

  METHOD test_element.

    DATA(lo_msg) = NEW lcl_help_msg_mapper( ).

    IF lo_msg->get( 'das ist ein text' )-message = 'das ist ein test'.
      cl_aunit_assert=>fail(
         msg    = 'get element: type wrong'    " Error Message
         quit = if_aunit_constants=>no  ).
    ENDIF.

  ENDMETHOD.

  METHOD test_msg.

    DATA(lo_msg) = NEW lcl_help_msg_mapper( ).
    DATA(lv_msgid) = 'MSGCLASS' .
    DATA(ls_msg) = lo_msg->get_by_msg( id = lv_msgid ).

    IF ls_msg-s_bapi-id <> lv_msgid.

      cl_aunit_assert=>fail(
            msg    = 'get_msg: id wrong'    " Error Message
            quit = if_aunit_constants=>no  ).

    ENDIF.

    MESSAGE e398(00) WITH 'V1' 'V2' 'V3' 'V4' INTO DATA(lv_dummy).
    DATA(ls_sy) = sy.

    IF lo_msg->get( ls_sy )-s_bapi-message_v1 <> 'V1'.

      cl_aunit_assert=>fail(
           msg    = 'message info wrong'    " Error Message
           quit = if_aunit_constants=>no  ).


    ENDIF.

    IF lo_msg->get( ls_sy )-s_bapi-message_v2 <> 'V2'.

      cl_aunit_assert=>fail(
         msg    = 'message info wrong'    " Error Message
         quit = if_aunit_constants=>no  ).

    ENDIF.

    IF lo_msg->get( ls_sy )-s_bapi-message_v3 <> 'V3'.

      cl_aunit_assert=>fail(
         msg    = 'message info wrong'    " Error Message
         quit = if_aunit_constants=>no  ).

    ENDIF.

    IF lo_msg->get( ls_sy )-s_bapi-message_v4 <> 'V4'.

      cl_aunit_assert=>fail(
         msg    = 'message info wrong'    " Error Message
         quit = if_aunit_constants=>no  ).

    ENDIF.

    IF lo_msg->get( ls_sy )-message <> 'V1 V2 V3 V4'.

      cl_aunit_assert=>fail(
         msg    = 'message info wrong'    " Error Message
         quit = if_aunit_constants=>no  ).

    ENDIF.

    IF lo_msg->get( ls_sy )-type <> 'E'.

      cl_aunit_assert=>fail(
         msg    = 'message info wrong'    " Error Message
         quit = if_aunit_constants=>no  ).

    ENDIF.

    IF lo_msg->get( ls_sy )-is_error <> abap_true.

      cl_aunit_assert=>fail(
         msg    = 'message info wrong'    " Error Message
         quit = if_aunit_constants=>no  ).

    ENDIF.

    IF lo_msg->get( ls_sy )-s_bapi-id = '398'.

      cl_aunit_assert=>fail(
         msg    = 'message info wrong'    " Error Message
         quit = if_aunit_constants=>no  ).

    ENDIF.

  ENDMETHOD.

  METHOD test_tab.

    DATA lt_t100 TYPE STANDARD TABLE OF t100.
    DATA lt_bapi TYPE bapiret2_tab.
    DATA lt_bapi1 TYPE bapirettab.
    DATA lt_sy TYPE STANDARD TABLE OF sy.
    DATA lt_sy_copy TYPE STANDARD TABLE OF sy.

    MESSAGE e398(00) WITH 'V1' 'V2' INTO DATA(lv_dummy).
    DATA(ls_sy) = sy.
    INSERT ls_sy INTO TABLE lt_sy.
    INSERT ls_sy INTO TABLE lt_sy.

    lt_sy_copy = lt_sy.

    NEW lcl_help_msg_mapper( )->main(
      EXPORTING
        input           = lt_sy
      IMPORTING
        result          = lt_bapi
    ).

    NEW lcl_help_msg_mapper( )->main(
     EXPORTING
       input           = lt_bapi
     IMPORTING
       result          = lt_bapi1
   ).

    NEW lcl_help_msg_mapper( )->main(
     EXPORTING
       input           = lt_bapi1
     IMPORTING
       result          = lt_sy
   ).

    IF lt_sy[ 2 ]-msgv1 <> lt_sy_copy[ 2 ]-msgv1 OR
      lt_sy[ 2 ]-msgv2 <> lt_sy_copy[ 2 ]-msgv2 OR
      lt_sy[ 2 ]-msgv3 <> lt_sy_copy[ 2 ]-msgv3 OR
      lt_sy[ 2 ]-msgv4 <> lt_sy_copy[ 2 ]-msgv4 OR
      lt_sy[ 2 ]-msgid <> lt_sy_copy[ 2 ]-msgid OR
      lt_sy[ 2 ]-msgno <> lt_sy_copy[ 2 ]-msgno
      .

      cl_aunit_assert=>fail(
         msg    = 'message info wrong'    " Error Message
         quit = if_aunit_constants=>no  ).

    ENDIF.

    NEW lcl_help_msg_mapper( )->main(
     EXPORTING
       input           = lt_sy
     IMPORTING
       result          = lt_t100
   ).

    NEW lcl_help_msg_mapper( )->main(
     EXPORTING
       input           = lt_t100
     IMPORTING
       result          = lt_sy
   ).

    IF lt_sy[ 2 ]-msgid <> lt_sy[ 2 ]-msgid OR
      lt_sy[ 2 ]-msgno <> lt_sy[ 2 ]-msgno
      .

      cl_aunit_assert=>fail(
         msg    = 'message info wrong'    " Error Message
         quit = if_aunit_constants=>no  ).

    ENDIF.


  ENDMETHOD.

  METHOD test_obj.

    TRY.

        RAISE EXCEPTION NEW cx_t100_msg(
          t100_msgid = '00'
          t100_msgno = '398'
          t100_msgv1 = 'V1'
          t100_msgv2 = 'V2'
          t100_msgv3 = 'V3'
          t100_msgv4 = 'V4'
        ).

      CATCH cx_root INTO DATA(lx).

        MESSAGE e398(00) WITH 'V1' 'V2' 'V3' 'V4' INTO DATA(lv_dummy).
        DATA(ls_sy) = sy.
        DATA(ls_sy_copy) = ls_sy.

        DATA(ls_bapi) = NEW lcl_help_msg_mapper( )->get( lx )-s_bapi.

        NEW lcl_help_msg_mapper( )->main(
          EXPORTING
            input           = ls_bapi
*    io_typedescr_in =
          IMPORTING
            result          = ls_sy
        ).

        IF
            ls_sy-msgid <> ls_sy_copy-msgid OR
            ls_sy-msgno <> ls_sy_copy-msgno.
*            ls_sy-msgv1 <> ls_sy_copy-msgv1.
          .
          cl_aunit_assert=>fail(
             msg    = 'message info wrong'    " Error Message
             quit = if_aunit_constants=>no  ).
        ENDIF.

    ENDTRY.


    TRY.
        MESSAGE e398(00) WITH 'V1' 'V2' 'V3' 'V4' INTO lv_dummy.
        RAISE EXCEPTION TYPE cx_business_place USING MESSAGE.


      CATCH cx_root INTO lx.

        MESSAGE e398(00) WITH 'V1' 'V2' 'V3' 'V4' INTO lv_dummy.
        ls_sy = sy.
        ls_sy_copy = ls_sy.

        ls_bapi = NEW lcl_help_msg_mapper( )->get( lx )-s_bapi.

        NEW lcl_help_msg_mapper( )->main(
          EXPORTING
            input           = ls_bapi
*    io_typedescr_in =
          IMPORTING
            result          = ls_sy
        ).

        IF
               ls_sy-msgid <> ls_sy_copy-msgid OR
               ls_sy-msgno <> ls_sy_copy-msgno OR
               ls_sy-msgv1 <> ls_sy_copy-msgv1 OR
                ls_sy-msgv2 <> ls_sy_copy-msgv2 OR
                 ls_sy-msgv3 <> ls_sy_copy-msgv3 OR
                  ls_sy-msgv4 <> ls_sy_copy-msgv4
                  .

          cl_aunit_assert=>fail(
             msg    = 'message info wrong'    " Error Message
             quit = if_aunit_constants=>no  ).
        ENDIF.

    ENDTRY.


    TRY.
* MESSAGE e398(00) WITH 'V1' 'V2' 'V3' 'V4' INTO lv_dummy.

        RAISE EXCEPTION TYPE  cx_business_place MESSAGE ID '00' NUMBER '398' WITH 'V1' 'V2' 'V3' 'V4'.


      CATCH cx_root INTO lx.

        MESSAGE e398(00) WITH 'V1' 'V2' 'V3' 'V4' INTO lv_dummy.
        ls_sy = sy.
        ls_sy_copy = ls_sy.

        ls_bapi = NEW lcl_help_msg_mapper( )->get( lx )-s_bapi.

        NEW lcl_help_msg_mapper( )->main(
          EXPORTING
            input           = ls_bapi
*    io_typedescr_in =
          IMPORTING
            result          = ls_sy
        ).

        IF
             ls_sy-msgid <> ls_sy_copy-msgid OR
             ls_sy-msgno <> ls_sy_copy-msgno OR
             ls_sy-msgv1 <> ls_sy_copy-msgv1 OR
              ls_sy-msgv2 <> ls_sy_copy-msgv2 OR
               ls_sy-msgv3 <> ls_sy_copy-msgv3 OR
                ls_sy-msgv4 <> ls_sy_copy-msgv4
                .

          cl_aunit_assert=>fail(
             msg    = 'message info wrong'    " Error Message
             quit = if_aunit_constants=>no  ).
        ENDIF.

    ENDTRY.

    TRY.
* MESSAGE e398(00) WITH 'V1' 'V2' 'V3' 'V4' INTO lv_dummy.

        RAISE EXCEPTION TYPE cx_edm_call_error
          EXPORTING
            textid = VALUE #(
                msgid = '00'
                msgno = '398'
                attr1 = 'V1'
                attr2 = 'V2'
                attr3 = 'V3'
                attr4 = 'V4'
            ).

      CATCH cx_root INTO lx.

        MESSAGE e398(00) WITH 'V1' 'V2' 'V3' 'V4' INTO lv_dummy.
        ls_sy = sy.
        ls_sy_copy = ls_sy.

        ls_bapi = NEW lcl_help_msg_mapper( )->get( lx )-s_bapi.

        NEW lcl_help_msg_mapper( )->main(
          EXPORTING
            input           = ls_bapi
*    io_typedescr_in =
          IMPORTING
            result          = ls_sy
        ).

        IF
          ls_sy-msgid <> ls_sy_copy-msgid OR
          ls_sy-msgno <> ls_sy_copy-msgno OR
          ls_sy-msgv1 <> ls_sy_copy-msgv1.
          .

          cl_aunit_assert=>fail(
             msg    = 'message info wrong'    " Error Message
             quit = if_aunit_constants=>no  ).
        ENDIF.

    ENDTRY.


  ENDMETHOD.

ENDCLASS.
