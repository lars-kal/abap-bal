class zls_cl_log definition
  public
  final
  create public .

public section.

  interfaces IF_SERIALIZABLE_OBJECT .

    types ty_o_me type ref to zls_cl_log.

  types:
    BEGIN OF ty_S_log,
        is_db_entry TYPE abap_bool,
        type        TYPE msgty,
        date        TYPE sydatum,
        time        TYPE syuzeit,
        message     TYPE bapiret2-message,
        tstmp       TYPE timestampl,
        msgid       TYPE msgid,
        msgno       TYPE msgno,
        v1          TYPE bapiret2-message_v1,
        v2          TYPE bapiret2-message_v2,
        v3          TYPE bapiret2-message_v3,
        v4          TYPE bapiret2-message_v4,
      END OF ty_S_log .
  types:
    ty_T_log TYPE STANDARD TABLE OF ty_S_log WITH EMPTY KEY .
  types:
    BEGIN OF ty_S_result_get_msg,
        is_error TYPE abap_bool,
        type     TYPE string,
        t_bapi   TYPE bapiret2_tab,
      END OF ty_S_result_get_msg .

  data MS_BALHDR type BALHDR read-only .
  data MT_LOG type TY_T_LOG read-only .

    class-data:
      begin of cs_default,
        object type string value 'ZKAL',
        subobject type string value '001',
        days_delete type i value 30,
      end of cs_default.

  methods CONSTRUCTOR
    importing
      !IV_OBJECT    type clike default cs_default-object
      !IV_SUBOBJECT type clike default cs_default-subobject.

    "! <p class="shorttext synchronized" lang="de">sy/bapiret/bapiret2/exception</p>
  methods add
    importing
      value(I_ANY) type ANY
    returning
      value(RESULT) type ty_o_me  .
    "! <p class="shorttext synchronized" lang="de">add message</p>
    "!
    "! @parameter iv_msgty     | <p class="shorttext synchronized" lang="de"></p>
    "! @parameter iv_msgno     | <p class="shorttext synchronized" lang="de"></p>
  methods ADD_MSG
    importing
      !IV_MSGTY type CLIKE
      !IV_MSGID type CLIKE
      !IV_MSGNO type CLIKE
      !IV_MSGV1 type CLIKE optional
      !IV_MSGV2 type CLIKE optional
      !IV_MSGV3 type CLIKE optional
      !IV_MSGV4 type CLIKE optional
    returning
      value(RESULT) type ty_o_me  .
    "! <p class="shorttext synchronized" lang="de">add tag</p>
    "!
    "! @parameter iv_ident | <p class="shorttext synchronized" lang="de">Anwendungs-Log: Parameter</p>
  methods add_tag
    importing
      !IV_IDENT type BALPAR
      !I_ANY type ANY .
    "! <p class="shorttext synchronized" lang="de">get messages and info</p>
  methods get
    returning
      value(RESULT) type TY_S_RESULT_GET_MSG .
    "! <p class="shorttext synchronized" lang="de">save to bal database</p>
  methods DB_SAVE
    returning
      value(RESULT) type ty_o_me.
    "! <p class="shorttext synchronized" lang="de">create log with entries from bal database</p>
  class-methods FACTORY_BY_BAL
    importing
      !IV_OBJECT type clike    default cs_default-object
      !IV_SUBOBJECT type clike default cs_default-subobject
      !IV_EXTNUMBER type CLIKE optional
      !IV_LOGNUMBER type CLIKE optional
    preferred parameter IV_EXTNUMBER
    returning
      value(RESULT) type ty_o_me.
    "! <p class="shorttext synchronized" lang="en"> do not use!!! use method db_save instead</p>
  class-methods _RFC_CALL_SAVE
    importing
      !IV_LOG_AS_XML type STRING .

  PROTECTED SECTION.

    "! <p class="shorttext synchronized" lang="en">Anwendungs-Log: Tabelle mit Parametern zu Protokollmeldungen</p>
    DATA mt_tag TYPE bal_t_par.
    DATA mt_stack TYPE abap_callstack.

    METHODS tag_save.
    METHODS tag_cleanup.

    METHODS bal_load.
    METHODS bal_save.

    METHODS init
      IMPORTING
        iv_subobject TYPE any
        iv_object    TYPE any
        iv_extnumber TYPE any OPTIONAL
        iv_lognumber TYPE any OPTIONAL.

private section.
ENDCLASS.



CLASS zls_cl_log IMPLEMENTATION.

  METHOD add.
    TRY.
        result = me.

        DATA(lt_log) = VALUE ty_t_log( ).
        NEW lcl_help_msg_mapper( )->main(
          EXPORTING
            input           = i_any
          IMPORTING
            result          = lt_log
        ).

        DATA(ls_time) = lcl_help=>get_time( ).

        LOOP AT lt_log INTO DATA(ls_log).

          ls_log-tstmp   = ls_time-tstmp.
          ls_log-date    = ls_time-date.
          ls_log-time    = ls_time-time.
          ls_log-message = lcl_help=>msg( ls_log )-message.

          INSERT ls_log INTO TABLE mt_log.
        ENDLOOP.

      CATCH cx_root.
        "this log will not interrupt you
    ENDTRY.
  ENDMETHOD.


  METHOD add_tag.
    TRY.

        DATA: ls_balmp TYPE bal_s_par.

        DATA(lo_descr) = cl_abap_typedescr=>describe_by_data( i_any ).

        CASE lo_descr->kind.
          WHEN cl_abap_typedescr=>kind_elem.

            ls_balmp-parname = iv_ident.
            ls_balmp-parvalue = CONV #( i_any ).
            APPEND ls_balmp TO mt_tag.

          WHEN cl_abap_typedescr=>kind_struct.

            ASSIGN COMPONENT iv_ident OF STRUCTURE i_any TO FIELD-SYMBOL(<fs_value>).
            IF <fs_value> IS ASSIGNED.
              ls_balmp-parname = iv_ident.
              ls_balmp-parvalue = CONV #( <fs_value> ).
              APPEND ls_balmp TO mt_tag.
            ENDIF.

          WHEN cl_abap_typedescr=>kind_table.

            FIELD-SYMBOLS: <fs_table> TYPE STANDARD TABLE.

            ASSIGN i_any TO <fs_table>.

            LOOP AT <fs_table> ASSIGNING FIELD-SYMBOL(<fs_line>).
              ASSIGN COMPONENT iv_ident OF STRUCTURE <fs_line> TO <fs_value>.
              IF <fs_value> IS ASSIGNED.
                ls_balmp-parname = iv_ident.
                ls_balmp-parvalue = CONV #( <fs_value> ).
                APPEND ls_balmp TO mt_tag.
              ENDIF.
            ENDLOOP.

        ENDCASE.

      CATCH cx_root.
        "this log will not interrupt you
    ENDTRY.
  ENDMETHOD.


  METHOD add_msg.
    TRY.
        result = me.

        add( VALUE bapiret2(
           type       = CONV #( iv_msgty )
           id         = CONV #( iv_msgid )
           number     = CONV #( iv_msgno )
           message_v1 = CONV #( iv_msgv1 )
           message_v2 = CONV #( iv_msgv2 )
           message_v3 = CONV #( iv_msgv3 )
           message_v4 = CONV #( iv_msgv4 )
          ) ).


      CATCH cx_root.
        "this log will not interrupt you
    ENDTRY.
  ENDMETHOD.

  METHOD bal_load.

    DATA:
      ls_extn   TYPE bal_s_extn,
      ls_sub    TYPE bal_s_sub,
      ls_obj    TYPE bal_s_obj,
      ls_filt   TYPE bal_s_lfil,
      ls_logn   TYPE bal_s_logn,
      lt_header TYPE balhdr_t,
      ls_header TYPE LINE OF balhdr_t.

    ls_extn-sign   = ls_sub-sign   = ls_obj-sign   = ls_logn-sign   =  'I'.
    ls_extn-option = ls_sub-option = ls_obj-option = ls_logn-option = 'EQ'.

    ls_obj-low  = ms_balhdr-object    .
    ls_sub-low  = ms_balhdr-subobject .
    ls_extn-low = ms_balhdr-extnumber.
    ls_logn-low = ms_balhdr-lognumber.

    IF ms_balhdr-extnumber IS NOT INITIAL.
      APPEND: ls_extn TO ls_filt-extnumber.
    ENDIF.

    APPEND ls_obj TO ls_filt-object.
    APPEND ls_sub TO ls_filt-subobject.

    IF ms_balhdr-lognumber IS NOT INITIAL.
      APPEND ls_logn TO ls_filt-lognumber.
    ENDIF.

    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING
        i_s_log_filter = ls_filt    " Log header data filter
      IMPORTING
        e_t_log_header = lt_header    " Table of log header data read
      EXCEPTIONS
        error_message  = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF lines( lt_header ) <> 1.
      RETURN.
    ENDIF.

    DATA lt_lognumber TYPE STANDARD TABLE OF szal_lognumber.

    LOOP AT lt_header INTO ls_header.
      APPEND ls_header-lognumber TO lt_lognumber.
    ENDLOOP.

    DATA lt_message TYPE STANDARD TABLE OF balm.
    DATA lt_parms   TYPE STANDARD TABLE OF balmp.

    CALL FUNCTION 'APPL_LOG_READ_DB_WITH_LOGNO'
      TABLES
        lognumbers         = lt_lognumber
        messages           = lt_message    " Log messages
        message_parameters = lt_parms   " Message parameters
      EXCEPTIONS
        error_message      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_message INTO DATA(ls_bal).

      DATA(ls_log) = VALUE ty_S_log(  ).

      new lcl_help_msg_mapper( )->main(
        EXPORTING
          input           = ls_bal
        IMPORTING
          result          = ls_log
      ).

      ls_log-type = ls_bal-msgty.
      ls_log-tstmp = ls_bal-time_stmp.

      DATA(ls_time) = lcl_help=>get_time( ls_bal-time_stmp ).
      ls_log-date = ls_time-date.
      ls_log-time = ls_time-time.

      ls_log-message = lcl_help=>msg( ls_log )-message.
      ls_log-is_db_entry = abap_true.
      INSERT ls_log INTO TABLE mt_log.

    ENDLOOP.

  ENDMETHOD.


  METHOD bal_save.

    DATA lt_log_header TYPE balhdr_t.

    DATA(ls_filter) = VALUE bal_s_lfil(
        object    = VALUE bal_r_obj( ( sign = 'I' option = 'EQ' low = ms_balhdr-object ) )
        subobject = VALUE bal_r_sub( ( sign = 'I' option = 'EQ' low = ms_balhdr-subobject ) )
        extnumber = VALUE bal_r_extn( ( sign = 'I' option = 'EQ' low =  ms_balhdr-extnumber ) )
         ).

    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING
        i_s_log_filter = ls_filter
      IMPORTING
        e_t_log_header = lt_log_header
      EXCEPTIONS
        error_message  = 1
        OTHERS         = 2.
    IF sy-subrc = 0.

      ms_balhdr = lt_log_header[ 1 ]. "log_handle.

      CALL FUNCTION 'BAL_DB_LOAD'
        EXPORTING
          i_t_log_handle         = VALUE bal_t_logh( ( ms_balhdr-log_handle ) )
          i_do_not_load_messages = abap_true
          i_lock_handling        = 1
        EXCEPTIONS
          error_message          = 1
          OTHERS                 = 2.

    ELSE.

      CALL FUNCTION 'BAL_LOG_CREATE'
        EXPORTING
          i_s_log       = CORRESPONDING bal_s_log( ms_balhdr )
        IMPORTING
          e_log_handle  = ms_balhdr-log_handle
        EXCEPTIONS
          error_message = 1
          OTHERS        = 2.

    ENDIF.


    LOOP AT mt_log INTO DATA(ls_log)
        WHERE is_db_entry = abaP_false.

      DATA(ls_bal) = VALUE bal_s_msg(  ).

      NEW lcl_help_msg_mapper( )->main(
        EXPORTING
          input           = ls_log
        IMPORTING
          result          = ls_bal
      ).
      ls_bal-time_stmp = ls_log-tstmp.
*      ls_bal-probclass = ms_cust-detlevel.

*--Meldung in Log...
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle     = ms_balhdr-log_handle
          i_s_msg          = ls_bal
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          error_message    = 4
          OTHERS           = 5.
      IF sy-subrc <> 0.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_in_update_task = ''
        i_save_all       = ''
        i_t_log_handle   = VALUE bal_t_logh( ( ms_balhdr-log_handle ) )
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        error_message    = 4
        OTHERS           = 5.

  ENDMETHOD.


  METHOD constructor.
    TRY.

        init(
         iv_extnumber = lcl_help=>get_guid16( )
         iv_subobject = iv_subobject
         iv_object    = iv_object ).

      CATCH cx_root.
        "this log will not interrupt you
    ENDTRY.
  ENDMETHOD.


  METHOD tag_cleanup.
    TRY.

        DATA lt_del TYPE RANGE OF balhdr-extnumber.

        "all entries of /zrg/ with no reference to balhdr
        SELECT FROM
           zls_t_log as log
        LEFT OUTER JOIN
           balhdr ON
            balhdr~extnumber = log~extnumber
        FIELDS
          'I' AS sign,
          'EQ' AS option,
          log~extnumber AS low
        WHERE
          log~extnumber IS NOT INITIAL AND
          balhdr~extnumber IS NULL
        INTO TABLE @lt_del.

        IF lt_del IS NOT INITIAL.
          DELETE FROM zls_t_log WHERE extnumber IN lt_del.
        ENDIF.

      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD db_save.
    TRY.
        result = me.

        "check customizing -> database log active?
*        CHECK ms_cust-active = abap_true.
        CHECK line_exists( mt_log[ is_db_entry = abap_false ] ).

        "save stack for search functions
        mt_stack = lcl_help=>get_callstack( 1  ).
        SORT mt_stack BY mainprogram blockname.
        DELETE ADJACENT DUPLICATES FROM mt_stack COMPARING mainprogram blockname.

        DATA(lv_log_as_xml) = lcl_help=>xml_obj_2_string( me ).
        DATA(lv_task)       = CONV string( lcl_help=>get_time( )-tstmp ).
        DATA(lv_trfcqnam)   = CONV trfcqnam( 'ZRG_' && ms_balhdr-subobject ).

        CALL FUNCTION 'ZLS_FM_01' "STARTING NEW TASK lv_task
          DESTINATION 'NONE'
          EXPORTING
            ij_objekt   = lv_log_as_xml
            iv_trfcqnam = lv_trfcqnam.

        "mark messages saved to database
        LOOP AT mt_log ASSIGNING FIELD-SYMBOL(<ls_log>).
          <ls_log>-is_db_entry = abap_true.
        ENDLOOP.

      CATCH cx_root.
        "this log will not interrupt you
    ENDTRY.
  ENDMETHOD.


  METHOD tag_save.
    DATA ls_tag TYPE zls_t_log.
    DATA: ls_t003 TYPE zls_t_log." Additional Identifier for Ballog
    DATA: lt_t003 TYPE STANDARD TABLE OF zls_t_log." Additional Identifier for Ballog

    LOOP AT me->mt_tag INTO DATA(ls_balmp).
      ls_tag-tagdata = ls_tag-tagdata && ls_balmp-parname &&  ls_balmp-parvalue.
      APPEND ls_t003 TO lt_t003.
    ENDLOOP.

    "create tags to activate database search functionality
    "msgid msgno..
    LOOP AT mt_log INTO DATA(ls_log).
      ls_tag-tagmsg = ls_tag-tagmsg  && ls_log-type && ls_log-msgno && '(' && ls_log-msgid && ')'.
    ENDLOOP.

    "create tags to activate database search functionality
    "called program, stack ...
    LOOP AT mt_stack INTO DATA(ls_stack).
      ls_tag-tagstack = ls_tag-tagstack && ls_stack-mainprogram && ls_stack-blockname.
*      replace '=' in ls_tag-tagstack with ''.
*      ls_tag-tagstack = replace( val = ls_tag-tagstack sub = '=' with = '' ).
*      replace '=' with '' into ls_tag-tagstack.
    ENDLOOP.

    ls_tag-id = lcl_help=>get_guid16( ).
    ls_tag-extnumber = ms_balhdr-extnumber.
*    ls_tag-parname = 'TAG'.
*    ls_tag-parvalue = 'TAG'.
    ls_tag-tagdata = ls_tag-tagdata && SWITCH string( me->get( )-type
      WHEN 'E' THEN 'ERROR'
      WHEN 'S' THEN 'SUCCESS'  ).

    INSERT ls_tag INTO TABLE lt_t003.

    DELETE FROM zls_t_log WHERE extnumber = ms_balhdr-extnumber.
    MODIFY zls_t_log FROM TABLE lt_t003.

  ENDMETHOD.


  METHOD factory_by_bal.

    result = NEW #( ).

    result->init(
      iv_subobject = iv_subobject
      iv_object    = iv_object
      iv_extnumber = iv_extnumber
      iv_lognumber = iv_lognumber
    ).

    result->bal_load( ).

  ENDMETHOD.


  METHOD get.
    TRY.

        result-type = COND #(
            WHEN line_exists( mt_log[ type = 'A' ] ) THEN 'A'
            WHEN line_exists( mt_log[ type = 'X' ] ) THEN 'X'
            WHEN line_exists( mt_log[ type = 'E' ] ) THEN 'E'
            WHEN line_exists( mt_log[ type = 'W' ] ) THEN 'W'
            WHEN line_exists( mt_log[ type = 'S' ] ) THEN 'S'
             ).

        result-is_error = COND #( WHEN result-type CA 'EAX' THEN abap_true ).

        LOOP AT mt_log INTO DATA(ls_log).
          INSERT lcl_help=>msg( ls_log )-s_bapi INTO TABLE result-t_bapi.
        ENDLOOP.

      CATCH cx_root.
        "this log will not interrupt you
    ENDTRY.
  ENDMETHOD.


  METHOD init.
    TRY.

        CLEAR mt_log.
        CLEAR ms_balhdr.
*        CLEAR ms_cust.

        DATA(ls_time) = lcl_help=>get_time( ).
        ms_balhdr-aldate = ls_time-date.
        ms_balhdr-altime = ls_time-time.

        ms_balhdr-aluser    = sy-uname.
        ms_balhdr-altcode   = sy-tcode.
        ms_balhdr-alprog    = sy-cprog.

        ms_balhdr-object    = iv_object.
        ms_balhdr-subobject = iv_subobject.
        ms_balhdr-extnumber = iv_extnumber.

        ms_balhdr-lognumber = iv_lognumber.

        ms_balhdr-aldate_del = sy-datlo + cs_default-days_delete.
*         COND #(
*            WHEN ms_cust-dele_duration IS NOT INITIAL THEN ms_cust-dele_duration
*            ELSE 30 ).

      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD _rfc_call_save.
    TRY.

        DATA(lo_log) = CAST zls_cl_log( lcl_help=>xml_string_2_obj( iv_log_as_xml ) ).

        "transport to database
        lo_log->bal_save( ).
        lo_log->tag_save( ).

        lo_log->tag_cleanup( ).

        COMMIT WORK AND WAIT.



      CATCH cx_root.
        "handle exception
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
