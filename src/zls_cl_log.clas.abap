CLASS zls_cl_log DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! log as exception
    DATA mx TYPE REF TO zls_cx_log.

    CONSTANTS:
      BEGIN OF ss_default,
        object      TYPE string VALUE 'ZSTC',
        subobject   TYPE string VALUE 'DEFAULT',
        extnumber   TYPE string VALUE '',
        description TYPE string VALUE '',
        days_delete TYPE i VALUE 90,
      END OF ss_default.

    INTERFACES if_serializable_object .

    TYPES ty_o_me TYPE REF TO zls_cl_log.

    TYPES:
      BEGIN OF ty_s_log,
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
      END OF ty_s_log .
    TYPES:
      ty_t_log TYPE STANDARD TABLE OF ty_s_log WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_s_result_get_msg,
        is_error     TYPE abap_bool,
        type         TYPE string,
        s_bapi_last  type bapiret2,
        t_bapi       TYPE bapiret2_tab,
      END OF ty_s_result_get_msg .

*    class-data:
*      begin of ty_S_in_factory,
*        iv_object      TYPE clike DEFAULT ss_default-object
*        iv_subobject   TYPE clike DEFAULT ss_default-subobject
*        iv_extnumber   TYPE clike OPTIONAL
*        iv_description TYPE clike OPTIONAL

    CONSTANTS:
      BEGIN OF cs_gui_action,
        popup           TYPE string VALUE 'POPUP',
        new_window_slg1 TYPE string VALUE 'NEW_WINDOW_SLG1',
        docking         TYPE string VALUE 'DOCKING',
      END OF cs_gui_action.

    DATA ms_balhdr TYPE balhdr READ-ONLY .
    DATA mt_log TYPE ty_t_log READ-ONLY .


    "! <p class="shorttext synchronized" lang="en">sy/bapiret/bapiret2/exception</p>
    METHODS add
      IMPORTING
        VALUE(val)    TYPE any
        VALUE(ty)     TYPE any OPTIONAL
        !v1           TYPE clike OPTIONAL
        !v2           TYPE clike OPTIONAL
        !v3           TYPE clike OPTIONAL
        !v4           TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_o_me  .

    METHODS add_sy
      RETURNING
        VALUE(result) TYPE ty_o_me  .

    "! <p class="shorttext synchronized" lang="en">add message</p>
    "!
    "! @parameter ty     | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter no     | <p class="shorttext synchronized" lang="en"></p>
    METHODS add_msg
      IMPORTING
        !ty           TYPE clike
        !id           TYPE clike
        !no           TYPE clike
        !v1           TYPE clike OPTIONAL
        !v2           TYPE clike OPTIONAL
        !v3           TYPE clike OPTIONAL
        !v4           TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_o_me  .
    "! <p class="shorttext synchronized" lang="en">add tag</p>
    "!
    "! @parameter name | <p class="shorttext synchronized" lang="en">Anwendungs-Log: Parameter</p>
    METHODS add_tag
      IMPORTING
        !name TYPE any
        !val  TYPE any .

    METHODS check_msg_error
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! <p class="shorttext synchronized" lang="en">get messages and info</p>
    METHODS get
      RETURNING
        VALUE(result) TYPE ty_s_result_get_msg .

    "! <p class="shorttext synchronized" lang="en">get ballog identifier</p>
    METHODS get_id
      RETURNING
        VALUE(result) TYPE string.



    "! <p class="shorttext synchronized" lang="en">save to bal database</p>
    METHODS db_save
      RETURNING
        VALUE(result) TYPE ty_o_me.


    METHODS constructor
      IMPORTING
        ix    TYPE REF TO zls_cx_log OPTIONAL
        is_in LIKE ss_default DEFAULT ss_default.
    "! <p class="shorttext synchronized" lang="en">sap gui display</p>
    "! @parameter iv_action | choose the way
    METHODS gui
      IMPORTING
        iv_action TYPE clike DEFAULT cs_gui_action-popup.

    "! <p class="shorttext synchronized" lang="en">create new log</p>
    "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS factory
      IMPORTING
        is_in         LIKE ss_default DEFAULT ss_default
*        iv_object      TYPE clike DEFAULT ss_default-object
*        iv_subobject   TYPE clike DEFAULT ss_default-subobject
*        iv_extnumber   TYPE clike OPTIONAL
*        iv_description TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_o_me.

    "! <p class="shorttext synchronized" lang="en">create log with entries from bal database</p>
    CLASS-METHODS factory_by_bal
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE ty_o_me.
    "! <p class="shorttext synchronized" lang="en"> do not use!!! use method db_save instead</p>
    CLASS-METHODS _rfc_call_save
      IMPORTING
        !iv_log_as_xml TYPE string.




  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_s_tag,
        is_db_entry TYPE abap_bool,
        name        TYPE string,
        value       TYPE string,
      END OF ty_s_tag.

    DATA mt_tag TYPE STANDARD TABLE OF ty_s_tag. "bal_t_par.
    DATA mt_stack TYPE abap_callstack.

    METHODS tag_save.
    METHODS tag_cleanup.


    METHODS bal_read_head
      IMPORTING
        i_val         TYPE any
      RETURNING
        VALUE(result) TYPE balhdr.
    METHODS bal_read_items.
    METHODS bal_save.

    METHODS init
      IMPORTING
        iv_object      TYPE any
        iv_subobject   TYPE any
        iv_extnumber   TYPE any OPTIONAL
        iv_days_delete TYPE i   OPTIONAL
        iv_lognumber   TYPE any OPTIONAL
        iv_description TYPE any OPTIONAL.


  PRIVATE SECTION.
ENDCLASS.



CLASS zls_cl_log IMPLEMENTATION.


  METHOD add.
    TRY.
        result = me.

        IF val IS INITIAL.
          RETURN.
        ENDIF.

        DATA(lt_log) = VALUE ty_t_log( ).
        NEW lcl_help_msg_mapper( )->main(
          EXPORTING
            input           = val
          IMPORTING
            result          = lt_log
        ).

        IF lt_log IS INITIAL.
          RETURN.
        ENDIF.



        DATA(ls_time) = lcl_help=>get_time( ).

        LOOP AT lt_log INTO DATA(ls_log).

          IF ty CA 'ES'.
            ls_log-type = ty.
          ENDIF.

          ls_log-tstmp   = ls_time-tstmp.
          ls_log-date    = ls_time-date.
          ls_log-time    = ls_time-time.
          ls_log-message = NEW lcl_help_msg_mapper( )->get( ls_log )-message.

        if v1 is not initial or v2 is not INITIAL or v3 is not initial or v4 is NOT INITIAL.
        data(ls_msg) = lcl_help_msg_mapper=>factory( )->get_by_text(
            val    = ls_log-message
            type   = ls_log-type
            v1     = v1
            v2     = v2
            v3     = v3
            v4     = v4 ).

          ls_log-msgid = ls_msg-s_bapi-id.
          ls_log-msgno = ls_msg-s_bapi-number.
          ls_log-v1    = ls_msg-s_bapi-message_v1.
          ls_log-v2    = ls_msg-s_bapi-message_v2.
          ls_log-v3    = ls_msg-s_bapi-message_v3.
          ls_log-v4    = ls_msg-s_bapi-message_v4.
          ls_log-message = ls_msg-s_bapi-message.

        endif.

          INSERT ls_log INTO TABLE mt_log.
        ENDLOOP.

      CATCH cx_root.
        "this log will not interrupt you
    ENDTRY.
  ENDMETHOD.


  METHOD add_msg.
    TRY.
        result = me.

        add( VALUE bapiret2(
           type       = CONV #( ty )
           id         = CONV #( id )
           number     = CONV #( no )
           message_v1 = CONV #( v1 )
           message_v2 = CONV #( v2 )
           message_v3 = CONV #( v3 )
           message_v4 = CONV #( v4 )
          ) ).


      CATCH cx_root.
        "this log will not interrupt you
    ENDTRY.
  ENDMETHOD.


  METHOD add_sy.

    DATA(ls_sy) = sy.
    result = add( ls_sy ).

  ENDMETHOD.


  METHOD add_tag.
    TRY.

        DATA: ls_balmp TYPE bal_s_par.

        DATA(lo_descr) = cl_abap_typedescr=>describe_by_data( val ).

        CASE lo_descr->kind.
          WHEN cl_abap_typedescr=>kind_elem.

            INSERT VALUE #(
                name  = name
                value = CONV #( val )
               ) INTO TABLE mt_tag.

          WHEN cl_abap_typedescr=>kind_struct.

            ASSIGN COMPONENT name OF STRUCTURE val TO FIELD-SYMBOL(<fs_value>).
            IF <fs_value> IS ASSIGNED.
              INSERT VALUE #(
               name  = name
               value = CONV #( <fs_value> )
              ) INTO TABLE mt_tag.
            ENDIF.

          WHEN cl_abap_typedescr=>kind_table.

            FIELD-SYMBOLS: <fs_table> TYPE STANDARD TABLE.

            ASSIGN val TO <fs_table>.

            LOOP AT <fs_table> ASSIGNING FIELD-SYMBOL(<fs_line>).
              ASSIGN COMPONENT name OF STRUCTURE <fs_line> TO <fs_value>.
              IF <fs_value> IS ASSIGNED.
                INSERT VALUE #(
                  name  = name
                  value = CONV #( <fs_value> )
                 ) INTO TABLE mt_tag.
              ENDIF.
            ENDLOOP.

        ENDCASE.

      CATCH cx_root.
        "this log will not interrupt you
    ENDTRY.
  ENDMETHOD.


  METHOD bal_read_head.
    TRY.

        DATA(lv_input) = shift_left( shift_right(  i_val ) ).
        IF lv_input IS INITIAL.
          RETURN.
        ENDIF.

        IF strlen( lv_input ) > 20.
          DATA(lt_r_id) = VALUE rseloption( ( sign = 'I' option = 'EQ' low = lv_input ) ).
        ELSE.
          DATA(lt_r_lognumber) = VALUE rseloption( ( sign = 'I' option = 'EQ' low = lv_input ) ).
        ENDIF.

        SELECT SINGLE *
        FROM balhdr
        INTO CORRESPONDING FIELDS OF result
        WHERE
         lognumber IN lt_r_lognumber AND
         userexitp IN lt_r_id.

      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD bal_read_items.
    TRY.

        DATA:
          ls_extn   TYPE bal_s_extn,
          ls_sub    TYPE bal_s_sub,
          ls_obj    TYPE bal_s_obj,
          ls_filt   TYPE bal_s_lfil,
          ls_logn   TYPE bal_s_logn,
          lt_header TYPE balhdr_t,
          ls_header TYPE LINE OF balhdr_t.

*    ls_extn-sign   = ls_sub-sign   = ls_obj-sign   = ls_logn-sign   =  'I'.
*    ls_extn-option = ls_sub-option = ls_obj-option = ls_logn-option = 'EQ'.
*
*    ls_obj-low  = ms_balhdr-object    .
*    ls_sub-low  = ms_balhdr-subobject .
*    ls_extn-low = ms_balhdr-extnumber.
*    ls_logn-low = ms_balhdr-lognumber.
*
*    IF ms_balhdr-extnumber IS NOT INITIAL.
*      APPEND: ls_extn TO ls_filt-extnumber.
*    ENDIF.
*
*    APPEND ls_obj TO ls_filt-object.
*    APPEND ls_sub TO ls_filt-subobject.
*
*    IF ms_balhdr-lognumber IS NOT INITIAL.
*      APPEND ls_logn TO ls_filt-lognumber.
*    ENDIF.
*
*    CALL FUNCTION 'BAL_DB_SEARCH'
*      EXPORTING
*        i_s_log_filter = ls_filt    " Log header data filter
*      IMPORTING
*        e_t_log_header = lt_header    " Table of log header data read
*      EXCEPTIONS
*        error_message  = 1
*        OTHERS         = 2.
*    IF sy-subrc <> 0.
*      RETURN.
*    ENDIF.
*
*    IF lines( lt_header ) <> 1.
*      RETURN.
*    ENDIF.
*
        DATA lt_lognumber TYPE STANDARD TABLE OF szal_lognumber.
*
*    LOOP AT lt_header INTO ls_header.
*      APPEND ls_header-lognumber TO lt_lognumber.
*    ENDLOOP.
*
        lt_lognumber = VALUE #(  ( CONV #(  ms_balhdr-lognumber  ) ) ).

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

          DATA(ls_log) = VALUE ty_s_log(  ).

          NEW lcl_help_msg_mapper( )->main(
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

          ls_log-message = NEW lcl_help_msg_mapper( )->get( ls_log )-message.
          ls_log-is_db_entry = abap_true.
          INSERT ls_log INTO TABLE mt_log.

        ENDLOOP.

      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD bal_save.

    IF ms_balhdr-log_handle IS INITIAL.

      ms_balhdr-log_handle = bal_read_head( SWITCH #( ms_balhdr-lognumber
          WHEN `` THEN ms_balhdr-userexitp
          ELSE ms_balhdr-lognumber )
           )-log_handle.

    ENDIF.

    IF ms_balhdr-log_handle IS NOT INITIAL.

      CALL FUNCTION 'BAL_DB_LOAD'
        EXPORTING
          i_t_log_handle         = VALUE bal_t_logh( ( ms_balhdr-log_handle ) )
          i_do_not_load_messages = abap_true
          i_lock_handling        = 1
        EXCEPTIONS
          error_message          = 1
          OTHERS                 = 2.

    ELSE.

      DATA(ls_log_head) = CORRESPONDING bal_s_log( ms_balhdr EXCEPT alprog ).
      ls_log_head-params-callback-userexitp = ms_balhdr-userexitp.
      ls_log_head-params-callback-userexitf = ms_balhdr-userexitf.

      CALL FUNCTION 'BAL_LOG_CREATE'
        EXPORTING
          i_s_log       = ls_log_head
        IMPORTING
          e_log_handle  = ms_balhdr-log_handle
        EXCEPTIONS
          error_message = 1
          OTHERS        = 2.

    ENDIF.


    LOOP AT mt_log INTO DATA(ls_log)
        WHERE is_db_entry = abap_false.

      DATA(ls_bal) = VALUE bal_s_msg(  ).

      NEW lcl_help_msg_mapper( )->main(
        EXPORTING
          input           = ls_log
        IMPORTING
          result          = ls_bal
      ).

      ls_bal-time_stmp = ls_log-tstmp.

      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle  = ms_balhdr-log_handle
          i_s_msg       = ls_bal
        EXCEPTIONS
          error_message = 1
          OTHERS        = 2.

    ENDLOOP.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_in_update_task = ''
        i_save_all       = ''
        i_t_log_handle   = VALUE bal_t_logh( ( ms_balhdr-log_handle ) )
      EXCEPTIONS
        error_message    = 4
        OTHERS           = 5.

  ENDMETHOD.


  METHOD check_msg_error.

    result = get( )-is_error.

  ENDMETHOD.


  METHOD constructor.

    IF ix IS BOUND.
      mx = ix.
    ELSE.
      mx = NEW #( bal = me ).
    ENDIF.

    init(
     iv_extnumber   = is_in-extnumber
     iv_subobject   = is_in-subobject
     iv_object      = is_in-object
     iv_description = is_in-description ).

  ENDMETHOD.


  METHOD db_save.
    TRY.
        result = me.

        "check customizing -> database log active?
        CHECK line_exists( mt_log[ is_db_entry = abap_false ] ) OR
              line_exists( mt_tag[ is_db_entry = abap_false ] ).

        "save stack for search functions
        mt_stack = lcl_help=>get_callstack( 1 ).
        SORT mt_stack BY mainprogram blockname.
        DELETE ADJACENT DUPLICATES FROM mt_stack COMPARING mainprogram blockname.

        DATA(lv_log_as_xml) = lcl_help=>xml_obj_2_string( me ).
        DATA(lv_task)       = CONV string( lcl_help=>get_time( )-tstmp ).
        DATA(lv_trfcqnam)   = CONV trfcqnam( 'ZRG_' && ms_balhdr-subobject ).

        CALL FUNCTION 'ZFM_STC_002' "STARTING NEW TASK lv_task
          DESTINATION 'NONE'
          EXPORTING
            ij_objekt   = lv_log_as_xml
            iv_trfcqnam = lv_trfcqnam.

        "mark messages saved to database
        mt_log = VALUE #( LET tab  = mt_log IN FOR row  IN tab  ( VALUE #( BASE row  is_db_entry = abap_true ) ) ).
        mt_tag = VALUE #( LET tab2 = mt_tag IN FOR row2 IN tab2 ( VALUE #( BASE row2 is_db_entry = abap_true ) ) ).

        IF ms_balhdr-lognumber IS INITIAL.
          ms_balhdr-lognumber = bal_read_head( get_id(  ) )-lognumber.
        ENDIF.

      CATCH cx_root.
        "this log will not interrupt you
    ENDTRY.
  ENDMETHOD.


  METHOD factory.

    result = NEW #( ).

  ENDMETHOD.


  METHOD factory_by_bal.
    TRY.

        result = NEW #( ).

        CLEAR result->ms_balhdr.
        result->ms_balhdr = result->bal_read_head( val ).
        result->bal_read_items( ).

      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD get.
    TRY.

*        result-type = COND #(
*            WHEN line_exists( mt_log[ type = 'A' ] ) THEN 'A'
*            WHEN line_exists( mt_log[ type = 'X' ] ) THEN 'X'
*            WHEN line_exists( mt_log[ type = 'E' ] ) THEN 'E'
*            WHEN line_exists( mt_log[ type = 'W' ] ) THEN 'W'
*            WHEN line_exists( mt_log[ type = 'S' ] ) THEN 'S'
*             ).

*        result-is_error = COND #( WHEN result-type CA 'EAX' THEN abap_true ).

        data(ls_msg) =  NEW lcl_help_msg_mapper( )->get( mt_log ).
        result-is_error = ls_msg-is_error.
        result-type = ls_msg-type.
        result-t_bapi = ls_msg-t_bapi.
        result-s_bapi_last = ls_msg-t_bapi[ lines( ls_msg-t_bapi ) ].

*        LOOP AT mt_log INTO DATA(ls_log).
*          INSERT lcl_help=>msg( ls_log )-s_bapi INTO TABLE result-t_bapi.
*        ENDLOOP.

      CATCH cx_root.
        "this log will not interrupt you
    ENDTRY.
  ENDMETHOD.


  METHOD get_id.

    result = COND #(
         WHEN ms_balhdr-lognumber IS NOT INITIAL THEN ms_balhdr-lognumber
         ELSE ms_balhdr-userexitp ).

  ENDMETHOD.


  METHOD gui.

    DATA lt_balm TYPE STANDARD TABLE OF balm.

    lt_balm = VALUE #( FOR row IN mt_log (
        msgid = row-msgid
        msgno = row-msgno
        msgty = row-type
        msgv1 = row-v1
        msgv2 = row-v2
        msgv3 = row-v3
        msgv4 = row-v4
        time_stmp = row-tstmp
         ) ).

    CASE iv_action.

      WHEN cs_gui_action-popup.

*        zcl_stc_utility_abap_2102=>gui( balm = 'X' val = lt_balm ).

    ENDCASE.

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

        ms_balhdr-altext    = iv_description.
        ms_balhdr-userexitp = lcl_help=>get_guid16( ).
        ms_balhdr-userexitf = 'form'.

        ms_balhdr-aldate_del = sy-datlo + iv_days_delete.

      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD tag_cleanup.
    TRY.

*        DATA(lv_random_int) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( ) min = 0 max = 1000 )->get_next( ).
*        IF lv_random_int <> 10.
*          RETURN.
*        ENDIF.
*
*        DATA(lt_del) = VALUE rseloption( ).
*
*        "all entries of /zrg/ with no reference to balhdr
*        SELECT FROM
*           zstc_t_bal_ext AS log
*        LEFT OUTER JOIN
*           balhdr ON
*            balhdr~lognumber = log~log_id
*        FIELDS
*          'I' AS sign,
*          'EQ' AS option,
*          log~log_id AS low
*        WHERE
*          log~log_id IS NOT INITIAL AND
*          balhdr~extnumber IS NULL
*        INTO TABLE @lt_del.
*
*        IF lt_del IS NOT INITIAL.
*          DELETE FROM zls_t_log WHERE extnumber IN lt_del.
*        ENDIF.

      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD tag_save.

    DATA ls_tag TYPE zls_t_log.
    DATA: ls_t003 like ls_tag." Additional Identifier for Ballog
    DATA: lt_t003 like STANDARD TABLE OF ls_tag." Additional Identifier for Ballog

    LOOP AT me->mt_tag INTO DATA(ls_balmp).
      ls_tag-tagdata = ls_tag-tagdata && ls_balmp-name &&  ls_balmp-value.
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
    ls_tag-log_id = get_id( ).
*    ls_tag-parname = 'TAG'.
*    ls_tag-parvalue = 'TAG'.
    ls_tag-tagdata = ls_tag-tagdata && SWITCH string( me->get( )-type
      WHEN 'E' THEN 'ERROR'
      WHEN 'S' THEN 'SUCCESS'  ).

    INSERT ls_tag INTO TABLE lt_t003.

    DELETE FROM  zls_t_log WHERE log_id = ls_tag-log_id.
    MODIFY zls_t_log FROM TABLE lt_t003.

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
