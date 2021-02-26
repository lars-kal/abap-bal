
CLASS lcl_help_msg_mapper DEFINITION.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_s_result_get,
        message  TYPE string,
        is_error TYPE abap_bool,
        type     TYPE abap_bool,
        t_bapi   TYPE bapirettab,
        s_bapi   TYPE LINE OF bapirettab,
      END OF ty_s_result_get.

    METHODS get
      IMPORTING
        val           TYPE any
        type          TYPE any OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_s_result_get.

    METHODS get_by_msg
      IMPORTING
        id            TYPE any OPTIONAL
        no            TYPE any OPTIONAL
        type          TYPE any OPTIONAL
        v1            TYPE any OPTIONAL
        v2            TYPE any OPTIONAL
        v3            TYPE any OPTIONAL
        v4            TYPE any OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_s_result_get.

    METHODS get_by_text
      IMPORTING
        VALUE(val)    TYPE any
        type          TYPE any OPTIONAL
        v1            TYPE any OPTIONAL
        v2            TYPE any OPTIONAL
        v3            TYPE any OPTIONAL
        v4            TYPE any OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_s_result_get.

    METHODS main
      IMPORTING
        input           TYPE any
        io_typedescr_in TYPE REF TO cl_abap_typedescr OPTIONAL
      EXPORTING
        VALUE(result)   TYPE any.

    CLASS-METHODS factory
      RETURNING
        VALUE(r_result) TYPE REF TO lcl_help_msg_mapper.

    CLASS-METHODS class_constructor.

  PROTECTED SECTION.

    TYPES ty_t_string_hash TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
    TYPES ty_t_string_deep TYPE STANDARD TABLE OF ty_t_string_hash WITH EMPTY KEY.

    CLASS-DATA: st_groups TYPE ty_t_string_deep.

    DATA mo_typedescr_in  TYPE REF TO cl_abap_typedescr.
    DATA mo_typedescr_out TYPE REF TO cl_abap_typedescr.

    METHODS _map_struct_2_struct
      IMPORTING
        is_in  TYPE any
      EXPORTING
        es_out TYPE any.

    METHODS _map_obj_2_tab
      IMPORTING
        value  TYPE any
      EXPORTING
        result TYPE bapiret2_tab.


ENDCLASS.



CLASS lcl_help_msg_mapper IMPLEMENTATION.

  METHOD factory.

    r_result = NEW #( ).

  ENDMETHOD.


  METHOD class_constructor.

    st_groups = VALUE ty_t_string_deep(
       ( VALUE #( ( `ID`) ( `MSGID`) ( `ARBGB` ) ( `SYMSGID` ) ( `SYST_MSGID` ) ( `AG` )
       ) )
       ( VALUE #( ( `NO`) ( `MSGNO`) ( `MSGNR`) ( `NUMBER`) ( `SYMSGNO`) ( `SYST_MSGNO`)
       ) )
       ( VALUE #( ( `TY`) ( `TYPE`) ( `MSGTY`) ( `MSGTYP`) ( `SEVERITY`) ( `SYST_MSGTY`) ( `BAPI_MTYPE`)
       ) )
       ( VALUE #( ( `MSGV1`) ( `MESSAGE_V1`) ( `ATTR1` ) ( `V1` )
       ) )
       ( VALUE #( ( `MSGV2`) ( `MESSAGE_V2`) ( `ATTR2` ) ( `V2` )
       ) )
       ( VALUE #( ( `MSGV3`) ( `MESSAGE_V3`) ( `ATTR3` ) ( `V3` )
       ) )
       ( VALUE #( ( `MSGV4`) ( `MESSAGE_V4`) ( `ATTR4` ) ( `V4` )
       ) )
     ).

  ENDMETHOD.


  METHOD get.

    main(
      EXPORTING
        input  = val
      IMPORTING
        result = result-t_bapi
    ).

    IF result-t_bapi IS INITIAL.
      RETURN.
    ENDIF.


    LOOP AT result-t_bapi REFERENCE INTO DATA(lr_bapi).
      IF type IS NOT INITIAL.
        lr_bapi->type = type.
      ENDIF.

      MESSAGE ID lr_bapi->id TYPE 'I' NUMBER lr_bapi->number
        WITH lr_bapi->message_v1 lr_bapi->message_v2 lr_bapi->message_v3 lr_bapi->message_v4
        INTO lr_bapi->message.

      CLEAR: sy-msgty,sy-msgno, sy-msgid, sy-msgv1, sy-msgv2, sy-msgv3, sy-msgv4.

    ENDLOOP.


    result-s_bapi = result-t_bapi[ 1 ].
    result-message = result-s_bapi-message.

    result-type = COND #(
        WHEN line_exists( result-t_bapi[ type = 'E' ] ) THEN 'E'
        WHEN line_exists( result-t_bapi[ type = 'W' ] ) THEN 'W'
        WHEN line_exists( result-t_bapi[ type = 'S' ] ) THEN 'S'
        ) .

    result-is_error = SWITCH #( result-type WHEN 'E' THEN abap_true ELSE abap_false ).

  ENDMETHOD.


  METHOD get_by_msg.

    result = get( VALUE bapiret2(
      type       = type
      number     = no
      id         = id
      message_v1 = v1
      message_v2 = v2
      message_v3 = v3
      message_v4 = v4
    ) ).

  ENDMETHOD.


  METHOD main.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " check in/out types

    mo_typedescr_in = COND #( WHEN io_typedescr_in IS INITIAL
        THEN cl_abap_typedescr=>describe_by_data( input )
        ELSE io_typedescr_in ).

    mo_typedescr_out = cl_abap_typedescr=>describe_by_data( result ).


    FIELD-SYMBOLS <tab_in>  TYPE STANDARD TABLE.
    FIELD-SYMBOLS <tab_out> TYPE STANDARD TABLE.

    CASE mo_typedescr_in->kind.

        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        " element to any

      WHEN cl_abap_typedescr=>kind_elem.
        DATA(lv_string) = CONV string( input ).
        NEW lcl_help_msg_mapper( )->main(
         EXPORTING
             input  = VALUE bapiret2(
             id = '00'
             number = '398'
             message_v1 = input
             message_v2 = COND #( WHEN strlen( input ) > 50  THEN input+50 )
             message_v3 = COND #( WHEN strlen( input ) > 100 THEN input+100 )
             message_v4 = COND #( WHEN strlen( input ) > 150 THEN input+150 )
             )
         IMPORTING
           result = result
       ).

        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        " structure to any

      WHEN cl_abap_typedescr=>kind_struct.

        CASE  mo_typedescr_out->kind.

          WHEN cl_abap_typedescr=>kind_struct.

            _map_struct_2_struct(
              EXPORTING
                is_in  = input
              IMPORTING
                es_out = result
            ).

          WHEN cl_abap_typedescr=>kind_table.

            ASSIGN result TO <tab_out>.
            INSERT INITIAL LINE INTO TABLE <tab_out> ASSIGNING FIELD-SYMBOL(<row_out>).

            factory( )->main(
              EXPORTING
                input = input
                io_typedescr_in = mo_typedescr_in
              IMPORTING
                result = <row_out>
            ).


        ENDCASE.


        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        " table to any

      WHEN cl_abap_typedescr=>kind_table.

        CASE  mo_typedescr_out->kind.

          WHEN cl_abap_typedescr=>kind_struct.

            ASSIGN input TO <tab_in>.
            ASSIGN <tab_in>[ 1 ] TO FIELD-SYMBOL(<row_in>).

            main(
              EXPORTING
                input = <row_in>
              IMPORTING
                result = result
             ).


          WHEN cl_abap_typedescr=>kind_table.

            ASSIGN input TO <tab_in>.
            ASSIGN result TO <tab_out>.

            LOOP AT <tab_in> ASSIGNING <row_in>.

              INSERT INITIAL LINE INTO TABLE <tab_out> ASSIGNING <row_out>.

              main(
                EXPORTING
                  input = <row_in>
                IMPORTING
                   result = <row_out>
               ).

            ENDLOOP.

        ENDCASE.

        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        " object to any

      WHEN cl_abap_typedescr=>kind_ref.

        _map_obj_2_tab(
          EXPORTING
            value  = input
          IMPORTING
            result = DATA(lt_bapi)
        ).

        factory(  )->main(
          EXPORTING
            input           = lt_bapi
          IMPORTING
            result          = result
        ).


    ENDCASE.

  ENDMETHOD.


  METHOD _map_struct_2_struct.


    DATA(lt_in)  = VALUE string_table( FOR row IN CAST cl_abap_structdescr( mo_typedescr_in )->components
        ( CONV string( row-name ) ) ).

    DATA(lt_out) = VALUE string_table( FOR row IN CAST cl_abap_structdescr( mo_typedescr_out )->components
        ( CONV string( row-name ) ) ).

    "Check every component of output....
    LOOP AT lt_out INTO DATA(comp_out).

      "is there a group fitting the component?
      LOOP AT st_groups INTO DATA(lt_group).

        "if not, next
        CHECK line_exists( lt_group[ table_line = comp_out ] ).

        "if yes, is there an input fit which fits the same group?
        LOOP AT lt_in INTO DATA(comp_in).

          "if not, next
          IF NOT line_exists( lt_group[ table_line = comp_in ] ).
            CONTINUE.
          ENDIF.

          "if yes, write input to output
          ASSIGN COMPONENT comp_in  OF STRUCTURE is_in  TO FIELD-SYMBOL(<in>).
          ASSIGN COMPONENT comp_out OF STRUCTURE es_out TO FIELD-SYMBOL(<out>).

*          <out> =  to_upper( shift_left( shift_right( CONV string( <in> ) ) ) ) .
          <out> = CONV string( <in> ).
          EXIT.
        ENDLOOP.
        EXIT.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

  METHOD _map_obj_2_tab.

    CASE TYPE OF value.

      WHEN TYPE zls_cx_log INTO DATA(lx_bal).

        factory( )->main(
          EXPORTING
            input           = lx_bal->mo_bal
          IMPORTING
            result          = result
        ).

      WHEN TYPE cx_root INTO DATA(lx_root).

        WHILE lx_root IS BOUND.

          DATA(ls_bapi) = VALUE bapiret2(  ).

          CASE TYPE OF lx_root.

            WHEN TYPE if_t100_message INTO DATA(li_t100).

              ls_bapi = factory( )->get( li_t100->t100key )-s_bapi.

              CASE TYPE OF li_t100.

                WHEN TYPE if_t100_dyn_msg INTO DATA(li_t100_dyn).

                  ls_bapi-type = li_t100_dyn->msgty.
                  ls_bapi-message_v1 = li_t100_dyn->msgv1.
                  ls_bapi-message_v2 = li_t100_dyn->msgv2.
                  ls_bapi-message_v3 = li_t100_dyn->msgv3.
                  ls_bapi-message_v4 = li_t100_dyn->msgv4.


              ENDCASE.

            WHEN TYPE cx_root INTO lx_root.

              ls_bapi = factory( )->get_by_text( lx_root->get_text( ) )-s_bapi.

          ENDCASE.
          ls_bapi-type = 'E'.
          INSERT ls_bapi INTO TABLE result.
          lx_root = lx_root->previous.
        ENDWHILE.

      WHEN TYPE zls_cl_log INTO DATA(lo_log).

        factory( )->main(
          EXPORTING
            input           = lo_log->mt_log
          IMPORTING
            result          = result
        ).


    ENDCASE.

  ENDMETHOD.

  METHOD get_by_text.

    IF v1 IS NOT INITIAL.
      REPLACE '&1' IN val WITH v1.
      IF sy-subrc <> 0.
        REPLACE '&' IN val WITH v1.
      ENDIF.
    ENDIF.

    IF v2 IS NOT INITIAL.
      REPLACE '&2' IN val WITH v2.
      IF sy-subrc <> 0.
        REPLACE '&' IN val WITH v2.
      ENDIF.
    ENDIF.

    IF v3 IS NOT INITIAL.
      REPLACE '&3' IN val WITH v3.
      IF sy-subrc <> 0.
        REPLACE '&' IN val WITH v3.
      ENDIF.
    ENDIF.

    IF v4 IS NOT INITIAL.
      REPLACE '&4' IN val WITH v4.
      IF sy-subrc <> 0.
        REPLACE '&' IN val WITH v4.
      ENDIF.
    ENDIF.

    result = get_by_msg(
         id     = '00'
         no     = '398'
         type   = type
         v1     = CONV string( val )
         v2     = COND string( LET l = strlen( val ) IN WHEN l >= 50  THEN val+50  ELSE '' )
         v3     = COND string( LET l = strlen( val ) IN WHEN l >= 100 THEN val+100 ELSE '' )
         v4     = COND string( LET l = strlen( val ) IN WHEN l >= 150 THEN val+150 ELSE '' )
     ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_help DEFINITION.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_s_result_time,
             date  TYPE d,
             time  TYPE t,
             tstmp TYPE timestampl,
           END OF ty_s_result_time.

    TYPES ty_s_result_msg TYPE lcl_help_msg_mapper=>ty_s_result_get.

*    CLASS-METHODS msg
*      IMPORTING
*        val           TYPE any
*      RETURNING
*        VALUE(result) TYPE ty_s_result_msg.

    CLASS-METHODS xml_obj_2_string
      IMPORTING
        val           TYPE REF TO if_serializable_object
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS xml_string_2_obj
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE REF TO if_serializable_object.

    CLASS-METHODS get_callstack
      IMPORTING
        iv_number_skips TYPE int4 DEFAULT 1
      RETURNING
        VALUE(result)   TYPE abap_callstack.

    CLASS-METHODS get_time
      IMPORTING
        iv_tmstp      TYPE timestampl OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_s_result_time.

    CLASS-METHODS get_guid16
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.

CLASS lcl_help IMPLEMENTATION.

  METHOD get_callstack.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      EXPORTING
        max_level = '0'
      IMPORTING
        callstack = result.

    DELETE result INDEX 1.
    DO iv_number_skips TIMES.
      DELETE result INDEX 1.
      IF result IS INITIAL.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.

  METHOD xml_obj_2_string.

    CALL TRANSFORMATION id
       OPTIONS data_refs = 'heap-or-create'
       SOURCE test =  val
       RESULT XML result.

  ENDMETHOD.

  METHOD xml_string_2_obj.

    DATA(lv_data) = shift_left( val ).

    CALL TRANSFORMATION id
     SOURCE XML lv_data
     RESULT test = result.

  ENDMETHOD.

  METHOD get_time.

    IF iv_tmstp IS INITIAL.
      GET TIME STAMP FIELD result-tstmp.
    ELSE.
      result-tstmp = iv_tmstp.
    ENDIF.

    CONVERT TIME STAMP result-tstmp TIME ZONE sy-zonlo
    INTO DATE result-date TIME result-time.

  ENDMETHOD.

*  METHOD msg.
*
*    result = NEW lcl_help_msg_mapper( )->get( val ).
*
*  ENDMETHOD.

  METHOD get_guid16.
    TRY.
        result = cl_system_uuid=>create_uuid_x16_static( ).

      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
