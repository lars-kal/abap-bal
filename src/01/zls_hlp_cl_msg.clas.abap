class zls_hlp_cl_msg definition
  public
  final
  create public .

  public section.

    types:
      begin of ty_s_result_get,
        txt      type string,
        is_error type abap_bool,
        type     type abap_bool,
*        id       type string,
*        no       type string,
*        v1       type string,
*        v2       type string,
*        v3       type string,
*        v4       type string,
        t_msg    type bapirettab,
        s_msg    type line of bapirettab,
      end of ty_s_result_get.
    types ty_o_me type ref to zls_hlp_cl_msg.

    class-methods get
      importing
        val           type any
        langu         type any default sy-langu
        id            type any optional
        no            type any optional
        v1            type any optional
        v2            type any optional
        v3            type any optional
        v4            type any optional
      returning
        value(result) type ty_s_result_get.

    class-methods map
      importing
        input  type any
      exporting
        result type any.

    class-methods class_constructor.
    class-methods log_factory
      returning
        value(result) type ty_o_me.

    methods log
      importing
        val           type any
      returning
        value(result) type ty_o_me.

    methods log_get
      returning
        value(result) type ty_s_result_get.


  protected section.

    types ty_t_string_hash type hashed table of string with unique key table_line.
    types ty_t_string_deep type standard table of ty_t_string_hash with empty key.

    class-data: st_groups type ty_t_string_deep.

    class-methods _map_structure_2_structure
      importing
        is_in  type any
      exporting
        es_out type any.

private section.
ENDCLASS.



CLASS zls_hlp_cl_msg IMPLEMENTATION.


  method class_constructor.

    st_groups = value ty_t_string_deep(
       ( value #( ( `ID`) ( `MSGID`) ( `ARBGB` ) ( `SYMSGID` ) ( `SYST_MSGID` ) ( `AG` )
       ) )
       ( value #( ( `NO`) ( `MSGNO`) ( `MSGNR`) ( `NUMBER`) ( `SYMSGNO`) ( `SYST_MSGNO`)
       ) )
       ( value #( ( `TYPE`) ( `MSGTY`) ( `MSGTYP`) ( `SEVERITY`) ( `SYST_MSGTY`) ( `BAPI_MTYPE`)
       ) )
     ).

  endmethod.


  method get.

    map(
      exporting
        input  = val
      importing
        result = result-t_msg
    ).



  "test

    if result-t_msg is initial.
      return.
    endif.

    result-s_msg = result-t_msg[ 1 ].

    message id result-s_msg-id type 'I' number result-s_msg-number
    with result-s_msg-message_v1 result-s_msg-message_v2 result-s_msg-message_v3 result-s_msg-message_v4
    into result-txt.
    clear: sy-msgty,sy-msgno, sy-msgid, sy-msgv1, sy-msgv2, sy-msgv3, sy-msgv4.

    result-type = cond #(
        when line_exists( result-t_msg[ type = 'E' ] ) then 'E'
        when line_exists( result-t_msg[ type = 'W' ] ) then 'W'
        when line_exists( result-t_msg[ type = 'S' ] ) then 'S'
        ) .

    result-is_error = switch #( result-type when 'E' then abap_true else abap_false ).

  endmethod.


  method log.

  endmethod.


  method log_factory.

  endmethod.


  method log_get.

  endmethod.


  method map.

    field-symbols <tab_in>  type standard table.
    field-symbols <tab_out> type standard table.

    data(lv_kind_in)  =  cl_abap_typedescr=>describe_by_data( input  )->kind.
    data(lv_kind_out) =  cl_abap_typedescr=>describe_by_data( result )->kind.

    case lv_kind_in.


        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        " structure to any

      when cl_abap_typedescr=>kind_struct.

        case  lv_kind_out.

          when cl_abap_typedescr=>kind_struct.

            _map_structure_2_structure(
              exporting
                is_in  = input
              importing
                es_out = result
            ).

          when cl_abap_typedescr=>kind_table.

            assign result to <tab_out>.
            insert initial line into table <tab_out> assigning field-symbol(<row_out>).

            map(
              exporting
                input = input
              importing
                result = <row_out>
            ).


        endcase.


        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        " table to any

      when cl_abap_typedescr=>kind_table.

        case  lv_kind_out.

          when cl_abap_typedescr=>kind_struct.

            assign input to <tab_in>.
            assign <tab_in>[ 1 ] to field-symbol(<row_in>).

            map(
              exporting
                input = <row_in>
              importing
                 result = result
             ).


          when cl_abap_typedescr=>kind_table.

            assign input to <tab_in>.
            assign result to <tab_out>.

            loop at <tab_in> assigning <row_in>.

              insert initial line into table <tab_out> assigning <row_out>.

              map(
                exporting
                  input = <row_in>
                importing
                   result = <row_out>
               ).

            endloop.

        endcase.
    endcase.

  endmethod.


  method _map_structure_2_structure.


    data(lt_in)  = value string_table( for row in cast cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( is_in  ) )->components ( conv string( row-name ) ) ).
    data(lt_out) = value string_table( for row in cast cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( es_out ) )->components ( conv string( row-name ) ) ).


    "Check every component of output....
    loop at lt_out into data(comp_out).

      "is there a group fitting the component?
      loop at st_groups into data(lt_group).

        "if not, next
        if not line_exists( lt_group[ table_line = comp_out ] ).
          continue.
        endif.

        "if yes, is there an input fit which fits the same group?
        loop at lt_in into data(comp_in).

          "if not, next
          if not line_exists( lt_group[ table_line = comp_in ] ).
            continue.
          endif.

          "if yes, write input to out output
          assign component comp_in  of structure is_in  to field-symbol(<in>).
          assign component comp_out of structure es_out to field-symbol(<out>).

          "Formatierung
          if line_exists( lt_group[ table_line = `ID` ] )
          or line_exists( lt_group[ table_line = `NO` ] )
          or line_exists( lt_group[ table_line = `TYPE` ] ).
            <out> = to_upper( shift_left( shift_right( <in> ) ) ).
          else.
            <out> = <in>.
          endif.

          exit.
        endloop.
        exit.
      endloop.
    endloop.

  endmethod.
ENDCLASS.


