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
        t_bapi   type bapirettab,
        s_bapi   type line of bapirettab,
      end of ty_s_result_get.

    methods get
      importing
        val           type any
        type          type any optional
      returning
        value(result) type ty_s_result_get.

    methods get_by_msg
      importing
        id            type any optional
        no            type any optional
        type          type any optional
        v1            type any optional
        v2            type any optional
        v3            type any optional
        v4            type any optional
      returning
        value(result) type ty_s_result_get.

    methods get_by_text
      importing
        val           type any
        type          type any optional
        v1            type any optional
        v2            type any optional
        v3            type any optional
        v4            type any optional
      returning
        value(result) type ty_s_result_get.

    methods main
      importing
        input           type any
        io_typedescr_in type ref to cl_abap_typedescr optional
      exporting
        value(result)   type any.

    class-methods factory
      returning
        value(r_result) type ref to zls_hlp_cl_msg.

    class-methods class_constructor.

  protected section.

    types ty_t_string_hash type hashed table of string with unique key table_line.
    types ty_t_string_deep type standard table of ty_t_string_hash with empty key.

    class-data: st_groups type ty_t_string_deep.

    data mo_typedescr_in  type ref to cl_abap_typedescr.
    data mo_typedescr_out type ref to cl_abap_typedescr.

    methods _map_struct_2_struct
      importing
        is_in  type any
      exporting
        es_out type any.

    methods _map_obj_2_tab
      importing
        value  type any
      exporting
        result type bapiret2_tab.


endclass.



class zls_hlp_cl_msg implementation.

  method factory.

    r_result = new #( ).

  endmethod.


  method class_constructor.

    st_groups = value ty_t_string_deep(
       ( value #( ( `ID`) ( `MSGID`) ( `ARBGB` ) ( `SYMSGID` ) ( `SYST_MSGID` ) ( `AG` )
       ) )
       ( value #( ( `NO`) ( `MSGNO`) ( `MSGNR`) ( `NUMBER`) ( `SYMSGNO`) ( `SYST_MSGNO`)
       ) )
       ( value #( ( `TYPE`) ( `MSGTY`) ( `MSGTYP`) ( `SEVERITY`) ( `SYST_MSGTY`) ( `BAPI_MTYPE`)
       ) )
       ( value #( ( `MSGV1`) ( `MESSAGE_V1`)
       ) )
       ( value #( ( `MSGV2`) ( `MESSAGE_V2`)
       ) )
       ( value #( ( `MSGV3`) ( `MESSAGE_V3`)
       ) )
       ( value #( ( `MSGV4`) ( `MESSAGE_V4`)
       ) )
     ).

  endmethod.


  method get.

    main(
      exporting
        input  = val
      importing
        result = result-t_bapi
    ).

    if result-t_bapi is initial.
      return.
    endif.

    if type is not initial.
      loop at result-t_bapi reference into data(lr_bapi).
        lr_bapi->type = type.
      endloop.
    endif.

    result-s_bapi = result-t_bapi[ 1 ].

    message id result-s_bapi-id type 'I' number result-s_bapi-number
    with result-s_bapi-message_v1 result-s_bapi-message_v2 result-s_bapi-message_v3 result-s_bapi-message_v4
    into result-txt.
    clear: sy-msgty,sy-msgno, sy-msgid, sy-msgv1, sy-msgv2, sy-msgv3, sy-msgv4.

    result-type = cond #(
        when line_exists( result-t_bapi[ type = 'E' ] ) then 'E'
        when line_exists( result-t_bapi[ type = 'W' ] ) then 'W'
        when line_exists( result-t_bapi[ type = 'S' ] ) then 'S'
        ) .

    result-is_error = switch #( result-type when 'E' then abap_true else abap_false ).

  endmethod.


  method get_by_msg.

    result = get( value bapiret2(
      type       = type
      number     = no
      id         = id
      message_v1 = v1
      message_v2 = v2
      message_v3 = v3
      message_v4 = v4
    ) ).

  endmethod.


  method main.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " check in/out types

    mo_typedescr_in = cond #( when io_typedescr_in is initial
        then cl_abap_typedescr=>describe_by_data( input )
        else io_typedescr_in ).

    mo_typedescr_out = cl_abap_typedescr=>describe_by_data( result ).


    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " structure to any

    field-symbols <tab_in>  type standard table.
    field-symbols <tab_out> type standard table.

    case mo_typedescr_in->kind.

        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        " structure to any

      when cl_abap_typedescr=>kind_struct.

        case  mo_typedescr_out->kind.

          when cl_abap_typedescr=>kind_struct.

            _map_struct_2_struct(
              exporting
                is_in  = input
              importing
                es_out = result
            ).

          when cl_abap_typedescr=>kind_table.

            assign result to <tab_out>.
            insert initial line into table <tab_out> assigning field-symbol(<row_out>).

            factory( )->main(
              exporting
                input = input
                io_typedescr_in = mo_typedescr_in
              importing
                result = <row_out>
            ).


        endcase.


        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        " table to any

      when cl_abap_typedescr=>kind_table.

        case  mo_typedescr_out->kind.

          when cl_abap_typedescr=>kind_struct.

            assign input to <tab_in>.
            assign <tab_in>[ 1 ] to field-symbol(<row_in>).

            main(
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

              main(
                exporting
                  input = <row_in>
                importing
                   result = <row_out>
               ).

            endloop.

        endcase.

        """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        " table to any

      when cl_abap_typedescr=>kind_ref.

        _map_obj_2_tab(
          exporting
            value  = input
          importing
            result = data(lt_bapi)
        ).

        factory(  )->main(
          exporting
            input           = lt_bapi
          importing
            result          = result
        ).


    endcase.

  endmethod.


  method _map_struct_2_struct.


    data(lt_in)  = value string_table( for row in cast cl_abap_structdescr( mo_typedescr_in )->components
        ( conv string( row-name ) ) ).

    data(lt_out) = value string_table( for row in cast cl_abap_structdescr( mo_typedescr_out )->components
        ( conv string( row-name ) ) ).

    "Check every component of output....
    loop at lt_out into data(comp_out).

      "is there a group fitting the component?
      loop at st_groups into data(lt_group).

        "if not, next
        check line_exists( lt_group[ table_line = comp_out ] ).

        "if yes, is there an input fit which fits the same group?
        loop at lt_in into data(comp_in).

          "if not, next
          if not line_exists( lt_group[ table_line = comp_in ] ).
            continue.
          endif.

          "if yes, write input to output
          assign component comp_in  of structure is_in  to field-symbol(<in>).
          assign component comp_out of structure es_out to field-symbol(<out>).

          <out> =  to_upper( shift_left( shift_right( conv string( <in> ) ) ) ) .

          exit.
        endloop.
        exit.
      endloop.
    endloop.

  endmethod.

  method _map_obj_2_tab.

    case type of value.

      when type cx_root into data(lx_root).

        while lx_root is bound.

          data(ls_bapi) = value bapiret2(  ).

          case type of lx_root.

            when type if_t100_message into data(li_t100).

              ls_bapi = factory( )->get( li_t100 )-s_bapi.

              case type of li_t100.

                when type if_t100_dyn_msg into data(li_t100_dyn).

                  ls_bapi-type = li_t100_dyn->msgty.
                  ls_bapi-message_v1 = li_t100_dyn->msgv1.
                  ls_bapi-message_v2 = li_t100_dyn->msgv2.
                  ls_bapi-message_v3 = li_t100_dyn->msgv3.
                  ls_bapi-message_v4 = li_t100_dyn->msgv4.

              endcase.

            when type cx_root into lx_root.

              ls_bapi = factory( )->get_by_text( lx_root->get_text( ) )-s_bapi.

          endcase.
          insert ls_bapi into table result.
          lx_root = lx_root->previous.
        endwhile.

    endcase.

  endmethod.

  method get_by_text.

    result = get_by_msg(
         id     = '00'
         no     = '398'
         type   = type
         v1     = conv string( val )
         v2     = cond string( let l = strlen( val ) in when l >= 50  then val+50  else '' )
         v3     = cond string( let l = strlen( val ) in when l >= 100 then val+100 else '' )
         v4     = cond string( let l = strlen( val ) in when l >= 150 then val+150 else '' )
     ).

  endmethod.

endclass.
