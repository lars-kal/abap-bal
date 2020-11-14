class zls_cl_at_string definition
  public
  final
  create public .

  public section.

    data result type string.

    types ty_o_me type ref to zls_cl_at_string.

    methods trim
      returning
        value(result) type ty_o_me.

    methods upper_case
      returning
        value(result) type ty_o_me.

    methods segment
        importing
        sep type any
        index type any
        returning
        value(result) type ty_o_me.
*        importin

endclass.

class zls_cl_at_string implementation.

  method trim.

"test

  endmethod.

  method upper_case.

  endmethod.

  method segment.

*    result = segment( value = result sep = '/' index = 1 ).

  endmethod.

endclass.
