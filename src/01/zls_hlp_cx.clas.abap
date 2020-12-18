class zls_hlp_cx definition
  public
  inheriting from cx_no_check
*  final
  create public .

  public section.
    types ty_o_me type ref to zls_hlp_cx.

    data mt_log type bapiret2_tab.
    data mv_test type string.

    interfaces if_t100_dyn_msg .
    interfaces if_t100_message .

    methods constructor
      importing
        !textid   like if_t100_message=>t100key optional
        !previous like previous optional .

    class-methods factory
      importing
        val           type any
        previous      type ref to cx_root
      returning
        value(result) type ty_o_me.

    class-methods factory_sy
      importing
        value(is_sy)  type sy default sy
        previous      type ref to cx_root optional
          preferred parameter is_sy
      returning
        value(result) type ty_o_me.

    class-methods factory_select
      importing
        val           type any
        previous      type ref to cx_root
      returning
        value(result) type ty_o_me.

    class-methods factory_fm
      importing
        val           type any
        previous      type ref to cx_root
      returning
        value(result) type ty_o_me.

    class-methods get_log
      importing
        any           type any
      returning
        value(result) type bapiret2_tab.

  protected section.
  private section.
endclass.



class zls_hlp_cx implementation.


  method constructor ##ADT_SUPPRESS_GENERATION.
    call method super->constructor
      exporting
        previous = previous.
    clear me->textid.
    if textid is initial.
      if_t100_message~t100key = if_t100_message=>default_textid.
    else.
      if_t100_message~t100key = textid.
    endif.
  endmethod.
  method factory.

  endmethod.

  method factory_sy.

    result = new #(  ).

    "sy variablen hinzufügen

  endmethod.

  method factory_fm.

  endmethod.

  method factory_select.

    "extra info here

    result = factory_sy( sy ).
  endmethod.

  method get_log.

  endmethod.

endclass.
