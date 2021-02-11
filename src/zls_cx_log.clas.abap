class zls_cx_log definition
  public
  final
  create public
  inheriting from cx_no_check .


  public section.

    data mo_bal type ref to zls_cl_log read-only.

    interfaces if_t100_dyn_msg.

    types ty_o_me type ref to zls_cx_log.

    class-methods factory
      importing
        val           type any optional
        previous      type ref to cx_root optional
          preferred parameter val
      returning
        value(result) type  ty_o_me.

    methods constructor
      importing
        val      type any optional
        previous type ref to cx_root optional
        bal      type ref to zls_cl_log optional.

    methods get_text redefinition.
    methods get_longtext redefinition.

  protected section.
    data mv_text type string.
    data mt_text type stringtab.

endclass.

class zls_cx_log implementation.

  method constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor(
*  EXPORTING
*    textid   =
        previous = previous
    ).

    if bal is bound.
      mo_bal = bal.
    else.
      mo_bal = new #( ix = me ).
    endif.

    mo_bal->add( val ).
    data(lt_msg) = mo_bal->get(  )-t_bapi.

    try.

        data(ls_msg) = lt_msg[ 1 ].
        if_t100_message~t100key-msgid = ls_msg-id.
        if_t100_message~t100key-msgno = ls_msg-number.
        if_t100_dyn_msg~msgty = ls_msg-type.
        if_t100_dyn_msg~msgv1 = ls_msg-message_v1.
        if_t100_dyn_msg~msgv2 = ls_msg-message_v2.
        if_t100_dyn_msg~msgv3 = ls_msg-message_v3.
        if_t100_dyn_msg~msgv4 = ls_msg-message_v4.

      catch cx_sy_itab_line_not_found.
    endtry.

    mt_text = value #( for row in lt_msg ( conv #( row-message ) ) ).

    mv_text = me->get_text(  ).


  endmethod.

  method factory.

    result = new #( val = val previous = previous ).

  endmethod.

  method get_longtext.

    result = super->get_longtext( ).

    try.
        data(lt_log) = mo_bal->get( )-t_bapi.
        result = lt_log[ 1 ]-message.
      catch cx_sy_itab_line_not_found.
    endtry.

  endmethod.

  method get_text.

   result = super->get_text( ).

    try.
        data(lt_log) = mo_bal->get( )-t_bapi.
        result = lt_log[ 1 ]-message.
      catch cx_sy_itab_line_not_found.
    endtry.
  endmethod.

endclass.
