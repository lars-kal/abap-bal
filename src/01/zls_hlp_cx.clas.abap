class zls_hlp_cx definition
  public
  inheriting from cx_no_check
*  final
  create public .

  public section.
    types ty_o_me type ref to zls_hlp_cx.

    data mt_log type bapiret2_tab.
    data mv_text type string.

    interfaces if_t100_dyn_msg .
    interfaces if_t100_message .

    methods constructor
      importing
        !textid   like if_t100_message=>t100key optional
        !previous like previous optional
     VALUE(is_log) TYPE bapiret2 OPTIONAL.

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
        val           type any
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



       IF is_log IS NOT INITIAL.
      if_t100_message~t100key-msgid = is_log-id.
      if_t100_message~t100key-msgno = is_log-number.
      if_t100_dyn_msg~msgty = is_log-type.
      if_t100_dyn_msg~msgv1 = is_log-message_v1.
      if_t100_dyn_msg~msgv2 = is_log-message_v2.
      if_t100_dyn_msg~msgv3 = is_log-message_v3.
      if_t100_dyn_msg~msgv4 = is_log-message_v4.

      MESSAGE ID is_log-id TYPE is_log-type NUMBER is_log-number
          WITH is_log-message_v1 is_log-message_v2 is_log-message_v3 is_log-message_v4
          INTO is_log-message.
      mv_text = is_log-message.

      INSERT is_log INTO TABLE mt_log.

    ELSE.

      mv_text = me->get_text(  ).

    ENDIF.

  endmethod.
  method factory.

  endmethod.

  method factory_sy.

   result = NEW #(
        is_log = CORRESPONDING #( sy
                    MAPPING
                      id         = msgid
                      number     = msgno
                      type       = msgty
                      message_v1 = msgv1
                      message_v2 = msgv2
                      message_v3 = msgv3
                      message_v4 = msgv4
                       )
        previous = previous ).

  endmethod.

  method factory_fm.

  endmethod.

  method factory_select.

    "extra info here

    result = factory_sy( sy ).
  endmethod.

  method get_log.

    CASE TYPE OF val.
      WHEN TYPE zls_hlp_cx.
        result = CAST zls_hlp_cx( val )->mt_log.
      WHEN OTHERS.
        INSERT VALUE #( message = CAST cx_root( val )->get_text(  ) ) INTO TABLE result.
    ENDCASE.

  endmethod.

endclass.
