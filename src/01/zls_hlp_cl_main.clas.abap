class zls_hlp_cl_main definition
  public
*  final
  create public .

  public section.

    class-data error type ref to zls_hlp_cx.
    class-data msg type ref to zls_hlp_cl_msg.
    methods constructor.

  protected section.
  private section.
endclass.



class zls_hlp_cl_main implementation.

  method constructor.
*    mx = new #( ).
*    msg = new #(  ).
  endmethod.
endclass.
