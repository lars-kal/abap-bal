class zls_cl_at_unit_test definition
  public
  final
  create public .

  public section.
    class-methods main.
  protected section.
  private section.
endclass.



class zls_cl_at_unit_test implementation.

  method main.

    data(hlp) = new zls_cl_at_main(  ).


    data(lv_string) = conv string(  'test' ).

*    break-point.

    data(lo_obj) = hlp->rtti( lv_string ).
    data(lo_obj2) = hlp->rtti( hlp ).


    hlp->text( ).
    "hlp->i18n-wm- ).

    "implement tests here


  endmethod.

endclass.
