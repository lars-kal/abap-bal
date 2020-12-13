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

    data(lv_string) = conv string( 'test').
    data(lo_obj) = hlp->rtti( lv_string ).
    data(lo_obj2) = hlp->rtti( hlp ).

    hlp->text( ).


*    if 'test' = hlp->text( lv_text )->get(  ).
    try.

        data(lv_text) = hlp->i18n-tech-no_data_found.
        raise exception new zls_cx_at_error( lv_text ).



      catch zls_cx_at_error into data(lx).
        case lx->i18n_txt.
          when hlp->i18n-tech-unexpected_error.
            raise exception lx.
        endcase.
    endtry.
  endmethod.

endclass.
