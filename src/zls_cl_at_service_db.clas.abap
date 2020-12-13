class zls_cl_at_service_db definition
  public
  final
  create public .

  public section.

    class-data hlp type ref to zls_cl_at_main.

    methods read_t100
      importing
        is_in         type hlp->ty_s-t100
      returning
        value(result) type hlp->ty_s-t100.

    methods read_t100_multi
      importing
        it_range      type hlp->ty_t-range
      returning
        value(result) type hlp->ty_t-t100..

  protected section.
  private section.
endclass.



class zls_cl_at_service_db implementation.

  method read_t100_multi.

  endmethod.

  method read_t100.

  endmethod.

endclass.
