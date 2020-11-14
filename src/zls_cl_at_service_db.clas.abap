class zls_cl_at_service_db definition
  public
  final
  create public .

  public section.

    class-data hlp type ref to zls_cl_at_main.

    methods read_T100
        importing
          it_range type hlp->ty_t-range.


  protected section.
  private section.
ENDCLASS.



CLASS ZLS_CL_AT_SERVICE_DB IMPLEMENTATION.


  method read_t100.




  endmethod.
ENDCLASS.
