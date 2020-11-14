class zls_cl_at_manifest definition
  public
  final
  create public .

  public section.
    CLASS-METHODS class_constructor.

    class-data:
      begin of ss_config,
        raise_error type abap_bool,
        langu       type sy-langu,
        stack_type  type string, "onprem or cloud
      end of ss_config.

  protected section.
  private section.
endclass.



class zls_cl_at_manifest implementation.

  method class_constructor.

    ss_config-langu = sy-langu.
    ss_config-stack_type = 'ONPREM'.
    ss_config-raise_error = abap_false.

  endmethod.


endclass.
