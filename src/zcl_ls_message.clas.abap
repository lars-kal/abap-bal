class zcl_ls_message definition
  public
  final
  create public .

  public section.
    class-data i18n like zcl_ls_i18n_container=>i18n.
*   constants i18n like zcl_ls_i18n_container=>i18n value zcl_ls_i18n_container=>i18n.
  methods main.
  protected section.
  private section.
endclass.



class zcl_ls_message implementation.
  method main.

*    i18n-word-

*    i18n-o_service->



  endmethod.

endclass.
