class zls_cl_at_main definition
  public
  create public .

  public section.

    interfaces zls_if_at_types.

    types ty type zls_cl_at_types=>ty.
    types ty_s type zls_cl_at_types=>ty_s.
    types ty_t type zls_cl_at_types=>ty_t.
*    types ty type zls_cl_at_types=>ty.
*aliases type for zls_if_at_types~.

*    class-data type type ref to zls_cl_at_types.

    class-data i18n like zls_cl_at_i18n=>i18n.
    class-data manifest type ref to zls_cl_at_manifest.
*     value zcl_utility_i18n=>i18n.

    class-data db type ref to zls_cl_at_service_db.

    class-methods text
      returning value(result) type ref to zls_cl_at_i18n.

    class-methods class_constructor.
    class-methods string
      importing
        val           type any
      returning
        value(result) type ref to zls_cl_at_string.

    methods rtti
      importing
        val           type any
      returning
        value(result) type ref to zls_cl_at_rtti.

    methods msg
      importing
        val           type any
      returning
        value(result) type ref to zls_cl_at_msg.




    data ref_buffer type ref to object.

private section.
ENDCLASS.



CLASS ZLS_CL_AT_MAIN IMPLEMENTATION.


  method class_constructor.
    manifest = new #(   ).
*    type = new #(   ).
  endmethod.


  method msg.
    result = new #( ).
  endmethod.


  method rtti.
*    clear ref_buffer.
*    ref_buffer = new zls_cl_at_rtti( val ).
*    result ?= ref_buffer.
*
    result = new zls_cl_at_rtti( any = val hlp = me ).
  endmethod.


  method string.

  endmethod.


  method text.

*    case hlp->rtti( txt )->ms_info-kind.
*
*      when cl_abap_datadescr=>kind_elem.
*
*        me->i18n_txt = i18n_txt.
*
*      when


  endmethod.
ENDCLASS.
