class zls_cl_at_rtti definition
  public
  final
  create public .

  public section.

    data hlp type ref to zls_cl_at_main.

    data:
      begin of ms_info,
        type      type string,
        type_kind type string,
        kind      type string,
      end of ms_info.

    methods constructor
      importing
        any  type any
        !hlp type ref to zls_cl_at_main.

  protected section.

    data dataref   type ref to data.
    data objectref type ref to object.

    data mo_elemdescr type ref to cl_abap_elemdescr.
    data mo_objdescr type ref to cl_abap_objectdescr.

  private section.

ENDCLASS.



CLASS ZLS_CL_AT_RTTI IMPLEMENTATION.


  method constructor.
    try.

        me->hlp = hlp.

        data(lo_typedescr) = cl_abap_typedescr=>describe_by_data( any ).

        ms_info-kind      = lo_typedescr->kind.     "ref, deref, data usw
        ms_info-type_kind = lo_typedescr->type_kind. "T,S,E USW
        ms_info-type      = lo_typedescr->absolute_name.


        case ms_info-kind.

          when cl_abap_typedescr=>kind_ref.

            case ms_info-type_kind.

              when cl_abap_typedescr=>typekind_oref.
                objectref = any.
                mo_objdescr = cast #( lo_typedescr ).

              when others.
                raise exception new zls_cx_at_error(
                  hlp->i18n-tech-input_not_supported ).

            endcase.

          when cl_abap_typedescr=>kind_elem.
            dataref = ref #( any ).
            mo_elemdescr = cast #( lo_typedescr ).

          when others.
*            raise exception new zls_cx_at_error( hlp->msg( ''
           " ls_bapi "
*              ) ).

        endcase.

      catch zls_cx_at_error into data(lx).
        raise exception lx.
      catch cx_root into data(lx_root).
        raise exception new zls_cx_at_error(
          txt = hlp->i18n-tech-unexpected_error
          previous = lx_root ).
    endtry.
  endmethod.
ENDCLASS.
