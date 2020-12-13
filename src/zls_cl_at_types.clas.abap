class zls_cl_at_types definition
  public
  final
  create public .

  public section.

    types:
      begin of ty_s,
        begin of txt,
          i18n_name  type string,  "NO_INPUT_&1_FOUND
          i18n_value type string,  "
          v1         type string,  "AA
          v2         type string,  "BB
          v3         type string,
          v4         type string,
        end of txt,
        begin of range,
          name   type c length 10,
          sign   type c length 1,
          option type c length 2,
          low    type string,
          high   type string,
        end of range,
        begin of t100,
          id   type string,
          no   type string,
          lang type string,
          text type string,
        end of t100,
      end of ty_s.

    types:
      begin of ty_t,
        range type standard table of ty_s-range with empty key,
        t100  type standard table of ty_S-t100 with empty key,
      end of ty_t.
    types:
      begin of ty,
*        o_me       type ref to zcl_utility_abap_2011,
        cx         type ref to zls_cx_at_error,
        t_ref_data type standard table of ref to data with empty key,
        t_ref_obj  type standard table of ref to object with empty key,
*        t_bal      type standard table of balm with empty key,
*        t_seqg3    type standard table of seqg3 with empty key,
*        t_char70   type standard table of char70 with empty key,
        begin of ty_s_col_setup,
          name       type string,
          is_tech    type abap_bool,
          is_icon    type abap_bool,
          is_hotspot type abap_bool,
          is_button  type abap_bool,
          out_length type string,
          alignement type string,
          title      type string,
          color      type string,
        end of ty_s_col_setup,
        ty_t_setup type standard table of ty-ty_s_col_setup with default key,
      end of ty.


endclass.



class zls_cl_at_types implementation.
endclass.
