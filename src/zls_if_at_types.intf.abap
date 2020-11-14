interface ZLS_IF_AT_TYPES
  public .



    types:
        o_me                type ref to zcl_utility_abap_2011,
        t_ref_data          type standard table of ref to data with empty key,
        t_ref_obj           type standard table of ref to object with empty key,
        t_bal               type standard table of balm with empty key,
        t_seqg3             type standard table of seqg3 with empty key,
        t_char70            type standard table of char70 with empty key,
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
        ty_t_setup          type standard table of ty_s_col_setup with default key.


endinterface.
