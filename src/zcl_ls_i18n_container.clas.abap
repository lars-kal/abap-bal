class zcl_ls_i18n_container definition
  public
  final
  create public .

  public section.

    class-data:
      begin of i18n,
        begin of wm,
          warehouse_v1_no_material_v2 type string value 'MSGCL/100/ZZMSGCLASS',
        end of wm,
        begin of tech,
*        beg
          tech_no_data_found            type string value '100/ZZMSGCLASS',
          tech_very_long_message_wareho type string value '100/ZZMSGCLASS',
        end of tech,
        begin of word,
          material type string value 'DD04t/MATNR',
        end of word,
        o_service type ref to zcl_ls_i18n_container,
      end of i18n.

    methods get
      importing
        val type string.

    methods constructor.
    methods set_config
      importing
        lang type string optional.

    class-methods class_constructor.

  protected section.
    data:
      begin of ms_config,
        language       type string,
        language_prio2 type string,
        language_prio3 type string,
      end of ms_config.

    methods example.
    methods fill_buffer_once.
    methods unit_test.
    methods read_t100.
    methods read_sotr.
    methods read_dd04t.

  private section.
endclass.


class zcl_ls_i18n_container implementation.

  method constructor.

    fill_buffer_once(  ).

  endmethod.

  method class_constructor.

    i18n-o_service = new #(  ).

  endmethod.

  method example.

    data(lv_text) = i18n-wm-warehouse_v1_no_material_v2.
    data(lv_text2) = i18n-tech-tech_no_data_found.
    data(lv_text3) = i18n-wm-warehouse_v1_no_material_v2.

*hlp=>cx->raise( i18n-warehouse_v1_no_material_v2 ).
*hlp=>txt->
*hlp=>trans->
*hlp=>i18n->
*hlp=>x_raise( hlp=>i18n-warehouse_v1_no_material_v2 ).
*hlp=>msg( hlp=>i18n-wm_warehouse_v1_no_material_v2 )-t_log
*hlp=>msg( hlp=>i18n-abap_warehouse_v1_no_material_v2 )-t_log
*hlp=>msg( hlp=>i18n-sd_warehouse_v1_no_material_v2 )-t_log

  endmethod.

  method unit_test.

    "Über i18n drübergehen
    " messages auslesen
    "prüfen ob in sprache vorhanden


  endmethod.

  method read_dd04t.

    "dd04t lesen
    "und buffern

  endmethod.

  method read_sotr.

  endmethod.

  method read_t100.

  endmethod.

  method get.

  endmethod.

  method set_config.

  endmethod.

  method fill_buffer_once.

  endmethod.

endclass.
