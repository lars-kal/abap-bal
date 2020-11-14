class zls_cl_at_i18n definition
  public
  final
  create public .

  public section.

    class-data hlp type ref to zls_cl_at_main.

    class-data:
      begin of i18n,
        begin of wm,
          warehouse_v1_no_material_v2 type string value 'MSAG/100/ZZMSGCLASS',
        end of wm,
        begin of tech,
          input_not_supported         type string value '???',
          unexpected_error            type string value 'adf',
*        beg
          no_data_found               type string value 'MSAG/100/ZZMSGCLASS',
          very_long_message_warehouse type string value 'MSAG/100/ZZMSGCLASS',
        end of tech,
        begin of word,
          material type string value 'DD04t/S/MATNR',
        end of word,
*        o_service type ref to zcl_ls_i18n_container,
      end of i18n.

    methods get_by_i18n
      importing
        val           type string
      returning
        value(result) type hlp->ty_s-txt.

    methods get_with_v
      importing
        val type string.


    methods constructor.
    methods set_config
      importing
        lang type string optional.

    class-methods class_constructor.

    constants:
      begin of cs_types,
        message_class type string value 'MSAG',
        so_text       type string value 'SOTS',
        dd04t         type string value 'DD04T',
      end of cs_types.

  protected section.

    types:
      begin of ty_s_const,
        key type string,
        val type string,
      end of ty_s_const,
      ty_t_const_h type hashed table of ty_s_const with unique key key.

    types:
      begin of ty_s_text,
        val   type string,
        langu type c length 1,
        text  type string,
      end of ty_s_text,
      ty_t_text_h type hashed table of ty_s_text with unique key val langu.

    class-data: t_buffer_const type ty_t_const_h.
    class-data: t_buffer_text  type ty_t_text_h.

    "struktur durchegehn und alles initialisieren

*    tab1
*    constantname / constant_value
*    'we-xy-'       'MSG/004/klasse1'

*    tab2
*    constant_value / lang / text
*        key         key  / text

    class-methods get_constant_by_id
      importing
        val           type string
      returning
        value(result) type string.
    class-methods _read_constants
      returning
        value(result) type string_table.
    data:
      begin of ms_config,
        language       type string,
        language_prio2 type string,
        language_prio3 type string,
      end of ms_config.

    methods example.
    class-methods fill_buffer_once.
    methods unit_test.
    methods unit_check_all_exist_langu.

    methods read
      importing
        val           type any
      returning
        value(result) type hlp->ty_s-txt.

    methods read_t100
      importing
        id            type any
        no            type any
      returning
        value(result) type string.

    methods read_sotr.
    methods read_dd04t.

  private section.
ENDCLASS.



CLASS ZLS_CL_AT_I18N IMPLEMENTATION.


  method class_constructor.

    hlp = new #(  ).
    fill_buffer_once(  ).

  endmethod.


  method constructor.

  endmethod.


  method example.

    "Ideen
    "alle texte lesen






    data(lv_text) = i18n-wm-warehouse_v1_no_material_v2.
    data(lv_text2) = i18n-tech-no_data_found.
    data(lv_text3) = i18n-wm-warehouse_v1_no_material_v2.


*    data(lo_txt) = new zls_cl_at_i18n(  ).


    hlp->text( )->get_by_i18n( hlp->i18n-wm-warehouse_v1_no_material_v2 ).

    data(lv_text5) = hlp->i18n-wm-warehouse_v1_no_material_v2.


*    data lt_test44 type hlp->type->ty_t_setup.

*    data test type hlp2=>type2.


*    hlp->txt->get( lv_text5 ).


*     data(lv_test) = hlp->i18n-wm-warehouse_v1_no_material_v2.
*     hlp->text( )->get( hlp=>i18n-wm-warehouse_v1_no_material_v2 ).

    try.

*        raise exception new lcx(
*           i18n-wm-warehouse_v1_no_material_v2
*        ).


*    data(lvasfd) = hlp=>i18n-wm-no_stock_availible.

*hlp=>text( )->get( i18n-matnr ).
*hlp=>cx( )->

*i18n
*lcx
*hlp



      catch cx_root into data(lx).
*        case lx->i18n_txt.

*          when lo_txt->i18n-wm-warehouse_v1_no_material_v2.
*
*        endcase.
    endtry.



*hlp=>cx->raise( i18n-warehouse_v1_no_material_v2 ).
*hlp=>txt->
*hlp=>trans->
*hlp=>i18n->
*hlp=>x_raise( hlp=>i18n-warehouse_v1_no_material_v2 ).
*hlp=>msg( hlp=>i18n-wm_warehouse_v1_no_material_v2 )-t_log
*hlp=>msg( hlp=>i18n-abap_warehouse_v1_no_material_v2 )-t_log
*hlp=>msg( hlp=>i18n-sd_warehouse_v1_no_material_v2 )-t_log

  endmethod.


  method fill_buffer_once.

    "tech-unit_test_usw

    data(lt_const) = _read_constants( ).

*    t_buffer_const = value #( for row in lt_const ( key = row val =  ) )
    loop at lt_const reference into data(lr_row).
      insert initial line into table t_buffer_const reference into data(lr_row_dest).
      lr_row_dest->key = lr_row->*.
      assign (lr_row_dest->key) to field-symbol(<value>).
      lr_row_dest->val = <value>.
    endloop.


    data(lt_range_t100) = value hlp->ty_t-range(  ).

    loop at t_buffer_const reference into data(lr_row2).
      data(lv_type) = hlp->string( lr_row2->val )->trim( )->upper_case( )->segment( sep = '/' index = 0 )->result.

      case lv_type.

        when cs_types-message_class.

          data(lv_msgno) = hlp->string( lr_row2->val )->trim( )->upper_case( )->segment( sep = '/' index = 1 )->result.
          data(lv_msgid) = hlp->string( lr_row2->val )->trim( )->upper_case( )->segment( sep = '/' index = 2 )->result.

          insert lines of value hlp->ty_t-range(
            ( name = 'MSGNO' sign = 'I' option = 'EQ' low = lv_msgno )
            ( name = 'MSGNO' sign = 'I' option = 'EQ' low = lv_msgno )
          ) into table lt_range_t100.


        when cs_types-dd04t.

*          read_dd04t(  ).

      endcase.

    endloop.

    if lt_range_t100 is not initial.
      hlp->db->read_t100( lt_range_t100 ).
    endif.

  endmethod.


  method get_by_i18n.

    data(lv_type) = hlp->string( val )->trim( )->upper_case( )->segment( sep = '/' index = 0 )->result.

    case lv_type.

      when cs_types-message_class.

        data(lv_msgno) = hlp->string( val )->trim( )->upper_case( )->segment( sep = '/' index = 1 )->result.
        data(lv_msgid) = hlp->string( val )->trim( )->upper_case( )->segment( sep = '/' index = 2 )->result.

        hlp->db->read_t100( value #(


          ) ).
        read_t100(
            id = lv_msgid
            no = lv_msgno
        ).

      when cs_types-dd04t.

        read_dd04t(  ).

    endcase.


*    "wenn kein ergebnis dann den namen der variable
*    if result is not initial.
*    return.
*    endif.
*
*   result = get_constant_by_id( val ).
*    if result is not initial.
*    return.
*    endif.
*
*    result = val.


  endmethod.


  method get_constant_by_id.

    "struktur durchegehn und alles initialisieren

*    tab1
*    constantname / constant_value
*    'we-xy-'       'MSG/004/klasse1'

*    tab2
*    constant_value / lang / text
*        key         key  / text

  endmethod.


  method get_with_v.

  endmethod.


  method read.

    data(lv_type) = hlp->string( val )->trim( )->upper_case( )->segment( sep = '/' index = 0 )->result.

    case lv_type.

      when cs_types-message_class.

        data(lv_msgno) = hlp->string( val )->trim( )->upper_case( )->segment( sep = '/' index = 1 )->result.
        data(lv_msgid) = hlp->string( val )->trim( )->upper_case( )->segment( sep = '/' index = 2 )->result.

        read_t100(
            id = lv_msgid
            no = lv_msgno
        ).

      when cs_types-dd04t.

        read_dd04t(  ).

    endcase.
*
*
**    "wenn kein ergebnis dann den namen der variable
**    if result is not initial.
**    return.
**    endif.
**
**   result = get_constant_by_id( val ).
**    if result is not initial.
**    return.
**    endif.
**
**    result = val.
*

  endmethod.


  method read_dd04t.

    "dd04t lesen
    "und buffern

  endmethod.


  method read_sotr.

  endmethod.


  method read_t100.

  endmethod.


  method set_config.

  endmethod.


  method unit_check_all_exist_langu.

  endmethod.


  method unit_test.

    "Über i18n drübergehen
    " messages auslesen
    "prüfen ob in sprache vorhanden


  endmethod.


  method _read_constants.

    result = value #(
     ( hlp->string( 'i18n-wm-warehouse_v1_no_material_v2' )->trim( )->upper_case( )->result )
     ).


  endmethod.
ENDCLASS.
