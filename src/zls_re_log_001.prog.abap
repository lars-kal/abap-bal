report zls_re_log_001.


CLASS lcl_app DEFINITION DEFERRED.
CLASS lcl_main_alv_ida DEFINITION DEFERRED.
CLASS lcl_popup_salv_table DEFINITION DEFERRED.
CLASS lcl_selscreen DEFINITION DEFERRED.
*DATA go_app TYPE REF TO lcl_app.

*parameters pa_table type string.

CLASS hlp DEFINITION INHERITING FROM zcl_utility_abap_2011.
ENDCLASS.

CLASS lcl_app DEFINITION.

  PUBLIC SECTION.

    CLASS-DATA so_app TYPE REF TO lcl_app READ-ONLY.

    DATA mo_alv_main TYPE REF TO lcl_main_alv_ida.
    DATA mo_alv_popup TYPE REF TO lcl_popup_salv_table.
    DATA mo_selscreen TYPE REF TO lcl_selscreen.
    METHODS main.

    CLASS-METHODS factory
      RETURNING
        VALUE(result) TYPE REF TO lcl_app.

ENDCLASS.

CLASS lcl_post DEFINITION.

ENDCLASS.


CLASS lcl_salv_table DEFINITION.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_s_out,
*        s_file     type lcl_app=>ty_s_file,
*        s_db       type lcl_app=>ty_s_db,
*        matnr       type  matnr,
*        ebeln       type  ebeln,
*        ebelp       type  ebelp,

*        icon_type      type string,
*        button_display type string,
*        button_info    type string,
*        type_descr     type string,
*        user           type syuname,
*        date           type sydatum,
*        time           type sy-uzeit,
*        t_alv_style type lvc_t_styl,
        t_celltype TYPE salv_t_int4_column,
        t_color    TYPE lvc_t_scol,
*        s_data         type input_type
      END OF ty_s_out.

    TYPES ty_t_out TYPE STANDARD TABLE OF ty_s_out WITH EMPTY KEY.

    CLASS-DATA st_out TYPE ty_t_out.
    CLASS-DATA so_grid TYPE REF TO cl_salv_table.
    CLASS-DATA so_parent_cont TYPE REF TO cl_gui_container.
    CLASS-METHODS set_table
      IMPORTING VALUE(it_data) TYPE STANDARD TABLE OPTIONAL.

    CLASS-METHODS cfw_init.
    CLASS-METHODS cfw_free.
    CLASS-METHODS cfw_display.



  PROTECTED SECTION.

    CLASS-METHODS on_added_function
      FOR EVENT added_function
      OF cl_salv_events
      IMPORTING e_salv_function sender.

    CLASS-METHODS on_link_click
      FOR EVENT link_click
      OF cl_salv_events_table
      IMPORTING row column sender.

    CLASS-METHODS on_double_click
      FOR EVENT double_click
      OF cl_salv_events_table
      IMPORTING row column sender.

    CLASS-METHODS on_before_salv_function
      FOR EVENT before_salv_function
      OF cl_salv_events
      IMPORTING e_salv_function sender.

    CLASS-METHODS on_after_salv_function
      FOR EVENT after_salv_function
      OF cl_salv_events
      IMPORTING e_salv_function sender.

    CLASS-METHODS set_handler.
    CLASS-METHODS set_alv
      RAISING
        cx_salv_existing
        cx_salv_not_found
        cx_salv_wrong_call.

ENDCLASS.




CLASS lcl_salv_table IMPLEMENTATION.

  METHOD set_table.

    DATA lv_data TYPE string.
    DATA ls_out TYPE ty_s_out.
    CLEAR st_out.

    LOOP AT it_data INTO lv_data.

      CLEAR ls_out.
*      ls_out-s_file = lv_data.

      INSERT VALUE #(
*           fname = 'TEXT'
           color-col = 2  "grün
           color-int = 1
           color-inv = 0
        ) INTO TABLE ls_out-t_color.


*      if sy-tabix < 13.
*        continue.
*      endif.
*
*      clear ls_out.
**      ls_out-s_db-kostl = lv_data(6).
**      ls_out-s_db-ebelp = lv_data+10(4).
**      ls_out-s_db-matnr = lv_da<ta+34(20).
*
*
*      if sy-tabix = 2.
*
**      insert value #(
**           columnname = 'S_DB-MATNR'
**           value      = if_salv_c_cell_type=>button
**        ) into table ls_out-t_celltype.
**
*      endif.
*
*
*      if sy-tabix = 3.
*
**      insert value #(
**           columnname = 'S_DB-MATNR'
**           value      = if_salv_c_cell_type=>hotspot
**        ) into table ls_out-t_celltype.
*
*      endif.

      INSERT ls_out INTO TABLE st_out.

    ENDLOOP.


  ENDMETHOD.

  METHOD cfw_init.
    TRY.

*    if  so_grid is not bound.

        so_parent_cont = NEW cl_gui_docking_container( repid     = sy-repid
                          dynnr     = sy-dynnr
                          side      = cl_gui_docking_container=>dock_at_bottom
                          extension = '100' "cl_gui_docking_container=>ws_ws_maximizebox

*                    ratio     = 50
                        ).

        cl_salv_table=>factory(
      EXPORTING
*        list_display   = IF_SALV_C_BOOL_SAP=>FALSE    " ALV Displayed in List Mode
        r_container    =  so_parent_cont    " Abstract Container for GUI Controls
*        container_name =
          IMPORTING
            r_salv_table   = so_grid    " Basis Class Simple ALV Tables
          CHANGING
            t_table        = st_out
        ).
*      catch cx_salv_msg.    "



*
        set_alv( ).
        set_handler( ).

*    endif.

      CATCH cx_root INTO DATA(lx_root).
        hlp=>x_raise(  lx_root ).
    ENDTRY.
  ENDMETHOD.

  METHOD cfw_free.

    IF so_grid IS BOUND.
      so_grid->close_screen(  ).
      FREE so_grid.
    ENDIF.

    IF so_parent_cont IS BOUND.
      so_parent_cont->free( ).
      FREE so_parent_cont.
    ENDIF.

  ENDMETHOD.

  METHOD cfw_display.

    IF so_grid IS NOT BOUND.

      cfw_init(  ).
      so_grid->display(  ).
    ELSE.

      so_grid->refresh(
*          exporting
*            s_stable     =     " ALV Control: Refresh Stability
*            refresh_mode = IF_SALV_C_REFRESH=>SOFT    " ALV: Data Element for Constants
      ).

    ENDIF.


*    cl_abap_list_layout=>suppress_toolbar( ).
*    write 'ALV AUSGABE'.


  ENDMETHOD.

  METHOD on_added_function.

    CASE e_salv_function.

      WHEN 'ZDELETE'.
*        if  lcl_help=>check( popup_confirm = 'X' i_any = 'Datei aus Programm entfernen?' ).
*          clear lcl_selscreen=>ss_1000-t_file.
*          cfw_free(  ).
        "On screen vom selbild muss durchlauen werden mit Button wieder rot wird
*          cl_gui_cfw=>set_new_ok_code( 'DUMMY' ).
*        endif.

      WHEN 'BTN2'.
        MESSAGE 'Button2 gedrueckt.' TYPE 'I'.
      WHEN 'BTN3'.
        MESSAGE 'Button3 gedrueckt.' TYPE 'I'.
      WHEN 'BTN4'.
        MESSAGE 'Button3 gedrueckt mit geandertem Funktionscode.' TYPE 'I'.
    ENDCASE.


  ENDMETHOD.


  METHOD set_handler.

    SET HANDLER on_added_function
    FOR so_grid->get_event( ).

    SET HANDLER on_double_click
    FOR so_grid->get_event( ).

    SET HANDLER on_before_salv_function
    FOR so_grid->get_event( ).

    SET HANDLER on_after_salv_function
    FOR so_grid->get_event( ).

    SET HANDLER on_link_click
    FOR so_grid->get_event( ).

  ENDMETHOD.


  METHOD set_alv.

    TRY.
* Grid-Header
*    DATA(o_grid_header) = NEW cl_salv_form_layout_grid( ).
*
** Überschrift
*    o_grid_header->create_header_information( row     = 1
*                                              column  = 1
*                                              text    = 'Überschrift'
*                                              tooltip = 'Tooltip' ).
*
** Kursivtext
*    o_grid_header->create_action_information( row     = 2
*                                              column  = 1
*                                              text    = 'Action-Überschrift'
*                                              tooltip = 'Tooltip' ).
*
** Fließtext
*    o_grid_header->create_flow( EXPORTING row = 3 column = 1 )->create_text( text = 'Flow:').
*    o_grid_header->create_flow( EXPORTING row = 3 column = 2 )->create_text( text = 'Test').
*
** Groupbox 1
*    DATA(o_grp_sel) = NEW cl_salv_form_groupbox( header = 'Text 1' ).
*    o_grid_header->set_element( row = 4 column = 1 r_element = o_grp_sel ).
*
*    DATA(o_grp_head_grid) = o_grp_sel->create_grid( ).
*    o_grp_head_grid->set_grid_lines( if_salv_form_c_grid_lines=>no_lines ).
*
** Label + Text
*    DATA(o_label_v) = o_grp_head_grid->create_label( row = 1 column = 1 text = |Jahr:| ).
*    DATA(o_text_v) = o_grp_head_grid->create_text( row = 1 column = 2 text = |2000| ).
*    o_label_v->set_label_for( o_text_v ).
*
** Label + Text
*    DATA(o_label_l) = o_grp_head_grid->create_label( row = 1 column = 3 text = |Auswahl:| ).
*    DATA(o_text_l) = o_grp_head_grid->create_text( row = 1 column = 4 text = |X| ).
*    o_label_l->set_label_for( o_text_l ).
*
*    so_grid->set_top_of_list( o_grid_header ).


        "Ueberschrift festlegen
        so_grid->get_display_settings( )->set_list_header( 'Folgende Datei wird für die Migration verwendet:' ). " | ' && conv string( lines( st_out ) ) && 'Einträge | Editieren mit Doppelklick ins Feld' ).
        so_grid->get_display_settings( )->set_list_header_size( value = 1 ).
*    lcl_help=>gui_screen( title_set = 'X' i_any = 'Folgende Daten werden gespeichert:' ).

        "Alle Standardfunktionen einblenden
        so_grid->get_functions( )->set_all(
         value = if_salv_c_bool_sap=>true ).


        so_grid->get_columns( )->set_color_column( 'T_COLOR' ).
        so_grid->get_columns( )->set_optimize(  ).


*    go_column ?= so_grid->get_columns( )->get_column( 'S_DB-MANDT' ).
*    go_column->set_visible( abap_false ).



        so_grid->get_functions( )->add_function(
        name =    'ZDELETE' "Funktionscode
        icon =    '@11@' "Irgendein ICON aus Tabelle ICON
        text =    'Datei löschen'
        tooltip = 'Datei löschen'
        position = if_salv_c_function_position=>right_of_salv_functions ).


        TRY.
            so_grid->get_columns( )->set_cell_type_column( 'T_CELLTYPE' ).
          CATCH cx_salv_data_error.
        ENDTRY.

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD on_link_click.

    BREAK-POINT.

  ENDMETHOD.

  METHOD on_double_click.

*row column sender.
*break-point.
*    data(lr_row) = ref #( st_out[ row ] ).
*    assign lr_row->* to field-symbol(<row>).
*
*    hlp=>gui_popup(
*    exporting
*        xml = 'X'
*        i_any            = lcl_help=>get( xml = 'X' i_any = <row> )
*      importing
*        e_any            = <row>
**        ev_answer        =
*    ).
*
*    cfw_display(  ).

  ENDMETHOD.

  METHOD on_before_salv_function.

  ENDMETHOD.

  METHOD on_after_salv_function.

  ENDMETHOD.

ENDCLASS.





CLASS lcl_selscreen DEFINITION.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_s_1000,
        pa_werks TYPE werks_d,
        pa_lgort TYPE lgort_d,
        pa_lgorp TYPE lgort_d,
        pa_lgnum TYPE lgnum,
        pa_lgtyp TYPE lgtyp,
        pa_lgpla TYPE lgpla,
        rb1      TYPE abap_bool,
        rb2      TYPE abap_bool,
*        pa_sintf type z034tbct_schnitt-schni,
*        t_file   type standard table of lcl_app=>ty_s_file with default key,
      END OF ty_s_1000.

    DATA mo_app TYPE REF TO lcl_app.

    CLASS-DATA ss_1000 TYPE ty_s_1000.

    METHODS on_init.
    METHODS on_output.
    METHODS on_screen.
    METHODS on_start.

    CLASS-DATA sv_title TYPE string.

  PROTECTED SECTION.

    CLASS-METHODS send_data_2_screen.
    CLASS-METHODS send_screen_2_data.
    CLASS-METHODS upload_file.
    CLASS-METHODS check_before_start.
    CLASS-METHODS mock_data.

ENDCLASS.

CLASS lcl_selscreen IMPLEMENTATION.

  METHOD on_init.


    IF 1 = 0.

    ENDIF.



*    break-point.


*    send_screen_2_data(  ).

*    com1 = 'Sofort ausführen'.
*    com2 = 'ALV Ausgabe'.
*    com3 = 'Aktionen des Programms:'.
*    com4 = '1. Datei über Schnittstelle einlesen'.
*    com5 = '2. ALV Ausgabe (optional)'.
*    com6 = '3. Inhalt der Tabelle Z034TPP_T_SST_03 löschen'.
*    com7 = '4. Werte aus der Datei in die Tabelle schreiben'.
*    but1 = 'Aktuelle Daten im SAP anzeigen'.

*    %_pa_lgorp_%_app_%-text = 'Partnerlagerort'.
**    %_pa_sep_%_app_%-text = .
**    %_pa_head_%_app_%-text = .
*
*    com1 = 'Anzahl Kopfzeilen (werden ignoriert)'.
*    com2 = 'Trennzeichen der Spalten'.
*
*    b1t = 'Buchungsparameter'.
*    b2t = 'Datei'.

    sv_title = 'Migration Bestände'.
    sy-title = sv_title.

*    but1 = '     Datei laden...'.
*    send_data_2_screen(  ).

  ENDMETHOD.

  METHOD on_output.

*    com3 = icon_led_green && ` Report kann ausgeführt werden`.
*    lcl_help=>gui_screen( elem_intensify = 'X' i_any = 'COM3' ).
*
*    if ss_1000-t_file is initial.
*      but1(4) = icon_incomplete.
*      com3 = icon_led_red && ` Vor dem Ausführen bitte Datei laden`.
*    else.
*      but1(4) = icon_checked.
*    endif.
*
*    if pa_lgnum is initial
*    or pa_lgorp is initial
*    or pa_lgort is initial
*    or pa_werks is initial
*    or pa_lgpla is initial
*    or pa_lgtyp is initial.
**      com3 = icon_led_red && ` Vor dem Ausführen bitte alle Buchungsparameter füllen`.
*    endif.


  ENDMETHOD.

  METHOD on_screen.

    TRY.

*        CASE sy-ucomm.
*
*          WHEN 'ZMOCK'.
*            mock_data( ).
*
*          WHEN 'ZTAB'.
**            hlp=>gui_popup( start_se16n_edit = 'X' i_any = 'Z034TPP_T_SST_02' ).
*
*          WHEN 'ZFILE'.
*            upload_file( ).
*
*        ENDCASE.


      CATCH cx_root INTO DATA(lx_root).
        hlp=>gui_popup( lx_root ).
    ENDTRY.

  ENDMETHOD.

  METHOD on_start.

    TRY.

        check_before_start( ).
        send_screen_2_data(  ).
        mo_app->main(   ).

      CATCH cx_root INTO DATA(lx_root).
        hlp=>gui_popup( lx_root ).
        LEAVE LIST-PROCESSING.
    ENDTRY.

  ENDMETHOD.


  METHOD send_data_2_screen.

    "Daten an Selbild übertragen
    hlp=>gui_screen(
      seldata_set = 'X'
      val = ss_1000 ).

  ENDMETHOD.

  METHOD send_screen_2_data.

*    data(lt_file) = ss_1000-t_file.

    "Selbild Daten laden
    hlp=>gui_screen(
      EXPORTING
        seldata_get = 'X'
      IMPORTING
        result = ss_1000 ).

*    ss_1000-t_file = lt_file.

  ENDMETHOD.


  METHOD upload_file.
*
*    data lt_asc type string_table.
*
*    hlp=>gui_screen( status_progress = 'X' val = text-002 ). "Ladevorgang...
*
*    hlp=>gui_popup(
*       exporting
*        file_upload      = 'X'
*        i_any            = 'ASC'
*        raise_error      = abap_true
*      importing
*        e_any            = lt_asc
*        ev_answer        = data(lv_answer)
*    ).
*
*    if lv_answer = lcl_help=>cs-s_popup_answer-exit.
*      return.
*    endif.
*
*    data lv_index type i.
**    data ls_file type lcl_app=>ty_s_file. "ty_s_csv.
*    clear ss_1000-t_file.
*
*    loop at lt_asc into data(lv_line).
*      if sy-tabix <= pa_head.
*        continue.
*      endif.
*
*      clear ls_file.
*
*      do.
*        try.
*            lv_index = sy-index.
*            assign component lv_index of structure ls_file to field-symbol(<lv_component>).
*            if sy-subrc <> 0.
*              exit.
*            endif.
*            <lv_component> = segment( val = lv_line  sep = pa_sep index = lv_index ) .
*
*          catch cx_root.
*        endtry.
*      enddo.
*
*      insert ls_file into table ss_1000-t_file .
*    endloop.
*
*    lcl_salv_table=>set_table( ss_1000-t_file ).
*    lcl_salv_table=>cfw_display( ).

  ENDMETHOD.


  METHOD check_before_start.
*
*    if ss_1000-t_file is initial.
*      lcl_help=>x_raise( 'Bitte zuerst die Migrationsdatei laden' ).
*    endif.
*
*
*    if pa_lgnum is initial
*    or pa_lgorp is initial
*    or pa_lgort is initial
*    or pa_werks is initial
*    or pa_lgpla is initial
*    or pa_lgtyp is initial.
*      lcl_help=>x_raise( `Vor dem Ausführen bitte alle Buchungsparameter füllen` ).
*    endif.
*
*

  ENDMETHOD.


  METHOD mock_data.

*
*    data ls_file type lcl_app=>ty_s_file.
*    ls_file-sernr = '72261100194668'.
*    ls_file-matnr = 'A9672603700'.
*    ls_file-lgtyp = '66'.
*    ls_file-lgpla = '101014'.
*    ls_file-exidv_1 = '1'.
*    ls_file-vhilm_1 = 'T50223'.
*    ls_file-exidv_2 = '14'.
*    ls_file-vhilm_2 = 'T50223'.
*
**    insert lcl_help=>msg( 'Das iste ine NAchricht' )-s_bapi into table ls_db-t_log.
**    insert ls_db into table st_db.
*    insert ls_file into table ss_1000-t_file.
*
*    ls_file-sernr = 'ABCDEFGH'.
**    ls_file-exidv_2 = '14'.
*    insert ls_file into table ss_1000-t_file.
*
**    ls_db-s_file-sernr = '100000D0995118'.
**insert ls_db into table st_db.
*
**    insert lcl_help=>msg( i_any = 'Datei unvollständigt' i_type = 'E' )-s_bapi into table ls_db-t_log.
*    insert ls_file into table ss_1000-t_file.
*
*    insert initial line into table ss_1000-t_file.
*    insert initial line into table ss_1000-t_file.
*    insert initial line into table ss_1000-t_file.

  ENDMETHOD.

ENDCLASS.



CLASS lcl_alv_grid DEFINITION.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_s_out,
        alv_ampel           TYPE char10, "char10,
*        s_file              type lcl_app=>ty_s_file,
*        s_db                type lcl_app=>ty_s_db,
*        exidv_child         type exidv,
*        exidv_parent        type exidv,
*        sernr              type sernr,
*        matnr              type matnr,
*        menge              type vepo-vemng,
*        unit               type vepo-vemeh,
*        werk               type werks_d,
*        exidv_1            type exidv,
*        vhilm_1            type vhilm,
*        exidv_2            type exidv,
*        vhilm_3            type vhilm,
*        lgort              type lgort_d,
*        lgnum              type lgnum,
*        lgtyp              type lgtyp,
*        lgpla              type lgpla,
        button_create_sernr TYPE string,
        button_create_hu    TYPE string,
        button_pack_hu      TYPE string,
        button_transfer_hu  TYPE string,
*
        button_log          TYPE string,
        t_log               TYPE bapiret2_tab,
        t_alv_style         TYPE lvc_t_styl,
        t_color             TYPE lvc_t_scol,
      END OF ty_s_out.

    TYPES ty_t_out TYPE STANDARD TABLE OF ty_s_out WITH EMPTY KEY.

    CLASS-DATA st_out TYPE ty_t_out.
    CLASS-DATA so_grid TYPE REF TO cl_gui_alv_grid.

    CLASS-METHODS set_table
      IMPORTING VALUE(it_data) TYPE STANDARD TABLE OPTIONAL.

    CLASS-METHODS cfw_init.
    CLASS-METHODS cfw_free.
    CLASS-METHODS cfw_display.


  PROTECTED SECTION.

    CLASS-DATA st_fcat     TYPE STANDARD TABLE OF lvc_s_fcat.
    CLASS-DATA ss_layout   TYPE lvc_s_layo.
    CLASS-DATA ss_variant  TYPE disvariant.
    CLASS-DATA st_excl_tb  TYPE ui_functions.

    CLASS-METHODS on_user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm sender.

    CLASS-METHODS on_data_changed
      FOR EVENT data_changed  OF  cl_gui_alv_grid
      IMPORTING er_data_changed e_ucomm sender .

    CLASS-METHODS on_data_changed_finished
      FOR EVENT data_changed_finished  OF  cl_gui_alv_grid
      IMPORTING et_good_cells e_modified sender.

    CLASS-METHODS on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object e_interactive sender.

    CLASS-METHODS  on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id e_column_id es_row_no.

    CLASS-METHODS on_button_click FOR EVENT button_click OF cl_gui_alv_grid
      IMPORTING
        es_col_id
        es_row_no
        sender.

    CLASS-METHODS set_handler.
    CLASS-METHODS set_style.
    CLASS-METHODS set_color.
    CLASS-METHODS set_fcat.

    CLASS-METHODS set_layout
      RETURNING
        VALUE(rs_layout) TYPE lvc_s_layo.
    CLASS-METHODS set_variant
      RETURNING
        VALUE(rs_variant) TYPE disvariant.
    CLASS-METHODS set_toolbar_excl
      RETURNING
        VALUE(rt_excl_tb) TYPE ui_functions..

ENDCLASS.


CLASS lcl_alv_grid IMPLEMENTATION.

  METHOD cfw_display.

*    field-symbols <tab> type standard table.
*    assign st_r_out->* to <tab>.

*    if st_out is initial.
*      lcl_help=>x_raise('ZCX_ALV_ERROR_CALL_METHOD_SET_OUTTAB_FIRST').
*    endif.


    set_style( ).
    set_color(  ).
    set_fcat( ).

    IF so_grid IS NOT BOUND.

      cfw_init( ).
      set_handler( ).
      set_layout( ).
      set_variant( ).
      set_toolbar_excl( ).

      so_grid->set_table_for_first_display(
        EXPORTING
*        i_buffer_active               =     " Buffering Active
*        i_bypassing_buffer            =     " Switch Off Buffer
*        i_consistency_check           =     " Starting Consistency Check for Interface Error Recognition
*        i_structure_name              =     " Internal Output Table Structure Name
          is_variant                    =  ss_variant   " Layout
          i_save                        =  'A'   " Save Layout
          i_default                     = 'X'    " Default Display Variant
          is_layout                     =  ss_layout   " Layout
*        is_print                      =     " Print Control
*        it_special_groups             =     " Field Groups
          it_toolbar_excluding          =  st_excl_tb   " Excluded Toolbar Standard Functions
*        it_hyperlink                  =     " Hyperlinks
*        it_alv_graphics               =     " Table of Structure DTC_S_TC
*        it_except_qinfo               =     " Table for Exception Quickinfo
*        ir_salv_adapter               =     " Interface ALV Adapter
        CHANGING
          it_outtab                     = st_out   " Output Table
          it_fieldcatalog               = st_fcat    " Field Catalog
*        it_sort                       =     " Sort Criteria
*        it_filter                     =     " Filter Criteria
        EXCEPTIONS
          OTHERS                        = 4
      ).
*      hlp=>x_raise_check( sy_subrc = 'X' is_sy = sy ).

      cl_abap_list_layout=>suppress_toolbar( ).
      WRITE ' '.
      cl_gui_alv_grid=>set_focus( control = so_grid ).


*      hlp=>gui_screen(
*        title_set = 'X'
*        i_any     = lcl_help=>msg(
*                      i_any = lcl_selscreen=>sv_title && ` (&1 Einträge)`
*                      i_v1 = lines( st_out ) )-text ).

    ELSE.

      so_grid->refresh_table_display(
*      exporting
*        is_stable      =     " With Stable Rows/Columns
*        i_soft_refresh =     " Without Sort, Filter, etc.
*      exceptions
*        finished       = 1
*        others         = 2
      ).
      IF sy-subrc <> 0.
*     message id sy-msgid type sy-msgty number sy-msgno
*                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD cfw_init.

    CREATE OBJECT so_grid
      EXPORTING
*       i_shellstyle      = 0    " Control Style
*       i_lifetime        =     " Lifetime
        i_parent = cl_gui_container=>screen0 "screen0 "default_screen    " Parent Container
*       i_appl_events     = SPACE    " Register Events as Application Events
*       i_parentdbg       =     " Internal, Do not Use
*       i_applogparent    =     " Container for Application Log
*       i_graphicsparent  =     " Container for Graphics
*       i_name   =     " Name
*       i_fcat_complete   = SPACE    " Boolean Variable (X=True, Space=False)
      EXCEPTIONS
        OTHERS   = 4.
*    hlp=>x_raise_check( sy_subrc = 'X' is_sy = sy ).

  ENDMETHOD.

  METHOD cfw_free.

    IF so_grid IS BOUND.
      so_grid->free(  ).
    ENDIF.

    CLEAR so_grid.

  ENDMETHOD.


  METHOD set_style.

    LOOP AT st_out ASSIGNING FIELD-SYMBOL(<ls_out>).



*      case abap_true.

**        when <ls_out>-create_sernr_active.
*
*
**      <ls_out>-button_display = '@0L@'.
*          insert value
*            lvc_s_styl(
*              fieldname = 'BUTTON_STOCK'
*              style     = cl_gui_alv_grid=>mc_style_button
*            )
*          into table <ls_out>-t_alv_style.
*
**      <ls_out>-button_info =  '@A0@'.
**      insert value lvc_s_styl(
**        fieldname = 'BUTTON_CREATE_HU'
**        style     = cl_gui_alv_grid=>mc_style_button
**        ) into table <ls_out>-t_alv_style.
*
*
**      <ls_out>-button_info =  '@A0@'.
**      insert value lvc_s_styl(
**        fieldname = 'BUTTON_PACK_HU'
**        style     = cl_gui_alv_grid=>mc_style_disabled
**        ) into table <ls_out>-t_alv_style.
*
*
*          if <ls_out>-t_log is not initial.
*
**      <ls_out>-button_info =  '@A0@'.
*            insert value lvc_s_styl(
*              fieldname = 'BUTTON_LOG'
*              style     = cl_gui_alv_grid=>mc_style_hotspot
*              ) into table <ls_out>-t_alv_style.
*
*          endif.


    ENDLOOP.

  ENDMETHOD.

  METHOD set_color.

    LOOP AT st_out ASSIGNING FIELD-SYMBOL(<ls_out>).

*      ls_color-fname = 'BUTTON_CREATE_HU'.
*      ls_color-color-col = 5.
*      ls_color-color-int = 0.
*      ls_color-color-inv = 0.
*      insert ls_color into table <ls_out>-t_alv_color.

    ENDLOOP.

  ENDMETHOD.

  METHOD set_fcat.

    hlp=>gui_cfw(
      EXPORTING
        get_fcat = 'X'
        i_any = st_out
      IMPORTING
        e_any = st_fcat ).

*    select *
*    from dd03l
*    into table @data(lt_dd03l)
*    where tabname = @lcl_selscreen=>ss_1000-pa_tabname.
*    lcl_help=>x_raise_check( select = 'X' ).

    LOOP AT st_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).

      IF <ls_fcat>-fieldname(4) = 'S_DB'.
        <ls_fcat>-tech = abap_true.
      ENDIF.



      CASE <ls_fcat>-fieldname.

        WHEN 'S_DB-S_EQUI-SERNR'.
          <ls_fcat>-tech = abap_false.
        WHEN 'S_DB-S_LQUA-LGTYP'.
          <ls_fcat>-tech = abap_false.
        WHEN 'S_DB-S_LQUA-LGPLA'.
          <ls_fcat>-tech = abap_false.
        WHEN 'S_DB-S_TAS20-WERKS'.
          <ls_fcat>-tech = abap_false.
          <ls_fcat>-scrtext_l = 'TAS20-Werk'.
          <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_l.
          <ls_fcat>-reptext  = <ls_fcat>-scrtext_l.

        WHEN 'ALV_AMPEL'.
          <ls_fcat>-icon = abap_true.
          <ls_fcat>-scrtext_l = 'Status'.
          <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_l.
          <ls_fcat>-reptext  = <ls_fcat>-scrtext_l.


        WHEN 'S_DB-S_VEKP-EXIDV'.
          <ls_fcat>-tech = abap_false.
          <ls_fcat>-scrtext_l = 'HU-1'.
          <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_l.
          <ls_fcat>-reptext  = <ls_fcat>-scrtext_l.

        WHEN 'S_DB-S_VEKP_HEAD-EXIDV'.
          <ls_fcat>-tech = abap_false.
          <ls_fcat>-scrtext_l = 'HU-2'.
          <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_l.
          <ls_fcat>-reptext  = <ls_fcat>-scrtext_l.

        WHEN 'S_FILE-EXIDV_1'.
          <ls_fcat>-tech = abap_false.
          <ls_fcat>-scrtext_l = 'HU-1'.
          <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_l.
          <ls_fcat>-reptext  = <ls_fcat>-scrtext_l.

        WHEN 'S_FILE-EXIDV_2'.
          <ls_fcat>-tech = abap_false.
          <ls_fcat>-scrtext_l = 'HU-2'.
          <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_l.
          <ls_fcat>-reptext  = <ls_fcat>-scrtext_l.

*      read table lt_dd03l into data(ls_dd03l)
*          with key fieldname = <ls_fcat>-fieldname.
*      if sy-subrc <> 0.
*        continue.
*      endif.

*      if ls_dd03l-keyflag <> abap_true.
*      <ls_fcat>-edit = 'X'.
*      endif.



      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD on_data_changed.

  ENDMETHOD.

  METHOD on_data_changed_finished.

  ENDMETHOD.

  METHOD on_toolbar.

    "Seperator
    INSERT VALUE #(
        butn_type = 3
     ) INTO TABLE e_object->mt_toolbar.



    INSERT VALUE #(
         function  = 'ZREFRESH'
         icon      = icon_refresh
         quickinfo = 'Aktualisieren'
         butn_type = 4
         disabled  = ' '
         text      = 'Aktualisieren'
     ) INTO TABLE e_object->mt_toolbar.

    INSERT VALUE #(
         function  = 'ZPOST'
         icon      = icon_transport
         quickinfo = 'Einträge komplett buchen'
         butn_type = 4
         disabled  = ' '
         text      = 'Markierte Einträge komplett buchen'
     ) INTO TABLE e_object->mt_toolbar.


    INSERT VALUE #(
*         function  = 'ZPOST'
*         icon      = '@K4@'
*         quickinfo = 'Markierte Einträge komplett buchen'
         butn_type = 3
         disabled  = ' '
*         text      = 'Markierte Einträge komplett buchen'
     ) INTO TABLE e_object->mt_toolbar.

    INSERT VALUE #(
         function  = 'Z_CREATE_SERNR'
*         icon      = '@K4@'
         quickinfo = '1. Serialnr. erstellen'
         butn_type = 4
         disabled  = ' '
         text      =  '1. Serialnr. erstellen'
     ) INTO TABLE e_object->mt_toolbar.


    INSERT VALUE #(
         function  = 'Z_CREATE_HU1'
*         icon      = '@K4@'
         quickinfo = '2. HU1 erstellen'
         butn_type = 4
         disabled  = ' '
         text      = '2. HU1 erstellen'
     ) INTO TABLE e_object->mt_toolbar.

    INSERT VALUE #(
    function  = 'Z_CREATE_HU2'
*         icon      = '@K4@'
    quickinfo = '3. HU2 erstellen und verpacken'
    butn_type = 4
    disabled  = ' '
    text      =  '3. HU2 erstellen und verpacken'
) INTO TABLE e_object->mt_toolbar.

    INSERT VALUE #(
    function  = 'Z_TRANSFER_HU'
*         icon      = '@K4@'
    quickinfo = '4. HU umbuchen'
    butn_type = 4
    disabled  = ' '
    text      =  '4. HU umbuchen'
) INTO TABLE e_object->mt_toolbar.

    IF sy-uname = 'LKALDEW'.

      INSERT VALUE #(
       function  = 'ZUNDO'
*         icon      = '@K4@'
       quickinfo = 'Aktion Rückgängig machen'
       butn_type = 4
       disabled  = ' '
       text      = 'Aktion Rückgängig machen'
   ) INTO TABLE e_object->mt_toolbar.

    ENDIF.

*    insert value #(
*        function  = 'ZDELETE'
**         icon      = '@K4@'
*        quickinfo = 'Speichern'
*        butn_type = 4
*        disabled  = ' '
*        text      = 'Delete selected entries'
*    ) into table e_object->mt_toolbar.

  ENDMETHOD.

  METHOD on_hotspot_click.

    TRY.

        DATA(ls_out) = st_out[  e_row_id ].

        CASE e_column_id-fieldname.

          WHEN 'BUTTON_LOG'.

            hlp=>gui_popup( ls_out-t_log ).


          WHEN 'S_EB-S_VEKP-EXIDV'.

*                      lcl_help=>call_transaction(
*                iv_transaction_name = i_any
*                iv_field1_name      = i_any2
*                iv_field1_value     = i_any3
*                iv_field2_name      = i_any3
*                iv_field3_value     = i_any4 ).

*            lcl_help=>gui_popup(  start_Tcode = 'X' ).

        ENDCASE.

      CATCH cx_root INTO DATA(lx_root).
        hlp=>gui_popup( lx_root ).
    ENDTRY.

  ENDMETHOD.

  METHOD set_handler.

    so_grid->register_edit_event(
        i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    SET HANDLER on_hotspot_click          FOR so_grid.
    SET HANDLER on_toolbar                FOR so_grid.
    SET HANDLER on_user_command           FOR so_grid.
    SET HANDLER on_button_click           FOR so_grid.
    SET HANDLER on_data_changed           FOR so_grid.
    SET HANDLER on_data_changed_finished  FOR so_grid.
    SET HANDLER on_button_click           FOR so_grid.

  ENDMETHOD.

  METHOD on_user_command.

    TRY.

*        lcl_help=>gui_screen( status_progress = 'X' i_any = text-002 ). "Ladevorgang...
*        so_grid->get_selected_rows( importing et_index_rows = data(lt_alv_selected_rows) ).
*
*        case e_ucomm.
*
*          when 'ZPOST'.
*            if lt_alv_selected_rows is initial.
*              lcl_help=>gui_popup( msg_error = 'X' i_any = 'Bitte mindestens eine Zeile markieren' ).
*              return.
*            endif.
*
*            if lcl_help=>check( popup_confirm = 'X' i_any = conv char4( lines( lt_alv_selected_rows ) ) && ` Einträge komplett durchbuchen?` ).
*            else.
*              return.
*            endif.
*            lcl_app=>post_complet( lt_alv_selected_rows ).
*
*
*          when 'ZUNDO'.
*            if lt_alv_selected_rows is initial.
*              lcl_help=>gui_popup( msg_error = 'X' i_any = 'Bitte mindestens eine Zeile markieren' ).
*              return.
*            endif.
*
*            if lcl_help=>check( popup_confirm = 'X' i_any = conv char4( lines( lt_alv_selected_rows ) ) && ` Buchung zurücknehmen?` ).
*            else.
*              return.
*            endif.
*            lcl_app=>post_complet_undo( lt_alv_selected_rows ).
*
*
*          when 'ZREFRESH'.
*
*            lcl_app=>main(  ).
*
*
*          when 'Z_CREATE_SERNR'.
*
*            if lt_alv_selected_rows is initial.
*              lcl_help=>gui_popup( msg_error = 'X' i_any = 'Bitte mindestens eine Zeile markieren' ).
*              return.
*            endif.
*
*            if lcl_help=>check( popup_confirm = 'X' i_any = lcl_help=>msg(
*                            i_any = ` Prozess Serialnummern erstellen für &1 Einträge starten?`
*                            i_v1 = lines( lt_alv_selected_rows ) ) ).
*            else.
*              return.
*            endif.
*            lcl_app=>post_create_sernr( lt_alv_selected_rows ).
*
*          when 'Z_CREATE_HU1'.
*
*            if lt_alv_selected_rows is initial.
*              lcl_help=>gui_popup( msg_error = 'X' i_any = 'Bitte mindestens eine Zeile markieren' ).
*              return.
*            endif.
*
*            if lcl_help=>check( popup_confirm = 'X' i_any = `Handling Units erstellen?` ) = abap_false.
*              return.
*            endif.
*            lcl_app=>post_create_hu1( lt_alv_selected_rows ).
*
*          when 'Z_CREATE_HU2'.
*
*            if lt_alv_selected_rows is initial.
*              lcl_help=>gui_popup( msg_error = 'X' i_any = 'Bitte mindestens eine Zeile markieren' ).
*              return.
*            endif.
*
*            if lcl_help=>check( popup_confirm = 'X' i_any = ` Handling Units Ebene 2 erstellen und verpacken?` ).
*            else.
*              return.
*            endif.
*            lcl_app=>post_create_hu2( lt_alv_selected_rows ).
*
*          when 'Z_TRANSFER_HU'.
*
*            if lt_alv_selected_rows is initial.
*              lcl_help=>gui_popup( msg_error = 'X' i_any = 'Bitte mindestens eine Zeile markieren' ).
*              return.
*            endif.
*
*            if lcl_help=>check( popup_confirm = 'X' i_any = ` Handling Units umbuchen?` ).
*            else.
*              return.
*            endif.
*            lcl_app=>post_transfer_hu( lt_alv_selected_rows ).
*
*          when others.
*        endcase.

      CATCH cx_root INTO DATA(lx_root).
        hlp=>gui_popup( lx_root ).
    ENDTRY.

  ENDMETHOD.



  METHOD on_button_click.

*    try.
*
*        lcl_help=>gui_screen( status_progress = 'X' i_any = text-002 ). "Ladevorgang...
*        data(ls_out) = st_out[  es_row_no-row_id ].
*
*        case es_col_id-fieldname.
*
*          when 'BUTTON_CREATE_SERNR'.
*
*            lcl_crud_stock=>goods_receipt_561(
*              exporting
*                iv_matnr    = ls_out-s_file-matnr
*                iv_quantity = '1'
*                iv_unit     = 'ST'
*                iv_lgort    = lcl_selscreen=>ss_1000-pa_lgorp
*                iv_werks    = lcl_selscreen=>ss_1000-pa_werks
*                it_sernr    = value string_table( ( conv #( ls_out-s_db-s_equi-sernr ) ) )
*              importing
*                ev_is_error = data(lv_is_error)
*                et_log      = data(lt_log)
**                ev_mblnr    =
**                ev_mjahr    =
*            ).
*
*            if lv_is_error = abap_true.
*              lcl_help=>x_raise( lt_log ).
*            endif.
*            lcl_help=>gui_popup( lt_log ).
*
*            lcl_app=>main(  ).
*
*
*          when 'BUTTON_CREATE_HU'.
*
*            lcl_crud_handling_unit=>create_w_pos_matnr(
*              exporting
*                iv_matnr    = ls_out-s_file-matnr
*                iv_quantity = '1'
*                iv_unit     = 'ST'
*                iv_lgort    = lcl_selscreen=>ss_1000-pa_lgorp
*                iv_werks    = lcl_selscreen=>ss_1000-pa_werks
*                iv_lgtyp    = lcl_selscreen=>ss_1000-pa_lgtyp
*                iv_lgpla    = lcl_selscreen=>ss_1000-pa_lgpla
*                it_sernr    = value string_table( ( conv #( ls_out-s_db-s_equi-sernr ) ) )
*                iv_umlgo    = lcl_selscreen=>ss_1000-pa_lgort
*                iv_vhilm    = ls_out-s_file-vhilm_1
*              importing
*                ev_is_error = data(lv_is_error_hu_create)
*                et_log      = data(lt_log_hu_create)
*                ev_exidv    = data(lv_exidv)
*                ).
*
*            if lv_is_error_hu_create = abap_true.
*              lcl_help=>x_raise( lt_log_hu_create ).
*            endif.
*            lcl_help=>gui_popup( lt_log_hu_create ).
*
*            lcl_app=>main(  ).
*
*          when 'BUTTON_PACK_HU'.
*
*            lcl_crud_handling_unit=>create_w_pos_hu_assign(
*              exporting
*                iv_vhilm     = ls_out-s_file-vhilm_2
*                it_pos_exidv = value string_table(  ( conv #( ls_out-s_db-s_vekp-exidv ) ) )
*              importing
*                ev_is_error  = data(lv_is_error_pack_hu)
*                et_log       = data(lt_log_pack_hu)
*                ev_exidv     = data(lv_exidv_pack)
*            ).
*
*            if lv_is_error_pack_hu = abap_true.
*              lcl_help=>x_raise( lt_log_pack_hu ).
*            endif.
*            lcl_help=>gui_popup( lt_log_pack_hu ).
*
*            lcl_app=>main(  ).
*
*          when 'BUTTON_TRANSFER_HU'.
*
*            lcl_crud_handling_unit=>update_lgtyp_lgpla(
*              exporting
*                iv_exidv     = ls_out-s_db-s_vekp_head-exidv
*                iv_lgtyp_new = lcl_selscreen=>ss_1000-pa_lgtyp
*                iv_lgpla_new = lcl_selscreen=>ss_1000-pa_lgpla
*              importing
*                ev_is_error  = data(lv_is_transfer_error)
*                et_log       = data(lt_transfer_log)
*                ev_tanum     = data(lv_tanum)
*            ).
*
*            if lv_is_error_pack_hu = abap_true.
*              lcl_help=>x_raise( lt_transfer_log ).
*            endif.
*            lcl_help=>gui_popup( lt_transfer_log ).
*
*            lcl_app=>main(  ).
*
*
*          when 'BUTTON_LOG'.
*
*            hlp=>gui_popup( ls_out-t_log ).
*
*          when 'BUTTON_MSG'.
*
*            hlp=>gui_popup( msg = 'X' i_any = 'Kein Log vorhanden' ).
*
*        endcase.
*
*      catch cx_root into data(lx_root).
*        lcl_help=>gui_popup( lx_root ).
*    endtry.

  ENDMETHOD.


  METHOD set_table.

**    data lr_data type ref to lcl_app=>ty_s_db.
*    data ls_out type ty_s_out.
*    data ls_color type lvc_s_scol.
*    clear st_out.
*
*    loop at it_data reference into lr_data.
*
*      clear ls_out.
*      ls_out-s_db = lr_data->*.
*      ls_out-button_create_sernr = 'Serialnr. erzeugen'.
*      ls_out-button_create_hu = 'HU erzeugen'.
*
*      if lr_data->s_file-exidv_2 is not initial.
*        ls_out-button_pack_hu = 'HU verpacken'.
*      endif.
*      ls_out-button_transfer_hu = 'HU umbuchen'.
**      ls_out-exidv_child = ls_out-s_db-s_vekp-exidv.
**      ls_out-exidv_parent = ls_out-s_db-s_vekp_head-exidv.
*      ls_out-s_file = lr_data->s_file.
*      ls_out-t_log = lr_data->t_log.
*
*      insert value #(
*   fname = 'ALV_AMPEL'
*   color-col = 2  "grün
*   color-int = 1
*   color-inv = 0
*) into table ls_out-t_color.
*
*      insert value #(
*           fname = 'S_FILE-SERNR'
*           color-col = 2  "grün
*           color-int = 1
*           color-inv = 0
*        ) into table ls_out-t_color.
*
*      insert value #(
*       fname = 'S_FILE-MATNR'
*       color-col = 2  "grün
*       color-int = 1
*       color-inv = 0
*    ) into table ls_out-t_color.
*
*      insert value #(
*   fname = 'S_FILE-LGTYP'
*   color-col = 2  "grün
*   color-int = 1
*   color-inv = 0
*) into table ls_out-t_color.
*
*      insert value #(
*   fname = 'S_FILE-LGPLA'
*   color-col = 2  "grün
*   color-int = 1
*   color-inv = 0
*) into table ls_out-t_color.
*
*      insert value #(
*   fname = 'S_FILE-EXIDV_1'
*   color-col = 2  "grün
*   color-int = 1
*   color-inv = 0
*) into table ls_out-t_color.
*
*      insert value #(
*fname = 'S_FILE-VHILM_1'
*color-col = 2  "grün
*color-int = 1
*color-inv = 0
*) into table ls_out-t_color.
*
*      insert value #(
*   fname = 'S_FILE-EXIDV_2'
*   color-col = 2  "grün
*   color-int = 1
*   color-inv = 0
*) into table ls_out-t_color.
*
*      insert value #(
*fname = 'S_FILE-VHILM_2'
*color-col = 2  "grün
*color-int = 1
*color-inv = 0
*) into table ls_out-t_color.
*
*      if lines( ls_out-t_log ) > 0.
*
*        ls_out-button_log = `Protokoll ` && `(` && shift_right( conv string( lines( ls_out-t_log ) ) ) && `)`.
*        insert value lvc_s_styl(
*          fieldname = 'BUTTON_LOG'
*          style     = cl_gui_alv_grid=>mc_style_hotspot
*          ) into table ls_out-t_alv_style.
*
*      endif.
*
*      ls_out-alv_ampel = switch #( lcl_help=>msg( ls_out-t_log )-type
*                            when 'S' then icon_led_green
*                            when 'W' then icon_led_yellow
*                            when 'E' then icon_led_red
*                          ).
*
*      case abap_true.
*
*        when lr_data->create_sernr_active.
*
**          insert value
**            lvc_s_styl(
**              fieldname = 'BUTTON_CREATE_SERNR'
**              style     = cl_gui_alv_grid=>mc_style_button
**            )
**          into table ls_out-t_alv_style.
*
*
*        when lr_data->create_hu_active.
*
*          ls_color-fname = 'BUTTON_CREATE_SERNR'.
*          ls_color-color-col = 5.
*          ls_color-color-int = 1.
*          ls_color-color-inv = 1.
*          insert ls_color into table ls_out-t_color.
*
**          insert value
**        lvc_s_styl(
**          fieldname = 'BUTTON_CREATE_HU'
**          style     = cl_gui_alv_grid=>mc_style_button
**        )
**      into table ls_out-t_alv_style.
*
*        when lr_data->create_pack_active.
*
*          ls_color-fname = 'BUTTON_CREATE_SERNR'.
*          ls_color-color-col = 5.
*          ls_color-color-int = 1.
*          ls_color-color-inv = 1.
*          insert ls_color into table ls_out-t_color.
*          ls_color-fname = 'BUTTON_CREATE_HU'.
*          insert ls_color into table ls_out-t_color.
*
**          insert value
**        lvc_s_styl(
**          fieldname = 'BUTTON_PACK_HU'
**          style     = cl_gui_alv_grid=>mc_style_button
**        )
**      into table ls_out-t_alv_style.
*
*
*        when lr_data->create_transfer_active.
*
*          ls_color-fname = 'BUTTON_CREATE_SERNR'.
*          ls_color-color-col = 5.
*          ls_color-color-int = 1.
*          ls_color-color-inv = 1.
*          insert ls_color into table ls_out-t_color.
*          ls_color-fname = 'BUTTON_CREATE_HU'.
*          insert ls_color into table ls_out-t_color.
*          ls_color-fname = 'BUTTON_PACK_HU'.
*          insert ls_color into table ls_out-t_color.
*
**          insert value
**        lvc_s_styl(
**          fieldname = 'BUTTON_TRANSFER_HU'
**          style     = cl_gui_alv_grid=>mc_style_button
**        )
**      into table ls_out-t_alv_style.
*
*        when lr_data->is_finished.
*
*          ls_color-fname = 'BUTTON_CREATE_SERNR'.
*          ls_color-color-col = 5.
*          ls_color-color-int = 1.
*          ls_color-color-inv = 1.
*          insert ls_color into table ls_out-t_color.
*          ls_color-fname = 'BUTTON_CREATE_HU'.
*          insert ls_color into table ls_out-t_color.
*          ls_color-fname = 'BUTTON_PACK_HU'.
*          insert ls_color into table ls_out-t_color.
*          ls_color-fname = 'BUTTON_TRANSFER_HU'.
*          insert ls_color into table ls_out-t_color.
*
*        when others.
*
*
*          clear ls_color.
*          ls_color-fname = 'BUTTON_CREATE_HU'.
*          ls_color-color-col = 6.
*          ls_color-color-int = 0.
*          ls_color-color-inv = 0.
*          insert ls_color into table ls_out-t_color.
*          ls_color-fname = 'BUTTON_CREATE_SERNR'.
*          insert ls_color into table ls_out-t_color.
*          ls_color-fname = 'BUTTON_PACK_HU'.
*          insert ls_color into table ls_out-t_color.
*          ls_color-fname = 'BUTTON_TRANSFER_HU'.
*          insert ls_color into table ls_out-t_color.
*
*          ls_out-button_create_sernr = ''.
*          ls_out-button_create_hu = ''.
*          ls_out-button_pack_hu = ''.
*          ls_out-button_transfer_hu = ''.
*
*      endcase.
*
*      insert ls_out into table st_out.
*
*    endloop.


  ENDMETHOD.


  METHOD set_layout.

    ss_layout-cwidth_opt = 'A'.
*    ss_layout-grid_title = 'Anpassung Customizing Stammdaten'.
    ss_layout-sel_mode   = 'A'.
    ss_layout-stylefname = 'T_ALV_STYLE'.
    ss_layout-ctab_fname = 'T_COLOR'.

  ENDMETHOD.


  METHOD set_variant.

    "U" = Benutzerbezogen
    "X" = global
    "A" = benutzerbezogen u global
    ss_variant-report = sy-repid.
    ss_variant-handle = '001'.

  ENDMETHOD.


  METHOD set_toolbar_excl.

*    insert cl_gui_alv_grid=>mc_fc_loc_delete_row    into table st_excl_tb.
*    insert cl_gui_alv_grid=>mc_fc_loc_insert_row    into table rt_excl_tb.
*    insert cl_gui_alv_grid=>mc_fc_loc_copy_row      into table rt_excl_tb.
*    insert cl_gui_alv_grid=>mc_fc_loc_paste         into table rt_excl_tb.
*    insert cl_gui_alv_grid=>mc_fc_loc_paste_new_row into table rt_excl_tb.
    INSERT cl_gui_alv_grid=>mc_fc_graph             INTO TABLE st_excl_tb.
    INSERT cl_gui_alv_grid=>mc_fc_info              INTO TABLE st_excl_tb.

  ENDMETHOD.

ENDCLASS.





CLASS lcl_popup_salv_table DEFINITION.

  PUBLIC SECTION.
    DATA mo_app TYPE REF TO lcl_app.

    DATA mo_object TYPE REF TO cl_salv_table.
    DATA mo_dock TYPE REF TO cl_gui_docking_container.
    DATA mt_out TYPE zls_cl_log=>ty_t_log.

    METHODS display
      IMPORTING
        is_row TYPE any.


    METHODS on_toolbar_click FOR EVENT added_function OF cl_salv_events_table
      IMPORTING
        e_salv_function
        sender.
  PROTECTED SECTION.

    METHODS init.

ENDCLASS.

CLASS lcl_main_alv_ida DEFINITION.

  PUBLIC SECTION.

    DATA mo_app TYPE REF TO lcl_app.

    INTERFACES: if_salv_ida_calc_field_handler.

    CONSTANTS: co_table_name TYPE dbtabl VALUE 'ZLS_CDS_LOG_01'.


    TYPES ty_S_out TYPE zls_cds_log_01.

    METHODS: on_cell_action FOR EVENT cell_action OF if_salv_gui_field_display_opt
      IMPORTING
        ev_field_name
        eo_row_data
        sender.

    METHODS: on_function_selected FOR EVENT function_selected OF if_salv_gui_toolbar_ida
      IMPORTING
        ev_fcode
        sender.

    METHODS: on_double_click FOR EVENT double_click OF if_salv_gui_table_display_opt
      IMPORTING
        ev_field_name
        eo_row_data
        sender.

    DATA mv_is_checked TYPE abap_bool VALUE abap_true.
    DATA: mo_object TYPE REF TO if_salv_gui_table_ida.

    METHODS init.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_calc_field,
        icon   TYPE char4,
        button TYPE lvc_value,
      END OF ty_calc_field.

    TYPES: BEGIN OF ty_line.
             INCLUDE TYPE sflight.
             INCLUDE TYPE ty_calc_field.
    TYPES: END OF ty_line.

ENDCLASS.



CLASS lcl_popup_salv_table IMPLEMENTATION.

  METHOD display.


    DATA(ls_row) = CONV lcl_main_alv_ida=>ty_S_out( is_row ).

    IF mo_dock IS INITIAL.
      init( ).

    ENDIF.

    mt_out = CORRESPONDING #( zls_cl_log=>factory_by_bal(
                                iv_object    = ls_row-object
                                iv_subobject = ls_row-subobject
*                                iv_extnumber =
                                iv_lognumber = ls_row-lognumber
                              )->mt_log ).

    mo_object->get_display_settings( )->set_list_header( value = CONV #(
     `Log ` && shift_left( val = ls_row-lognumber sub = '0' ) && ` | tcode ` && ls_row-tcode && ` | user ` && ls_row-aluser && ` (#`
       && shift_right( CONV string( lines( mt_out ) )  ) && `)` ) ).

    mo_dock->set_visible( abap_true ).
    mo_object->refresh( ).

  ENDMETHOD.


  METHOD init.

    CREATE OBJECT mo_dock.

    mo_dock->dock_at(
      EXPORTING
        side              = cl_gui_docking_container=>dock_at_bottom
*      EXCEPTIONS
*        cntl_error        = 1
*        cntl_system_error = 2
*        others            = 3
    ).

    mo_dock->set_height(
      EXPORTING
        height     = 180
*  EXCEPTIONS
*    cntl_error = 1
*    others     = 2
    ).
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    cl_salv_table=>factory(
           EXPORTING
             r_container  = mo_dock
           IMPORTING
             r_salv_table = mo_object
           CHANGING
             t_table      = mt_out
         ).

    mo_object->get_columns( )->get_column( columnname = 'IS_DB_ENTRY' )->set_technical( if_salv_c_bool_sap=>true  ).
    mo_object->get_columns( )->get_column( columnname = 'MESSAGE' )->set_output_length( value = '90' ).
    mo_object->get_columns( )->get_column( columnname = 'TSTMP' )->set_technical( if_salv_c_bool_sap=>true  ).
    mo_object->get_columns( )->get_column( columnname = 'V1' )->set_technical( if_salv_c_bool_sap=>true  ).
    mo_object->get_columns( )->get_column( columnname = 'V2' )->set_technical( if_salv_c_bool_sap=>true  ).
    mo_object->get_columns( )->get_column( columnname = 'V3' )->set_technical( if_salv_c_bool_sap=>true  ).
    mo_object->get_columns( )->get_column( columnname = 'V4' )->set_technical( if_salv_c_bool_sap=>true  ).
    mo_object->get_functions( )->add_function( name = 'ZHIDE'
                                         icon = |{ icon_cancel }|
*                                         text = 'Export'
                                         tooltip = 'close'
                                         position = if_salv_c_function_position=>right_of_salv_functions ).

    mo_object->get_display_settings( )->set_list_header_size( value = '1' ).
    mo_object->get_functions( )->set_default( ).

    "Eventhandler für Klicks in die Toolbar des SALV-Grids setzen
    SET HANDLER on_toolbar_click FOR mo_object->get_event( ).

    mo_object->display( ).

  ENDMETHOD.

  METHOD on_toolbar_click.

    CASE e_salv_function.
      WHEN 'ZHIDE'.
        DATA lv_c TYPE c.
        mo_dock->get_visible(
          IMPORTING
            visible           = lv_c
        ).
        cl_gui_cfw=>flush(
*          EXCEPTIONS
*            cntl_system_error = 1
*            cntl_error        = 2
*            others            = 3
        ).
        IF sy-subrc <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

        IF lv_c = abap_false.
          mo_dock->set_visible( abap_true ).

        ELSE.
          mo_dock->set_visible( abap_false ).
        ENDIF.
        .
    ENDCASE.

  ENDMETHOD.

ENDCLASS.



CLASS lcl_main_alv_ida IMPLEMENTATION.

  METHOD init.


*    if pa_table is INITIAL.
    mo_object = cl_salv_gui_table_ida=>create_for_cds_view( iv_cds_view_name = co_table_name ).
*    else.
*     mo_object = cl_salv_gui_table_ida=>create( iv_table_name = conv #(  pa_table ) ).



*        mo_object->field_catalog( )->enable_text_search( 'ALUSER' ).
*      o_salv_ida->field_catalog( )->enable_text_search( 'HUIDENT' ).
*      o_salv_ida->field_catalog( )->enable_text_search( 'PARVALUE' ).
*        o_salv_ida->field_catalog( )->enable_text_search( 'TYPE' ).
*      o_salv_ida->field_catalog( )->enable_text_search( 'PARNAME' ).
*    mo_object->field_catalog( )->enable_text_search( 'OBJECT' ).
*    mo_object->field_catalog( )->enable_text_search( 'SUBOBJECT' ).
*try.
*
*    DATA: ls_components TYPE abap_compdescr.
*DATA: lo_strucdescr TYPE REF TO cl_abap_structdescr.
**      lo_strucdescr ?= cl_abap_typedescr=>describe_by_name( pa_table ).
*
*
*LOOP AT lo_strucdescr->components INTO ls_components.
*
*try.
*  mo_object->field_catalog( )->enable_text_search( ls_components-name ).
*catch cx_root.
*endtry.
*
*ENDLOOP.


*    hlp=>gui_cfw(
**      EXPORTING
*        get_fcat            = 'X'
**        fcat_set_title      = abap_false
**        do_suppress_toolbar = abap_false
*        i_any               =
**        raise_error         = abap_false
*      IMPORTING
*        e_any               =
*    ).

*    mo_object->standard_functions( )->set_text_search_active( abap_true ).
*catch cx_root.
*endtry.
*    return.
*    endif.


*    DATA(o_salv_ida) = cl_salv_gui_table_ida=>create( iv_table_name = co_table_name ).
* Authority Check (löst evtl. Exception aus, wenn Berechtigungen fehlen)
*      o_salv_ida->add_authorization_for_object( iv_authorization_object = 'S_CARRID'
*                                                it_activities           = VALUE #( ( auth_field = 'ACTVT' value = '03' ) )
*                                                it_field_mapping        = VALUE #( ( auth_field = 'CARRID' view_field = 'CARRID' ) ) ).

* SELECT-Konditionen festlegen
*      DATA(o_cond_factory) = o_salv_ida->condition_factory( ).

* RANGE definieren: SELECT-OPTIONS
*      DATA(o_ranges) = NEW cl_salv_range_tab_collector( ).
*      o_ranges->add_ranges_for_name( iv_name = 'CARRID' it_ranges = so_carr[] ).
*      o_ranges->get_collected_ranges( IMPORTING
*                                        et_named_ranges = DATA(it_name_range_pairs) ).
* RANGE definieren: manuell
*      DATA(it_fixed_ranges) = VALUE if_salv_service_types=>yt_named_ranges( ( name = 'FORCURKEY' sign = 'E' option = 'EQ' low = 'GBP' high = '' ) ).

*      APPEND LINES OF it_fixed_ranges TO it_name_range_pairs.

* SELECT-Konditionen und RANGES übergeben
*      o_salv_ida->set_select_options( it_ranges    = it_name_range_pairs
*                                      io_condition = o_cond_factory->equals( name = 'FLDATE' value = '20120523' )->and(
*                                                     o_cond_factory->covers_pattern( name = 'PASSNAME' pattern = '*Anna*')->and(
*                                                     o_cond_factory->equals( name = 'FORCURKEY' value = 'EUR' )->or(
*                                                     o_cond_factory->equals( name = 'FORCURKEY' value = 'USD' ) ) ) ) ).

* Spaltenüberschrift ändern
*      o_salv_ida->field_catalog( )->set_field_header_texts( iv_field_name        = 'PASSNAME'
*                                                            iv_header_text       = 'Name des Passagiers'
*                                                            iv_tooltip_text      = 'Standard: 40 Zeichen zur Verfügung'
*                                                            iv_tooltip_text_long = 'Erweiterung: 100 Zeichen zur Verfügung bei richtiger SAP GUI und SAP Basis Version' ).

* Sortierungen festlegen
    DATA(it_sort_order) = VALUE if_salv_gui_types_ida=>yt_sort_rule(
     ( field_name = 'ALDATE' descending = abap_true is_grouped = abap_false )
         ( field_name = 'ALTIME' descending = abap_true is_grouped = abap_false )
                                                                       ).

    mo_object->default_layout( )->set_sort_order( it_sort_order = it_sort_order ).

* manuelle Sortierung für Spalte ausschalten
*      o_salv_ida->field_catalog( )->disable_sort( iv_field_name = 'PASSNAME' ).

* Filter ausschalten
*      o_salv_ida->field_catalog( )->disable_filter( iv_field_name = 'PASSNAME' ).

* nur bestimmte Spalten darstellen 1
*      o_salv_ida->field_catalog( )->get_all_fields( IMPORTING ets_field_names = DATA(it_field_names) ).
*      DELETE it_field_names WHERE table_line CP 'SEATS*'.

* nur bestimmte Spalten darstellen 2
*        DATA(it_field_names) = VALUE if_salv_gui_types_ida=>yts_field_name( ( CONV #( 'CARRID' ) )
*                                                                            ( CONV #( 'CONNID' ) )
*                                                                            ( CONV #( 'FLDATE' ) )
*                                                                            ( CONV #( 'FORCURKEY' ) )
*                                                                            ( CONV #( 'CUSTTYPE' ) )
*                                                                            ( CONV #( 'PASSNAME' ) ) ).

*      o_salv_ida->field_catalog( )->set_available_fields( its_field_names = it_field_names ).
*o_salv_ida->field_catalog( )->
* passenden Beschreibungstext anhand der Domäne darstellen
*      o_salv_ida->field_catalog( )->display_options( )->set_formatting( iv_field_name        = 'CUSTTYPE'
*                                                                        iv_presentation_mode = if_salv_gui_types_ida=>cs_presentation_mode-description ).

* Textsuche im Gitter für Spalten 'CARRID' und 'PASSNAME' aktivieren (Button mit Lupe oben rechts)
*      o_salv_ida->field_catalog( )->enable_text_search( 'CARRID' ).
    mo_object->field_catalog( )->enable_text_search( 'ALUSER' ).
*      o_salv_ida->field_catalog( )->enable_text_search( 'HUIDENT' ).
*      o_salv_ida->field_catalog( )->enable_text_search( 'PARVALUE' ).
*        o_salv_ida->field_catalog( )->enable_text_search( 'TYPE' ).
*      o_salv_ida->field_catalog( )->enable_text_search( 'PARNAME' ).
    mo_object->field_catalog( )->enable_text_search( 'OBJECT' ).
    mo_object->field_catalog( )->enable_text_search( 'SUBOBJECT' ).
    mo_object->field_catalog( )->enable_text_search( 'TAGDATA' ).
    mo_object->field_catalog( )->enable_text_search( 'TAGMSG' ).
    mo_object->field_catalog( )->enable_text_search( 'TAGSTACK' ).
    mo_object->standard_functions( )->set_text_search_active( abap_true ).

*        o_salv_ida->standard_functions( )->set_text_search_active( iv_active =  ).

*      data lts_field_names type if_salv_gui_types_ida=>yts_field_name.
*        insert conv #( 'HUIDENT'  ) into table lts_field_names.

*  o_salv_ida->text_search( )->set_search_scope(
*  its_field_names = lts_field_names ).

* Layouts verwalten
    mo_object->layout_persistence( )->set_persistence_options( is_persistence_key           = VALUE #( report_name = sy-repid )
                                                                i_global_save_allowed        = abap_true
                                                                i_user_specific_save_allowed = abap_true ).
*
    mo_object->toolbar( )->enable_listbox_for_layouts( ).
*

*      o_salv_ida->toolbar( )->add_button(
*          iv_fcode                     = 'ZFILTER_E'
*          iv_icon                      = icon_led_red
*          iv_is_checked                = 'X'
*      ).
*
*      o_salv_ida->toolbar( )->add_button(
*       iv_fcode                     = 'ZFILTER_S'
*       iv_icon                      = icon_led_green
*       iv_is_checked                = 'X'
*   ).
*
*      o_salv_ida->toolbar( )->add_button(
*       iv_fcode                     = 'ZFILTER_O'
*       iv_icon                      = icon_led_inactive
*       iv_is_checked                = 'X'
*   ).


* Click-Handler für Icons und Buttons aktivieren
    SET HANDLER on_function_selected FOR mo_object->toolbar( ).

*    CATCH cx_salv_ida_gui_fcode_reserved.

** Titel
*      o_salv_ida->display_options( )->set_title( 'Überschrift' ).
*
** Text für leere Tabelle
    mo_object->display_options( )->set_empty_table_text( 'Keine Daten vorhanden.' ).
*
** Zebrastreifen
    mo_object->display_options( )->enable_alternating_row_pattern( ).
*
** Datenbankfähigkeiten abfragen
*      IF abap_true = cl_salv_gui_table_ida=>db_capabilities( )->is_text_search_supported( ).
** unscharfe Suche (Fuzzy) -> wird nur von HANA unterstützt
*        o_salv_ida->text_search( )->set_field_similarity( '0.8' ).
*        o_salv_ida->text_search( )->set_search_term( |Fehler| ).
*      ENDIF.
*
** Wird eine Beschränkung der Anzahl der Datensätze empfohlen?
*      IF cl_salv_gui_table_ida=>db_capabilities( )->is_max_rows_recommended( ).
** max. Anzahl der Datensätze beschränken
*        o_salv_ida->set_maximum_number_of_rows( iv_number_of_rows = 500 ).
*      ENDIF.
* Double-Click für alle Zellen
    mo_object->display_options( )->enable_double_click( ).
    SET HANDLER on_double_click FOR mo_object->display_options( ).



  ENDMETHOD.

  METHOD on_cell_action.
    DATA: lv_row TYPE ty_line.

    eo_row_data->get_row_data( EXPORTING
                                 iv_request_type = if_salv_gui_selection_ida=>cs_request_type-all_fields
                               IMPORTING
                                 es_row = lv_row ).

    cl_salv_ida_show_data_row=>display( iv_text = |Datenfeld: { ev_field_name }|
                                        is_data = lv_row ).
  ENDMETHOD.

  METHOD if_salv_ida_calc_field_handler~get_calc_field_structure.
* Struktur-Deklaration für zus. Felder ICON und BUTTON zurückgeben
    ro_calc_field_structure = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( 'TY_CALC_FIELD' ) ).
  ENDMETHOD.

  METHOD if_salv_ida_calc_field_handler~get_requested_fields.
  ENDMETHOD.

  METHOD if_salv_ida_calc_field_handler~calculate_line.

* Daten akt. Zeile holen
    DATA(lv_sflight) = CONV sflight( is_data_base_line ).

* Freie Sitze ausrechnen
    DATA(lv_cnt_free_seats) = lv_sflight-seatsmax - lv_sflight-seatsocc.

* Calculated Fields bestimmen
    es_calculated_fields = COND ty_calc_field( WHEN lv_cnt_free_seats = 0  THEN VALUE #( icon = icon_red_light    button = icon_delete )
                                               WHEN lv_cnt_free_seats > 10 THEN VALUE #( icon = icon_green_light  button = icon_okay )
                                                                           ELSE VALUE #( icon = icon_yellow_light button = space )
                                             ).

  ENDMETHOD.

  METHOD if_salv_ida_calc_field_handler~start_page.
  ENDMETHOD.

  METHOD if_salv_ida_calc_field_handler~end_page.
  ENDMETHOD.

  METHOD on_function_selected.

    DATA lr_range_type TYPE RANGE OF char1.

    IF mv_is_checked = abap_true.
      mv_is_checked = abap_false.
      lr_range_type = VALUE #( ( sign = 'E' option = 'EQ' low = 'E' ) ).
    ELSE.
      mv_is_checked = abap_true.
      lr_range_type = VALUE #( ( sign = 'I' option = 'EQ' low = 'E' ) ).
    ENDIF.

    DATA(lo_collector) = NEW cl_salv_range_tab_collector( ).

    lo_collector->add_ranges_for_name( iv_name = 'TYPE' it_ranges =  lr_range_type ).
    lo_collector->get_collected_ranges( IMPORTING et_named_ranges = DATA(lt_name_range_pairs) ).
    mo_object->set_select_options( it_ranges = lt_name_range_pairs ).

*    sender->mt

*      o_salv_ida->toolbar( )->add_button(
*          iv_fcode                     = 'ZFILTER_E'
*          iv_icon                      = icon_led_red
*          iv_is_checked                = ''
*      ).

*    BREAK-POINT.

  ENDMETHOD.

  METHOD on_double_click.
    TRY.
*    DATA: lv_row TYPE sflight.
        DATA(ls_row) = VALUE zls_cds_log_01( ).
* Daten der geklickten Zeile holen
        eo_row_data->get_row_data( EXPORTING
                                     iv_request_type = if_salv_gui_selection_ida=>cs_request_type-all_fields
                                    its_requested_fields = VALUE #( ( CONV #( 'EXTNUMBER' ) ) )
                                   IMPORTING
                                     es_row =  ls_row ).

* Daten anzeigen
*    cl_salv_ida_show_data_row=>display( iv_text = |Datenfeld: { ev_field_name }|
*                                        is_data = ls_row ).

        mo_app->mo_alv_popup->display( ls_row ).


      CATCH cx_root INTO DATA(lx).
        hlp=>gui_popup( lx ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.



CLASS lcl_app IMPLEMENTATION.

  METHOD factory.

    IF so_app IS NOT BOUND.
      so_app = NEW #( ).
      so_app->mo_selscreen = NEW #( ).
      so_app->mo_selscreen->mo_app = so_app.
      so_app->mo_alv_main = NEW #( ).
      so_app->mo_alv_main->mo_app = so_app.
      so_app->mo_alv_popup = NEW #( ).
      so_app->mo_alv_popup->mo_app = so_app.
    ENDIF.

    result = so_app.

  ENDMETHOD.


  METHOD main.

    mo_alv_main->init( ).
    mo_alv_main->mo_object->fullscreen( )->display( ).

  ENDMETHOD.

ENDCLASS.

*LOAD-OF-PROGRAM.

INITIALIZATION.
  DATA(go_app) = lcl_app=>factory( ).
  go_app->mo_selscreen->on_init( ).

AT SELECTION-SCREEN OUTPUT.
*  go_app->mo_selscreen->on_output(  ).

AT SELECTION-SCREEN.
*  go_app->mo_selscreen->on_screen( ).

START-OF-SELECTION.
  go_app->mo_selscreen->on_start(  ).
