REPORT zabappedia_object_existence.

PARAMETERS:
  p_path   TYPE string LOWER CASE DEFAULT 'C:\Temp\',
  p_file_i TYPE string LOWER CASE DEFAULT 'steampunk_2111.json',
  p_file_o TYPE string LOWER CASE DEFAULT 'on_prem_<release>.json'.

TYPES:
  BEGIN OF ty_system,
    sysid      TYPE sy-sysid,
    timestamp  TYPE timestamp,
    components TYPE SORTED TABLE OF cvers WITH UNIQUE DEFAULT KEY,
  END OF ty_system,

  BEGIN OF ty_tadir,
    object   TYPE tadir-object,
    obj_name TYPE tadir-obj_name,
    exists   TYPE abap_bool,
  END OF ty_tadir,

  BEGIN OF ty_check,
    system      TYPE ty_system,
    object_list TYPE SORTED TABLE OF ty_tadir WITH UNIQUE KEY object obj_name,
  END OF ty_check.

CLASS lcl_check DEFINITION.

  PUBLIC SECTION.

    METHODS:
      load
        IMPORTING
          iv_file        TYPE string
        RETURNING
          VALUE(rs_data) TYPE ty_check
        RAISING
          zcx_abapgit_ajson_error,

      system
        RETURNING
          VALUE(rs_system) TYPE ty_check-system,

      compare
        IMPORTING
          it_tadir        TYPE ty_check-object_list
        RETURNING
          VALUE(rt_tadir) TYPE ty_check-object_list
        RAISING
          zcx_abapgit_ajson_error,

      save
        IMPORTING
          iv_file TYPE string
          is_data TYPE ty_check
        RAISING
          zcx_abapgit_ajson_error.

ENDCLASS.

CLASS lcl_check IMPLEMENTATION.

  METHOD load.

    DATA:
      lt_data TYPE string_table,
      lv_data TYPE string,
      lo_json TYPE REF TO zcl_abapgit_ajson.

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = iv_file
      TABLES
        data_tab                = lt_data
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.
    IF sy-subrc <> 0.
      zcx_abapgit_ajson_error=>raise( 'Upload error' ).
    ENDIF.

    CONCATENATE LINES OF lt_data INTO lv_data SEPARATED BY cl_abap_char_utilities=>newline.

    lo_json = zcl_abapgit_ajson=>parse( lv_data ).

    lo_json->to_abap(
      IMPORTING
        ev_container = rs_data ).

  ENDMETHOD.

  METHOD system.

    GET TIME STAMP FIELD rs_system-timestamp.
    rs_system-sysid = sy-sysid.

    SELECT * FROM cvers INTO TABLE rs_system-components
      WHERE component = 'SAP_BASIS' OR component = 'SAP_ABA'.

  ENDMETHOD.

  METHOD compare.

    DATA lt_tadir LIKE it_tadir.

    FIELD-SYMBOLS <ls_tadir> LIKE LINE OF it_tadir.

    IF it_tadir IS INITIAL.
      zcx_abapgit_ajson_error=>raise( 'Empty object list' ).
    ENDIF.

    rt_tadir = it_tadir.

    SELECT * FROM tadir INTO CORRESPONDING FIELDS OF TABLE lt_tadir
      FOR ALL ENTRIES IN it_tadir
      WHERE pgmid = 'R3TR' AND object = it_tadir-object AND obj_name = it_tadir-obj_name.

    IF lt_tadir IS INITIAL.
      zcx_abapgit_ajson_error=>raise( 'No objects found' ).
    ENDIF.

    LOOP AT rt_tadir ASSIGNING <ls_tadir>.
      READ TABLE lt_tadir TRANSPORTING NO FIELDS WITH TABLE KEY
        object   = <ls_tadir>-object
        obj_name = <ls_tadir>-obj_name.
      IF sy-subrc = 0.
        <ls_tadir>-exists = abap_true.
      ELSE.
        <ls_tadir>-exists = abap_false.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD save.

    DATA:
      lt_data TYPE string_table,
      lv_data TYPE string,
      lv_file TYPE string,
      lo_json TYPE REF TO zcl_abapgit_ajson.

    lo_json = zcl_abapgit_ajson=>create_empty( ).

    lo_json->keep_item_order( ).

    lo_json->set(
      iv_path = '/'
      iv_val  = is_data ).

    lv_data = lo_json->stringify( 2 ).

    SPLIT lv_data AT cl_abap_char_utilities=>newline INTO TABLE lt_data.

    lv_file = replace(
      val  = iv_file
      sub  = '<release>'
      with = |{ sy-saprl }| ).

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename                = lv_file
      CHANGING
        data_tab                = lt_data
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24.
    IF sy-subrc <> 0.
      zcx_abapgit_ajson_error=>raise( 'Download error' ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

DATA:
  gx_error     TYPE REF TO zcx_abapgit_ajson_error,
  go_check     TYPE REF TO lcl_check,
  gs_check_in  TYPE ty_check,
  gs_check_out TYPE ty_check,
  gv_msg       TYPE string.

START-OF-SELECTION.

  CREATE OBJECT go_check.

  TRY.
      gs_check_in = go_check->load( p_path && p_file_i ).

      gs_check_out-system = go_check->system( ).

      gs_check_out-object_list = go_check->compare( gs_check_in-object_list ).

      go_check->save(
        iv_file = p_path && p_file_o
        is_data = gs_check_out ).

    CATCH zcx_abapgit_ajson_error INTO gx_error.
      gv_msg = gx_error->get_text( ).
      MESSAGE gv_msg TYPE 'E' DISPLAY LIKE 'I'.
  ENDTRY.
