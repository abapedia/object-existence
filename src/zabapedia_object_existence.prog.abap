**********************************************************************
* abapedia object existence
*
* Reads the list of release objects for a given Steampunk release from
* the corresponding repository on abapedia
* https://github.com/orgs/abapedia/repositories?q=steampunk
*
* For example, for Steampunk 2305, the following file is used:
* https://github.com/abapedia/steampunk-2305-api/blob/main/src/_status.json
*
* The list of objects is filtered by status = "RELEASED" and compared
* to the objects existing in this system. Objects with status =
* "DEPRECATED" are ignored.
*
* The result of the comparison is then saved as a JSON file.
* The JSON file can then be uploaded for use in abapedia.org to
* https://github.com/abapedia/object-existence/tree/main/json
**********************************************************************
REPORT zabapedia_object_existence.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS:
  p_repo   TYPE string LOWER CASE DEFAULT 'steampunk-2305-api'.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
PARAMETERS:
  p_path   TYPE string LOWER CASE DEFAULT 'C:\Temp\',
  p_file_o TYPE string LOWER CASE DEFAULT 'on_prem_<release>.json'.
SELECTION-SCREEN END OF BLOCK b2.

TYPES:
  BEGIN OF ty_system,
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
          iv_repo        TYPE string
        RETURNING
          value(rs_data) TYPE ty_check
        RAISING
          zcx_abapgit_ajson_error,

      system
        RETURNING
          value(rs_system) TYPE ty_check-system,

      compare
        IMPORTING
          it_tadir        TYPE ty_check-object_list
        RETURNING
          value(rt_tadir) TYPE ty_check-object_list
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
      lv_url      TYPE string,
      lv_data     TYPE string,
      lv_object   TYPE string,
      lt_objects TYPE string_table,
      ls_object   TYPE ty_tadir,
      li_agent    TYPE REF TO zif_abapgit_http_agent,
      li_response TYPE REF TO zif_abapgit_http_response,
      li_json     TYPE REF TO zif_abapgit_ajson,
      lx_error    TYPE REF TO zcx_abapgit_exception.

    FIELD-SYMBOLS <ls_object> LIKE LINE OF rs_data-object_list.

    lv_url   = |https://raw.githubusercontent.com/abapedia/{ iv_repo }/main/src/_status.json|.

    li_agent = zcl_abapgit_factory=>get_http_agent( ).

    TRY.
        li_response = li_agent->request( lv_url ).

        lv_data = li_response->cdata( ).

      CATCH zcx_abapgit_exception INTO lx_error.
        zcx_abapgit_ajson_error=>raise( lx_error->get_text( ) ).
    ENDTRY.

    lv_data = replace(
      val  = lv_data
      sub  = '/'
      with = '#'
      occ  = 0 ).

    li_json = zcl_abapgit_ajson=>parse( lv_data ).

    lt_objects = li_json->members( '/' ).

    LOOP AT lt_objects INTO lv_object.
      IF li_json->get( |/{ lv_object }/status| ) = 'RELEASED'.
        CLEAR ls_object.
        SPLIT to_upper( lv_object ) AT ',' INTO ls_object-object ls_object-obj_name.

        ls_object-obj_name = replace(
          val  = ls_object-obj_name
          sub  = '#'
          with = '/'
          occ  = 0 ).
        INSERT ls_object INTO TABLE rs_data-object_list.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD system.

    GET TIME STAMP FIELD rs_system-timestamp.

    SELECT component release extrelease FROM cvers INTO CORRESPONDING FIELDS OF TABLE rs_system-components
      WHERE component = 'SAP_BASIS' OR component = 'SAP_ABA' ##SUBRC_OK ##TOO_MANY_ITAB_FIELDS.

  ENDMETHOD.

  METHOD compare.

    DATA lt_tadir LIKE it_tadir.

    FIELD-SYMBOLS <ls_tadir> LIKE LINE OF it_tadir.

    rt_tadir = it_tadir.

    IF it_tadir IS INITIAL.
      zcx_abapgit_ajson_error=>raise( 'Empty object list' ).
    ENDIF.

    SELECT * FROM tadir INTO CORRESPONDING FIELDS OF TABLE lt_tadir
      FOR ALL ENTRIES IN it_tadir
      WHERE pgmid = 'R3TR' AND object = it_tadir-object AND obj_name = it_tadir-obj_name ##TOO_MANY_ITAB_FIELDS.

    IF sy-subrc <> 0.
      zcx_abapgit_ajson_error=>raise( 'No objects found' ).
    ENDIF.

    LOOP AT rt_tadir ASSIGNING <ls_tadir>.
      READ TABLE lt_tadir TRANSPORTING NO FIELDS WITH TABLE KEY
        object   = <ls_tadir>-object
        obj_name = <ls_tadir>-obj_name.
      <ls_tadir>-exists = boolc( sy-subrc = 0 ).
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

    cl_gui_frontend_services=>gui_download(
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
        OTHERS                  = 24 ).
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
      gs_check_in = go_check->load( p_repo ).

      gs_check_out-system = go_check->system( ).

      gs_check_out-object_list = go_check->compare( gs_check_in-object_list ).

      go_check->save(
        iv_file = p_path && p_file_o
        is_data = gs_check_out ).

    CATCH zcx_abapgit_ajson_error INTO gx_error.
      gv_msg = gx_error->get_text( ).
      MESSAGE gv_msg TYPE 'E' DISPLAY LIKE 'I'.
  ENDTRY.
