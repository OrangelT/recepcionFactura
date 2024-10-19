FUNCTION zfi_0062dte.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(GV_SIZE) TYPE  I OPTIONAL
*"     VALUE(PATH) TYPE  STRING OPTIONAL
*"     VALUE(I_DATOS) TYPE  ZEFI_0026 OPTIONAL
*"  EXPORTING
*"     VALUE(E_PATH) TYPE  STRING
*"  TABLES
*"      BINARY_TAB
*"----------------------------------------------------------------------
  DATA: activex_is_enabled TYPE flag.
  DATA: sepa TYPE string.
  DATA: filename TYPE string.
  lt_data[] = binary_tab[].
*  CALL FUNCTION 'ALEWEB_DOWNLOAD'
*    EXPORTING
*      data_len          = gv_size
*      mime_type         = 'application/pdf'
*      data_type         = '<b>RAW</b>'
*    TABLES
*      data_tab          = lt_data
*    EXCEPTIONS
*      its_not_available = 1
*      OTHERS            = 2.
*  IF sy-subrc <> 0.

  IF  cl_gui_object=>www_active IS INITIAL.
    CALL FUNCTION 'GUI_HAS_ACTIVEX'
      IMPORTING
        return = activex_is_enabled.

    IF NOT activex_is_enabled IS INITIAL. "Si es X es Windows
      sepa = '\'.
    ELSE.
      sepa = '/'.
    ENDIF.
    IF path IS INITIAL.
      CALL METHOD cl_gui_frontend_services=>directory_browse
        EXPORTING
          window_title         = 'Directorio'
*         initial_folder       =
        CHANGING
          selected_folder      = path
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          not_supported_by_gui = 3
          OTHERS               = 4.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.


    ENDIF.
  ELSE.
    CALL FUNCTION 'GUI_HAS_ACTIVEX'
      IMPORTING
        return = activex_is_enabled.

    IF NOT activex_is_enabled IS INITIAL. "Si es X es Windows
      sepa = '\'.
      path = 'Z:\'.
    ELSE.
      sepa = '/'.
      path = '/Z/'.
    ENDIF.
  ENDIF.

  filename = |{ path }{ sepa }{ i_datos-tipodte }_{ i_datos-xblnr }_{ i_datos-lifnr }.pdf|.


  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      bin_filesize            = gv_size
      filename                = filename
      filetype                = 'BIN'
*     append                  = SPACE
*     write_field_separator   = SPACE
*     header                  = '00'
*     trunc_trailing_blanks   = SPACE
*     write_lf                = 'X'
*     col_select              = SPACE
*     col_select_mask         = SPACE
*     dat_mode                = SPACE
*     confirm_overwrite       = SPACE
*     no_auth_check           = SPACE
*     codepage                = SPACE
*     ignore_cerr             = ABAP_TRUE
*     replacement             = '#'
*     write_bom               = SPACE
*     trunc_trailing_blanks_eol = 'X'
*     wk1_n_format            = SPACE
*     wk1_n_size              = SPACE
*     wk1_t_format            = SPACE
*     wk1_t_size              = SPACE
*     show_transfer_status    = 'X'
*     fieldnames              =
*     write_lf_after_last_line  = 'X'
*     virus_scan_profile      = '/SCET/GUI_DOWNLOAD'
*    IMPORTING
*     filelength              =
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
*   Implement suitable error handling here
  ENDIF.
  e_path = path.
  CALL METHOD cl_gui_frontend_services=>execute
    EXPORTING
      document               = filename
      minimized              = 'X'
*     synchronous            =
      operation              = 'OPEN'
    EXCEPTIONS
      cntl_error             = 1
      error_no_gui           = 2
      bad_parameter          = 3
      file_not_found         = 4
      path_not_found         = 5
      file_extension_unknown = 6
      error_execute_failed   = 7
      synchronous_failed     = 8
      not_supported_by_gui   = 9
      OTHERS                 = 10.
  IF sy-subrc <> 0.
*     Implement suitable error handling here
  ENDIF.





*  ENDIF.

ENDFUNCTION.
