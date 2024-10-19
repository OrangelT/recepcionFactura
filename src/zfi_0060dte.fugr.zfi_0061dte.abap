FUNCTION zfi_0061dte.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  TABLES
*"      BINARY_TAB
*"----------------------------------------------------------------------
  DATA : g_html_container TYPE REF TO cl_gui_custom_container,
         g_html_control   TYPE REF TO cl_gui_html_viewer.
  DATA: lv_url TYPE char255.

  lt_data[] = binary_tab[].

  IF g_html_container IS INITIAL.
    CREATE OBJECT g_html_container
      EXPORTING
        container_name = 'PDF'.
  ENDIF.
  IF g_html_control IS INITIAL.

    CREATE OBJECT g_html_control
      EXPORTING
        parent = g_html_container.
  ENDIF.

  CLEAR lv_url.

  CALL METHOD g_html_control->load_data(
    EXPORTING
      type                 = 'application'
      subtype              = 'pdf'
    IMPORTING
      assigned_url         = lv_url
    CHANGING
      data_table           = lt_data
    EXCEPTIONS
      dp_invalid_parameter = 1
      dp_error_general     = 2
      cntl_error           = 3
      OTHERS               = 4 ).

  CALL METHOD g_html_control->show_data
    EXPORTING
      url      = lv_url
      in_place = ' '.

ENDFUNCTION.
