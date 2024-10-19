*----------------------------------------------------------------------*
***INCLUDE LZFI_0060DTEO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9001 OUTPUT.

*  SET PF-STATUS 'STATUS100'.
*  SET TITLEBAR 'Z_PDFVIEW'.

  CREATE OBJECT go_pdf_dialog
    EXPORTING
      width  = 900
      height = 450.


  CREATE OBJECT go_pdf_object
    EXPORTING
      parent = go_pdf_dialog.

  CLEAR lv_url.

  CALL METHOD go_pdf_object->load_data(
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

  CALL METHOD go_pdf_object->show_data
    EXPORTING
      url      = lv_url
      in_place = abap_true.

  CALL METHOD go_pdf_dialog->set_visible
    EXPORTING
      visible = abap_true.




ENDMODULE.
