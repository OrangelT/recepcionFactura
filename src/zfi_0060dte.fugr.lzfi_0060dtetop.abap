FUNCTION-POOL zfi_0060dte.                  "MESSAGE-ID ..
DATA ok_code(20).
DATA: go_pdf_object TYPE REF TO cl_gui_html_viewer,
      go_pdf_dialog TYPE REF TO cl_gui_dialogbox_container.
DATA : g_html_container TYPE REF TO cl_gui_custom_container,
       g_html_control   TYPE REF TO  cl_gui_html_viewer.
DATA: lv_url TYPE char255.
DATA: lt_data TYPE STANDARD TABLE OF x255.
CLASS cl_event_handler DEFINITION DEFERRED.

* INCLUDE LZFI_0060DTED...                   " Local class definition

CLASS cl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS
      handler_close FOR EVENT close OF cl_gui_dialogbox_container
        IMPORTING sender.
ENDCLASS.

CLASS cl_event_handler IMPLEMENTATION.
  METHOD handler_close.
    PERFORM handler_close USING sender.
  ENDMETHOD.
ENDCLASS.
