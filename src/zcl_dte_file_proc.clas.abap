class ZCL_DTE_FILE_PROC definition
  public
  final
  create public .

public section.

  methods GET_PARSE_TABLE
    importing
      !I_SEPARATOR type CHAR01 default ';'
      !I_ANYTABLE type ANY TABLE
      !I_TABNAME type TABNAME
    exporting
      !E_TABLE type ANY TABLE .
  methods FILE_OPEN_DIALOG .
  methods SET_MESSAGE_ERROR
    importing
      !I_MSGID type SYMSGID
      !I_MSGTY type SYMSGTY
      !I_MSGNO type SYMSGNO
      !I_MSGV1 type MSGV1 optional
      !I_MSGV2 type MSGV2 optional
      !I_MSGV3 type MSGV3 optional
      !I_MSGV4 type MSGV4 optional .
  methods ADD_MESSAGE
    importing
      !I_RETURN type BAPIRET2 .
  methods GET_FILENAME
    returning
      value(R_VALUE) type STRING .
  methods SET_FILENAME
    importing
      !I_FILENAME type STRING .
  methods GET_RESULT
    returning
      value(R_VALUE) type BAPIRETURN_T .
  methods READ_FILE
    exporting
      !T_DATA type STANDARD TABLE .
  methods FILE_EXIST
    importing
      !I_FILENAME type STRING
    returning
      value(R_RESULT) type ABAP_BOOL .
protected section.
private section.

  data:
    gt_data TYPE TABLE OF string .
  data GT_RETURN type BAPIRETURN_T .
  data GV_FILENAME type STRING .
ENDCLASS.



CLASS ZCL_DTE_FILE_PROC IMPLEMENTATION.


METHOD ADD_MESSAGE.
  APPEND i_return TO me->gt_return.
ENDMETHOD.


METHOD file_exist.
**//..
  CALL METHOD cl_gui_frontend_services=>file_exist
    EXPORTING
      file                 = i_filename
    RECEIVING
      result               = r_result
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      wrong_parameter      = 3
      not_supported_by_gui = 4
      OTHERS               = 5.
  IF sy-subrc NE 0.
    me->set_message_error( i_msgid = sy-msgid
                           i_msgty = sy-msgty
                           i_msgno = sy-msgno
                           i_msgv1 = sy-msgv1
                           i_msgv2 = sy-msgv2
                           i_msgv3 = sy-msgv3
                           i_msgv4 = sy-msgv4 ).
  ELSE.
    IF r_result IS INITIAL.
      me->set_message_error( i_msgid = 'PC'
                             i_msgty = 'E'
                             i_msgno = '003' ).
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD file_open_dialog.
  DATA: lv_filename  TYPE string,
      lt_filetable TYPE filetable,
      ls_file      TYPE file_table,
      lv_rc        TYPE i.

**//..
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Abrir archivo'
      multiselection          = abap_false
    CHANGING
      file_table              = lt_filetable
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4.
  .

  IF sy-subrc = 0.
    READ TABLE lt_filetable INTO ls_file INDEX 1.
    IF sy-subrc = 0.
      me->gv_filename =  ls_file-filename.
    ENDIF.
  ELSE.
    CASE lv_rc.
      WHEN 1.
*        me->addmessage( EXPORTING return = me->setmessage_error('Error de apertura de Diálogo') ).
        me->set_message_error( i_msgid = 'FES'
                               i_msgty = 'E'
                               i_msgno = '003').
      WHEN 2.
*        me->addmessage( EXPORTING return = me->setmessage_error('Error del Control GUI') ).
        me->set_message_error( i_msgid = 'FES'
                               i_msgty = 'E'
                               i_msgno = '003').
      WHEN 3.
*        me->addmessage( EXPORTING return = me->setmessage_error('No se ha llamado desde el SAP GUI') ).
        me->set_message_error( i_msgid = 'FES'
                               i_msgty = 'E'
                               i_msgno = '003').
      WHEN 4.
*        me->addmessage( EXPORTING return = me->setmessage_error('Funcionalidad no soportada por el GUI') ).
        me->set_message_error( i_msgid = 'FES'
                               i_msgty = 'E'
                               i_msgno = '003').
    ENDCASE.

  ENDIF.
ENDMETHOD.


METHOD get_filename.
  r_value = me->gv_filename.
ENDMETHOD.


METHOD get_parse_table.
  DATA: lt_split  TYPE STANDARD TABLE OF string,
        ldo_data  TYPE REF TO data,
        ldo_data1 TYPE REF TO data.

  FIELD-SYMBOLS: <f_field> TYPE any,
                 <wa>      TYPE any,
                 <table>   TYPE STANDARD TABLE,
                 <lt>      TYPE STANDARD TABLE.
  DATA: lt_data TYPE TABLE OF string.

  lt_data = i_anytable.
* Se crea una estructura generica
  CREATE DATA ldo_data  TYPE STANDARD TABLE OF (i_tabname).
  CREATE DATA ldo_data1 TYPE (i_tabname).
  ASSIGN ldo_data->*  TO <table>.
  ASSIGN ldo_data1->* TO <wa>.

  LOOP AT lt_data INTO DATA(ls_data).
    CLEAR  <wa>.
    SPLIT ls_data AT i_separator INTO TABLE lt_split.

    LOOP AT lt_split INTO DATA(ls_split).
      ASSIGN COMPONENT sy-tabix
          OF STRUCTURE <wa> TO <f_field>.
      IF sy-subrc EQ 0.
        MOVE ls_split TO <f_field>.
      ENDIF.
    ENDLOOP.

    APPEND <wa> TO <table>.
  ENDLOOP.

  IF <table> IS ASSIGNED.
    MOVE <table> TO e_table.
  ENDIF.
ENDMETHOD.


METHOD get_result.
  r_value = me->gt_return[].
ENDMETHOD.


METHOD read_file.
  FIELD-SYMBOLS: <table> TYPE ANY TABLE.

  ASSIGN t_data TO <table>.

**//..
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = me->gv_filename
      filetype                = 'ASC'
    CHANGING
      data_tab                = <table>
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
      not_supported_by_gui    = 17
      error_no_gui            = 18.

  CASE sy-subrc.
    WHEN 0.
      t_data = <table>.
    WHEN 1.
      me->set_message_error( i_msgid = 'FES'
                             i_msgty = 'E'
                             i_msgno = '001').
    WHEN 2.
      me->set_message_error( i_msgid = 'FES'
                             i_msgty = 'E'
                             i_msgno = '001').
    WHEN 3.
      me->set_message_error( i_msgid = 'FES'
                             i_msgty = 'E'
                             i_msgno = '002').
    WHEN 4.
      me->set_message_error( i_msgid = 'PC'
                             i_msgty = 'E'
                             i_msgno = '015').
    WHEN 5.
      me->set_message_error( i_msgid = 'FES'
                             i_msgty = 'E'
                             i_msgno = '004').
    WHEN 6.
      me->set_message_error( i_msgid = 'PC'
                             i_msgty = 'E'
                             i_msgno = '044').
    WHEN 7.
      me->set_message_error( i_msgid = 'FES'
                             i_msgty = 'E'
                             i_msgno = '006').
    WHEN 8.
      me->set_message_error( i_msgid = 'FES'
                             i_msgty = 'E'
                             i_msgno = '007').
    WHEN 9.
      me->set_message_error( i_msgid = 'FES'
                             i_msgty = 'E'
                             i_msgno = '008').
    WHEN 10.
      me->set_message_error( i_msgid = 'FES'
                             i_msgty = 'E'
                             i_msgno = '009').
    WHEN 11.
      me->set_message_error( i_msgid = 'FES'
                             i_msgty = 'E'
                             i_msgno = '010').
    WHEN 12.
      me->set_message_error( i_msgid = 'FES'
                             i_msgty = 'E'
                             i_msgno = '011').
    WHEN 13.
      me->set_message_error( i_msgid = 'FES'
                             i_msgty = 'E'
                             i_msgno = '012').
    WHEN 14.
      me->set_message_error( i_msgid = 'FES'
                             i_msgty = 'E'
                             i_msgno = '013').
    WHEN 15.
      me->set_message_error( i_msgid = 'FES'
                             i_msgty = 'E'
                             i_msgno = '014').
    WHEN 16.
      me->set_message_error( i_msgid = 'FES'
                             i_msgty = 'E'
                             i_msgno = '015').
    WHEN 17.
      me->set_message_error( i_msgid = 'FES'
                             i_msgty = 'E'
                             i_msgno = '003').
    WHEN 18.
      me->set_message_error( i_msgid = 'FES'
                             i_msgty = 'E'
                             i_msgno = '003').
  ENDCASE.

ENDMETHOD.


METHOD set_filename.
  IF me->gv_filename NE i_filename.
    me->gv_filename = i_filename.
  ENDIF.
ENDMETHOD.


METHOD set_message_error.
  DATA: ls_return TYPE bapiret2.

  CLEAR ls_return.
  ls_return-type       = i_msgty.
  ls_return-id         = i_msgid.
  ls_return-number     = i_msgno.
  ls_return-message_v1 = i_msgv1.
  ls_return-message_v2 = i_msgv2.
  ls_return-message_v3 = i_msgv3.
  ls_return-message_v4 = i_msgv4.

  MESSAGE ID i_msgid TYPE i_msgty NUMBER i_msgno
                     WITH i_msgv1 i_msgv2 i_msgv3 i_msgv4 INTO ls_return-message.

  me->add_message( EXPORTING i_return = ls_return ).

ENDMETHOD.
ENDCLASS.
