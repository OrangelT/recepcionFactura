FUNCTION zfi_0028.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(I_LIFNR) TYPE  LFA1-LIFNR OPTIONAL
*"     REFERENCE(I_BUKRS) TYPE  ZTFI_0074-BUKRS OPTIONAL
*"     REFERENCE(I_XBLNR) TYPE  ZTFI_0074-XBLNR OPTIONAL
*"     REFERENCE(I_TIPODTE) TYPE  ZTFI_0074-TIPODTE OPTIONAL
*"     REFERENCE(I_GLOSA) TYPE  ZGLOSA OPTIONAL
*"     REFERENCE(I_COMMIT) TYPE  ZTFI_0074-STATUS OPTIONAL
*"----------------------------------------------------------------------
  DATA: lv_stcd1 TYPE lfa1-stcd1,
        lv_adrnr TYPE lfa1-adrnr,
        lv_name  TYPE tdobname,
        changed  TYPE boolean.
  DATA: lv_lineas_txt TYPE i.
  DATA: lt_objpack TYPE STANDARD TABLE OF sopcklsti1,
        wa_objpack LIKE LINE OF lt_objpack.

  lc_lifnr   = i_lifnr.
  lc_bukrs   = i_bukrs.
  lc_xblnr   = i_xblnr.
  lc_tipodte = i_tipodte.
  lc_glosa   = i_glosa.

  SELECT  stcd1 adrnr
  INTO (lv_stcd1 , lv_adrnr )
   FROM lfa1 UP TO 1 ROWS
  WHERE lifnr = i_lifnr
  ORDER BY stcd1.
  ENDSELECT.
  CHECK sy-dbcnt = 1.

  CHECK lv_adrnr IS NOT INITIAL.

  TYPE-POOLS: szadr.
  DATA: lv_addr1_complete TYPE szadr_addr1_complete.
  CLEAR lv_addr1_complete.
  DATA:lv_addrnumber TYPE addr1_sel-addrnumber.
  lv_addrnumber = lv_adrnr.

  CALL FUNCTION 'ADDR_GET_COMPLETE'
    EXPORTING
      addrnumber           = lv_addrnumber
      iv_current_comm_data = 'X'
    IMPORTING
      addr1_complete       = lv_addr1_complete.



  DATA:lt_addr1_tab  TYPE STANDARD TABLE OF szadr_addr1_line,
       lt_adtel_tab  TYPE STANDARD TABLE OF szadr_adtel_line,
       lt_adsmtp_tab TYPE STANDARD TABLE OF szadr_adsmtp_line.

  DATA: ln_addr1_tab  LIKE LINE OF lt_addr1_tab,
        ln_adtel_tab  LIKE LINE OF lt_adtel_tab,
        ln_adsmtp_tab LIKE LINE OF lt_adsmtp_tab.

  lt_addr1_tab[] = lv_addr1_complete-addr1_tab[].
  lt_adtel_tab[] = lv_addr1_complete-adtel_tab[].
  lt_adsmtp_tab[] = lv_addr1_complete-adsmtp_tab[].


  DATA: lv_email TYPE string.
  CLEAR lv_email.

  READ TABLE lt_adsmtp_tab INTO ln_adsmtp_tab INDEX 1.
  IF sy-subrc = 0.
    lv_email = ln_adsmtp_tab-adsmtp-smtp_addr.
  ENDIF.

  READ TABLE lt_addr1_tab INTO ln_addr1_tab INDEX 1.
  IF sy-subrc = 0.
    CONCATENATE ln_addr1_tab-data-name1 ln_addr1_tab-data-name2 ln_addr1_tab-data-name3
    ln_addr1_tab-data-name4 INTO lv_nombreprovee.
    CONDENSE lv_nombreprovee.
  ENDIF.

  "-----Envío de e-mail.
  DATA: lt_mailsubject     TYPE sodocchgi1.
  DATA: lt_mailrecipients  TYPE STANDARD TABLE OF somlreci1 WITH HEADER LINE.
  DATA: lt_mailtxt         TYPE STANDARD TABLE OF solisti1     WITH HEADER LINE.
* Recipients
  lt_mailrecipients-rec_type  = 'U'.
  lt_mailrecipients-receiver = lv_email.
  APPEND lt_mailrecipients .
  CLEAR lt_mailrecipients .
* Subject.
  DATA: lv_texto TYPE char200.
  CLEAR lv_texto.
  CONCATENATE 'Doc. Rechazado : ' i_bukrs '/' i_xblnr '/' i_tipodte INTO lv_texto.
  lt_mailsubject-obj_name = 'EMAIL'.
  lt_mailsubject-obj_langu = sy-langu.
  lt_mailsubject-obj_descr =  lv_texto.

  "---->ticket 5000073197 novis
  "---->se busca en tabla conf. correo emisor y extras.

  SELECT * INTO ls_0078
  FROM ztfi_0078 UP TO 1 ROWS
  WHERE bukrs EQ i_bukrs
  ORDER BY PRIMARY KEY.
  ENDSELECT.


  CLEAR: lv_mnttotal, lv_bldat, lc_mnttotal, lv_waers, lc_bldat, lv_ebeln,
         ls_0074.
  SELECT * INTO ls_0074
  FROM ztfi_0074 UP TO 1 ROWS
  WHERE bukrs   EQ i_bukrs
    AND tipodte EQ i_tipodte
    AND xblnr   EQ i_xblnr
    AND lifnr   EQ i_lifnr
  ORDER BY PRIMARY KEY.
  ENDSELECT.
  IF sy-subrc NE 0.
    SELECT * INTO CORRESPONDING FIELDS OF ls_0074
   FROM ztfi_0074fr UP TO 1 ROWS
   WHERE bukrs   EQ i_bukrs
     AND tipodte EQ i_tipodte
     AND xblnr   EQ i_xblnr
     AND stcd1   EQ lv_stcd1
   ORDER BY PRIMARY KEY.
    ENDSELECT.
  ENDIF.
  lv_mnttotal = ls_0074-mnttotal.
  lv_bldat  = ls_0074-bldat.
  lv_waers =  ls_0074-waers.
  lv_ebeln =  ls_0074-ebeln.


  SELECT title_medi INTO lv_tratamiento
  FROM tsad3t UP TO 1 ROWS
  WHERE langu EQ sy-langu
    AND title EQ ln_addr1_tab-data-title
  ORDER BY PRIMARY KEY.
  ENDSELECT.

  SET COUNTRY 'CL'.
  IF lv_mnttotal IS NOT INITIAL.
    WRITE: lv_mnttotal TO lc_mnttotal CURRENCY lv_waers.
    CONDENSE lc_mnttotal.
  ENDIF.
  IF lv_bldat IS NOT INITIAL.
    WRITE: lv_bldat TO lc_bldat DD/MM/YYYY.
  ENDIF.
  SET COUNTRY space.

  "---->texto creado
  CLEAR lv_name.
  lv_name = ls_0078-texto.
  IF lv_name IS INITIAL.
    lv_name = 'ZDTE_EMAIL'.
  ENDIF.
  DATA: lt_lineas TYPE STANDARD TABLE OF tline WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE,
        ls_header TYPE thead.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = 'ST'
      language                = sy-langu
      name                    = lv_name
      object                  = 'TEXT'
    IMPORTING
      header                  = ls_header
    TABLES
      lines                   = lt_lineas
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  IF sy-subrc  = 0.
    FREE lt_mailtxt.
    TRANSLATE ls_0078-url_texto TO LOWER CASE.
    TRANSLATE ls_0078-correo_texto TO LOWER CASE.
    DESCRIBE TABLE lt_lineas LINES lv_lineas_txt.

    CALL FUNCTION 'TEXT_SYMBOL_REPLACE'
      EXPORTING
        endline          = 20
        header           = ls_header
*       INIT             = ' '
        option_dialog    = ' '
        program          = 'SAPLZFI_0028'
        replace_program  = 'X'
        replace_standard = ' '
        replace_system   = 'X'
        replace_text     = 'X'
        startline        = 1
* IMPORTING
*       CHANGED          =
*       NEWHEADER        =
      TABLES
        lines            = lt_lineas.
  ENDIF.
  "---->se agrega correo emisor.
  DATA: lv_sender TYPE soextreci1-receiver.

  DATA: lt_html_text    TYPE STANDARD TABLE OF htmlline,
        lt_cuerpo_email TYPE soli_tab.

  CALL FUNCTION 'CONVERT_ITF_TO_HTML'
    EXPORTING
      i_header       = ls_header
    TABLES
      t_itf_text     = lt_lineas
      t_html_text    = lt_html_text
    EXCEPTIONS
      syntax_check   = 1
      replace        = 2
      illegal_header = 3
      OTHERS         = 4.
  APPEND LINES OF lt_html_text TO lt_cuerpo_email.


  IF ls_0078-sender IS NOT INITIAL.
    lv_sender = ls_0078-sender.

    "--->si existe emisor se envia el correo.
    DESCRIBE TABLE lt_cuerpo_email LINES lv_lineas_txt.
    CLEAR wa_objpack-transf_bin.
    wa_objpack-head_start = 1.
    wa_objpack-head_num = 0.
    wa_objpack-body_start = 1.
    wa_objpack-body_num = lv_lineas_txt.
    wa_objpack-doc_type = 'HTM'.
    APPEND wa_objpack TO lt_objpack.

    DATA: lv_tipo LIKE  soextreci1-adr_typ.

    lv_tipo = 'SMTP'.

    CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
      EXPORTING
        document_data              = lt_mailsubject
        sender_address             = lv_sender
        sender_address_type        = lv_tipo
      TABLES
        packing_list               = lt_objpack
        contents_txt               = lt_cuerpo_email
        receivers                  = lt_mailrecipients
      EXCEPTIONS
        too_many_receivers         = 1
        document_not_sent          = 2
        document_type_not_exist    = 3
        operation_no_authorization = 4
        parameter_error            = 5
        x_error                    = 6
        enqueue_error              = 7
        OTHERS                     = 8.
    IF sy-subrc EQ 0.
      IF i_commit IS INITIAL.
        COMMIT WORK.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFUNCTION.
