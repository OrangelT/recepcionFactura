FUNCTION ZFI_0010.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(I_LIFNR) TYPE  LFA1-LIFNR OPTIONAL
*"     REFERENCE(I_BUKRS) TYPE  ZTFI_0074-BUKRS OPTIONAL
*"     REFERENCE(I_XBLNR) TYPE  ZTFI_0074-XBLNR OPTIONAL
*"     REFERENCE(I_TIPODTE) TYPE  ZTFI_0074-TIPODTE OPTIONAL
*"     REFERENCE(I_GLOSA) TYPE  STRING OPTIONAL
*"     REFERENCE(I_COMMIT) TYPE  ZTFI_0074-STATUS OPTIONAL
*"----------------------------------------------------------------------
DATA: lv_stcd1 TYPE lfa1-stcd1,
      lv_adrnr TYPE lfa1-adrnr.

  SELECT SINGLE stcd1 adrnr
  FROM lfa1
  INTO (lv_stcd1 , lv_adrnr)
  WHERE lifnr = i_lifnr.
  CHECK sy-dbcnt = 1.

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



  DATA:lt_addr1_tab TYPE STANDARD TABLE OF szadr_addr1_line,
       lt_adtel_tab TYPE STANDARD TABLE OF szadr_adtel_line,
       lt_adsmtp_tab TYPE STANDARD TABLE OF szadr_adsmtp_line.

  DATA: ln_addr1_tab LIKE LINE OF lt_addr1_tab,
        ln_adtel_tab LIKE LINE OF lt_adtel_tab,
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
  DATA: wa_ztfi_0078 TYPE ztfi_0078.

  SELECT SINGLE *
  FROM ztfi_0078
  INTO wa_ztfi_0078
  WHERE bukrs = i_bukrs.

  "---->texto creado
  DATA: lt_lineas TYPE STANDARD TABLE OF tline WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = 'ST'
      language                = sy-langu
      name                    = 'ZDTE_EMAIL'
      object                  = 'TEXT'
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
    TRANSLATE wa_ztfi_0078-url_texto TO LOWER CASE.
    TRANSLATE wa_ztfi_0078-correo_texto TO LOWER CASE.

    LOOP AT lt_lineas.
      CLEAR lt_mailtxt.
      lt_mailtxt = lt_lineas-tdline.

      REPLACE ALL OCCURRENCES OF '(GLOSA)' IN lt_mailtxt WITH i_glosa.
      REPLACE ALL OCCURRENCES OF '(LINK)'  IN lt_mailtxt WITH wa_ztfi_0078-url_texto.
      REPLACE ALL OCCURRENCES OF '(CORREO)' IN lt_mailtxt WITH wa_ztfi_0078-correo_texto.

      APPEND lt_mailtxt.
    ENDLOOP.
  ENDIF.
  "---->se agrega correo emisor.
  DATA: lv_sender TYPE soextreci1-receiver.


  IF wa_ztfi_0078-sender IS NOT INITIAL.
    lv_sender = wa_ztfi_0078-sender.

    "--->si existe emisor se envia el correo.
    DATA: lv_lineas_txt TYPE i.
    DATA: lt_objpack TYPE STANDARD TABLE OF sopcklsti1,
          wa_objpack LIKE LINE OF lt_objpack.

    DESCRIBE TABLE lt_mailtxt LINES lv_lineas_txt.
    CLEAR wa_objpack-transf_bin.
    wa_objpack-head_start = 1.
    wa_objpack-head_num = 0.
    wa_objpack-body_start = 1.
    wa_objpack-body_num = lv_lineas_txt.
    wa_objpack-doc_type = 'RAW'.
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
        contents_txt               = lt_mailtxt
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
      if i_commit is INITIAL.
        COMMIT WORK.
      endif.
    ENDIF.
  ENDIF.

ENDFUNCTION.
