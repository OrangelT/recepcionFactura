*****           Implementation of object type ZFI_00001            *****
INCLUDE <object>.
TABLES: ztfi_0074, ztfi_0078.

*&---------------------------------------------------------------------*
*&      Object
*&---------------------------------------------------------------------*
begin_data object. " Do not change.. DATA is generated
* only private members may be inserted into structure private
DATA:
" begin of private,
"   to declare private attributes remove comments and
"   insert private attributes here ...
" end of private,
  BEGIN OF key,
      sociedad LIKE ztfi_0074-bukrs,
      factura LIKE ztfi_0074-xblnr,
      proveedor LIKE ztfi_0074-lifnr,
      tipodte LIKE ztfi_0074-tipodte,
  END OF key,
      _ztfi_0074 LIKE ztfi_0074,
      _ztfi_0078 LIKE ztfi_0078.
end_data object. " Do not change.. DATA is generated

*&---------------------------------------------------------------------*
*&      Method  zemail_proveedor
*&---------------------------------------------------------------------*
begin_method zemail_proveedor changing container.

DATA: email TYPE sza1_d0100-smtp_addr.
DATA: lv_adrnr TYPE lfa1-adrnr.
DATA: lv_email TYPE adr6-smtp_addr.

CLEAR email.

SELECT SINGLE adrnr FROM lfa1 INTO lv_adrnr
  WHERE lifnr = object-key-proveedor.

IF sy-dbcnt = 1.
  SELECT SINGLE smtp_addr FROM adr6 INTO lv_email
    WHERE addrnumber = lv_adrnr.
  IF sy-dbcnt = 1.
    email = lv_email.
    swc_set_element container 'email' email.
  ENDIF.
ENDIF.

end_method.

*&---------------------------------------------------------------------*
*&      Method  zcambiar_status
*&---------------------------------------------------------------------*
begin_method zcambiar_status changing container.

DATA: lv_status TYPE ztfi_0074-status.
DATA: lv_usuario(14) TYPE c.
DATA: lv_usuario_real(12) TYPE c.
DATA: old_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074 WITH HEADER LINE WITH NON-UNIQUE DEFAULT KEY.
FREE old_ztfi_0074.
CLEAR old_ztfi_0074.
DATA: new_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074 WITH HEADER LINE WITH NON-UNIQUE DEFAULT KEY.
FREE new_ztfi_0074.
CLEAR new_ztfi_0074.


lv_status = '6'.

DATA: lv_flag TYPE ztfi_0078-flag_wf_norsii.

CLEAR lv_flag.
SELECT SINGLE flag_wf_norsii
FROM ztfi_0078
INTO lv_flag
WHERE bukrs = object-key-sociedad.
IF sy-subrc = 0.
    IF lv_flag = 'X'.
        lv_status = '9'.
    ENDIF.
ENDIF.


swc_get_element container 'usuario' lv_usuario.
lv_usuario_real = lv_usuario+2(12).

SELECT * FROM ztfi_0074 INTO TABLE old_ztfi_0074
  WHERE bukrs EQ object-key-sociedad
    AND xblnr EQ object-key-factura
    AND lifnr EQ object-key-proveedor
    AND tipodte EQ object-key-tipodte.

IF sy-subrc = 0.

  READ TABLE old_ztfi_0074 INDEX 1.

  IF old_ztfi_0074-status <> lv_status OR
     old_ztfi_0074-aprobador_real <> lv_usuario_real.

    new_ztfi_0074 = old_ztfi_0074.
    new_ztfi_0074-status = lv_status.
    new_ztfi_0074-aprobador_real = lv_usuario_real.
    new_ztfi_0074-fechaaprob = sy-datum.
    new_ztfi_0074-horaaprob = sy-uzeit.
    APPEND new_ztfi_0074.

    CALL FUNCTION 'ZTFI_0074_WRITE_DOCUMENT'
      IN UPDATE TASK
      EXPORTING
        objectid      = new_ztfi_0074+0(35)
        tcode         = 'Wf:ZFI_00001'
        utime         = sy-uzeit
        udate         = sy-datum
        username      = sy-uname
        upd_ztfi_0074 = 'U'
      TABLES
        xztfi_0074    = new_ztfi_0074
        yztfi_0074    = old_ztfi_0074.

  ENDIF.
ENDIF.

UPDATE ztfi_0074 SET status = lv_status
                     fechaaprob = sy-datum
                     horaaprob = sy-uzeit
                     aprobador_real = lv_usuario_real
                 WHERE bukrs = object-key-sociedad AND
                       xblnr = object-key-factura AND
                       lifnr = object-key-proveedor AND
                       tipodte = object-key-tipodte.

end_method.

*&---------------------------------------------------------------------*
*&      Method  zenviar_idoc_rechazo
*&---------------------------------------------------------------------*
begin_method zenviar_idoc_rechazo changing container.

"---->validación rechazo con idoc.
DATA: lv_glosa TYPE char100.
DATA: lv_flag TYPE ztfi_0078-flag_wf_norsii.

CLEAR lv_flag.
SELECT SINGLE flag_wf_norsii
FROM ztfi_0078
INTO lv_flag
WHERE bukrs = object-key-sociedad.
IF sy-subrc = 0.
    IF lv_flag = 'X'.
      CLEAR lv_glosa.
      swc_get_element container 'motivo_rechazo' lv_glosa.
      IF lv_glosa IS INITIAL.
        lv_glosa = 'Su factura fue rechazada'.
      ENDIF.
      update ztfi_0074
      set glosa = lv_glosa
      where bukrs =  object-key-sociedad AND
            xblnr = object-key-factura AND
            lifnr = object-key-proveedor AND
            tipodte = object-key-tipodte.

    ENDIF.
ENDIF.
check lv_flag is INITIAL.
"---->fin.


TYPE-POOLS: szadr.
DATA: e_salida TYPE zst_dte_ack.
DATA: i_entrada TYPE ztfi_0074.
DATA: lv_paval TYPE t001z-paval.
DATA: lv_stcd1 TYPE lfa1-stcd1,
      lv_adrnr TYPE lfa1-adrnr.
DATA: lv_addrnumber TYPE addr1_sel-addrnumber.
DATA: lv_addr1_complete TYPE szadr_addr1_complete.
DATA: lt_addr1_tab TYPE STANDARD TABLE OF szadr_addr1_line,
      lt_adtel_tab TYPE STANDARD TABLE OF szadr_adtel_line,
      lt_adsmtp_tab TYPE STANDARD TABLE OF szadr_adsmtp_line.
DATA: ln_addr1_tab LIKE LINE OF lt_addr1_tab,
      ln_adtel_tab LIKE LINE OF lt_adtel_tab,
      ln_adsmtp_tab LIKE LINE OF lt_adsmtp_tab.
DATA: lv_monto TYPE string.


SELECT SINGLE * FROM ztfi_0074 INTO i_entrada
  WHERE bukrs =  object-key-sociedad AND
        xblnr = object-key-factura AND
        lifnr = object-key-proveedor AND
        tipodte = object-key-tipodte.

CHECK sy-dbcnt = 1.

SELECT SINGLE paval FROM t001z INTO lv_paval
  WHERE bukrs = object-key-sociedad AND
        party = 'TAXNR'.

SELECT SINGLE stcd1 adrnr FROM lfa1 INTO (lv_stcd1 , lv_adrnr)
  WHERE lifnr = object-key-proveedor.

REPLACE ALL OCCURRENCES OF '.' IN lv_paval WITH space.
REPLACE ALL OCCURRENCES OF '.' IN lv_stcd1 WITH space.

CLEAR e_salida.
e_salida-bukrs = object-key-sociedad.
e_salida-rutreceptor = lv_stcd1.
e_salida-rutrecibe = lv_paval.
e_salida-rutreceptor = lv_paval.
lv_addrnumber = lv_adrnr.

CLEAR lv_addr1_complete.

CALL FUNCTION 'ADDR_GET_COMPLETE'
  EXPORTING
    addrnumber           = lv_addrnumber
    iv_current_comm_data = 'X'
  IMPORTING
    addr1_complete       = lv_addr1_complete.

lt_addr1_tab[] = lv_addr1_complete-addr1_tab[].
lt_adtel_tab[] = lv_addr1_complete-adtel_tab[].
lt_adsmtp_tab[] = lv_addr1_complete-adsmtp_tab[].

READ TABLE lt_addr1_tab INTO ln_addr1_tab INDEX 1.
IF sy-subrc = 0.
  e_salida-nmbcontacto = ln_addr1_tab-data-name1.
ENDIF.

*e_salida-tipodte = '33'.
e_salida-tipodte = i_entrada-tipodte.
e_salida-folio = i_entrada-xblnr.
CONCATENATE  i_entrada-bldat+0(4) '-' i_entrada-bldat+4(2) '-' i_entrada-bldat+6(2) INTO e_salida-fchemis.
e_salida-rutemisor = lv_stcd1.
lv_monto = i_entrada-wrbtr.
e_salida-mnttotal = lv_monto.
e_salida-codenvio = i_entrada-docnum.
e_salida-idrespuesta = i_entrada-docnum.
e_salida-estadodte = '2'.

CLEAR lv_glosa.
swc_get_element container 'motivo_rechazo' lv_glosa.
IF lv_glosa IS NOT INITIAL.
  e_salida-estadodteglosa = lv_glosa.
ELSE.
  e_salida-estadodteglosa = 'Su factura fue rechazada'.
ENDIF.

CALL FUNCTION 'ZFI_0008DTE'
  EXPORTING
    acknowledgment = e_salida
    commit         = ' '.

end_method.

*&---------------------------------------------------------------------*
*&      Method  zgenerar_attach_pdf
*&---------------------------------------------------------------------*
begin_method zgenerar_attach_pdf changing container.

DATA: wa_ztfi_0078 TYPE ztfi_0078.
DATA: lv_pdf TYPE xstring.
DATA: lv_subscriber TYPE char100,
      lv_username LIKE lv_subscriber,
      lv_password LIKE lv_subscriber,
      lv_sender LIKE lv_subscriber,
      lv_documenttype LIKE lv_subscriber,
      lv_documentnumber LIKE lv_subscriber.
DATA: lv_paval TYPE t001z-paval.
DATA: lv_stcd1 TYPE lfa1-stcd1.
DATA: lv_header TYPE swr_att_header.
DATA: lv_workitem TYPE swr_struct-workitemid.
DATA: lv_att_id TYPE swr_att_id.
DATA: lv_rut_emisor TYPE stcd1.
DATA: lv_rut_receptor TYPE zrutrecep.

*data a type i.
*a = 0.
*while a = 0.
*endwhile.
SELECT SINGLE * FROM ztfi_0078 INTO wa_ztfi_0078
  WHERE bukrs = object-key-sociedad.

IF sy-dbcnt = 1.
  IF wa_ztfi_0078-visupdf = 'A'. "Archivo_PDF

     CLEAR lv_pdf.
     CLEAR: lv_paval, lv_rut_emisor, lv_rut_receptor.

    SELECT SINGLE paval FROM t001z INTO lv_paval
    WHERE bukrs = object-key-sociedad AND
          party = 'TAXNR'.
      lv_rut_receptor =  lv_paval.
      DATA: lv_guion TYPE c,
            lv_guion1 TYPE c.

      SPLIT lv_paval AT '-' INTO lv_guion lv_guion1.


    SELECT SINGLE stcd1 FROM ztfi_0074 INTO lv_rut_emisor
    WHERE bukrs = object-key-sociedad
      AND xblnr = object-key-factura
      AND tipodte = object-key-tipodte
      AND lifnr = object-key-proveedor.

      REPLACE ALL OCCURRENCES OF '.' IN lv_rut_receptor WITH space.
      CONDENSE lv_rut_receptor NO-GAPS.
      CONCATENATE lv_rut_receptor '-' lv_guion1 INTO lv_rut_receptor.


     CALL METHOD zcl_dte_monitor=>ver_pdf_arch
                  EXPORTING
                    xblnr   = object-key-factura
                    bukrs   = object-key-sociedad
                    tipo_dte = object-key-tipodte
                    rut_emisor   = lv_rut_emisor
                    rut_receptor = lv_rut_receptor
                    fondo = 'X'
                  IMPORTING
                    pdf     = lv_pdf.
     IF sy-subrc = 0.
       CLEAR lv_header.
       swc_get_element container 'workitem' lv_workitem.
       lv_header-file_type = 'B'.
       lv_header-file_name = 'Factura'.
       lv_header-file_extension  = 'PDF'.
       lv_header-language = sy-langu.
       CLEAR lv_att_id.

       CALL FUNCTION 'SAP_WAPI_ATTACHMENT_ADD'
       EXPORTING
         workitem_id = lv_workitem
         att_header  = lv_header
         att_bin     = lv_pdf
       IMPORTING
         att_id      = lv_att_id.

     CHECK lv_att_id IS NOT INITIAL.

"     swc_set_element container 'sofm' lv_att_id-doc_id.
"      swc_create_object sofm 'SOFM' lv_att_id-doc_id.
"      SWC_SET_ELEMENT CONTAINER 'PDF' sofm.
    ENDIF.

  ELSE.
  CLEAR lv_pdf.
  CLEAR: lv_paval,
         lv_subscriber,
        lv_username,
        lv_password,
        lv_sender,
        lv_documenttype ,
        lv_documentnumber.

  SELECT SINGLE paval FROM t001z INTO lv_paval
    WHERE bukrs = object-key-sociedad AND
          party = 'TAXNR'.

  IF sy-dbcnt = 1.
    lv_sender = lv_paval.
    REPLACE ALL OCCURRENCES OF '.' IN lv_sender WITH space.
    CONDENSE lv_sender NO-GAPS.
  ENDIF.

  lv_subscriber = wa_ztfi_0078-suscriber.
  lv_username = wa_ztfi_0078-usuario.
  lv_password = wa_ztfi_0078-password.
  lv_documenttype =  object-key-tipodte.
  lv_documentnumber = object-key-factura.

  SELECT SINGLE stcd1 FROM lfa1 INTO lv_stcd1
    WHERE lifnr = object-key-proveedor.
  IF sy-dbcnt = 1.
    lv_sender = lv_stcd1.
    REPLACE ALL OCCURRENCES OF '.' IN lv_sender WITH space.
    CONDENSE lv_sender NO-GAPS.
  ENDIF.

*  CALL FUNCTION 'ZFI_0009'
*    EXPORTING
*      i_subscriber       = lv_subscriber
*      i_username         = lv_username
*      i_password         = lv_password
*      i_sender           = lv_sender
*      i_documenttype     = lv_documenttype
*      i_documentnumber   = lv_documentnumber
*      i_format           = 'PDFG'
*    IMPORTING
*      e_pdf              = lv_pdf
*    EXCEPTIONS
*      error_comunicacion = 1
*      OTHERS             = 2.

*  IF sy-subrc = 0.
*
*    CLEAR lv_header.
*    swc_get_element container 'workitem' lv_workitem.
*    lv_header-file_type = 'B'.
*    lv_header-file_name = 'test'.
*    lv_header-file_extension  = 'PDF'.
*    lv_header-language = sy-langu.
*    CLEAR lv_att_id.
*
*    CALL FUNCTION 'SAP_WAPI_ATTACHMENT_ADD'
*      EXPORTING
*        workitem_id = lv_workitem
*        att_header  = lv_header
*        att_bin     = lv_pdf
*      IMPORTING
*        att_id      = lv_att_id.
*
*    CHECK lv_att_id IS NOT INITIAL.
*
*    swc_set_element container 'sofm' lv_att_id-doc_id.
*
*  ENDIF.
  ENDIF.
ENDIF.
end_method.

*&---------------------------------------------------------------------*
*&      get_table_property ztfi_0074
*&---------------------------------------------------------------------*
get_table_property ztfi_0074.

DATA subrc LIKE sy-subrc.
PERFORM select_table_ztfi_0074 USING subrc.
IF subrc NE 0.
  exit_object_not_found.
ENDIF.

end_property.

*&---------------------------------------------------------------------*
*&      FORM select_table_ztfi_0074
*&---------------------------------------------------------------------*
FORM select_table_ztfi_0074 USING subrc LIKE sy-subrc.

  IF object-_ztfi_0074-mandt IS INITIAL AND
     object-_ztfi_0074-bukrs IS INITIAL AND
     object-_ztfi_0074-xblnr IS INITIAL AND
     object-_ztfi_0074-lifnr IS INITIAL AND
     object-_ztfi_0074-tipodte IS INITIAL.
    SELECT SINGLE * FROM ztfi_0074 CLIENT SPECIFIED
      WHERE mandt = sy-mandt
        AND bukrs = object-key-sociedad
        AND xblnr = object-key-factura
        AND lifnr = object-key-proveedor
        AND tipodte = object-key-tipodte.
    subrc = sy-subrc.
    IF subrc NE 0. EXIT. ENDIF.
    object-_ztfi_0074 = ztfi_0074.
  ELSE.
    subrc = 0.
    ztfi_0074 = object-_ztfi_0074.
  ENDIF.

ENDFORM.                    "SELECT_TABLE_ZTFI_0074

*&---------------------------------------------------------------------*
*&      Method  zcambiar_status_aprob
*&---------------------------------------------------------------------*
begin_method zcambiar_status_aprob changing container.

DATA: lv_status TYPE ztfi_0074-status.
DATA: lv_usuario(14) TYPE c.
DATA: lv_usuario_real(12) TYPE c.
DATA: old_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074 WITH HEADER LINE WITH NON-UNIQUE DEFAULT KEY.
FREE old_ztfi_0074.
CLEAR old_ztfi_0074.
DATA: new_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074 WITH HEADER LINE WITH NON-UNIQUE DEFAULT KEY.
FREE new_ztfi_0074.
CLEAR new_ztfi_0074.

CLEAR lv_status.
lv_status = '3'.
swc_get_element container 'usuario' lv_usuario.
lv_usuario_real = lv_usuario+2(12).

SELECT * FROM ztfi_0074 INTO TABLE old_ztfi_0074
  WHERE bukrs EQ object-key-sociedad
    AND xblnr EQ object-key-factura
    AND lifnr EQ object-key-proveedor
    AND tipodte EQ object-key-tipodte.

IF sy-subrc = 0.

  READ TABLE old_ztfi_0074 INDEX 1.

  IF old_ztfi_0074-status <> lv_status OR
     old_ztfi_0074-aprobador_real <> lv_usuario_real.

    new_ztfi_0074 = old_ztfi_0074.
    new_ztfi_0074-status = lv_status.
    new_ztfi_0074-aprobador_real = lv_usuario_real.
    new_ztfi_0074-fechaaprob = sy-datum.
    new_ztfi_0074-horaaprob = sy-uzeit.
    APPEND new_ztfi_0074.

    CALL FUNCTION 'ZTFI_0074_WRITE_DOCUMENT'
      IN UPDATE TASK
      EXPORTING
        objectid      = new_ztfi_0074+0(35)
        tcode         = 'Wf:ZFI_00001'
        utime         = sy-uzeit
        udate         = sy-datum
        username      = sy-uname
        upd_ztfi_0074 = 'U'
      TABLES
        xztfi_0074    = new_ztfi_0074
        yztfi_0074    = old_ztfi_0074.

  ENDIF.
ENDIF.

UPDATE ztfi_0074 SET status = lv_status
                     fechaaprob = sy-datum
                     horaaprob = sy-uzeit
                     aprobador_real = lv_usuario_real
                 WHERE bukrs = object-key-sociedad AND
                       xblnr = object-key-factura AND
                       lifnr = object-key-proveedor AND
                       tipodte = object-key-tipodte.

end_method.

*&---------------------------------------------------------------------*
*&      PROPERTY ztfi_0078
*&---------------------------------------------------------------------*
get_table_property ztfi_0078.

DATA subrc LIKE sy-subrc.
PERFORM select_table_ztfi_0078 USING subrc.
IF subrc NE 0.
  exit_object_not_found.
ENDIF.

end_property.

*&---------------------------------------------------------------------*
*&      FORM select_table_ztfi_0074
*&---------------------------------------------------------------------*
FORM select_table_ztfi_0078 USING subrc LIKE sy-subrc.

  IF object-_ztfi_0078-mandt IS INITIAL AND
     object-_ztfi_0078-bukrs IS INITIAL.
    SELECT SINGLE * FROM ztfi_0078 CLIENT SPECIFIED
        WHERE mandt = sy-mandt
        AND bukrs = object-key-sociedad.
    subrc = sy-subrc.
    IF subrc NE 0. EXIT. ENDIF.
    object-_ztfi_0078 = ztfi_0078.
  ELSE.
    subrc = 0.
    ztfi_0078 = object-_ztfi_0078.
  ENDIF.

ENDFORM.                    "SELECT_TABLE_ZTFI_0078

*&---------------------------------------------------------------------*
*&      Method  zenvio_email_prov
*&---------------------------------------------------------------------*

begin_method zenvio_email_prov changing container.
DATA:
      ilifnr TYPE lfa1-lifnr,
      ibukrs TYPE ztfi_0074-bukrs,
      ixblnr TYPE ztfi_0074-xblnr,
      itipodte TYPE ztfi_0074-tipodte,
      icommit TYPE ztfi_0074-status.
  swc_get_element container 'ILifnr' ilifnr.
  swc_get_element container 'IBukrs' ibukrs.
  swc_get_element container 'IXblnr' ixblnr.
  swc_get_element container 'ITipodte' itipodte.
  swc_get_element container 'ICommit' icommit.
  CALL FUNCTION 'ZFI_0028'
    EXPORTING
      i_lifnr = ilifnr
      i_bukrs = ibukrs
      i_xblnr = ixblnr
      i_tipodte = itipodte
      i_commit = icommit
    EXCEPTIONS
      OTHERS = 01.
  CASE sy-subrc.
    WHEN 0.            " OK
    WHEN OTHERS.       " to be implemented
  ENDCASE.
end_method.
