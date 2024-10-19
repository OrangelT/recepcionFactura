*----------------------------------------------------------------------*
***INCLUDE LZMM_0021F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_INICIALIZAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IDOC_CONTRL[]  text
*----------------------------------------------------------------------*
FORM f_inicializar          USING   pe_idoc_contrl  TYPE zbc_t_edidc.

  DATA:   r_idoc_contrl TYPE edidc.
* Tipo base incorrecto
  LOOP AT pe_idoc_contrl INTO r_idoc_contrl.
    IF r_idoc_contrl-idoctp <> co_zid_idoc.
      RAISE wrong_function_called.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " F_INICIALIZAR
*&---------------------------------------------------------------------*
*&      Form  F_GRABAR_DATOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IDOC_DATA[]  text
*      <--P_IDOC_STATUS[]  text
*----------------------------------------------------------------------*
FORM f_grabar_datos          USING     u_idoc_data   TYPE zbc_t_edidd
                             CHANGING  c_idoc_status TYPE zbc_t_edids.
  DATA: ls_idoc_data     TYPE edidd,
        ls_idoc_status   TYPE bdidocstat,
        lv_docnum_output LIKE edidd-docnum.
  DATA: lv_error TYPE ty_boolean.
  DATA: lv_item_ant TYPE ztfi_0075-texto.
  DATA: lv_subtot.
  DATA: lv_err_pos.
  DATA: sw_imp TYPE i.
  DATA: sw_rf TYPE  i.
  DATA: lv_pos TYPE i.

  READ TABLE u_idoc_data INTO ls_idoc_data INDEX 1.
  IF sy-subrc = 0.
    gv_docnum = ls_idoc_data-docnum.
  ENDIF.

  lv_error = co_false.
  lv_err_pos = co_false.
  dte_relevante = co_true.
  sw_imp = 0.
  sw_rf  = 0.
  lv_pos = 0.

  CLEAR wa_cabecera. CLEAR wa_detalle. CLEAR wa_tfi_0074fr. REFRESH it_detalle. REFRESH it_recref.
  REFRESH it_otroimpuesto.
  LOOP AT u_idoc_data INTO ls_idoc_data.

    CLEAR: wa_recref,
           wa_otroimpuesto.

    CASE ls_idoc_data-segnam.
      WHEN co_z1mm_dte_encabezado."Cabecera
        wa_z1mm_dte_encabezado  = ls_idoc_data-sdata.
        PERFORM fill_encabezado USING ls_idoc_data-docnum CHANGING lv_error c_idoc_status.
      WHEN co_z1mm_dte_encabezado2.
        wa_z1mm_dte_encabezado2 = ls_idoc_data-sdata.
        PERFORM fill_encabezado2.
      WHEN co_z1mm_dte_montopago. "Cabecera
        wa_z1mm_dte_montopago   = ls_idoc_data-sdata.
        PERFORM fill_montopago.
      WHEN co_z1mm_dte_impret. "Cabecera
        wa_z1mm_dte_impret      = ls_idoc_data-sdata.
        PERFORM fill_impret.
      WHEN co_z1mm_dte_impotrmon."Cabecera
        wa_z1mm_dte_impotrmon   = ls_idoc_data-sdata.
        PERFORM fill_impotrmon.
      WHEN co_z1mm_dte_refer.
        wa_z1mm_dte_refer       = ls_idoc_data-sdata.
        PERFORM fill_refer CHANGING lv_error c_idoc_status.
      WHEN co_z1mm_dte_detalle.
        ADD 1 TO lv_pos.
        wa_z1mm_dte_detalle     = ls_idoc_data-sdata.

**        IF NOT lv_item_ant IS INITIAL AND lv_item_ant <> wa_z1mm_dte_detalle-nmbitem.
**          IF NOT wa_detalle IS INITIAL.
**            IF lv_err_pos = co_false.
**              APPEND wa_detalle TO it_detalle.
**              CLEAR wa_detalle.
**            ELSE.
**              lv_err_pos = co_false.
**            ENDIF.
**          ENDIF.
**        ENDIF.

        PERFORM fill_detalle USING    lv_pos
                             CHANGING lv_err_pos.

**        lv_item_ant = wa_detalle-texto.
      WHEN co_z1mm_dte_det_codl.
        wa_z1mm_dte_det_codl       = ls_idoc_data-sdata.
        PERFORM fill_pos.

      WHEN co_z1mm_dte_subtot.
        wa_z1mm_dte_subtot      = ls_idoc_data-sdata.
        PERFORM fill_subtot.

      WHEN co_z1mm_dte_filename.
        wa_z1mm_dte_filename     = ls_idoc_data-sdata.
        wa_cabecera-filename = wa_z1mm_dte_filename-filename.

      WHEN co_z1mm_dte_ted.
    ENDCASE.

    IF NOT wa_recref IS INITIAL.
      sw_rf = sw_rf + 1.
      wa_recref-pos = sw_rf.
      PERFORM convert_alpha_input USING      wa_recref-pos
                           CHANGING   wa_recref-pos.
      APPEND wa_recref TO it_recref.

      IF wa_refhes IS NOT INITIAL.
        sw_rf = sw_rf + 1.
        wa_refhes-pos = sw_rf.
        PERFORM convert_alpha_input USING      wa_refhes-pos
                            CHANGING   wa_refhes-pos.
        APPEND wa_refhes TO it_recref.
      ENDIF.

      CLEAR: wa_recref,
             wa_refhes.
    ENDIF.

    IF NOT wa_otroimpuesto IS INITIAL. "llenar otros impuestos
      sw_imp = sw_imp + 1.
      wa_otroimpuesto-pos = sw_imp.
      APPEND wa_otroimpuesto TO it_otroimpuesto.
      CLEAR wa_otroimpuesto.
    ENDIF.

  ENDLOOP.

*  IF NOT wa_detalle IS INITIAL.
*    IF lv_err_pos = co_false.
*      APPEND wa_detalle TO it_detalle.
*      CLEAR wa_detalle.
*    ENDIF.
*  ENDIF.

  IF lv_error = co_false AND dte_relevante = co_true.
    PERFORM grabar_tablas CHANGING c_idoc_status.
  ENDIF.

ENDFORM.                    " F_GRABAR_DATOS
*&---------------------------------------------------------------------*
*&      Form  FILL_ENCABEZADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_encabezado USING u_docnum CHANGING c_error c_idoc_status TYPE zbc_t_edids.

  DATA: ls_idoc_status TYPE bdidocstat.
**Tabla de cabecera
*wa_z1mm_dte_encabezado
  DATA: lv_rut TYPE string.
  DATA: wa_mensaje TYPE char256.
  DATA: lv_folio TYPE i.

  wa_cabecera-tipodte = wa_z1mm_dte_encabezado-tipodte.

  IF  wa_z1mm_dte_encabezado-tipodte = '33'  OR
      wa_z1mm_dte_encabezado-tipodte = '34'  OR
      wa_z1mm_dte_encabezado-tipodte = '61'  OR
      wa_z1mm_dte_encabezado-tipodte = '56'  OR
      wa_z1mm_dte_encabezado-tipodte = '110' OR
      wa_z1mm_dte_encabezado-tipodte = '111' OR
      wa_z1mm_dte_encabezado-tipodte = '112' .

    SELECT SINGLE bukrs INTO wa_cabecera-bukrs
    FROM t001z
    WHERE party = co_taxnr
      AND paval = wa_z1mm_dte_encabezado-rutrecep.
    IF sy-subrc NE 0.
      PERFORM format_rut USING wa_z1mm_dte_encabezado-rutrecep
                        CHANGING lv_rut.

      SELECT SINGLE bukrs INTO wa_cabecera-bukrs
      FROM t001z
      WHERE party = co_taxnr
        AND paval = lv_rut.

    ENDIF.
*---> Valida Sociedad
    PERFORM valida_soc USING wa_z1mm_dte_encabezado-rutrecep wa_cabecera-bukrs  CHANGING dte_relevante c_idoc_status.
    CHECK  dte_relevante NE co_false.

*    SELECT SINGLE lfa1~lifnr " Edwar Soto
*      INTO  wa_cabecera-lifnr " Edwar Soto 10.08.2017
    SELECT SINGLE lfa1~lifnr lfb1~zzaprobador lfb1~zznombana
      INTO (wa_cabecera-lifnr, wa_cabecera-aprobador, wa_cabecera-nombana)
      FROM lfa1 INNER JOIN lfb1 ON lfa1~lifnr = lfb1~lifnr
       WHERE lfb1~bukrs EQ wa_cabecera-bukrs
       AND   lfa1~stcd1 EQ wa_z1mm_dte_encabezado-rutemisor
       AND   lfa1~sperr EQ space
       AND   lfa1~sperq EQ space
       AND   lfb1~sperr EQ space
       AND   lfa1~loevm EQ space
       AND   lfb1~loevm EQ space.
    IF sy-subrc EQ 0.
      wa_cabecera-status = '1'.
      wa_cabecera-docnum = u_docnum.
*Folio sin 000 adelante
      CLEAR lv_folio.
      lv_folio =  wa_z1mm_dte_encabezado-folio.
*      wa_cabecera-xblnr         =  wa_z1mm_dte_encabezado-folio.
      wa_cabecera-xblnr         =  lv_folio.
      CONDENSE  wa_cabecera-xblnr  NO-GAPS.

      CONCATENATE wa_z1mm_dte_encabezado-fchemis(4) wa_z1mm_dte_encabezado-fchemis+5(2) wa_z1mm_dte_encabezado-fchemis+8(2) INTO wa_cabecera-bldat.
      SELECT SINGLE name1 INTO wa_cabecera-name1 FROM lfa1 WHERE lifnr = wa_cabecera-lifnr.


      wa_cabecera-stcd1         = wa_z1mm_dte_encabezado-rutemisor.

      CONCATENATE wa_z1mm_dte_encabezado-fchcancel(4) wa_z1mm_dte_encabezado-fchcancel+5(2) wa_z1mm_dte_encabezado-fchcancel+8(2) INTO wa_cabecera-fchcancel.
      wa_cabecera-mntcancel     = wa_z1mm_dte_encabezado-mntcancel / 100.
      wa_cabecera-termpagocdg   = wa_z1mm_dte_encabezado-termpagocdg.
      wa_cabecera-termpagoglosa = wa_z1mm_dte_encabezado-termpagoglosa.
      wa_cabecera-termpagodias  = wa_z1mm_dte_encabezado-termpagodias.

      CONCATENATE wa_z1mm_dte_encabezado-fchvenc(4) wa_z1mm_dte_encabezado-fchvenc+5(2) wa_z1mm_dte_encabezado-fchvenc+8(2) INTO wa_cabecera-fchvenc.
      wa_cabecera-rutemisor     = wa_z1mm_dte_encabezado-rutemisor.
      wa_cabecera-rznsoc        = wa_z1mm_dte_encabezado-rznsoc.
      wa_cabecera-giroemis      = wa_z1mm_dte_encabezado-giroemis.
      wa_cabecera-correoemisor  = wa_z1mm_dte_encabezado-correoemisor.
      wa_cabecera-dirorigen     = wa_z1mm_dte_encabezado-dirorigen.
      wa_cabecera-cmnaorigen    = wa_z1mm_dte_encabezado-cmnaorigen.
      wa_cabecera-ciudadorigen  = wa_z1mm_dte_encabezado-ciudadorigen.
      wa_cabecera-rutmandante   = wa_z1mm_dte_encabezado-rutmandante.
      wa_cabecera-rutrecep      = wa_z1mm_dte_encabezado-rutrecep.
      wa_cabecera-rznsocrecep   = wa_z1mm_dte_encabezado-rznsocrecep.
      wa_cabecera-totitems      = wa_z1mm_dte_encabezado-totitems.
      wa_cabecera-totbultos     = wa_z1mm_dte_encabezado-totbultos.
      wa_cabecera-mntneto       = wa_z1mm_dte_encabezado-mntneto / 100.
      wa_cabecera-mntexe        = wa_z1mm_dte_encabezado-mntexe / 100.
      wa_cabecera-tasaiva       = wa_z1mm_dte_encabezado-tasaiva.
      wa_cabecera-iva           = wa_z1mm_dte_encabezado-iva / 100.
      wa_cabecera-ivaprop       = wa_z1mm_dte_encabezado-ivaprop / 100.
      wa_cabecera-ivaterc       = wa_z1mm_dte_encabezado-ivaterc / 100.
      wa_cabecera-ivanoret      = wa_z1mm_dte_encabezado-ivanoret / 100.
      wa_cabecera-mnttotal      = wa_z1mm_dte_encabezado-mnttotal / 100.
      wa_cabecera-vlrpagar      = wa_z1mm_dte_encabezado-vlrpagar / 100.
      wa_cabecera-tpocambio     = wa_z1mm_dte_encabezado-tpocambio.
      wa_cabecera-mntnetootrmnda = wa_z1mm_dte_encabezado-mntnetootrmnda / 100.
      wa_cabecera-mntexeotrmnda = wa_z1mm_dte_encabezado-mntexeotrmnda / 100.
      wa_cabecera-ivaotrmnda    = wa_z1mm_dte_encabezado-ivaotrmnda / 100.
      wa_cabecera-ivanoretotrmnda = wa_z1mm_dte_encabezado-ivanoretotrmnda / 100.
      wa_cabecera-mnttototrmnda = wa_z1mm_dte_encabezado-mnttototrmnda / 100.
      wa_cabecera-wrbtr = wa_cabecera-mnttotal.
*  SELECT SINGLE waers INTO wa_cabecera-waers FROM t001 WHERE bukrs = wa_cabecera-bukrs .
      wa_cabecera-waers = co_clp.
      wa_cabecera-cpudt = sy-datum.
      wa_cabecera-cputm = sy-uzeit.
    ELSE.
      c_error = co_true.
      ls_idoc_status-docnum = gv_docnum.
      ls_idoc_status-status = co_51.                        "value 51
      ls_idoc_status-msgid  = co_zfi_0003.
      ls_idoc_status-msgno  = co_121.
      ls_idoc_status-msgv1  = wa_z1mm_dte_encabezado-rutemisor.
      APPEND  ls_idoc_status TO c_idoc_status.
*inicio    "---> Registro para estatus = A.

      CLEAR lv_folio.CLEAR wa_tfi_0074fr.
      lv_folio =  wa_z1mm_dte_encabezado-folio.
      wa_tfi_0074fr-xblnr  =  lv_folio.
      CONDENSE  wa_tfi_0074fr-xblnr   NO-GAPS.

      SELECT * INTO wa_tfi_0074fr FROM ztfi_0074fr UP TO 1 ROWS
      WHERE bukrs EQ wa_cabecera-bukrs
      AND tipodte EQ wa_cabecera-tipodte
      AND xblnr EQ wa_tfi_0074fr-xblnr
      AND stcd1 EQ wa_z1mm_dte_encabezado-rutemisor
      ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF sy-subrc  EQ 0.
        wa_tfi_0074fr-exml    = 'X'.
        wa_tfi_0074fr-docnum  = gv_docnum.
      ELSE.
        wa_tfi_0074fr-bukrs   = wa_cabecera-bukrs.
        wa_tfi_0074fr-tipodte = wa_cabecera-tipodte.
        wa_tfi_0074fr-stcd1   = wa_z1mm_dte_encabezado-rutemisor.
        CONCATENATE wa_z1mm_dte_encabezado-fchemis(4) wa_z1mm_dte_encabezado-fchemis+5(2) wa_z1mm_dte_encabezado-fchemis+8(2) INTO wa_tfi_0074fr-bldat.
        wa_tfi_0074fr-exml    = 'X'.
        wa_tfi_0074fr-docnum  = gv_docnum.
      ENDIF.
      MODIFY ztfi_0074fr FROM wa_tfi_0074fr.
      COMMIT WORK AND WAIT.
*Fin      "---> Registro para estatus = A.

**** Envío de correo por idoc con status 51
      MESSAGE ID co_zfi_0003 TYPE 'E' NUMBER co_121 WITH wa_z1mm_dte_encabezado-rutemisor INTO wa_mensaje.
      PERFORM envia_mail USING wa_cabecera-bukrs u_docnum wa_mensaje.
      CLEAR   ls_idoc_status.
    ENDIF.
  ELSE."Tipo Doc no se debe procesar

*    c_error = co_true.

    dte_relevante = co_false.
    ls_idoc_status-docnum = gv_docnum.
    ls_idoc_status-status = co_53.                          "value 51
    ls_idoc_status-msgid  = co_zfi_0003.
    ls_idoc_status-msgno  = co_122.
    APPEND  ls_idoc_status TO c_idoc_status.
    CLEAR   ls_idoc_status.
  ENDIF.

  SELECT SINGLE  * FROM ztfi_0074
   WHERE bukrs EQ wa_cabecera-bukrs
     AND tipodte EQ wa_cabecera-tipodte
     AND xblnr EQ wa_cabecera-xblnr
*     AND lifnr EQ wa_cabecera-lifnr. "Cambio por Doc. Rechazado en Estatus S, pueden no tener proveedor
     AND stcd1 EQ wa_cabecera-stcd1.  "No deben poder ser ingregados porque ya se rechazo
  IF sy-subrc = 0.
    dte_relevante = co_false.
    ls_idoc_status-docnum = gv_docnum.
    ls_idoc_status-status = co_53.                          "value 51
    ls_idoc_status-msgid  = co_zfi_0003.
    ls_idoc_status-msgno  = co_045.

    APPEND  ls_idoc_status TO c_idoc_status.
    CLEAR   ls_idoc_status.
  ENDIF.
ENDFORM.                    " FILL_ENCABEZADO
*&---------------------------------------------------------------------*
*&      Form  FILL_DETALLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_detalle USING    u_pos    TYPE i
                  CHANGING c_err_pos.
**Tabla de DETALLE  wa_z1mm_dte_detalle
  CLEAR wa_detalle.
  wa_detalle-bukrs   = wa_cabecera-bukrs.
  wa_detalle-xblnr   = wa_cabecera-xblnr.
  wa_detalle-lifnr   = wa_cabecera-lifnr.
  wa_detalle-tipodte = wa_cabecera-tipodte. "OT Se agrega tipodte
  wa_detalle-pos     = u_pos. "wa_z1mm_dte_detalle-nrolindet.
  wa_detalle-texto   = wa_z1mm_dte_detalle-nmbitem.
  wa_detalle-menge   = wa_z1mm_dte_detalle-qtyitem.
  wa_detalle-prcref  = wa_z1mm_dte_detalle-prcref.
  wa_detalle-prcitem = wa_z1mm_dte_detalle-prcitem.
  wa_detalle-mntitem = wa_z1mm_dte_detalle-mntitem.

**  IF NOT ( wa_detalle-prcitem IS NOT INITIAL OR wa_detalle-mntitem IS NOT INITIAL ).
**    c_err_pos = co_true.
**  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
    EXPORTING
      input          = wa_z1mm_dte_detalle-unmdref
*     LANGUAGE       = SY-LANGU
    IMPORTING
      output         = wa_detalle-meins
    EXCEPTIONS
      unit_not_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  PERFORM convert_alpha_input USING    wa_detalle-pos
                              CHANGING wa_detalle-pos.

  wa_detalle-ebeln = wa_cabecera-ebeln.
  APPEND wa_detalle TO it_detalle.

ENDFORM.                    " FILL_DETALLE
*&---------------------------------------------------------------------*
*&      Form  FILL_REFER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_refer  CHANGING c_error c_idoc_status TYPE zbc_t_edids.
**Tabla Refer  wa_z1mm_dte_refer
  DATA: lv_lifnr TYPE lfa1-lifnr.
  DATA: ls_idoc_status TYPE bdidocstat.
  DATA: ls_ztfi_0089 TYPE ztfi_0089,
        lv_ebeln     TYPE ekko-ebeln,
        lv_lblni     TYPE essr-lblni.


  CASE wa_z1mm_dte_refer-tpodocref.
    WHEN co_801.
      CLEAR lv_ebeln.
      PERFORM convert_alpha_input USING  wa_z1mm_dte_refer-folioref(10)
                              CHANGING lv_ebeln.
      SELECT SINGLE ebeln lifnr
      INTO (wa_cabecera-ebeln, lv_lifnr)
      FROM ekko
      WHERE ebeln = lv_ebeln "wa_z1mm_dte_refer-folioref
        AND bstyp = 'F'.
    WHEN co_803. "802
      CLEAR lv_ebeln.
      PERFORM convert_alpha_input USING  wa_z1mm_dte_refer-folioref(10)
                              CHANGING lv_ebeln.
      SELECT SINGLE ebeln lifnr
      INTO (wa_cabecera-ebeln, lv_lifnr)
      FROM ekko
      WHERE ebeln = lv_ebeln  "wa_z1mm_dte_refer-folioref
        AND bstyp = 'F'.
      IF sy-subrc NE 0.
        lv_lblni = wa_z1mm_dte_refer-folioref.
        PERFORM convert_alpha_input  USING    lv_lblni
                                     CHANGING lv_lblni.
        SELECT SINGLE lblni INTO lv_lblni
        FROM essr
        WHERE lblni EQ lv_lblni.
        IF sy-subrc EQ 0.
          CLEAR: wa_refhes.
          MOVE-CORRESPONDING wa_cabecera TO wa_refhes.
          wa_refhes-tiporef  = co_hes.
          wa_refhes-folioref = wa_z1mm_dte_refer-folioref.

          CONCATENATE wa_z1mm_dte_refer-fchref(4) wa_z1mm_dte_refer-fchref+5(2)
                      wa_z1mm_dte_refer-fchref+8(2) INTO wa_refhes-fecharef.
        ENDIF.
      ENDIF.

  ENDCASE.

  MOVE-CORRESPONDING wa_cabecera TO wa_recref.
  wa_recref-tiporef = wa_z1mm_dte_refer-tpodocref.
  WRITE: wa_z1mm_dte_refer-folioref TO  wa_recref-folioref NO-ZERO.
  CONDENSE wa_recref-folioref.
*  wa_recref-folioref =  wa_z1mm_dte_refer-folioref.

  CONCATENATE wa_z1mm_dte_refer-fchref(4) wa_z1mm_dte_refer-fchref+5(2) wa_z1mm_dte_refer-fchref+8(2) INTO wa_recref-fecharef.





ENDFORM.                    " FILL_REFER
*&---------------------------------------------------------------------*
*&      Form  FILL_SUBTOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_subtot .
**Tabla de DETALLE


ENDFORM.                    " FILL_SUBTOT
*&---------------------------------------------------------------------*
*&      Form  FILL_MONTOPAGO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_montopago .
**Tabla de cabecera



  CONCATENATE wa_z1mm_dte_montopago-fchpago(4) wa_z1mm_dte_montopago-fchpago+5(2) wa_z1mm_dte_montopago-fchpago+8(2) INTO wa_cabecera-fchpago.

  wa_cabecera-mntpago       = wa_z1mm_dte_montopago-mntpago.
  wa_cabecera-periododesde  = wa_z1mm_dte_montopago-periododesde.


ENDFORM.                    " FILL_MONTOPAGO
*&---------------------------------------------------------------------*
*&      Form  FILL_IMPRET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_impret .
**Tabla de cabecera
  MOVE-CORRESPONDING wa_cabecera TO wa_otroimpuesto.
  wa_cabecera-tipoimp       = wa_z1mm_dte_impret-tipoimp.
  wa_cabecera-tasaimp       = wa_z1mm_dte_impret-tasaimp.
  wa_cabecera-montoimp      = wa_z1mm_dte_impret-montoimp / 100.

ENDFORM.                    " FILL_IMPRET
*&---------------------------------------------------------------------*
*&      Form  FILL_IMPOTRMON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_impotrmon .
**Tabla de cabecera


  wa_cabecera-tipoimpotrmnda  = wa_z1mm_dte_impotrmon-tipoimpotrmnda.
  wa_cabecera-tasaimpotrmnda  = wa_z1mm_dte_impotrmon-tasaimpotrmnda.
  wa_cabecera-valorimpotrmnda = wa_z1mm_dte_impotrmon-valorimpotrmnda.


ENDFORM.                    " FILL_IMPOTRMON
*&---------------------------------------------------------------------*
*&      Form  GRABAR_TABLAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM grabar_tablas CHANGING  c_idoc_status TYPE zbc_t_edids.

  DATA: ls_idoc_status TYPE bdidocstat.

  MODIFY ztfi_0074 FROM wa_cabecera.
  CLEAR wa_tfi_0074fr.
  MOVE-CORRESPONDING wa_cabecera TO wa_tfi_0074fr.

  wa_tfi_0074fr-cpudt = sy-datum.
  wa_tfi_0074fr-cputm = sy-uzeit.
  wa_tfi_0074fr-exml ='X'.

  IF wa_tfi_0074fr-tipodte ='61' OR "Solo el caso de NC/ND
     wa_tfi_0074fr-tipodte ='56'.

    SELECT fecharsii INTO   wa_tfi_0074fr-fecharsii FROM ztfi_0074fr UP TO 1 ROWS
    WHERE bukrs EQ wa_cabecera-bukrs
    AND tipodte EQ wa_cabecera-tipodte
    AND xblnr   EQ wa_cabecera-xblnr
    AND stcd1   EQ wa_cabecera-rutemisor
    ORDER BY PRIMARY KEY.
    ENDSELECT.

  ENDIF.
  MODIFY ztfi_0074fr FROM wa_tfi_0074fr.

  MODIFY ztfi_0075 FROM TABLE it_detalle.

  MODIFY ztfi_0077 FROM TABLE it_recref.

  MODIFY ztfi_0090 FROM TABLE it_otroimpuesto.

  ls_idoc_status-docnum = gv_docnum.
  ls_idoc_status-status = co_53.
  ls_idoc_status-msgid  = co_zfi_0003.
  ls_idoc_status-msgno  = co_120.
  ls_idoc_status-msgv1  = wa_cabecera-xblnr.

  APPEND  ls_idoc_status TO c_idoc_status.
  CLEAR   ls_idoc_status.

  COMMIT WORK AND WAIT.

ENDFORM.                    " GRABAR_TABLAS
*&---------------------------------------------------------------------*
*&      Form  MAP_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM map_fields USING ls_struc TYPE zst_dte_ack.

  CLEAR wa_z1mm_dte_response.

  wa_z1mm_dte_response-sociedad       = ls_struc-bukrs.
  wa_z1mm_dte_response-rutresponde    = ls_struc-rutemisor.
  wa_z1mm_dte_response-rutrecibe      = ls_struc-rutreceptor.
  wa_z1mm_dte_response-idrespuesta    = ls_struc-idrespuesta.
  wa_z1mm_dte_response-nrodetalles    = ls_struc-nrodetalles.
  wa_z1mm_dte_response-nmbcontacto    = ls_struc-nrodetalles.
  wa_z1mm_dte_response-tmstfirmaresp  = ls_struc-tmstfirmaresp.
  wa_z1mm_dte_response-tipodte        = ls_struc-tipodte.
  wa_z1mm_dte_response-folio          = ls_struc-folio.
  wa_z1mm_dte_response-fchemis        = ls_struc-fchemis.
  wa_z1mm_dte_response-rutemisor      = ls_struc-rutemisor.
  wa_z1mm_dte_response-rutreceptor    = ls_struc-rutreceptor.
  wa_z1mm_dte_response-mnttotal       = ls_struc-mnttotal.
  wa_z1mm_dte_response-codenvio       = ls_struc-codenvio.
  wa_z1mm_dte_response-estadodte      = ls_struc-estadodte.
  wa_z1mm_dte_response-estadodteglosa = ls_struc-estadodteglosa.


ENDFORM.                    " MAP_FIELDS
*&---------------------------------------------------------------------*
*&      Form  ENVIAR_IDOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM enviar_idoc USING u_commit.

  DATA:  ls_control TYPE edidc.
  DATA:  ls_datos TYPE edidd.

* tablas internas locales .
  DATA: it_comm  TYPE TABLE OF  edidc,
        it_datos TYPE TABLE OF  edidd.

* datos de control.

*  ls_control-doctyp   = co_z_dte_felec.
  ls_control-mestyp   = co_z_dte_felec.
  ls_control-idoctp   = co_zid_dte_felec_resp.
  ls_control-direct   = co_uno .
  ls_control-serial   = sy-datum.
  ls_control-serial+8 = sy-uzeit.

  SELECT SINGLE rcvpor INTO ls_control-rcvpor
  FROM edp13
  WHERE rcvprn  EQ co_rcvprn
    AND rcvprt  EQ co_sndprt
    AND mestyp  EQ co_z_dte_felec
    AND idoctyp EQ co_zid_dte_felec_resp.
  IF sy-subrc EQ 0.
  ENDIF.

  ls_control-rcvprt   = co_sndprt.
  ls_control-rcvprn   = co_rcvprn. "iterlocutor
  CONCATENATE sy-sysid 'CLNT' sy-mandt INTO ls_control-sndprn.
  CONCATENATE 'SAP' sy-sysid INTO ls_control-sndpor.
  ls_control-sndprt = co_sndprt.
  ls_control-outmod = '2'.
  ls_control-status = '03'.

  CLEAR ls_datos.
  ls_datos-segnam = co_z1mm_dte_response.
  ls_datos-sdata = wa_z1mm_dte_response.
  APPEND  ls_datos TO it_datos  .


  CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE' IN UPDATE TASK
    EXPORTING
      master_idoc_control            = ls_control
    TABLES
      communication_idoc_control     = it_comm
      master_idoc_data               = it_datos
    EXCEPTIONS
      error_in_idoc_control          = 1
      error_writing_idoc_status      = 2
      error_in_idoc_data             = 3
      sending_logical_system_unknown = 4
      OTHERS                         = 5.



  IF sy-subrc IS INITIAL.

    IF u_commit = 'X'.

      COMMIT WORK.

    ENDIF.

  ENDIF.


  CLEAR it_comm[].
  CLEAR it_datos[].

ENDFORM.                    " ENVIAR_IDOC
*&---------------------------------------------------------------------*
*&      Form  FORMAT_RUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_Z1MM_DTE_ENCABEZADO_RUTRECE  text
*      <--P_LV_RUT  text
*----------------------------------------------------------------------*
FORM format_rut  USING    u_rut_rec
                 CHANGING c_rut.

  DATA: lv_ver      TYPE c,
        lv_rut_full TYPE string,
        lv_rut_pref TYPE c LENGTH 10.

  SPLIT u_rut_rec AT '-' INTO lv_rut_pref lv_ver.

  SHIFT lv_rut_pref LEFT DELETING LEADING  space.
  WRITE lv_rut_pref TO lv_rut_pref USING EDIT MASK '__.___.___'.

  CONCATENATE lv_rut_pref '-' lv_ver INTO lv_rut_full.

  c_rut = lv_rut_full.

ENDFORM.                    " FORMAT_RUT
*&---------------------------------------------------------------------*
*&      Form  FILL_POS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_pos .
*wa_z1mm_dte_det_codl

  IF wa_z1mm_dte_det_codl-tpocodigo = 'QBLI'.
    wa_detalle-ebelp   = wa_z1mm_dte_det_codl-vlrcodigo.
  ENDIF.

  CLEAR: wa_z1mm_dte_det_codl.

ENDFORM.                    " FILL_POS
*&---------------------------------------------------------------------*
*&      Form  FILL_ENCABEZADO2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_encabezado2 .
  CONCATENATE wa_z1mm_dte_encabezado2-fechabase(4)
              wa_z1mm_dte_encabezado2-fechabase+5(2)
              wa_z1mm_dte_encabezado2-fechabase+8(2)
              INTO wa_cabecera-fechabase.

  MOVE: wa_z1mm_dte_encabezado2-fmapago     TO  wa_cabecera-fmapago,
        wa_z1mm_dte_encabezado2-cdgintrecep TO  wa_cabecera-cdgintrecep.

* wa_z1mm_dte_encabezado-DIRORIGEN    TO wa_z1mm_dte_encabezado2-DIRRECEP,
* wa_z1mm_dte_encabezado-GIROEMIS     TO wa_z1mm_dte_encabezado2-GIRORECEP,
* wa_z1mm_dte_encabezado-CMNAORIGEN   TO wa_z1mm_dte_encabezado2-CMNARECEP,
* wa_z1mm_dte_encabezado-CIUDADORIGEN TO wa_z1mm_dte_encabezado2-CIUDADRECEP,

ENDFORM.                    " FILL_ENCABEZADO2
*&---------------------------------------------------------------------*
*&      Form  ENVIA_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_bukrs      text
*      -->P_DOCNUM  text
*      -->P_MENSAJE  text
*----------------------------------------------------------------------*
FORM envia_mail  USING    p_bukrs
                          p_docnum
                          p_mensaje.

  DATA: wa_ztfi_0078 TYPE ztfi_0078.

  DATA: lv_correo TYPE comm_id_long,
        i_text    TYPE STANDARD TABLE OF solisti1,
        r_text    TYPE solisti1,
        wa_mail   TYPE ztfi_0078-correo_texto.

  SELECT SINGLE * FROM ztfi_0078 INTO wa_ztfi_0078
  WHERE bukrs = p_bukrs.

  TRANSLATE wa_ztfi_0078-correo_idoc TO LOWER CASE.
  lv_correo = wa_ztfi_0078-correo_idoc.

  TRANSLATE wa_ztfi_0078-sender TO LOWER CASE.

  r_text = 'Estimados'.
  APPEND r_text TO i_text.
  CLEAR r_text.
  APPEND r_text TO i_text.
  CONCATENATE 'El proceso del IDOC ' p_docnum ' ha finalizado con error.' INTO r_text.
  APPEND r_text TO i_text.

  CLEAR r_text.
  APPEND r_text TO i_text.
  r_text = 'Detalle de errores del Idoc:'.
  APPEND r_text TO i_text.

  CLEAR r_text.
  APPEND r_text TO i_text.
  r_text = p_mensaje.
  APPEND r_text TO i_text.

  CLEAR r_text.
  APPEND r_text TO i_text.
  r_text = 'Este correo ha sido generado automaticamente. Por favor no responder'.
  APPEND r_text TO i_text.

  "-----Envío de e-mail.
  DATA: lt_mailsubject     TYPE sodocchgi1.
  DATA: lt_mailrecipients  TYPE STANDARD TABLE OF somlreci1 WITH HEADER LINE.

* Recipients
  lt_mailrecipients-rec_type  = 'U'.
  lt_mailrecipients-receiver = lv_correo.
  APPEND lt_mailrecipients .
  CLEAR lt_mailrecipients .
* Subject.
  DATA: lv_texto TYPE char200.
  CLEAR lv_texto.
  CONCATENATE 'Error Idoc: ' p_docnum INTO lv_texto.
  lt_mailsubject-obj_name = 'EMAIL'.
  lt_mailsubject-obj_langu = sy-langu.
  lt_mailsubject-obj_descr =  lv_texto.
*Emisor
  DATA: lv_sender TYPE soextreci1-receiver.

  lv_sender = wa_ztfi_0078-sender.
* Texto
  DATA: lv_lineas_txt TYPE i.
  DATA: lt_objpack TYPE STANDARD TABLE OF sopcklsti1,
        wa_objpack LIKE LINE OF lt_objpack.

  DESCRIBE TABLE i_text LINES lv_lineas_txt.
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
      contents_txt               = i_text
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
    COMMIT WORK.
  ENDIF.
ENDFORM.                    " ENVIA_MAIL
*&---------------------------------------------------------------------*
*&      Form  ENVIAR_WS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM enviar_ws .
**  DATA: wa_input  TYPE zwsset_rejection_soap_in.
**  DATA: wa_output TYPE zwsset_rejection_soap_out.
**
**  DATA: proxy_dte TYPE REF TO zwsco_supplier_etdrejection_so.
**
**  DATA lv_largo TYPE i.
**  DATA: oref TYPE REF TO cx_ai_system_fault,
**        iref TYPE REF TO cx_ai_application_fault,
**        text TYPE string.
**
**  "---> Crea Objeto Logs de interfaz.
**  CONCATENATE wa_z1mm_dte_response-sociedad wa_z1mm_dte_response-rutresponde
**               wa_z1mm_dte_response-tipodte '-' wa_z1mm_dte_response-folio
**             INTO gs_bal_log-extnumber.
***   GS_BAL_LOG-EXTNUMBER  = 'BUKRSRUTEMISORRXBLNR'.
**  gs_bal_log-object     = 'ZDTE'.
**  gs_bal_log-subobject  = 'RECEPCION'.
**  gs_bal_log-aldate     = syst-datum.
**  gs_bal_log-altime     = syst-uzeit.
**  gs_bal_log-aluser     = syst-uname.
**  gs_bal_log-alprog     = syst-repid.
**  FREE go_log.
**  CREATE OBJECT go_log
**    EXPORTING
**      i_s_object = gs_bal_log.
**
**  CLEAR wa_output. CLEAR wa_input. CLEAR so_bukrs. FREE so_bukrs.
**  CLEAR wa_0078 .
**  so_bukrs-sign = 'I'.
**  so_bukrs-option = 'EQ'.
**  so_bukrs-low = wa_z1mm_dte_response-sociedad.
**  APPEND so_bukrs.
**
***ReclamoDTE
**  CLEAR lv_rutempresa.
**  lv_rutempresa = wa_z1mm_dte_response-rutrecibe.
**  REPLACE ALL OCCURRENCES OF '.' IN  lv_rutempresa WITH space.
**  CONDENSE lv_rutempresa NO-GAPS.
**
**  wa_input-company = lv_rutempresa."wa_z1mm_dte_response-rutrecibe.
**  wa_input-company_code_sii = wa_z1mm_dte_response-rutresponde.
**  wa_input-document_type = wa_z1mm_dte_response-tipodte.
**  wa_input-document_number = wa_z1mm_dte_response-folio.
**  IF  wa_z1mm_dte_response-estadodte NE '2'.
**    wa_input-status_code = 'ACD'.
**  ELSE.
**    wa_input-status_code = 'RCD'.
**  ENDIF.
**  SELECT SINGLE *  INTO
**      CORRESPONDING FIELDS OF wa_0078 FROM ztfi_0078
**  WHERE bukrs = wa_z1mm_dte_response-sociedad.
**  TRY.
**      CREATE OBJECT proxy_dte
**        EXPORTING
**          logical_port_name = 'ZACEP_RECL'.
**    CATCH cx_ai_system_fault.
**  ENDTRY.
**
****Ini. Autenticación
**  DATA: ws_header TYPE REF TO if_wsprotocol_ws_header.
**
*** get ws_header protocol
**
**  ws_header ?= proxy_dte->get_protocol('IF_WSPROTOCOL_WS_HEADER').
**
*****************************************************************************
*** set somehow header as iXML-DOM tree
**  DATA: ixml TYPE REF TO if_ixml,
**        xml_document TYPE REF TO if_ixml_document,
**        xml_root TYPE REF TO if_ixml_element,
**        xml_element TYPE REF TO if_ixml_element,
**        xml_node TYPE REF TO if_ixml_node,
**        name TYPE string,
**        namespace TYPE string.
**  DATA l_xstring TYPE xstring.
**  DATA l_string TYPE string.
**  DATA fecha_c(24) TYPE c.
**  DATA hora_c(10) TYPE c.
**
**  CLEAR fecha_c. CLEAR hora_c.
**
**  CONCATENATE sy-datum(4) '-' sy-datum+4(2) '-'  sy-datum+6(2) INTO fecha_c.
**  CONDENSE fecha_c NO-GAPS.
**  WRITE: sy-uzeit TO hora_c USING EDIT MASK 'T__:__:__Z'.
**  CONDENSE hora_c NO-GAPS.
**  CONCATENATE fecha_c hora_c INTO fecha_c.
**  CONDENSE fecha_c NO-GAPS.
**
**  CONCATENATE
**'<soapenv:Header>'
**'<wsse:Security soapenv:mustUnderstand="1" xmlns:wsse="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd"'
**'xmlns:wsu="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd"'
**'xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">'
**'<wsse:UsernameToken>'
**'<wsse:Username>' wa_0078-usuario '</wsse:Username>'
**'<wsse:Password Type="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-'
**'1.0#PasswordText">' wa_0078-password '</wsse:Password>'
**'<wsse:Nonce>HuoTyp2613MK1BVOqIhF0g==</wsse:Nonce>'
**'<wsu:Created>' fecha_c '</wsu:Created>'
**'</wsse:UsernameToken>'
**'</wsse:Security>'
**'</soapenv:Header>'
**    INTO l_string.
*** convert to xstring
**  l_xstring = cl_proxy_service=>cstring2xstring( l_string ).
**  IF NOT l_string IS INITIAL.
*** create ixml dom document from xml xstring
**    CALL FUNCTION 'SDIXML_XML_TO_DOM'
**      EXPORTING
**        xml           = l_xstring
**      IMPORTING
**        document      = xml_document
**      EXCEPTIONS
**        invalid_input = 1
**        OTHERS        = 2.
**    IF sy-subrc = 0 AND NOT xml_document IS INITIAL.
**      xml_root = xml_document->get_root_element( ).
**      xml_element ?= xml_root->get_first_child( ).
*** add header element by element to soap header
**      WHILE NOT xml_element IS INITIAL.
**        name = xml_element->get_name( ).
**        namespace = xml_element->get_namespace_uri( ).
**        ws_header->set_request_header( name = name namespace = namespace dom = xml_element ).
**        xml_element ?= xml_element->get_next( ).
**      ENDWHILE.
**    ENDIF.
**  ENDIF.
****FIn. Autenticación
**  TRY.
**      CALL METHOD proxy_dte->set_rejection
**        EXPORTING
**          input  = wa_input
**        IMPORTING
**          output = wa_output.
**    CATCH cx_ai_system_fault INTO oref.
**      text = oref->get_text( ).
**    CATCH cx_ai_application_fault INTO iref.
**      text = iref->get_text( ).
**  ENDTRY.
**  IF wa_output-set_rejection_result-codigo EQ 'DOK'. "proceso OK
****** Confirmo reclamo
**    "---> Logs de interfaz.
**    CLEAR gs_bal_msg.
**    gs_bal_msg-msgid = 'ZDTE_0001'.
**    gs_bal_msg-msgno = '038'.
**    gs_bal_msg-msgty = 'S'.
**    gs_bal_msg-msgv1 = 'Envio de Aceptacion/Reclamo Exitoso'.
**    go_log->add_msg( i_s_msg = gs_bal_msg ).
**    go_log->save( ).
**
**  ELSE.
**    CLEAR gs_bal_msg.
**    gs_bal_msg-msgid = 'ZDTE_0001'.
**    gs_bal_msg-msgno = '039'.
**    gs_bal_msg-msgty = 'E'.
**    go_log->contenate_msg(
**            EXPORTING texto = text
**              descripcion_operacion = wa_output-set_rejection_result-mensaje
**            IMPORTING msgv1 = gs_bal_msg-msgv1
**                      msgv2 = gs_bal_msg-msgv2
**                      msgv3 = gs_bal_msg-msgv3
**                      msgv4 = gs_bal_msg-msgv4 ).
**    go_log->add_msg( i_s_msg = gs_bal_msg ).
**    go_log->save( ).
**
**  ENDIF.

ENDFORM.                    " ENVIAR_WS
*&---------------------------------------------------------------------*
*&      Form  ENVIAR_WSCOMERCIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM enviar_wscomercial .
**  DATA: wa_input  TYPE zwsset_supplier_etdbusiness_s1.
**  DATA: wa_output TYPE zwsset_supplier_etdbusiness_st.
**
**  DATA: proxy_dteco TYPE REF TO zwsco_supplier_etdbusiness_sta.
**
**  DATA lv_largo TYPE i.
**  DATA: oref TYPE REF TO cx_ai_system_fault,
**        iref TYPE REF TO cx_ai_application_fault,
**        text TYPE string.
**  DATA: lv_dv TYPE c.
**
**  CLEAR wa_output. CLEAR wa_input.
**
***ReclamoDTE
**  CLEAR lv_rutempresa. CLEAR lv_dv.
**  lv_rutempresa = wa_z1mm_dte_response-rutrecibe.
**  REPLACE ALL OCCURRENCES OF '.' IN  lv_rutempresa WITH space.
**  CONDENSE lv_rutempresa NO-GAPS.
***  wa_input-company = lv_rutempresa."wa_z1mm_dte_response-rutrecibe.
**  SPLIT lv_rutempresa  AT '-' INTO wa_input-company lv_dv.
**
***  wa_input-company_code_sii = wa_z1mm_dte_response-rutresponde.
**  CLEAR lv_dv.
**  lv_rutempresa =   wa_z1mm_dte_response-rutresponde.
**  SPLIT lv_rutempresa  AT '-' INTO wa_input-company_code_sii lv_dv.
**  wa_input-document_type = wa_z1mm_dte_response-tipodte.
**  wa_input-document_number = wa_z1mm_dte_response-folio.
**  IF  wa_z1mm_dte_response-estadodte NE '2'.
**    wa_input-status_code = 'APR'.
**    wa_input-reason_desc = 'ReclamoAceptacion registrado con Exito'.
**  ELSE.
**    wa_input-status_code = 'REC'.
**    IF wa_z1mm_dte_response-estadodteglosa IS INITIAL.
**      wa_input-reason_desc = 'reclamo por error en contenido Doc.Electronico'.
**    ELSE.
**      wa_input-reason_desc = wa_z1mm_dte_response-estadodteglosa.
**    ENDIF.
**  ENDIF.
**
**  TRY.
**      CREATE OBJECT proxy_dteco
**        EXPORTING
**          logical_port_name = 'ZRESPCOMER'.
**    CATCH cx_ai_system_fault.
**  ENDTRY.
**
****Ini. Autenticación
**  SELECT SINGLE *  INTO
**      CORRESPONDING FIELDS OF wa_0078 FROM ztfi_0078
**  WHERE bukrs = wa_z1mm_dte_response-sociedad.
**  DATA: ws_header TYPE REF TO if_wsprotocol_ws_header.
**
*** get ws_header protocol
**
**  ws_header ?= proxy_dteco->get_protocol('IF_WSPROTOCOL_WS_HEADER').
**
*****************************************************************************
*** set somehow header as iXML-DOM tree
**  DATA: ixml TYPE REF TO if_ixml,
**        xml_document TYPE REF TO if_ixml_document,
**        xml_root TYPE REF TO if_ixml_element,
**        xml_element TYPE REF TO if_ixml_element,
**        xml_node TYPE REF TO if_ixml_node,
**        name TYPE string,
**        namespace TYPE string.
**  DATA l_xstring TYPE xstring.
**  DATA l_string TYPE string.
**  DATA fecha_c(24) TYPE c.
**  DATA hora_c(10) TYPE c.
**
**  CLEAR fecha_c. CLEAR hora_c.
**
**  CONCATENATE sy-datum(4) '-' sy-datum+4(2) '-'  sy-datum+6(2) INTO fecha_c.
**  CONDENSE fecha_c NO-GAPS.
**  WRITE: sy-uzeit TO hora_c USING EDIT MASK 'T__:__:__Z'.
**  CONDENSE hora_c NO-GAPS.
**  CONCATENATE fecha_c hora_c INTO fecha_c.
**  CONDENSE fecha_c NO-GAPS.
**
**  CONCATENATE
**'<soapenv:Header>'
**'<wsse:Security soapenv:mustUnderstand="1" xmlns:wsse="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd"'
**'xmlns:wsu="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd"'
**'xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">'
**'<wsse:UsernameToken>'
**'<wsse:Username>' wa_0078-usuario '</wsse:Username>'
**'<wsse:Password Type="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-'
**'1.0#PasswordText">' wa_0078-password '</wsse:Password>'
**'<wsse:Nonce>HuoTyp2613MK1BVOqIhF0g==</wsse:Nonce>'
**'<wsu:Created>' fecha_c '</wsu:Created>'
**'</wsse:UsernameToken>'
**'</wsse:Security>'
**'</soapenv:Header>'
**    INTO l_string.
*** convert to xstring
**  l_xstring = cl_proxy_service=>cstring2xstring( l_string ).
**  IF NOT l_string IS INITIAL.
*** create ixml dom document from xml xstring
**    CALL FUNCTION 'SDIXML_XML_TO_DOM'
**      EXPORTING
**        xml           = l_xstring
**      IMPORTING
**        document      = xml_document
**      EXCEPTIONS
**        invalid_input = 1
**        OTHERS        = 2.
**    IF sy-subrc = 0 AND NOT xml_document IS INITIAL.
**      xml_root = xml_document->get_root_element( ).
**      xml_element ?= xml_root->get_first_child( ).
*** add header element by element to soap header
**      WHILE NOT xml_element IS INITIAL.
**        name = xml_element->get_name( ).
**        namespace = xml_element->get_namespace_uri( ).
**        ws_header->set_request_header( name = name namespace = namespace dom = xml_element ).
**        xml_element ?= xml_element->get_next( ).
**      ENDWHILE.
**    ENDIF.
**  ENDIF.
****FIn. Autenticación
**  TRY.
**      CALL METHOD proxy_dteco->set_supplier_etdbusiness_state
**        EXPORTING
**          input  = wa_input
**        IMPORTING
**          output = wa_output.
**    CATCH cx_ai_system_fault INTO oref.
**      text = oref->get_text( ).
**    CATCH cx_ai_application_fault INTO iref.
**      text = iref->get_text( ).
**  ENDTRY.
**
**  IF wa_output-set_supplier_etdbusiness_state-codigo EQ 'DOK'. "proceso OK
****** Confirmo reclamo
**    "---> Logs de interfaz.
**    CLEAR gs_bal_msg.
**    gs_bal_msg-msgid = 'ZDTE_0001'.
**    gs_bal_msg-msgno = '038'.
**    gs_bal_msg-msgty = 'S'.
**    gs_bal_msg-msgv1 = 'Envio de Aceptacion/Reclamo Exitoso'.
**    go_log->add_msg( i_s_msg = gs_bal_msg ).
**    go_log->save( ).
**
**  ELSE.
**    CLEAR gs_bal_msg.
**    gs_bal_msg-msgid = 'ZDTE_0001'.
**    gs_bal_msg-msgno = '039'.
**    gs_bal_msg-msgty = 'E'.
**    go_log->contenate_msg(
**            EXPORTING texto = text
**              descripcion_operacion = wa_output-set_supplier_etdbusiness_state-descripcion
**            IMPORTING msgv1 = gs_bal_msg-msgv1
**                      msgv2 = gs_bal_msg-msgv2
**                      msgv3 = gs_bal_msg-msgv3
**                      msgv4 = gs_bal_msg-msgv4 ).
**    go_log->add_msg( i_s_msg = gs_bal_msg ).
**    go_log->save( ).
**
**  ENDIF.

ENDFORM.                    " ENVIAR_WSCOMERCIAL
*&---------------------------------------------------------------------*
*&      Form  AUTDBNET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM autdbnet .



ENDFORM.                    " AUTDBNET
*&---------------------------------------------------------------------*
*&      Form  CONVERT_ALPHA_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CAMPO  text
*      <--P_CAMPO  text
*----------------------------------------------------------------------*
FORM convert_alpha_input  USING    u_campo
                          CHANGING c_campo.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = u_campo
    IMPORTING
      output = c_campo.

ENDFORM.                    " CONVERT_ALPHA_INPUT
*&---------------------------------------------------------------------*
*&      Form  VALIDA_SOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_Z1MM_DTE_ENCABEZADO_RUTRECE  text
*      -->P_WA_CABECERA_BUKRS  text
*      <--P_DTE_RELEVANTE  text
*      <--P_C_IDOC_STATUS  text
*----------------------------------------------------------------------*
FORM valida_soc  USING    p_lv_rut p_wa_cabecerabukrs
                 CHANGING p_dte_relevante c_idoc_status TYPE zbc_t_edids.
  DATA: lv_suscriber TYPE ztfi_0078-suscriber.
  DATA: ls_idoc_status TYPE bdidocstat.

*---> Si No encotro Sociedad por Rut.
  IF p_wa_cabecerabukrs IS INITIAL.
    p_dte_relevante = co_false.
    ls_idoc_status-docnum = gv_docnum.
    ls_idoc_status-status = co_53.
    ls_idoc_status-msgid  = co_zfi_0003.
    ls_idoc_status-msgno  = '125'.
    ls_idoc_status-msgv1  =  p_lv_rut.
    APPEND  ls_idoc_status TO c_idoc_status.
    CLEAR   ls_idoc_status.
  ELSE.
*---> Si Rut exite en Parametros de Rec.Fac.Elec
    SELECT suscriber INTO lv_suscriber FROM ztfi_0078  UP TO 1 ROWS
     WHERE bukrs EQ p_wa_cabecerabukrs
      ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc EQ 0.
    ELSE.
      p_dte_relevante = co_false.
      ls_idoc_status-docnum = gv_docnum.
      ls_idoc_status-status = co_53.
      ls_idoc_status-msgid  = co_zfi_0003.
      ls_idoc_status-msgno  = '202'.
      ls_idoc_status-msgv1  =  p_wa_cabecerabukrs.
      APPEND  ls_idoc_status TO c_idoc_status.
      CLEAR   ls_idoc_status.
    ENDIF.
  ENDIF.

ENDFORM.
