*&---------------------------------------------------------------------*
*&  Include           ZVRFE_109
*&---------------------------------------------------------------------*
FORM  validacion_109 TABLES
       lt_lfb1 STRUCTURE lfb1
       lt_lfb1_oc STRUCTURE lfb1
       lt_lfa1 STRUCTURE lfa1
       lt_ekko STRUCTURE ekko
       lt_ekpo STRUCTURE ekpo
       lt_zdte_cldoc STRUCTURE ztfi_0001b
       lt_zdte_dias STRUCTURE  ztfi_0076
       lt_zdte_posfact STRUCTURE ztfi_0075
       lt_zdt_recref  STRUCTURE ztfi_0077
       lt_ekbe STRUCTURE  ekbe
       lt_ekkn STRUCTURE  ekkn
       ti_essr STRUCTURE zty_ess
       ti_ekko_essr STRUCTURE ekko
       ti_ekpo_essr STRUCTURE ekpo
  USING
       lt_zcb_recfactprov STRUCTURE  ztfi_0074
       ztfi_0087-st_ok
       ztfi_0087-st_nok

   CHANGING msgid msgty msgno msgv1 msgv2 msgv3 msgv4 estatus.
  "VAlidacion Entrada de mercancia
*"Solo OC que No sean servicio(9)
*Por sustitucion Los pedidolim van por tipo proveedor = 3.
*  DATA: lt_ekber TYPE STANDARD TABLE OF zcds_ekbe."nvsacc 031024
*  DATA: ls_ekber TYPE zcds_ekbe."nvsacc 031024
  DATA: lt_ekber TYPE STANDARD TABLE OF zddl_i_ekbe. "nvsacc 031024
  DATA: ls_ekber TYPE zddl_i_ekbe.  "nvsacc 031024
  DATA: totalc TYPE menge_d.
  DATA: totalf TYPE menge_d.
  DATA: correo TYPE char70.
  DATA: nombred  TYPE char70,
        lv_ernam TYPE ernam,
        sw       TYPE i.

  CONSTANTS: co_diasfecha TYPE  char16 VALUE 'Z_DTE_DIASRAUT'.
  DATA: n_dias       TYPE numc4,
        n_diasi      TYPE i,
        lv_dias_calc TYPE i,
        ls_tvarvc    TYPE tvarvc.

  DATA: ls_rc  TYPE sy-subrc,
        ls_doc TYPE zst_dte_ack,
        i_pdf  TYPE xstring,
        ls_msg TYPE  bapi_msg.

  estatus = ztfi_0087-st_ok.

  CHECK lt_zcb_recfactprov-tipodte NE '61' AND
        lt_zcb_recfactprov-tipodte NE '56'.

  READ TABLE lt_ekpo WITH KEY ebeln = lt_zcb_recfactprov-ebeln.
  IF sy-subrc = 0.

    DATA: lv_error TYPE c.
    CLEAR lv_error.CLEAR correo . CLEAR nombred.
    CLEAR sw.

    LOOP AT lt_ekpo WHERE ebeln = lt_zcb_recfactprov-ebeln AND
                          pstyp NE '9'.

      IF lt_ekpo-wepos IS INITIAL. " Si esta en blanco envio solo correo/No valido
        " No pasa nada, no valido
        sw = 1.
      ENDIF.
      CLEAR totalc. CLEAR totalf.
      READ TABLE lt_ekbe WITH KEY ebeln = lt_ekpo-ebeln
                                ebelp = lt_ekpo-ebelp
                                bewtp = 'E'
                                bwart = '101'.
      IF sy-subrc NE 0.
        READ TABLE lt_ekbe WITH KEY ebeln = lt_ekpo-ebeln
                               ebelp = lt_ekpo-ebelp
                               bewtp = 'E'
                               bwart = '105'.
        IF sy-subrc NE 0.
          estatus = ztfi_0087-st_nok.
          IF correo IS INITIAL.

            SELECT SINGLE ernam INTO lv_ernam FROM eban
            WHERE  banfn EQ lt_ekpo-banfn
              AND  bnfpo EQ lt_ekpo-bnfpo .
            IF sy-subrc = 0.
              SELECT SINGLE adr6~smtp_addr INTO correo
              FROM usr21
               INNER JOIN adr6
                ON  usr21~addrnumber = adr6~addrnumber
                AND usr21~persnumber = adr6~persnumber
                    WHERE bname = lv_ernam.

              SELECT SINGLE name_textc INTO nombred  FROM user_addr
             WHERE bname EQ lv_ernam.
            ENDIF.
          ENDIF.
        ELSE.
          "--- Validar que este sin anular y No facturado
          "-- Si hay documentos EM , validar si No estan anulados o facturados
*          SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_ekber FROM  zcds_ekbe"nvsacc
          SELECT * INTO CORRESPONDING FIELDS OF TABLE @lt_ekber FROM  zddl_i_ekbe   "nvsacc 031024
            WHERE ebeln = @lt_ekpo-ebeln
              AND ebelp = @lt_ekpo-ebelp
              AND  bewtp IN ('E','Q').

          LOOP AT lt_ekber INTO ls_ekber.
            IF ls_ekber-bewtp = 'E' AND ls_ekber-shkzg = 'S'.
              totalc = totalc + ls_ekber-totalc.
            ELSEIF ls_ekber-bewtp = 'E' AND ls_ekber-shkzg = 'H'.
              totalc = totalc - ls_ekber-totalc.
            ELSEIF ls_ekber-bewtp = 'Q' AND ls_ekber-shkzg = 'S'.
              totalf = totalf + ls_ekber-totalc.
            ELSEIF ls_ekber-bewtp = 'Q' AND ls_ekber-shkzg = 'H'.
              totalf = totalf - ls_ekber-totalc.
            ENDIF.
          ENDLOOP.

          IF  ( totalc - totalf ) GT 0.
            estatus = ztfi_0087-st_ok.
            EXIT.
          ELSE.
            estatus = ztfi_0087-st_nok.
***//..     Busco correo y Nombre de destinatario
            IF correo IS INITIAL.

              SELECT SINGLE ernam INTO lv_ernam FROM eban
              WHERE  banfn EQ lt_ekpo-banfn
                AND  bnfpo EQ lt_ekpo-bnfpo .
              IF sy-subrc = 0.
                SELECT SINGLE adr6~smtp_addr INTO correo
                FROM usr21
                 INNER JOIN adr6
                  ON  usr21~addrnumber = adr6~addrnumber
                  AND usr21~persnumber = adr6~persnumber
                      WHERE bname = lv_ernam.

                SELECT SINGLE name_textc INTO nombred  FROM user_addr
               WHERE bname EQ lv_ernam.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        "-- Si hay documentos EM , validar si No estan anulados o facturados
*        SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_ekber FROM  zcds_ekbe  "nvsacc 031024
        SELECT * INTO CORRESPONDING FIELDS OF TABLE @lt_ekber FROM  zddl_i_ekbe  "nvsacc 031024
           WHERE ebeln = @lt_ekpo-ebeln
             AND ebelp = @lt_ekpo-ebelp
             AND  bewtp IN ('E','Q').

        LOOP AT lt_ekber INTO ls_ekber.
          IF ls_ekber-bewtp = 'E' AND ls_ekber-shkzg = 'S'.
            totalc = totalc + ls_ekber-totalc.
          ELSEIF ls_ekber-bewtp = 'E' AND ls_ekber-shkzg = 'H'.
            totalc = totalc - ls_ekber-totalc.
          ELSEIF ls_ekber-bewtp = 'Q' AND ls_ekber-shkzg = 'S'.
            totalf = totalf + ls_ekber-totalc.
          ELSEIF ls_ekber-bewtp = 'Q' AND ls_ekber-shkzg = 'H'.
            totalf = totalf - ls_ekber-totalc.
          ENDIF.
        ENDLOOP.

        IF  ( totalc - totalf ) GT 0.
          estatus = ztfi_0087-st_ok.
          EXIT.
        ELSE.
          estatus = ztfi_0087-st_nok.
          IF correo IS INITIAL.

            SELECT SINGLE ernam INTO lv_ernam FROM eban
            WHERE  banfn EQ lt_ekpo-banfn
              AND  bnfpo EQ lt_ekpo-bnfpo .
            IF sy-subrc = 0.
              SELECT SINGLE adr6~smtp_addr INTO correo
              FROM usr21
               INNER JOIN adr6
                ON  usr21~addrnumber = adr6~addrnumber
                AND usr21~persnumber = adr6~persnumber
                    WHERE bname = lv_ernam.

              SELECT SINGLE name_textc INTO nombred  FROM user_addr
             WHERE bname EQ lv_ernam.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
*    "reinicio valor por registro
    CALL METHOD zcl_dte_fi0057v2_cl=>set_fecha_a_em
      EXPORTING
        fecha = '00000000'.

    IF estatus = ztfi_0087-st_nok.
**// " si hay posiones con VefEM en blanco dejo pasar pero envio Correo.
      IF sw = 1.
        estatus = ztfi_0087-st_ok.
      ELSE.
        msgid = 'ZDTE_0001'.
        msgty = 'E'.
        msgno = '018'.
        msgv1 = lt_zcb_recfactprov-bukrs.  msgv2 = ''. msgv3 = ''. msgv4 = ''.
      ENDIF.
***//     Envio correo de alerta al Solicitante
      IF lt_zcb_recfactprov-fchcancel NE sy-datum. "Una solo vez al dia
        CLEAR: ls_rc, ls_msg, ls_doc.
        ls_doc-bukrs = lt_zcb_recfactprov-bukrs.
        ls_doc-tipodte =  lt_zcb_recfactprov-tipodte.
        ls_doc-folio =  lt_zcb_recfactprov-xblnr.
        ls_doc-rutemisor =  lt_zcb_recfactprov-stcd1.
        IF i_pdf IS INITIAL.
          PERFORM busca_pdf USING  lt_zcb_recfactprov CHANGING i_pdf.
        ENDIF.

        CALL FUNCTION 'ZFI_0028DTEV1'
          EXPORTING
            i_codtextoc = 'Z_ENVIO_SOLEM'
            i_correo    = correo
            i_doc       = ls_doc
*           I_XML       =
            I_PDF       = i_pdf
*           I_CSENDER   =
          IMPORTING
            e_rc        = ls_rc
            e_msg       = ls_msg.

        CALL METHOD zcl_dte_fi0057v2_cl=>set_fecha_a_em
          EXPORTING
            fecha = sy-datum.
      ENDIF.
**//    " Se Pasa a Rechazo si el error tiene mas de dias que  parametro
      IF estatus = ztfi_0087-st_nok.

        CLEAR ls_tvarvc.CLEAR lv_dias_calc.
        SELECT * INTO ls_tvarvc FROM tvarvc UP TO 1 ROWS
       WHERE name =  co_diasfecha
         ORDER BY PRIMARY KEY.
        ENDSELECT.
        IF sy-subrc = 0.
          CLEAR n_dias . CLEAR n_diasi.
          n_dias = ls_tvarvc-low.
          n_diasi = n_dias.
        ELSE.
          n_diasi = 0.
        ENDIF.

        IF n_diasi GT 0.
          lv_dias_calc =  sy-datum - lt_zcb_recfactprov-cpudt .
          IF lv_dias_calc GE  n_diasi.
            estatus = '6' . "Estatus a rechazo.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    "validacion_109
*&---------------------------------------------------------------------*
*& Form busca_pdf
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_ZCB_RECFACTPROV
*&      <-- I_PDF
*&---------------------------------------------------------------------*
FORM busca_pdf  USING    lt_zcb_recfactprov TYPE ztfi_0074
                CHANGING i_pdf type xstring.


  DATA: wa_ztfi_0078 TYPE ztfi_0078.
  DATA: co_type TYPE string.
  DATA: co_met TYPE string.


  SELECT SINGLE * FROM ztfi_0078 INTO wa_ztfi_0078
WHERE bukrs = lt_zcb_recfactprov-bukrs.

  CLEAR  co_type.
  SELECT SINGLE nombre_int INTO co_type FROM ztfi_0078b
     WHERE vatint = wa_ztfi_0078-vatint
     AND   codint = '0003'.
  IF co_type IS NOT INITIAL.
    IF wa_ztfi_0078-visupdf = 'A'.
      TRY.
          CALL METHOD (co_type)=>('VER_PDF_ARCH') "zcl_dte_monitor=>ver_pdf_arch
            EXPORTING
              xblnr        = lt_zcb_recfactprov-xblnr
              bukrs        = lt_zcb_recfactprov-bukrs
              tipo_dte     = lt_zcb_recfactprov-tipodte
              rut_emisor   = lt_zcb_recfactprov-stcd1
              rut_receptor = lt_zcb_recfactprov-rutrecep
              fondo        = 'X'
            IMPORTING
              pdf          = i_pdf.
        CATCH cx_static_check .

        CATCH cx_ai_application_fault.
      ENDTRY.
    ELSE."--> Si es Webservices
*Funcion de envio de pdf
      CLEAR co_met.
      SELECT SINGLE nombre_int INTO co_met FROM ztfi_0078b
             WHERE vatint = wa_ztfi_0078-vatint
             AND   codint = '0012'.
      IF sy-subrc = 0.
      ELSE.
        co_met = 'VER_PDF'.
      ENDIF.
      TRY.

          CLEAR i_pdf .
          CALL METHOD (co_type)=>(co_met) "zcl_dte_monitor=>ver_pdf
            EXPORTING
              xblnr   = lt_zcb_recfactprov-xblnr
              bukrs   = lt_zcb_recfactprov-bukrs
              tipodte = lt_zcb_recfactprov-tipodte
              stcd1   = lt_zcb_recfactprov-stcd1
            IMPORTING
              pdf     = i_pdf.
        CATCH cx_static_check .

*      CATCH cx_ai_system_fault.
*      text = oref->get_text( ).
        CATCH cx_ai_application_fault.
*      text = iref->get_text( ).
      ENDTRY.
    ENDIF.
  ENDIF.

ENDFORM.
