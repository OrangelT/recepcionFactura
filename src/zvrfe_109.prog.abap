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
  DATA: lt_ekber TYPE STANDARD TABLE OF zcds_ekbe.
  DATA: ls_ekber TYPE zcds_ekbe.
  DATA: totalc TYPE menge_d.
  DATA: totalf TYPE menge_d.
  DATA: correo TYPE char70.
  DATA: nombred TYPE char70,
        sw      TYPE i.
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
***//..     Busco correo y Nombre de destinatario
*            IF correo IS INITIAL.
*              SELECT SINGLE name_textc INTO nombred  FROM user_addr
*             WHERE bname EQ lt_ekpo-afnam.
*
*              SELECT SINGLE correo INTO correo FROM ztfi_0095
*                WHERE afnam EQ lt_ekpo-afnam.
*
*            ENDIF.

        ELSE.
          "--- Validar que este sin anular y No facturado
          "-- Si hay documentos EM , validar si No estan anulados o facturados
          SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_ekber FROM  zcds_ekbe
            WHERE ebeln = lt_ekpo-ebeln
              AND ebelp = lt_ekpo-ebelp
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
*            IF correo IS INITIAL.
*              SELECT SINGLE name_textc INTO nombred  FROM user_addr
*             WHERE bname EQ lt_ekpo-afnam.
*
*              SELECT SINGLE correo INTO correo FROM ztfi_0095
*                WHERE afnam EQ lt_ekpo-afnam.
*
*            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        "-- Si hay documentos EM , validar si No estan anulados o facturados
        SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_ekber FROM  zcds_ekbe
          WHERE ebeln = lt_ekpo-ebeln
            AND ebelp = lt_ekpo-ebelp
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
*          IF correo IS INITIAL.
*            SELECT SINGLE bname INTO nombred  FROM user_addr
*           WHERE bname EQ lt_ekpo-afnam.
*
*            SELECT SINGLE correo INTO correo FROM ztfi_0095
*              WHERE afnam EQ lt_ekpo-afnam.
*
*          ENDIF.
        ENDIF.
      ENDIF.
*      ENDIF.
    ENDLOOP.
*    "reinicio valor por registro
*    CALL METHOD zcl_dte_fi0057v2_cl=>set_fecha_a_em
*      EXPORTING
*        fecha = '00000000'.

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
*      IF lt_zcb_recfactprov-fchcancel NE sy-datum. "Una solo vez al dia
*        CALL FUNCTION 'ZFI_0031DTE'
*          EXPORTING
*            i_ztfi_0074  = lt_zcb_recfactprov
*            i_codtextoc  = 'ZDTE_ALE_EM_CU'
*            i_codtextoas = 'ZDTE_ALE_EM_AS'
*            i_correo     = correo
*            i_nomb       = nombred.
*        CALL METHOD zcl_dte_fi0057v2_cl=>set_fecha_a_em
*          EXPORTING
*            fecha = sy-datum.
*      ENDIF.
    ENDIF.
  ENDIF.
* version anterior
*  estatus = ztfi_0087-st_ok.
*  READ TABLE lt_ekpo WITH KEY ebeln = lt_zcb_recfactprov-ebeln.
*  IF sy-subrc = 0.
*
*    DATA: lv_error TYPE c.
*    CLEAR lv_error.
*
*
*    LOOP AT lt_ekpo WHERE ebeln = lt_zcb_recfactprov-ebeln AND
*                          pstyp NE '9'.
*      IF lt_ekpo-wepos IS INITIAL.
*        " No pasa nada, no valido
*      ELSE.
*        READ TABLE lt_ekbe WITH KEY ebeln = lt_ekpo-ebeln
*                                  ebelp = lt_ekpo-ebelp
*                                  bewtp = 'E'
*                                  bwart = '101'.
*        IF sy-subrc NE 0.
*          READ TABLE lt_ekbe WITH KEY ebeln = lt_ekpo-ebeln
*                                 ebelp = lt_ekpo-ebelp
*                                 bewtp = 'E'
*                                 bwart = '105'.
*          IF sy-subrc NE 0.
*            estatus = ztfi_0087-st_nok.
**            msgid = 'ZDTE_0001'.
**            msgty = 'E'.
**            msgno = '018'.
**            msgv1 = lt_zcb_recfactprov-bukrs.  msgv2 = ''. msgv3 = ''. msgv4 = ''.
*          ELSE.
*            estatus = ztfi_0087-st_ok.
*            EXIT.
*          ENDIF.
*        ELSE.
*          estatus = ztfi_0087-st_ok.
*          EXIT.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*    CHECK lt_zcb_recfactprov-tipodte NE '61' AND
*          lt_zcb_recfactprov-tipodte NE '56'.
*    IF estatus = ztfi_0087-st_nok.
*      msgid = 'ZDTE_0001'.
*      msgty = 'E'.
*      msgno = '018'.
*      msgv1 = lt_zcb_recfactprov-bukrs.  msgv2 = ''. msgv3 = ''. msgv4 = ''.
*    ENDIF.
*  ENDIF.
ENDFORM.                    "validacion_109
