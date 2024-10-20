*&---------------------------------------------------------------------*
*&  Include           ZVRFE_108
*&---------------------------------------------------------------------*
FORM  validacion_108 TABLES
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
       st_ok
       st_nok

   CHANGING msgid msgty msgno msgv1 msgv2 msgv3 msgv4 estatus.
  "----- validación de HES Aprobada solo para OC de servicio
  estatus = st_ok. "ztfi_0087-st_ok.



  DATA: cont     TYPE i,
        gv_frgkl LIKE essr-frgkl.
  DATA: lv_error TYPE c,
        lv_exit  TYPE c.
  FIELD-SYMBOLS: <f1> TYPE ekbe.

  READ TABLE lt_ekpo WITH KEY ebeln = lt_zcb_recfactprov-ebeln "OC de servicio
                                                 pstyp = '9'.


*// Valido que si envian HES; el Nro. enviado este Aprobado
*// de no enviar HES, Se buscan en todas las posiciones si al menos una  HES aprobada.
  IF sy-subrc = 0.
*    READ TABLE  lt_zdt_recref WITH KEY  bukrs = lt_zcb_recfactprov-bukrs
*                                       xblnr = lt_zcb_recfactprov-xblnr
*                                       lifnr = lt_zcb_recfactprov-lifnr
*                                       tiporef = 'HES'.
*
*
*    IF sy-subrc = 0.
*
*      CLEAR lv_error.
*      LOOP AT lt_ekpo WHERE ebeln = lt_zcb_recfactprov-ebeln AND pstyp = '9'.
*        READ TABLE lt_ekbe WITH KEY ebeln = lt_ekpo-ebeln
*                                    ebelp = lt_ekpo-ebelp
*                                    lfbnr = lt_zdt_recref-folioref
*                                    bewtp = 'D'.
*        cont = 0.
*        IF sy-subrc = 0.
*          READ TABLE lt_ekbe WITH KEY ebeln = lt_ekpo-ebeln "si hay HES facturas sin anular
*                                      ebelp = lt_ekpo-ebelp
*                                      bewtp = 'Q'
*                                      lfbnr =  lt_zdt_recref-folioref
*                                      shkzg = 'S'.
*          IF sy-subrc = 0.
*            cont = cont + 1.
*          ENDIF.
*          READ TABLE lt_ekbe WITH KEY ebeln = lt_ekpo-ebeln "si hay HES facturas anuladas
*                                  ebelp = lt_ekpo-ebelp
*                                  bewtp = 'Q'
*                                  lfbnr =  <f1>-belnr
**                                    lfgja =  <f1>-gjahr
*                                  shkzg = 'H'.
*          IF sy-subrc = 0.
*            cont = cont - 1.
*          ENDIF.
*          IF cont = 0. " Hay una HES sin factura.
*            SELECT SINGLE frgkl INTO gv_frgkl  FROM essr "Validar si la HES no esta aprobada
*            WHERE lblni EQ <f1>-belnr
*            AND KZABN eq 'X'.
*            IF sy-subrc = 0.
*              cont = 0.
*            ELSE.
*              cont = 2.
*            ENDIF.
*            IF cont = 0.
*              EXIT.
*            ENDIF.
*          ENDIF.
*
*        ELSE.
*          estatus = st_nok. "ztfi_0087-st_nok.
*          msgid = 'ZDTE_0001'.
*          msgty = 'E'.
*          msgno = '020'.
*          msgv1 = lt_zcb_recfactprov-bukrs.  msgv2 = ''. msgv3 = ''. msgv4 = ''.
*        ENDIF.
*      ENDLOOP.
*
*    ELSE.
    "---->Para todas las posiciones Sin HES en especifico.

    CLEAR lv_error. CLEAR lv_exit.
    LOOP AT lt_ekpo WHERE ebeln = lt_zcb_recfactprov-ebeln AND pstyp = '9'.
      CHECK lv_exit IS INITIAL.
      READ TABLE lt_ekbe WITH KEY ebeln = lt_ekpo-ebeln
*                                    ebelp = lt_ekpo-ebelp "Se valida si hay alguna HES por OC
*                                                          "Luego la posiciones se validan en sig.Loop
                                  bewtp = 'D'.
      IF sy-subrc = 0.
        cont = 0.
        LOOP AT lt_ekbe ASSIGNING <f1> WHERE ebeln = lt_ekpo-ebeln
                                                     AND  ebelp = lt_ekpo-ebelp
                                                     AND  bewtp = 'D'.
          cont = 0.

          READ TABLE lt_ekbe WITH KEY ebeln = lt_ekpo-ebeln "si hay sin anular
                                  ebelp = lt_ekpo-ebelp
                                  bewtp = 'Q'
                                  lfbnr =  <f1>-belnr
*                                    lfgja =  <f1>-gjahr
                                  shkzg = 'S'.
          IF sy-subrc = 0.
            cont = cont + 1.
          ENDIF.
          READ TABLE lt_ekbe WITH KEY ebeln = lt_ekpo-ebeln "si hay facturas anuladas
                                  ebelp = lt_ekpo-ebelp
                                  bewtp = 'Q'
                                  lfbnr =  <f1>-belnr
*                                    lfgja =  <f1>-gjahr
                                  shkzg = 'H'.
          IF sy-subrc = 0.
            cont = cont - 1.
          ENDIF.
          IF cont = 0. " Hay una HES sin factura.
            SELECT SINGLE frgkl INTO gv_frgkl  FROM essr "Validar si la HES no este aceptada
            WHERE lblni EQ <f1>-belnr
            AND kzabn EQ 'X'.
            IF sy-subrc = 0.
              cont = 0.
            ELSE.
              cont = 2.
            ENDIF.
            IF cont = 0.
              lv_exit = 'X'.
              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.

      ELSE.
        estatus = st_nok. "ztfi_0087-st_nok.
        msgid = 'ZDTE_0001'.
        msgty = 'E'.
        msgno = '005'.
        msgv1 = lt_zcb_recfactprov-bukrs.  msgv2 = ''. msgv3 = ''. msgv4 = ''.
      ENDIF.
    ENDLOOP.
*   ENDIF.
    CHECK lt_zcb_recfactprov-tipodte NE '61' AND
          lt_zcb_recfactprov-tipodte NE '56'.
    IF cont = 1. "Todas las HES facturadas para una posicion.
      estatus = st_nok. "ztfi_0087-st_nok.
      msgid = 'ZDTE_0001'.
      msgty = 'E'.
      msgno = '024'.
      msgv1 = lt_zcb_recfactprov-bukrs.  msgv2 = ''. msgv3 = ''. msgv4 = ''.
    ENDIF.
    IF cont = 2. "
      estatus = st_nok. "ztfi_0087-st_nok.
      msgid = 'ZDTE_0001'.
      msgty = 'E'.
      msgno = '018'.
      msgv1 = lt_zcb_recfactprov-bukrs.  msgv2 = ''. msgv3 = ''. msgv4 = ''.
    ENDIF.
  ENDIF.

ENDFORM.                    "validacion_108
