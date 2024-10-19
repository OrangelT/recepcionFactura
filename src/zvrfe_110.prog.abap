*&---------------------------------------------------------------------*
*&  Include           ZVRFE_110
*&---------------------------------------------------------------------*
 FORM  validacion_110 TABLES
       lt_lfb1 STRUCTURE lfb1
       lt_lfb1_oc STRUCTURE lfb1
       lt_lfa1 STRUCTURE lfa1
       lt_ekko STRUCTURE ekko
       lt_ekpo STRUCTURE ekpo
       lt_zdte_cldoc STRUCTURE ztfi_0001B
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
  READ TABLE lt_ekpo WITH KEY ebeln = lt_zcb_recfactprov-ebeln "OC de servicio
                                                pstyp = '9'.
  IF sy-subrc = 0.
    READ TABLE lt_zdt_recref WITH KEY  bukrs = lt_zcb_recfactprov-bukrs
                                      xblnr = lt_zcb_recfactprov-xblnr
                                      lifnr = lt_zcb_recfactprov-lifnr
                                      tiporef = 'HES'.
    IF sy-subrc = 0.
      "---->igual a 9
      DATA: lv_error TYPE c.
      CLEAR lv_error.
      LOOP AT lt_ekpo WHERE ebeln = lt_zcb_recfactprov-ebeln AND pstyp = '9'.
        READ TABLE lt_ekbe WITH KEY ebeln = lt_ekpo-ebeln
                                    ebelp = lt_ekpo-ebelp
                                    bewtp = 'D'.
        IF sy-subrc = 0.
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
              estatus = st_nok. "ztfi_0087-st_nok.
              msgid = 'ZDTE_0001'.
              msgty = 'E'.
              msgno = '018'.
              msgv1 = lt_zcb_recfactprov-bukrs.  msgv2 = ''. msgv3 = ''. msgv4 = ''.
            ENDIF.
          ENDIF.
        ELSE.
          estatus = st_nok. "ztfi_0087-st_nok.
          msgid = 'ZDTE_0001'.
          msgty = 'E'.
          msgno = '005'.
          msgv1 = lt_zcb_recfactprov-bukrs.  msgv2 = ''. msgv3 = ''. msgv4 = ''.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.
