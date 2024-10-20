*&---------------------------------------------------------------------*
*&  Include           ZVRFE_103
*&---------------------------------------------------------------------*
FORM  validacion_103 TABLES
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
  estatus = st_ok. "ztfi_0087-st_ok.
  IF lt_zcb_recfactprov-ebeln IS INITIAL.
    estatus = st_nok. "ztfi_0087-st_nok.
    msgid = 'ZDTE_0001'.
    msgty = 'E'.
    msgno = '002'.
    msgv1 = lt_zcb_recfactprov-bukrs.  msgv2 = ''. msgv3 = ''. msgv4 = ''.
  ELSE."Valido que sea una OC SAP y no sea un Pedido de traslado (ZPTL)
    READ TABLE lt_ekko  WITH KEY ebeln = lt_zcb_recfactprov-ebeln
                                           bstyp = 'F'.
    IF sy-subrc = 0.
      IF lt_ekko-bsart EQ 'ZPTL'.
        estatus = st_nok. "ztfi_0087-st_nok.
        msgid = 'ZDTE_0001'.
        msgty = 'E'.
        msgno = '021'.
        msgv1 = lt_zcb_recfactprov-bukrs.  msgv2 = ''. msgv3 = ''. msgv4 = ''.

      ENDIF.
    ELSE.
      estatus = st_nok. "ztfi_0087-st_nok.
      msgid = 'ZDTE_0001'.
      msgty = 'E'.
      msgno = '021'.
      msgv1 = lt_zcb_recfactprov-bukrs.  msgv2 = ''. msgv3 = ''. msgv4 = ''.
    ENDIF.
  ENDIF.
ENDFORM.                    "VALIDACION_103
