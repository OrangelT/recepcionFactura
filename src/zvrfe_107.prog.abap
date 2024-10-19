*&---------------------------------------------------------------------*
*&  Include           ZVRFE_107
*&---------------------------------------------------------------------*
FORM  validacion_107 TABLES
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
  "----- validación de entrega de HES solo para OC servicios
  estatus = st_ok. "ztfi_0087-st_ok.
   "VAlidacion Posicion de OC

  DATA: lv_registro_pos LIKE sy-tabix.
  READ TABLE lt_ekpo WITH KEY ebeln = lt_zcb_recfactprov-ebeln.
  IF sy-subrc = 0.
    READ TABLE lt_zdte_posfact WITH KEY ebeln = lt_zcb_recfactprov-ebeln.
    IF sy-subrc NE 0.
      estatus = st_nok.
      msgid = 'ZDTE_0001'.
      msgty = 'E'.
      msgno = '010'.
      msgv1 = lt_zcb_recfactprov-bukrs.  msgv2 = ''. msgv3 = ''. msgv4 = ''.
    ELSE.
      LOOP AT lt_zdte_posfact WHERE ebeln = lt_zcb_recfactprov-ebeln.
        lv_registro_pos = sy-tabix.
        IF lt_zdte_posfact-ebelp IS INITIAL.
          estatus = st_nok.
          msgid = 'ZDTE_0001'.
          msgty = 'E'.
          msgno = '010'.
          msgv1 = lt_zcb_recfactprov-bukrs.  msgv2 = ''. msgv3 = ''. msgv4 = ''.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ELSE.
    estatus = st_nok.
    msgid = 'ZDTE_0001'.
    msgty = 'E'.
    msgno = '010'.
    msgv1 = lt_zcb_recfactprov-bukrs.  msgv2 = ''. msgv3 = ''. msgv4 = ''.
  ENDIF.


ENDFORM.                    "VALIDACION_107
