*&---------------------------------------------------------------------*
*&  Include           ZVRFE_102
*&---------------------------------------------------------------------*
FORM  validacion_102 TABLES
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
       TI_ESSR structure zty_ess
       ti_ekko_essr structure ekko
       ti_ekpo_essr structure ekpo
  USING
       lt_zcb_recfactprov STRUCTURE  ztfi_0074
       st_ok
       st_nok

   CHANGING msgid msgty msgno msgv1 msgv2 msgv3 msgv4 estatus.

*Validar que Proveedor de Factura, que Existe en Sociedad FI .
  estatus = st_ok."ztfi_0087-st_ok.
  READ TABLE lt_lfa1 WITH KEY lifnr = lt_zcb_recfactprov-lifnr.
  IF sy-subrc NE 0.

    estatus = st_nok."ztfi_0087-st_nok.
    msgid = 'ZDTE_0001'.
    msgty = 'E'.
    msgno = '000'.
    msgv1 = lt_zcb_recfactprov-bukrs.  msgv2 = ''. msgv3 = ''. msgv4 = ''.
  ELSE.
    READ TABLE lt_lfb1 WITH KEY lifnr = lt_lfa1-lifnr
                                bukrs = lt_zcb_recfactprov-bukrs.
    IF sy-subrc NE 0.

      estatus = st_nok."ztfi_0087-st_nok.
      msgid = 'ZDTE_0001'.
      msgty = 'E'.
      msgno = '000'.
      msgv1 = lt_zcb_recfactprov-bukrs.  msgv2 = ''. msgv3 = ''. msgv4 = ''.
    ENDIF.
  ENDIF.


ENDFORM.                    "VALIDACION_102
