*&---------------------------------------------------------------------*
*&  Include           ZVRFE_204
*&---------------------------------------------------------------------*
*
FORM VALIDACION_204 TABLES lt_lfb1 STRUCTURE lfb1
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
  READ TABLE lt_lfb1 WITH KEY lifnr = lt_zcb_recfactprov-lifnr
                              bukrs = lt_zcb_recfactprov-bukrs.
  IF sy-subrc EQ 0.
    IF lt_lfb1-zzdtcta IS INITIAL.
      estatus = st_nok.
      msgid = 'ZDTE_0001'.
      msgty = 'E'.
      msgno = '031'.
      msgv1 = lt_zcb_recfactprov-lifnr.
      msgv2 = ''. msgv3 = ''. msgv4 = ''.
    ENDIF.
  ENDIF.


ENDFORM.
