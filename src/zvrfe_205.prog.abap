*&---------------------------------------------------------------------*
*&  Include           ZVRFE_205
*&---------------------------------------------------------------------*

FORM VALIDACION_205 TABLES lt_lfb1 STRUCTURE lfb1
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


    DATA: ls_ztfi_0091 TYPE ztfi_0091.
  "----> Validar que tabla este llena.
  SELECT SINGLE * INTO ls_ztfi_0091
  FROM ztfi_0091.
  IF sy-subrc NE 0.
    estatus = st_nok.
    msgid = 'ZFI_0003'.
    msgty = 'E'.
    msgno = '032'.
    msgv1 = ''. msgv2 = ''.
    msgv3 = ''. msgv4 = ''.
  ELSE.
    "---->
    READ TABLE lt_lfb1 WITH KEY lifnr = lt_zcb_recfactprov-lifnr
                                bukrs = lt_zcb_recfactprov-bukrs.
    IF sy-subrc EQ 0.
      IF lt_lfb1-zzdte_tipo EQ '5'.
        SELECT SINGLE * INTO ls_ztfi_0091
        FROM ztfi_0091
        WHERE dtcta EQ lt_lfb1-zzdtcta.
        IF sy-subrc NE 0.
          estatus = st_nok.
          msgid = 'ZDTE_0001'.
          msgty = 'E'.
          msgno = '033'.
          msgv1 = ''. msgv2 = ''.
          msgv3 = ''. msgv4 = ''.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
