*&---------------------------------------------------------------------*
*&  Include           ZVRFE_207
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           ZVRFE_207
*&---------------------------------------------------------------------*
FORM  validacion_207  TABLES
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

  DATA: ls_belnr TYPE belnr_d,
        ls_gjahr TYPE gjahr.
  estatus = st_ok.
  CLEAR ls_belnr. CLEAR ls_gjahr.

*&"----->validación de factura doble FI
  SELECT  a~belnr a~gjahr INTO (ls_belnr, ls_gjahr)  FROM bsip AS a
   INNER JOIN bkpf AS b  ON a~bukrs = b~bukrs AND
                            a~belnr = b~belnr AND
                            a~gjahr = b~gjahr  UP TO 1 ROWS
   WHERE a~bukrs EQ lt_zcb_recfactprov-bukrs
     AND a~lifnr EQ lt_zcb_recfactprov-lifnr
     AND a~waers EQ lt_zcb_recfactprov-waers
     AND a~bldat EQ lt_zcb_recfactprov-bldat
     AND a~xblnr EQ lt_zcb_recfactprov-xblnr
     AND b~stblg EQ space
      ORDER BY a~bukrs a~gjahr a~belnr.
  ENDSELECT.

  IF sy-subrc EQ 0.

    estatus = st_nok.
    msgid = 'ZDTE_0001'.
    msgty = 'E'.
    msgno = '083'.
    msgv1 =  lt_zcb_recfactprov-bukrs.
    msgv2 = ls_belnr.
    msgv3 = ls_gjahr.
    msgv4 = ''.

  ENDIF.

ENDFORM.
