*&---------------------------------------------------------------------*
*&  Include           ZVRFE_101
*&---------------------------------------------------------------------*
*&"----->validación dias factura.
form  VALIDACION_101  TABLES
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

   CHANGING msgid msgty msgno msgv1 msgv2 msgv3 msgv4 ESTATUS.

      IF lt_zdte_dias[] IS NOT INITIAL.
        READ TABLE lt_zdte_dias WITH KEY bukrs = lt_zcb_recfactprov-bukrs BINARY SEARCH.
        IF sy-subrc = 0.
          DATA:lv_dias(3) TYPE n.
          lv_dias = lt_zcb_recfactprov-bldat - sy-datum.
          IF lv_dias > lt_zdte_dias-dias.
            estatus = st_nok."ztfi_0087-st_nok.
            msgid = 'ZDTE_0001'.
            msgty = 'E'.
            msgno = '001'.
            msgv1 = lt_zcb_recfactprov-bukrs.  msgv2 = ''. msgv3 = ''. msgv4 = ''.
          ENDIF.
        ENDIF.
      ELSE."No hay Parametrizacion de dias x sociedad
        estatus = '7'.
        msgid = 'ZDTE_0001'.
        msgty = 'E'.
        msgno = '011'.
        msgv1 = ''. msgv2 = ''. msgv3 = ''. msgv4 = ''.
      ENDIF.

ENDFORM.
