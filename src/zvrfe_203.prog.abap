*&---------------------------------------------------------------------*
*&      Form  VALIDACION_203
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LT_LFB1             text
*      -->LT_LFB1_OC          text
*      -->LT_LFA1             text
*      -->LT_EKKO             text
*      -->LT_EKPO             text
*      -->LT_ZDTE_CLDOC       text
*      -->LT_ZDTE_DIAS        text
*      -->LT_ZDTE_POSFACT     text
*      -->LT_EKBE             text
*      -->LT_EKKN             text
*      -->TI_ESSR             text
*      -->LT_ESSR_EKKO        text
*      -->LT_ESSR_EKPO        text
*      -->LT_ZCB_RECFACTPROV  text
*      -->ZTFI_0087-ST_OK     text
*      -->ZTFI_0087-ST_NOK    text
*      -->MSGID               text
*      -->MSGTY               text
*      -->MSGNO               text
*      -->MSGV1               text
*      -->MSGV2               text
*      -->MSGV3               text
*      -->MSGV4               text
*      -->ESTATUS             text
*----------------------------------------------------------------------*
FORM validacion_203 TABLES lt_lfb1 STRUCTURE lfb1
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
                     USING lt_zcb_recfactprov STRUCTURE  ztfi_0074
                           ztfi_0087-st_ok
                           ztfi_0087-st_nok
                  CHANGING msgid
                           msgty
                           msgno
                           msgv1
                           msgv2
                           msgv3
                           msgv4
                           estatus.
* Envio de Error de proveedor No clasificado
  estatus = ztfi_0087-st_nok.
  msgid = 'ZDTE_0001'.
  msgty = 'E'.
  msgno = '012'.
  msgv1 = 'Proveedor No clasificado en TipoDTE'.  msgv2 = ''. msgv3 = ''. msgv4 = ''.
  estatus = ztfi_0087-st_nok.

ENDFORM.                    "VALIDACION_203
