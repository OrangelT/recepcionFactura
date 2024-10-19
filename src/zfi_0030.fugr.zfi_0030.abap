FUNCTION zfi_0030.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(LT_ZCB_RECFACTPROV) LIKE  ZTFI_0074 STRUCTURE
*"        ZTFI_0074
*"     REFERENCE(ZDTE_TIPO) TYPE  ZFI_TIPO
*"     REFERENCE(REPID) TYPE  REPID
*"  TABLES
*"      LT_LFB1 STRUCTURE  LFB1
*"      LT_LFB1_OC STRUCTURE  LFB1
*"      LT_LFA1 STRUCTURE  LFA1
*"      LT_EKKO STRUCTURE  EKKO
*"      LT_EKPO STRUCTURE  EKPO
*"      LT_ZDTE_CLDOC STRUCTURE  ZTFI_0001B
*"      LT_ZDTE_DIAS STRUCTURE  ZTFI_0076
*"      LT_ZDTE_POSFACT STRUCTURE  ZTFI_0075
*"      LT_ZDT_RECREF STRUCTURE  ZTFI_0077
*"      LT_EKBE STRUCTURE  EKBE
*"      LT_EKKN STRUCTURE  EKKN
*"      TI_ESSR STRUCTURE  ZTY_ESS
*"      TI_EKKO_ESSR STRUCTURE  EKKO
*"      TI_EKPO_ESSR STRUCTURE  EKPO
*"  CHANGING
*"     REFERENCE(MSGID) TYPE  MSGID
*"     REFERENCE(MSGTY) TYPE  MSGTY
*"     REFERENCE(MSGNO) TYPE  MSGNO
*"     REFERENCE(MSGV1) TYPE  MSGV1
*"     REFERENCE(MSGV2) TYPE  MSGV2
*"     REFERENCE(MSGV3) TYPE  MSGV3
*"     REFERENCE(MSGV4) TYPE  MSGV4
*"     REFERENCE(ESTATUS) TYPE  CHAR01
*"  EXCEPTIONS
*"      ENVIA_ERROR
*"----------------------------------------------------------------------
  DATA aux_msgtyp TYPE symsgty.
  DATA: lt_0087 TYPE STANDARD TABLE OF ztfi_0087.
  DATA: wa_0087 TYPE ztfi_0087.

  formid = 'VALIDACION_?2'.
*  CLEAR: SY-MSGID, SY-MSGTY, SY-MSGNO, SY-MSGV1,
*         SY-MSGV2, SY-MSGV3, SY-MSGV4.
  CLEAR: msgid, msgty, msgno, msgv1, msgv2, msgv3, msgv4,  wa_0087.
  REFRESH lt_0087.

  SELECT * FROM ztfi_0087 INTO TABLE lt_0087
    WHERE zdte_tipo EQ  zdte_tipo
                              AND repid EQ repid
                              AND activo EQ 'X'.
  SORT lt_0087 BY zdte_tipo repid sec.
  LOOP AT lt_0087 INTO wa_0087.
* Por defecto todo OK.
    estatus = ztfi_0087-st_ok.
    REPLACE '?2' WITH  wa_0087-grpno INTO formid.
    PERFORM (formid) IN PROGRAM zfir_0062
     TABLES
       lt_lfb1
       lt_lfb1_oc
       lt_lfa1
       lt_ekko
       lt_ekpo
       lt_zdte_cldoc
       lt_zdte_dias
       lt_zdte_posfact
       lt_zdt_recref
       lt_ekbe
       lt_ekkn
       ti_essr
       ti_ekko_essr
       ti_ekpo_essr
     USING
       lt_zcb_recfactprov
       wa_0087-st_ok
       wa_0087-st_nok

   CHANGING msgid msgty msgno msgv1 msgv2 msgv3 msgv4 estatus.
    CLEAR formid.
    formid = 'VALIDACION_?2'.
    IF msgid NE space. "SI hay error salgo

      EXIT.
    ENDIF.
  ENDLOOP.


ENDFUNCTION.
