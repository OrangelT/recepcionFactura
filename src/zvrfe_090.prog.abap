*&---------------------------------------------------------------------*
*&  Include           ZVRFE_090
*&---------------------------------------------------------------------*
 FORM  validacion_090 TABLES
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
   "----- Validacion de NC y ND , donde sus facturas refeencias no pueden estar rechazadas.
   DATA:
*             gr_sii      TYPE REF TO zcl_sii,
     itab        TYPE zdte_fechasii,
     lt_eve      TYPE zdte_liseve_t,
     zdte_respar TYPE zdte_respar,
     ls_destsid  TYPE string,
     i_bal_s_msg TYPE STANDARD TABLE OF symsg,
     sw          TYPE i.

   estatus = st_ok. "ztfi_0087-st_ok.
   "----> Solo facturas
   CHECK lt_zcb_recfactprov-tipodte = '33' OR
         lt_zcb_recfactprov-tipodte = '34'.

   itab-tipodoc = lt_zcb_recfactprov-tipodte(3).
   itab-folio   = lt_zcb_recfactprov-xblnr.
   SPLIT lt_zcb_recfactprov-stcd1 AT  '-' INTO itab-rutemisor itab-dvemisor.

*   lv_sociedad = lt_zcb_recfactprov-bukrs.
*   PERFORM autenticacion_sii.

*   CREATE OBJECT gr_sii.
*   IF gr_sii IS BOUND.
***//.. Llamar al SII
*     CALL METHOD gr_sii->display_eventos
*       EXPORTING
*         itab    = itab
*         i_bukrs = lt_zcb_recfactprov-bukrs
*       IMPORTING
*         lt_eve  = lt_eve.
*   ENDIF.
***//.. Llamar al SII
   "Autenticar
   CALL FUNCTION 'ZFI_0008ADTE'
     EXPORTING
       i_bukrs = lt_zcb_recfactprov-bukrs.
   "llamar a eventos
   CALL FUNCTION 'ZFI_0008LDTE'
     EXPORTING
       itab    = itab
       i_bukrs = lt_zcb_recfactprov-bukrs
     IMPORTING
       lt_eve  = lt_eve.

   READ TABLE lt_eve WITH KEY codevento = 'RCD' TRANSPORTING NO FIELDS. " leo si tiene rechazo
   IF sy-subrc = 0.
     estatus = st_nok. "ztfi_0087-st_nok.
     msgid = 'ZFI_0003'.
     msgty = 'E'.
     msgno = '085'.
     msgv1 = lt_zcb_recfactprov-bukrs.  msgv2 = ''. msgv3 = ''. msgv4 = ''.
   ENDIF.

 ENDFORM.
