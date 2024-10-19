*&---------------------------------------------------------------------*
*&  Include           ZVRFE_201
*&---------------------------------------------------------------------*
FORM validacion_201 TABLES
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
  "VAlidacion de clase de doc.FI homologacion a doc.Tipo DTE
  DATA: e_knttp LIKE ekpo-knttp.
  DATA: e_scope LIKE proj-scope.
  DATA: e_pronr LIKE afko-pronr.
  DATA: e_bsart LIKE ekko-bsart.



  estatus = st_ok. "ztfi_0087-st_ok.
  CLEAR  e_knttp. CLEAR e_scope.
  IF lt_zdte_cldoc[] IS INITIAL."--->si la tabla de homologación esta vacía.
    estatus = st_nok. "ztfi_0087-st_nok.
    msgid = 'ZDTE_0001'.
    msgty = 'E'.
    msgno = '023'.
    msgv1 = ''.  msgv2 = ''. msgv3 = ''. msgv4 = ''.
  ENDIF.

  "---->Buscar BSART
  READ TABLE lt_ekko WITH KEY ebeln = lt_zcb_recfactprov-ebeln.
  IF sy-subrc EQ 0.
    e_bsart = lt_ekko-bsart.
  ENDIF.

  "---->valor scope.
  READ TABLE lt_ekpo WITH KEY ebeln = lt_zcb_recfactprov-ebeln
                              knttp = 'N'.
  IF sy-subrc = 0.
    e_knttp = lt_ekpo-knttp.
**    READ TABLE lt_ekkn WITH KEY ebeln = lt_ekpo-ebeln
**                                ebelp = lt_ekpo-ebelp BINARY SEARCH.
**    IF sy-subrc = 0.
**      SELECT SINGLE pronr FROM afko INTO e_pronr  WHERE aufnr = lt_ekkn-nplnr.
**      IF sy-subrc = 0.
**        SELECT SINGLE scope INTO e_scope FROM proj WHERE pspnr = e_pronr.
**        IF sy-subrc NE 0.
**          estatus = st_nok. "ztfi_0087-st_nok.
**          msgid = 'ZDTE_0001'.
**          msgty = 'E'.
**          msgno = '012'.
**          msgv1 = 'No se encontro Tipo de Proyecto Para selecciona Cl.Doc'.  msgv2 = ''. msgv3 = ''. msgv4 = ''.
**
**        ENDIF.
**      ELSE.
**        estatus = st_nok. "ztfi_0087-st_nok.
**        msgid = 'ZDTE_0001'.
**        msgty = 'E'.
**        msgno = '012'.
**        msgv1 = 'No Se encontro Grafo Para seleccionar Cl.Doc'.  msgv2 = ''. msgv3 = ''. msgv4 = ''.
**
**      ENDIF.
**    ENDIF.
  ELSE.
    READ TABLE lt_ekpo WITH KEY ebeln = lt_zcb_recfactprov-ebeln.
    IF sy-subrc = 0.
      e_knttp = lt_ekpo-knttp.
    ELSE.
      e_knttp = ' '.
    ENDIF.
  ENDIF.
 READ TABLE lt_lfb1  WITH KEY lifnr = lt_zcb_recfactprov-lifnr
                              bukrs = lt_zcb_recfactprov-bukrs.

  IF estatus = st_ok. "ztfi_0087-st_ok.
    CASE lt_zcb_recfactprov-tipodte.
      WHEN '110' OR '111' OR '112'.
        READ TABLE lt_zdte_cldoc WITH KEY bukrs = lt_zcb_recfactprov-bukrs
                                          tipop = lt_lfb1-zzdte_tipo
                                          tipodte = lt_zcb_recfactprov-tipodte
                                          knttp = e_knttp
                                          bsart = e_bsart.
        IF sy-subrc NE 0.
          estatus = st_nok. "ztfi_0087-st_nok.
          msgid = 'ZDTE_0001'.
          msgty = 'E'.
          msgno = '009'.
          msgv1 = ''.  msgv2 = ''. msgv3 = ''. msgv4 = ''.
        ENDIF.
      WHEN OTHERS.
        READ TABLE lt_zdte_cldoc WITH KEY bukrs   = lt_zcb_recfactprov-bukrs
                                          tipop   = lt_lfb1-zzdte_tipo
                                          tipodte = lt_zcb_recfactprov-tipodte
                                          knttp   = e_knttp.
        IF sy-subrc NE 0.
          estatus = st_nok. "ztfi_0087-st_nok.
          msgid = 'ZDTE_0001'.
          msgty = 'E'.
          msgno = '009'.
          msgv1 = ''.  msgv2 = ''. msgv3 = ''. msgv4 = ''.
        ENDIF.
    ENDCASE.
  ENDIF.

ENDFORM.                    "VALIDACION_201
