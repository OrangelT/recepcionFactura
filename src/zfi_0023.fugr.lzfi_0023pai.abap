*----------------------------------------------------------------------*
***INCLUDE LZFI_0023PAI .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  CARGAR_TIPOS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cargar_tipos INPUT.
  TYPE-POOLS : vrm.
  DATA: itab2 TYPE vrm_values,
        wa2   LIKE LINE OF itab2.

****1******* ********* ********* ********* ********* ********* ***
  REFRESH : itab2[].

  DATA: lt_dom2 TYPE STANDARD TABLE OF dd07v WITH HEADER LINE.

  CLEAR lt_dom2.

  FREE lt_dom2.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname    = 'ZFI_DOM_TIPO'
      text       = 'X'
    TABLES
      values_tab = lt_dom2.


  TYPES: BEGIN OF ty_modalidad,
           texto     TYPE zdte_desctipo, "char100,
           modalidad TYPE zfi_tipo,
         END OF ty_modalidad.
  DATA: lt_modalidad TYPE STANDARD TABLE OF ty_modalidad WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.


  FREE lt_modalidad.

  LOOP AT lt_dom2.
    CLEAR: lt_modalidad.
    lt_modalidad-texto = lt_dom2-ddtext.
    lt_modalidad-modalidad = lt_dom2-domvalue_l.
    APPEND lt_modalidad.
  ENDLOOP.
  DATA: lt_retorno_hp TYPE STANDARD TABLE OF ddshretval WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
  CLEAR lt_retorno_hp.
  FREE lt_retorno_hp.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'Tipo'
      dynpprog        = sy-repid
      value_org       = 'S'
    TABLES
      value_tab       = lt_modalidad
      return_tab      = lt_retorno_hp
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc = 0.
    READ TABLE lt_retorno_hp  INDEX 1.
    IF sy-subrc = 0.
      DATA: lt_pantalla_hp TYPE STANDARD TABLE OF dynpread WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
      CLEAR lt_pantalla_hp.
      FREE lt_pantalla_hp.

      lt_pantalla_hp-fieldname = 'GV_TIPO'.
      lt_pantalla_hp-fieldvalue = lt_retorno_hp-fieldval.
      APPEND lt_pantalla_hp.


      CALL FUNCTION 'DYNP_VALUES_UPDATE'
        EXPORTING
          dyname     = sy-cprog
          dynumb     = sy-dynnr
        TABLES
          dynpfields = lt_pantalla_hp.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CARGAR_TIPOS  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_TIPODTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_tipodte INPUT.
  DATA: lt_dom3 TYPE STANDARD TABLE OF dd07v WITH HEADER LINE.

  CLEAR lt_dom3.

  FREE lt_dom3.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname    = 'ZFI_DOM_TIPO'
      text       = 'X'
    TABLES
      values_tab = lt_dom3.

  CLEAR ls_tipo.
  IF  gv_tipo IS NOT INITIAL.

    READ TABLE lt_dom3 WITH KEY domvalue_l = gv_tipo.
    IF sy-subrc NE 0.
      MESSAGE ID 'ZDTE_0001' TYPE 'E' NUMBER '073'
      WITH gv_tipo.
    ENDIF.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_APROBADOR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_aprobador INPUT.
 CLEAR ls_bname.
  IF datos9000-aprobador IS NOT INITIAL.
    SELECT SINGLE bname INTO ls_bname FROM usr02
     WHERE  bname EQ datos9000-aprobador.
    IF sy-subrc NE 0.
      MESSAGE ID 'ZDTE_0001' TYPE 'E' NUMBER '074'
                   WITH datos9000-aprobador.

    ENDIF.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_ANALISTA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_analista INPUT.
 CLEAR ls_bname.
  IF datos9000-analista IS NOT INITIAL.
    SELECT SINGLE bname INTO ls_bname FROM usr02
    WHERE  bname EQ datos9000-analista.
    IF sy-subrc NE 0.
      MESSAGE ID 'ZDTE_0001' TYPE 'E' NUMBER '074'
                       WITH datos9000-analista.
    ENDIF.
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  CHECK_ZZDTCTA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_zzdtcta INPUT.
  CLEAR ls_dtcta.
  IF datos9000-zzdtcta IS NOT INITIAL.
    SELECT SINGLE dtcta INTO ls_dtcta
    FROM ztfi_0091
    WHERE dtcta EQ datos9000-zzdtcta.
    IF sy-subrc NE 0.
      MESSAGE ID 'ZDTE_0001' TYPE 'E' NUMBER '123'
                       WITH datos9000-zzdtcta.
    ENDIF.

  ENDIF.
ENDMODULE.
