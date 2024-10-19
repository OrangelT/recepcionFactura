*&---------------------------------------------------------------------*
*& Report  ZFIR_0058V2
*&
*&---------------------------------------------------------------------*
*&Autor: Orangel Tochón Allen                                          *
*&Fecha: 29.01.2019                                                    *
*&Reporte : Proceso de Precontabilización                              *
*&---------------------------------------------------------------------*
INCLUDE ZFIR_0058V2_TOP_OLD.
*INCLUDE zfir_0058v2_top.

*----------------------------------------------------------------------*
*                 I N I T I A L I Z A T I O N                          *
*----------------------------------------------------------------------*
INITIALIZATION.
  AUTHORITY-CHECK OBJECT 'S_TCODE'
                  ID     'TCD'
                  FIELD  sy-tcode.
  IF sy-subrc NE 0.
    MESSAGE e077(s#) WITH sy-tcode.
  ENDIF.

*----------------------------------------------------------------------*
* SELECTION-SCREEN                                                     *
*----------------------------------------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_bukrs FOR  ztfi_0074-bukrs OBLIGATORY,
                  s_fecha FOR  ztfi_0074-bldat,
                  s_folio FOR ztfi_0074-xblnr,
                  s_status FOR ztfi_0074-status OBLIGATORY.
  SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
*  AT SELECTION-SCREEN OUTPUT                                          *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  SELECT bukrs butxt INTO CORRESPONDING FIELDS OF TABLE gt_t001
 FROM t001
 WHERE bukrs IN s_bukrs.
  IF sy-subrc EQ 0.
    LOOP AT gt_t001 INTO gs_t001.
      PERFORM fo_authority_check  USING    gs_t001-bukrs
                                  CHANGING sy-subrc.
      IF sy-subrc NE 0.
        MESSAGE e800(fr) WITH gs_t001-bukrs.
      ENDIF.
    ENDLOOP.
  ELSE. "???
    MESSAGE e101(f5).
  ENDIF.


*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM data_seleccionar.
  PERFORM ejecuta_proceso.


*----------------------------------------------------------------------*
* END-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM generar_alv.

INCLUDE ZFIR_0058V2_F01_OLD.
*  INCLUDE zfir_0058v2_f01.
INCLUDE ZFIR_0058V2_F02_OLD.
*  INCLUDE zfir_0058v2_f02.
