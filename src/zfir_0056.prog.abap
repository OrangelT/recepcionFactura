*&---------------------------------------------------------------------*
*& Report  ZFIR_0055
*&
*&---------------------------------------------------------------------*
*&Autor: Orangel Tochón Allen                                          *
*&Fecha: 01.07.2020                                                    *
*&Reporte : reporte de Ajuste de Fechas Contables                      *
*&---------------------------------------------------------------------*
INCLUDE zfir_0056_top.


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

  SELECT-OPTIONS: s_bukrs  FOR ztfi_0074-bukrs   OBLIGATORY,
                  s_doc    FOR ztfi_0074-belnr,
                  s_anno   FOR ztfi_0074-gjahr,
                  s_lifnr  FOR ztfi_0074-lifnr,
                  s_bldat  FOR ztfi_0074-bldat,
                  s_fsii   FOR ztfi_0074fr-fecharsii,
                  s_fecb   FOR ztfi_0074-fechabase,
                  s_stat   FOR ztfi_0074-status.


  SELECTION-SCREEN END OF BLOCK b1.
  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS : s_fecha TYPE datum OBLIGATORY.
  SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN                                                  *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

*----------------------------------------------------------------------*
*  AT SELECTION-SCREEN OUTPUT                                          *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM autorizacion.
  PERFORM ejecucion.

*----------------------------------------------------------------------*
* END-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
END-OF-SELECTION.
  INCLUDE zfir_0056_f01.
