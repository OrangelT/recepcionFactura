*&---------------------------------------------------------------------*
*& Report ZFIR_0058V2
*&---------------------------------------------------------------------*
*&Autor: Orangel Tochón Allen                                          *
*&Fecha: 22.02.2021                                                    *
*&Reporte : Proceso de Contabilización                              *
*&---------------------------------------------------------------------**&
REPORT ZFIR_0058V2.
INCLUDE zfir_0058v2_top.

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
*  AT SELECTION-SCREEN OUTPUT                                          *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.


*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM validar.

  IF co_type IS INITIAL.
    MESSAGE e007(zdte_0001) WITH 'La Var.Interfaz No actualizada Para Contabilizar'.
  ENDIF.

  TRY.
      CREATE OBJECT wa_objeto TYPE (co_type)
           EXPORTING
             i_bukrs =  s_bukrs[]
             i_xblnr =  s_folio[]
             i_status = s_status[]
             i_bldat =  s_fecha[].
      IF wa_objeto IS BOUND.
**//.. Seleccionar datos
        CALL METHOD wa_objeto->('DATA_SELECCIONAR').
**//.. Procesar documentos Electronios
        CALL METHOD wa_objeto->('EJECUTA_PROCESO')
          IMPORTING
            ti_log = ti_log[].
      ENDIF.
    CATCH cx_sql_exception.
  ENDTRY.

*----------------------------------------------------------------------*
* END-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
END-OF-SELECTION.
**//.. Ejecutar Salida de Log
  PERFORM generar_alv.
  INCLUDE zfir_0058v2_f01.
