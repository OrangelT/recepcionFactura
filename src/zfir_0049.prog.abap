*&---------------------------------------------------------------------*
*& Report  ZFIR_0049
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*&Change requesr    : Proyecto recepción de factura electronica
*&Analista          :
*&Author            : Orangel Tochón
*&Date              : 30/05/2018
*&Development ID    :
*&Description       : Programa de Obtención de XML/PDF de custodio
*&
*&=====================================================================*
REPORT zfir_0049.
INCLUDE zfir_0049_top.
*----------------------------------------------------------------------*
* SELECTION-SCREEN                                                     *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: so_bukrs FOR t001-bukrs OBLIGATORY. " Sociedad
SELECT-OPTIONS: so_fecha FOR sy-datum.
PARAMETERS: i_flag TYPE flag AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK blk1.

*----------------------------------------------------------------------*
*                 I N I T I A L I Z A T I O N                          *
*----------------------------------------------------------------------*
INITIALIZATION.
  AUTHORITY-CHECK OBJECT 'S_TCODE'
                  ID     'TCD'
                  FIELD  co_tran.
  IF sy-subrc NE 0.
    MESSAGE e077(s#) WITH sy-tcode.
  ENDIF.

*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM validar.

  CLEAR wa_itabv. REFRESH itabv. CLEAR co_type. CLEAR wa_0078. REFRESH t_0078.
  SELECT * INTO TABLE t_0078 FROM ztfi_0078
     WHERE bukrs IN so_bukrs.

  LOOP AT t_0078 INTO wa_0078.
    wa_itabv-vatint = wa_0078-vatint.
    COLLECT  wa_itabv INTO itabv.
  ENDLOOP.

  IF lines( itabv ) NE 1 .
    MESSAGE e007(zdte_0001) WITH 'Todas las sociedades deben tener la misma variante de Interfaz'.
  ELSE.

    SELECT SINGLE nombre_int INTO co_type FROM ztfi_0078b
       WHERE vatint = wa_itabv-vatint
       AND   codint = '0000'.
    IF sy-subrc NE 0.
      MESSAGE e007(zdte_0001) WITH 'La variante Interfaz No actualizada Para leer XML'.
    ENDIF.

  ENDIF.
  IF co_type IS INITIAL.
    MESSAGE e007(zdte_0001) WITH 'La variante Interfaz No actualizada Para leer XML'.
  ENDIF.
**//.. Instancear clase
  TRY.
      CREATE OBJECT gr_dte_recep TYPE (co_type)
        EXPORTING
          i_bukrs_r = so_bukrs[]
          i_flagdir = ' '.

      IF gr_dte_recep IS BOUND.
**//.. Importar Archivo
        TRY.
            CALL METHOD gr_dte_recep->('WS_IMPORT_DOCUMENTS') "ws_import_documents.
              EXPORTING
                s_bukrs   = so_bukrs[]
                s_fecha   = so_fecha[]
                p_creasol = i_flag.
          CATCH cx_root INTO lo_err.
            WRITE: lo_err->get_text( ).
        ENDTRY.
**//.. Display Log
        TRY.
            CALL METHOD gr_dte_recep->('DISPLAY_LOG'). "display_log.
          CATCH cx_root INTO lo_err.
            WRITE: lo_err->get_text( ).
        ENDTRY.

      ENDIF.

    CATCH cx_sql_exception.
    CATCH cx_root INTO lo_err.
      WRITE: lo_err->get_text( ).
*      WRITE: text-100.
  ENDTRY.

  INCLUDE zfir_0049_e01.
