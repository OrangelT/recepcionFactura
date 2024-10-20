*&---------------------------------------------------------------------*
*& Report ZFIR_0053
*&---------------------------------------------------------------------*
*&Autor: Orangel Tochón Allen
*&Fecha: 07.12.2021
*&Reporte : Proceso Facturas Cedidas a Factorin SII.
*&---------------------------------------------------------------------*
REPORT zfir_0053.

INCLUDE zfir_0053_top.
*----------------------------------------------------------------------*
* SELECTION-SCREEN                                                     *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: so_bukrs FOR t001-bukrs OBLIGATORY. " Sociedad
SELECT-OPTIONS: so_fecha FOR sy-datum  MODIF ID ccc.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE TEXT-002.
PARAMETERS: r_pc  TYPE flag RADIOBUTTON GROUP rd DEFAULT 'X' USER-COMMAND abcd,
            r_dr  TYPE flag RADIOBUTTON GROUP rd,
            r_sii TYPE flag RADIOBUTTON GROUP rd.

SELECTION-SCREEN END OF BLOCK blk2.

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
  PERFORM set_screen.

*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM validar.
**//.. Instancear clase
  TRY.
      CREATE OBJECT gr_dte_sii.
      IF r_sii = 'X'.

        IF gr_dte_sii IS BOUND.
**//.. Leer registro y buscar si tiene cesion
          CALL METHOD gr_dte_sii->ws_factorin
            EXPORTING
              s_bukrs       = so_bukrs[]
              p_fecha       = so_fecha[]
            IMPORTING
              e_factory_tab = lt_factory.
**//.. Display Log
          CALL METHOD gr_dte_sii->display_log.
        ENDIF.

      ELSE.

**//.. Se define si es Servidor o Pc
        IF r_dr = abap_true.
          pa_fldir = abap_true.
        ELSEIF r_pc = abap_true.
          CLEAR: pa_fldir.
        ENDIF.
**// Leer registros del archivo o Servidor
        CALL METHOD gr_dte_sii->arc_factorin
          EXPORTING
            s_bukrs = so_bukrs[]
            p_fecha = so_fecha[]
            p_dir   = pa_fldir.
**//.. Display Log
        CALL METHOD gr_dte_sii->display_log.
      ENDIF.
    CATCH cx_sql_exception.
  ENDTRY.

  INCLUDE zfir_0053_f01.
