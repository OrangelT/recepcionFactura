*&---------------------------------------------------------------------*
*&  Include           ZFIR_0052_E01
*&---------------------------------------------------------------------*
*&******************************************************************** *
*                 I N I T I A L I Z A T I O N                          *
*&******************************************************************** *
INITIALIZATION.
  AUTHORITY-CHECK OBJECT 'S_TCODE'
                  ID     'TCD'
                  FIELD  sy-tcode.
  IF sy-subrc NE 0.
    MESSAGE e077(s#) WITH sy-tcode.
  ENDIF.

*&******************************************************************** *
*              A T   S E L E C T I O N - S C R E E N                   *
*&******************************************************************** *
AT SELECTION-SCREEN ON so_bukrs.
  SELECT bukrs butxt INTO TABLE gt_t001
  FROM t001
  WHERE bukrs IN so_bukrs.
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

*&******************************************************************** *
*               S T A R T - O F - S E L E C T I O N                    *
*&******************************************************************** *
START-OF-SELECTION.
**//.. Instancear clase
  TRY.
      CREATE OBJECT gr_dte_recep
        EXPORTING
          i_bukrs_r = so_bukrs[]
          i_flagdir = space.

      IF gr_dte_recep IS BOUND.
**//.. Importar Archivo
        CALL METHOD gr_dte_recep->delete_prcxml.
**//.. Display Log
        CALL METHOD gr_dte_recep->display_log.
      ENDIF.
    CATCH cx_sql_exception.
*      WRITE: text-100.
  ENDTRY.
