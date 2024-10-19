*&---------------------------------------------------------------------*
*& Include          ZFIR_0050_E01
*&---------------------------------------------------------------------*
*&******************************************************************** *
*                 I N I T I A L I Z A T I O N                          *
*&******************************************************************** *
*
INITIALIZATION.
  AUTHORITY-CHECK OBJECT 'S_TCODE'
                  ID     'TCD'
                  FIELD  sy-tcode.
  IF sy-subrc NE 0.
    MESSAGE e077(s#) WITH sy-tcode.
  ENDIF.

*&******************************************************************** *
*               S T A R T - O F - S E L E C T I O N                    *
*&******************************************************************** *
START-OF-SELECTION.
**//.. Instancear clase
  CLEAR wa_itab. REFRESH itab. CLEAR co_type. CLEAR wa_0078. REFRESH t_0078.
  TRY.
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
           AND   codint = '0001'.
        IF sy-subrc NE 0.
          MESSAGE e007(zdte_0001) WITH 'La variante Interfaz No actualizada Para leer XML'.
        ENDIF.

      ENDIF.
      IF co_type IS INITIAL.
        MESSAGE e007(zdte_0001) WITH 'La variante Interfaz No actualizada Para leer XML'.
      ENDIF.

      CREATE OBJECT gr_dte_recep TYPE (co_type)
        EXPORTING
          i_bukrs_r = so_bukrs[]
          i_flagdir = pa_fldir.

      IF gr_dte_recep IS BOUND.
**//.. Importar Archivo
        CALL METHOD gr_dte_recep->('IMPORT_FILE')."import_file.
**//.. Display Log
        CALL METHOD gr_dte_recep->('DISPLAY_LOG').
      ELSE.
      ENDIF.
    CATCH cx_sql_exception.
*      WRITE: text-100.
  ENDTRY.

  TRY. "LLama A Entidad Gubernamental para completar datos.

      CLEAR co_type.
      SELECT SINGLE nombre_int INTO co_type FROM ztfi_0078b
         WHERE vatint = wa_itabv-vatint
         AND   codint = '0002'.

      IF co_type IS  NOT INITIAL.

        CREATE OBJECT gr_dte_sii TYPE (co_type).
        IF  gr_dte_sii IS BOUND.
          CALL METHOD gr_dte_sii->('MAIN')
            EXPORTING
              r_bukrs    = so_bukrs[]
              pa_fldir = pa_fldir.
**//.. Display Log
          CALL METHOD gr_dte_sii->('DISPLAY_LOG').
        ENDIF.
      ENDIF.
  ENDTRY.

*      CREATE OBJECT gr_dte_sii.
*      IF  gr_dte_sii IS BOUND.
*        cant = 1.
*        IF pa_fldir = 'X' .
*          cant = 500.
*        ELSE.
*          cant = 100.
*        ENDIF.
*        SELECT * FROM ztfi_0074fr UP TO cant ROWS
*        INTO TABLE lt_zcb_recfactprovfechasii
*        WHERE bukrs IN so_bukrs
*          AND estatus EQ space
*          AND tipodte NE '56' AND tipodte NE '61'
*          ORDER BY PRIMARY KEY.
*
*        IF lt_zcb_recfactprovfechasii[] IS NOT INITIAL.
*          "----> Selecciono sociedades
*          LOOP AT lt_zcb_recfactprovfechasii INTO wa.
*            wa_itab-bukrs  = wa-bukrs.
*            COLLECT wa_itab INTO itab.
*          ENDLOOP.
*
*          "----> Busco los registros por soc.y los envio a buscar Fecha SII
*          LOOP AT itab INTO wa_itab.
*            CLEAR wa. REFRESH lt_zcb_recfactprovfechasii_soc.
*            LOOP AT lt_zcb_recfactprovfechasii INTO wa WHERE bukrs = wa_itab-bukrs.
*              APPEND  wa TO  lt_zcb_recfactprovfechasii_soc.
*            ENDLOOP.
*
*            CALL METHOD gr_dte_sii->('WRITE_FECHASII')
*              EXPORTING
*                s_bukrs                    = wa_itab-bukrs
*              CHANGING
*                lt_zcb_recfactprovfechasii = lt_zcb_recfactprovfechasii_soc[].
*          ENDLOOP.
*        ELSE.
*          IF go_log IS NOT BOUND.
*            CLEAR: gs_bal_log, gs_bal_msg.
*            "---> Crea Objeto Logs de interfaz.
*            CONCATENATE sy-datum sy-uzeit
*                        INTO gs_bal_log-extnumber.
*
*            gs_bal_log-object     = 'ZDTE'.
*            gs_bal_log-subobject  = 'RECEPCION'.
*            gs_bal_log-aldate     = syst-datum.
*            gs_bal_log-altime     = syst-uzeit.
*            gs_bal_log-aluser     = syst-uname.
*            gs_bal_log-alprog     = syst-repid.
*            FREE go_log.
*            CREATE OBJECT go_log
*              EXPORTING
*                i_s_object = gs_bal_log.
*            CLEAR gs_bal_msg.
*            gs_bal_msg-msgty = 'S'.
*            gs_bal_msg-msgid = 'ZDTE_0001'.
*            gs_bal_msg-msgno = '046'.
*            gs_bal_msg-msgv1 = ''.
*            go_log->add_msg( i_s_msg = gs_bal_msg ).
*            go_log->save( ).
*          ENDIF.
*
*        ENDIF.
*      ENDIF.
*
*      CALL METHOD gr_dte_recep->('DISPLAY_LOG').
*
*    CATCH cx_sql_exception.
*
*  ENDTRY.
