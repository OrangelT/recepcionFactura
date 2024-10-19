*&---------------------------------------------------------------------*
*&  Include           ZFIR_0056_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  EJECUCION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ejecucion .

  TRY.
      CREATE OBJECT wa_objeto
        EXPORTING
          fecha_c = s_fecha.

      IF wa_objeto IS BOUND.
        CALL METHOD wa_objeto->buscardatos
          EXPORTING
            ls_bukrs     = s_bukrs[]
            ls_lifnr     = s_lifnr[]
            ls_bldat     = s_bldat[]
            ls_fechabase = s_fecb[]
            ls_fechasii  = s_fsii[]
            ls_belnr     = s_doc[]
            ls_gjahr     = s_anno[]
            ls_status    = s_stat[].

        CALL METHOD wa_objeto->mod_fecha_cierre( IMPORTING lt_mod = lt_logt ).
        CALL METHOD wa_objeto->display_log.
      ENDIF.
    CATCH cx_sql_exception.
  ENDTRY.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FO_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_T001_BUKRS  text
*      <--P_SY_SUBRC  text
*----------------------------------------------------------------------*
FORM fo_authority_check  USING    u_bukrs TYPE bukrs
                         CHANGING c_subrc TYPE sysubrc.
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
           ID 'BUKRS' FIELD u_bukrs
           ID 'ACTVT' FIELD '01'
           ID 'ACTVT' FIELD '02'.
  c_subrc = sy-subrc.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  AUTORIZACION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM autorizacion .
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

ENDFORM.
