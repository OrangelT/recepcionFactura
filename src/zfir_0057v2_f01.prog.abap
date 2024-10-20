*----------------------------------------------------------------------*
***INCLUDE ZFIR_0057V2_F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GENERAR_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generar_alv .
  DATA: lt_catalogo TYPE slis_t_fieldcat_alv,
        ln_catalogo LIKE LINE OF lt_catalogo.

  FREE lt_catalogo.

  CLEAR ln_catalogo.
  ln_catalogo-fieldname = 'ESTADO'.
  ln_catalogo-tabname = 'TI_LOG'.
  ln_catalogo-seltext_s = TEXT-105."'Estado'.
  APPEND ln_catalogo TO lt_catalogo.

  CLEAR ln_catalogo.
  ln_catalogo-fieldname = 'DOCUMENTO'.
  ln_catalogo-tabname = 'TI_LOG'.
  ln_catalogo-seltext_s = TEXT-104."'Documento'.
  APPEND ln_catalogo TO lt_catalogo.

  CLEAR ln_catalogo.
  ln_catalogo-fieldname = 'SOCIEDAD'.
  ln_catalogo-tabname = 'TI_LOG'.
  ln_catalogo-seltext_s = TEXT-103."'Sociedad'.
  APPEND ln_catalogo TO lt_catalogo.

  CLEAR ln_catalogo.
  ln_catalogo-fieldname = 'PROVEEDOR'.
  ln_catalogo-tabname = 'TI_LOG'.
  ln_catalogo-seltext_s = TEXT-102."'Proveedor'.
  APPEND ln_catalogo TO lt_catalogo.

  CLEAR ln_catalogo.
  ln_catalogo-fieldname = 'FECHA'.
  ln_catalogo-tabname = 'TI_LOG'.
  ln_catalogo-seltext_s = TEXT-101."'Fecha'.
  APPEND ln_catalogo TO lt_catalogo.

  CLEAR ln_catalogo.
  ln_catalogo-fieldname = 'MENSAJE'.
  ln_catalogo-tabname = 'TI_LOG'.
  ln_catalogo-seltext_s = TEXT-100."'Mensaje'.
  APPEND ln_catalogo TO lt_catalogo.

  DATA: lv_layout TYPE slis_layout_alv.
  CLEAR lv_layout.

  lv_layout-colwidth_optimize = 'X'.
  lv_layout-zebra = 'X'.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = lv_layout
      it_fieldcat        = lt_catalogo
    TABLES
      t_outtab           = ti_log
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VALIDAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validar .
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
** Validar y buscar Clase segun cod.Variante de interfaz
  CLEAR wa_itabv. REFRESH itabv. CLEAR co_type. CLEAR wa_0078. REFRESH t_0078.
  SELECT * INTO TABLE t_0078 FROM ztfi_0078
     WHERE bukrs IN s_bukrs.

  IF lines( gt_t001[] ) NE lines( t_0078 ).
    MESSAGE e007(zdte_0001) WITH 'Sociedad No Conf. Para Rece.Fact.electronica'.
  ENDIF.

  LOOP AT t_0078 INTO wa_0078.
    wa_itabv-vatint = wa_0078-vatint.
    COLLECT  wa_itabv INTO itabv.
  ENDLOOP.

  IF lines( itabv ) NE 1 .
    MESSAGE e007(zdte_0001) WITH 'Todas las Soc. deben tener misma variante de Interfaz'.
  ELSE.

    SELECT SINGLE nombre_int INTO co_type FROM ztfi_0078b
       WHERE vatint = wa_itabv-vatint
       AND   codint = '0006'.
    IF sy-subrc NE 0.
      MESSAGE e007(zdte_0001) WITH 'La Var.Interfaz No actualizada para Precontabilizar'.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FO_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_T001_BUKRS  text
*      <--P_SY_SUBRC  text
*----------------------------------------------------------------------*
FORM fo_authority_check  USING     u_bukrs TYPE bukrs
                         CHANGING c_subrc TYPE sysubrc.


  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
           ID 'BUKRS' FIELD u_bukrs
           ID 'ACTVT' FIELD '01'
           ID 'ACTVT' FIELD '02'.
  c_subrc = sy-subrc.

ENDFORM.
