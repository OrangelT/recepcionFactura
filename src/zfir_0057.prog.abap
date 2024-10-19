*&---------------------------------------------------------------------*
*& Report  ZFIR_0057
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

INCLUDE zfir_0057_top             .    " global Data
*INCLUDE zfir_0057_f01             .  " FORM-Routines
 INCLUDE zfir_0057_f01nw           .  " FORM-Routines

START-OF-SELECTION.
  SELECT bukrs wfvar
  FROM t001
  INTO TABLE lt_t001
  WHERE bukrs IN s_bukrs.
  IF sy-dbcnt >= 1.
    sort lt_t001 by bukrs.
    delete ADJACENT DUPLICATES FROM lt_t001.
    LOOP AT lt_t001.
      AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
               ID 'BUKRS' FIELD lt_t001-bukrs
               ID 'ACTVT' FIELD '01'.
      IF sy-subrc NE 0.
        DATA: lv_mensaje TYPE char200.

        CLEAR lv_mensaje.
        CONCATENATE text-350 ' sociedad: ' lt_t001-bukrs INTO lv_mensaje.
        MESSAGE lv_mensaje TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE PROGRAM.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "---->menor nivel por sociedad para liberación.
  SELECT *
  FROM ztfi_0083
  INTO TABLE ti_ztfi_0083
  WHERE sociedad IN s_bukrs.
  IF sy-dbcnt >= 1.
    sort ti_ztfi_0083 by sociedad ASCENDING clase_documento ASCENDING  nivel ASCENDING.
    delete ADJACENT DUPLICATES FROM ti_ztfi_0083 comparing sociedad clase_documento nivel.
  ENDIF.

*  PERFORM generar_precont.
   PERFORM generar_precontnw.
