*&---------------------------------------------------------------------*
*& Report  ZFIR_0058
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

INCLUDE ZFIR_0058_TOP                .    " global Data

INCLUDE ZFIR_0058_F01                .  " FORM-Routines


START-OF-SELECTION.
 TYPES: BEGIN OF ty_t001,
            bukrs TYPE t001-bukrs,
         END OF ty_t001.

  DATA: lt_t001 TYPE STANDARD TABLE OF ty_t001 WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
  SELECT bukrs
  FROM t001
  INTO TABLE lt_t001
  WHERE bukrs IN s_bukrs.
  IF sy-dbcnt >= 1.
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
  PERFORM generar_cont.
