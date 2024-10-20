*----------------------------------------------------------------------*
***INCLUDE ZFIR_0053_F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_SCREEN.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_screen .
  DATA: lv_input TYPE screen-input.

  IF r_sii EQ abap_false.

    LOOP AT SCREEN.

      "---> para PC el campo s_bukrs-High no se debe ver
      IF r_pc EQ abap_true.
        IF screen-name CP 'SO_BUKRS-HIGH'.
          screen-input = '0'.
          screen-invisible = '1'.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name CP '%_SO_BUKRS_%_APP_%-OPTI_PUSH'.
          screen-input = '0'.
          screen-invisible = '1'.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name CP '%_SO_BUKRS_%_APP_%-VALU_PUSH'.
          screen-input = '0'.
          screen-invisible = '1'.
          MODIFY SCREEN.
        ENDIF.

      ENDIF.

     "-----> Fecha solo para WebservicesSII
      CASE screen-group1.
        WHEN 'CCC'.
          screen-input = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.
  ENDIF.

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
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FO_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_T001_BUKRS  text
*      <--P_SY_SUBRC  text
*----------------------------------------------------------------------*
FORM fo_authority_check  USING   u_bukrs TYPE bukrs
                         CHANGING c_subrc TYPE sysubrc.
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
            ID 'BUKRS' FIELD u_bukrs
            ID 'ACTVT' FIELD '01'
            ID 'ACTVT' FIELD '02'.
  c_subrc = sy-subrc.

ENDFORM.
