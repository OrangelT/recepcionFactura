*----------------------------------------------------------------------*
***INCLUDE LZGF_ZVFI_0075F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FO_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_U_BUKRS  text
*      <--P_SY_SUBRC  text
*----------------------------------------------------------------------*
FORM fo_authority_check  USING    u_bukrs TYPE bukrs
                         CHANGING c_subrc TYPE sysubrc.
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
           ID 'BUKRS' FIELD u_bukrs
           ID 'ACTVT' FIELD '01'
           ID 'ACTVT' FIELD '02'.
  c_subrc = sy-subrc.
ENDFORM.                    " FO_AUTHORITY_CHECK

*&---------------------------------------------------------------------*
*&      Form  FO_NEW_ENTRY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fo_new_entry.

  PERFORM fo_authority_check USING    zvfi_0075-bukrs
                             CHANGING sy-subrc.
  IF sy-subrc NE 0.
    MESSAGE e800(fr) WITH zvfi_0075-bukrs.
  ENDIF.
ENDFORM. " FO_NEW_ENTRY

*&---------------------------------------------------------------------*
*&      Form  FO_TABLE_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fo_table_get_data.
  DATA: ls_data_extract TYPE ty_extract.
  FIELD-SYMBOLS: <fs_data_total> TYPE ANY.

  PERFORM get_data_zvfi_0075.

  LOOP AT total.
    ASSIGN total TO <fs_data_total>.
    ls_data_extract = <fs_data_total>.

    PERFORM fo_authority_check USING    ls_data_extract-bukrs
                               CHANGING sy-subrc.
    IF sy-subrc NE 0.
      DELETE total.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "FO_TABLE_GET_DATA
