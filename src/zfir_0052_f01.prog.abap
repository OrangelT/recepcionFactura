*&---------------------------------------------------------------------*
*&  Include           ZFIR_0052_F01
*&---------------------------------------------------------------------*
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
