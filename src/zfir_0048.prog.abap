*&---------------------------------------------------------------------*
*& Report ZFIR_0048
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir_0048.


AUTHORITY-CHECK OBJECT 'S_TCODE'
                 ID     'TCD'
                 FIELD  sy-tcode.
IF sy-subrc NE 0.
  MESSAGE e077(s#) WITH sy-tcode.
ENDIF.

CALL FUNCTION 'STREE_EXTERNAL_DISPLAY'
  EXPORTING
    structure_id = '0299DF38BB181EDC869010659963639F'
    language     = sy-langu.
