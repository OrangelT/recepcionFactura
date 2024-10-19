*----------------------------------------------------------------------*
***INCLUDE LZGF_ZVFI_0087I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
*  DATA repid_del(30).
*  DATA repid_marc LIKE sy-repid VALUE 'ZFIR_0062'.
*  DATA textline1(70).
*  DATA tmp_repid LIKE sy-repid.

**//..
  CASE function.
    WHEN 'CODE'.
      PERFORM obtain_data_record.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
