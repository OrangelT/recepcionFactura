*----------------------------------------------------------------------*
***INCLUDE LZFI_0023O01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PANTALLA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pantalla OUTPUT.
  CHECK  gv_act = 'A'.

  LOOP AT SCREEN.
    IF screen-name = 'GV_TIPO'
      OR screen-name = 'DATOS9000-ANALISTA'
      OR screen-name = 'DATOS9000-APROBADOR'
      OR screen-name = 'DATOS9000-ZZDTCTA'
      .
      screen-input = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " PANTALLA  OUTPUT
