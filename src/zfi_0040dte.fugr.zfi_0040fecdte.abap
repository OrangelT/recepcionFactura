FUNCTION zfi_0040fecdte.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(START_COLUMN) DEFAULT 25
*"     VALUE(START_ROW) DEFAULT 6
*"  EXPORTING
*"     REFERENCE(ANSWER) TYPE  SYUCOMM
*"  CHANGING
*"     VALUE(LS_FECHAC) TYPE  DATUM OPTIONAL
*"----------------------------------------------------------------------

  DATA: x1 TYPE i VALUE 5,
        y1 TYPE i VALUE 5,
        x2 TYPE i VALUE 10,
        y2 TYPE i VALUE 10.

  x1 = start_column.
  x2 = x1       + 30.
  y1 = start_row.
  y2 = start_row + 2.

 CALL SCREEN 900 STARTING AT x1 y1 ENDING AT x2 y2.
*
  CASE ok_code.
    WHEN 'ENTE'.
      ls_fechac = gv_fechac.
    WHEN 'CANC'.
      CLEAR gv_fechac.
    WHEN OTHERS.
      CLEAR gv_fechac.
  ENDCASE.
  answer = ok_code.


ENDFUNCTION.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0900  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0900 OUTPUT.
  SET PF-STATUS '0900'.
  SET TITLEBAR '001' with 'Mod. fecha Contable'.
ENDMODULE.

MODULE user_command_0900 INPUT.
 CASE ok_code.
    WHEN 'ENTE'.
      IF gv_fechac <> space.
        SET SCREEN 0.
        LEAVE SCREEN.
      ELSE.
        MESSAGE s012(zdte_0001) WITH 'Debe Indicar fecha Contable'.
      ENDIF.
    WHEN 'CANC'.
      SET SCREEN 0.
      LEAVE SCREEN.
*    WHEN 'TCOP'.
*      gv_targettext = gv_sourcetext.
  ENDCASE.
ENDMODULE.
