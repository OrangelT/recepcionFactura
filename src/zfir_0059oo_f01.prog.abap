*----------------------------------------------------------------------*
***INCLUDE ZFIR_0059OO_F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DATA_RETRIEVAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_retrieval .

  CREATE OBJECT wa_objeto TYPE (co_type)
    EXPORTING
      bukrs   = p_bukrs
      dtetipo = s_dtet[]
      lifnr   = s_lifnr[]
      stcd1   = s_stcd1[]
      bldat   = s_bldat[]
      nombana = p_resp[]
      folio = s_folio[]
      tipd = s_tipd[]
      status  = p_status[]
      dias      = lc_dias
      cpudt    = s_cpudt[]
      archivado = lc_flag.


  CALL METHOD wa_objeto->('MAIN').

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_SETTINGS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_settings .
  CALL SCREEN 9000.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'ZSTAND'.
  SET TITLEBAR 'ZFIR0059'."  OF PROGRAM 'ZFIR_0059OO'.
  CALL METHOD wa_objeto->('DISPLAY'). "display
*  IMPORTING
*    r_return =
  .

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  CASE sy-ucomm.
    WHEN '&F03'.  LEAVE TO SCREEN 0.
    WHEN '&F15'.  LEAVE TO SCREEN 0.
    WHEN '&F12'.  LEAVE TO SCREEN 0.
    WHEN '&SAVE'. PERFORM save.
  ENDCASE.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  Save
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save .
  DATA: lv_respuesta TYPE c.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question = TEXT-300
      text_button_1 = 'Si'
      text_button_2 = 'No'
    IMPORTING
      answer        = lv_respuesta.
  CHECK lv_respuesta = '1'.
  CALL METHOD wa_objeto->('SAVE').

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
  AUTHORITY-CHECK OBJECT 'ZFI01'
                 ID 'BUKRS' FIELD p_bukrs
                 ID 'ACTVT' FIELD '02'.
  IF sy-subrc NE 0.
    AUTHORITY-CHECK OBJECT 'ZFI01'
          ID 'BUKRS' FIELD p_bukrs
          ID 'ACTVT' FIELD '03'.
    IF sy-subrc NE 0.
      CLEAR lv_mensaje.
      CONCATENATE TEXT-350 ' sociedad: ' p_bukrs INTO lv_mensaje.
      MESSAGE lv_mensaje TYPE 'I' DISPLAY LIKE 'E'.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

  CLEAR wa_itabv. REFRESH itabv. CLEAR co_type. CLEAR wa_0078. REFRESH t_0078.
  SELECT SINGLE * INTO  wa_0078 FROM ztfi_0078
     WHERE bukrs EQ p_bukrs.

  SELECT SINGLE nombre_int INTO co_type FROM ztfi_0078b
     WHERE vatint = wa_0078-vatint
     AND   codint = '0003'.
  IF sy-subrc NE 0.
    MESSAGE e007(zdte_0001) WITH 'La variante Interfaz No Actualizada Para Monitor Factura Electronica'.
  ENDIF.

  IF co_type IS INITIAL.
    MESSAGE e007(zdte_0001) WITH 'La variante Interfaz No actualizada Monitor Factura Electronica'.
  ENDIF.


ENDFORM.
