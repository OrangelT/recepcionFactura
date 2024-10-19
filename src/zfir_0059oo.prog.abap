*&---------------------------------------------------------------------*
*& Report  ZFIR_0059OO
*&
*&---------------------------------------------------------------------*
*&Autor: Orangel Tochón Allen                                          *
*&Fecha: 10.01.2018                                                    *
*&Reporte : Monitor de Factura electronica OO                          *
*&---------------------------------------------------------------------*
INCLUDE zfir_0059oo_top.

*----------------------------------------------------------------------*
* SELECTION-SCREEN                                                     *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_bukrs TYPE bukrs OBLIGATORY.
SELECT-OPTIONS: s_dtet  FOR  lfb1-zzdte_tipo,
                s_lifnr FOR ztfi_0074-lifnr,
                s_stcd1 FOR lfa1-stcd1,
                s_tipd FOR   ztfi_0074-tipodte,
                s_folio FOR ztfi_0074-xblnr,
                s_bldat FOR ztfi_0074-bldat,
                s_cpudt FOR ztfi_0074-cpudt,
                 p_resp FOR wrf_ppw_us_usgp-bname.
SELECT-OPTIONS:  p_status FOR ztfi_0074-status.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN PUSHBUTTON 2(23)  b2 USER-COMMAND usr2.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: lc_dias TYPE zdte_a_evaluation_period DEFAULT 365 MODIF ID m2,
            lc_flag TYPE flag  MODIF ID m2.
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  b2 = 'Button2'.
  v_flag = 'X'.
*----------------------------------------------------------------------*
*  AT SELECTION-SCREEN.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

* Check the user command.
  IF sy-ucomm = 'USR2'.

    IF v_flag IS INITIAL.
      v_flag = 'X'.
    ELSE.
      CLEAR v_flag.
    ENDIF.
  ENDIF.
*----------------------------------------------------------------------*
*  AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  IF v_flag = 'X'.
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name   = icon_expand
        text   = 'Config.'
        info   = 'Datos Adicionales'
      IMPORTING
        result = b2
      EXCEPTIONS
        OTHERS = 0.
  ELSEIF v_flag IS INITIAL.
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name   = icon_collapse
        text   = 'Config.'
        info   = 'Datos Adicionales'
      IMPORTING
        result = b2
      EXCEPTIONS
        OTHERS = 0.
  ENDIF.
  LOOP AT SCREEN.
* Expand collapse block2
    IF v_flag = 'X' AND screen-group1 = 'M2'.
      screen-active = 0.
    ELSEIF v_flag IS INITIAL AND screen-group1 = 'M2'.
      screen-active = 1.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM validar.
  PERFORM data_retrieval.
  PERFORM display_settings.


  INCLUDE zfir_0059oo_f01.
