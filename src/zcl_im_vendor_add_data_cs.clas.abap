class ZCL_IM_VENDOR_ADD_DATA_CS definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_VENDOR_ADD_DATA_CS .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_VENDOR_ADD_DATA_CS IMPLEMENTATION.


METHOD IF_EX_VENDOR_ADD_DATA_CS~GET_DATA.
  FIELD-SYMBOLS:<fse> TYPE ANY.
  CONSTANTS: c_tipo(35) VALUE '(SAPLZFI_0023)GV_TIPO'.

  ASSIGN (c_tipo) TO <fse>.
  IF sy-subrc = 0.
    s_lfb1-zzdte_tipo = <fse>.
    UNASSIGN <fse>.
  ENDIF.

  DATA datos9000 TYPE lfb1.
  CALL FUNCTION 'ZFI_0024_GET_DATA'
    IMPORTING
      i_lfb1 = datos9000.
  IF datos9000 IS NOT INITIAL.
    s_lfb1-zznombana = datos9000-zznombana.
    s_lfb1-zzaprobador = datos9000-zzaprobador.
    s_lfb1-zzdtcta = datos9000-zzdtcta.
  ENDIF.

ENDMETHOD.


method IF_EX_VENDOR_ADD_DATA_CS~GET_FIELDNAME_FOR_CHANGEDOC.
endmethod.


METHOD IF_EX_VENDOR_ADD_DATA_CS~GET_TAXI_SCREEN.

  FIELD-SYMBOLS:<fse> TYPE ANY.
  UNASSIGN <fse>.
  CONSTANTS: c_tipo(35) VALUE '(SAPMF02K)RF02K-BUKRS'.

  ASSIGN (c_tipo) TO <fse>.
  IF <fse> IS ASSIGNED.
    IF <fse> IS INITIAL.
      MESSAGE 'No se ha indicado sociedad' TYPE 'I'.
      LEAVE TO SCREEN 0101.
    ELSE.
      IF flt_val = 'ZD'  AND i_taxi_fcode = 'ZDTETIP'.
        e_screen   = '9000'.
        e_program  = 'SAPLZFI_0023'.
        e_headerscreen_layout = 'B'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD IF_EX_VENDOR_ADD_DATA_CS~SET_DATA.

  CALL FUNCTION 'ZFI_0024'
    EXPORTING
      i_tipo = i_lfb1-zzdte_tipo
      i_act  = i_activity
      i_lfb1 = i_lfb1.

ENDMETHOD.


method IF_EX_VENDOR_ADD_DATA_CS~SET_FCODE.
endmethod.


method IF_EX_VENDOR_ADD_DATA_CS~SUPPRESS_TAXI_TABSTRIPS.
endmethod.
ENDCLASS.
