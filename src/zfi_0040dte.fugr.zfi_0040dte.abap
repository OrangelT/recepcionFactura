FUNCTION zfi_0040dte.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(LS_SALIDA) TYPE  ZEFI_0026
*"     REFERENCE(FECHA_CIERRE) TYPE  SY-DATUM OPTIONAL
*"  EXPORTING
*"     REFERENCE(LS_SALIDA_M) TYPE  ZTFI_0074
*"----------------------------------------------------------------------
  DATA: gr_57v TYPE REF TO  object.
  DATA: lc_vatint TYPE ztfi_0078-vatint.
  DATA: co_type TYPE ztfi_0078b-nombre_int.
*-->Ini Manejo de fecha de contabilizacion <> a sy-datum
  DATA: fecha_in TYPE sy-datum.
*-->Fin Manejo de fecha de contabilizacion <> a sy-datum
  ls_salidag = ls_salida.

  SELECT SINGLE vatint INTO ( lc_vatint ) FROM ztfi_0078
         WHERE bukrs = ls_salidag-bukrs.

  SELECT SINGLE nombre_int INTO co_type FROM ztfi_0078b
         WHERE vatint = lc_vatint
         AND   codint = '0006'.

  FREE gr_57v.
  TRY.
      CREATE OBJECT gr_57v TYPE (co_type).

    CATCH cx_ai_system_fault.
  ENDTRY.
  PERFORM data_genera.
  PERFORM sutitucion.
  s_tipodte = ls_salidag-zdte_tipo.
  estatus =  ls_salidag-status.
*-->Ini Manejo de fecha de contabilizacion <> a sy-datum
  CALL METHOD gr_57v->('GET_FECHACIERRE')
    EXPORTING
      fecha_cierre = fecha_cierre.
*-->Fin Manejo de fecha de contabilizacion <> a sy-datum
  CALL METHOD gr_57v->('GENERAR_MIRA')
    EXPORTING
      lt_ekko       = lt_ekko[]
      lt_ekpo       = lt_ekpo[]
      lt_ztfi_0075  = lt_zdte_posfact_oc[] "lt_zdte_posfact
      lt_ekkn       = lt_ekkn[]
      lt_afko       = lt_afko[]
      lt_proj       = lt_proj[]
      lt_zdte_cldoc = lt_zdte_cldoc[]
      lt_t001       = lt_t001[]
      ti_vbwf16     = ti_vbwf16[]
      s_tipodte     = s_tipodte
      s_estatus     = estatus
    CHANGING
      p_entrada     = lt_zcb_recfactprov.
*-->Ini Manejo de fecha de contabilizacion <> a sy-datum
  CLEAR fecha_in.
  CALL METHOD gr_57v->('GET_FECHACIERRE')
    EXPORTING
      fecha_cierre = fecha_in.
*-->Fin Manejo de fecha de contabilizacion <> a sy-datum
  IF lt_zcb_recfactprov-belnr NE  ls_salidag-belnr.
    ls_salida_m = lt_zcb_recfactprov.
  ELSE.
  ENDIF.

ENDFUNCTION.
