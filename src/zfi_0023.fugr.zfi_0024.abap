FUNCTION ZFI_0024.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(I_TIPO) TYPE  ZFI_TIPO
*"     REFERENCE(I_ACT) TYPE  CHAR1
*"     VALUE(I_LFB1) TYPE  LFB1 OPTIONAL
*"----------------------------------------------------------------------
gv_tipo = i_tipo.
  gv_act = i_act.

  " Edwar Soto 10-08-2017
  CLEAR datos9000.
  datos9000-analista  = i_lfb1-zznombana.
  datos9000-aprobador = i_lfb1-zzaprobador.
  datos9000-zzdtcta = i_lfb1-zzdtcta.





ENDFUNCTION.
