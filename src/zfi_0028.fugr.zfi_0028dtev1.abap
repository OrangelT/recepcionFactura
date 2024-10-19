FUNCTION ZFI_0028DTEV1.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(I_CODTEXTOC) TYPE  CHAR20 OPTIONAL
*"     VALUE(I_CORREO) TYPE  CHAR100 OPTIONAL
*"     VALUE(I_DOC) TYPE  ZST_DTE_ACK OPTIONAL
*"     VALUE(I_XML) TYPE  XSTRING OPTIONAL
*"     VALUE(I_PDF) TYPE  XSTRING OPTIONAL
*"     VALUE(I_CSENDER) TYPE  CHAR100 OPTIONAL
*"  EXPORTING
*"     VALUE(E_RC) TYPE  SY-SUBRC
*"     VALUE(E_MSG) TYPE  BAPI_MSG
*"----------------------------------------------------------------------
 CHECK i_correo IS NOT INITIAL.

  PERFORM send_email  USING i_doc
                            i_correo
                            i_pdf
                            i_xml
                            i_csender
                            i_codtextoc
                      changing e_rc e_msg.




ENDFUNCTION.
