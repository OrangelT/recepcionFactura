FUNCTION zfi_0050dte.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(P_ENTRADA) TYPE  ZTFI_0074
*"  EXPORTING
*"     VALUE(P_RESULT) TYPE  SY-SUBRC
*"----------------------------------------------------------------------

  DATA: lv_pdf    TYPE xstring,
        lv_fname  TYPE string,
        lv_size   TYPE wsrm_error-wsrm_direction, "i,
        ls_obja   TYPE borident,
        ls_objb   TYPE borident,
        lv_obj    TYPE swc_object,
        ls_binrel TYPE gbinrel,
        lv_belnr  TYPE belnr_d,
        lv_gjahr  TYPE gjahr,
        lv_awkey  TYPE awkey,
        lt_binatt TYPE STANDARD TABLE OF brelattr,
        lt_data   TYPE TABLE OF solix.

  CLEAR: ls_obja, lv_belnr,
 ls_objb, lv_gjahr, lv_awkey,
 lv_pdf,
 lt_data.

**//
  IF p_entrada-tcode = 'FBV1' OR  p_entrada-tcode = 'FBV3'.
    ls_obja-objkey+0(4) =  p_entrada-bukrs.
    ls_obja-objkey+4(10) = p_entrada-belnr.
    ls_obja-objkey+14(4) = p_entrada-gjahr.
  ELSE.
    CONCATENATE p_entrada-belnr p_entrada-gjahr INTO lv_awkey.
    SELECT SINGLE belnr gjahr INTO ( lv_belnr, lv_gjahr )
      FROM bkpf
      WHERE awkey EQ lv_awkey "p_entrada-awkey
      AND   awtyp EQ 'RMRP'.  "p_entrada-awtyp

    IF sy-subrc EQ 0.
      ls_obja-objkey+0(4)  =  p_entrada-bukrs.
      ls_obja-objkey+4(10) =  lv_belnr.
      ls_obja-objkey+10(4) =  lv_gjahr.
    ENDIF.
  ENDIF.


  CHECK ls_obja-objkey IS NOT INITIAL.

**//.. buscar PDF en WS
  CALL METHOD zcl_dte_monitor=>ver_pdf
    EXPORTING
      xblnr   = p_entrada-xblnr
      bukrs   = p_entrada-bukrs
      tipodte = p_entrada-tipodte
      stcd1   = p_entrada-stcd1
    IMPORTING
      pdf     = lv_pdf.
  IF lv_pdf IS NOT INITIAL.
**//..
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_pdf
      IMPORTING
        output_length = lv_size
      TABLES
        binary_tab    = lt_data.

    IF lines( lt_data ) GT 0.
      CLEAR: lv_fname.
      lv_size = lines( lt_data ) * 255.

      " Crear Nombre del archivo
      CONCATENATE p_entrada-stcd1   p_entrada-rutrecep
                  p_entrada-tipodte p_entrada-xblnr '.PDF'
                  INTO lv_fname.

      swc_container      lv_cont.
      swc_create_object  lv_obj  'MESSAGE'       ''.
      swc_set_element    lv_cont 'NO_DIALOG'     'X'.
      swc_set_element    lv_cont 'DOCUMENTTITLE' lv_fname.
      swc_set_table      lv_cont 'Content_Hex'   lt_data.
      swc_set_element    lv_cont 'DOCUMENTTYPE'  'PDF'. " EXT
      swc_set_element    lv_cont 'DOCUMENTNAME' 'MESSAGE'.
      swc_set_element    lv_cont 'DOCUMENTSIZE'  lv_size.
      swc_refresh_object lv_obj.
      swc_call_method    lv_obj  'CREATE'        lv_cont.
      swc_get_object_key lv_obj  ls_objb-objkey.
*
      ls_objb-objtype = 'MESSAGE'.   "type of attach document
      ls_obja-objtype = 'BKPF'.    "BO of SAP Document.

**//..
      CALL FUNCTION 'BINARY_RELATION_CREATE_COMMIT'
        EXPORTING
          obj_rolea      = ls_obja
          obj_roleb      = ls_objb
          relationtype   = 'ATTA'
        IMPORTING
          binrel         = ls_binrel
        TABLES
          binrel_attrib  = lt_binatt
        EXCEPTIONS
          no_model       = 1
          internal_error = 2
          unknown        = 3
          OTHERS         = 4.
      IF sy-subrc EQ 0.
*        MESSAGE s043(sgos_msg).
      ENDIF.
    ENDIF.
  ENDIF.


ENDFUNCTION.
