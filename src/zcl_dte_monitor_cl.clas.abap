class ZCL_DTE_MONITOR_CL definition
  public
  inheriting from ZCL_DTE_MONITOR
  final
  create public .

public section.

  class-methods DATOS_COR_RECH
    importing
      value(I_REG) type ZEFI_0026 optional
    exporting
      value(E_CORREO) type CHAR100
      value(E_SENDER) type CHAR100
      value(E_TEXTCOR) type CHAR20 .

  methods FUNCIONACRE
    redefinition .
  methods GENERAR_CATALOGOOO
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DTE_MONITOR_CL IMPLEMENTATION.


  METHOD datos_cor_rech.
    DATA: lv_addr1_complete TYPE szadr_addr1_complete.
    DATA:lt_addr1_tab  TYPE STANDARD TABLE OF szadr_addr1_line,
         lt_adtel_tab  TYPE STANDARD TABLE OF szadr_adtel_line,
         lt_adsmtp_tab TYPE STANDARD TABLE OF szadr_adsmtp_line.

    DATA: ln_addr1_tab  LIKE LINE OF lt_addr1_tab,
          ln_adtel_tab  LIKE LINE OF lt_adtel_tab,
          lv_addrnumber TYPE lfa1-adrnr,
          ln_adsmtp_tab LIKE LINE OF lt_adsmtp_tab.

    SELECT SINGLE adrnr INTO lv_addrnumber FROM lfa1 WHERE lifnr EQ i_reg-lifnr.
    CLEAR lv_addr1_complete.

    CALL FUNCTION 'ADDR_GET_COMPLETE'
      EXPORTING
        addrnumber           = lv_addrnumber
        iv_current_comm_data = 'X'
      IMPORTING
        addr1_complete       = lv_addr1_complete.
    lt_addr1_tab[] = lv_addr1_complete-addr1_tab[].
    lt_adtel_tab[] = lv_addr1_complete-adtel_tab[].
    lt_adsmtp_tab[] = lv_addr1_complete-adsmtp_tab[].

    IF lines( lt_adsmtp_tab ) GT 0.
      READ TABLE  lt_adsmtp_tab INTO  ln_adsmtp_tab INDEX 1.
      IF sy-subrc = 0.
        e_correo =  ln_adsmtp_tab-adsmtp-smtp_addr(100).
      ENDIF.
    ENDIF.
*---> Correo de remitente / emailTemplate
    SELECT SINGLE sender, texto INTO ( @DATA(lv_correo), @DATA(lv_texto) ) FROM ztfi_0078
    WHERE bukrs EQ @i_reg-bukrs.
    IF sy-subrc = 0.
      e_sender = lv_correo(100).
      e_textcor = lv_texto.
    ENDIF.

  ENDMETHOD.


  METHOD funcionacre.
    DATA: lv_error_acep        TYPE c,
          wa_salida            TYPE zefi_0026,
          ti_ztfi_0074_bd      TYPE STANDARD TABLE OF ztfi_0074,
          wa_selected_row      LIKE LINE OF lt_selected_rows,
          lv_registro          TYPE i,
          lv_glosa             TYPE string,
          ln_salida_or         TYPE zefi_0026,
          lv_salida_original   TYPE zefi_0026,
          vl_mens              TYPE char100,
          lv_mensaje           TYPE char200,
          lv_documento_mensaje TYPE char200,
          edo_bloqueo          TYPE abap_bool,
**          lv_glosa(100)        TYPE c,
          lv_text              TYPE trm080-text,
          ln_status            TYPE ztfi_0079,
          ls_ztfi_0074_bd      LIKE LINE OF ti_ztfi_0074_bd,
          lv_respuesta         TYPE c,
          lv_texto_popup       TYPE char100,
          lv_sourcetext        TYPE  trm080-text,
          lv_titel             TYPE  trm060-text,
          lt_mod_save          TYPE STANDARD TABLE OF ty_mod,
          ls_mod_save          TYPE ty_mod,
          lv_glosa_rec         TYPE text80,
          ls_doc               TYPE zst_dte_ack,
          ls_rc                TYPE sy-subrc,
          ls_msg               TYPE bapi_msg, "bapiret2_tab,
          lv_glosa1            TYPE char100.
    CLEAR lv_error_acep.


    me->liberarobj( ).

    lv_respuesta = 0.
    IF lt_salida_temp[] IS NOT INITIAL.
      SELECT *
       FROM ztfi_0074
       INTO TABLE ti_ztfi_0074_bd
       FOR ALL ENTRIES IN lt_salida_temp
       WHERE bukrs = lt_salida_temp-bukrs AND
             xblnr = lt_salida_temp-xblnr AND
             lifnr = lt_salida_temp-lifnr AND
             tipodte = lt_salida_temp-tipodte.
      IF sy-dbcnt >= 1.
        SORT ti_ztfi_0074_bd BY bukrs xblnr lifnr tipodte.
        DELETE ADJACENT DUPLICATES FROM ti_ztfi_0074_bd.
      ENDIF.

      IF lv_status = '5'.
        lv_text = TEXT-010.
      ELSE.
        lv_text = TEXT-011.
      ENDIF.

      CONCATENATE lv_text '?' INTO lv_texto_popup SEPARATED BY space.

*Pregunta  Si desea aceptar/rechazar los documentos
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          text_question = lv_texto_popup
          text_button_1 = 'Si'
          text_button_2 = 'No'
        IMPORTING
          answer        = lv_respuesta.

    ENDIF.

    IF lv_respuesta = '1'. " Si es la respuesta es SI
      IF lv_status = '6' AND me->gv_glosa IS INITIAL.
        lv_sourcetext = TEXT-012.
        lv_titel = TEXT-013.
        CALL FUNCTION 'ADA_POPUP_TEXT_INPUT'
          EXPORTING
            sourcetext   = lv_sourcetext     "Ingrese Glosa de Rechazo
            titel        = lv_titel          "Monitor DTE
            start_column = 25
            start_row    = 6
*     IMPORTING
*           ANSWER       =
          CHANGING
            targettext   = lv_glosa_rec.

        me->gv_glosa = lv_glosa_rec.

        IF me->gv_glosa IS INITIAL.
          MESSAGE TEXT-214 TYPE 'I' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

      ENDIF.

      LOOP AT lt_selected_rows INTO wa_selected_row.
        READ TABLE me->datos INTO wa_salida INDEX wa_selected_row-row_id.
        lv_registro = wa_selected_row-row_id.
        CLEAR lv_documento_mensaje. CLEAR edo_bloqueo.
*        clear ti_mod_save.
        CONCATENATE wa_salida-bukrs '/' wa_salida-xblnr INTO lv_documento_mensaje.

        "-----Bloqueo documento.
        edo_bloqueo = me->bloqueo( EXPORTING lt_zcb_recfactprov = wa_salida ).
        IF edo_bloqueo NE 0.

          CLEAR: lv_mensaje,
          ls_mod_save.
          CONCATENATE TEXT-216 lv_documento_mensaje
                      TEXT-222 INTO lv_mensaje SEPARATED BY space.

          ls_mod_save-icono     = icon_red_light.
          ls_mod_save-documento = lv_documento_mensaje.
          ls_mod_save-mensaje   = lv_mensaje.
          APPEND ls_mod_save TO lt_mod_save.

          CONTINUE.
        ENDIF.
        "-----No se pude aceptar/rechazar tipos A
        IF wa_salida-status = 'A'.
          CLEAR: lv_mensaje,
           ls_mod_save.
          CONCATENATE TEXT-216 lv_documento_mensaje
                      TEXT-224 INTO lv_mensaje SEPARATED BY space.

          ls_mod_save-icono     = icon_red_light.
          ls_mod_save-documento = lv_documento_mensaje.
          ls_mod_save-mensaje   = lv_mensaje.
          APPEND ls_mod_save TO lt_mod_save.

          CONTINUE.
        ENDIF.

        "------ se valida registro existente.

        "--->registro en BD actual.
        CLEAR  ls_ztfi_0074_bd .
        READ TABLE ti_ztfi_0074_bd INTO ls_ztfi_0074_bd WITH KEY bukrs = wa_salida-bukrs
                                            xblnr = wa_salida-xblnr
                                            lifnr = wa_salida-lifnr
                                            tipodte = wa_salida-tipodte.
        IF sy-subrc = 0.
          "DATA: ln_salida_or LIKE LINE OF ti_salida_original.
          READ TABLE me->datos_or INTO ln_salida_or WITH KEY bukrs = wa_salida-bukrs
                                                 xblnr = wa_salida-xblnr
                                                 lifnr = wa_salida-lifnr
                                                 tipodte = wa_salida-tipodte.
          IF sy-subrc = 0.
            "DATA: lv_salida_original TYPE ztfi_0074.
            MOVE-CORRESPONDING ln_salida_or TO lv_salida_original.
          ENDIF.

        ENDIF.

        DATA: lv_flag TYPE c.
        lv_flag = 'X'.
        IF ls_ztfi_0074_bd-aprobador   NE  lv_salida_original-aprobador OR
           ls_ztfi_0074_bd-nombana     NE lv_salida_original-nombana OR
           ls_ztfi_0074_bd-texto_libre NE lv_salida_original-texto_libre OR
           ls_ztfi_0074_bd-status      NE lv_salida_original-status OR
           ls_ztfi_0074_bd-zfecharec   NE lv_salida_original-zfecharec OR
           ls_ztfi_0074_bd-zfechapro   NE lv_salida_original-zfechapro OR
           ls_ztfi_0074_bd-glosa       NE lv_salida_original-glosa.
          CLEAR: lv_flag.
          "---->validación sesion.

          "--->Fin
          IF lv_flag NE 'X'.
            CLEAR lv_mensaje.
            CLEAR lv_flag.

            CONCATENATE TEXT-216 lv_documento_mensaje
                        TEXT-217  INTO lv_mensaje SEPARATED BY space.
            ls_mod_save-icono     = icon_red_light.
            ls_mod_save-documento = lv_documento_mensaje.
            ls_mod_save-mensaje   = lv_mensaje.
            APPEND ls_mod_save TO lt_mod_save.
            CONTINUE.
*           MESSAGE lv_mensaje TYPE 'I' DISPLAY LIKE 'E'.
          ENDIF.
        ENDIF.

        IF lv_flag = 'X'.
          "---> Se llama aceptacion/rechazo
          CALL METHOD me->acepta_rechaza_doc
            EXPORTING
              entrada        = wa_salida
              acepta_rechaza = lv_status "'5'
            IMPORTING
              error          = lv_error_acep
              glosa          = lv_glosa1.
          IF lv_error_acep IS INITIAL.

            wa_salida-status = lv_status."'5'.
            IF lv_status = '5'.
              wa_salida-glosa = TEXT-220.
            ELSEIF lv_status = 'N'.
              wa_salida-glosa = TEXT-221.
            ELSE.
              IF me->gv_glosa IS NOT INITIAL.
                wa_salida-glosa = me->gv_glosa.
              ENDIF.

              "---Envio de e-mail rechazo
              me->datos_cor_rech( EXPORTING i_reg = wa_salida
                                 IMPORTING e_correo = DATA(ls_correo) e_sender = DATA(ls_csender) e_textcor = DATA(ls_textcor) ).
              "---> Estructura para KeyFields de Cds
              MOVE-CORRESPONDING wa_salida TO ls_doc.
              ls_doc-folio =  wa_salida-xblnr.
              IF ls_textcor IS INITIAL.
                ls_textcor = 'Z_ENVIO_PROVEREC'.
              ENDIF.

              CALL FUNCTION 'ZFI_0028DTEV1'
                EXPORTING
                  i_codtextoc = ls_textcor
                  i_correo    = ls_correo
                  i_doc       = ls_doc
*                 I_XML       =
*                 I_PDF       =
                  i_csender   = ls_csender
                IMPORTING
                  e_rc        = ls_rc
                  e_msg       = ls_msg.
            ENDIF.
            CLEAR ln_status.
            READ TABLE  me->conf_status INTO ln_status WITH KEY estatus =  wa_salida-status.
            IF sy-subrc = 0.
              wa_salida-icon = ln_status-icono.
            ENDIF.
            MODIFY me->datos FROM wa_salida INDEX lv_registro.

            "--->datos de sesion
**          CLEAR wa_sesion.
**          MOVE-CORRESPONDING wa_salida TO wa_sesion.
**          wa_sesion-usuario = sy-uname.
**          APPEND wa_sesion TO gt_sesion.
**          EXPORT  gt_sesion = gt_sesion TO DATABASE indx(zq) CLIENT sy-mandt ID 'ZTFI0074'.
            "---fin datos de sesion.

            IF lv_status = '5'.
              CLEAR: lv_mensaje,
              ls_mod_save.
              CONCATENATE TEXT-216 lv_documento_mensaje
                          TEXT-202 INTO lv_mensaje SEPARATED BY space.

              ls_mod_save-icono     = icon_green_light.
              ls_mod_save-documento = lv_documento_mensaje.
              ls_mod_save-mensaje   = lv_mensaje.
              APPEND ls_mod_save TO lt_mod_save.
*             MESSAGE text-202 TYPE 'I'.
            ELSE.
              CLEAR: lv_mensaje,
                ls_mod_save.
              CONCATENATE TEXT-216 lv_documento_mensaje
                          TEXT-213 INTO lv_mensaje SEPARATED BY space.

              ls_mod_save-icono     = icon_green_light.
              ls_mod_save-documento = lv_documento_mensaje.
              ls_mod_save-mensaje   = lv_mensaje.
              APPEND ls_mod_save TO lt_mod_save.
*              MESSAGE text-213 TYPE 'I'.
            ENDIF.
          ELSEIF lv_error_acep = 'G'.
            CLEAR: lv_mensaje,
               ls_mod_save.
            CONCATENATE TEXT-216 lv_documento_mensaje
                        TEXT-214 INTO lv_mensaje SEPARATED BY space.

            ls_mod_save-icono     =  icon_red_light.
            ls_mod_save-documento = lv_documento_mensaje.
            ls_mod_save-mensaje   = lv_mensaje.
            APPEND ls_mod_save TO lt_mod_save.
*            MESSAGE text-214 TYPE 'I' DISPLAY LIKE 'E'.
          ELSEIF lv_error_acep = 'W'.
            wa_salida-status = 'N'. "Error en el WS no se acepto/rechazo
            wa_salida-glosa = TEXT-221.
            CLEAR: lv_mensaje,
                 ls_mod_save.
            CONCATENATE TEXT-216 lv_documento_mensaje
                        TEXT-221 INTO lv_mensaje SEPARATED BY space.

            ls_mod_save-icono     =  icon_red_light.
            ls_mod_save-documento = lv_documento_mensaje.
            ls_mod_save-mensaje   = lv_mensaje.
            APPEND ls_mod_save TO lt_mod_save.
*            MESSAGE text-221 TYPE 'I' DISPLAY LIKE 'E'.
            CLEAR ln_status.
            READ TABLE  me->conf_status INTO ln_status WITH KEY estatus =  wa_salida-status.
            IF sy-subrc = 0.
              wa_salida-icon = ln_status-icono.
            ENDIF.
            MODIFY me->datos FROM wa_salida INDEX lv_registro.

          ENDIF.

        ENDIF.
        "-----> Desbloqueo registro
        me->desbloqueo( EXPORTING lt_zcb_recfactprov = wa_salida ).

      ENDLOOP.
      me->liberarobj( ).
      "log de modificaciones o bloqueos realizados.
      IF lt_mod_save[] IS NOT INITIAL.
        me->mostrar_log_proc_masi( EXPORTING  lt_mod_save = lt_mod_save[] ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


method GENERAR_CATALOGOOO.
*CALL METHOD SUPER->GENERAR_CATALOGOOO
**  IMPORTING
**    ti_catalogo =
*    .
 DATA: ls_fcat TYPE lvc_s_fcat.
    DATA: lv_registro LIKE sy-tabix,
          lt_catalogo TYPE lvc_t_fcat,
          ln_catalogo LIKE LINE OF lt_catalogo.
    CLEAR ls_fcat.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = me->gv_structure
      CHANGING
        ct_fieldcat      = lt_catalogo.

    IF lt_catalogo[] IS NOT INITIAL.

      READ TABLE lt_catalogo INTO ln_catalogo INDEX 1.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
        CLEAR: ln_catalogo-scrtext_l,
               ln_catalogo-scrtext_m,
               ln_catalogo-scrtext_s.
*             ln_catalogo-reptext_ddic.
        MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
      ENDIF.

      READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'GLOSA'.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
        ln_catalogo-col_opt ='X'.
*        ln_catalogo-scrtext_m = ln_catalogo-scrtext_l = text-300.

        MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
      ENDIF.

      READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'CORRELATIVO'.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
        ln_catalogo-hotspot = 'X'.
        ln_catalogo-col_opt ='X'.
*        ln_catalogo-scrtext_m = ln_catalogo-scrtext_l = text-302.
        MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
      ENDIF.

      READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'NOMBANA'.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
        ln_catalogo-edit = 'X'.
        ln_catalogo-col_opt ='X'.
        ln_catalogo-ref_table = 'WRF_PPW_US_USGP'.
        ln_catalogo-ref_field = 'BNAME'.
        MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
      ENDIF.

      READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'APROBADOR'.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
        ln_catalogo-edit = 'X'.
        ln_catalogo-col_opt ='X'.
        ln_catalogo-ref_table = 'WRF_PPW_US_USGP'.
        ln_catalogo-ref_field = 'BNAME'.
        ln_catalogo-scrtext_l = ln_catalogo-scrtext_s = ln_catalogo-scrtext_m = 'Aprobador de GTo'.
*        ln_catalogo-reptext_ddic = 'Aprobador de GTo'.
        ln_catalogo-dd_outlen = 15.
        ln_catalogo-outputlen = 15.
        MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
      ENDIF.

      READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'DIAS_TRANS'.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
        ln_catalogo-scrtext_m = ln_catalogo-scrtext_l = TEXT-301.
        MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
      ENDIF.
      READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'ZDOCFI'.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
        ln_catalogo-hotspot = 'X'.
        ln_catalogo-scrtext_s = TEXT-325.
        ln_catalogo-scrtext_m = ln_catalogo-scrtext_l = TEXT-324.
        MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
      ENDIF.


      READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'FECHABASE'.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
        ln_catalogo-scrtext_m = ln_catalogo-scrtext_l = TEXT-323.
        MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
      ENDIF.

      READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'DOCNUM'.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
        ln_catalogo-hotspot = 'X'.
        MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
      ENDIF.

      READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'EBELN'.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
        ln_catalogo-hotspot = 'X'.
        MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
      ENDIF.

      READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'BELNR'.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
        ln_catalogo-hotspot = 'X'.
        MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
      ENDIF.

      READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'CENTRO'.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
*        ln_catalogo-scrtext_l = ln_catalogo-scrtext_s = ln_catalogo-scrtext_m = text-320.
        MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
      ENDIF.

      READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'HES'.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
*       CLEAR ln_catalogo-reptext_ddic.
        ln_catalogo-scrtext_l = ln_catalogo-scrtext_s = ln_catalogo-scrtext_m = TEXT-321.
        MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
      ENDIF.

      READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'KONNR'.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
*       CLEAR ln_catalogo-reptext_ddic.
*        ln_catalogo-scrtext_l = ln_catalogo-scrtext_s = ln_catalogo-scrtext_m = text-322.
        MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
      ENDIF.

      "----->inserción de campos tipo dte.
      READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'ZDTE_TIPO'.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
*        CLEAR ln_catalogo-reptext_ddic.
*        ln_catalogo-scrtext_l = ln_catalogo-scrtext_s = ln_catalogo-scrtext_m  = text-310.
        MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
      ENDIF.
      READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'DESC_TIPO'.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
*        CLEAR ln_catalogo-reptext_ddic.
        ln_catalogo-scrtext_l = ln_catalogo-scrtext_s = ln_catalogo-scrtext_m  = TEXT-311.
        MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
      ENDIF.

      "---->actualizacipon números de columnas.
      DATA: lv_colpos LIKE ln_catalogo-col_pos.
      lv_colpos = 1.
      LOOP AT lt_catalogo INTO ln_catalogo.
        lv_registro = sy-tabix.
        ln_catalogo-col_pos = lv_colpos.
        MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
        ADD 1 TO lv_colpos.
      ENDLOOP.


      DELETE lt_catalogo WHERE fieldname = 'SEL'.

      DATA: r_campos  TYPE RANGE OF slis_fieldname,
            ln_campos LIKE LINE OF r_campos.

*// Para Excluir campos de catalogo por pais
      SELECT SINGLE nombre_int INTO @DATA(ls_catext)   FROM ztfi_0078b
       WHERE vatint EQ  @gs_vatint
       AND codint EQ '0011'.
      IF sy-subrc = 0.
        SELECT 'I' AS sign,  'EQ' AS option,  low
        INTO CORRESPONDING FIELDS OF TABLE @r_campos FROM tvarvc
        WHERE name = @ls_catext
        ORDER BY PRIMARY KEY.
      ENDIF.
*// Para Excluir campos de catalogo por pais

      IF r_campos IS INITIAL. " Si no se ha cargado el stvarvc con los campos, Se excluyen estos por defecto

        CLEAR ln_campos.
        ln_campos-sign = 'I'.
        ln_campos-option = 'EQ'.
        ln_campos-low = 'FCHCANCEL'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'MNTCANCEL'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'TERMPAGOCDG'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'TERMPAGOGLOSA'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'TERMPAGODIAS'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'FCHVENC'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'RUTEMISOR'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'RZNSOC'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'GIROEMIS'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'CORREOEMISOR'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'DIRORIGEN'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'CMNAORIGEN'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'CIUDADORIGEN'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'RUTMANDANTE'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'RUTRECEP'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'RZNSOCRECEP'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'TOTITEMS'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'TOTBULTOS'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'MNTNETO'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'MNTEXE'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'TASAIVA'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'IVANORET'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'IVAPROP'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'IVATERC'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'TIVANORET'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'MNTTOTAL'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'VLRPAGAR'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'TPOCAMBIO'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'MNTNETOOTRMNDA'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'MNTEXEOTRMNDA'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'IVAOTRMNDA'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'IVANORETOTRMNDA'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'MNTTOTOTRMNDA'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'FCHPAGO'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'MNTPAGO'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'PERIODODESDE'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'TIPOIMP'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'TASAIMP'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'MONTOIMP'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'TIPOIMPOTRMNDA'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'TASAIMPOTRMNDA'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'VALORIMPOTRMNDA'.
        APPEND ln_campos TO r_campos.


        ln_campos-low = 'FILENAME'.
        APPEND ln_campos TO r_campos.

        ln_campos-low = 'DIASING'.
        APPEND ln_campos TO r_campos.
      ENDIF.

      DELETE lt_catalogo WHERE fieldname IN r_campos.

      AUTHORITY-CHECK OBJECT 'ZFI01'
          ID 'BUKRS' FIELD me->bukrs
          ID 'ACTVT' FIELD '02'.
      IF sy-subrc = 0.
        READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'NOMBANA'.
        IF sy-subrc = 0.
          lv_registro = sy-tabix.
          ln_catalogo-edit = 'X'.
          MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
        ENDIF.
        READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'APROBADOR'.
        IF sy-subrc = 0.
          lv_registro = sy-tabix.
          ln_catalogo-edit = 'X'.
          MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
        ENDIF.
        READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'TEXTO_LIBRE'.
        IF sy-subrc = 0.
          lv_registro = sy-tabix.
          ln_catalogo-edit = 'X'.
          MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
        ENDIF.
        READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'ZFECHAREC'.
        IF sy-subrc = 0.
          lv_registro = sy-tabix.
          ln_catalogo-edit = 'X'.
          MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
        ENDIF.
        READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'ZFECHAPRO'.
        IF sy-subrc = 0.
          lv_registro = sy-tabix.
          ln_catalogo-edit = 'X'.
          MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
        ENDIF.
*        lv_mod = 'X'.
      ELSE.
        AUTHORITY-CHECK OBJECT 'ZFI01'
              ID 'BUKRS' FIELD me->bukrs
              ID 'ACTVT' FIELD '03'.
        IF sy-subrc = 0.
          READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'NOMBANA'.
          IF sy-subrc = 0.
            lv_registro = sy-tabix.
            ln_catalogo-edit = space.
            MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
          ENDIF.
          READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'APROBADOR'.
          IF sy-subrc = 0.
            lv_registro = sy-tabix.
            ln_catalogo-edit = space.
            MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
          ENDIF.

          READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'TEXTO_LIBRE'.
          IF sy-subrc = 0.
            lv_registro = sy-tabix.
            ln_catalogo-edit = space.
            MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
          ENDIF.

          READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'ZFECHAREC'.
          IF sy-subrc = 0.
            lv_registro = sy-tabix.
            ln_catalogo-edit = space.
            MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
          ENDIF.

          READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'ZFECHAPRO'.
          IF sy-subrc = 0.
            lv_registro = sy-tabix.
            ln_catalogo-edit = space.
            MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
          ENDIF.
*    ELSE.
*     MESSAGE 'Usuario sin autorización para acceder a la transacción' TYPE 'E'.  jlw Novis
*      MESSAGE TEXT-400 TYPE 'E'.
        ENDIF.
      ENDIF.

      ti_catalogo[] = lt_catalogo[].
      "---->fin validación de autorización.
    ENDIF.
  endmethod.
ENDCLASS.
