class ZCL_DTE_FI0058V2_CL definition
  public
  inheriting from ZCL_DTE_FI0058V2
  final
  create public .

public section.

  methods GENERAR_RECHAZO
    redefinition .
  methods VALIDAOC
    redefinition .
  methods GRABAR
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DTE_FI0058V2_CL IMPLEMENTATION.


  method GENERAR_RECHAZO.
 DATA: e_salida TYPE zst_dte_ack..
    DATA: error TYPE char1.
    DATA: ls_lfa1  TYPE lfa1,
          ls_paval TYPE t001z.
    CLEAR e_salida. CLEAR error. CLEAR ls_lfa1. CLEAR  ls_paval .
    e_salida-bukrs = p_entrada-bukrs.

    e_salida-rutreceptor = p_entrada-stcd1.
    READ TABLE lt_paval INTO ls_paval WITH KEY bukrs = p_entrada-bukrs.
    IF sy-subrc = 0.
      REPLACE ALL OCCURRENCES OF '.' IN ls_paval-paval WITH space.
      CONDENSE ls_paval-paval NO-GAPS.
      e_salida-rutrecibe = ls_paval-paval.
      e_salida-rutreceptor = ls_paval-paval.
    ENDIF.

    DATA:lv_addrnumber TYPE addr1_sel-addrnumber.

    READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = p_entrada-lifnr.
    IF sy-subrc = 0.
      lv_addrnumber = ls_lfa1-adrnr.
    ENDIF.

    TYPE-POOLS: szadr.
    DATA: lv_addr1_complete TYPE szadr_addr1_complete.
    CLEAR lv_addr1_complete.

    CALL FUNCTION 'ADDR_GET_COMPLETE'
      EXPORTING
        addrnumber           = lv_addrnumber
        iv_current_comm_data = 'X'
      IMPORTING
        addr1_complete       = lv_addr1_complete.



    DATA:lt_addr1_tab  TYPE STANDARD TABLE OF szadr_addr1_line,
         lt_adtel_tab  TYPE STANDARD TABLE OF szadr_adtel_line,
         lt_adsmtp_tab TYPE STANDARD TABLE OF szadr_adsmtp_line.

    DATA: ln_addr1_tab  LIKE LINE OF lt_addr1_tab,
          ln_adtel_tab  LIKE LINE OF lt_adtel_tab,
          ln_adsmtp_tab LIKE LINE OF lt_adsmtp_tab.

    lt_addr1_tab[] = lv_addr1_complete-addr1_tab[].
    lt_adtel_tab[] = lv_addr1_complete-adtel_tab[].
    lt_adsmtp_tab[] = lv_addr1_complete-adsmtp_tab[].



    READ TABLE lt_addr1_tab INTO ln_addr1_tab INDEX 1.
    IF sy-subrc = 0.

      e_salida-nmbcontacto = ln_addr1_tab-data-name1.
    ENDIF.

    e_salida-tipodte =  p_entrada-tipodte." '33'.
    e_salida-folio = p_entrada-xblnr.
    CONCATENATE  p_entrada-bldat+0(4) '-' p_entrada-bldat+4(2) '-' p_entrada-bldat+6(2) INTO e_salida-fchemis.

    e_salida-rutemisor = p_entrada-stcd1.



    DATA: lv_monto TYPE string.

    "lv_monto = p_entrada-wrbtr.

    DATA: lv_amount TYPE bapicurr_d.
    CLEAR lv_amount.
    CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
      EXPORTING
        currency        = p_entrada-waers
        amount_internal = p_entrada-wrbtr
      IMPORTING
        amount_external = lv_amount.

    lv_monto = lv_amount.

    e_salida-mnttotal = lv_monto.

    e_salida-codenvio = p_entrada-docnum.
    e_salida-idrespuesta = p_entrada-docnum.
    e_salida-estadodte = '2'.
    e_salida-nrodetalles = '1'.

    IF p_glosa IS NOT INITIAL.
      e_salida-estadodteglosa = p_glosa.
    ELSE.
      e_salida-estadodteglosa = TEXT-gl1.
    ENDIF.
*    "----Envio de Rechazo al SII
*    CALL FUNCTION 'ZFI_0008DTE'
*      EXPORTING
*        acknowledgment = e_salida
*        commit         = 'X'
*      IMPORTING
*        error          = error.
    me->enviar_rechazoacep( EXPORTING  i_salida = e_salida i_commit = 'X' i_entrada = p_entrada
                        IMPORTING e_error = error ).

*   PERFORM generar_log USING p_entrada 'E' p_numero ''.
    me->generar_log( EXPORTING p_entrada = p_entrada   p_msgty = 'E' p_numero = p_numero  p_variable = space ).
*********************************************************
* Al No poder Rechazar, dejamos en error la Factura en
* el Monitor con "N" para que se intente manualmente el
* Rechazo.
*********************************************************
    IF error = 'W'.
      p_entrada-status = 'N'.
      p_entrada-glosa = 'Error WS al rechazar, Ver log'.
    ELSEIF error = '8'.
      p_entrada-status = '8'.
      p_entrada-glosa = 'Error WS al rechazar, Ver log'.
    ELSEIF error = '3'.
      p_entrada-status = 'N'.
      p_entrada-glosa = 'Error WS al rechazar, Ver log'.
    ELSE.
      p_entrada-status = '6'.
      p_entrada-glosa = e_salida-estadodteglosa.

      "---Envío de e-mail rechazo
      "Se mueva al grabar y tener la Glosa en BD
    ENDIF.

  endmethod.


  METHOD grabar.
    DATA: ls_doc        TYPE zst_dte_ack,
          ls_correo     TYPE char100,
          ls_csender    TYPE char100,
          ls_codtextoc  TYPE char20,
          ls_rc         TYPE sy-subrc,
          lv_addrnumber TYPE lfa1-adrnr,
          lt_msg        TYPE bapi_msg. "bapiret2_tab.
    DATA: lv_addr1_complete TYPE szadr_addr1_complete.
    DATA:lt_addr1_tab  TYPE STANDARD TABLE OF szadr_addr1_line,
         lt_adtel_tab  TYPE STANDARD TABLE OF szadr_adtel_line,
         lt_adsmtp_tab TYPE STANDARD TABLE OF szadr_adsmtp_line.

    DATA: ln_addr1_tab  LIKE LINE OF lt_addr1_tab,
          ln_adtel_tab  LIKE LINE OF lt_adtel_tab,
          ln_adsmtp_tab LIKE LINE OF lt_adsmtp_tab.

    CALL METHOD super->grabar.

*---> Envio Correo de rechazo a Proveedor si estatus es 6
    IF wa_recfactprov-status = '6'.

*---> Correo de destinatario-Proveedor
      READ TABLE lt_lfa1 ASSIGNING FIELD-SYMBOL(<lt2>) WITH KEY lifnr =  wa_recfactprov-lifnr.
      IF sy-subrc = 0.
        lv_addrnumber = <lt2>-adrnr.
      ENDIF.
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
          ls_correo =  ln_adsmtp_tab-adsmtp-smtp_addr(100).
        ENDIF.
      ENDIF.
*---> Correo de remitente
      READ TABLE lt_zdte_valida  ASSIGNING FIELD-SYMBOL(<lt1>) WITH KEY bukrs = wa_recfactprov-bukrs.
      IF sy-subrc = 0.
        ls_csender =  <lt1>-correo_texto.
        ls_codtextoc =  <lt1>-texto.
      ENDIF.
      IF ls_codtextoc  IS INITIAL.
        ls_codtextoc = 'Z_ENVIO_PROVREC'.
      ENDIF.
*---> Estructura para KeyFields de Cds
      MOVE-CORRESPONDING wa_recfactprov TO ls_doc.
      ls_doc-folio = wa_recfactprov-xblnr.
*---> Envio de correo
      CALL FUNCTION 'ZFI_0028DTEV1'
        EXPORTING
          i_codtextoc = ls_codtextoc
          i_correo    = ls_correo
          i_doc       = ls_doc
*         i_xml       =
*         i_pdf       =
          i_csender   = ls_csender
        IMPORTING
          e_rc        = ls_rc
          e_msg       = lt_msg.
    ENDIF.

  ENDMETHOD.


  METHOD validaoc.
    CLEAR  lv_bewtp.
    IF lv_valida_prov = 'X'.
      READ TABLE lt_ekpo INTO ls_ekpo WITH KEY ebeln = wa_recfactprov-ebeln.
      IF sy-subrc = 0.
        IF ls_ekpo-pstyp = '9'. "pedido de servicio
          lv_servicio = 'X'.
          lv_bewtp = 'D'.
        ELSEIF ls_ekpo-pstyp = '1'. "Pedido limite
          res = 0.
          lv_error = space.
          lv_trx = 'MIR7'.
          EXIT.
        ELSE.
          lv_bewtp = 'E'.
          lv_servicio = ' '.
        ENDIF.
      ENDIF.
      "CLC ------ validación proveedor
      CLEAR ls_lfb1.
      READ TABLE lt_lfb1 INTO ls_lfb1 WITH KEY lifnr =  wa_recfactprov-lifnr
                                 bukrs =  wa_recfactprov-bukrs.
      IF sy-subrc = 0.
        IF  ls_lfb1-zzdte_tipo = '4'. "Costos indirectos van por la MIR7
          lv_trx = 'MIR7'.
        ENDIF.
      ENDIF.
      CLEAR lv_error.
      IF lv_trx NE 'MIR7'.
        LOOP AT lt_ekpo INTO ls_ekpo WHERE ebeln = wa_recfactprov-ebeln.

          IF lv_servicio = space.

            READ TABLE lt_ekbe INTO ls_ekbe WITH KEY ebeln = ls_ekpo-ebeln
                                             ebelp = ls_ekpo-ebelp
                                             bewtp = 'E'
                                             bwart = '101'.
            IF sy-subrc NE 0.
              READ TABLE lt_ekbe INTO ls_ekbe WITH KEY ebeln = ls_ekpo-ebeln
                                            ebelp = ls_ekpo-ebelp
                                            bewtp = 'E'
                                            bwart = '105'.
              IF sy-subrc NE 0.
                IF ls_ekpo-pstyp NE '1'.
                  "--->error.
                  wa_recfactprov-status = '8'.
                  MESSAGE e016(zdte_0001) INTO wa_log-mensaje.
*                  PERFORM generar_log USING wa_recfactprov 'E' '016' ''.
                  me->generar_log( EXPORTING p_entrada = wa_recfactprov   p_msgty = 'E' p_numero = '016' p_variable = space ).
                  MODIFY  lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.


                  wa_log-estado = icon_red_light.
                  wa_log-documento = wa_recfactprov-xblnr.
                  wa_log-mensaje = wa_log-mensaje.
                  wa_log-sociedad = wa_recfactprov-bukrs.
                  wa_log-proveedor = wa_recfactprov-lifnr.
                  wa_log-fecha = wa_recfactprov-bldat.
                  APPEND  wa_log  TO ti_log.
                  lv_error = 'X'.
                  me->grabar(  ).
                  CONTINUE.
                ENDIF.
              ENDIF.
            ENDIF.
          ELSE.
            READ TABLE lt_ekbe INTO ls_ekbe WITH KEY ebeln = ls_ekpo-ebeln
                                             ebelp = ls_ekpo-ebelp
                                             bewtp = 'D'.
            IF sy-subrc = 0.
              READ TABLE lt_ekbe INTO ls_ekbe WITH KEY ebeln = ls_ekpo-ebeln
                                        ebelp = ls_ekpo-ebelp
                                        bewtp = 'E'
                                        bwart = '101'.
              IF sy-subrc NE 0.
                "--->error
                lv_error = 'X'.
                wa_recfactprov-status = '8'.
                MESSAGE e014(zdte_0001) INTO wa_log-mensaje.
*                PERFORM generar_log USING wa_recfactprov 'E' '014' ''.
                me->generar_log( EXPORTING p_entrada = wa_recfactprov   p_msgty = 'E' p_numero = '014' p_variable = space ).
                MODIFY  lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.

                wa_log-estado = icon_red_light.
                wa_log-documento = wa_recfactprov-xblnr.
                wa_log-mensaje = wa_log-mensaje.
                wa_log-sociedad = wa_recfactprov-bukrs.
                wa_log-proveedor = wa_recfactprov-lifnr.
                wa_log-fecha = wa_recfactprov-bldat.
                APPEND  wa_log  TO ti_log.
                me->grabar(  ).

                CONTINUE.
              ENDIF.
            ELSE.
              "---->error
              lv_error = 'X'.
              wa_recfactprov-status = '8'.
              MESSAGE e015(zdte_0001) INTO wa_log-mensaje.
*             PERFORM generar_log USING wa_recfactprov 'E' '015' ''.
              me->generar_log( EXPORTING p_entrada = wa_recfactprov   p_msgty = 'E' p_numero = '015' p_variable = space ).
              MODIFY  lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.
*              "PERFORM generar_rechazo TABLES lt_t001z  lt_lfa1 USING  ti_log-mensaje '010' CHANGING lt_zcb_recfactprov.
              "me->generar_rechazo( EXPORTING lt_paval = lt_t001z   lt_lfa1 = lt_lfa1  p_glosa = wa_log-mensaje p_numero = '010' CHANGING p_entrada = wa_recfactprov ).

              wa_log-estado = icon_red_light.
              wa_log-documento = wa_recfactprov-xblnr.
              wa_log-mensaje = wa_log-mensaje.
              wa_log-sociedad = wa_recfactprov-bukrs.
              wa_log-proveedor = wa_recfactprov-lifnr.
              wa_log-fecha = wa_recfactprov-bldat.
              APPEND  wa_log  TO ti_log.
              me->grabar(  ).
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
      IF lv_error = 'X'.
*      continue.
        res = 4.
      ENDIF.

    ELSEIF lv_valida_prov = 'C'.
      lv_trx = 'FBV3'.
    ENDIF.
    IF res NE 0.
      me->desbloqueo( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
