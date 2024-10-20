class ZCL_DTE_FI0057V2_CL definition
  public
  inheriting from ZCL_DTE_FI0057V2
  final
  create public .

public section.

  class-methods GET_FECHA_A_EM
    returning
      value(FECHA) type DATUM .
  class-methods SET_FECHA_A_EM
    importing
      !FECHA type DATUM .

  methods GENERAR_FBV1_BAPI
    redefinition .
  methods GENERAR_MIR7
    redefinition .
  methods GENERAR_MIR7_COMEX
    redefinition .
  methods GENERAR_MIRA
    redefinition .
  methods GENERAR_RECHAZO
    redefinition .
  methods GRABAR
    redefinition .
  methods SUSTITUCION
    redefinition .
  methods VALIDACION
    redefinition .
  methods LLAMABAPIS
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DTE_FI0057V2_CL IMPLEMENTATION.


  METHOD generar_fbv1_bapi.
    DATA: ls_documentheader TYPE bapiache09,
          ls_customercpd    TYPE bapiacpa09,
          lt_accountgl      TYPE STANDARD TABLE OF bapiacgl09,
          ls_accountgl      TYPE bapiacgl09,
          lt_accountpayable TYPE STANDARD TABLE OF bapiacap09,
          ls_accountpayable TYPE bapiacap09,
          lt_accounttax     TYPE STANDARD TABLE OF bapiactx09,
          ls_accounttax     TYPE bapiactx09,
          lt_currencyamount TYPE STANDARD TABLE OF bapiaccr09,
          ls_currencyamount TYPE bapiaccr09,
          lt_return         TYPE STANDARD TABLE OF bapiret2,
          lt_extension2     TYPE STANDARD TABLE OF bapiparex,
          ls_extension2     TYPE bapiparex,
          lt_accountwt      TYPE STANDARD TABLE OF bapiacwt09, "BoletasH
          ls_accountwt      TYPE  bapiacwt09, "BoletasH
          lt_lfbw           TYPE STANDARD TABLE OF lfbw,  "BoletasH
          ls_lfbw           TYPE  lfbw,  "BoletasH
          lt_extension1     TYPE STANDARD TABLE OF bapiacextc,
          ls_extension1     TYPE bapiacextc,
          ls_cladte         TYPE ztfi_0001b,
          lv_blart          TYPE bkpf-blart,
          ls_lfb1           TYPE lfb1,
          lv_fdocto         TYPE string,
          lv_fecont         TYPE string,
          lv_itemacc        TYPE posnr_acc,
          ls_shkzg          TYPE bseg-shkzg,
          lv_obj_key        TYPE bapiache09-obj_key,
          lv_belnr          TYPE bseg-belnr,
          lv_bukrs          TYPE bukrs,
          lv_gjahr          TYPE gjahr,
          lv_cdsii          TYPE ztfi_0093-cdsii VALUE '15',
          lv_fact_s         TYPE p DECIMALS 2 VALUE 1,
          lv_fact_k         TYPE p DECIMALS 2 VALUE 1,
*         s_xcpd            TYPE lfa1-xcpdk,
*        s_ciudad          LIKE bsec-ort01,
*        s_direccion       LIKE bsec-stras,
*        s_name1           LIKE bsec-name1,
          lv_amount         TYPE bapicurr_d.
    DATA: ls_ekpo   TYPE ekpo,
          ls_cldoc  TYPE ztfi_0001b,
          ls_0093   TYPE ztfi_0093,
          ls_0092   TYPE ztfi_0092,
          ls_return TYPE bapiret2.

    CLEAR: ls_ekpo, ls_cldoc, ls_0093,ls_0092,ls_return.

    REFRESH lt_accountpayable. REFRESH lt_accountgl. REFRESH lt_accounttax.
    CLEAR  ls_documentheader. CLEAR lv_itemacc.

*<<<<<<<<< Datos de cabecera >>>>>>>>.

    "--> homologación tipo dte <=> clase doc.
    ls_cladte-bukrs   = c_recfactprov-bukrs.
    ls_cladte-tipodte = c_recfactprov-tipodte.

    READ TABLE lt_zdte_cldoc INTO ls_cldoc  WITH KEY bukrs = ls_cladte-bukrs
                                       tipop = s_tipodte
                                       tipodte = ls_cladte-tipodte
                                       bsart = space
                                       knttp = '*'.

    IF sy-subrc = 0.
      lv_blart = ls_cldoc-blart.
    ELSE.
      CLEAR wa_log.
      wa_log-estado    = icon_red_light.
      wa_log-documento = c_recfactprov-xblnr.
      wa_log-sociedad  = c_recfactprov-bukrs.
      wa_log-proveedor = c_recfactprov-lifnr.
      wa_log-fecha     = c_recfactprov-bldat.
      MESSAGE ID 'ZDTE_0001' TYPE 'E' NUMBER '035' INTO wa_log-mensaje.
      CONCATENATE TEXT-410 wa_log-mensaje INTO wa_log-mensaje
                                          SEPARATED BY space.
      APPEND wa_log TO ti_log.

      c_recfactprov-status = '7'.
      me->generar_log( EXPORTING p_entrada = c_recfactprov
                xmsgty = 'E'  xmsgid = 'ZDTE_0001' xmsgno = '035' xmsgv1 = space
                xmsgv2 = space xmsgv3 = space xmsgv4 = space ).

*      PERFORM generar_log USING  c_recfactprov        'E'
*                                 'ZDTE_0001'        '035'
*                                 space        space
*                                 space       space.
      EXIT.
    ENDIF.
    "--> Proveedor
    READ TABLE t_lfb1 WITH KEY bukrs = c_recfactprov-bukrs
                               lifnr = c_recfactprov-lifnr INTO ls_lfb1.
    IF sy-subrc NE 0.
      " Error??
    ENDIF.

    "--> Fecha Documento
    lv_fdocto = c_recfactprov-bldat.

    "--> Fecha Contabilización
    lv_fecont = sy-datum.

    "---- Determ. Clave de contabilización
    CASE c_recfactprov-tipodte.
      WHEN '33' OR '34' OR '56' OR '110' OR '111' OR '43' OR '46' OR '66'. "BoletasH
        ls_shkzg = 'S'.
        lv_fact_k = - 1.
        lv_fact_s = 1.

      WHEN OTHERS.
        ls_shkzg = 'H'.
        lv_fact_s = - 1.
        lv_fact_k = 1.
    ENDCASE.


    "--> Asignación de datos de bapi.
    ls_documentheader-comp_code =  c_recfactprov-bukrs. "Sociedad
    ls_documentheader-doc_date  =  lv_fdocto.           "fecha de documento
    ls_documentheader-pstng_date = lv_fecont.           "fecha de contabilización
    ls_documentheader-trans_date = lv_fdocto.           "fecha de conversión para mon.Fuerte.
    ls_documentheader-doc_type =   lv_blart.            "Clase de documento.
    ls_documentheader-ref_doc_no =  c_recfactprov-xblnr."Nro. de Folio
*    ls_documentheader-header_txt = 'Doc.Fact.Elect'.    "texto de cabecera
    ls_documentheader-header_txt =  c_recfactprov-xblnr .
    CONDENSE ls_documentheader-header_txt NO-GAPS.
    ls_documentheader-username = sy-uname.              "Nombre de usuario.
    ls_documentheader-bus_act = 'RFBU'.                 "Precontabilizar
*OT CLC PARKING
*ls_documentheader-bus_act = 'RFBV'.
*  ls_documentheader-doc_status = '2'.                 "Estatus para precontabilizar
*OT CLC PARKING

*<<<<<<<< Datos de CPD >>>>>>>>>>>>>
*  CLEAR s_ciudad. CLEAR s_direccion. CLEAR s_name1. CLEAR s_xcpd.
*  CLEAR ls_customercpd.
*  SELECT SINGLE xcpdk INTO s_xcpd FROM lfa1
*     WHERE lifnr EQ  c_recfactprov-lifnr .

*  IF  s_xcpd = 'X'.
*    IF c_recfactprov-rznsoc IS INITIAL.
*      s_name1 = c_recfactprov-name1.
*    ELSE.
*      s_name1 = c_recfactprov-rznsoc(35).
*    ENDIF.
*    IF c_recfactprov-dirorigen  IS INITIAL.
*      s_direccion = 'SANTIAGO'.
*    ELSE.
*      s_direccion = c_recfactprov-dirorigen(35).
*    ENDIF.
*
*    IF c_recfactprov-ciudadorigen  IS INITIAL.
*      s_ciudad = 'SANTIAGO'.
*    ELSE.
*      s_ciudad = c_recfactprov-ciudadorigen(20).
*    ENDIF.
*
*    ls_customercpd-name =  s_name1.
*    ls_customercpd-city =  s_ciudad.
*    ls_customercpd-country = 'CL'.
*    ls_customercpd-langu_iso = 'ES'.
*    ls_customercpd-street = s_direccion.
*    ls_customercpd-tax_no_1 = c_recfactprov-stcd1.
*  ENDIF.


*<<<<<<<<< Cuenta por Pagar >>>>>>>>.
    ADD  1 TO lv_itemacc.
    ls_accountpayable-itemno_acc = lv_itemacc."'0000000001'.
    ls_accountpayable-vendor_no = c_recfactprov-lifnr.
    ls_accountpayable-bline_date = c_recfactprov-fechabase.
    ls_accountpayable-pmnttrms = ls_lfb1-zterm.

*MEDS Texto asignacion / posicion
    ls_accountpayable-alloc_nmbr = c_recfactprov-xblnr.
    CONDENSE ls_accountpayable-alloc_nmbr NO-GAPS.

    CONCATENATE  c_recfactprov-xblnr '-' c_recfactprov-name1 INTO ls_accountpayable-item_text.
    CONDENSE ls_accountpayable-item_text NO-GAPS.

    IF  c_recfactprov-tipodte = '66'. "BoletasHN
      ls_accountpayable-item_text = |Honorios Dr. {  c_recfactprov-name1 }|.
    ENDIF.




    IF c_recfactprov-iva GT 0.
      READ TABLE t_ztfi_0093 INTO ls_0093 WITH KEY cdsii = lv_cdsii
                                                   bukrs = c_recfactprov-bukrs.
      IF sy-subrc NE 0.
        CLEAR wa_log.
        wa_log-estado    = icon_red_light.
        wa_log-documento = c_recfactprov-xblnr.
        wa_log-sociedad  = c_recfactprov-bukrs.
        wa_log-proveedor = c_recfactprov-lifnr.
        wa_log-fecha     = c_recfactprov-bldat.
*----> 'Error'
        MESSAGE ID 'ZDTE_0001' TYPE 'E' NUMBER '034' WITH lv_cdsii
                                                    INTO wa_log-mensaje.
        CONCATENATE TEXT-410 wa_log-mensaje INTO wa_log-mensaje
                                            SEPARATED BY space.
        APPEND wa_log TO ti_log.

        c_recfactprov-status = '7'.

*        PERFORM generar_log USING  c_recfactprov        'E'
*                                   'ZDTE_0001'           '034'
*                                   lv_cdsii             space
*                                   space                space.
        msgv1 = lv_cdsii.
        me->generar_log( EXPORTING p_entrada = c_recfactprov
                   xmsgty = 'E'  xmsgid = 'ZDTE_0001' xmsgno = '034' xmsgv1 =  msgv1
                   xmsgv2 = space xmsgv3 = space xmsgv4 = space ).

        EXIT.

      ENDIF.
      ls_accountpayable-tax_code = ls_0093-mwskz_cpr. "'C1'. .
    ELSE.
      ls_accountpayable-tax_code = 'C0'.
    ENDIF.

    APPEND ls_accountpayable TO lt_accountpayable. CLEAR: ls_accountpayable.

    CLEAR ls_currencyamount.
    ls_currencyamount-itemno_acc = lv_itemacc."'0000000001'.
    ls_currencyamount-currency =   c_recfactprov-waers.    "Moneda

    CLEAR lv_amount.
    me->currency_conv( EXPORTING  p_waers = c_recfactprov-waers
                                  p_mntto =  c_recfactprov-mnttotal
                          CHANGING p_lv_amount = lv_amount ).  "Monto Factura
    ls_currencyamount-amt_doccur = lv_amount * lv_fact_k . "Monto Factura

    IF c_recfactprov-iva GT 0.
      CLEAR lv_amount.
      me->currency_conv( EXPORTING   p_waers = c_recfactprov-waers
                                     p_mntto = c_recfactprov-iva
                         CHANGING    p_lv_amount = lv_amount ).   "Monto IVA

*      PERFORM currency_conv USING    c_recfactprov-waers
*                                     c_recfactprov-iva
*                            CHANGING lv_amount.
      ls_currencyamount-tax_amt =   lv_amount * lv_fact_k.   "Monto IVA
    ENDIF.

    APPEND ls_currencyamount TO lt_currencyamount.

*<<<<<<<< Retenciones >>>>>>>>>>
    REFRESH lt_accountwt.
    CLEAR ls_accountwt.
    SELECT * INTO TABLE lt_lfbw FROM lfbw
    WHERE bukrs EQ c_recfactprov-bukrs
    AND  lifnr EQ c_recfactprov-lifnr.
    LOOP AT lt_lfbw INTO ls_lfbw.
      CLEAR ls_accountwt.
      IF ls_lfbw-wt_subjct EQ 'X'. "sujeto a retencion
        ls_accountwt-itemno_acc = lv_itemacc.
        ls_accountwt-wt_type =  ls_lfbw-witht.
        ls_accountwt-wt_code = ls_lfbw-wt_withcd.
        APPEND ls_accountwt TO lt_accountwt.
      ENDIF.
    ENDLOOP.

*<<<<<<<<< IVA >>>>>>>>.

    IF c_recfactprov-iva GT 0.
      ADD  1 TO lv_itemacc.
      CLEAR ls_accounttax.
      ls_accounttax-itemno_acc = lv_itemacc. "'0000000002'.
      ls_accounttax-acct_key = 'VST'.
*      ls_accounttax-direct_tax = 'X'. "Solo con Imputacion Directa Impuesto
      ls_accounttax-tax_code =  ls_0093-mwskz_cpr. "'C1'.

      IF ls_0093-mwskz_cpr NE 'C2'.
        APPEND ls_accounttax TO lt_accounttax.
      ENDIF.
      CLEAR ls_currencyamount.
      ls_currencyamount-itemno_acc = lv_itemacc."'0000000002'.
      ls_currencyamount-currency =   c_recfactprov-waers. "Moneda
*
      CLEAR lv_amount.
*      PERFORM currency_conv USING    c_recfactprov-waers
*                                     c_recfactprov-iva
*                            CHANGING lv_amount.  "Monto IVA
      me->currency_conv( EXPORTING  p_waers = c_recfactprov-waers
                                    p_mntto = c_recfactprov-iva
                                 CHANGING p_lv_amount = lv_amount ).   "Monto IVA


      ls_currencyamount-amt_doccur = lv_amount * lv_fact_s.   "Monto IVA
*
      CLEAR lv_amount.
*      PERFORM currency_conv USING     c_recfactprov-waers
*                                     c_recfactprov-mntneto
*                            CHANGING  lv_amount.  "BaseImpuesto
      me->currency_conv( EXPORTING  p_waers = c_recfactprov-waers
                                    p_mntto = c_recfactprov-mntneto
                             CHANGING p_lv_amount = lv_amount ).   "BaseImpuesto

      ls_currencyamount-amt_base = lv_amount * lv_fact_s. "BaseImpuesto

      APPEND ls_currencyamount TO lt_currencyamount.

    ENDIF.
*---> Solo exento o  Exento y afecto al mismo tiempo
    IF  c_recfactprov-iva EQ 0 OR ( c_recfactprov-mntneto NE 0  AND  c_recfactprov-mntexe NE 0 ).
      ADD  1 TO lv_itemacc.
      CLEAR ls_accounttax.
      ls_accounttax-itemno_acc = lv_itemacc. "'0000000002'.
      ls_accounttax-acct_key = 'VST'.
      ls_accounttax-tax_code =  'C0'.
*    ls_accounttax-direct_tax = 'X'. "Solo con Imputacion Directa Impuesto
      APPEND ls_accounttax TO lt_accounttax.

      CLEAR ls_currencyamount.
      ls_currencyamount-itemno_acc = lv_itemacc."'0000000002'.
      ls_currencyamount-currency =   c_recfactprov-waers. "Moneda
      ls_currencyamount-amt_doccur = 0.   "Monto IVA

      CLEAR lv_amount.
*      PERFORM currency_conv USING    c_recfactprov-waers
*                                     c_recfactprov-mntexe
*                            CHANGING lv_amount.  "BaseImpuesto
      me->currency_conv( EXPORTING  p_waers = c_recfactprov-waers
                                    p_mntto =  c_recfactprov-mntexe
                                   CHANGING p_lv_amount = lv_amount ).

      ls_currencyamount-amt_base = lv_amount * lv_fact_s. "BaseImpuesto

      APPEND ls_currencyamount TO lt_currencyamount.
    ENDIF.

*<<<<<<<<< CUENTAS DE MAYOR >>>>>>>>.
    IF  u_tipodte = '5'. "Si es tipo determ.Cta.Automatico
    ls_documentheader-doc_status = '2'."'3'.  "Estatus para FUllParking


      READ TABLE t_ztfi_0092 INTO ls_0092 WITH KEY bukrs = c_recfactprov-bukrs
                                      dtcta = ls_lfb1-zzdtcta
                                      shkzg = ls_shkzg.
      IF sy-subrc NE 0.
*----> 'Error'
        CLEAR wa_log.
        wa_log-estado    = icon_red_light.
        wa_log-documento = c_recfactprov-xblnr.
        wa_log-sociedad  = c_recfactprov-bukrs.
        wa_log-proveedor = c_recfactprov-lifnr.
        wa_log-fecha     = c_recfactprov-bldat.

        MESSAGE ID 'ZDTE_0001' TYPE 'E' NUMBER '036' WITH ls_lfb1-zzdtcta
                                                         ls_shkzg
                                                    INTO wa_log-mensaje.
        CONCATENATE TEXT-410 wa_log-mensaje INTO wa_log-mensaje
                                            SEPARATED BY space.
        APPEND wa_log TO ti_log.

        c_recfactprov-status = '7'.

*        PERFORM generar_log USING  c_recfactprov        'E'
*                                   'ZDTE_0001'           '036'
*                                   ls_lfb1-zzdtcta      ls_shkzg
*                                   space                space.
        msgv1 = ls_lfb1-zzdtcta.
        msgv2 = ls_shkzg.
        me->generar_log( EXPORTING p_entrada = c_recfactprov
                          xmsgty = 'E'  xmsgid = 'ZDTE_0001' xmsgno = '036' xmsgv1 = msgv1
                          xmsgv2 =  msgv2 xmsgv3 = space xmsgv4 = space ).

        EXIT.
      ENDIF.

      IF ls_0092-hkont NE space.
        ADD  1 TO lv_itemacc.

        CLEAR ls_accountgl .
        ls_accountgl-itemno_acc = lv_itemacc."'0000000003'.
        ls_accountgl-gl_account =  ls_0092-hkont.
        ls_accountgl-costcenter = ls_0092-kostl.

        IF c_recfactprov-iva GT 0.
          ls_accountgl-tax_code = ls_0093-mwskz_cpr. "'C1'. .
        ELSE.
          ls_accountgl-tax_code = 'C0'.
        ENDIF.

*MEDS Texto de asignacion / texto de posicion
        ls_accountgl-alloc_nmbr = c_recfactprov-xblnr .
        CONDENSE ls_accountgl-alloc_nmbr NO-GAPS.

        CONCATENATE c_recfactprov-xblnr '-' c_recfactprov-name1 INTO ls_accountgl-item_text.
        CONDENSE  ls_accountgl-item_text NO-GAPS.
        APPEND ls_accountgl TO lt_accountgl.

        CLEAR ls_currencyamount.
        ls_currencyamount-itemno_acc = lv_itemacc."'0000000003'.
        ls_currencyamount-currency =   c_recfactprov-waers. "Moneda

*
        IF c_recfactprov-iva GT 0.
          CLEAR lv_amount.
*        PERFORM currency_conv USING c_recfactprov-waers  c_recfactprov-mntneto CHANGING lv_amount.  "BaseImpuesto
          me->currency_conv( EXPORTING  p_waers = c_recfactprov-waers
                                  p_mntto =  c_recfactprov-mntneto
                                 CHANGING p_lv_amount = lv_amount ).
          ls_currencyamount-amt_doccur = lv_amount * lv_fact_s. "BaseImpuesto
        ELSE.
          CLEAR lv_amount.
*          PERFORM currency_conv USING c_recfactprov-waers  c_recfactprov-mntexe CHANGING lv_amount.  "BaseImpuesto
          me->currency_conv( EXPORTING  p_waers = c_recfactprov-waers
                                              p_mntto =  c_recfactprov-mntexe
                                             CHANGING p_lv_amount = lv_amount ).
          ls_currencyamount-amt_doccur = lv_amount * lv_fact_s. "BaseImpuesto
        ENDIF.

        APPEND ls_currencyamount TO lt_currencyamount.
        IF c_recfactprov-iva GT 0 AND c_recfactprov-mntexe NE 0. "Para facturas afectas y exentas.
          ADD  1 TO lv_itemacc.

          CLEAR ls_accountgl .
          ls_accountgl-itemno_acc = lv_itemacc."'0000000003'.
          ls_accountgl-gl_account =  ls_0092-hkont.
          ls_accountgl-costcenter = ls_0092-kostl.
          ls_accountgl-tax_code = 'C0'.
          CONCATENATE c_recfactprov-tipodte '-' c_recfactprov-xblnr INTO ls_accountgl-alloc_nmbr.
          CONDENSE ls_accountgl-alloc_nmbr NO-GAPS.

          CONCATENATE c_recfactprov-tipodte '-' c_recfactprov-xblnr '-' c_recfactprov-name1 INTO ls_accountgl-item_text.
          CONDENSE  ls_accountgl-item_text NO-GAPS.
          APPEND ls_accountgl TO lt_accountgl.
          CLEAR ls_currencyamount.
          ls_currencyamount-itemno_acc = lv_itemacc."'0000000003'.
          ls_currencyamount-currency =   c_recfactprov-waers. "Moneda
*
          CLEAR lv_amount.
*          PERFORM currency_conv USING c_recfactprov-waers  c_recfactprov-mntexe CHANGING lv_amount.  "BaseImpuesto
          me->currency_conv( EXPORTING  p_waers = c_recfactprov-waers
                                                     p_mntto =  c_recfactprov-mntexe
                                                    CHANGING p_lv_amount = lv_amount ).
          ls_currencyamount-amt_doccur = lv_amount * lv_fact_s. "BaseImpuesto

          APPEND ls_currencyamount TO lt_currencyamount.
        ENDIF.

      ENDIF.
    ENDIF.
*<<<<<<<<< Otros Impuestos >>>>>>>>.

*<<<<<<<<< Extension de BAPI usando BADI AC_DOCUMENT >>>>>>>>
*  ls_extension2-structure = 'ZDTE_EXTENSION2'.
*  ls_extension2-valuepart1 = 'FV60'.
*  APPEND ls_extension2 TO lt_extension2.

*<<<<<<<<< Extension de BAPI/ Usando BTE >>>>>>>>>
    CLEAR ls_extension1. REFRESH lt_extension1.
    ls_extension1-field1 = 'DTE_PRECONT'.
    ls_extension1-field2 = '2'.
    APPEND ls_extension1 TO lt_extension1.

    "---> LLamar Bapi
    CLEAR lv_obj_key.
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader = ls_documentheader
*       CUSTOMERCPD    =
*       CONTRACTHEADER =
      IMPORTING
*       OBJ_TYPE       =
        obj_key        = lv_obj_key
*       OBJ_SYS        =
      TABLES
        accountgl      = lt_accountgl
*       ACCOUNTRECEIVABLE       =
        accountpayable = lt_accountpayable
        accounttax     = lt_accounttax
        currencyamount = lt_currencyamount
        extension1     = lt_extension1
*       CRITERIA       =
*       VALUEFIELD     =
*       EXTENSION1     =
        return         = lt_return
*       PAYMENTCARD    =
*       CONTRACTITEM   =
*       extension2     = lt_extension2
*       REALESTATE     =
        accountwt      = lt_accountwt
      EXCEPTIONS
        error_message  = 1.

    READ TABLE lt_return  INTO ls_return WITH KEY type = 'E'.
    IF sy-subrc = 0.
      LOOP AT lt_return INTO ls_return.
        IF ls_return-type = 'E'.
          CLEAR wa_log.
          wa_log-estado    = icon_red_light.
          wa_log-documento = c_recfactprov-xblnr.
          wa_log-sociedad  = c_recfactprov-bukrs.
          wa_log-proveedor = c_recfactprov-lifnr.
          wa_log-fecha     = c_recfactprov-bldat.
*----> 'Error al crear doc. trx FBV1,
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO wa_log-mensaje.

          CONCATENATE TEXT-410 ls_return-message INTO wa_log-mensaje
                                                   SEPARATED BY space.
          APPEND wa_log TO ti_log.

          c_recfactprov-status = '7'.
          me->generar_log( EXPORTING p_entrada = c_recfactprov
                    xmsgty = ls_return-type  xmsgid = ls_return-id xmsgno = ls_return-number xmsgv1 = ls_return-message_v1
                    xmsgv2 = ls_return-message_v2 xmsgv3 = ls_return-message_v3 xmsgv4 = ls_return-message_v4 ).
        ENDIF.
      ENDLOOP.
    ELSE.
      IF  lv_obj_key NE '$' AND lv_obj_key NE space.
        CLEAR lv_belnr. CLEAR lv_bukrs. CLEAR lv_gjahr.
        MOVE: lv_obj_key(10) TO lv_belnr,
              lv_obj_key+10(4) TO lv_bukrs,
              lv_obj_key+14(4) TO lv_gjahr.
**//.. Commit
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

**//.. Llenar Log
        CLEAR wa_log.
        wa_log-estado    = icon_green_light.
        wa_log-documento = c_recfactprov-xblnr.
        wa_log-sociedad  = c_recfactprov-bukrs.
        wa_log-proveedor = c_recfactprov-lifnr.
        wa_log-fecha     = c_recfactprov-bldat.
*---> 'Documento creado : '
        CONCATENATE TEXT-409 lv_belnr '/' lv_gjahr INTO wa_log-mensaje.
        APPEND wa_log TO ti_log.
        c_recfactprov-status = u_estatus.
        c_recfactprov-belnr  = lv_belnr.
        c_recfactprov-gjahr  = lv_gjahr.
        c_recfactprov-tcode  = 'FBV3'.
*      c_recfactprov-awkey =  me->arma_awkey( EXPORTING c_recfactprov = c_recfactprov ).
*      c_recfactprov-awtyp =  'BKPFF'.
        me->generar_log( EXPORTING p_entrada = c_recfactprov
                                      xmsgty = sy-msgty  xmsgid =  sy-msgid xmsgno = sy-msgno xmsgv1 = sy-msgv1
                                      xmsgv2 = sy-msgv2 xmsgv3 =  sy-msgv3  xmsgv4 = sy-msgv4 ).
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD generar_mir7.
    DATA: ls_headerdata TYPE bapi_incinv_create_header,
          lt_itemdata   TYPE STANDARD TABLE OF bapi_incinv_create_item,
          lt_return     TYPE STANDARD TABLE OF bapiret2.

    DATA: lt_tax_data TYPE STANDARD TABLE OF bapi_incinv_create_tax,
          lv_cdsii    TYPE ztfi_0093-cdsii VALUE '15',
          error       TYPE int4.


    DATA: ls_ekpo     TYPE ekpo,
          ls_ekko     TYPE ekko,
          ls_ekkn     TYPE ekkn,
          ls_afko     TYPE afko,
          ls_proj     TYPE proj,
          ls_cldoc    TYPE ztfi_0001b,
          ls_t001     TYPE t001,
          ls_vbwf16   TYPE vbwf16,
          ls_0085     TYPE ztfi_0085,
          ls_0083     TYPE ztfi_0083,
          ls_return   TYPE bapiret2,
          ls_023      TYPE t023t,
          ls_tax_data TYPE bapi_incinv_create_tax,
          ls_itemdata TYPE bapi_incinv_create_item.



    CLEAR: ls_ekpo,
           ls_ekko,
           ls_ekkn,
           ls_afko,
           ls_proj,
           ls_cldoc,
           ls_t001,
           ls_vbwf16,
           ls_0085,
           ls_0083,
           ls_return ,
           ls_tax_data,
           ls_023,
           ls_itemdata.




    CLEAR: ls_itemdata,
           ls_return.

    FREE: lt_itemdata,
          lt_return.

    CLEAR ls_headerdata.

    DATA: lv_debe_haber TYPE shkzg.
    IF p_entrada-tipodte = '33' OR p_entrada-tipodte = '34'
    OR p_entrada-tipodte = '110' OR  p_entrada-tipodte = '46' OR  p_entrada-tipodte = '43'.
      ls_headerdata-invoice_ind = 'X'.
      lv_debe_haber = 'S'.
    ELSEIF p_entrada-tipodte = '61' OR  p_entrada-tipodte = '112'.
      ls_headerdata-invoice_ind = space.
      lv_debe_haber = 'H'.
    ELSEIF p_entrada-tipodte = '56' OR p_entrada-tipodte = '111'.
      ls_headerdata-invoice_ind = 'X'.
      lv_debe_haber = 'S'.
      "ls_itemdata-de_cre_ind  = 'X'.
    ENDIF.

    "------->homologación tipodte <=>clase doc.

    DATA: e_clase TYPE ztfi_0001b.
    CLEAR e_clase.

    e_clase-bukrs  = p_entrada-bukrs.
    e_clase-tipodte = p_entrada-tipodte.


    "---->Buscar BSART
*    READ TABLE lt_ekko INTO ls_ekko WITH KEY ebeln = p_oc.
*    IF sy-subrc EQ 0.
*      e_clase-bsart = ls_ekko-bsart.
*    ENDIF.

    "---->valor scope.
    READ TABLE lt_ekpo INTO ls_ekpo WITH KEY ebeln = p_oc.
    IF sy-subrc = 0.
      e_clase-knttp = ls_ekpo-knttp.
    ELSE.
      e_clase-knttp = ' '.
    ENDIF.

    IF ls_ekpo-matkl IS NOT INITIAL.
      SELECT SINGLE * FROM t023t INTO ls_023
       WHERE spras = sy-langu
         AND matkl = ls_ekpo-matkl  .
    ENDIF.

    READ TABLE lt_zdte_cldoc INTO ls_cldoc WITH KEY bukrs = e_clase-bukrs
                                     tipop = s_tipodte
                                     tipodte = e_clase-tipodte
                                     knttp = e_clase-knttp
                                     bsart = space.
    IF sy-subrc = 0.
      ls_headerdata-doc_type = ls_cldoc-blart.
    ELSE.
      READ TABLE lt_zdte_cldoc INTO ls_cldoc WITH KEY bukrs = e_clase-bukrs
                                       tipop = s_tipodte
                                       tipodte = e_clase-tipodte
                                       knttp = '*'
                                       bsart = space.
      IF sy-subrc EQ 0.
        ls_headerdata-doc_type = ls_cldoc-blart.
      ELSE.

      ENDIF.
    ENDIF.

    "----->determinación bloqueo para pago.
    READ TABLE lt_t001  INTO ls_t001 WITH KEY bukrs = p_entrada-bukrs.
    IF sy-subrc = 0.
      READ TABLE ti_vbwf16 INTO ls_vbwf16 WITH KEY wfvar = ls_t001-wfvar
                                    blart = ls_headerdata-doc_type.
      IF sy-subrc = 0.
        ls_headerdata-pmnt_block = 'P'.
      ENDIF.
    ENDIF.
    "---->fin.



    DATA: lv_amount_item     TYPE bapicurr_d,
          lv_amount_item_imp LIKE lv_amount_item.

    CLEAR lv_amount_item.
    CLEAR lv_amount_item_imp.
    DATA: lv_monto    LIKE p_entrada-wrbtr,
          lv_impuesto LIKE lv_monto.

    lv_monto = p_entrada-wrbtr.
    lv_impuesto = p_entrada-wrbtr - p_entrada-iva.

    CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
      EXPORTING
        currency        = p_entrada-waers
        amount_internal = lv_monto
      IMPORTING
        amount_external = lv_amount_item.

    ls_headerdata-doc_date = p_entrada-bldat.
    ls_headerdata-pstng_date = sy-datum.
    ls_headerdata-ref_doc_no = p_entrada-xblnr.
    ls_headerdata-comp_code = p_entrada-bukrs.
    ls_headerdata-currency = p_entrada-waers.
    ls_headerdata-gross_amount = lv_amount_item.
    ls_headerdata-calc_tax_ind  = ' '.
    ls_headerdata-bline_date = p_entrada-fechabase. "p_entrada-bldat.
    DATA: lv_invoice_doc_item TYPE rblgp.
    lv_invoice_doc_item  = '00001'.


*OT Texto y Asignación
    CONCATENATE p_entrada-tipodte '-' p_entrada-xblnr INTO  ls_headerdata-alloc_nmbr.
    CONDENSE ls_headerdata-alloc_nmbr NO-GAPS.
    CONCATENATE p_entrada-tipodte '-' p_entrada-xblnr INTO ls_headerdata-header_txt.
    CONDENSE ls_headerdata-header_txt NO-GAPS.

*
    IF ls_023-wgbez IS NOT INITIAL.
      ls_headerdata-item_text = ls_023-wgbez .
    ELSE.
      CONCATENATE  p_entrada-xblnr '-' p_entrada-name1  INTO ls_headerdata-item_text.
    ENDIF.
    CONDENSE ls_headerdata-item_text.


*<<<<< IVA >>>>>>>

    DATA: lv_tax_amount TYPE bapiwmwst. "lt_tax_data-tax_amount.

    CLEAR lv_tax_amount. CLEAR ls_tax_data.
    CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
      EXPORTING
        currency        = p_entrada-waers
        amount_internal = p_entrada-iva
      IMPORTING
        amount_external = lv_tax_amount.

    ls_tax_data-tax_amount = lv_tax_amount.


    IF  p_entrada-iva NE 0.
      me->indiva_defecto( EXPORTING t_ztfi_0093 = lt_ztfi_0093
                                    lv_cdsii = lv_cdsii
                                    p_entrada = p_entrada
                         CHANGING p_lt_tax_data_tax_code = ls_tax_data-tax_code
                         p_error = error ).
      CLEAR lv_tax_amount.
      CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
        EXPORTING
          currency        = p_entrada-waers
          amount_internal = p_entrada-mntneto
        IMPORTING
          amount_external = lv_tax_amount.

      ls_tax_data-tax_base_amount = lv_tax_amount.

    ELSE.
      CLEAR lv_tax_amount.
      CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
        EXPORTING
          currency        = p_entrada-waers
          amount_internal = p_entrada-mntexe
        IMPORTING
          amount_external = lv_tax_amount.
      ls_tax_data-tax_base_amount = lv_tax_amount.

      ls_tax_data-tax_code = 'C0'.
    ENDIF.

    APPEND ls_tax_data TO lt_tax_data .

    IF  p_entrada-mntneto NE 0  AND  p_entrada-mntexe NE 0.
      CLEAR lv_tax_amount. CLEAR ls_tax_data.
      CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
        EXPORTING
          currency        = p_entrada-waers
          amount_internal = p_entrada-mntexe
        IMPORTING
          amount_external = lv_tax_amount.
      ls_tax_data-tax_base_amount = lv_tax_amount.
      ls_tax_data-tax_amount = 0.
      ls_tax_data-tax_code = 'C0'.
      APPEND ls_tax_data TO lt_tax_data.
    ENDIF.



    DATA:lt_impu TYPE STANDARD TABLE OF bapi_incinv_create_account.
    DATA: ls_impu TYPE  bapi_incinv_create_account.
    FREE lt_impu.CLEAR ls_impu.


    DATA: lt_glimpu TYPE STANDARD TABLE OF bapi_incinv_create_gl_account,
          ln_glimpu LIKE LINE OF lt_glimpu.

    FREE lt_glimpu.
    CLEAR ln_glimpu.
    LOOP AT lt_ekpo INTO ls_ekpo WHERE ebeln = p_oc AND loekz NE 'X'.
      "----->estructura itemdata.
      CLEAR ls_itemdata.
      IF p_entrada-tipodte = '56' OR p_entrada-tipodte = '111'.
        ls_itemdata-de_cre_ind  = 'X'.
      ENDIF.
      ls_itemdata-po_number = ls_ekpo-ebeln.
      ls_itemdata-po_item = ls_ekpo-ebelp.
      ls_itemdata-invoice_doc_item = lv_invoice_doc_item.
      IF ls_ekpo-knttp NE 'U'.
        ls_itemdata-item_amount = lv_amount_item.
        ls_itemdata-tax_code = ls_ekpo-mwskz.
      ELSE.
        CLEAR:  ls_itemdata-item_amount,
                ls_itemdata-tax_code.
      ENDIF.

      APPEND ls_itemdata TO lt_itemdata.
      "---->si es tipo de imputación K o U
      IF ls_ekpo-knttp NE 'U'.
        "----->estructura imputaciones
        IF p_entrada-mntneto NE 0.
          CLEAR: ls_impu, lv_amount_item_imp.

          ls_impu-invoice_doc_item = lv_invoice_doc_item.
          READ TABLE lt_ekkn  INTO ls_ekkn WITH KEY ebeln = ls_ekpo-ebeln.
          IF sy-subrc = 0.
            ls_impu-gl_account = ls_ekkn-sakto.
            ls_impu-costcenter = ls_ekkn-kostl.
          ENDIF.
          ls_impu-xunpl = 'X'.
*        ls_impu-serial_no = '01'.
          ls_impu-tax_code = ls_ekpo-mwskz.

          CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
            EXPORTING
              currency        = p_entrada-waers
              amount_internal = p_entrada-mntneto "lv_impuesto
            IMPORTING
              amount_external = lv_amount_item_imp.

          ls_impu-item_amount = lv_amount_item_imp."p_entrada-wrbtr.
          APPEND ls_impu TO lt_impu.
        ENDIF.
        IF p_entrada-mntexe NE 0.
          CLEAR: ls_impu, lv_amount_item_imp.

          ls_impu-invoice_doc_item = lv_invoice_doc_item.
          READ TABLE lt_ekkn  INTO ls_ekkn WITH KEY ebeln = ls_ekpo-ebeln.
          IF sy-subrc = 0.
            ls_impu-gl_account = ls_ekkn-sakto.
            ls_impu-costcenter = ls_ekkn-kostl.
          ENDIF.
          ls_impu-xunpl = 'X'.
*        ls_impu-serial_no = '01'.
          ls_impu-tax_code = 'C0'.

          CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
            EXPORTING
              currency        = p_entrada-waers
              amount_internal = p_entrada-mntexe "lv_impuesto
            IMPORTING
              amount_external = lv_amount_item_imp.

          ls_impu-item_amount = lv_amount_item_imp."p_entrada-wrbtr.
          APPEND ls_impu TO lt_impu.
        ENDIF.


        ADD 1 TO lv_invoice_doc_item.

      ELSE.
        "---->se agrega condicionante por imputación U.
        READ TABLE ti_ztfi_0085 INTO ls_0085 WITH KEY bukrs = ls_ekpo-bukrs BINARY SEARCH.
        IF sy-subrc = 0.
          CLEAR ln_glimpu.
          ln_glimpu-gl_account = ls_0085-hkont.

          CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
            EXPORTING
              currency        = p_entrada-waers
              amount_internal = lv_impuesto
            IMPORTING
              amount_external = lv_amount_item_imp.

          ln_glimpu-item_amount = lv_amount_item_imp.
          ln_glimpu-costcenter = ls_0085-kostl.
          ln_glimpu-tax_code = ls_ekpo-mwskz.
          ln_glimpu-comp_code = ls_ekpo-bukrs.
          ln_glimpu-invoice_doc_item = lv_invoice_doc_item.
          ADD 1 TO lv_invoice_doc_item.
          ln_glimpu-db_cr_ind = lv_debe_haber.
          APPEND ln_glimpu TO lt_glimpu.
        ENDIF.
      ENDIF.
    ENDLOOP.

    DATA: lv_documento TYPE bapi_incinv_fld-inv_doc_no,
          lv_gjahr     TYPE bapi_incinv_fld-fisc_year.

    CLEAR: lv_documento,
           lv_gjahr.

    ls_headerdata-bline_date = p_entrada-fechabase.


    CALL FUNCTION 'BAPI_INCOMINGINVOICE_PARK'
      EXPORTING
        headerdata       = ls_headerdata
*       ADDRESSDATA      =
      IMPORTING
        invoicedocnumber = lv_documento
        fiscalyear       = lv_gjahr
      TABLES
        itemdata         = lt_itemdata
        accountingdata   = lt_impu
        glaccountdata    = lt_glimpu
        taxdata          = lt_tax_data
        return           = lt_return.

    IF lt_return[] IS NOT INITIAL.
      LOOP AT lt_return  INTO ls_return.
        IF ls_return-type = 'E' OR
           ls_return-type = 'A'.
          CLEAR wa_log.
          wa_log-estado = icon_red_light.
          wa_log-documento = p_entrada-xblnr.
          wa_log-sociedad = p_entrada-bukrs.
          wa_log-proveedor = p_entrada-lifnr.
          wa_log-fecha = p_entrada-bldat.
*--->  'Error al crear doc. trx MIR7, '
          CONCATENATE TEXT-404 ls_return-message INTO wa_log-mensaje
                                                       SEPARATED BY space.
          APPEND wa_log TO ti_log.
          p_entrada-status = '7'.
*          PERFORM generar_log USING p_entrada ls_return-type ls_return-id ls_return-number ls_return-message_v1
*                ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
          me->generar_log( EXPORTING p_entrada = p_entrada
            xmsgty = ls_return-type  xmsgid = ls_return-id xmsgno = ls_return-number xmsgv1 = ls_return-message_v1
            xmsgv2 = ls_return-message_v2 xmsgv3 = ls_return-message_v3 xmsgv4 = ls_return-message_v4 ).

        ENDIF.
      ENDLOOP.
    ELSEIF lt_return[] IS  INITIAL AND lv_documento IS NOT INITIAL AND lv_gjahr IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      CLEAR wa_log.
      wa_log-estado = icon_green_light.
      wa_log-documento = p_entrada-xblnr.
      wa_log-sociedad = p_entrada-bukrs.
      wa_log-proveedor = p_entrada-lifnr.
      wa_log-fecha = p_entrada-bldat.
*--->    'Documento creado MIR7: '
      CONCATENATE TEXT-405 lv_documento '/' lv_gjahr  INTO wa_log-mensaje.
      APPEND wa_log TO ti_log.
      p_entrada-status = '2'.
      p_entrada-belnr  = lv_documento.
      p_entrada-gjahr = lv_gjahr.
      p_entrada-tcode = 'MIR4'.
*    p_entrada-awkey =  me->arma_awkey( EXPORTING c_recfactprov = p_entrada ).
*    p_entrada-awtyp = 'RMRP'.
*    PERFORM generar_log USING p_entrada '012' wa_log-mensaje.
*      PERFORM generar_log USING p_entrada  ls_return-type ls_return-id ls_return-number ls_return-message_v1
*               ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
      me->generar_log( EXPORTING p_entrada = p_entrada
                        xmsgty = ls_return-type  xmsgid = ls_return-id xmsgno = ls_return-number xmsgv1 = ls_return-message_v1
                        xmsgv2 = ls_return-message_v2 xmsgv3 = ls_return-message_v3 xmsgv4 = ls_return-message_v4 ).
      READ TABLE ti_ztfi_0083 INTO ls_0083 WITH KEY sociedad = e_clase-bukrs
                                         clase_documento = ls_headerdata-doc_type.
      IF sy-subrc = 0.
        p_entrada-codlib = ls_0083-cod_lib.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD generar_mir7_comex.
    DATA: ls_headerdata TYPE bapi_incinv_create_header,
          lt_itemdata   TYPE STANDARD TABLE OF bapi_incinv_create_item,
          lt_return     TYPE STANDARD TABLE OF bapiret2.

    DATA: lt_tax_data TYPE STANDARD TABLE OF bapi_incinv_create_tax,
          lv_cdsii    TYPE ztfi_0093-cdsii VALUE '15',
          error       TYPE int4.


    DATA: ls_ekpo     TYPE ekpo,
          ls_ekko     TYPE ekko,
          ls_ekkn     TYPE ekkn,
          ls_afko     TYPE afko,
          ls_proj     TYPE proj,
          ls_cldoc    TYPE ztfi_0001b,
          ls_t001     TYPE t001,
          ls_vbwf16   TYPE vbwf16,
          ls_0085     TYPE ztfi_0085,
          ls_0083     TYPE ztfi_0083,
          ls_return   TYPE bapiret2,
          ls_023      TYPE t023t,
          ls_tax_data TYPE bapi_incinv_create_tax,
          ls_itemdata TYPE bapi_incinv_create_item.



    CLEAR: ls_ekpo,
           ls_ekko,
           ls_ekkn,
           ls_afko,
           ls_proj,
           ls_cldoc,
           ls_t001,
           ls_vbwf16,
           ls_0085,
           ls_0083,
           ls_return ,
           ls_tax_data,
           ls_023,
           ls_itemdata.




    CLEAR: ls_itemdata,
           ls_return.

    FREE: lt_itemdata,
          lt_return.

    CLEAR ls_headerdata.

    DATA: lv_debe_haber TYPE shkzg.
    IF p_entrada-tipodte = '33' OR p_entrada-tipodte = '34'
    OR p_entrada-tipodte = '110' OR p_entrada-tipodte = '46' OR  p_entrada-tipodte = '43'.
      ls_headerdata-invoice_ind = 'X'.
      lv_debe_haber = 'S'.
    ELSEIF p_entrada-tipodte = '61' OR  p_entrada-tipodte = '112'.
      ls_headerdata-invoice_ind = space.
      lv_debe_haber = 'H'.
    ELSEIF p_entrada-tipodte = '56' OR p_entrada-tipodte = '111'.
      ls_headerdata-invoice_ind = 'X'.
      lv_debe_haber = 'S'.
      "ls_itemdata-de_cre_ind  = 'X'.
    ENDIF.

    "------->homologación tipodte <=>clase doc.

    DATA: e_clase TYPE ztfi_0001b.
    CLEAR e_clase.

    e_clase-bukrs  = p_entrada-bukrs.
    e_clase-tipodte = p_entrada-tipodte.


    "---->Buscar BSART
*    READ TABLE lt_ekko INTO ls_ekko WITH KEY ebeln = p_oc.
*    IF sy-subrc EQ 0.
*      e_clase-bsart = ls_ekko-bsart.
*    ENDIF.

    "---->valor scope.
    READ TABLE lt_ekpo INTO ls_ekpo WITH KEY ebeln = p_oc.
    IF sy-subrc = 0.
      e_clase-knttp = ls_ekpo-knttp.
    ELSE.
      e_clase-knttp = ' '.
    ENDIF.

    IF ls_ekpo-matkl IS NOT INITIAL.
      SELECT SINGLE * FROM t023t INTO ls_023
       WHERE spras = sy-langu
         AND matkl = ls_ekpo-matkl  .
    ENDIF.

    READ TABLE lt_zdte_cldoc INTO ls_cldoc WITH KEY bukrs = e_clase-bukrs
                                     tipop = s_tipodte
                                     tipodte = e_clase-tipodte
                                     knttp = e_clase-knttp
                                     bsart = space.
    IF sy-subrc = 0.
      ls_headerdata-doc_type = ls_cldoc-blart.
    ELSE.
      READ TABLE lt_zdte_cldoc INTO ls_cldoc WITH KEY bukrs = e_clase-bukrs
                                       tipop = s_tipodte
                                       tipodte = e_clase-tipodte
                                       knttp = '*'
                                       bsart = space.
      IF sy-subrc EQ 0.
        ls_headerdata-doc_type = ls_cldoc-blart.
      ELSE.

      ENDIF.
    ENDIF.

    "----->determinación bloqueo para pago.
    READ TABLE lt_t001  INTO ls_t001 WITH KEY bukrs = p_entrada-bukrs.
    IF sy-subrc = 0.
      READ TABLE ti_vbwf16 INTO ls_vbwf16 WITH KEY wfvar = ls_t001-wfvar
                                    blart = ls_headerdata-doc_type.
      IF sy-subrc = 0.
        ls_headerdata-pmnt_block = 'P'.
      ENDIF.
    ENDIF.
    "---->fin.



    DATA: lv_amount_item     TYPE bapicurr_d,
          lv_amount_item_imp LIKE lv_amount_item.

    CLEAR lv_amount_item.
    CLEAR lv_amount_item_imp.
    DATA: lv_monto    LIKE p_entrada-wrbtr,
          lv_impuesto LIKE lv_monto.

    lv_monto = p_entrada-wrbtr.
    lv_impuesto = p_entrada-wrbtr - p_entrada-iva.

    CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
      EXPORTING
        currency        = p_entrada-waers
        amount_internal = lv_monto
      IMPORTING
        amount_external = lv_amount_item.

    ls_headerdata-diff_inv = p_entrada-lifnr.
    ls_headerdata-doc_date = p_entrada-bldat.
    ls_headerdata-pstng_date = sy-datum.
    ls_headerdata-ref_doc_no = p_entrada-xblnr.
    ls_headerdata-comp_code = p_entrada-bukrs.
    ls_headerdata-currency = p_entrada-waers.
    ls_headerdata-gross_amount = lv_amount_item.
    ls_headerdata-calc_tax_ind  = ' '.
    ls_headerdata-bline_date = p_entrada-fechabase. "p_entrada-bldat.
    DATA: lv_invoice_doc_item TYPE rblgp.
    lv_invoice_doc_item  = '00001'.


*OT Texto y Asignación
    CONCATENATE p_entrada-tipodte '-' p_entrada-xblnr INTO  ls_headerdata-alloc_nmbr.
    CONDENSE ls_headerdata-alloc_nmbr NO-GAPS.

    CONCATENATE p_entrada-tipodte '-' p_entrada-xblnr INTO ls_headerdata-header_txt.
    CONDENSE ls_headerdata-header_txt NO-GAPS.

    IF ls_023-wgbez IS NOT INITIAL.
      ls_headerdata-item_text = ls_023-wgbez .
    ELSE.
      CONCATENATE  p_entrada-xblnr '-' p_entrada-name1  INTO ls_headerdata-item_text.
    ENDIF.
    CONDENSE ls_headerdata-item_text.



*<<<<< IVA >>>>>>>

    DATA: lv_tax_amount TYPE bapiwmwst. "lt_tax_data-tax_amount.

    CLEAR lv_tax_amount. CLEAR ls_tax_data.
    CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
      EXPORTING
        currency        = p_entrada-waers
        amount_internal = p_entrada-iva
      IMPORTING
        amount_external = lv_tax_amount.

    ls_tax_data-tax_amount = lv_tax_amount.


    IF  p_entrada-iva NE 0.
      me->indiva_defecto( EXPORTING t_ztfi_0093 = lt_ztfi_0093
                                    lv_cdsii = lv_cdsii
                                    p_entrada = p_entrada
                         CHANGING p_lt_tax_data_tax_code = ls_tax_data-tax_code
                         p_error = error ).
      CLEAR lv_tax_amount.
      CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
        EXPORTING
          currency        = p_entrada-waers
          amount_internal = p_entrada-mntneto
        IMPORTING
          amount_external = lv_tax_amount.

      ls_tax_data-tax_base_amount = lv_tax_amount.

    ELSE.
      CLEAR lv_tax_amount.
      CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
        EXPORTING
          currency        = p_entrada-waers
          amount_internal = p_entrada-mntexe
        IMPORTING
          amount_external = lv_tax_amount.
      ls_tax_data-tax_base_amount = lv_tax_amount.

      ls_tax_data-tax_code = 'C0'.
    ENDIF.

    APPEND ls_tax_data TO lt_tax_data .

    IF  p_entrada-mntneto NE 0  AND  p_entrada-mntexe NE 0.
      CLEAR lv_tax_amount. CLEAR ls_tax_data.
      CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
        EXPORTING
          currency        = p_entrada-waers
          amount_internal = p_entrada-mntexe
        IMPORTING
          amount_external = lv_tax_amount.
      ls_tax_data-tax_base_amount = lv_tax_amount.
      ls_tax_data-tax_amount = 0.
      ls_tax_data-tax_code = 'C0'.
      APPEND ls_tax_data TO lt_tax_data.
    ENDIF.


    DATA: lv_documento TYPE bapi_incinv_fld-inv_doc_no,
          lv_gjahr     TYPE bapi_incinv_fld-fisc_year.

    CLEAR: lv_documento,
           lv_gjahr.


    CALL FUNCTION 'BAPI_INCOMINGINVOICE_PARK'
      EXPORTING
        headerdata       = ls_headerdata
*       ADDRESSDATA      =
      IMPORTING
        invoicedocnumber = lv_documento
        fiscalyear       = lv_gjahr
      TABLES
        itemdata         = lt_itemdata
        taxdata          = lt_tax_data
*       accountingdata   = lt_impu
*       glaccountdata    = lt_glimpu
        return           = lt_return.

    IF lt_return[] IS NOT INITIAL.
      LOOP AT lt_return  INTO ls_return.
        IF ls_return-type = 'E' OR
           ls_return-type = 'A'.
          CLEAR wa_log.
          wa_log-estado = icon_red_light.
          wa_log-documento = p_entrada-xblnr.
          wa_log-sociedad = p_entrada-bukrs.
          wa_log-proveedor = p_entrada-lifnr.
          wa_log-fecha = p_entrada-bldat.
*--->  'Error al crear doc. trx MIR7, '
          CONCATENATE TEXT-404 ls_return-message INTO wa_log-mensaje
                                                       SEPARATED BY space.
          APPEND wa_log TO ti_log.
          p_entrada-status = '7'.
          me->generar_log( EXPORTING p_entrada = p_entrada
            xmsgty = ls_return-type  xmsgid = ls_return-id xmsgno = ls_return-number xmsgv1 = ls_return-message_v1
            xmsgv2 = ls_return-message_v2 xmsgv3 = ls_return-message_v3 xmsgv4 = ls_return-message_v4 ).

        ENDIF.
      ENDLOOP.
    ELSEIF lt_return[] IS  INITIAL AND lv_documento IS NOT INITIAL AND lv_gjahr IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      CLEAR wa_log.
      wa_log-estado = icon_green_light.
      wa_log-documento = p_entrada-xblnr.
      wa_log-sociedad = p_entrada-bukrs.
      wa_log-proveedor = p_entrada-lifnr.
      wa_log-fecha = p_entrada-bldat.
*--->    'Documento creado MIR7: '
      CONCATENATE TEXT-405 lv_documento '/' lv_gjahr  INTO wa_log-mensaje.
      APPEND wa_log TO ti_log.
      p_entrada-status = '2'.
      p_entrada-belnr  = lv_documento.
      p_entrada-gjahr = lv_gjahr.
      p_entrada-tcode = 'MIR4'.
*      p_entrada-awkey =  me->arma_awkey( EXPORTING c_recfactprov = p_entrada ).
*      p_entrada-awtyp = 'RMRP'.
      me->generar_log( EXPORTING p_entrada = p_entrada
                        xmsgty = ls_return-type  xmsgid = ls_return-id xmsgno = ls_return-number xmsgv1 = ls_return-message_v1
                        xmsgv2 = ls_return-message_v2 xmsgv3 = ls_return-message_v3 xmsgv4 = ls_return-message_v4 ).
      READ TABLE ti_ztfi_0083 INTO ls_0083 WITH KEY sociedad = e_clase-bukrs
                                         clase_documento = ls_headerdata-doc_type.
      IF sy-subrc = 0.
        p_entrada-codlib = ls_0083-cod_lib.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD generar_mira.
    DATA: lt_return TYPE STANDARD TABLE OF bapiret2.
    DATA: ls_return TYPE bapiret2.
    DATA: ls_headerdata           TYPE bapi_incinv_create_header,
          ls_additionalheaderdata TYPE bapi_incinv_save_header_backgr,
          ls_refdoccategory       TYPE bapi_incinv_fld-ref_doc_category.

    DATA: lt_tax_data TYPE STANDARD TABLE OF bapi_incinv_create_tax.
    DATA: wa_tax TYPE bapi_incinv_create_tax.

    DATA: ls_ekko   TYPE ekko,
          ls_ekpo   TYPE ekpo,
          ls_0075   TYPE ztfi_0075,
          ls_ekkn   TYPE ekkn,
          ls_afko   TYPE afko,
          ls_proj   TYPE proj,
          ls_cldoc  TYPE ztfi_0001b,
          ls_t001   TYPE t001,
          ls_vbwf16 TYPE vbwf16,
          ls_023    TYPE t023t,
          ls_0083   TYPE ztfi_0083.
    CLEAR: ls_ekko,
      ls_ekpo,
      ls_0075,
      ls_ekkn,
      ls_afko,
      ls_proj,
      ls_cldoc,
      ls_t001,
      ls_vbwf16,
      ls_023,
      ls_0083.

    CLEAR: lt_return,
           ls_headerdata,
            ls_additionalheaderdata,
            ls_refdoccategory,
            lt_tax_data.

    FREE lt_tax_data.


    IF p_entrada-tipodte = '33' OR p_entrada-tipodte = '34'
    OR p_entrada-tipodte = '110' OR  p_entrada-tipodte = '43' OR  p_entrada-tipodte = '46'.
      ls_headerdata-invoice_ind = 'X'.
    ELSEIF p_entrada-tipodte = '61' OR p_entrada-tipodte = '112'.
      ls_headerdata-invoice_ind = space.
    ELSEIF p_entrada-tipodte = '56' OR p_entrada-tipodte = '111' .
      ls_headerdata-invoice_ind = 'X'.
      "lt_itemdata-de_cre_ind  = 'X'.
    ENDIF.


    "------->homologación tipodte <=>clase doc.

    DATA: e_clase TYPE ztfi_0001b.
    CLEAR e_clase.

    e_clase-bukrs   = p_entrada-bukrs.
    e_clase-tipodte = p_entrada-tipodte.


    "---->Buscar BSART
*    READ TABLE lt_ekko INTO ls_ekko WITH KEY ebeln = p_entrada-ebeln.
*    IF sy-subrc EQ 0.
*      e_clase-bsart = ls_ekko-bsart.
*    ENDIF.

    "---->valor scope.
    READ TABLE lt_ekpo INTO ls_ekpo WITH KEY ebeln = p_entrada-ebeln.
    IF sy-subrc = 0.
      e_clase-knttp = ls_ekpo-knttp.
    ELSE.
      e_clase-knttp = ' '.
    ENDIF.

    IF ls_ekpo-matkl IS NOT INITIAL.
      SELECT SINGLE * FROM t023t INTO ls_023
       WHERE spras = sy-langu
         AND matkl = ls_ekpo-matkl  .
    ENDIF.

    READ TABLE lt_zdte_cldoc INTO ls_cldoc WITH KEY bukrs = e_clase-bukrs
                                     tipop = s_tipodte
                                     tipodte = e_clase-tipodte
                                     knttp = e_clase-knttp
                                     bsart = space.
    IF sy-subrc = 0.
      ls_headerdata-doc_type = ls_cldoc-blart.
    ELSE.
      READ TABLE lt_zdte_cldoc INTO ls_cldoc WITH KEY bukrs = e_clase-bukrs
                                       tipop = s_tipodte
                                       tipodte = e_clase-tipodte
                                       knttp = '*'
                                       bsart = space.
      IF sy-subrc EQ 0.
        ls_headerdata-doc_type = ls_cldoc-blart.
      ELSE.

      ENDIF.
    ENDIF.


    "---->fin homologación

    "----->determinación bloqueo para pago.
    READ TABLE lt_t001 INTO ls_t001 WITH KEY bukrs = p_entrada-bukrs.
    IF sy-subrc = 0.
      READ TABLE ti_vbwf16 INTO ls_vbwf16 WITH KEY wfvar = ls_t001-wfvar
                                           blart = ls_headerdata-doc_type.
      IF sy-subrc = 0.
        ls_headerdata-pmnt_block = 'P'.
      ENDIF.
    ENDIF.
    "---->fin.

    ls_headerdata-comp_code = p_entrada-bukrs.
    ls_headerdata-ref_doc_no = p_entrada-xblnr.
    CONCATENATE p_entrada-tipodte '-' p_entrada-xblnr INTO ls_headerdata-header_txt.
    ls_headerdata-pstng_date  = sy-datum.
    ls_headerdata-bline_date = p_entrada-fechabase. "p_entrada-bldat.
*MEDS  Texto y Asignación
    CLEAR ls_headerdata-item_text.
    CONCATENATE 'OC-' p_entrada-ebeln INTO ls_headerdata-header_txt.
    CONDENSE ls_headerdata-header_txt NO-GAPS.

    ls_headerdata-alloc_nmbr =  p_entrada-xblnr.
    CONDENSE ls_headerdata-alloc_nmbr NO-GAPS.

*
    IF ls_023-wgbez IS NOT INITIAL.
      ls_headerdata-item_text = ls_023-wgbez .
    ELSE.
      CONCATENATE  p_entrada-xblnr '-' p_entrada-name1  INTO ls_headerdata-item_text.
    ENDIF.
    CONDENSE ls_headerdata-item_text.


    DATA: lv_amount TYPE bapicurr_d.
    CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
      EXPORTING
        currency        = p_entrada-waers
        amount_internal = p_entrada-wrbtr
      IMPORTING
        amount_external = lv_amount.

    ls_headerdata-gross_amount = lv_amount.
    ls_headerdata-currency = p_entrada-waers.
    ls_headerdata-doc_date = p_entrada-bldat.

    CLEAR lt_tax_data.
    FREE lt_tax_data.

    DATA: lv_tax_amount TYPE bapiwmwst.

    CLEAR lv_tax_amount.
    CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
      EXPORTING
        currency        = p_entrada-waers
        amount_internal = p_entrada-iva
      IMPORTING
        amount_external = lv_tax_amount.

    IF lv_tax_amount NE 0 AND s_tipodte = '4'. "Costos Ind/Ind.IVa
      wa_tax-tax_code = 'C1'.
    ENDIF.
    IF lv_tax_amount EQ 0 AND s_tipodte = '4'. "Costos Ind/ind.IVa
      wa_tax-tax_code = 'C0'.
    ENDIF.

    wa_tax-tax_amount = lv_tax_amount.
    APPEND wa_tax TO lt_tax_data.

    DATA: lt_selectpo TYPE STANDARD TABLE OF bapi_incinv_select_po.
    DATA: ls_po TYPE  bapi_incinv_select_po.

*         lt_selectsv       TYPE STANDARD TABLE OF bapi_incinv_select_service WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE,
*        lt_selectdelivery TYPE STANDARD TABLE OF bapi_incinv_select_delivery WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
*
*  CLEAR lt_selectpo.CLEAR lt_selectsv.CLEAR lt_selectdelivery.
*  FREE lt_selectpo.FREE  lt_selectsv. FREE lt_selectdelivery.
*
**<<<<< Servicios HES >>>>>
*  LOOP AT lt_zdt_recref WHERE bukrs = p_entrada-bukrs
*                         AND xblnr = p_entrada-xblnr
*                         AND lifnr = p_entrada-lifnr
*                         AND tipodte = p_entrada-tipodte
*                         AND tiporef = 'HES'.
*    lt_selectsv-sheet_no = lt_zdt_recref-folioref.
*    APPEND lt_selectsv.
*  ENDLOOP.
*  DELETE ADJACENT DUPLICATES FROM lt_selectsv COMPARING ALL FIELDS.
*
**<<<<< Nota de entrega Guia de despacho >>>>>
*  LOOP AT lt_zdt_recref WHERE tiporef = '52'
*                           AND xblnr = p_entrada-xblnr
*                           AND bukrs = p_entrada-bukrs
*                           AND lifnr = p_entrada-lifnr.
*    lt_selectdelivery-delivery_note = lt_zdt_recref-folioref.
*    APPEND lt_selectdelivery.
*
*  ENDLOOP.
*  DELETE ADJACENT DUPLICATES FROM lt_selectdelivery COMPARING ALL FIELDS.
*
**<<<<< Por posiciones OC >>>>>

    CLEAR lt_selectpo.CLEAR ls_po.
    FREE lt_selectpo.
    LOOP AT  lt_ztfi_0075 INTO ls_0075 WHERE ebeln = p_entrada-ebeln
                          AND  bukrs = p_entrada-bukrs
                          AND  xblnr = p_entrada-xblnr
                          AND  lifnr = p_entrada-lifnr
                          AND  tipodte = p_entrada-tipodte.

      ls_po-po_number = p_entrada-ebeln.
      ls_po-po_item = ls_0075-ebelp.
      APPEND ls_po TO lt_selectpo.
    ENDLOOP.

    SORT lt_selectpo BY po_number po_item.

    DELETE ADJACENT DUPLICATES FROM lt_selectpo COMPARING ALL FIELDS.

    DATA: lv_invoicedocnumber TYPE re_belnr,
          lv_fiscalyear       TYPE gjahr.
*  *<<<<< Caso de NC/ND siempre va por OC >>>>>>
*  IF p_entrada-tipodte = '56' OR  p_entrada-tipodte = '61'.
*    ls_refdoccategory = '1'. "Por OC
*    FREE  lt_selectsv. CLEAR  lt_selectsv.
*    REFRESH lt_selectdelivery. CLEAR lt_selectdelivery.
*    ls_additionalheaderdata-sel_vendor = space.
*  ELSE.

    IF s_tipodte = '4'. "Costos Ind.-Proveedor
      ls_refdoccategory = '1'. "Por Proveedor
    ELSE.
      ls_refdoccategory = '1'. "prov.con OC
* *<<<<<Pedido de servicio >>>>>>>>>>>>
*    READ TABLE lt_ekpo WITH KEY ebeln = p_entrada-ebeln
*                                      pstyp = '9'.
*    IF sy-subrc = '0'.
*      IF lines( lt_selectsv[] ) GT 0.
*        ls_refdoccategory = '4'. " Hoja de entrada de servicios
*        REFRESH lt_selectdelivery. CLEAR lt_selectdelivery.
*        CLEAR lt_selectpo.  FREE lt_selectpo.
*        ls_additionalheaderdata-sel_vendor = space.
*      ELSE.
*        ls_refdoccategory = '1'. "Por OC
*        FREE  lt_selectsv. CLEAR  lt_selectsv.
*        REFRESH lt_selectdelivery. CLEAR lt_selectdelivery.
*        ls_additionalheaderdata-sel_vendor = space.
*      ENDIF.
*
*    ELSE.
**<<<<<<Pedidos Materiales/Imputados >>>>>>>>>
*      IF lines( lt_selectdelivery[] ) GT 0.
*        ls_refdoccategory = '2'. "Por Guia
*        REFRESH lt_selectpo. CLEAR lt_selectpo.
*        FREE  lt_selectsv. CLEAR  lt_selectsv.
*        ls_additionalheaderdata-sel_vendor =  lt_ekko-lifnr.
*      ELSE.
*        ls_refdoccategory = '2'. "Por Nro.Factura
*        REFRESH lt_selectpo. CLEAR lt_selectpo.
*        FREE  lt_selectsv.   CLEAR  lt_selectsv.
*        REFRESH lt_selectdelivery. CLEAR lt_selectdelivery.
*        lt_selectdelivery-delivery_note =  p_entrada-xblnr.
*        APPEND lt_selectdelivery.
*        ls_additionalheaderdata-sel_vendor =  lt_ekko-lifnr.
*
*      ENDIF.
*    ENDIF.
*
    ENDIF.
*ENDIF. "<<<<< Caso de NC/ND siempre va por OC >>>>>
    ls_additionalheaderdata-assign_deliv = 'X'.
    ls_additionalheaderdata-assign_return = 'X'.
    ls_additionalheaderdata-deliv_posting = 'S'.
    ls_additionalheaderdata-return_posting = 'H'.

    IF s_tipodte = '4'. "Costos Ind y Proveedor divergente a Factura IMP
      ls_headerdata-diff_inv = p_entrada-lifnr.
      ls_additionalheaderdata-sel_deliv_costs = 'X'.

    ELSE.
      ls_additionalheaderdata-sel_goods = 'X'.
    ENDIF.

    ls_headerdata-bline_date = p_entrada-fechabase.

    CALL FUNCTION 'BAPI_INCOMINGINVOICE_SAVE'
      EXPORTING
        headerdata           = ls_headerdata
        additionalheaderdata = ls_additionalheaderdata
        refdoccategory       = ls_refdoccategory
*       ADDRESSDATA          =
      IMPORTING
        invoicedocnumber     = lv_invoicedocnumber
        fiscalyear           = lv_fiscalyear
      TABLES
        selectpo             = lt_selectpo
*       SELECTDELIVERY       = lt_selectdelivery
*       SELECTBILLLADING     =
*       SELECTSERVICE        = lt_selectsv
*       SELECTPLANT          =
        taxdata              = lt_tax_data
*       WITHTAXDATA          =
*       VENDORITEMSPLITDATA  =
        return               = lt_return
*       EXTENSIONIN          =
      .

*  IF sy-subrc = 0.
    READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
    IF sy-subrc = 0.
      CLEAR wa_log.
      wa_log-estado = icon_red_light.
      wa_log-documento = p_entrada-xblnr.
      wa_log-sociedad = p_entrada-bukrs.
      wa_log-proveedor = p_entrada-lifnr.
      wa_log-fecha = p_entrada-bldat.
*----> 'Error al crear doc. trx MIRA, '
      CONCATENATE TEXT-402 ls_return-message INTO wa_log-mensaje
                                                     SEPARATED BY space.
      APPEND wa_log TO  ti_log.
      p_entrada-status = '7'.
*      PERFORM generar_log USING p_entrada '012'  wa_log-mensaje.
      me->generar_log( EXPORTING p_entrada = p_entrada
                xmsgty = ls_return-type  xmsgid = ls_return-id xmsgno = ls_return-number xmsgv1 = ls_return-message_v1
                xmsgv2 = ls_return-message_v2 xmsgv3 = ls_return-message_v3 xmsgv4 = ls_return-message_v4 ).

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      CLEAR wa_log.
      wa_log-estado = icon_green_light.
      wa_log-documento = p_entrada-xblnr.
      wa_log-sociedad = p_entrada-bukrs.
      wa_log-proveedor = p_entrada-lifnr.
      wa_log-fecha = p_entrada-bldat.
*---> 'Documento creado MIRA: '
      CONCATENATE TEXT-403 lv_invoicedocnumber '/' lv_fiscalyear INTO wa_log-mensaje.
      APPEND wa_log TO ti_log.
      p_entrada-status = s_estatus.                           " '3'.
      p_entrada-belnr = lv_invoicedocnumber.
      p_entrada-gjahr = lv_fiscalyear.
      p_entrada-tcode = 'MIR4'.
*      p_entrada-awkey =  me->arma_awkey( EXPORTING c_recfactprov = p_entrada ).
*      p_entrada-awtyp = 'RMRP'.
      me->generar_log( EXPORTING p_entrada = p_entrada
                    xmsgty = ls_return-type  xmsgid = ls_return-id xmsgno = ls_return-number xmsgv1 = ls_return-message_v1
                    xmsgv2 = ls_return-message_v2 xmsgv3 = ls_return-message_v3 xmsgv4 = ls_return-message_v4 ).

      CLEAR ls_0083.
      READ TABLE ti_ztfi_0083 INTO ls_0083 WITH KEY sociedad = e_clase-bukrs
                                  clase_documento = ls_headerdata-doc_type.
      IF sy-subrc = 0.
        p_entrada-codlib = ls_0083-cod_lib.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  method GENERAR_RECHAZO.
DATA: e_salida TYPE zst_dte_ack.
  DATA: error TYPE char1.
  DATA: lv_monto TYPE string.
  DATA:lv_addrnumber TYPE addr1_sel-addrnumber.
  TYPE-POOLS: szadr.
  DATA: lv_addr1_complete TYPE szadr_addr1_complete.
  DATA:lt_addr1_tab  TYPE STANDARD TABLE OF szadr_addr1_line,
       lt_adtel_tab  TYPE STANDARD TABLE OF szadr_adtel_line,
       lt_adsmtp_tab TYPE STANDARD TABLE OF szadr_adsmtp_line.

  DATA: ln_addr1_tab  LIKE LINE OF lt_addr1_tab,
        ln_adtel_tab  LIKE LINE OF lt_adtel_tab,
        ln_adsmtp_tab LIKE LINE OF lt_adsmtp_tab.

  CLEAR e_salida.CLEAR error.
  e_salida-bukrs = p_entrada-bukrs.

  e_salida-rutreceptor = p_entrada-stcd1.
  READ TABLE lt_paval ASSIGNING FIELD-SYMBOL(<lt1>) WITH KEY bukrs = p_entrada-bukrs.
  IF sy-subrc = 0.
    REPLACE ALL OCCURRENCES OF '.' IN <lt1>-paval WITH space.
    CONDENSE <lt1>-paval NO-GAPS.
    e_salida-rutrecibe = <lt1>-paval.
    e_salida-rutreceptor = <lt1>-paval.
  ENDIF.


  READ TABLE lt_lfa1 ASSIGNING FIELD-SYMBOL(<lt2>) WITH KEY lifnr = p_entrada-lifnr.
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


  READ TABLE lt_addr1_tab INTO ln_addr1_tab INDEX 1.
  IF sy-subrc = 0.

    e_salida-nmbcontacto = ln_addr1_tab-data-name1.
  ENDIF.

  e_salida-tipodte = p_entrada-tipodte."'33'.
  e_salida-folio = p_entrada-xblnr.
  CONCATENATE  p_entrada-bldat+0(4) '-' p_entrada-bldat+4(2) '-' p_entrada-bldat+6(2) INTO e_salida-fchemis.

  e_salida-rutemisor = p_entrada-stcd1.


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

*   "----Envio de Rechazo al SII
*   CALL FUNCTION 'ZFI_0008DTE'
*     EXPORTING
*       acknowledgment = e_salida
*       commit         = 'X'
*     IMPORTING
*       error          = error.

  CALL METHOD me->enviar_rechazoacep
    EXPORTING
      i_salida  = e_salida
      i_entrada = p_entrada
      i_commit  = 'X'
    IMPORTING
      e_error   = error.


*********************************************************
* Al No poder Rechazar, dejamos en error la Factura en
* el Monitor con "N" para que se intente manualmente el
* Rechazo.
*********************************************************

  IF error = 'W'.
    p_entrada-status = 'N'.
    p_entrada-glosa = 'Error WS al Rechazar Doc, Ver log'.
  ELSEIF error = '8'.
    p_entrada-status = '7'.
    p_entrada-glosa = 'Error WS al Rechazar Doc, Ver log'.
  ELSEIF error = '3'.
    p_entrada-status = 'N'.
    p_entrada-glosa = 'Error WS al Rechazar Doc, Ver log'.
  ELSEIF error = '27'.
    p_entrada-status = 'N'.
    p_entrada-glosa = 'Error WS al Rechazar Doc, Ver log'.
  ELSE.
    p_entrada-status = '6'.
    p_entrada-glosa = e_salida-estadodteglosa.
  ENDIF.
  endmethod.


  method GET_FECHA_A_EM.
   fecha =   zcl_dte_fi0057v2_cl=>fecha_e_aem .
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
*---> Grabo las tablas
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


  METHOD llamabapis.
    IF  s_tipodte EQ '1'. "Proveedor Siempre Con OC x MM-MIRA
*OT Envio BAPI
      IF estatus EQ '2' OR estatus EQ '3'.
        wa_recfactprov-status = estatus.
        me->generar_mira( EXPORTING  lt_ekko = lt_ekko
                                     lt_ekpo = lt_ekpo
                                     lt_ztfi_0075 = lt_zdte_posfact_oc "lt_zdte_posfact
                                     lt_ekkn = lt_ekkn
                                     lt_afko =  lt_afko
                                     lt_proj = lt_proj
                                     lt_zdte_cldoc = lt_zdte_cldoc
                                     lt_t001 = lt_t001
                                     ti_vbwf16 = ti_vbwf16
                                     s_tipodte = s_tipodte
                                     s_estatus = estatus
                             CHANGING p_entrada = wa_recfactprov ).
        MODIFY  lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.
      ELSE.
        wa_recfactprov-status = estatus.
        wa_log-estado = icon_red_light.
        MESSAGE ID msgid TYPE msgty NUMBER msgno
        WITH msgv1 msgv2 msgv3 msgv4  INTO wa_log-mensaje.
        APPEND  wa_log TO ti_log.
        IF estatus = '6'.
          me->generar_log( EXPORTING p_entrada = wa_recfactprov  xmsgty = msgty xmsgid = msgid
                                         xmsgno = msgno  xmsgv1 = msgv1 xmsgv2 = msgv2 xmsgv3 = msgv3
                                         xmsgv4 = msgv4 ).
          cnum = '000'.
          me->generar_rechazo( EXPORTING lt_paval = lt_t001z  lt_lfa1 = lt_lfa1  p_glosa = wa_log-mensaje p_numero = cnum
                             CHANGING p_entrada = wa_recfactprov ).  "TABLES lt_t001z  lt_lfa1 USING  wa_log-mensaje '000' CHANGING wa_recfactprov.
        ELSE.
          me->generar_log( EXPORTING p_entrada = wa_recfactprov  xmsgty = msgty xmsgid = msgid
                                         xmsgno = msgno  xmsgv1 = msgv1 xmsgv2 = msgv2 xmsgv3 = msgv3
                                         xmsgv4 = msgv4 ).  "using wa_recfactprov msgty msgid msgno msgv1 msgv2 msgv3 msgv4.
        ENDIF.
        MODIFY  lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.

      ENDIF.

    ELSEIF  s_tipodte EQ '2'. "Proveedor Sin OC x FI-FBV1
*OT Envio BAPI
      IF  estatus EQ '2' OR estatus EQ '3'.
        wa_recfactprov-status = estatus.
        me->generar_fbv1_bapi( EXPORTING t_ekpo =    lt_ekpo
                               t_ztfi_0075      =    lt_zdte_posfact
                               t_ekkn =              lt_ekkn
                               t_afko =              lt_afko
                               t_proj =              lt_proj
                               t_lfb1 =              lt_lfb1
                               t_zdte_cldoc =        lt_zdte_cldoc
                               t_ztfi_0090 =         lt_ztfi_0090
                               t_ztfi_0092 =         lt_ztfi_0092
                               t_ztfi_0093 =         lt_ztfi_0093
                               t_ztfi_0077 =         lt_zdt_recref
*                                          lt_ztfi_0074fr
                               u_tipodte  =    s_tipodte
                               u_estatus = estatus
                                  CHANGING  c_recfactprov = wa_recfactprov ).
        MODIFY  lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.
      ELSE.
        wa_recfactprov-status = estatus.
        wa_log-estado = icon_red_light.
        MESSAGE ID msgid TYPE msgty NUMBER msgno
        WITH msgv1 msgv2 msgv3 msgv4 INTO wa_log-mensaje.
        APPEND  wa_log TO ti_log.
        IF estatus = '6'.
          me->generar_log(  EXPORTING p_entrada = wa_recfactprov  xmsgty = msgty xmsgid = msgid
                         xmsgno = msgno  xmsgv1 = msgv1 xmsgv2 = msgv2 xmsgv3 = msgv3 xmsgv4 = msgv4 )
                                         . "using wa_recfactprov msgty msgid msgno msgv1 msgv2 msgv3 msgv4.
          cnum = '000'.
          me->generar_rechazo( EXPORTING lt_paval = lt_t001z  lt_lfa1 = lt_lfa1  p_glosa = wa_log-mensaje p_numero = cnum
                             CHANGING p_entrada = wa_recfactprov ).
        ELSE.
          me->generar_log(  EXPORTING p_entrada = wa_recfactprov  xmsgty = msgty xmsgid = msgid
                         xmsgno = msgno  xmsgv1 = msgv1 xmsgv2 = msgv2 xmsgv3 = msgv3 xmsgv4 = msgv4 ).
        ENDIF.
        MODIFY  lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.
      ENDIF.

    ELSEIF  s_tipodte EQ '3'. "Proveedor por Pedido limite MM-MIR7
*OT Envio BAPI
      IF  estatus EQ '2' OR estatus EQ '3'.
        wa_recfactprov-status = estatus.
        me->generar_mir7( EXPORTING  lt_zdte_cldoc = lt_zdte_cldoc
                                     lt_ekko = lt_ekko_lim
                                     lt_ekpo = lt_ekpo_lim
                                     lt_ekkn = lt_ekkn_lim
                                     lt_afko = lt_afko_lim
                                     lt_proj = lt_proj_lim
                                     p_oc =  wa_recfactprov-ebeln
                             CHANGING p_entrada = wa_recfactprov ).
        MODIFY  lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.
      ELSE.
        wa_recfactprov-status = estatus.
        wa_log-estado = icon_red_light.
        MESSAGE ID msgid TYPE msgty NUMBER msgno
        WITH msgv1 msgv2 msgv3 msgv4  INTO wa_log-mensaje.
        APPEND  wa_log TO ti_log.
        IF estatus = '6'.
          me->generar_log(  EXPORTING p_entrada = wa_recfactprov  xmsgty = msgty xmsgid = msgid
                          xmsgno = msgno  xmsgv1 = msgv1 xmsgv2 = msgv2 xmsgv3 = msgv3 xmsgv4 = msgv4 ).
          cnum = '000'.
          me->generar_rechazo( EXPORTING lt_paval = lt_t001z  lt_lfa1 = lt_lfa1  p_glosa = wa_log-mensaje p_numero = cnum
                             CHANGING p_entrada = wa_recfactprov ).

        ELSE.
          me->generar_log(  EXPORTING p_entrada = wa_recfactprov  xmsgty = msgty xmsgid = msgid
                           xmsgno = msgno  xmsgv1 = msgv1 xmsgv2 = msgv2 xmsgv3 = msgv3 xmsgv4 = msgv4 ).
        ENDIF.
        MODIFY  lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.

      ENDIF.


    ELSEIF  s_tipodte EQ '4'. "Proveedor OC  costos ind x MM-MIR7

*OT Envio BAPI
      IF estatus EQ '2' OR estatus EQ '3'.
        wa_recfactprov-status = estatus.

        me->generar_mir7_comex( EXPORTING  lt_zdte_cldoc = lt_zdte_cldoc
                                    lt_ekko = lt_ekko_lim
                                    lt_ekpo = lt_ekpo_lim
                                    lt_ekkn = lt_ekkn_lim
                                    lt_afko = lt_afko_lim
                                    lt_proj = lt_proj_lim
                                    p_oc =  wa_recfactprov-ebeln
                            CHANGING p_entrada = wa_recfactprov ).

        MODIFY  lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.
      ELSE.
        wa_recfactprov-status = estatus.
        wa_log-estado = icon_red_light.
        MESSAGE ID msgid TYPE msgty NUMBER msgno
        WITH msgv1 msgv2 msgv3 msgv4  INTO wa_log-mensaje.
        APPEND  wa_log TO ti_log.
        IF estatus = '6'.
          me->generar_log(  EXPORTING p_entrada = wa_recfactprov  xmsgty = msgty xmsgid = msgid
                           xmsgno = msgno  xmsgv1 = msgv1 xmsgv2 = msgv2 xmsgv3 = msgv3 xmsgv4 = msgv4 ).
          cnum = '000'.
          me->generar_rechazo( EXPORTING lt_paval = lt_t001z  lt_lfa1 = lt_lfa1  p_glosa = wa_log-mensaje p_numero = cnum
                             CHANGING p_entrada = wa_recfactprov ).

        ELSE.
          me->generar_log(  EXPORTING p_entrada = wa_recfactprov  xmsgty = msgty xmsgid = msgid
                            xmsgno = msgno  xmsgv1 = msgv1 xmsgv2 = msgv2 xmsgv3 = msgv3 xmsgv4 = msgv4 ).
        ENDIF.
        MODIFY  lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.
      ENDIF.
    ELSEIF  s_tipodte EQ '5'. "Proveedor Sin OC Y Dte.Cta.Automatica
*OT Envio BAPI
      IF  estatus EQ '2' OR estatus EQ '3'.
        wa_recfactprov-status = estatus.
        me->generar_fbv1_bapi( EXPORTING t_ekpo =    lt_ekpo
                                     t_ztfi_0075      =    lt_zdte_posfact
                                     t_ekkn =              lt_ekkn
                                     t_afko =              lt_afko
                                     t_proj =              lt_proj
                                     t_lfb1 =              lt_lfb1
                                     t_zdte_cldoc =        lt_zdte_cldoc
                                     t_ztfi_0090 =         lt_ztfi_0090
                                     t_ztfi_0092 =         lt_ztfi_0092
                                     t_ztfi_0093 =         lt_ztfi_0093
                                     t_ztfi_0077 =         lt_zdt_recref
*                                          lt_ztfi_0074fr
                                     u_tipodte  =    s_tipodte
                                     u_estatus = estatus
                                        CHANGING  c_recfactprov = wa_recfactprov ).
        MODIFY  lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.
      ELSE.
        wa_recfactprov-status = estatus.
        wa_log-estado = icon_red_light.
        MESSAGE ID msgid TYPE msgty NUMBER msgno
        WITH msgv1 msgv2 msgv3 msgv4 INTO wa_log-mensaje.
        APPEND  wa_log TO ti_log.
        IF estatus = '6'.
          me->generar_log(  EXPORTING p_entrada = wa_recfactprov  xmsgty = msgty xmsgid = msgid
                          xmsgno = msgno  xmsgv1 = msgv1 xmsgv2 = msgv2 xmsgv3 = msgv3 xmsgv4 = msgv4 ).
          cnum = '000'.
          me->generar_rechazo( EXPORTING lt_paval = lt_t001z  lt_lfa1 = lt_lfa1  p_glosa = wa_log-mensaje p_numero = cnum
                             CHANGING p_entrada = wa_recfactprov ).
        ELSE.
          me->generar_log(  EXPORTING p_entrada = wa_recfactprov  xmsgty = msgty xmsgid = msgid
                             xmsgno = msgno  xmsgv1 = msgv1 xmsgv2 = msgv2 xmsgv3 = msgv3 xmsgv4 = msgv4 ).
        ENDIF.
        MODIFY  lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.
      ENDIF.
    ELSE.
      IF estatus IS INITIAL.
        estatus = '7'.
      ENDIF.
*Proveedor no clasificado 1,2,3,4.
      wa_recfactprov-status = estatus.
      wa_log-estado = icon_red_light.

      IF  msgid IS NOT INITIAL
      AND msgty IS NOT INITIAL.
        MESSAGE ID msgid TYPE msgty NUMBER msgno
        WITH msgv1 msgv2 msgv3 msgv4 INTO wa_log-mensaje.
      ELSE.
        MESSAGE ID 'ZDTE_0001' TYPE 'E' NUMBER '008'
        WITH wa_recfactprov-lifnr INTO wa_log-mensaje.
      ENDIF.

      APPEND  wa_log TO ti_log.
      me->generar_log(  EXPORTING p_entrada = wa_recfactprov  xmsgty = msgty xmsgid = msgid
                           xmsgno = msgno  xmsgv1 = msgv1 xmsgv2 = msgv2 xmsgv3 = msgv3 xmsgv4 = msgv4 ).
      MODIFY  lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.

    ENDIF.
  ENDMETHOD.


  METHOD set_fecha_a_em.

    zcl_dte_fi0057v2_cl=>fecha_e_aem = fecha.

  ENDMETHOD.


  method SUSTITUCION.
*&--------------------------------------------------------------------------
* OT Sustitucion
* Se realiza sustitucion de OC y Posicion de OC
* SI el proveedor es 1  ; OC inicial -> HES -> Busca OC SAP
* SI el Proveedor es 3  ; OC incial  -> Busca  Adicional HES ->Pedido limite
*&--------------------------------------------------------------------------

  FIELD-SYMBOLS: <lt2> TYPE ztfi_0077,
                 <lt3> TYPE zty_ess,
                 <lt4> TYPE ztfi_0075,
                 <lt6> TYPE ekpo,
                 <lt7> TYPE  ekko.
  DATA: wa_zdte_posfact_oc TYPE ztfi_0075.
  DATA: ls_lfb1  TYPE lfb1.
  DATA: ls_pstyp TYPE pstyp.
  CLEAR ls_lfb1.

  READ TABLE lt_lfb1 INTO ls_lfb1   WITH KEY lifnr = wa_recfactprov-lifnr
                                   bukrs = wa_recfactprov-bukrs.

  IF ls_lfb1-zzdte_tipo = '1'. "Proveedor tipo Orden de compra
    IF wa_recfactprov-ebeln IS INITIAL.
*Buscar si hay HES, y de alli derivar la OC y Posicion OC.
      READ TABLE lt_zdt_recref ASSIGNING  <lt2> WITH KEY  bukrs = wa_recfactprov-bukrs
                                     xblnr = wa_recfactprov-xblnr
                                     lifnr = wa_recfactprov-lifnr
                                     tipodte = wa_recfactprov-tipodte
                                     tiporef = 'HES'.
      IF sy-subrc = 0.
        READ TABLE ti_essr ASSIGNING <lt3> WITH KEY lblni = <lt2>-folioref.
        IF sy-subrc = 0.
          wa_recfactprov-ebeln = <lt3>-ebeln.
          ls_lfb1-zzdte_tipo = '1'. "cambio de 1 a 3, para que entre a validacion OC(1)
          MODIFY  lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.
          READ TABLE lt_zdte_posfact ASSIGNING <lt4> WITH KEY  bukrs = wa_recfactprov-bukrs
                                     xblnr = wa_recfactprov-xblnr
                                     lifnr = wa_recfactprov-lifnr
                                     tipodte =  wa_recfactprov-tipodte.

          <lt4>-ebeln = <lt3>-ebeln.
          <lt4>-ebelp = <lt3>-ebelp.
*           MODIFY lt_zdte_posfact FROM lt_zdte_posfact  TRANSPORTING ebeln ebelp WHERE  bukrs = wa_recfactprov-bukrs
*                                     AND xblnr = wa_recfactprov-xblnr
*                                     AND  lifnr = wa_recfactprov-lifnr
*                                     AND tipodte =  wa_recfactprov-tipodte.
        ENDIF.
      ENDIF.
    ELSE. "Si ya se tiene OC, para el tipo 3, se verifica si se validarra como OC normal o OC pedido limite
      CLEAR t_bstyp.
      SELECT SINGLE bstyp INTO t_bstyp FROM ekko WHERE  ebeln = wa_recfactprov-ebeln
                                  AND  bstyp = 'F'.

      IF sy-subrc = 0.

      ELSE. "Si no encuentra ese doc como Pedido en SAP, que valide como tipo 1.

        READ TABLE lt_zdt_recref ASSIGNING  <lt2> WITH KEY  bukrs = wa_recfactprov-bukrs
                    xblnr = wa_recfactprov-xblnr
                    lifnr = wa_recfactprov-lifnr
                    tipodte = wa_recfactprov-tipodte
                    tiporef = 'HES'.
        IF sy-subrc = 0.
          READ TABLE ti_essr ASSIGNING <lt3> WITH KEY lblni = <lt2>-folioref.
          IF sy-subrc = 0.
            wa_recfactprov-ebeln = <lt3>-ebeln.

            ls_lfb1-zzdte_tipo = '1'. "cambio de 1 a 3, para que entre a validacion OC(1)
            MODIFY  lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.
            READ TABLE lt_zdte_posfact ASSIGNING <lt4> WITH KEY  bukrs = wa_recfactprov-bukrs
                                                     xblnr = wa_recfactprov-xblnr
                                                     lifnr = wa_recfactprov-lifnr
                                                     tipodte = wa_recfactprov-tipodte.

            <lt4>-ebeln = <lt3>-ebeln.
            <lt4>-ebelp = <lt3>-ebelp.
*             MODIFY lt_zdte_posfact FROM lt_zdte_posfact  TRANSPORTING ebeln ebelp WHERE  bukrs = wa_recfactprov-bukrs
*                                       AND xblnr = wa_recfactprov-xblnr
*                                       AND  lifnr = wa_recfactprov-lifnr
*                                       AND tipodte = wa_recfactprov-tipodte.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  IF ls_lfb1-zzdte_tipo = '3'. "Proveedor tipo MIXTO
    IF wa_recfactprov-ebeln IS INITIAL.
*Buscar si hay HES, y de alli derivar la OC y Posicion OC.
      READ TABLE lt_zdt_recref ASSIGNING  <lt2> WITH KEY  bukrs = wa_recfactprov-bukrs
                                     xblnr = wa_recfactprov-xblnr
                                     lifnr = wa_recfactprov-lifnr
                                     tipodte = wa_recfactprov-tipodte
                                     tiporef = 'HES'.
      IF sy-subrc = 0.
        READ TABLE ti_essr ASSIGNING <lt3> WITH KEY lblni = <lt2>-folioref.
        IF sy-subrc = 0.
          wa_recfactprov-ebeln = <lt3>-ebeln.
          ls_lfb1-zzdte_tipo = '1'. "cambio de 1 a 3, para que entre a validacion OC(1)
          MODIFY  lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.
          READ TABLE lt_zdte_posfact ASSIGNING <lt4> WITH KEY  bukrs = wa_recfactprov-bukrs
                                     xblnr = wa_recfactprov-xblnr
                                     lifnr = wa_recfactprov-lifnr
                                     tipodte = wa_recfactprov-tipodte.

          <lt4>-ebeln = <lt3>-ebeln.
          <lt4>-ebelp = <lt3>-ebelp.
*           MODIFY lt_zdte_posfact FROM lt_zdte_posfact  TRANSPORTING ebeln ebelp WHERE  bukrs = wa_recfactprov-bukrs
*                                     AND xblnr = wa_recfactprov-xblnr
*                                     AND  lifnr = wa_recfactprov-lifnr
*                                     AND tipodte = wa_recfactprov-tipodte.
*
        ELSE. "valor Hes enviado No existe en SAP
          IF ls_lfb1-zzdte_tipo = '3'. "SI es 3, busco adicionalmente Pedido limite
            LOOP AT lt_ekko_lim ASSIGNING <lt7> WHERE kdatb <= wa_recfactprov-bldat
                                               AND kdate >= wa_recfactprov-bldat
                                               AND lifnr EQ wa_recfactprov-lifnr.
              READ TABLE lt_ekpo_lim ASSIGNING <lt6> WITH KEY ebeln = <lt7>-ebeln.
              IF sy-subrc = 0.
                wa_recfactprov-ebeln = <lt7>-ebeln.
                MODIFY  lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.
                READ TABLE lt_zdte_posfact ASSIGNING <lt4> WITH KEY  bukrs = wa_recfactprov-bukrs
                                                         xblnr = wa_recfactprov-xblnr
                                                         lifnr = wa_recfactprov-lifnr
                                                         tipodte = wa_recfactprov-tipodte.

                <lt4>-ebeln = <lt6>-ebeln.
                <lt4>-ebelp = <lt6>-ebelp.
*                 MODIFY lt_zdte_posfact FROM lt_zdte_posfact  TRANSPORTING ebeln ebelp WHERE  bukrs = wa_recfactprov-bukrs
*                                           AND xblnr = wa_recfactprov-xblnr
*                                           AND  lifnr = wa_recfactprov-lifnr
*                                           AND tipodte = wa_recfactprov-tipodte.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ELSE.
        IF ls_lfb1-zzdte_tipo = '3'. "SI es 3, busco adicionalmente Pedido limite
          LOOP AT lt_ekko_lim ASSIGNING <lt7> WHERE kdatb <= wa_recfactprov-bldat
                                             AND kdate >= wa_recfactprov-bldat
                                             AND lifnr EQ wa_recfactprov-lifnr.
            READ TABLE lt_ekpo_lim ASSIGNING <lt6> WITH KEY ebeln = <lt7>-ebeln.
            IF sy-subrc = 0.
              wa_recfactprov-ebeln = <lt7>-ebeln.

              MODIFY  lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.
              READ TABLE lt_zdte_posfact ASSIGNING <lt4> WITH KEY  bukrs = wa_recfactprov-bukrs
                                                       xblnr = wa_recfactprov-xblnr
                                                       lifnr = wa_recfactprov-lifnr
                                                       tipodte = wa_recfactprov-tipodte.

              <lt4>-ebeln = <lt6>-ebeln.
              <lt4>-ebelp = <lt6>-ebelp.
*               MODIFY lt_zdte_posfact FROM lt_zdte_posfact  TRANSPORTING ebeln ebelp WHERE  bukrs = wa_recfactprov-bukrs
*                                         AND xblnr = wa_recfactprov-xblnr
*                                         AND  lifnr = wa_recfactprov-lifnr
*                                         AND tipodte = wa_recfactprov-tipodte.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ELSE. "Si ya se tiene OC, para el tipo 3, se verifica si se validarra como OC normal o OC pedido limite
      CLEAR t_bstyp.
      SELECT SINGLE bstyp INTO t_bstyp FROM ekko WHERE  ebeln = wa_recfactprov-ebeln
                                  AND  bstyp = 'F'.

      IF sy-subrc = 0.
        CLEAR  t_pstyp.
        SELECT SINGLE  pstyp INTO t_pstyp FROM ekpo WHERE ebeln = wa_recfactprov-ebeln
                                   AND  pstyp = '1'.

        IF t_pstyp NE '1'. "No es Pedido limite
          ls_lfb1-zzdte_tipo = '1'.
        ELSE.
          ls_lfb1-zzdte_tipo = '3'.
          LOOP AT lt_ekko_lim ASSIGNING <lt7> WHERE kdatb <= wa_recfactprov-bldat
                                   AND kdate >= wa_recfactprov-bldat
                                   AND lifnr EQ wa_recfactprov-lifnr.
            READ TABLE lt_ekpo_lim ASSIGNING <lt6> WITH KEY ebeln = <lt7>-ebeln.
            IF sy-subrc = 0.
              wa_recfactprov-ebeln = <lt7>-ebeln.

              MODIFY  lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.
              READ TABLE lt_zdte_posfact ASSIGNING <lt4> WITH KEY  bukrs = wa_recfactprov-bukrs
                                                       xblnr = wa_recfactprov-xblnr
                                                       lifnr = wa_recfactprov-lifnr
                                                       tipodte = wa_recfactprov-tipodte.

              <lt4>-ebeln = <lt6>-ebeln.
              <lt4>-ebelp = <lt6>-ebelp.
*               MODIFY lt_zdte_posfact FROM lt_zdte_posfact  TRANSPORTING ebeln ebelp WHERE  bukrs = wa_recfactprov-bukrs
*                                         AND xblnr = wa_recfactprov-xblnr
*                                         AND  lifnr = wa_recfactprov-lifnr
*                                         AND tipodte = wa_recfactprov-tipodte.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ELSE. "Si no encuentra ese doc como Pedido en SAP, que valide como tipo 1.
*            lt_lfb1-zzdte_tipo = '1'.
        READ TABLE lt_zdt_recref ASSIGNING  <lt2> WITH KEY  bukrs = wa_recfactprov-bukrs
                    xblnr = wa_recfactprov-xblnr
                    lifnr = wa_recfactprov-lifnr
                    tipodte = wa_recfactprov-tipodte
                    tiporef = 'HES'.
        IF sy-subrc = 0.
          READ TABLE ti_essr ASSIGNING <lt3> WITH KEY lblni = <lt2>-folioref.
          IF sy-subrc = 0.
            wa_recfactprov-ebeln = <lt3>-ebeln.

            ls_lfb1-zzdte_tipo = '1'. "cambio de 1 a 3, para que entre a validacion OC(1)
            MODIFY  lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.
            READ TABLE lt_zdte_posfact ASSIGNING <lt4> WITH KEY  bukrs = wa_recfactprov-bukrs
                                                     xblnr = wa_recfactprov-xblnr
                                                     lifnr = wa_recfactprov-lifnr
                                                     tipodte = wa_recfactprov-tipodte.

            <lt4>-ebeln = <lt3>-ebeln.
            <lt4>-ebelp = <lt3>-ebelp.
*             MODIFY lt_zdte_posfact FROM lt_zdte_posfact  TRANSPORTING ebeln ebelp WHERE  bukrs = wa_recfactprov-bukrs
*                                       AND xblnr = wa_recfactprov-xblnr
*                                       AND  lifnr = wa_recfactprov-lifnr
*                                       AND tipodte = wa_recfactprov-tipodte.
          ELSE. "valor Hes enviado No existe en SAP
            IF ls_lfb1-zzdte_tipo = '3'. "SI es 3, busco adicionalmente Pedido limite
              LOOP AT lt_ekko_lim ASSIGNING <lt7> WHERE kdatb <= wa_recfactprov-bldat
                                                 AND kdate >= wa_recfactprov-bldat
                                                 AND lifnr EQ wa_recfactprov-lifnr.
                READ TABLE lt_ekpo_lim ASSIGNING <lt6> WITH KEY ebeln = <lt7>-ebeln.
                IF sy-subrc = 0.
                  wa_recfactprov-ebeln = <lt7>-ebeln.

                  MODIFY  lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.
                  READ TABLE lt_zdte_posfact ASSIGNING <lt4> WITH KEY  bukrs = wa_recfactprov-bukrs
                                                           xblnr = wa_recfactprov-xblnr
                                                           lifnr = wa_recfactprov-lifnr
                                                           tipodte = wa_recfactprov-tipodte.

                  <lt4>-ebeln = <lt6>-ebeln.
                  <lt4>-ebelp = <lt6>-ebelp.
*                   MODIFY lt_zdte_posfact FROM lt_zdte_posfact  TRANSPORTING ebeln ebelp WHERE  bukrs = wa_recfactprov-bukrs
*                                             AND xblnr = wa_recfactprov-xblnr
*                                             AND  lifnr = wa_recfactprov-lifnr
*                                             AND tipodte = wa_recfactprov-tipodte.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ELSE.
          IF ls_lfb1-zzdte_tipo = '3'. "SI es 3, busco adicionalmente Pedido limite
            LOOP AT lt_ekko_lim ASSIGNING <lt7> WHERE kdatb <= wa_recfactprov-bldat
                                               AND kdate >= wa_recfactprov-bldat
                                               AND lifnr EQ wa_recfactprov-lifnr.
              READ TABLE lt_ekpo_lim ASSIGNING <lt6> WITH KEY ebeln = <lt7>-ebeln.
              IF sy-subrc = 0.
                wa_recfactprov-ebeln = <lt7>-ebeln.

                MODIFY  lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.
                READ TABLE lt_zdte_posfact ASSIGNING <lt4> WITH KEY  bukrs = wa_recfactprov-bukrs
                                                         xblnr = wa_recfactprov-xblnr
                                                         lifnr = wa_recfactprov-lifnr
                                                         tipodte = wa_recfactprov-tipodte.

                <lt4>-ebeln = <lt6>-ebeln.
                <lt4>-ebelp = <lt6>-ebelp.
*                 MODIFY lt_zdte_posfact FROM lt_zdte_posfact  TRANSPORTING ebeln ebelp WHERE  bukrs = wa_recfactprov-bukrs
*                                           AND xblnr = wa_recfactprov-xblnr
*                                           AND  lifnr = wa_recfactprov-lifnr
*                                           AND tipodte = wa_recfactprov-tipodte.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDIF.
*Sustitucion de Posicion OC.
  CLEAR lv_registro_pos.
  CLEAR wa_zdte_posfact_oc. REFRESH lt_zdte_posfact_oc.
  LOOP AT lt_zdte_posfact ASSIGNING <lt4>. "WHERE ebeln = wa_recfactprov-ebeln.
    lv_registro_pos = sy-tabix.
    IF <lt4>-ebeln IS INITIAL.
      <lt4>-ebeln = wa_recfactprov-ebeln.
*      MODIFY lt_zdte_posfact INDEX lv_registro_pos.
    ENDIF.
    IF <lt4>-ebelp IS INITIAL.
      READ TABLE  lt_zdt_recref ASSIGNING  <lt2> WITH KEY  bukrs = wa_recfactprov-bukrs
                                          xblnr = wa_recfactprov-xblnr
                                          lifnr = wa_recfactprov-lifnr
                                          tiporef = 'HES'.
      IF sy-subrc = 0.
        READ TABLE ti_essr ASSIGNING <lt3> WITH KEY lblni = <lt2>-folioref.
        IF sy-subrc = 0.
          <lt4>-ebelp = <lt3>-ebelp.
*           MODIFY lt_zdte_posfact INDEX lv_registro_pos .
*Solo OC y/o Pos del documneto procesado.
          APPEND <lt4> TO lt_zdte_posfact_oc.
        ENDIF.
      ENDIF.
    ELSE.
*Solo OC y/o Pos del documneto procesado.
      APPEND <lt4> TO lt_zdte_posfact_oc.
    ENDIF.
  ENDLOOP.
*OT Fin de Sustitucion
*.INI Buscas OC por referencia de NC/ND
*      clear ls_ebeln.
  IF wa_recfactprov-ebeln IS INITIAL AND ( wa_recfactprov-tipodte = '56' OR
                                               wa_recfactprov-tipodte = '61' )
                                         AND ( ls_lfb1-zzdte_tipo = '1' OR
                                               ls_lfb1-zzdte_tipo = '3' OR
                                               ls_lfb1-zzdte_tipo = '4' ).

    REFRESH  lt_docele. CLEAR  wa_docele. REFRESH lt_factmm. CLEAR wa_factmm.
    IF <lt2> IS ASSIGNED.
      UNASSIGN <lt2>.
    ENDIF.
    LOOP AT lt_zdt_recref ASSIGNING <lt2> WHERE   bukrs = wa_recfactprov-bukrs
                                 AND     xblnr = wa_recfactprov-xblnr
                                 AND     lifnr = wa_recfactprov-lifnr
                                 AND     tipodte = wa_recfactprov-tipodte
                                 AND     ( tiporef = '33' OR
                                           tiporef = '34' OR
                                           tiporef = '43' OR
                                           tiporef = '46' OR
                                           tiporef = '56' OR
                                           tiporef = '61'
                                           ).
      SELECT * INTO wa_docele FROM ztfi_0074 UP TO 1 ROWS
                         WHERE bukrs = <lt2>-bukrs AND
                               xblnr = <lt2>-folioref AND
                               lifnr = <lt2>-lifnr AND
                               tipodte = <lt2>-tiporef
                               ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF sy-subrc = 0.
        wa_recfactprov-ebeln = wa_docele-ebeln.
        MODIFY  lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.
*          EXIT.
      ENDIF.
      IF  wa_recfactprov-ebeln IS INITIAL. "Busco en MM sino esta en la tabla de Cab.Fact
        SELECT * INTO @wa_factmm FROM ZDDL_I_FACTMM UP TO 1 ROWS
        WHERE bukrs = @<lt2>-bukrs AND
                              xblnr = @<lt2>-folioref AND
                              lifnr = @<lt2>-lifnr AND
                              tipodte = @<lt2>-tiporef
                              ORDER BY bukrs, xblnr.
        ENDSELECT.
        IF sy-subrc = 0.
          wa_recfactprov-ebeln = wa_factmm-ebeln.
          MODIFY  lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.
*         EXIT.
        ENDIF.
      ENDIF.
      IF  wa_recfactprov-ebeln IS NOT  INITIAL. "Si ya encontro OC , de alguna de las 2formas se sale
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDIF.

*.FIN Buscas OC por referencia de NC/ND
**Valido si lt_zdte_posfact_oc tiene posiciones.
  IF    wa_recfactprov-ebeln IS NOT INITIAL .

    IF  lt_zdte_posfact_oc[] IS INITIAL .
      CLEAR wa_zdte_posfact_oc.
      SELECT *  FROM ekpo INTO TABLE lt_ekpo
         WHERE ebeln EQ wa_recfactprov-ebeln.
      IF sy-subrc = 0.
        LOOP AT lt_ekpo ASSIGNING FIELD-SYMBOL(<f1>).
          wa_zdte_posfact_oc-mandt = wa_recfactprov-mandt.
          wa_zdte_posfact_oc-bukrs = wa_recfactprov-bukrs.
          wa_zdte_posfact_oc-xblnr = wa_recfactprov-xblnr.
          wa_zdte_posfact_oc-lifnr = wa_recfactprov-lifnr.
          wa_zdte_posfact_oc-tipodte = wa_recfactprov-tipodte.
          wa_zdte_posfact_oc-ebelp = <f1>-ebelp.
          wa_zdte_posfact_oc-ebeln = wa_recfactprov-ebeln.
          APPEND wa_zdte_posfact_oc TO lt_zdte_posfact_oc.
        ENDLOOP.
      ENDIF.
    ELSE.

    ENDIF.
  ENDIF.

*OT Fin de Sustitucion
*---->
  IF    wa_recfactprov-ebeln IS NOT INITIAL AND ls_lfb1-zzdte_tipo = '1' .
    CLEAR ls_pstyp.
    SELECT SINGLE pstyp INTO ls_pstyp FROM ekpo
    WHERE ebeln = wa_recfactprov-ebeln
      AND pstyp = '2'.
    IF sy-subrc = 0.
      ls_lfb1-zzdte_tipo = '5'.
    ENDIF.
  ENDIF.
*---->  Asigno tipo DTE a valiables por el tipo mixto que es 1 y 3
  s_tipodte = ls_lfb1-zzdte_tipo.
  endmethod.


  METHOD validacion.
    DATA: lv_fecha TYPE datum.
    CLEAR lv_fecha.

    CALL METHOD super->validacion.
**// Seteo de envio de correo X EM
    lv_fecha = zcl_dte_fi0057v2_cl=>get_fecha_a_em( ).

    IF  lv_fecha IS NOT INITIAL.
      wa_recfactprov-fchcancel = lv_fecha.
      MODIFY lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.
    ENDIF.
**// Valida Rechazos
    IF estatus = '6'.
      "-----> Exclusiones por Proveedor.
      CLEAR ls_0096.
      READ TABLE lt_ztfi_0096 INTO ls_0096 WITH KEY bukrs = wa_recfactprov-bukrs
                                                   stcd1  = wa_recfactprov-stcd1.
      IF sy-subrc EQ 0.
        estatus = '7'.  "Se pasa de rechazo a error
      ENDIF.

      "-----> Exclusiones para Forma de pago al contado.
      IF  wa_recfactprov-fmapago EQ '1'.
        estatus = '7'. "Se pasa de rechazo a error
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
