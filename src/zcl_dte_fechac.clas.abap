class ZCL_DTE_FECHAC definition
  public
  create public .

public section.

  types:
    R_BUKRS type RANGE OF bukrs .
  types:
    R_LIFNR type RANGE OF lifnr .
  types:
    r_bldat type range of bldat .
  types:
    t_bapiactx09 TYPE STANDARD TABLE OF bapiactx09 .
  types:
    r_fecb type range of datum .
  types:
    t_bapiacgl09 TYPE STANDARD TABLE OF bapiacgl09 .
  types:
    r_fsii type range of datum .
  types:
    t_bapiacap09 TYPE STANDARD TABLE OF bapiacap09 .
  types:
    r_doc  type range of belnr_d .
  types:
    t_bapiaccr09 TYPE STANDARD TABLE OF bapiaccr09 .
  types:
    r_anno type range of gjahr .
  types:
    t_bseg TYPE STANDARD TABLE OF fvbseg .
  types:
    r_stat type range of ztfi_0074-status .
  types:
    t_bset TYPE  STANDARD TABLE OF fvbset .
  types:
    BEGIN OF ty_mod,
        icono         TYPE icon_d,
        documento(30) TYPE c,
        mensaje(200)  TYPE c,
      END OF ty_mod .
  types:
    t_zefi_0026 TYPE STANDARD TABLE OF zefi_0026 .
  types:
    t_modlog TYPE STANDARD TABLE OF  ty_mod .

  data LV_ITEMACC type POSNR_ACC .
  data GO_LOGC type ref to ZCL_BAL_LOG_DTE .
  data GS_BAL_LOG type BAL_S_LOG .
  data GS_BAL_MSG type BAL_S_MSG .
  data LT_SALIDA_TEMP type T_ZEFI_0026 .
  data FECHA_CIERRE type DATUM .
  data LT_MOD_SAVE type T_MODLOG .
  data LV_DOCUMENTO_MENSAJE type CHAR200 .
  constants CO_DIASFECHAC type CHAR10 value 'ZDTE_FECHA' ##NO_TEXT.
  data DIAS type ZDTE_A_EVALUATION_PERIOD value 365 ##NO_TEXT.

  methods VALIDA_FECHAC
    returning
      value(LS_CHECK) type SUBRC .
  methods CONV
    importing
      !LS_WAERS type WAERS
      !LS_MONTO type DMBTR
    returning
      value(LS_AMOUNT) type BAPICURR_D .
  methods VALIDA_DIAS
    importing
      !LS_SALIDA type ZEFI_0026
    exporting
      !CHECK type SY-SUBRC
      !DOC_NEW type ABAP_BOOL .
  methods VALIDA_ESTAT
    importing
      !LS_SALIDA type ZEFI_0026
    returning
      value(LS_CHECK) type SY-SUBRC .
  methods MOD_FECHA_CIERRE
    exporting
      !LT_MOD type T_MODLOG .
  methods CONSTRUCTOR
    importing
      !FECHA_C type DATUM .
  methods DISPLAY_LOG .
  methods MM_MIRA1
    importing
      !LS_SALIDA type ZEFI_0026 .
  methods MM_MIRA2
    importing
      !LS_SALIDA type ZEFI_0026 .
  methods VALIDA_TIPODOCUMENTO
    importing
      !LS_SALIDA type ZEFI_0026
    returning
      value(LS_CHECK) type SY-SUBRC .
  methods VALIDA_DOCUMENTO
    importing
      !LS_SALIDA type ZEFI_0026
    returning
      value(LS_CHECK) type SY-SUBRC .
  methods VALIDA_CONTADO
    importing
      !LS_SALIDA type ZEFI_0026
    returning
      value(LS_CHECK) type SY-SUBRC .
  methods VALIDA_MESC
    importing
      value(LS_SALIDA) type ZEFI_0026
    returning
      value(LS_CHECK) type SY-SUBRC .
  methods VALIDA_OC
    importing
      !LS_SALIDA type ZEFI_0026
    returning
      value(LS_CHECK) type SY-SUBRC .
  methods BUSCARDATOS
    importing
      value(LS_BUKRS) type R_BUKRS optional
      value(LS_LIFNR) type R_LIFNR optional
      value(LS_BLDAT) type R_BLDAT optional
      value(LS_FECHABASE) type R_FECB optional
      value(LS_FECHASII) type R_FSII optional
      value(LS_BELNR) type R_DOC optional
      value(LS_GJAHR) type R_ANNO optional
      value(LS_STATUS) type R_STAT optional .
  methods MUEVE_VALOR
    importing
      value(LT_ENTRA) type T_ZEFI_0026 optional .
  methods LLENA_CABECERA
    importing
      !LS_SALIDA type ZEFI_0026
      !LS_VBKPF type FVBKPF
    exporting
      !LS_DOC type BAPIACHE09 .
  methods LLENA_POSICION
    importing
      !LS_SALIDA type ZEFI_0026
      !LT_BSEG type T_BSEG
      !LS_VBKPF type FVBKPF
      !LT_BSET type T_BSET
    exporting
      !LT_ACP type T_BAPIACAP09
      !LT_CURRENCY type T_BAPIACCR09
      !LT_ACCOUNT type T_BAPIACGL09
      !LT_ACCOUNTAX type T_BAPIACTX09 .
  methods BAPI_FICREA
    importing
      !LS_SALIDA type ZEFI_0026 .
  methods BAPI_FIMOD
    importing
      !LS_SALIDA type ZEFI_0026
    exporting
      !C_ERROR type ABAP_BOOL .
  methods BAPI_MM
    importing
      !LS_SALIDA type ZEFI_0026
      !DOC_NEW type ABAP_BOOL .
  methods BLOQUEO
    importing
      !LT_ZCB_RECFACTPROV type ZEFI_0026
    returning
      value(ESTADO) type ABAP_BOOL .
  methods DESBLOQUEO
    importing
      !LT_ZCB_RECFACTPROV type ZEFI_0026
      !TABLA type RSTABLE-TABNAME .
  class-methods BORRARDOCFI
    importing
      value(S_BUKRS) type BUKRS
      value(S_BELNR) type BELNR_D
      value(S_GJAHR) type GJAHR
    exporting
      value(S_CHECK) type SY-SUBRC
      value(LT_MENSAJE) type TAB_BDCMSGCOLL .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DTE_FECHAC IMPLEMENTATION.


  METHOD bapi_ficrea.
    DATA: ti_vbkpf TYPE STANDARD TABLE OF fvbkpf,
          ti_vbsec TYPE  STANDARD TABLE OF fvbsec,
          ti_vbseg TYPE  STANDARD TABLE OF fvbseg,
          ti_vbset TYPE  STANDARD TABLE OF fvbset.
    DATA: ls_documentheader TYPE  bapiache09,
          lv_obj_key        TYPE bapiache09-obj_key,
          ls_customercpd    TYPE  bapiacpa09,
          lt_accountgl      TYPE STANDARD TABLE OF bapiacgl09,
          ls_accountgl      TYPE bapiacgl09,
          lt_accountpayable TYPE STANDARD TABLE OF bapiacap09,
          ls_accountpayable TYPE  bapiacap09,
          lt_accounttax     TYPE STANDARD TABLE OF bapiactx09,
          ls_accounttax     TYPE bapiactx09,
          lt_currencyamount TYPE STANDARD TABLE OF bapiaccr09,
          ls_currencyamount TYPE bapiaccr09,
          lt_return         TYPE STANDARD TABLE OF bapiret2,
          ls_return         TYPE bapiret2,
          entrada           TYPE ztfi_0074,
          lv_mensaje        TYPE char200,
          ls_mod_save       TYPE ty_mod,
          lv_belnr          TYPE belnr_d,
          lv_bukrs          TYPE bukrs,
          lv_gjahr          TYPE gjahr.
    DATA: objkey        TYPE cdhdr-objectid,
          old_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074,
          old_wa        TYPE ztfi_0074,
          new_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074,
          new_wa        TYPE ztfi_0074.
    DATA: s_check    TYPE sy-subrc,
          ls_men     TYPE bdcmsgcoll,
          lt_mensaje TYPE tab_bdcmsgcoll.

    CLEAR old_ztfi_0074[].
    CLEAR old_wa.
    CLEAR new_ztfi_0074[].
    CLEAR new_wa.

    MOVE-CORRESPONDING ls_salida TO entrada.

    CALL FUNCTION 'PRELIMINARY_POSTING_DOC_READ'
      EXPORTING
        belnr                   = ls_salida-belnr
        bukrs                   = ls_salida-bukrs
        gjahr                   = ls_salida-gjahr
      TABLES
        t_vbkpf                 = ti_vbkpf[]
        t_vbsec                 = ti_vbsec[]
        t_vbseg                 = ti_vbseg[]
        t_vbset                 = ti_vbset[]
*       T_VACSPLT               =
*       T_VSPLTWT               =
      EXCEPTIONS
        document_line_not_found = 1
        document_not_found      = 2
        input_incomplete        = 3
        OTHERS                  = 4.
    IF sy-subrc <> 0.

    ELSE.
      "--->llena_cabecera
      READ TABLE  ti_vbkpf ASSIGNING FIELD-SYMBOL(<fk>) WITH KEY belnr = ls_salida-belnr.
      me->llena_cabecera( EXPORTING ls_salida = ls_salida
                                    ls_vbkpf  =  <fk>
                           IMPORTING ls_doc    = ls_documentheader
                          ).


      "--->llena_posicion
      me->llena_posicion(  EXPORTING ls_salida = ls_salida
                                     lt_bseg   =  ti_vbseg
                                     lt_bset  = ti_vbset
                                     ls_vbkpf =  <fk>
                           IMPORTING lt_acp    = lt_accountpayable
                                     lt_account =  lt_accountgl
                                     lt_currency = lt_currencyamount
                                     lt_accountax = lt_accounttax ).
      "---> LLamar Bapi
      CLEAR lv_obj_key.
      CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
        EXPORTING
          documentheader = ls_documentheader
*         CUSTOMERCPD    =
*         CONTRACTHEADER =
        IMPORTING
*         OBJ_TYPE       =
          obj_key        = lv_obj_key
*         OBJ_SYS        =
        TABLES
          accountgl      = lt_accountgl
*         ACCOUNTRECEIVABLE       =
          accountpayable = lt_accountpayable
          accounttax     = lt_accounttax
          currencyamount = lt_currencyamount
*         CRITERIA       =
*         VALUEFIELD     =
*         EXTENSION1     =
          return         = lt_return
*         PAYMENTCARD    =
*         CONTRACTITEM   =
*         extension2     = lt_extension2
*         REALESTATE     =
*         ACCOUNTWT      =
        EXCEPTIONS
          error_message  = 1.

      READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
      IF sy-subrc = 0.
        LOOP AT lt_return INTO ls_return.
          IF ls_return-type = 'E'.
**//.. Llenar Log
            CONCATENATE TEXT-406 ls_return-message INTO lv_mensaje.
            ls_mod_save-icono     = icon_red_light.
            ls_mod_save-documento = lv_documento_mensaje.
            ls_mod_save-mensaje   = lv_mensaje.
            APPEND ls_mod_save TO lt_mod_save.

            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'E'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '007'.
            gs_bal_msg-msgv1 = lv_mensaje.
            go_logc->add_msg( i_s_msg = gs_bal_msg ).
            go_logc->save( ).

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

          MOVE-CORRESPONDING entrada TO old_wa.
          new_wa = old_wa.
          new_wa-belnr =  lv_belnr.
          new_wa-gjahr = lv_gjahr.
          APPEND new_wa TO new_ztfi_0074.
          APPEND old_wa TO old_ztfi_0074.
          objkey(3) = sy-mandt.
          objkey+3(4) = entrada-bukrs.
          objkey+7(16) = entrada-xblnr.
          objkey+23(10) = entrada-lifnr.
          objkey+33(3) = entrada-tipodte.

          CALL FUNCTION 'ZTFI_0074_WRITE_DOCUMENT'
            IN UPDATE TASK
            EXPORTING
              objectid      = objkey
              tcode         = 'ZDTE_00'
              utime         = sy-uzeit
              udate         = sy-datum
              username      = sy-uname
              upd_ztfi_0074 = 'U'
            TABLES
              xztfi_0074    = new_ztfi_0074
              yztfi_0074    = old_ztfi_0074.

          MODIFY ztfi_0074 FROM  new_wa.
          COMMIT WORK AND WAIT.
**//.. Llenar Log

          CONCATENATE TEXT-407 lv_belnr '/' lv_gjahr INTO lv_mensaje.
          ls_mod_save-icono     = icon_green_light.
          ls_mod_save-documento = lv_documento_mensaje.
          ls_mod_save-mensaje   = lv_mensaje.
          APPEND ls_mod_save TO lt_mod_save.

          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'S'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '007'.
          gs_bal_msg-msgv1 = lv_mensaje.
          go_logc->add_msg( i_s_msg = gs_bal_msg ).
          go_logc->save( ).
**//.. Borro doc.Precontable anterior.
          REFRESH lt_mensaje.
          CALL METHOD zcl_dte_fechac=>borrardocfi
            EXPORTING
              s_bukrs    = ls_salida-bukrs
              s_belnr    = ls_salida-belnr
              s_gjahr    = ls_salida-gjahr
            IMPORTING
              s_check    = s_check
              lt_mensaje = lt_mensaje.

          CLEAR ls_men.
          LOOP AT lt_mensaje INTO ls_men.
            CLEAR lv_mensaje.
            MESSAGE ID ls_men-msgid TYPE ls_men-msgtyp NUMBER  ls_men-msgnr
                 WITH ls_men-msgv1 ls_men-msgv2 ls_men-msgv3 ls_men-msgv4 INTO lv_mensaje.

**//.. Llenar Log
            IF ls_men-msgtyp = 'S'.
              ls_mod_save-icono     = icon_green_light.
            ELSE.
              ls_mod_save-icono     = icon_yellow_light.
            ENDIF.
            ls_mod_save-documento = lv_documento_mensaje.
            ls_mod_save-mensaje   = lv_mensaje.
            APPEND ls_mod_save TO lt_mod_save.

            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = ls_men-msgtyp.
            gs_bal_msg-msgid = ls_men-msgid.
            gs_bal_msg-msgno = ls_men-msgnr.
            gs_bal_msg-msgv1 = ls_men-msgv1.
            gs_bal_msg-msgv2 = ls_men-msgv2.
            gs_bal_msg-msgv3 = ls_men-msgv3.
            gs_bal_msg-msgv4 = ls_men-msgv4.
            go_logc->add_msg( i_s_msg = gs_bal_msg ).
            go_logc->save( ).

          ENDLOOP.

        ENDIF.

      ENDIF.


    ENDIF.

  ENDMETHOD.


  METHOD bapi_fimod.
    DATA: ls_vbkpf    TYPE vbkpf,
          lt_return   TYPE bapiret2_t,
          lv_fecha    TYPE char10,
          lv_mensaje  TYPE char200,
          ls_mod_save TYPE ty_mod.

    CLEAR:      c_error.



**//.. Buscar info factura preliminar recibida
    CLEAR: ls_vbkpf.

    SELECT SINGLE * INTO ls_vbkpf
    FROM vbkpf
    WHERE bukrs EQ ls_salida-bukrs
      AND belnr EQ ls_salida-belnr
      AND gjahr EQ ls_salida-gjahr.
    IF sy-subrc EQ 0.
      CHECK ls_vbkpf-budat NE fecha_cierre.

**//.. Validar estatus
      CASE ls_vbkpf-bstat.
        WHEN 'V'.
          DATA: lt_bdcdata TYPE TABLE OF bdcdata,
                ls_opt     TYPE ctu_params,
                lt_mensaje TYPE TABLE OF bdcmsgcoll,
                ls_mensaje TYPE bdcmsgcoll.
          WRITE fecha_cierre TO lv_fecha.
**//..
          lt_bdcdata = VALUE #( ( program  = 'SAPMF05V'    dynpro = '0100' dynbegin = 'X' )
                                ( fnam     = 'BDC_CURSOR'  fval = 'RF05V-BUKRS' )
                                ( fnam     = 'RF05V-BUKRS' fval = ls_vbkpf-bukrs )
                                ( fnam     = 'RF05V-BELNR' fval = ls_vbkpf-belnr )
                                ( fnam     = 'RF05V-GJAHR' fval = ls_vbkpf-gjahr )
                                ( fnam     = 'BDC_OKCODE'  fval = '/00' )
                                ( program  = 'SAPLF040'    dynpro = '0700' dynbegin = 'X' )
                                ( fnam     = 'BDC_OKCODE'  fval = '=BK' )
                                ( program  = 'SAPLF040'    dynpro = '0600' dynbegin = 'X' )
                                ( fnam     = 'BKPF-BUDAT'  fval = lv_fecha )
                                ( fnam     = 'BDC_OKCODE'  fval = '=AB' )
                                ( program  = 'SAPLF040'    dynpro = '0700' dynbegin = 'X' )
                                ( fnam     = 'BDC_OKCODE'  fval = '=BP' )
                               ).


          ls_opt-dismode = 'N'.

          TRY.
              CALL TRANSACTION 'FBV0' USING lt_bdcdata OPTIONS FROM ls_opt
                                      MESSAGES INTO lt_mensaje.
            CATCH cx_sy_authorization_error ##NO_HANDLER.
          ENDTRY.

          LOOP AT lt_mensaje INTO ls_mensaje.
            IF  ls_mensaje-msgid EQ 'FP'
            AND ls_mensaje-msgnr EQ '099'.
              c_error = abap_true.
              CLEAR: lv_mensaje,
               ls_mod_save.
              MESSAGE ID ls_mensaje-msgid TYPE 'I'
                                          NUMBER ls_mensaje-msgnr
                                          WITH ls_mensaje-msgv1
                                               ls_mensaje-msgv2
                                               ls_mensaje-msgv3
                                               ls_mensaje-msgv4
                                          INTO lv_mensaje.

              ls_mod_save-icono     = icon_red_light.
              ls_mod_save-documento = lv_documento_mensaje.
              ls_mod_save-mensaje   = lv_mensaje.
              APPEND ls_mod_save TO lt_mod_save.

              CLEAR gs_bal_msg.
              gs_bal_msg-msgty = 'E'.
              gs_bal_msg-msgid = 'ZDTE_0001'.
              gs_bal_msg-msgno = '012'.
              gs_bal_msg-msgv1 = TEXT-216.
              gs_bal_msg-msgv2 = lv_documento_mensaje.
              gs_bal_msg-msgv3 = lv_mensaje.
              go_logc->add_msg( i_s_msg = gs_bal_msg ).
              go_logc->save( ).

              EXIT.
            ENDIF.

            IF ls_mensaje-msgtyp CA 'EA'.
              c_error = abap_true.
              CLEAR: lv_mensaje,
              ls_mod_save.
              MESSAGE ID ls_mensaje-msgid TYPE 'E'
                                          NUMBER ls_mensaje-msgnr
                                          WITH ls_mensaje-msgv1
                                               ls_mensaje-msgv2
                                               ls_mensaje-msgv3
                                               ls_mensaje-msgv4
                                          INTO lv_mensaje.

              ls_mod_save-icono     = icon_red_light.
              ls_mod_save-documento = lv_documento_mensaje.
              ls_mod_save-mensaje   = lv_mensaje.
              APPEND ls_mod_save TO lt_mod_save.

              CLEAR gs_bal_msg.
              gs_bal_msg-msgty = 'E'.
              gs_bal_msg-msgid = 'ZDTE_0001'.
              gs_bal_msg-msgno = '012'.
              gs_bal_msg-msgv1 = TEXT-216.
              gs_bal_msg-msgv2 = lv_documento_mensaje.
              gs_bal_msg-msgv3 = lv_mensaje.
              go_logc->add_msg( i_s_msg = gs_bal_msg ).
              go_logc->save( ).
              EXIT.
            ENDIF.
          ENDLOOP.
        WHEN OTHERS.
      ENDCASE.

      IF c_error EQ abap_false.
        CLEAR: lv_mensaje,
        ls_mod_save.
        CONCATENATE lv_documento_mensaje TEXT-408 INTO
         lv_mensaje SEPARATED BY space.
        ls_mod_save-icono     = icon_green_light.
        ls_mod_save-documento = lv_documento_mensaje.
        ls_mod_save-mensaje   = lv_mensaje.
        APPEND ls_mod_save TO lt_mod_save.

        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'S'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '012'.
        gs_bal_msg-msgv1 = TEXT-216.
        gs_bal_msg-msgv2 = lv_documento_mensaje.
        gs_bal_msg-msgv3 = TEXT-408.
        go_logc->add_msg( i_s_msg = gs_bal_msg ).
        go_logc->save( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD bapi_mm.
    DATA: ls_docnew   TYPE bapi_incinv_fld-inv_doc_no,
          ls_yearnew  TYPE bapi_incinv_fld-fisc_year,
          ls_headata  TYPE bapi_incinv_chng_header,
          ls_headatax TYPE bapi_incinv_chng_headerx,
          lt_return   TYPE STANDARD TABLE OF bapiret2,
          wa_return   TYPE bapiret2,
          entrada     TYPE ztfi_0074,
          lv_mensaje  TYPE char200,
          lv_texto    TYPE char80,
          ls_mod_save TYPE ty_mod.
    DATA objkey TYPE cdhdr-objectid.
    DATA: old_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074.
    DATA: old_wa TYPE ztfi_0074.
    CLEAR old_ztfi_0074[].
    CLEAR old_wa.
    DATA: new_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074.
    DATA: new_wa TYPE ztfi_0074.
    CLEAR new_ztfi_0074[].
    CLEAR new_wa.CLEAR lv_texto.



    ls_headata-pstng_date = fecha_cierre.
    ls_headatax-pstng_date = 'X'.
    MOVE-CORRESPONDING ls_salida TO entrada.


    CALL FUNCTION 'BAPI_INCOMINGINVOICE_CHANGE'
      EXPORTING
        invoicedocnumber     = ls_salida-belnr
        fiscalyear           = ls_salida-gjahr
        invoice_doc_status   = space
*       TABLE_CHANGE         =
        headerdata_change    = ls_headata
        headerdata_changex   = ls_headatax
*       ADRESSDATA_CHANGE    =
*       ADRESSDATA_CHANGEX   =
      IMPORTING
        invoicedocnumber_new = ls_docnew
        fiscalyear_new       = ls_yearnew
      TABLES
*       ITEMDATA             =
*       ACCOUNTINGDATA       =
*       GLACCOUNTDATA        =
*       MATERIALDATA         =
*       TAXDATA              =
*       WITHTAXDATA          =
*       VENDORITEMSPLITDATA  =
        return               = lt_return
*       EXTENSIONIN          =
*       TM_ITEMDATA          =
      .

    "---> Grabar / Tablas Grabar Modificaciones
    CLEAR: lv_mensaje,
        ls_mod_save.

    READ TABLE lt_return INTO wa_return WITH KEY type = 'E'.
    IF sy-subrc NE 0.
      READ TABLE lt_return INTO wa_return WITH KEY type = 'A'.
    ENDIF.
    IF sy-subrc = 0.

      READ TABLE lt_return INTO wa_return WITH KEY type = 'E'
                                                number = '719'
                                                   id = 'M8'.

      IF sy-subrc = 0.
        IF doc_new EQ abap_false.
          me->mm_mira1( EXPORTING ls_salida = ls_salida ).
          EXIT.
        ELSE.
          me->mm_mira2( EXPORTING ls_salida = ls_salida ).
          EXIT.

        ENDIF.

      ENDIF.

      me->desbloqueo( EXPORTING lt_zcb_recfactprov = ls_salida tabla = 'RBKP' ).
      ls_mod_save-icono     = icon_red_light.
      CONCATENATE lv_documento_mensaje
               wa_return-message INTO lv_mensaje SEPARATED BY space.
      ls_mod_save-documento = lv_documento_mensaje.
      ls_mod_save-mensaje   = lv_mensaje.
      APPEND ls_mod_save TO lt_mod_save.

      CLEAR gs_bal_msg.
      DATA(s_len) = strlen( wa_return-message ).
      DATA(cont) = s_len / 2.
      gs_bal_msg-msgty = 'E'.
      gs_bal_msg-msgid = 'ZDTE_0001'.
      gs_bal_msg-msgno = '012'.
      gs_bal_msg-msgv1 = TEXT-216.
      gs_bal_msg-msgv2 = lv_documento_mensaje.
      gs_bal_msg-msgv3 = wa_return-message(cont).
      gs_bal_msg-msgv4 = wa_return-message+cont(cont).
      go_logc->add_msg( i_s_msg = gs_bal_msg ).
      go_logc->save( ).


    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.



      IF NOT  ls_docnew IS INITIAL.

        MOVE-CORRESPONDING entrada TO old_wa.
        new_wa = old_wa.
        new_wa-belnr =  ls_docnew.
        new_wa-gjahr = ls_yearnew.
        APPEND new_wa TO new_ztfi_0074.
        APPEND old_wa TO old_ztfi_0074.
        objkey(3) = sy-mandt.
        objkey+3(4) = entrada-bukrs.
        objkey+7(16) = entrada-xblnr.
        objkey+23(10) = entrada-lifnr.
        objkey+33(3) = entrada-tipodte.

        CALL FUNCTION 'ZTFI_0074_WRITE_DOCUMENT'
          IN UPDATE TASK
          EXPORTING
            objectid      = objkey
            tcode         = 'ZDTE_00'
            utime         = sy-uzeit
            udate         = sy-datum
            username      = sy-uname
            upd_ztfi_0074 = 'U'
          TABLES
            xztfi_0074    = new_ztfi_0074
            yztfi_0074    = old_ztfi_0074.

        MODIFY ztfi_0074 FROM  new_wa.
        COMMIT WORK AND WAIT.
        ls_mod_save-icono     = icon_green_light.
        CONCATENATE TEXT-407 ls_docnew '/' ls_yearnew
                    INTO lv_mensaje SEPARATED BY space.
        lv_texto = TEXT-407.

      ELSE.
        ls_mod_save-icono     = icon_green_light.
        CONCATENATE lv_documento_mensaje
                 TEXT-408 INTO lv_mensaje SEPARATED BY space.
        lv_texto = TEXT-408.

      ENDIF.
      ls_mod_save-documento = lv_documento_mensaje.
      ls_mod_save-mensaje   = lv_mensaje.
      APPEND ls_mod_save TO lt_mod_save.

      CLEAR gs_bal_msg.
      gs_bal_msg-msgty = 'S'.
      gs_bal_msg-msgid = 'ZDTE_0001'.
      gs_bal_msg-msgno = '012'.
      gs_bal_msg-msgv1 = lv_mensaje.
      gs_bal_msg-msgv1 = TEXT-216.
      gs_bal_msg-msgv2 = lv_documento_mensaje.
      gs_bal_msg-msgv3 = lv_texto.
      go_logc->add_msg( i_s_msg = gs_bal_msg ).
      go_logc->save( ).

    ENDIF.






  ENDMETHOD.


  METHOD bloqueo.
    DATA:
      lv_mensaje  TYPE char200,
      ls_mod_save TYPE ty_mod.
    CALL FUNCTION 'ENQUEUE_EZTFI_0074'
      EXPORTING
        mode_ztfi_0074 = 'E'
        mandt          = sy-mandt
        bukrs          = lt_zcb_recfactprov-bukrs
        xblnr          = lt_zcb_recfactprov-xblnr
        lifnr          = lt_zcb_recfactprov-lifnr
        tipodte        = lt_zcb_recfactprov-tipodte
*       X_BUKRS        = ' '
*       X_XBLNR        = ' '
*       X_LIFNR        = ' '
*       X_TIPODTE      = ' '
*       _SCOPE         = '2'
*       _WAIT          = ' '
*       _COLLECT       = ' '
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc NE 0.
      estado = 1.
    ELSE.
      estado = 0.
    ENDIF.
    IF estado NE 0.
      CLEAR: lv_mensaje,
          ls_mod_save.
      CONCATENATE TEXT-216 lv_documento_mensaje
                  TEXT-222 INTO lv_mensaje SEPARATED BY space.

      ls_mod_save-icono     = icon_red_light.
      ls_mod_save-documento = lv_documento_mensaje.
      ls_mod_save-mensaje   = lv_mensaje.
      APPEND ls_mod_save TO lt_mod_save.

      CLEAR gs_bal_msg.
      gs_bal_msg-msgty = 'E'.
      gs_bal_msg-msgid = 'ZDTE_0003'.
      gs_bal_msg-msgno = '012'.
      gs_bal_msg-msgv1 = TEXT-216.
      gs_bal_msg-msgv2 = lv_documento_mensaje.
      gs_bal_msg-msgv3 = TEXT-222.

      go_logc->add_msg( i_s_msg = gs_bal_msg ).
      go_logc->save( ).
    ENDIF.

  ENDMETHOD.


  METHOD borrardocfi.

    DATA: lt_bdcdata TYPE TABLE OF bdcdata,
          lv_fnam    TYPE bdcdata-fnam,
          ls_opt     TYPE ctu_params,
          ls_mensaje TYPE bdcmsgcoll.

    s_check = 0.
    lt_bdcdata = VALUE #( BASE lt_bdcdata ( program  = 'SAPMF05V' dynpro = '0100' dynbegin = 'X' )
    ( fnam = 'BDC_CURSOR'   fval = 'RF05V-GJAHR' )
    ( fnam =  'BDC_OKCODE' fval = '/00' )
    ( fnam = 'RF05V-BUKRS' fval = s_bukrs )
    ( fnam = 'RF05V-BELNR' fval = s_belnr )
    ( fnam = 'RF05V-GJAHR' fval = s_gjahr )
    ).

    lt_bdcdata = VALUE #( BASE lt_bdcdata ( program  = 'SAPLF040' dynpro = '0700' dynbegin = 'X'  )
    ( fnam = 'BDC_CURSOR'   fval = 'RF05V-ANZDT(01)' )
    ( fnam =  'BDC_OKCODE' fval = '=BL' )
    ).

    lt_bdcdata = VALUE #( BASE lt_bdcdata ( program  = 'SAPLSPO1' dynpro = '0200' dynbegin = 'X' )
    ( fnam =  'BDC_OKCODE' fval = '=YES' )
    ).
    ls_opt-dismode = 'N'.
    TRY.
        CALL TRANSACTION 'FBV0' USING lt_bdcdata OPTIONS FROM ls_opt
                                MESSAGES INTO lt_mensaje.
      CATCH cx_sy_authorization_error ##NO_HANDLER.
    ENDTRY.

    READ TABLE lt_mensaje  TRANSPORTING  NO FIELDS WITH KEY  msgtyp = 'E'.
    IF sy-subrc EQ 0.
      s_check = '4'.
    ENDIF.

    READ TABLE lt_mensaje  TRANSPORTING  NO FIELDS WITH KEY  msgtyp = 'A'.
    IF sy-subrc EQ 0.
      s_check = '4'.
    ENDIF.
  ENDMETHOD.


  METHOD buscardatos.
    DATA: lt_salida TYPE STANDARD TABLE OF  zefi_0026.
    CLEAR lt_salida . REFRESH lt_salida.

    SELECT * FROM zcds_0074u( p_dia = @sy-datum , P_TOTDIAS = @me->dias )
 INTO CORRESPONDING FIELDS OF TABLE @lt_salida
    WHERE bukrs  IN @ls_bukrs
      AND lifnr  IN @ls_lifnr
      AND bldat  IN @ls_bldat
      AND fechabase IN @ls_fechabase
      AND zfecharsii IN @ls_fechasii
      AND belnr  IN @ls_belnr
      AND gjahr  IN @ls_gjahr
      AND status IN @ls_status.

    me->mueve_valor( EXPORTING lt_entra = lt_salida[] ).

  ENDMETHOD.


  method CONSTRUCTOR.

  fecha_cierre = fecha_c.
  endmethod.


  METHOD conv.
    CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
      EXPORTING
        currency        = ls_waers
        amount_internal = ls_monto
      IMPORTING
        amount_external = ls_amount.
  ENDMETHOD.


  METHOD desbloqueo.

    IF tabla EQ 'ZTFI_0074'.
      CALL FUNCTION 'DEQUEUE_EZTFI_0074'
        EXPORTING
          mode_ztfi_0074 = 'E'
          mandt          = sy-mandt
          bukrs          = lt_zcb_recfactprov-bukrs
          xblnr          = lt_zcb_recfactprov-xblnr
          lifnr          = lt_zcb_recfactprov-lifnr
          tipodte        = lt_zcb_recfactprov-tipodte
*         X_BUKRS        = ' '
*         X_XBLNR        = ' '
*         X_LIFNR        = ' '
*         X_TIPODTE      = ' '
*         _SCOPE         = '3'
*         _SYNCHRON      = ' '
*         _COLLECT       = ' '
        .

    ELSEIF  tabla EQ 'RBKP'.

      CALL FUNCTION 'DEQUEUE_E_RBKP'
        EXPORTING
          mode_rbkp = 'E'
          mandt     = lt_zcb_recfactprov-mandt
          belnr     = lt_zcb_recfactprov-belnr
          gjahr     = lt_zcb_recfactprov-gjahr
*         X_BELNR   = ' '
*         X_GJAHR   = ' '
*         _SCOPE    = '3'
*         _SYNCHRON = ' '
*         _COLLECT  = ' '
        .

    ENDIF.
  ENDMETHOD.


  METHOD display_log.

    go_logc->display( ).

  ENDMETHOD.


  METHOD llena_cabecera.

    "--> Asignación de datos de bapi.
    ls_doc-comp_code  = ls_vbkpf-bukrs.
    ls_doc-doc_date   = ls_vbkpf-bldat.
    ls_doc-pstng_date = fecha_cierre.
    ls_doc-trans_date = ls_vbkpf-bldat.
    ls_doc-doc_type   = ls_vbkpf-blart.
    ls_doc-ref_doc_no = ls_vbkpf-xblnr.
    ls_doc-header_txt = ls_vbkpf-bktxt.
    ls_doc-username   = sy-uname.
    ls_doc-bus_act    = 'RFBV'.
    ls_doc-doc_status = '2'. "parcial
*    IF ls_salida-status = '2'. " si tiene ztfi_0074 tienes estatus no validado.
*      ls_doc-doc_status = '2'. "parcial
*    ELSE.
*      ls_doc-doc_status = '3'. "completo
*    ENDIF.

  ENDMETHOD.


  METHOD llena_posicion.
    DATA: ls_accountpayable TYPE bapiacap09,
          ls_currencyamount TYPE bapiaccr09,
          ls_accountgl      TYPE bapiacgl09,
          ls_accounttax     TYPE bapiactx09,
          lv_amount         TYPE bapicurr_d,
          lv_fact_k         TYPE p DECIMALS 2 VALUE 1,
          lv_fact_s         TYPE p DECIMALS 2 VALUE 1.

    LOOP AT lt_bseg ASSIGNING FIELD-SYMBOL(<fa>).
      IF <fa>-shkzg = 'H'.
        lv_fact_k = -1.
        lv_fact_s = -1.
      ELSE.
        lv_fact_k = 1.
        lv_fact_s = 1.
      ENDIF.

      ADD  1 TO lv_itemacc.

      IF <fa>-koart EQ 'K'.
        CLEAR ls_accountpayable.
        ls_accountpayable-itemno_acc = lv_itemacc."'0000000001'.
        ls_accountpayable-vendor_no  = <fa>-lifnr.
        ls_accountpayable-bline_date = <fa>-zfbdt.
        ls_accountpayable-pmnttrms   = <fa>-zterm.
        ls_accountpayable-alloc_nmbr = <fa>-zuonr.
        ls_accountpayable-item_text  = <fa>-sgtxt.
        ls_accountpayable-tax_code   = <fa>-mwskz.
        ls_accountpayable-gl_account = <fa>-hkont .
        APPEND ls_accountpayable TO lt_acp.

        CLEAR ls_currencyamount.
        ls_currencyamount-itemno_acc = lv_itemacc.
        ls_currencyamount-currency = ls_vbkpf-waers.

        CLEAR lv_amount.
        lv_amount = me->conv( EXPORTING ls_waers = ls_vbkpf-waers
                             ls_monto = <fa>-wrbtr ).
        ls_currencyamount-amt_doccur =  lv_amount * lv_fact_k.

        IF ls_vbkpf-xmwst = ' '.
          CLEAR lv_amount.
          lv_amount = me->conv( EXPORTING ls_waers = ls_vbkpf-waers
                         ls_monto = <fa>-wmwst ).
          ls_currencyamount-tax_amt =  lv_amount * lv_fact_k.
          APPEND ls_currencyamount TO lt_currency.
        ENDIF.
      ELSEIF  <fa>-koart EQ 'S'.
        CLEAR ls_accountgl.
        ls_accountgl-itemno_acc = lv_itemacc.
        ls_accountgl-gl_account =  <fa>-hkont.
        ls_accountgl-costcenter = <fa>-kostl.
        ls_accountgl-tax_code = <fa>-mwskz.
        ls_accountgl-alloc_nmbr = <fa>-zuonr.
        ls_accountgl-item_text = <fa>-sgtxt.
        APPEND ls_accountgl TO lt_account.

        CLEAR ls_currencyamount.
        ls_currencyamount-itemno_acc = lv_itemacc.
        ls_currencyamount-currency = ls_vbkpf-waers.
        CLEAR lv_amount.
        lv_amount = me->conv( EXPORTING ls_waers = ls_vbkpf-waers
                          ls_monto = <fa>-wrbtr ).
        ls_currencyamount-amt_doccur =  lv_amount * lv_fact_s.
        APPEND ls_currencyamount TO lt_currency.

      ENDIF.
      IF lines( lt_bset ) GT 0.
        LOOP AT lt_bset ASSIGNING FIELD-SYMBOL(<iva>).
          CLEAR ls_accounttax.
          ADD  1 TO lv_itemacc.

          ls_accounttax-itemno_acc = lv_itemacc. "'0000000002'.
          ls_accounttax-acct_key = 'VST'.
          ls_accounttax-tax_code =  <iva>-mwskz. "'C1'.

          APPEND ls_accounttax TO lt_accountax.

          CLEAR ls_currencyamount.
          ls_currencyamount-itemno_acc = lv_itemacc.
          ls_currencyamount-currency = ls_vbkpf-waers.

          CLEAR lv_amount.
          lv_amount = me->conv( EXPORTING ls_waers = ls_vbkpf-waers
                            ls_monto = <iva>-fwste ).
          ls_currencyamount-amt_doccur =  lv_amount * lv_fact_s.

          IF <iva>-fwste GT 0. "iva
            CLEAR lv_amount.
            lv_amount = me->conv( EXPORTING ls_waers = ls_vbkpf-waers
                             ls_monto = ls_salida-mntneto ).

            ls_currencyamount-amt_base = lv_amount * lv_fact_s. "BaseImpuesto
            APPEND ls_currencyamount TO lt_currency.
          ELSE.
            CLEAR lv_amount.
            lv_amount = me->conv( EXPORTING ls_waers = ls_vbkpf-waers
                             ls_monto = ls_salida-mntexe ).

            ls_currencyamount-amt_base = lv_amount * lv_fact_s. "BaseImpuesto
            APPEND ls_currencyamount TO lt_currency.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD mm_mira1.
    DATA: ls_rbkp     TYPE rbkp,
          c_error     TYPE abap_bool,
          lv_mensaje  TYPE char200,
          ls_mod_save TYPE ty_mod.

*//.. buscar info factura preliminar recibida
    SELECT SINGLE * INTO ls_rbkp
    FROM rbkp
    WHERE belnr EQ ls_salida-belnr
      AND gjahr EQ ls_salida-gjahr.
    IF sy-subrc EQ 0.
      CHECK ls_rbkp-budat NE sy-datum.

**//.. Validar estatus
      CASE ls_rbkp-rbstat.
        WHEN '1'.
          ls_rbkp-budat = fecha_cierre.
          UPDATE rbkp SET budat = ls_rbkp-budat
                       WHERE belnr EQ ls_rbkp-belnr
                         AND gjahr EQ ls_rbkp-gjahr.
          IF sy-subrc EQ 0.
            COMMIT WORK AND WAIT.
            me->desbloqueo( EXPORTING lt_zcb_recfactprov = ls_salida tabla = 'RBKP' ).
          ELSE.
            c_error = 'X'.
          ENDIF.
        WHEN '3'.
          ls_rbkp-budat = fecha_cierre.
          UPDATE rbkp SET budat = ls_rbkp-budat
                       WHERE belnr EQ ls_rbkp-belnr
                         AND gjahr EQ ls_rbkp-gjahr.
          IF sy-subrc EQ 0.
            COMMIT WORK AND WAIT.
            me->desbloqueo( EXPORTING lt_zcb_recfactprov = ls_salida tabla = 'RBKP' ).
          ELSE.
            c_error = 'X'.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.

      IF c_error = 'X'.
        CLEAR: lv_mensaje,
       ls_mod_save.
        ls_mod_save-icono     = icon_red_light.
        CONCATENATE lv_documento_mensaje
                 TEXT-409 INTO lv_mensaje SEPARATED BY space.
        ls_mod_save-documento = lv_documento_mensaje.
        ls_mod_save-mensaje   = lv_mensaje.
        APPEND ls_mod_save TO lt_mod_save.

        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '012'.
        gs_bal_msg-msgv1 = TEXT-216.
        gs_bal_msg-msgv2 = lv_documento_mensaje.
        gs_bal_msg-msgv3 = TEXT-409.
        go_logc->add_msg( i_s_msg = gs_bal_msg ).
        go_logc->save( ).
      ELSE.
        CLEAR: lv_mensaje,
     ls_mod_save.
        ls_mod_save-icono     = icon_green_light.
        CONCATENATE lv_documento_mensaje
         TEXT-408 INTO lv_mensaje SEPARATED BY space.
        ls_mod_save-documento = lv_documento_mensaje.
        ls_mod_save-mensaje   = lv_mensaje.
        APPEND ls_mod_save TO lt_mod_save.

        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'S'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '012'.
        gs_bal_msg-msgv1 = TEXT-216.
        gs_bal_msg-msgv2 = lv_documento_mensaje.
        gs_bal_msg-msgv3 = TEXT-408.
        go_logc->add_msg( i_s_msg = gs_bal_msg ).
        go_logc->save( ).

      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD mm_mira2.
    DATA: entrada     TYPE ztfi_0074,
          c_error     TYPE abap_bool,
          lv_mensaje  TYPE char200,
          ls_mod_save TYPE ty_mod.
    DATA:  ls_rbkpv TYPE mrm_rbkpv.
    DATA objkey TYPE cdhdr-objectid.
    DATA: old_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074.
    DATA: old_wa TYPE ztfi_0074.
    CLEAR old_ztfi_0074[].
    CLEAR old_wa.
    DATA: new_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074.
    DATA: new_wa TYPE ztfi_0074.
    CLEAR new_ztfi_0074[].
    CLEAR new_wa.
    CLEAR c_error.CLEAR  ls_rbkpv.
    CALL FUNCTION 'ZFI_0040DTE'
      EXPORTING
        ls_salida   = ls_salida
      IMPORTING
        ls_salida_m = entrada. "viene el valor nueo

    IF entrada-belnr NE ls_salida-belnr.

      MOVE-CORRESPONDING entrada TO new_wa.
      old_wa = new_wa.
      old_wa-belnr =  ls_salida-belnr.
      old_wa-gjahr = ls_salida-gjahr.
      APPEND new_wa TO new_ztfi_0074.
      APPEND old_wa TO old_ztfi_0074.
      objkey(3) = sy-mandt.
      objkey+3(4) = entrada-bukrs.
      objkey+7(16) = entrada-xblnr.
      objkey+23(10) = entrada-lifnr.
      objkey+33(3) = entrada-tipodte.

      CALL FUNCTION 'ZTFI_0074_WRITE_DOCUMENT'
        IN UPDATE TASK
        EXPORTING
          objectid      = objkey
          tcode         = 'ZDTE_00'
          utime         = sy-uzeit
          udate         = sy-datum
          username      = sy-uname
          upd_ztfi_0074 = 'U'
        TABLES
          xztfi_0074    = new_ztfi_0074
          yztfi_0074    = old_ztfi_0074.

      SELECT * FROM rbkp_v UP TO 1 ROWS
      INTO CORRESPONDING FIELDS OF ls_rbkpv
      WHERE belnr EQ  ls_salida-belnr
        AND gjahr EQ ls_salida-gjahr
        ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF sy-subrc = 0.

        CALL FUNCTION 'MRM_INVOICE_DOCUMENT_DELETE'
          EXPORTING
            i_rbkpv = ls_rbkpv
*           I_LOCK  = ' '
*           TABLES
*           T_DRSEG =
*           CHANGING
*           CT_ERRPROT              =
*           EXCEPTIONS
*           INVOICE_LOCKED          = 1
*           INVOICE_NOT_FOUND       = 2
*           OTHERS  = 3
          .
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.
      ENDIF.
      MODIFY ztfi_0074 FROM  new_wa.
      COMMIT WORK AND WAIT.
    ELSE.
      c_error = 'X'.
    ENDIF.

    IF c_error = abap_false.
      CLEAR: lv_mensaje,
         ls_mod_save.
      ls_mod_save-icono     = icon_green_light.
      CONCATENATE TEXT-407  entrada-belnr '/'  entrada-gjahr
                  INTO lv_mensaje SEPARATED BY space.

      ls_mod_save-documento = lv_documento_mensaje.
      ls_mod_save-mensaje   = lv_mensaje.
      APPEND ls_mod_save TO lt_mod_save.

      CLEAR gs_bal_msg.
      gs_bal_msg-msgty = 'S'.
      gs_bal_msg-msgid = 'ZDTE_0001'.
      gs_bal_msg-msgno = '012'.
      gs_bal_msg-msgv1 = lv_mensaje.
      gs_bal_msg-msgv1 = TEXT-216.
      gs_bal_msg-msgv2 = lv_documento_mensaje.
      gs_bal_msg-msgv3 = TEXT-407.
      go_logc->add_msg( i_s_msg = gs_bal_msg ).
      go_logc->save( ).
    ELSE.
      CLEAR: lv_mensaje,
          ls_mod_save.
      ls_mod_save-icono     = icon_red_light.
      CONCATENATE lv_documento_mensaje
               TEXT-409 INTO lv_mensaje SEPARATED BY space.
      ls_mod_save-documento = lv_documento_mensaje.
      ls_mod_save-mensaje   = lv_mensaje.
      APPEND ls_mod_save TO lt_mod_save.

      CLEAR gs_bal_msg.
      gs_bal_msg-msgty = 'E'.
      gs_bal_msg-msgid = 'ZDTE_0001'.
      gs_bal_msg-msgno = '012'.
      gs_bal_msg-msgv1 = lv_mensaje.
      gs_bal_msg-msgv1 = TEXT-216.
      gs_bal_msg-msgv2 = lv_documento_mensaje.
      gs_bal_msg-msgv3 = TEXT-409.
      go_logc->add_msg( i_s_msg = gs_bal_msg ).
      go_logc->save( ).
    ENDIF.
  ENDMETHOD.


  METHOD mod_fecha_cierre.

    DATA: lt_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074.
    DATA: ls_a TYPE abap_bool.
    DATA: ls_check TYPE sy-subrc.
    "---> Crea Objeto Logs de interfaz.
    CONCATENATE sy-datum sy-uzeit
                INTO gs_bal_log-extnumber.

    gs_bal_log-object     = 'ZDTE'.
    gs_bal_log-subobject  = 'FECHA_CI'.
    gs_bal_log-aldate     = syst-datum.
    gs_bal_log-altime     = syst-uzeit.
    gs_bal_log-aluser     = syst-uname.
    gs_bal_log-alprog     = syst-repid.

    FREE go_logc.
    CREATE OBJECT go_logc
      EXPORTING
        i_s_object = gs_bal_log.

    IF lines( lt_salida_temp ) EQ 0.
      CLEAR lv_documento_mensaje.
      CLEAR gs_bal_msg.
      gs_bal_msg-msgty = 'W'.
      gs_bal_msg-msgid = 'ZDTE_0001'.
      gs_bal_msg-msgno = '007'.
      gs_bal_msg-msgv1 = TEXT-415.
      go_logc->add_msg( i_s_msg = gs_bal_msg ).
      go_logc->save( ).
      EXIT.
    ENDIF.

   "---> Valido fecha de cierre/nueva contb. sea 1er dia de mes
    ls_check = me->valida_fechac( ).
    IF ls_check NE 0.
      lt_mod[] = lt_mod_save[].
    ENDIF.
    CHECK ls_check = 0.

    LOOP AT lt_salida_temp ASSIGNING FIELD-SYMBOL(<f1>).
      CLEAR ls_a. CLEAR ls_check.
      CLEAR lv_documento_mensaje.
      CONCATENATE <f1>-bukrs '/' <f1>-xblnr INTO lv_documento_mensaje.


      "---> Valido Mes fecha actual contb. NE a MES fecha de cierre.
      ls_check = me->valida_mesc( EXPORTING ls_salida =  <f1> ).
      CHECK ls_check = 0.

      "---> Valido si estatus
      ls_check = me->valida_estat( EXPORTING ls_salida =  <f1> ).
      CHECK ls_check = 0.

      "--> Valido que campo Documento tenga valor
      ls_check = me->valida_documento(  EXPORTING ls_salida =  <f1> ).
      CHECK ls_check = 0.

      "--> Valido que si es de contado/credito
      ls_check = me->valida_contado(  EXPORTING ls_salida =  <f1> ).
      CHECK ls_check = 0.

      "--> Valido tipo de documento 33/34
      ls_check = me->valida_tipodocumento(  EXPORTING ls_salida =  <f1> ).
      CHECK ls_check = 0.

      "---> Valido fecha 8 dias.
      me->valida_dias( EXPORTING ls_salida =  <f1>
                       IMPORTING check = ls_check
                                 doc_new  = ls_a ).
      CHECK ls_check = 0.

      "-----Bloqueo documento.
      ls_check = me->bloqueo( EXPORTING lt_zcb_recfactprov = <f1> ).
      CHECK ls_check = 0.
      "--->LLamo bapi mod o crear Doc.Nuevos
      IF ls_a = abap_true. "Crear Doc.Nuevos
        IF <f1>-zdte_tipo = '2' OR  <f1>-zdte_tipo = '5'. "FI
          me->bapi_ficrea( EXPORTING ls_salida = <f1> ).
        ELSE.                                             "MM

          "--> Valido que si OC al crear DOC
          ls_check = me->valida_oc(  EXPORTING ls_salida =  <f1> ).

          IF ls_check  NE 0.
            me->desbloqueo( EXPORTING lt_zcb_recfactprov = <f1> tabla = 'ZTFI_0074').
          ENDIF.
          CHECK ls_check = 0.

          me->bapi_mm( EXPORTING ls_salida = <f1>
                                 doc_new  = ls_a ).
        ENDIF.
      ELSE. " Modificar documentos.
        IF <f1>-zdte_tipo = '2' OR  <f1>-zdte_tipo = '5'. "FI
          me->bapi_fimod( EXPORTING ls_salida = <f1> ).
        ELSE.                                             "MM
          me->bapi_mm( EXPORTING ls_salida = <f1>
                                 doc_new = ls_a ).
        ENDIF.
      ENDIF.
      "-----> Desbloqueo registro
      me->desbloqueo( EXPORTING lt_zcb_recfactprov = <f1> tabla = 'ZTFI_0074' ).
    ENDLOOP.
    lt_mod[] = lt_mod_save[].
  ENDMETHOD.


  method MUEVE_VALOR.
    lt_salida_temp[] = lt_entra[].
  endmethod.


  METHOD VALIDA_CONTADO.
    DATA: lr_status   TYPE RANGE OF  ztfi_0074-status,
          lv_mensaje  TYPE char200,
          ls_mod_save TYPE ty_mod.


    IF ls_salida-FMAPAGO ne '1'.
      ls_check = 0.
    ELSE.
      ls_check = 4.
      CLEAR: lv_mensaje,
        ls_mod_save.
      CONCATENATE lv_documento_mensaje
                  TEXT-411 INTO lv_mensaje SEPARATED BY space.

      ls_mod_save-icono     = icon_red_light.
      ls_mod_save-documento = lv_documento_mensaje.
      ls_mod_save-mensaje   = lv_mensaje.
      APPEND ls_mod_save TO lt_mod_save.

      CLEAR gs_bal_msg.
      gs_bal_msg-msgty = 'E'.
      gs_bal_msg-msgid = 'ZDTE_0001'.
      gs_bal_msg-msgno = '012'.
      gs_bal_msg-msgv1 = TEXT-216.
      gs_bal_msg-msgv2 = lv_documento_mensaje.
      gs_bal_msg-msgv3 = TEXT-411.
      go_logc->add_msg( i_s_msg = gs_bal_msg ).
      go_logc->save( ).

    ENDIF.
  ENDMETHOD.


  METHOD valida_dias.
    DATA: lv_fecha_calc TYPE datum.
    DATA: lv_anno TYPE char4.
    DATA: lv_mes       TYPE char2,
          lv_mensaje   TYPE char200,
          ls_mod_save  TYPE ty_mod,
          n_dias       TYPE numc4,
          n_diasi      TYPE i,
          ls_tvarvc    TYPE tvarvc,
          lv_fecha_eva TYPE datum,
          lr_fechp     TYPE RANGE OF datum.

    check = 0.

    SELECT * INTO ls_tvarvc FROM tvarvc UP TO 1 ROWS
    WHERE name =  co_diasfechac
      ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc = 0.
      n_dias = ls_tvarvc-low.
      n_diasi = n_dias.
      n_diasi = n_diasi * -1.
      CALL FUNCTION 'CALCULATE_DATE'
        EXPORTING
          days        = n_diasi
          months      = '0'
          start_date  = fecha_cierre
        IMPORTING
          result_date = lv_fecha_calc.
    ELSE.
    ENDIF.

    IF ls_salida-zfecharsii IS NOT INITIAL.
      lv_fecha_eva = ls_salida-zfecharsii.
    ELSEIF  ls_salida-fechabase IS NOT INITIAL.
      lv_fecha_eva = ls_salida-fechabase.
    ELSE.
      lv_fecha_eva = ls_salida-bldat.
    ENDIF.

    lr_fechp = VALUE #( BASE lr_fechp ( sign = 'I' option = 'BT' low = lv_fecha_calc
                                                                high = fecha_cierre ) ).

    IF  lv_fecha_eva IN lr_fechp. "Se puede cambiar fecha contable.
      lv_anno = lv_fecha_eva(4).
      lv_mes =  lv_fecha_eva+4(2).
      IF fecha_cierre(4) NE  lv_anno .
        doc_new = 'X'.
      ENDIF.
    ELSE.
      check = 4.
      CLEAR: lv_mensaje,
        ls_mod_save.
      CONCATENATE lv_documento_mensaje
                  TEXT-406 INTO lv_mensaje SEPARATED BY space.

      ls_mod_save-icono     = icon_red_light.
      ls_mod_save-documento = lv_documento_mensaje.
      ls_mod_save-mensaje   = lv_mensaje.
      APPEND ls_mod_save TO lt_mod_save.

      CLEAR gs_bal_msg.
      gs_bal_msg-msgty = 'E'.
      gs_bal_msg-msgid = 'ZDTE_0001'.
      gs_bal_msg-msgno = '012'.
      gs_bal_msg-msgv1 = TEXT-216.
      gs_bal_msg-msgv2 = lv_documento_mensaje.
      gs_bal_msg-msgv3 = TEXT-406.

      go_logc->add_msg( i_s_msg = gs_bal_msg ).
      go_logc->save( ).
    ENDIF.

  ENDMETHOD.


  METHOD VALIDA_DOCUMENTO.
    DATA: lr_status   TYPE RANGE OF  ztfi_0074-status,
          lv_mensaje  TYPE char200,
          ls_mod_save TYPE ty_mod.


    IF ls_salida-belnr ne ''.
      ls_check = 0.
    ELSE.
      ls_check = 4.
      CLEAR: lv_mensaje,
        ls_mod_save.
      CONCATENATE lv_documento_mensaje
                  TEXT-412 INTO lv_mensaje SEPARATED BY space.

      ls_mod_save-icono     = icon_red_light.
      ls_mod_save-documento = lv_documento_mensaje.
      ls_mod_save-mensaje   = lv_mensaje.
      APPEND ls_mod_save TO lt_mod_save.

      CLEAR gs_bal_msg.
      gs_bal_msg-msgty = 'E'.
      gs_bal_msg-msgid = 'ZDTE_0001'.
      gs_bal_msg-msgno = '012'.
      gs_bal_msg-msgv1 = TEXT-216.
      gs_bal_msg-msgv2 = lv_documento_mensaje.
      gs_bal_msg-msgv3 = TEXT-412.
      go_logc->add_msg( i_s_msg = gs_bal_msg ).
      go_logc->save( ).

    ENDIF.
  ENDMETHOD.


  METHOD valida_estat.
    DATA: lr_status   TYPE RANGE OF  ztfi_0074-status,
          lv_mensaje  TYPE char200,
          ls_mod_save TYPE ty_mod.
    lr_status = VALUE #( BASE lr_status ( sign = 'I' option = 'EQ' low = '2')
                                        ( sign = 'I' option = 'EQ' low = '3')
                                        ( sign = 'I' option = 'EQ' low = '8')
                         ).

    IF ls_salida-status IN lr_status.
      ls_check = 0.
    ELSE.
      ls_check = 4.
      CLEAR: lv_mensaje,
        ls_mod_save.
      CONCATENATE lv_documento_mensaje
                  TEXT-404 INTO lv_mensaje SEPARATED BY space.

      ls_mod_save-icono     = icon_red_light.
      ls_mod_save-documento = lv_documento_mensaje.
      ls_mod_save-mensaje   = lv_mensaje.
      APPEND ls_mod_save TO lt_mod_save.

      CLEAR gs_bal_msg.
      gs_bal_msg-msgty = 'E'.
      gs_bal_msg-msgid = 'ZDTE_0001'.
      gs_bal_msg-msgno = '012'.
      gs_bal_msg-msgv1 = TEXT-216.
      gs_bal_msg-msgv2 = lv_documento_mensaje.
      gs_bal_msg-msgv3 = TEXT-404.
      go_logc->add_msg( i_s_msg = gs_bal_msg ).
      go_logc->save( ).

    ENDIF.
  ENDMETHOD.


  METHOD valida_fechac.
  DATA: lv_mensaje  TYPE char200,
          ls_mod_save TYPE ty_mod.
    IF fecha_cierre+6(2) EQ '01'.
      ls_check = 0.
    ELSE.
      ls_check = 4.
      CLEAR: lv_mensaje,
        ls_mod_save.
      CONCATENATE lv_documento_mensaje
                  TEXT-416 INTO lv_mensaje SEPARATED BY space.

      ls_mod_save-icono     = icon_yellow_light.
      ls_mod_save-documento = lv_documento_mensaje.
      ls_mod_save-mensaje   = lv_mensaje.
      APPEND ls_mod_save TO lt_mod_save.

      CLEAR gs_bal_msg.
      gs_bal_msg-msgty = 'E'.
      gs_bal_msg-msgid = 'ZDTE_0001'.
      gs_bal_msg-msgno = '012'.
      gs_bal_msg-msgv1 = TEXT-216.
      gs_bal_msg-msgv2 = lv_documento_mensaje.
      gs_bal_msg-msgv3 = TEXT-416.
      go_logc->add_msg( i_s_msg = gs_bal_msg ).
      go_logc->save( ).

    ENDIF.

  ENDMETHOD.


  METHOD valida_mesc.
    DATA: lr_status   TYPE RANGE OF  ztfi_0074-status,
          lv_mensaje  TYPE char200,
          ls_mod_save TYPE ty_mod,
          ls_fecha    TYPE sy-datum.

    IF ls_salida-zfechacont IS INITIAL.
      IF ls_salida-zdte_tipo = '1' OR
         ls_salida-zdte_tipo = '3' OR
         ls_salida-zdte_tipo = '4'.

        SELECT budat INTO ls_fecha
         FROM rbkp UP TO 1 ROWS
         WHERE belnr EQ ls_salida-belnr
          AND gjahr EQ ls_salida-gjahr
          ORDER BY PRIMARY KEY.
        ENDSELECT.
        IF sy-subrc = 0.
          ls_salida-zfechacont = ls_fecha.
        ENDIF.
      ENDIF.
    ENDIF.

    IF ls_salida-zfechacont(6) NE fecha_cierre(6)."AAAAMM
      ls_check = 0.
    ELSE.
      ls_check = 4.
      CLEAR: lv_mensaje,
        ls_mod_save.
      CONCATENATE lv_documento_mensaje
                  TEXT-413 INTO lv_mensaje SEPARATED BY space.

      ls_mod_save-icono     = icon_yellow_light.
      ls_mod_save-documento = lv_documento_mensaje.
      ls_mod_save-mensaje   = lv_mensaje.
      APPEND ls_mod_save TO lt_mod_save.

      CLEAR gs_bal_msg.
      gs_bal_msg-msgty = 'W'.
      gs_bal_msg-msgid = 'ZDTE_0001'.
      gs_bal_msg-msgno = '012'.
      gs_bal_msg-msgv1 = TEXT-216.
      gs_bal_msg-msgv2 = lv_documento_mensaje.
      gs_bal_msg-msgv3 = TEXT-413.
      go_logc->add_msg( i_s_msg = gs_bal_msg ).
      go_logc->save( ).

    ENDIF.
  ENDMETHOD.


  METHOD VALIDA_OC.
    DATA: lr_status   TYPE RANGE OF  ztfi_0074-status,
          lv_mensaje  TYPE char200,
          ls_mod_save TYPE ty_mod.


    IF ls_salida-ebeln is not initial.
      ls_check = 0.
    ELSE.
      ls_check = 4.
      CLEAR: lv_mensaje,
        ls_mod_save.
      CONCATENATE lv_documento_mensaje
                  TEXT-414 INTO lv_mensaje SEPARATED BY space.

      ls_mod_save-icono     = icon_red_light.
      ls_mod_save-documento = lv_documento_mensaje.
      ls_mod_save-mensaje   = lv_mensaje.
      APPEND ls_mod_save TO lt_mod_save.

      CLEAR gs_bal_msg.
      gs_bal_msg-msgty = 'E'.
      gs_bal_msg-msgid = 'ZDTE_0001'.
      gs_bal_msg-msgno = '012'.
      gs_bal_msg-msgv1 = TEXT-216.
      gs_bal_msg-msgv2 = lv_documento_mensaje.
      gs_bal_msg-msgv3 = TEXT-414.
      go_logc->add_msg( i_s_msg = gs_bal_msg ).
      go_logc->save( ).

    ENDIF.
  ENDMETHOD.


  METHOD VALIDA_TIPODOCUMENTO.
    DATA: lr_tipodte   TYPE RANGE OF  ztfi_0074-tipodte,
          lv_mensaje  TYPE char200,
          ls_mod_save TYPE ty_mod.
    lr_tipodte = VALUE #( BASE lr_tipodte ( sign = 'I' option = 'EQ' low = '33')
                                        ( sign = 'I' option = 'EQ' low = '34')
                                        ).

    IF ls_salida-tipodte IN lr_tipodte.
      ls_check = 0.
    ELSE.
      ls_check = 4.
      CLEAR: lv_mensaje,
        ls_mod_save.
      CONCATENATE lv_documento_mensaje
                  TEXT-410 INTO lv_mensaje SEPARATED BY space.

      ls_mod_save-icono     = icon_red_light.
      ls_mod_save-documento = lv_documento_mensaje.
      ls_mod_save-mensaje   = lv_mensaje.
      APPEND ls_mod_save TO lt_mod_save.

      CLEAR gs_bal_msg.
      gs_bal_msg-msgty = 'E'.
      gs_bal_msg-msgid = 'ZDTE_0001'.
      gs_bal_msg-msgno = '012'.
      gs_bal_msg-msgv1 = TEXT-216.
      gs_bal_msg-msgv2 = lv_documento_mensaje.
      gs_bal_msg-msgv3 = TEXT-410.
      go_logc->add_msg( i_s_msg = gs_bal_msg ).
      go_logc->save( ).

    ENDIF.
  ENDMETHOD.
ENDCLASS.
