class ZCL_DTE_FI0058V2 definition
  public
  create public .

public section.

  types:
    t_0090 TYPE STANDARD TABLE OF ztfi_0090 .
  types:
    t_lfb1 TYPE STANDARD TABLE OF lfb1 .
  types:
    t_0092 TYPE STANDARD TABLE OF ztfi_0092 .
  types:
    t_0093 TYPE STANDARD TABLE OF ztfi_0093 .
  types:
    t_0077 TYPE STANDARD TABLE OF ztfi_0077 .
  types:
    t_t001z TYPE STANDARD TABLE OF t001z .
  types:
    t_lfa1 TYPE STANDARD TABLE OF lfa1 .
  types:
    r_bldat TYPE RANGE OF sy-datum .
  types:
    r_folio TYPE RANGE OF ztfi_0074-xblnr .
  types:
    r_bukrs TYPE RANGE OF ztfi_0074-bukrs .
  types:
    r_sta   TYPE RANGE OF ztfi_0074-status .
  types:
    ty_essr  TYPE RANGE OF essr-lblni .
  types:
    l_essr TYPE LINE OF ty_essr .
  types:
    BEGIN OF ty_log,
        estado    TYPE icon_d,
        documento TYPE char20,
        sociedad  TYPE bukrs,
        proveedor TYPE lfa1-lifnr,
        fecha     TYPE sydatum,
        mensaje   TYPE char256,
      END OF ty_log .
  types:
    t_log  TYPE STANDARD TABLE OF ty_log .
  types:
    BEGIN OF ty_t001,
        bukrs TYPE t001-bukrs,
        wfvar TYPE t001-wfvar,
      END OF ty_t001 .
  types:
    t_ekko TYPE STANDARD TABLE OF ekko .
  types:
    t_ekpo TYPE STANDARD TABLE OF ekpo .
  types:
    t_0075 TYPE STANDARD TABLE OF ztfi_0075 .
  types:
    t_ekkn TYPE STANDARD TABLE OF ekkn .
  types:
    t_afko TYPE STANDARD TABLE OF afko .
  types:
    t_proj TYPE STANDARD TABLE OF proj .
  types:
    t_zdte_cldoc TYPE STANDARD TABLE OF ztfi_0001b .
  types:
    t_t001 TYPE STANDARD TABLE OF ty_t001 .
  types:
    t_vbwf16 TYPE STANDARD TABLE OF vbwf16 .
  types:
    r_fec  TYPE RANGE OF sydatum .

  data S_BUKRS type R_BUKRS .
  data S_BLDAT type R_BLDAT .
  data S_FOLIO type R_FOLIO .
  data CNUM type MSGNO .
  data S_STATUS type R_STA .
  data:
    ti_log TYPE STANDARD TABLE OF ty_log .
  data WA_LOG type TY_LOG .
  data:
    gt_t001 TYPE STANDARD TABLE OF t001 .
  data GS_T001 type T001 .
  data:
    lt_zdte_posfact TYPE STANDARD TABLE OF ztfi_0075 .
  data LS_ZDTE_POSFACT type ZTFI_0075 .
  data:
    lt_ekpo TYPE STANDARD TABLE OF ekpo .
  data LS_EKPO type EKPO .
  data:
    lt_ekbe TYPE STANDARD TABLE OF ekbe .
  data LS_EKBE type EKBE .
  data:
    lt_ztfi_0082 TYPE STANDARD TABLE OF ztfi_0082 .
  data LS_ZTFI_0082 type ZTFI_0082 .
  data:
    lt_t001z TYPE STANDARD TABLE OF t001z .
  data LS_T001Z type T001Z .
  data:
    lt_lfa1 TYPE STANDARD TABLE OF lfa1 .
  data LS_LFA1 type LFA1 .
  data:
    lt_lfb1 TYPE STANDARD TABLE OF lfb1 .
  data LS_LFB1 type LFB1 .
  data LV_REGISTRO like SY-TABIX .
  data LV_TRX type ZTFI_0074-TCODE .
  data LV_VALIDA_PROV type C .
  data LV_SERVICIO type C .
  data T_PSTYP type EKPO-PSTYP .
  data R_FECHA type R_FEC .
  data LS_R_FECHA type R_FEC .
  data:
    lt_ztfi_0077 TYPE STANDARD TABLE OF ztfi_0077 .
  data LS_ZTFI_0077 type ZTFI_0077 .
  data:
    lt_zcb_recfactprov TYPE STANDARD TABLE OF ztfi_0074 .
  data LV_TABIX type SYTABIX .
  data LS_RECFACTPROV type ZTFI_0074 .
  data LV_BEWTP type EKBE-BEWTP .
  data LV_ERROR type C .
  data RES type SY-SUBRC .
  data WA_OBJECTID type CDHDR-OBJECTID .
  data:
    old_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074 .
  data LS_OLD_ZTFI_0074 type ZTFI_0074 .
  data:
    new_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074 .
  data LS_NEW_ZTFI_0074 type ZTFI_0074 .
  data LV_DOC type SEQG3-GARG .
  data RESULTA type SY-SUBRC .
  data NUMERO_ENQ type SY-TABIX .
  data:
    t_enq TYPE  STANDARD TABLE OF seqg3 .
  data LS_ENQ type SEQG3 .
  data WA_RECFACTPROV type ZTFI_0074 .
  data GS_VARINT type ZDTE_VARIANT .

  methods SAVE_LOG
    importing
      value(U_HANDLE) type BALLOGHNDL optional .
  methods LIBERACION_MM
    importing
      value(P_BELNR) type BELNR_D optional
      value(P_GJAHR) type GJAHR optional
      value(P_BUKRS) type BUKRS optional .
  methods GENERAR_RECHAZO
    importing
      value(LT_PAVAL) type T_T001Z optional
      value(LT_LFA1) type T_LFA1 optional
      value(P_GLOSA) type CHAR256 optional
      value(P_NUMERO) type SYMSGNO optional
    changing
      value(P_ENTRADA) type ZTFI_0074 optional .
  methods GENERAR_MIRO_MIR7
    importing
      value(LT_T001Z) type T_T001Z optional
      value(LT_LFA1) type T_LFA1 optional
    changing
      value(P_ENTRADA) type ZTFI_0074 optional .
  methods GENERAR_MIRO_MIR4
    importing
      value(LT_T001Z) type T_T001Z optional
      value(LT_LFA1) type T_LFA1 optional
      value(P_SERVICIO) type C optional
    changing
      value(P_ENTRADA) type ZTFI_0074 optional .
  methods GENERAR_LOG
    importing
      !P_ENTRADA type ZTFI_0074
      !P_MSGTY type SYST_MSGTY
      !P_NUMERO type SYMSGNO
      !P_VARIABLE type CHAR256 .
  methods GENERAR_FBV1
    importing
      value(LT_T001Z) type T_T001Z optional
      value(LT_LFA1) type T_LFA1 optional
    changing
      value(P_ENTRADA) type ZTFI_0074 optional .
  methods GENERAR_APROB
    importing
      value(LT_PAVAL) type T_T001Z optional
      value(LT_LFA1) type T_LFA1 optional
      value(P_GLOSA) type CHAR256 optional
      value(P_NUMERO) type SYMSGNO optional
    changing
      value(P_ENTRADA) type ZTFI_0074 optional .
  methods ENVIAR_RECHAZOACEP
    importing
      value(I_SALIDA) type ANY optional
      value(I_COMMIT) type C optional
      value(I_ENTRADA) type ZTFI_0074 optional
    exporting
      value(E_ERROR) type C .
  methods CREAR_LOG
    importing
      value(P_ENTRADA) type ZTFI_0074 optional
    changing
      value(C_BAL_LOG) type BAL_S_LOG optional
      value(C_HANDLE) type BALLOGHNDL optional .
  methods ADD_LOG
    importing
      value(U_HANDLE) type BALLOGHNDL optional
      value(U_XMSG) type FIMSG1 optional .
  methods BLOQUEO .
  methods DESBLOQUEO .
  methods GRABAR .
  methods INILOG .
  methods LIBERAROBJ .
  methods LLAMABAPIS .
  methods VALIDACIONBLOQEST .
  methods VALIDACIONES .
  methods VALIDAOC .
  methods BUSCADOSGENERALES .
  methods DATOSDTE .
  methods CONSTRUCTOR
    importing
      value(I_BUKRS) type R_BUKRS optional
      value(I_XBLNR) type R_FOLIO optional
      value(I_STATUS) type R_STA optional
      value(I_BLDAT) type R_BLDAT optional .
  methods DATA_SELECCIONAR .
  methods EJECUTA_PROCESO
    exporting
      value(TI_LOG) type T_LOG .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DTE_FI0058V2 IMPLEMENTATION.


METHOD add_log.
    DATA: ls_bal_msg TYPE bal_s_msg.

    MOVE-CORRESPONDING u_xmsg TO ls_bal_msg.

**//..
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle     = u_handle
        i_s_msg          = ls_bal_msg
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'X' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


METHOD bloqueo.

    CALL FUNCTION 'ENQUEUE_EZTFI_0074'
      EXPORTING
        mode_ztfi_0074 = 'E'
        mandt          = sy-mandt
        bukrs          = wa_recfactprov-bukrs
        xblnr          = wa_recfactprov-xblnr
        lifnr          = wa_recfactprov-lifnr
        tipodte        = wa_recfactprov-tipodte
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
      res = sy-subrc.
* Implement suitable error handling here
      wa_log-estado    = icon_red_light.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                                        WITH sy-msgv1 sy-msgv2
                                             sy-msgv3 sy-msgv4
                                        INTO wa_log-mensaje.
      APPEND wa_log TO ti_log.
    ELSE.
      CLEAR ls_recfactprov.
      "----> Releo el Registro bloqueado, para traer el ultimo valor
      SELECT SINGLE * FROM ztfi_0074 INTO ls_recfactprov
        WHERE bukrs EQ wa_recfactprov-bukrs AND
              xblnr EQ wa_recfactprov-xblnr AND
              tipodte  EQ wa_recfactprov-tipodte AND
              lifnr EQ wa_recfactprov-lifnr AND
               belnr NE space AND
               gjahr NE space AND
              status IN s_status.
      IF sy-subrc = 0 . "
        "----> Entrego ultimo valor a linea
        wa_recfactprov =    ls_recfactprov.
        MODIFY lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.
      ELSE.
        res = sy-subrc.
        CALL FUNCTION 'DEQUEUE_EZTFI_0074'
          EXPORTING
            mode_ztfi_0074 = 'E'
            mandt          = sy-mandt
            bukrs          = wa_recfactprov-bukrs
            xblnr          = wa_recfactprov-xblnr
            lifnr          = wa_recfactprov-lifnr
            tipodte        = wa_recfactprov-tipodte
*           X_BUKRS        = ' '
*           X_XBLNR        = ' '
*           X_LIFNR        = ' '
*           X_TIPODTE      = ' '
*           _SCOPE         = '3'
*           _SYNCHRON      = ' '
*           _COLLECT       = ' '
          .
        wa_log-estado    = icon_red_light.
        MESSAGE e063(zdte_0001) INTO wa_log-mensaje.
        APPEND wa_log TO ti_log .
      ENDIF.
    ENDIF.

  ENDMETHOD.


METHOD buscadosgenerales.
    SELECT *
    FROM ztfi_0074
    INTO TABLE lt_zcb_recfactprov
    WHERE bukrs IN s_bukrs AND
          xblnr IN s_folio AND
          belnr NE space   AND
          gjahr NE space   AND
          bldat IN s_bldat AND
          status IN s_status.
    IF sy-dbcnt >= 1.


      IF lines( lt_zcb_recfactprov[] ) GT 0.

        SELECT * FROM ztfi_0082
          INTO TABLE lt_ztfi_0082
           FOR ALL ENTRIES IN lt_zcb_recfactprov
          WHERE bukrs = lt_zcb_recfactprov-bukrs.
        IF sy-dbcnt >= 1.
          SORT lt_ztfi_0082 BY bukrs.
          DELETE ADJACENT DUPLICATES FROM lt_ztfi_0082.
        ENDIF.

        SELECT  *
            FROM t001z
            INTO TABLE lt_t001z
            FOR ALL ENTRIES IN lt_zcb_recfactprov
            WHERE bukrs = lt_zcb_recfactprov-bukrs AND
                  party = 'TAXNR'.
        IF sy-dbcnt >= 1.
          SORT lt_t001z BY bukrs.
          DELETE ADJACENT DUPLICATES FROM lt_t001z.
        ENDIF.

        SELECT *
            FROM lfa1
            INTO TABLE lt_lfa1
            FOR ALL ENTRIES IN lt_zcb_recfactprov
            WHERE lifnr = lt_zcb_recfactprov-lifnr.
        IF sy-dbcnt >= 1.
          SORT lt_lfa1 BY lifnr.
          DELETE ADJACENT DUPLICATES FROM lt_lfa1.

          SELECT *
          FROM lfb1
          INTO TABLE lt_lfb1
          FOR ALL ENTRIES IN lt_zcb_recfactprov
          WHERE lifnr = lt_zcb_recfactprov-lifnr AND
                bukrs = lt_zcb_recfactprov-bukrs.
          IF sy-dbcnt >= 1.
            SORT lt_lfb1 BY lifnr bukrs.
            DELETE ADJACENT DUPLICATES FROM lt_lfb1.
          ENDIF.
        ENDIF.

        SELECT *
           FROM ztfi_0077
           INTO TABLE lt_ztfi_0077
           FOR ALL ENTRIES IN lt_zcb_recfactprov
           WHERE bukrs = lt_zcb_recfactprov-bukrs AND
                 xblnr = lt_zcb_recfactprov-xblnr AND
                 lifnr = lt_zcb_recfactprov-lifnr.
        IF sy-dbcnt >= 1.
          SORT lt_ztfi_0077 BY bukrs xblnr lifnr.
          DELETE ADJACENT DUPLICATES FROM lt_ztfi_0077.
        ENDIF.

        SELECT * FROM ztfi_0075
           INTO TABLE lt_zdte_posfact
           FOR ALL ENTRIES IN lt_zcb_recfactprov
           WHERE bukrs = lt_zcb_recfactprov-bukrs AND
                 lifnr = lt_zcb_recfactprov-lifnr AND
                 xblnr = lt_zcb_recfactprov-xblnr AND
                  tipodte = lt_zcb_recfactprov-tipodte.
        IF sy-dbcnt >= 1.
          SORT lt_zdte_posfact BY ebeln.
          DELETE ADJACENT DUPLICATES FROM lt_zdte_posfact.
        ENDIF.
        IF lines( lt_zdte_posfact[] ) GT 0.
          SELECT *
          FROM ekpo
          INTO TABLE lt_ekpo
          FOR ALL ENTRIES IN lt_zdte_posfact "lt_ekko
          WHERE ebeln = lt_zdte_posfact-ebeln AND
                ebelp = lt_zdte_posfact-ebelp."lt_ekko-ebeln.
          IF sy-dbcnt >= 1.
            SORT lt_ekpo BY ebeln.
            DELETE ADJACENT DUPLICATES FROM lt_ekpo.
            DELETE lt_ekpo WHERE loekz NE space.
          ENDIF.

          IF lines( lt_ekpo[] ) GT 0.
            SELECT *
            FROM ekbe
            INTO TABLE lt_ekbe
            FOR ALL ENTRIES IN lt_ekpo
            WHERE ebeln = lt_ekpo-ebeln AND
                  ebelp = lt_ekpo-ebelp.
            IF sy-dbcnt >= 1.
              SORT lt_ekbe BY ebeln ebelp.
              DELETE ADJACENT DUPLICATES FROM lt_ekbe.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


METHOD constructor.


  me->s_bukrs[] = i_bukrs[].
  me->s_folio[] = i_xblnr[].
  me->s_status[] = i_status[].
  me->s_bldat[] = i_bldat[].

  SELECT  vatint INTO gs_varint FROM ztfi_0078 UP TO 1 ROWS
  WHERE bukrs IN me->s_bukrs
  ORDER BY PRIMARY KEY.
  ENDSELECT.

ENDMETHOD.


METHOD crear_log.
    CONCATENATE p_entrada-bukrs p_entrada-lifnr
               p_entrada-xblnr p_entrada-tipodte
               INTO c_bal_log-extnumber.

    c_bal_log-object     = 'ZDTE'.
    c_bal_log-subobject  = 'RECEPCION'.
    c_bal_log-aldate     = syst-datum.
    c_bal_log-altime     = syst-uzeit.
    c_bal_log-aluser     = syst-uname.
    c_bal_log-alprog     = syst-repid.

    "----->se buscan los anteriores.
    DATA: lt_log_header TYPE balhdr_t,
          lv_log_filter TYPE bal_s_lfil.

    DATA:r_external  TYPE bal_r_extn,
         r_object    TYPE bal_r_obj,
         r_subobject TYPE bal_r_sub.

    DATA: ln_external  LIKE LINE OF r_external,
          ln_object    LIKE LINE OF r_object,
          ln_subobject LIKE LINE OF r_subobject.
    CLEAR:  ln_external,
            ln_object,
            ln_subobject.

    FREE: r_external,
          r_object,
          r_subobject.

    ln_external-sign = ln_object-sign = ln_subobject-sign = 'I'.
    ln_external-option = ln_object-option = ln_subobject-option = 'EQ'.
    ln_external-low  = c_bal_log-extnumber.
    ln_object-low    = 'ZDTE'.
    ln_subobject-low = 'RECEPCION'.

    APPEND ln_external TO r_external.
    APPEND ln_object TO r_object.
    APPEND ln_subobject TO r_subobject.

    lv_log_filter-extnumber[] = r_external[].
    lv_log_filter-object[] = r_object[].
    lv_log_filter-subobject[] = r_subobject[].
    FREE lt_log_header.

    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING
        i_s_log_filter     = lv_log_filter
      IMPORTING
        e_t_log_header     = lt_log_header
      EXCEPTIONS
        log_not_found      = 1
        no_filter_criteria = 2
        OTHERS             = 3.
    IF sy-subrc = 0 AND lt_log_header[] IS NOT INITIAL.
      CALL FUNCTION 'BAL_DB_DELETE'
        EXPORTING
          i_t_logs_to_delete = lt_log_header.
    ENDIF.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = c_bal_log
      IMPORTING
        e_log_handle = c_handle.
  ENDMETHOD.


METHOD data_seleccionar.
    me->buscadosgenerales( ).
    me->datosdte( ).
  ENDMETHOD.


method DATOSDTE.
  endmethod.


method DESBLOQUEO.
   CALL FUNCTION 'DEQUEUE_EZTFI_0074'
    EXPORTING
      mode_ztfi_0074 = 'E'
      mandt          = sy-mandt
      bukrs          = wa_recfactprov-bukrs
      xblnr          = wa_recfactprov-xblnr
      lifnr          = wa_recfactprov-lifnr
      tipodte        = wa_recfactprov-tipodte
*     X_BUKRS        = ' '
*     X_XBLNR        = ' '
*     X_LIFNR        = ' '
*     X_TIPODTE      = ' '
*     _SCOPE         = '3'
*     _SYNCHRON      = ' '
*     _COLLECT       = ' '
    .

  endmethod.


METHOD ejecuta_proceso.
    IF lines( lt_zcb_recfactprov[] ) GT 0.
      me->liberarobj( ).
      LOOP AT lt_zcb_recfactprov INTO wa_recfactprov.
        lv_registro = sy-tabix.
        lv_trx = wa_recfactprov-tcode.
        me->inilog( ).
        res = 0.
        me->bloqueo( ).
        CHECK res EQ 0.
        me->validaciones( ).
        CHECK res EQ 0.
        me->validaoc( ).
        CHECK res EQ 0.
        me->validacionbloqest( ).
        CHECK res EQ 0.
        me->llamabapis( ).
        me->grabar( ).
        me->desbloqueo( ).
      ENDLOOP.
      me->liberarobj( ).
      ti_log[] = me->ti_log[].
    ENDIF.
  ENDMETHOD.


METHOD ENVIAR_RECHAZOACEP.
  DATA: e_salida TYPE zst_dte_ack.
  DATA: error TYPE char1.
  DATA: co_type TYPE string.
  DATA: t_0078 TYPE STANDARD TABLE OF ztfi_0078.
  DATA: wa_0078 TYPE ztfi_0078.
  CLEAR co_type. CLEAR wa_0078. REFRESH t_0078.

*  "----llenar  Nueva estructura de campos adicionales por Pais.
*  " En chile son los mismos datos zst_dte_ack.

  e_salida = i_salida.


*  "--- manejo de funcion multi-Pais
  SELECT SINGLE * INTO  wa_0078 FROM ztfi_0078
      WHERE bukrs EQ i_entrada-bukrs.

  CLEAR  co_type.
  SELECT SINGLE nombre_int INTO co_type FROM ztfi_0078b
     WHERE vatint = wa_0078-vatint
     AND   codint = '0004'.

  CHECK co_type IS NOT INITIAL.

  "----Envio de Rechazo a  Externo/ o Ente Gubernamental
  CALL FUNCTION co_type "ZFI_0008DTE
    EXPORTING
      acknowledgment = e_salida
      commit         = i_commit
    IMPORTING
      error          = e_error.

  ENDMETHOD.


METHOD generar_aprob.
    DATA: e_salida TYPE zst_dte_ack.
    DATA: ls_error TYPE char1.
    DATA: ls_paval TYPE t001z,
          ls_lfa1  TYPE lfa1.

    CLEAR e_salida. CLEAR ls_error. CLEAR ls_paval. CLEAR ls_lfa1.
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

    READ TABLE lt_lfa1 into ls_lfa1 WITH KEY lifnr = p_entrada-lifnr.
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

    e_salida-tipodte = p_entrada-tipodte. "33
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
    e_salida-estadodte = '0'.
    e_salida-nrodetalles = '1'.

    IF p_glosa IS NOT INITIAL.
      e_salida-estadodteglosa = p_glosa.
    ELSE.
      e_salida-estadodteglosa = TEXT-gl2.
    ENDIF.

*    "----Envio de Aceptacion al SII
*    CALL FUNCTION 'ZFI_0008DTE'
*      EXPORTING
*        acknowledgment = e_salida
*        commit         = 'X'
*      IMPORTING
*        error          = ls_error.

    CALL METHOD me->enviar_rechazoacep
      EXPORTING
        i_salida  = e_salida
        i_entrada = p_entrada
        i_commit = 'X'
      IMPORTING
        e_error   = ls_error.

**********************************************************
* Como la aceptacion de no ejecutarse, el SII la realiza
* se deja en N
**********************************************************
    IF ls_error = 'W'.
      p_entrada-status = 'N'.
      p_entrada-glosa = 'Error WS al rechazar, Ver log'.
    ENDIF.

  ENDMETHOD.


METHOD generar_fbv1.
    DATA: xvbkpf    TYPE STANDARD TABLE OF vbkpf,
          ls_xvbkpf TYPE  vbkpf,
          xmsg      TYPE STANDARD TABLE OF fimsg1,
          lt_xmsg   TYPE STANDARD TABLE OF msg_tab_line.

    DATA: lv_posted  TYPE boole_d VALUE 'X',
          ls_bal_log TYPE bal_s_log,
          ls_bal_msg TYPE bal_s_msg,
          ls_xmsg    TYPE fimsg1,
          ls_handle  TYPE balloghndl,
          lv_mesn    TYPE char256.
    CLEAR ls_xvbkpf. CLEAR  lv_mesn.
    FREE xvbkpf.

    CLEAR ls_xmsg.
    FREE xmsg.

    ls_xvbkpf-belnr = p_entrada-belnr.
    ls_xvbkpf-bukrs = p_entrada-bukrs.
    ls_xvbkpf-gjahr = p_entrada-gjahr.
    APPEND ls_xvbkpf TO xvbkpf.

    DATA: lv_clase TYPE t100-arbgb,
          lv_id    TYPE t100-msgnr.
    DATA: lv_mensaje TYPE char200.

    CALL FUNCTION 'PRELIMINARY_POSTING_POST_ALL'
      EXPORTING
        nomsg   = 'X'
        synch   = 'X'
*       nocheck = 'X'
      TABLES
        t_vbkpf = xvbkpf
        t_msg   = xmsg
      EXCEPTIONS
        OTHERS  = 1.

    IF xmsg[] IS NOT INITIAL.
      LOOP AT xmsg INTO ls_xmsg WHERE posted NE 'X'.
        CLEAR: lv_posted.
        wa_log-estado    = icon_red_light.
        wa_log-documento = p_entrada-xblnr.
        wa_log-sociedad  = p_entrada-bukrs.
        wa_log-proveedor = p_entrada-lifnr.
        wa_log-fecha     = p_entrada-bldat.

        ls_xmsg-msgty = 'E'.
**      lv_clase = 'E'. "ls_xmsg-msgid.
**      lv_id    = ls_xmsg-msgno.
        CLEAR lv_mensaje.

        CALL FUNCTION 'MASS_MESSAGE_GET'
          EXPORTING
*           SPRSL             = SY-LANGU
            arbgb             = ls_xmsg-msgid
            msgnr             = ls_xmsg-msgno
            msgv1             = ls_xmsg-msgv1
            msgv2             = ls_xmsg-msgv2
            msgv3             = ls_xmsg-msgv3
            msgv4             = ls_xmsg-msgv4
          IMPORTING
            msgtext           = lv_mensaje
          EXCEPTIONS
            message_not_found = 1
            OTHERS            = 2.

        CONCATENATE 'Error al contabilizar FBV0 ' lv_mensaje INTO wa_log-mensaje
                                                  SEPARATED BY space.

**      PERFORM generar_log USING p_entrada '012' lv_mensaje.
        APPEND wa_log TO ti_log.
        p_entrada-status = '8'.

**//..
        IF ls_handle IS INITIAL.
          me->crear_log( EXPORTING p_entrada = p_entrada
                            CHANGING  c_bal_log = ls_bal_log c_handle = ls_handle ).
          me->add_log( EXPORTING u_handle = ls_handle
                                u_xmsg =  ls_xmsg ).
        ELSE.
          me->add_log( EXPORTING u_handle = ls_handle
                             u_xmsg =  ls_xmsg ).
        ENDIF.

        AT LAST.
          me->save_log( EXPORTING u_handle = ls_handle ).
        ENDAT.
      ENDLOOP.

      IF lv_posted EQ 'X'.
        READ TABLE xmsg INTO ls_xmsg WITH KEY posted = lv_posted.
        IF sy-subrc EQ 0.
          wa_log-estado    = icon_green_light.
          wa_log-documento = p_entrada-xblnr.
          wa_log-sociedad  = p_entrada-bukrs.
          wa_log-proveedor = p_entrada-lifnr.
          wa_log-fecha     = p_entrada-bldat.

          CLEAR lv_mensaje.
          CALL FUNCTION 'MASS_MESSAGE_GET'
            EXPORTING
*             SPRSL             = SY-LANGU
              arbgb             = ls_xmsg-msgid
              msgnr             = ls_xmsg-msgno
              msgv1             = ls_xmsg-msgv1
              msgv2             = ls_xmsg-msgv2
              msgv3             = ls_xmsg-msgv3
              msgv4             = ls_xmsg-msgv4
            IMPORTING
              msgtext           = lv_mensaje
            EXCEPTIONS
              message_not_found = 1
              OTHERS            = 2.

          CONCATENATE 'Documento contabilizado FBV0 ' p_entrada-belnr '/' p_entrada-gjahr INTO wa_log-mensaje
                                                       SEPARATED BY space.
*          PERFORM generar_log USING p_entrada 'S' '012' lv_mensaje.
          CLEAR  lv_mesn.
          lv_mesn = lv_mensaje.
          me->generar_log( EXPORTING p_entrada = p_entrada   p_msgty = 'S' p_numero = '012' p_variable = lv_mesn ).

          APPEND wa_log TO ti_log.

          CALL FUNCTION 'DEQUEUE_EFBKPF'
            EXPORTING
              belnr = p_entrada-belnr
              bukrs = p_entrada-bukrs
              gjahr = p_entrada-gjahr.
          p_entrada-status = '5'.
          p_entrada-glosa = TEXT-gl2.

*          PERFORM generar_aprob TABLES lt_t001z  lt_lfa1 USING  wa_log-mensaje '012' CHANGING p_entrada.
          me->generar_aprob( EXPORTING lt_paval = lt_t001z  lt_lfa1 =  lt_lfa1  p_glosa = wa_log-mensaje  p_numero = '012' CHANGING p_entrada = p_entrada ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


METHOD generar_log.
    DATA: lv_s_log  TYPE bal_s_log,
          lv_handle TYPE balloghndl,
          lv_stcd1  TYPE lfa1-stcd1.


    DATA: lv_s_msg TYPE bal_s_msg.

    IF p_msgty IS INITIAL.
      lv_s_msg-msgty = 'E'.
    ELSE.
      lv_s_msg-msgty = p_msgty.
    ENDIF.

    lv_s_msg-msgid = 'ZDTE_0001'.
    lv_s_msg-msgno = p_numero.
    lv_s_msg-msgv1 = p_variable.

    CLEAR lv_s_log.

    CONCATENATE p_entrada-bukrs p_entrada-lifnr p_entrada-xblnr p_entrada-tipodte INTO lv_s_log-extnumber.
    lv_s_log-object     = 'ZDTE'.
    lv_s_log-subobject  = 'RECEPCION'.
    lv_s_log-aldate     = syst-datum.
    lv_s_log-altime     = syst-uzeit.
    lv_s_log-aluser     = syst-uname.
    lv_s_log-alprog     = syst-repid.
    "----->se buscan los anteriores.
    DATA: lt_log_header TYPE balhdr_t,
          lv_log_filter TYPE bal_s_lfil.

    DATA:r_external  TYPE bal_r_extn,
         r_object    TYPE bal_r_obj,
         r_subobject TYPE bal_r_sub.

    DATA: ln_external  LIKE LINE OF r_external,
          ln_object    LIKE LINE OF r_object,
          ln_subobject LIKE LINE OF r_subobject.
    CLEAR:  ln_external,
            ln_object,
            ln_subobject.

    FREE: r_external,
          r_object,
          r_subobject.

    ln_external-sign = ln_object-sign = ln_subobject-sign = 'I'.
    ln_external-option = ln_object-option = ln_subobject-option = 'EQ'.
    ln_external-low  = lv_s_log-extnumber.
    ln_object-low    = 'ZDTE'.
    ln_subobject-low = 'RECEPCION'.

    APPEND ln_external TO r_external.
    APPEND ln_object TO r_object.
    APPEND ln_subobject TO r_subobject.

    lv_log_filter-extnumber[] = r_external[].
    lv_log_filter-object[] = r_object[].
    lv_log_filter-subobject[] = r_subobject[].
    FREE lt_log_header.


    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING
        i_s_log_filter     = lv_log_filter
      IMPORTING
        e_t_log_header     = lt_log_header
      EXCEPTIONS
        log_not_found      = 1
        no_filter_criteria = 2
        OTHERS             = 3.
    IF sy-subrc = 0 AND lt_log_header[] IS NOT INITIAL.
      CALL FUNCTION 'BAL_DB_DELETE'
        EXPORTING
          i_t_logs_to_delete = lt_log_header.
    ENDIF.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = lv_s_log
      IMPORTING
        e_log_handle = lv_handle.

    IF sy-subrc = 0 AND lv_handle IS NOT INITIAL.

      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle = lv_handle
          i_s_msg      = lv_s_msg.
      IF sy-subrc = 0.
        DATA: lt_log_input TYPE bal_t_logh.

        FREE lt_log_input.
        APPEND lv_handle TO lt_log_input.

        CALL FUNCTION 'BAL_DB_SAVE'
          EXPORTING
            i_client       = sy-mandt
            i_save_all     = 'X'
            i_t_log_handle = lt_log_input.
      ENDIF.

    ENDIF.
  ENDMETHOD.


METHOD generar_miro_mir4.
    TYPES: r_doc TYPE RANGE OF bkpf-belnr,
           r_gja TYPE RANGE OF bkpf-gjahr,
           r_us  TYPE RANGE OF usnam.

    DATA: s_bukrs  TYPE r_bukrs,
          ls_bukrs TYPE LINE OF r_bukrs,
          s_doc    TYPE r_doc,
          ls_doc   TYPE LINE OF r_doc,
          s_gjahr  TYPE r_gja,
          ls_gjahr TYPE LINE OF r_gja,
          s_user   TYPE r_us,
          ls_user  TYPE LINE OF r_us.


    DATA: lt_msg_handle   TYPE  bal_t_msgh,
          ln_msg_handle   LIKE LINE OF lt_msg_handle,
          lt_log_handle   TYPE  bal_t_logh,
          lt_log_handle_e TYPE  bal_t_logh,
          lv_mensaje      TYPE bal_s_msg.

    CLEAR: ls_bukrs,
           ls_doc,
           ls_gjahr.

    FREE: s_bukrs,
          s_doc,
          s_gjahr.

    ls_bukrs-sign = 'I'.
    ls_bukrs-option = 'EQ'.
    ls_bukrs-low = p_entrada-bukrs.
    APPEND ls_bukrs TO s_bukrs .

    ls_doc-sign = 'I'.
    ls_doc-option = 'EQ'.
    ls_doc-low = p_entrada-belnr.
    APPEND ls_doc TO s_doc.

    ls_gjahr-sign = 'I'.
    ls_gjahr-option = 'EQ'.
    ls_gjahr-low = p_entrada-gjahr.
    APPEND ls_gjahr TO s_gjahr.



    CLEAR s_user.
    FREE s_user.


    SUBMIT rmbabg00
               WITH so_bukrs IN s_bukrs
               WITH so_belnr IN s_doc
               WITH so_gjahr IN s_gjahr
               "WITH pa_test = 'X'
               WITH so_usnam IN s_user
               "USER sy-uname
               EXPORTING LIST TO MEMORY
               AND RETURN.



    DATA: lv_rbkp TYPE rbkp.

    SELECT SINGLE *
    FROM rbkp
    INTO lv_rbkp
    WHERE belnr = p_entrada-belnr AND
          gjahr = p_entrada-gjahr.
    IF sy-dbcnt = 1.
      IF lv_rbkp-rbstat = '3'.
        wa_log-estado = icon_red_light.
        wa_log-documento = p_entrada-xblnr.
        wa_log-mensaje = ''.
        REFRESH  lt_msg_handle.CLEAR  ln_msg_handle.CLEAR lv_mensaje.
        CALL FUNCTION 'MRM_APPL_LOG_DB_READ'
          EXPORTING
            i_belnr            = p_entrada-belnr
            i_gjahr            = p_entrada-gjahr
            i_not_load_msg     = space
          IMPORTING
*           TE_LOG_HANDLE      =
            te_msg_handle      = lt_msg_handle "lt_log_mrm
*   CHANGING
*           X_S_LOG_FILTER     =
          EXCEPTIONS
            load_error         = 1
            log_already_loaded = 2
            OTHERS             = 3.

        IF lt_msg_handle[] IS NOT INITIAL.

          LOOP AT lt_msg_handle INTO ln_msg_handle.

            CALL FUNCTION 'BAL_LOG_MSG_READ'
              EXPORTING
                i_s_msg_handle = ln_msg_handle
*               I_LANGU        = SY-LANGU
              IMPORTING
                e_s_msg        = lv_mensaje.
            IF sy-subrc = 0 AND
               ( lv_mensaje-msgty = 'E' OR lv_mensaje-msgty = 'A' ).
              MESSAGE ID lv_mensaje-msgid TYPE lv_mensaje-msgty NUMBER lv_mensaje-msgno
                   WITH lv_mensaje-msgv1 lv_mensaje-msgv2 lv_mensaje-msgv3 lv_mensaje-msgv4  INTO wa_log-mensaje.
            ENDIF.
          ENDLOOP.
        ENDIF.
        IF wa_log-mensaje = '' OR wa_log-mensaje IS INITIAL.
          wa_log-mensaje = 'Error en Contabilización'.
        ENDIF.
        IF lv_mensaje-msgid ='M8' AND lv_mensaje-msgno = '183'. "Si no se encuentra Hoja servicio.
*        Perform validar_HES.
        ENDIF.
        wa_log-sociedad = p_entrada-bukrs.
        wa_log-proveedor = p_entrada-lifnr.
        wa_log-fecha = p_entrada-bldat.
        p_entrada-status = '8'.
        APPEND wa_log TO ti_log.
*        PERFORM generar_log USING p_entrada 'E' '007' wa_log-mensaje.
        me->generar_log( EXPORTING p_entrada = p_entrada   p_msgty = 'E' p_numero = '007' p_variable = wa_log-mensaje ).
        me->liberacion_mm( EXPORTING p_belnr = p_entrada-belnr p_gjahr = p_entrada-gjahr p_bukrs =  p_entrada-bukrs ).
      ELSE.
        wa_log-estado = icon_green_light.
        wa_log-documento = p_entrada-xblnr.
        wa_log-sociedad = p_entrada-bukrs.
        CONCATENATE 'Documento Contabilizado: ' p_entrada-belnr '/' p_entrada-gjahr INTO wa_log-mensaje.
        wa_log-proveedor = p_entrada-lifnr.
        wa_log-fecha = p_entrada-bldat.
        p_entrada-status = '5'.
        p_entrada-glosa = TEXT-gl2.
        APPEND wa_log TO ti_log.
*        PERFORM generar_log USING p_entrada 'S' '007' wa_log-mensaje.
        me->generar_log( EXPORTING p_entrada = p_entrada   p_msgty = 'S' p_numero = '007' p_variable = space ).
        "----->se libera el documento para pago automáticamente.
        "CHECK p_servicio = 'X'.
*      IF p_entrada-wrbtr <= 10000.
*        DATA: lv_belnr TYPE bseg-belnr,
*              lv_bukrs TYPE bseg-bukrs,
*              lv_gjahr TYPE bseg-gjahr,
*              lv_buzei TYPE bseg-buzei.
*
*        CLEAR: lv_belnr,
*               lv_bukrs,
*               lv_gjahr,
*               lv_buzei.
*        DATA: lv_awkey TYPE bkpf-awkey.
*        CLEAR lv_awkey.
*
*        CONCATENATE p_entrada-belnr p_entrada-gjahr INTO lv_awkey.
*
*        SELECT SINGLE belnr
*        FROM bkpf
*        INTO lv_belnr
*        WHERE awtyp NE space AND
*               awkey = lv_awkey.
*        IF sy-dbcnt = 0.
*          CLEAR lv_belnr.
*        ENDIF.
*
*        lv_bukrs = p_entrada-bukrs.
*        lv_gjahr = p_entrada-gjahr.
*        lv_buzei = '001'.
*
*        CALL FUNCTION 'PR_WF_PAYMENT_BLOCK_RESET'
*          EXPORTING
*            belnr        = lv_belnr
*            bukrs        = lv_bukrs
*            gjahr        = lv_gjahr
*            buzei        = lv_buzei
*          EXCEPTIONS
*            foreign_lock = 1
*            bseg_lock    = 2
*            bkpf_read    = 3
*            bseg_update  = 4
*            bsik_update  = 5
*            bsik_open_fi = 6
*            bsid_update  = 7
*            bsid_open_fi = 8
*            OTHERS       = 9.
*        IF sy-subrc <> 0.
*          wa_log-estado = icon_red_light.
*          wa_log-documento = p_entrada-xblnr.
*          wa_log-sociedad = p_entrada-bukrs.
*          CONCATENATE 'Documento Contabilizaco: ' p_entrada-belnr '/' p_entrada-gjahr space text-500 INTO wa_log-mensaje.
*          wa_log-proveedor = p_entrada-lifnr.
*          wa_log-fecha = p_entrada-bldat.
*        ELSE.
*          wa_log-estado = icon_green_light.
*          wa_log-documento = p_entrada-xblnr.
*          wa_log-sociedad = p_entrada-bukrs.
*          CONCATENATE 'Documento Contabilizado: ' p_entrada-belnr '/' p_entrada-gjahr space text-501 INTO wa_log-mensaje.
*          wa_log-proveedor = p_entrada-lifnr.
*          wa_log-fecha = p_entrada-bldat.
*        ENDIF.
*        append wa_log to ti_log.
*        PERFORM generar_log USING p_entrada
*                     '007' wa_log-mensaje.
*      ENDIF.
        "---->fin liberación de pago
      ENDIF.
    ENDIF.

    IF p_entrada-status = '5'.
      me->generar_aprob( EXPORTING lt_paval = lt_t001z  lt_lfa1 =  lt_lfa1  p_glosa = wa_log-mensaje  p_numero = '012' CHANGING p_entrada = p_entrada ).
    ENDIF.

  ENDMETHOD.


METHOD generar_miro_mir7.
    DATA: lv_invoice TYPE bapi_incinv_fld-inv_doc_no,
          lv_fiscal  TYPE bapi_incinv_fld-fisc_year.
    DATA: lt_return TYPE STANDARD TABLE OF bapiret2,
          ls_return TYPE  bapiret2.

    CLEAR ls_return.
    FREE lt_return.

    lv_invoice = p_entrada-belnr.
    lv_fiscal = p_entrada-gjahr.

    CALL FUNCTION 'BAPI_INCOMINGINVOICE_POST'
      EXPORTING
        invoicedocnumber = lv_invoice
        fiscalyear       = lv_fiscal
      TABLES
        return           = lt_return.

    READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
    IF sy-subrc = 0.
      wa_log-estado = icon_red_light.
      wa_log-documento = p_entrada-xblnr.
      wa_log-mensaje = ls_return-message.
      wa_log-sociedad = p_entrada-bukrs.
      wa_log-proveedor = p_entrada-lifnr.
      wa_log-fecha = p_entrada-bldat.
      p_entrada-status = '8'.
      APPEND wa_log to ti_log.

*      PERFORM generar_log USING p_entrada 'E' '007' ls_return-message.

      me->generar_log( EXPORTING p_entrada = p_entrada   p_msgty = 'E' p_numero = '007' p_variable =  wa_log-mensaje ).
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      wa_log-estado = icon_green_light.
      wa_log-documento = p_entrada-xblnr.
      wa_log-mensaje = ls_return-message.
      wa_log-sociedad = p_entrada-bukrs.
      wa_log-proveedor = p_entrada-lifnr.
      wa_log-fecha = p_entrada-bldat.
      APPEND wa_log to ti_log.
      p_entrada-status = '5'.
      p_entrada-glosa = TEXT-gl2.

*      PERFORM generar_log USING p_entrada 'S' '007' ls_return-message.
      me->generar_log( EXPORTING p_entrada = p_entrada   p_msgty = 'S' p_numero = '007' p_variable = space ).
*      PERFORM generar_aprob TABLES lt_t001z  lt_lfa1 USING  wa_log-mensaje '012' CHANGING p_entrada.
      me->generar_aprob( EXPORTING lt_paval = lt_t001z  lt_lfa1 =  lt_lfa1  p_glosa = wa_log-mensaje  p_numero = '012' CHANGING p_entrada = p_entrada ).
    ENDIF.
  ENDMETHOD.


METHOD generar_rechazo.
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
      CALL FUNCTION 'ZFI_0028DTE'
        EXPORTING
          i_lifnr   = p_entrada-lifnr
          i_bukrs   = p_entrada-bukrs
          i_xblnr   = p_entrada-xblnr
          i_tipodte = p_entrada-tipodte.

    ENDIF.

  ENDMETHOD.


METHOD grabar.
    FREE old_ztfi_0074.
    CLEAR ls_old_ztfi_0074.
    FREE new_ztfi_0074.
    CLEAR ls_new_ztfi_0074.
    wa_objectid = wa_recfactprov+0(36).
    CLEAR: old_ztfi_0074[], new_ztfi_0074[].
    APPEND wa_recfactprov TO new_ztfi_0074.
    SELECT * FROM ztfi_0074 INTO TABLE old_ztfi_0074
             WHERE bukrs EQ wa_recfactprov-bukrs
               AND xblnr EQ wa_recfactprov-xblnr
               AND lifnr EQ wa_recfactprov-lifnr
               AND tipodte EQ wa_recfactprov-tipodte.
    IF sy-subrc EQ 0.
      READ TABLE old_ztfi_0074 into ls_old_ztfi_0074 INDEX 1.
      IF wa_recfactprov-status NE ls_old_ztfi_0074-status OR
         wa_recfactprov-belnr  NE ls_old_ztfi_0074-belnr OR
         wa_recfactprov-gjahr  NE ls_old_ztfi_0074-gjahr OR
         wa_recfactprov-aprobador_real NE ls_old_ztfi_0074-aprobador_real.
        CALL FUNCTION 'ZTFI_0074_WRITE_DOCUMENT'
          IN UPDATE TASK
          EXPORTING
            objectid      = wa_objectid
            tcode         = sy-tcode "'ZFI_0011'
            utime         = sy-uzeit
            udate         = sy-datum
            username      = sy-uname
            upd_ztfi_0074 = 'U'
          TABLES
            xztfi_0074    = new_ztfi_0074
            yztfi_0074    = old_ztfi_0074.
      ENDIF.
    ENDIF.
    UPDATE ztfi_0074 FROM  wa_recfactprov.
    COMMIT WORK AND WAIT.
  ENDMETHOD.


METHOD inilog.
    CLEAR wa_log.
    wa_log-documento = ls_recfactprov-xblnr.
    wa_log-sociedad = ls_recfactprov-bukrs.
    wa_log-proveedor = ls_recfactprov-lifnr.
    wa_log-fecha = ls_recfactprov-bldat.
  ENDMETHOD.


METHOD liberacion_mm.
    DATA: lt_bdcdata           TYPE TABLE OF bdcdata,
          lr_belnr             TYPE RANGE OF belnr,
          ls_belnr             LIKE LINE OF lr_belnr, "IBKK_R_BELNR.
          ls_opt               TYPE ctu_params,
          lt_mensaje           TYPE TABLE OF bdcmsgcoll,
          ls_mensaje           TYPE bdcmsgcoll,
          lv_fnam              TYPE bdcdata-fnam,
          lv_sign              TYPE rsdsselopt-sign VALUE 'I',
          lv_first_time        TYPE flag,
          lv_row_number        TYPE n LENGTH 2,

          lc_belnr             TYPE belnr_d,
          lv_mensaje           TYPE char200,
          lv_documento_mensaje TYPE char200.

    SELECT belnr INTO lc_belnr  FROM rbkp UP TO 1 ROWS
         WHERE belnr EQ p_belnr
           AND gjahr EQ p_gjahr
           AND rbstat EQ '3'.
    ENDSELECT.
    IF sy-subrc NE 0.
    ELSE.
      lt_bdcdata = VALUE #( BASE lt_bdcdata ( program  = 'SAPMM08N' dynpro = '0100' dynbegin = 'X' )
                                ( fnam = 'BDC_CURSOR'   fval = 'SO_BELNR-LOW' )
                                ( fnam =  'SO_BELNR-LOW' fval = p_belnr )
                                ( fnam = 'SO_GJAHR-LOW' fval = p_gjahr )
                                ( fnam = 'SO_USNAM-LOW' fval = '' )
                                ( fnam = 'SO_LIFNR-LOW' fval = '' )
                                ( fnam = 'SO_BUKRS-LOW' fval = p_bukrs )
                                ( fnam = 'P_IV_BG'      fval = abap_true )
                                ( fnam = 'BDC_CURSOR'   fval = 'SO_BELNR-LOW' )
                                ).

      lt_bdcdata = VALUE #( BASE lt_bdcdata ( program  = 'SAPMM08N' dynpro = '0100' dynbegin = 'X' )
                            ( fnam = 'BDC_CURSOR'   fval = 'P_ST_BG' )
                            ( fnam = 'BDC_OKCODE'   fval = '=STPO' )
                            ( fnam = 'P_ST_BG'      fval = abap_false )

                            ( fnam = 'P_ST_ERR'     fval = abap_true )

                            ( fnam = 'BDC_OKCODE'   fval = '=CRET'   )

                            ( program  = 'SAPMM08N' dynpro = '0201' dynbegin = 'X' )
                            ( fnam = 'BDC_OKCODE'   fval   = '=ASEL'   )

                            ( program  = 'SAPMM08N' dynpro = '0201' dynbegin = 'X' )
                            ( fnam = 'BDC_OKCODE'   fval   = '=ASBG'   )

                            ( program  = 'SAPMM08N' dynpro = '0201' dynbegin = 'X' )
                            ( fnam = 'BDC_OKCODE'   fval   = '=BACK'   )

                            ( program  = 'SAPMM08N' dynpro = '0100' dynbegin = 'X' )
                            ( fnam = 'BDC_OKCODE'   fval   = '/ECBAC'   )
                           ).

      ls_opt-dismode = 'N'.

      TRY.
          CALL TRANSACTION 'MIR6' USING lt_bdcdata OPTIONS FROM ls_opt
                                  MESSAGES INTO lt_mensaje.



        CATCH cx_sy_authorization_error ##NO_HANDLER.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


METHOD liberarobj.
  DATA: co_type TYPE string.
  DATA: ls_0074 TYPE ztfi_0074.
  DATA: t_0078 TYPE STANDARD TABLE OF ztfi_0078.
  DATA: wa_0078 TYPE ztfi_0078.
  CLEAR co_type. CLEAR wa_0078. REFRESH t_0078.

  READ TABLE lt_zcb_recfactprov INTO ls_0074 INDEX 1.

  SELECT SINGLE * INTO  wa_0078 FROM ztfi_0078
     WHERE bukrs EQ ls_0074-bukrs.


  CLEAR  co_type.
  SELECT SINGLE nombre_int INTO co_type FROM ztfi_0078b
     WHERE vatint = wa_0078-vatint
     AND   codint = '0005'.

  CHECK co_type IS NOT INITIAL.

  CALL FUNCTION co_type. "'ZFI_0008FDTE'.
ENDMETHOD.


METHOD llamabapis.
    CASE lv_trx.
      WHEN 'MIR4'.
        me->generar_miro_mir4( EXPORTING lt_t001z = lt_t001z  lt_lfa1 = lt_lfa1 p_servicio = lv_servicio
                            CHANGING p_entrada = wa_recfactprov ).
      WHEN 'MIR7'.
        me->generar_miro_mir7( EXPORTING lt_t001z = lt_t001z  lt_lfa1 = lt_lfa1
                              CHANGING p_entrada = wa_recfactprov ).
      WHEN 'FBV3'.
        me->generar_fbv1( EXPORTING  lt_t001z = lt_t001z  lt_lfa1 = lt_lfa1
                              CHANGING p_entrada = wa_recfactprov ).
    ENDCASE.
    MODIFY  lt_zcb_recfactprov FROM wa_recfactprov  INDEX lv_registro.
  ENDMETHOD.


METHOD save_log.
    DATA: lt_log_input TYPE bal_t_logh.

    FREE lt_log_input.
    APPEND u_handle TO lt_log_input.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_client       = sy-mandt
        i_save_all     = 'X'
        i_t_log_handle = lt_log_input.
  ENDMETHOD.


METHOD validacionbloqest.
    DATA: ls_lfb1 TYPE lfb1.
    CLEAR:  ls_enq,  ls_lfb1.
    res = 0.

    READ TABLE lt_lfb1 INTO ls_lfb1 WITH KEY lifnr = wa_recfactprov-lifnr
                              bukrs = wa_recfactprov-bukrs.
    IF sy-subrc = 0.
      IF ls_lfb1-zzdte_tipo NE '2'. "todos lo que no son FI
        CLEAR lv_doc.CLEAR resulta.CLEAR numero_enq.
        CONCATENATE sy-mandt wa_recfactprov-belnr  wa_recfactprov-gjahr INTO lv_doc.
        CALL FUNCTION 'ENQUE_READ'
          EXPORTING
            gclient = sy-mandt
            gname   = 'RBKP'
            garg    = lv_doc
*           GUNAME  = SY-UNAME
          IMPORTING
            number  = numero_enq
            subrc   = resulta
          TABLES
            enq     = t_enq.

        IF resulta = 0.
          READ TABLE t_enq INTO ls_enq WITH KEY gname = 'RBKP'.
          IF sy-subrc = 0.
            wa_log-estado    = icon_red_light.
            wa_recfactprov-status = '8'.
            MODIFY  lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.
            MESSAGE e063(zdte_0001) INTO wa_log-mensaje.
            APPEND wa_log TO ti_log.
*            PERFORM generar_log USING wa_recfactprov 'E' '007' wa_log-mensaje.
            me->generar_log( EXPORTING p_entrada = wa_recfactprov   p_msgty = 'E' p_numero = '063' p_variable = space ).
            me->grabar( ).
            res = 4.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


METHOD validaciones.
    DATA: ls_lfa1 TYPE lfa1,
          ls_lfb1 TYPE lfb1,
          ls_0082 TYPE ztfi_0082,
          ls_fe   TYPE LINE OF r_fec.

    CLEAR: ls_0082, ls_lfa1, ls_lfb1, ls_fe.
    CLEAR lv_valida_prov.
    CLEAR lv_servicio.
    "--->Validación periodo.

    IF sy-batch EQ 'X'.
      READ TABLE lt_ztfi_0082 INTO ls_0082 WITH KEY bukrs = wa_recfactprov-bukrs.
      IF sy-subrc = 0.
        LOOP AT  lt_ztfi_0082 INTO ls_0082.
          CLEAR ls_fe.
          ls_fe-sign = 'I'.
          ls_fe-option = 'BT'.
          ls_fe-low = ls_0082-begda.
          ls_fe-high = ls_0082-endda.
          APPEND ls_fe TO r_fecha.
          IF sy-datum  IN  r_fecha.
            MESSAGE e017(zdte_0001) INTO wa_log-mensaje.
            me->generar_log( EXPORTING p_entrada = wa_recfactprov   p_msgty = 'E' p_numero = '017' p_variable = space ).
            wa_log-estado = icon_red_light.
            wa_log-documento = wa_recfactprov-xblnr.
            wa_log-mensaje = wa_log-mensaje.
            wa_log-sociedad = wa_recfactprov-bukrs.
            wa_log-proveedor = wa_recfactprov-lifnr.
            wa_log-fecha = wa_recfactprov-bldat.
            APPEND wa_log TO ti_log.
            res = 4.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    "---->validación proveedor.
    READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = wa_recfactprov-lifnr.
    IF sy-subrc NE 0.
      wa_log-estado = icon_red_light.

      MESSAGE e000(zdte_0001) WITH wa_recfactprov-bukrs INTO wa_log-mensaje.
      me->generar_rechazo( EXPORTING lt_paval = lt_t001z   lt_lfa1 = lt_lfa1  p_glosa = wa_log-mensaje p_numero = '000'
                           CHANGING p_entrada = wa_recfactprov ).
      APPEND wa_log TO ti_log.
      MODIFY  lt_zcb_recfactprov  FROM  wa_recfactprov INDEX lv_registro.
      res = 4.
    ELSE.
      "------ validación proveedor
      READ TABLE lt_lfb1 INTO ls_lfb1 WITH KEY lifnr = ls_lfa1-lifnr
                                  bukrs = wa_recfactprov-bukrs.
      IF sy-subrc = 0.
        IF ls_lfb1-zzdte_tipo = '1' OR ls_lfb1-zzdte_tipo = '3' OR ls_lfb1-zzdte_tipo = '4'.
          lv_valida_prov = 'X'.
        ELSEIF ls_lfb1-zzdte_tipo = '2'.
          lv_valida_prov = 'C'.
        ENDIF.
      ELSE.
        wa_log-estado = icon_red_light.
        MESSAGE e000(zdte_0001) WITH wa_recfactprov-bukrs INTO wa_log-mensaje.
        me->generar_rechazo( EXPORTING lt_paval = lt_t001z   lt_lfa1 = lt_lfa1  p_glosa = wa_log-mensaje p_numero = '000'
                          CHANGING p_entrada = wa_recfactprov ).
        APPEND wa_log TO ti_log.
        MODIFY  lt_zcb_recfactprov  FROM  wa_recfactprov INDEX lv_registro.
        res = 4.
      ENDIF.
    ENDIF.

    IF res NE 0.
      me->grabar( ).
      me->desbloqueo( ).
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
