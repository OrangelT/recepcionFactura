class ZCL_DTE_FI0057V2 definition
  public
  create public .

public section.

  types:
    T_0090 TYPE STANDARD TABLE OF ztfi_0090 .
  types:
    T_LFB1 TYPE STANDARD TABLE OF lfb1 .
  types:
    T_0092 TYPE STANDARD TABLE OF ztfi_0092 .
  types:
    T_0093 TYPE STANDARD TABLE OF ztfi_0093 .
  types:
    T_0077 TYPE STANDARD TABLE OF ztfi_0077 .
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

  data LG_FECHACIERRE type SY-DATUM .
  data:
    LT_LFBW type STANDARD TABLE OF LFBW .
  data GS_VARINT type ZDTE_VARIANT .
  data:
    ti_log TYPE STANDARD TABLE OF ty_log .
  data WA_LOG type TY_LOG .
  data:
    gt_t001 TYPE STANDARD TABLE OF t001 .
  data GS_T001 type T001 .
  data:
    ti_ztfi_0083 TYPE STANDARD TABLE OF ztfi_0083 .
  data WA_FI_0083 type ZTFI_0083 .
  data:
    ti_vbwf16 TYPE STANDARD TABLE OF vbwf16 .
  data WA_VBWF16 type VBWF16 .
  data:
    lt_t001 TYPE STANDARD TABLE OF ty_t001 .
  data:
    ti_ztfi_0085 TYPE STANDARD TABLE OF ztfi_0085 .
  data T_BSTYP type EKKO-BSTYP .
  data T_PSTYP type EKPO-PSTYP .
  data:
    lt_zcb_recfactprov TYPE STANDARD TABLE OF ztfi_0074 .
  data LS_RECFACTPROV type ZTFI_0074 .
  data WA_RECFACTPROV type ZTFI_0074 .
  data LS_POS type ZTFI_0075 .
  data:
    lt_ekko TYPE STANDARD TABLE OF ekko .
  data:
    lt_lfb1_oc TYPE STANDARD TABLE OF lfb1 .
  data:
    lt_ekpo TYPE STANDARD TABLE OF ekpo .
  data:
    lt_ekkn TYPE STANDARD TABLE OF ekkn .
  data:
    lt_afko TYPE STANDARD TABLE OF afko .
  data:
    lt_proj TYPE STANDARD TABLE OF proj .
  data:
    lt_ztfi_0090 TYPE STANDARD TABLE OF ztfi_0090 .
  data:
    lt_ztfi_0093 TYPE STANDARD TABLE OF ztfi_0093 .
  data:
    lt_ztfi_0092 TYPE STANDARD TABLE OF ztfi_0092 .
  data LS_ZTFI_0089 type ZTFI_0089 .
  data LV_TABIX type SYTABIX .
  data WA_OBJECTID type CDHDR-OBJECTID .
  data:
    old_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074 .
  data:
    new_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074 .
  data:
    lt_zdte_posfact TYPE STANDARD TABLE OF ztfi_0075 .
  data:
    lt_zdte_posfact_oc TYPE STANDARD TABLE OF ztfi_0075 .
  data:
    lt_zdte_cldoc TYPE STANDARD TABLE OF ztfi_0001b .
  data:
    lt_zdt_recref TYPE STANDARD TABLE OF ztfi_0077 .
  data WA_ZDT_RECREF type ZTFI_0077 .
  data:
    lt_t001z TYPE STANDARD TABLE OF t001z .
  data:
    lt_lfa1 TYPE STANDARD TABLE OF lfa1 .
  data:
    lt_lfb1 TYPE STANDARD TABLE OF lfb1 .
  data:
    lt_zdte_dias TYPE STANDARD TABLE OF ztfi_0076 .
  data:
    lt_zdte_valida TYPE STANDARD TABLE OF ztfi_0078 .
  data:
    lt_ekko_lim TYPE STANDARD TABLE OF ekko .
  data:
    lt_ekpo_lim TYPE STANDARD TABLE OF ekpo .
  data:
    lt_ekkn_lim TYPE STANDARD TABLE OF ekkn .
  data:
    lt_afko_lim TYPE STANDARD TABLE OF afko .
  data:
    lt_proj_lim TYPE STANDARD TABLE OF proj .
  data:
    lt_ekbe TYPE STANDARD TABLE OF ekbe .
  data:
    ti_essr      TYPE STANDARD TABLE OF zty_ess .
  data:
    ti_ekko_essr TYPE STANDARD TABLE OF ekko .
  data:
    r_lblni TYPE RANGE OF essr-lblni .
  data:
    ti_ekpo_essr TYPE STANDARD TABLE OF ekpo .
  data S_TIPODTE type ZFI_TIPO .
  data LV_REGISTRO like SY-TABIX .
  data LV_REGISTRO_POS like SY-TABIX .
  data LV_FLAG_LFB1_OC type C .
  data LV_OC_LIMITE type EKKO-EBELN .
  data LV_LIM_LIFNR type EKKO-LIFNR .
  data LV_LIM_BUKRS type EKKO-BUKRS .
  data:
    wa_lt_ztfi_0075 LIKE LINE OF lt_zdte_posfact .
  data MSGID type MSGID .
  data MSGTY type MSGTY .
  data MSGNO type SYMSGNO .
  data MSGV1 type MSGV1 .
  data MSGV2 type MSGV2 .
  data MSGV3 type MSGV3 .
  data MSGV4 type MSGV4 .
  data ESTATUS type CHAR01 .
  data E_REPID type REPID .
  data:
    rg_ekorg TYPE RANGE OF ekko-ekorg .
  data:
    rg_ekgrp TYPE RANGE OF ekko-ekgrp .
  data:
    rg_bedat TYPE RANGE OF ekko-bedat .
  data RES type SY-SUBRC .
  data:
    ti_ztfi_0095 TYPE TABLE OF ztfi_0095 .
  data:
    lt_docele TYPE STANDARD TABLE OF ztfi_0074 .
  data WA_DOCELE type ZTFI_0074 .
  data:
    lt_factmm TYPE STANDARD TABLE OF zcds_factmm .
  data WA_FACTMM type ZCDS_FACTMM .
  data S_STATUS type R_STA .
  data S_BUKRS type R_BUKRS .
  data S_BLDAT type R_BLDAT .
  data S_FOLIO type R_FOLIO .
  data CNUM type MSGNO value '000' ##NO_TEXT.

  methods CURRENCY_CONV
    importing
      value(P_WAERS) type WAERS optional
      value(P_MNTTO) type ZMNTNETO optional
    changing
      value(P_LV_AMOUNT) type BAPICURR_D optional .
  methods GENERAR_MIR7
    importing
      value(LT_ZDTE_CLDOC) type T_ZDTE_CLDOC optional
      value(LT_EKKO) type T_EKKO optional
      value(LT_EKPO) type T_EKPO optional
      value(LT_EKKN) type T_EKKN optional
      value(LT_AFKO) type T_AFKO optional
      value(LT_PROJ) type T_PROJ optional
      value(P_OC) type EBELN optional
    changing
      value(P_ENTRADA) type ZTFI_0074 optional .
  methods INDIVA_DEFECTO
    importing
      value(T_ZTFI_0093) type T_0093 optional
      value(LV_CDSII) type ZFI_DTECDSII optional
      value(P_ENTRADA) type ZTFI_0074 optional
    changing
      value(P_LT_TAX_DATA_TAX_CODE) type MWSKZ optional
      value(P_ERROR) type INT4 optional .
  methods GENERAR_FBV1_BAPI
    importing
      value(T_EKPO) type T_EKPO optional
      value(T_ZTFI_0075) type T_0075 optional
      value(T_EKKN) type T_EKKN optional
      value(T_AFKO) type T_AFKO optional
      value(T_PROJ) type T_PROJ optional
      value(T_LFB1) type T_LFB1 optional
      value(T_ZDTE_CLDOC) type T_ZDTE_CLDOC optional
      value(T_ZTFI_0090) type T_0090 optional
      value(T_ZTFI_0092) type T_0092 optional
      value(T_ZTFI_0093) type T_0093 optional
      value(T_ZTFI_0077) type T_0077 optional
      value(U_TIPODTE) type ZFI_TIPO optional
      value(U_ESTATUS) type ZSTATDTE optional
    changing
      value(C_RECFACTPROV) type ZTFI_0074 optional .
  methods GENERAR_LOG
    importing
      value(P_ENTRADA) type ZTFI_0074 optional
      value(XMSGTY) type SYMSGTY optional
      value(XMSGID) type SYMSGID optional
      value(XMSGNO) type SYMSGNO optional
      value(XMSGV1) type SYMSGV optional
      value(XMSGV2) type SYMSGV optional
      value(XMSGV3) type SYMSGV optional
      value(XMSGV4) type SYMSGV optional .
  methods GENERAR_MIRA
    importing
      value(LT_EKKO) type T_EKKO optional
      value(LT_EKPO) type T_EKPO optional
      value(LT_ZTFI_0075) type T_0075 optional
      value(LT_EKKN) type T_EKKN optional
      value(LT_AFKO) type T_AFKO optional
      value(LT_PROJ) type T_PROJ optional
      value(LT_ZDTE_CLDOC) type T_ZDTE_CLDOC optional
      value(LT_T001) type T_T001 optional
      value(TI_VBWF16) type T_VBWF16 optional
      value(S_TIPODTE) type ZFI_TIPO optional
      value(S_ESTATUS) type ZSTATDTE optional
    changing
      value(P_ENTRADA) type ZTFI_0074 optional .
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
  methods ENVIAR_RECHAZOACEP
    importing
      value(I_SALIDA) type ANY optional
      value(I_COMMIT) type C optional
      value(I_ENTRADA) type ZTFI_0074 optional
    exporting
      value(E_ERROR) type C .
  methods GENERAR_RECHAZO
    importing
      value(LT_PAVAL) type T_T001Z
      value(LT_LFA1) type T_LFA1
      value(P_GLOSA) type CHAR256
      value(P_NUMERO) type MSGNO
    changing
      value(P_ENTRADA) type ZTFI_0074 .
  methods DATOSDTE .
  methods BUSCADOSGENERALES .
  methods DATOSPEDLIM .
  methods DATOSAPROBADOR .
  methods LIBERAROBJ .
  methods INILOG .
  methods BLOQUEO .
  methods VALIDARPROVEEDOR .
  methods SUSTITUCION .
  methods BUSCADATOSOC .
  methods DETERM_APROBADOR .
  methods VALIDACION .
  methods LLAMABAPIS .
  methods GRABAR .
  methods DESBLOQUEO .
  methods GENERAR_MIR7_COMEX
    importing
      value(LT_ZDTE_CLDOC) type T_ZDTE_CLDOC optional
      value(LT_EKKO) type T_EKKO optional
      value(LT_EKPO) type T_EKPO optional
      value(LT_EKKN) type T_EKKN optional
      value(LT_AFKO) type T_AFKO optional
      value(LT_PROJ) type T_PROJ optional
      value(P_OC) type EBELN optional
    changing
      value(P_ENTRADA) type ZTFI_0074 optional .
  methods GET_FECHACIERRE
    importing
      !FECHA_CIERRE type SY-DATUM optional .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DTE_FI0057V2 IMPLEMENTATION.


METHOD bloqueo.
    "____> Bloqueo de registro.
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
      CLEAR ls_recfactprov.CLEAR ls_pos.
      "----> Releo el Registro bloqueado, para traer el ultimo valor
      SELECT SINGLE * FROM ztfi_0074 INTO ls_recfactprov
        WHERE bukrs EQ wa_recfactprov-bukrs AND
              xblnr EQ wa_recfactprov-xblnr AND
              tipodte  EQ wa_recfactprov-tipodte AND
              lifnr EQ wa_recfactprov-lifnr AND
              belnr EQ '' AND
              status IN me->s_status.
      IF sy-subrc = 0 . "Busco si el Doc sigue sin Precontabilizar
        "----> Entrego ultimo valor a linea
        wa_recfactprov =     ls_recfactprov.
        MODIFY lt_zcb_recfactprov FROM wa_recfactprov  INDEX lv_registro.
        SELECT * FROM ztfi_0075 INTO TABLE lt_zdte_posfact
        WHERE bukrs = wa_recfactprov-bukrs AND
              lifnr = wa_recfactprov-lifnr AND
              xblnr = wa_recfactprov-xblnr AND
              tipodte = wa_recfactprov-tipodte.
        IF sy-dbcnt >= 1.
          SORT lt_zdte_posfact BY ebeln.
          DELETE ADJACENT DUPLICATES FROM lt_zdte_posfact.
        ENDIF.
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
        APPEND wa_log TO ti_log.
      ENDIF.
    ENDIF.

  ENDMETHOD.


method BUSCADATOSOC.
*    DATA: lt_ekko TYPE STANDARD TABLE OF ekko WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
   IF wa_recfactprov-ebeln IS NOT INITIAL.
     SELECT *
     FROM ekko
     INTO TABLE lt_ekko
*OT        FOR ALL ENTRIES IN wa_recfactprov
     WHERE ebeln = wa_recfactprov-ebeln.
     IF sy-dbcnt >= 1.
       SORT lt_ekko BY ebeln.
       DELETE ADJACENT DUPLICATES FROM lt_ekko.

*      DATA: lt_lfb1_oc TYPE STANDARD TABLE OF lfb1 WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
       SELECT *
       FROM lfb1
       INTO TABLE lt_lfb1_oc
       FOR ALL ENTRIES IN lt_ekko
       WHERE lifnr = lt_ekko-lifnr AND
            bukrs = lt_ekko-bukrs.
       IF sy-dbcnt >= 1.
         SORT lt_lfb1 BY lifnr bukrs.
         DELETE ADJACENT DUPLICATES FROM lt_lfb1.
       ENDIF.


*      DATA: lt_ekpo TYPE STANDARD TABLE OF ekpo WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
       IF lines( lt_zdte_posfact_oc[] ) GT 0.
         SELECT *
         FROM ekpo
         INTO TABLE lt_ekpo
         FOR ALL ENTRIES IN  lt_zdte_posfact_oc"lt_ekko
         WHERE ebeln = lt_zdte_posfact_oc-ebeln
           AND ebelp = lt_zdte_posfact_oc-ebelp."lt_ekko-ebeln.
         IF sy-dbcnt >= 1.
           SORT lt_ekpo BY ebeln.
           DELETE ADJACENT DUPLICATES FROM lt_ekpo.
           DELETE lt_ekpo WHERE loekz NE space.


           IF lt_ekpo[] IS NOT INITIAL.
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
*        DATA: lt_ekkn TYPE STANDARD TABLE OF ekkn WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
           IF lt_ekpo[] IS NOT INITIAL.
             SELECT *
             FROM ekkn
             INTO TABLE lt_ekkn
             FOR ALL ENTRIES IN lt_ekpo
             WHERE ebeln = lt_ekpo-ebeln AND
                   ebelp = lt_ekpo-ebelp.
             IF sy-dbcnt >= 1.
               SORT lt_ekkn BY nplnr.
               DELETE ADJACENT DUPLICATES FROM lt_ekkn.

*          DATA: lt_afko TYPE STANDARD TABLE OF afko WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.

               SELECT *
               FROM afko
               INTO TABLE lt_afko
               FOR ALL ENTRIES IN lt_ekkn
               WHERE aufnr = lt_ekkn-nplnr.
               IF sy-dbcnt >= 1.
                 SORT lt_afko BY aufnr.
                 DELETE ADJACENT DUPLICATES FROM lt_afko.

*            DATA: lt_proj TYPE STANDARD TABLE OF proj WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
                 SELECT *
                 FROM proj
                 INTO TABLE lt_proj
                 FOR ALL ENTRIES IN lt_afko
                 WHERE pspnr = lt_afko-pronr.
                 IF sy-dbcnt >= 1.
                   SORT lt_proj BY pspnr.
                   DELETE ADJACENT DUPLICATES FROM lt_proj.
                 ENDIF.
               ENDIF.
             ENDIF.
           ENDIF.
         ENDIF.
       ENDIF.
     ENDIF.
   ENDIF.
  endmethod.


METHOD buscadosgenerales.
  SELECT *
   FROM ztfi_0074
   INTO TABLE lt_zcb_recfactprov
   WHERE bukrs IN s_bukrs AND
         xblnr IN s_folio AND
         bldat IN s_bldat AND
         belnr EQ '' AND
         status IN s_status.
  IF sy-dbcnt >= 1.
    SORT lt_zcb_recfactprov BY bukrs tipodte xblnr lifnr bldat.
    DELETE ADJACENT DUPLICATES FROM lt_zcb_recfactprov.

*      IF s_fecha[] IS NOT INITIAL.
*        DELETE lt_zcb_recfactprov WHERE bldat NOT IN s_fecha.
*      ENDIF.

    IF lines( lt_zcb_recfactprov[] ) GT 0.
      "------ Busco Homlogacion de clase de documentos
      SELECT * FROM ztfi_0001b
        INTO TABLE lt_zdte_cldoc
        FOR ALL ENTRIES IN lt_zcb_recfactprov
        WHERE bukrs = lt_zcb_recfactprov-bukrs.
      IF sy-dbcnt >= 1.
        SORT lt_zdte_cldoc BY bukrs.
        DELETE ADJACENT DUPLICATES FROM lt_zdte_cldoc.
      ENDIF.


      "----Seleccion de Rut de empresa por Sociedad
      SELECT  * FROM t001z
      INTO TABLE lt_t001z
      FOR ALL ENTRIES IN lt_zcb_recfactprov
      WHERE bukrs = lt_zcb_recfactprov-bukrs AND
          party = 'TAXNR'.
      IF sy-dbcnt >= 1.
        SORT lt_t001z BY bukrs.
        DELETE ADJACENT DUPLICATES FROM lt_t001z.
      ENDIF.

      "---->selección de proveedores.
      SELECT * FROM lfa1
      INTO TABLE lt_lfa1
      FOR ALL ENTRIES IN lt_zcb_recfactprov
      WHERE lifnr = lt_zcb_recfactprov-lifnr.
      IF sy-dbcnt >= 1.
        SORT lt_lfa1 BY lifnr.
        DELETE ADJACENT DUPLICATES FROM lt_lfa1.
      ENDIF.

      SELECT * FROM lfb1
      INTO TABLE lt_lfb1
      FOR ALL ENTRIES IN lt_zcb_recfactprov
      WHERE lifnr = lt_zcb_recfactprov-lifnr AND
            bukrs = lt_zcb_recfactprov-bukrs.
      IF sy-dbcnt >= 1.
        SORT lt_lfb1 BY lifnr bukrs.
        DELETE ADJACENT DUPLICATES FROM lt_lfb1.
      ENDIF.

      "----- Dias Permitidos por Sociedad
      SELECT * FROM ztfi_0076
      INTO TABLE lt_zdte_dias
      FOR ALL ENTRIES IN lt_zcb_recfactprov
      WHERE bukrs EQ lt_zcb_recfactprov-bukrs.
      IF sy-dbcnt >= 1.
        SORT lt_zdte_dias BY bukrs.
        DELETE ADJACENT DUPLICATES FROM lt_zdte_dias.
      ENDIF.

      "-----Tabla de variantes de validacion y datos de webservice.
      SELECT * FROM ztfi_0078
      INTO TABLE lt_zdte_valida
      WHERE bukrs IN s_bukrs.
      IF sy-dbcnt >= 1.
        SORT lt_zdte_valida BY bukrs.
        DELETE ADJACENT DUPLICATES FROM lt_zdte_valida.
      ENDIF.

      "-----tabla imputación U.
      SELECT * FROM ztfi_0085
      INTO TABLE ti_ztfi_0085
      WHERE bukrs IN s_bukrs.
      IF sy-dbcnt >= 1.
        SORT ti_ztfi_0085 BY bukrs.
        DELETE ADJACENT DUPLICATES FROM ti_ztfi_0085.
      ENDIF.

      "-----> Homologación de tipo de impuesto EnteGubernamental a código SAP
      SELECT * INTO TABLE lt_ztfi_0093
      FROM ztfi_0093
      WHERE vatint EQ gs_varint.
      IF sy-subrc EQ 0.
      ENDIF.

      "-----Datos de WF de FI
      SELECT bukrs wfvar  FROM t001
      INTO TABLE lt_t001
      WHERE bukrs IN s_bukrs.

      SELECT * FROM vbwf16
      INTO TABLE ti_vbwf16
      FOR ALL ENTRIES IN lt_t001
      WHERE wfvar = lt_t001-wfvar.
      IF sy-dbcnt >= 1.
        SORT ti_vbwf16 BY wfvar blart.
        DELETE ADJACENT DUPLICATES FROM ti_vbwf16.
      ENDIF.

      "-----> Homologación determinador de cuenta a Cuenta de mayor
      SELECT * INTO TABLE lt_ztfi_0092
      FROM ztfi_0092.
      IF sy-subrc EQ 0.
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


METHOD currency_conv.
**//..
    CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
      EXPORTING
        currency        = p_waers
        amount_internal = p_mntto
      IMPORTING
        amount_external = p_lv_amount.

  ENDMETHOD.


METHOD data_seleccionar.
    me->buscadosgenerales( ).
    me->datosdte( ).
    me->datospedlim( ).
    me->datosaprobador( ).
  ENDMETHOD.


METHOD datosaprobador.
    IF lines( lt_zcb_recfactprov[] ) GT 0.
**//..
      SELECT * INTO TABLE ti_ztfi_0095
      FROM ztfi_0095.
    ENDIF.
  ENDMETHOD.


METHOD datosdte.
    IF lines( lt_zcb_recfactprov[] ) GT 0.
      "----selección de referencias de factura.
      SELECT * FROM ztfi_0077
      INTO TABLE lt_zdt_recref
      FOR ALL ENTRIES IN lt_zcb_recfactprov
      WHERE bukrs = lt_zcb_recfactprov-bukrs AND
            xblnr = lt_zcb_recfactprov-xblnr AND
            lifnr = lt_zcb_recfactprov-lifnr AND
            tipodte = lt_zcb_recfactprov-tipodte.
      IF sy-dbcnt >= 1.
        SORT lt_zdt_recref BY bukrs xblnr lifnr.
        DELETE ADJACENT DUPLICATES FROM lt_zdt_recref.
      ENDIF.

      "-----> Otros Impuestos Facturas Procesadas

      SELECT * INTO TABLE lt_ztfi_0090
      FROM ztfi_0090
      FOR ALL ENTRIES IN lt_zcb_recfactprov
      WHERE bukrs   EQ lt_zcb_recfactprov-bukrs
        AND xblnr   EQ lt_zcb_recfactprov-xblnr
        AND lifnr   EQ lt_zcb_recfactprov-lifnr
        AND tipodte EQ lt_zcb_recfactprov-tipodte.

    ENDIF.

  ENDMETHOD.


METHOD datospedlim.
    "----->sección pedido limite y HES
    " Mejora del Performance

    IF lines( lt_zcb_recfactprov[] ) GT 0.
      CLEAR: rg_ekorg[],
         rg_ekgrp[],
         rg_bedat[].
      SELECT *
           FROM ekko
           INTO TABLE lt_ekko_lim
           FOR ALL ENTRIES IN lt_zcb_recfactprov
           WHERE lifnr EQ lt_zcb_recfactprov-lifnr
                 AND ekorg IN rg_ekorg
                 AND ekgrp IN rg_ekgrp
                 AND bedat IN rg_bedat
                 AND bukrs EQ lt_zcb_recfactprov-bukrs
                 AND kdatb <= lt_zcb_recfactprov-bldat
                 AND kdate >= lt_zcb_recfactprov-bldat.
      IF sy-dbcnt >= 1.
        SORT lt_ekko_lim BY ebeln.
        DELETE ADJACENT DUPLICATES FROM lt_ekko_lim.

        SELECT *
        FROM ekpo
        INTO TABLE lt_ekpo_lim
        FOR ALL ENTRIES IN lt_ekko_lim
        WHERE ebeln = lt_ekko_lim-ebeln.
        IF sy-dbcnt >= 1.
          SORT lt_ekpo_lim BY ebeln ebelp.
          DELETE ADJACENT DUPLICATES FROM lt_ekpo_lim.
          DELETE lt_ekpo_lim WHERE loekz NE space.
          DELETE lt_ekpo_lim WHERE pstyp NE '1'.


          IF lt_ekpo_lim[] IS NOT INITIAL. " Evitamos la perdida de performs
            SELECT *
            FROM ekkn
            INTO TABLE lt_ekkn_lim
            FOR ALL ENTRIES IN lt_ekpo_lim
            WHERE ebeln = lt_ekpo_lim-ebeln AND
                  ebelp = lt_ekpo_lim-ebelp.
            IF sy-dbcnt >= 1.
              SORT lt_ekkn_lim BY nplnr.
              DELETE ADJACENT DUPLICATES FROM lt_ekkn_lim.


              SELECT *
              FROM afko
              INTO TABLE lt_afko_lim
              FOR ALL ENTRIES IN lt_ekkn_lim
              WHERE aufnr = lt_ekkn_lim-nplnr.
              IF sy-dbcnt >= 1.
                SORT lt_afko_lim BY aufnr.
                DELETE ADJACENT DUPLICATES FROM lt_afko_lim.

                SELECT *
                FROM proj
                INTO TABLE lt_proj_lim
                FOR ALL ENTRIES IN lt_afko_lim
                WHERE pspnr = lt_afko_lim-pronr.
                IF sy-dbcnt >= 1.
                  SORT lt_proj_lim BY pspnr.
                  DELETE ADJACENT DUPLICATES FROM lt_proj_lim.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    "----->fin sección pedido limite
    "----->búsqueda de Hojas de Servicios.
    IF lt_zdt_recref[] IS NOT INITIAL.

      READ TABLE lt_zdt_recref TRANSPORTING NO FIELDS WITH KEY tiporef = 'HES'.
      IF sy-subrc EQ 0.

*        DATA: r_lblni  TYPE RANGE OF essr-lblni WITH HEADER LINE.
        DATA: r_lblni  TYPE ty_essr.
        DATA: s_lblni  TYPE l_essr.

        CLEAR r_lblni.CLEAR s_lblni.
        FREE r_lblni.

        s_lblni-sign = 'I'.
        s_lblni-option = 'EQ'.
        LOOP AT lt_zdt_recref INTO wa_zdt_recref WHERE tiporef = 'GES' OR
                                    tiporef = 'HES'.
          s_lblni-low = wa_zdt_recref-folioref.
          APPEND s_lblni TO r_lblni.
        ENDLOOP.

        SELECT lblni ebeln ebelp
        FROM essr
        INTO TABLE ti_essr
        WHERE lblni IN r_lblni.
        IF sy-dbcnt >= 1.
          SORT ti_essr BY lblni.
          DELETE ADJACENT DUPLICATES FROM ti_essr.

          SELECT *
          FROM ekko
          INTO TABLE ti_ekko_essr
          FOR ALL ENTRIES IN ti_essr
          WHERE ebeln = ti_essr-ebeln.
          IF sy-dbcnt >= 1.
            SORT ti_ekko_essr BY ebeln.
            DELETE ADJACENT DUPLICATES FROM ti_ekko_essr.
*          APPEND LINES OF ti_ekko_essr TO lt_ekko.


            SELECT  *
            FROM ekpo
            INTO TABLE ti_ekpo_essr
            FOR ALL ENTRIES IN ti_ekko_essr
            WHERE ebeln = ti_ekko_essr-ebeln.
            IF sy-dbcnt >= 1.
              SORT ti_ekpo_essr BY ebeln ebelp.
              DELETE ADJACENT DUPLICATES FROM ti_ekpo_essr.

*            APPEND LINES OF ti_ekpo_essr TO lt_ekpo.

              "---->mov de mercancias de la HES
              SELECT *
              FROM ekbe
              APPENDING TABLE lt_ekbe
              FOR ALL ENTRIES IN ti_ekpo_essr
              WHERE ebeln = ti_ekpo_essr-ebeln AND
                    ebelp = ti_ekpo_essr-ebelp.
              IF sy-dbcnt >= 1.
                SORT lt_ekbe BY ebeln ebelp.
                DELETE ADJACENT DUPLICATES FROM lt_ekbe.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    "---->fin.
  ENDMETHOD.


method DESBLOQUEO.
*Ini.Desbloqueo
   CALL FUNCTION 'DEQUEUE_EZTFI_0074'
     EXPORTING
       mode_ztfi_0074 = 'E'
       mandt          = sy-mandt
       bukrs          = wa_recfactprov-bukrs
       xblnr          = wa_recfactprov-xblnr
       lifnr          = wa_recfactprov-lifnr
       tipodte        = wa_recfactprov-tipodte
**         X_BUKRS        = ' '
**         X_XBLNR        = ' '
**         X_LIFNR        = ' '
**         X_TIPODTE      = ' '
**         _SCOPE         = '3'
**         _SYNCHRON      = ' '
**         _COLLECT       = ' '
     .
*Fin.Desbloqueo
  endmethod.


METHOD determ_aprobador.
    DATA: lv_asignado  TYPE abap_bool,
          lv_datum     TYPE sy-datum,
          lv_monto     TYPE ztfi_0074-mnttotal,
          lv_amount    TYPE wmto_s-amount,
          ls_ztfi_0095 TYPE ztfi_0095.

    lv_asignado = abap_false.
    lv_datum    = sy-datum.


**//.. Buscar AAprobador por monto
    lv_amount = wa_recfactprov-mnttotal.

    CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_DISPLAY'
      EXPORTING
        currency            = 'CLP'
        amount_internal     = lv_amount
      IMPORTING
        amount_display      = lv_amount
      EXCEPTIONS
        internal_error      = 1
        conversion_overflow = 2
        OTHERS              = 3.

    lv_monto = lv_amount.

    LOOP AT ti_ztfi_0095 INTO ls_ztfi_0095
                         WHERE mtoini LE lv_monto
                           AND mtofin GE lv_monto.
      wa_recfactprov-aprobador = ls_ztfi_0095-aprobador.
      MODIFY lt_zcb_recfactprov FROM wa_recfactprov INDEX lv_registro.
      EXIT.
    ENDLOOP.
  ENDMETHOD.


METHOD ejecuta_proceso.
    IF lines( lt_zcb_recfactprov[] ) GT 0.
      me->liberarobj( ).
      LOOP AT lt_zcb_recfactprov INTO wa_recfactprov.
        lv_registro = sy-tabix.
        me->inilog( ).
        res = 0.
        me->bloqueo(  ) .
        CHECK res EQ 0.
        me->validarproveedor( ).
        CHECK res EQ 0.
        me->sustitucion( ).
        me->buscadatosoc( ).
        me->determ_aprobador( ).
        me->validacion( ).
        me->llamabapis( ).
        me->grabar( ).
        me->desbloqueo( ).
      ENDLOOP.
      me->liberarobj( ).
      ti_log[] = me->ti_log[].
    ENDIF.
  ENDMETHOD.


METHOD enviar_rechazoacep.
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


METHOD generar_fbv1_bapi.
  DATA: ls_documentheader TYPE  bapiache09,
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
        lt_extension2     TYPE STANDARD TABLE OF bapiparex,
        ls_extension2     TYPE  bapiparex,
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

  "-->valor scope.
  CLEAR ls_ekpo.
  READ TABLE t_ekpo INTO ls_ekpo  WITH KEY ebeln = c_recfactprov-ebeln
                             knttp = 'N'.
  IF sy-subrc = 0.
    ls_cladte-knttp = ls_ekpo-knttp.
  ELSE.
    READ TABLE t_ekpo INTO ls_ekpo  WITH KEY ebeln = c_recfactprov-ebeln.
    IF sy-subrc = 0.
      ls_cladte-knttp = ls_ekpo-knttp.
    ENDIF.
  ENDIF.

  READ TABLE lt_zdte_cldoc INTO ls_cldoc WITH KEY bukrs   = ls_cladte-bukrs
                                  tipop   = u_tipodte
                                  tipodte = ls_cladte-tipodte
                                  knttp   = ls_cladte-knttp.

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
    WHEN '33' OR '34' OR '56' OR '110' OR '111'.
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
  ls_documentheader-header_txt = 'Doc.Fact.Elect'.    "texto de cabecera
  ls_documentheader-username = sy-uname.              "Nombre de usuario.
  ls_documentheader-bus_act = 'RFBV'.                 "Precontabilizar
  ls_documentheader-doc_status = '2'.                 "Estatus para precontabilizar

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
  c_recfactprov-zterm = ls_lfb1-zterm.
  CONCATENATE c_recfactprov-tipodte '-' c_recfactprov-xblnr INTO ls_accountpayable-alloc_nmbr.
  CONDENSE ls_accountpayable-alloc_nmbr NO-GAPS.

  CONCATENATE c_recfactprov-tipodte '-' c_recfactprov-xblnr '-' c_recfactprov-name1 INTO ls_accountpayable-item_text.
  CONDENSE ls_accountpayable-item_text NO-GAPS.




  IF c_recfactprov-iva GT 0.
    READ TABLE t_ztfi_0093 INTO ls_0093 WITH KEY cdsii = lv_cdsii.
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

*<<<<<<<<< IVA >>>>>>>>.

  IF c_recfactprov-iva GT 0.
    ADD  1 TO lv_itemacc.
    CLEAR ls_accounttax.
    ls_accounttax-itemno_acc = lv_itemacc. "'0000000002'.
    ls_accounttax-acct_key = 'VST'.
    ls_accounttax-tax_code =  ls_0093-mwskz_cpr. "'C1'.
*    ls_accounttax-direct_tax = 'X'. "Solo con Imputacion Directa Impuesto
    APPEND ls_accounttax TO lt_accounttax.

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
                                    cdgintrecep = c_recfactprov-cdgintrecep
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

      CONCATENATE c_recfactprov-tipodte '-' c_recfactprov-xblnr INTO ls_accountgl-alloc_nmbr.
      CONDENSE ls_accountgl-alloc_nmbr NO-GAPS.

      CONCATENATE c_recfactprov-tipodte '-' c_recfactprov-xblnr '-' c_recfactprov-name1 INTO ls_accountgl-item_text.
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

*<<<<<<<<< Extension de BAPI >>>>>>>>.
*  ls_extension2-structure = 'ZDTE_EXTENSION2'.
*  ls_extension2-valuepart1 = 'FV60'.
*  APPEND ls_extension2 TO lt_extension2.

  "---> LLamar Bapi
  CLEAR lv_obj_key.
  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      documentheader = ls_documentheader
*     CUSTOMERCPD    =
*     CONTRACTHEADER =
    IMPORTING
*     OBJ_TYPE       =
      obj_key        = lv_obj_key
*     OBJ_SYS        =
    TABLES
      accountgl      = lt_accountgl
*     ACCOUNTRECEIVABLE       =
      accountpayable = lt_accountpayable
      accounttax     = lt_accounttax
      currencyamount = lt_currencyamount
*     CRITERIA       =
*     VALUEFIELD     =
*     EXTENSION1     =
      return         = lt_return
*     PAYMENTCARD    =
*     CONTRACTITEM   =
*     extension2     = lt_extension2
*     REALESTATE     =
*     ACCOUNTWT      =
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
      me->generar_log( EXPORTING p_entrada = c_recfactprov
                                    xmsgty = sy-msgty  xmsgid =  sy-msgid xmsgno = sy-msgno xmsgv1 = sy-msgv1
                                    xmsgv2 = sy-msgv2 xmsgv3 =  sy-msgv3  xmsgv4 = sy-msgv4 ).
    ENDIF.

  ENDIF.


ENDMETHOD.


METHOD generar_log.
    DATA: lv_s_log  TYPE bal_s_log,
          lv_handle TYPE balloghndl,
          lv_stcd1  TYPE lfa1-stcd1.
    DATA: lv_s_msg TYPE bal_s_msg.

    lv_s_msg-msgty = xmsgty.
    lv_s_msg-msgid = xmsgid.
    lv_s_msg-msgno = xmsgno.
    lv_s_msg-msgv1 = xmsgv1.
    lv_s_msg-msgv2 = xmsgv2.
    lv_s_msg-msgv3 = xmsgv3.
    lv_s_msg-msgv4 = xmsgv4.


    "Inicio Edwar Soto  12.08.2016 / Siempre tendremos algo que mostrar
    IF xmsgty IS INITIAL.
      xmsgty = 'S'.
    ENDIF.
    IF xmsgid IS INITIAL AND xmsgno IS INITIAL.
      xmsgid = 'ZDTE_0001'.
      xmsgno = 12.
    ENDIF.
    "Fin Edwar Soto  12.08.2016
    CLEAR lv_s_log.

    CONCATENATE p_entrada-bukrs p_entrada-lifnr p_entrada-xblnr p_entrada-tipodte INTO lv_s_log-extnumber.
    lv_s_log-object = 'ZDTE'.
    lv_s_log-subobject = 'RECEPCION'.
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
    ln_external-low = lv_s_log-extnumber.
    ln_object-low = 'ZDTE'.
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
          i_t_logs_to_delete = lt_log_header
        EXCEPTIONS
          no_logs_specified  = 1
          OTHERS             = 2.
    ENDIF.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = lv_s_log
      IMPORTING
        e_log_handle            = lv_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.

    IF sy-subrc = 0 AND lv_handle IS NOT INITIAL.

      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle     = lv_handle
          i_s_msg          = lv_s_msg
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          OTHERS           = 4.

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
        ls_proj     TYPE  proj,
        ls_cldoc    TYPE ztfi_0001b,
        ls_t001     TYPE t001,
        ls_vbwf16   TYPE vbwf16,
        ls_0085     TYPE ztfi_0085,
        ls_0083     TYPE ztfi_0083,
        ls_return   TYPE bapiret2,
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
         ls_itemdata.




  CLEAR: ls_itemdata,
         ls_return.

  FREE: lt_itemdata,
        lt_return.

  CLEAR ls_headerdata.

  DATA: lv_debe_haber TYPE shkzg.
  IF p_entrada-tipodte = '33' OR p_entrada-tipodte = '34'
  OR p_entrada-tipodte = '110'.
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
  READ TABLE lt_ekko INTO ls_ekko WITH KEY ebeln = p_oc.
  IF sy-subrc EQ 0.
    e_clase-bsart = ls_ekko-bsart.
    p_entrada-zterm = ls_ekko-zterm.
  ENDIF.

  "---->valor scope.
  READ TABLE lt_ekpo INTO ls_ekpo WITH KEY ebeln = p_oc
                              knttp = 'N'.
  IF sy-subrc = 0.
    e_clase-knttp = ls_ekpo-knttp.
*    READ TABLE lt_ekkn  into ls_ekkn WITH KEY ebeln = ls_ekpo-ebeln
*                                ebelp = ls_ekpo-ebelp BINARY SEARCH.
*    IF sy-subrc = 0.
*      READ TABLE lt_afko into ls_afko WITH KEY aufnr = ls_ekkn-nplnr BINARY SEARCH.
*      IF sy-subrc = 0.
*        READ TABLE lt_proj into ls_proj WITH KEY pspnr = ls_afko-pronr BINARY SEARCH.
*        IF sy-subrc = 0.
*          e_clase-scope = ls_proj-scope.
*        ENDIF.
*      ENDIF.
*    ENDIF.
  ELSE.
    READ TABLE lt_ekpo INTO ls_ekpo WITH KEY ebeln = p_oc.
    IF sy-subrc = 0.
      e_clase-knttp = ls_ekpo-knttp.
    ENDIF.
  ENDIF.

  READ TABLE lt_zdte_cldoc INTO ls_cldoc WITH KEY bukrs = e_clase-bukrs
                                    tipodte = e_clase-tipodte
                                   knttp = e_clase-knttp
                                   bsart = e_clase-bsart.
  IF sy-subrc = 0.
    ls_headerdata-doc_type = ls_cldoc-blart.
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
  ls_headerdata-bline_date = p_entrada-bldat.
  DATA: lv_invoice_doc_item TYPE rblgp.
  lv_invoice_doc_item  = '00001'.


*OT Texto y Asignación
  CONCATENATE p_entrada-tipodte '-' p_entrada-xblnr INTO  ls_headerdata-alloc_nmbr.
  CONDENSE ls_headerdata-alloc_nmbr NO-GAPS.
  SELECT  txz01 INTO ls_headerdata-item_text FROM ekpo UP TO 1 ROWS
    WHERE ebeln EQ p_entrada-ebeln
     AND loekz EQ space
  ORDER BY ebeln ebelp  DESCENDING.
  ENDSELECT.
  CONDENSE ls_headerdata-item_text NO-GAPS.

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


  DATA: lt_glimpu TYPE STANDARD TABLE OF  bapi_incinv_create_gl_account,
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
*     ADDRESSDATA      =
    IMPORTING
      invoicedocnumber = lv_documento
      fiscalyear       = lv_gjahr
    TABLES
      itemdata         = lt_itemdata
      accountingdata   = lt_impu
      glaccountdata    = lt_glimpu
      tax_data         = lt_tax_data
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
    READ TABLE lt_ekpo INTO ls_ekpo WITH KEY ebeln = p_oc
                                knttp = 'N'.
    IF sy-subrc = 0.
      e_clase-knttp = ls_ekpo-knttp.
*    READ TABLE lt_ekkn  into ls_ekkn WITH KEY ebeln = ls_ekpo-ebeln
*                                ebelp = ls_ekpo-ebelp BINARY SEARCH.
*    IF sy-subrc = 0.
*      READ TABLE lt_afko into ls_afko WITH KEY aufnr = ls_ekkn-nplnr BINARY SEARCH.
*      IF sy-subrc = 0.
*        READ TABLE lt_proj into ls_proj WITH KEY pspnr = ls_afko-pronr BINARY SEARCH.
*        IF sy-subrc = 0.
*          e_clase-scope = ls_proj-scope.
*        ENDIF.
*      ENDIF.
*    ENDIF.
    ELSE.
      READ TABLE lt_ekpo INTO ls_ekpo WITH KEY ebeln = p_oc.
      IF sy-subrc = 0.
        e_clase-knttp = ls_ekpo-knttp.
      ENDIF.
    ENDIF.

    READ TABLE lt_zdte_cldoc INTO ls_cldoc WITH KEY bukrs = e_clase-bukrs
                                      tipodte = e_clase-tipodte
                                     knttp = e_clase-knttp
                                     bsart = e_clase-bsart.
    IF sy-subrc = 0.
      ls_headerdata-doc_type = ls_cldoc-blart.
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
    ls_headerdata-bline_date = p_entrada-bldat.
    DATA: lv_invoice_doc_item TYPE rblgp.
    lv_invoice_doc_item  = '00001'.


*OT Texto y Asignación
    CONCATENATE p_entrada-tipodte '-' p_entrada-xblnr INTO  ls_headerdata-alloc_nmbr.
    CONDENSE ls_headerdata-alloc_nmbr NO-GAPS.

    CONCATENATE p_entrada-tipodte '-' p_entrada-xblnr INTO ls_headerdata-header_txt.
    CONDENSE ls_headerdata-header_txt NO-GAPS.

    SELECT  txz01 INTO ls_headerdata-item_text FROM ekpo UP TO 1 ROWS
      WHERE ebeln EQ p_entrada-ebeln
       AND loekz EQ space
    ORDER BY ebeln ebelp  DESCENDING.
    ENDSELECT.
    CONDENSE ls_headerdata-item_text NO-GAPS.

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
  DATA: ls_headerdata           TYPE  bapi_incinv_create_header,
        ls_additionalheaderdata TYPE  bapi_incinv_save_header_backgr,
        ls_refdoccategory       TYPE  bapi_incinv_fld-ref_doc_category.

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
        ls_0083   TYPE  ztfi_0083.
  CLEAR: ls_ekko,
    ls_ekpo,
    ls_0075,
    ls_ekkn,
    ls_afko,
    ls_proj,
    ls_cldoc,
    ls_t001,
    ls_vbwf16,
    ls_0083.

  CLEAR: lt_return,
         ls_headerdata,
          ls_additionalheaderdata,
          ls_refdoccategory,
          lt_tax_data.

  FREE lt_tax_data.


  IF p_entrada-tipodte = '33' OR p_entrada-tipodte = '34'
  OR p_entrada-tipodte = '110'.
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
  READ TABLE lt_ekko INTO ls_ekko WITH KEY ebeln = p_entrada-ebeln.
  IF sy-subrc EQ 0.
    e_clase-bsart = ls_ekko-bsart.
    p_entrada-zterm = ls_ekko-zterm.
  ENDIF.

  "---->valor scope.
  READ TABLE lt_ekpo INTO ls_ekpo WITH KEY ebeln = p_entrada-ebeln
                              knttp = 'N'.
  IF sy-subrc = 0.
    e_clase-knttp = ls_ekpo-knttp.
*    READ TABLE lt_ekkn WITH KEY ebeln = lt_ekpo-ebeln
*                                ebelp = lt_ekpo-ebelp BINARY SEARCH.
*    IF sy-subrc = 0.
*      READ TABLE lt_afko WITH KEY aufnr = lt_ekkn-nplnr BINARY SEARCH.
*      IF sy-subrc = 0.
*        READ TABLE lt_proj WITH KEY pspnr = lt_afko-pronr BINARY SEARCH.
*        IF sy-subrc = 0.
*          e_clase-scope = lt_proj-scope.
*        ENDIF.
*      ENDIF.
*    ENDIF.
  ELSE.
    READ TABLE lt_ekpo INTO ls_ekpo WITH KEY ebeln = p_entrada-ebeln.
    IF sy-subrc = 0.
      e_clase-knttp = ls_ekpo-knttp.
    ENDIF.
  ENDIF.

  READ TABLE lt_zdte_cldoc INTO ls_cldoc WITH KEY bukrs = e_clase-bukrs
                                    tipodte = e_clase-tipodte
                                    knttp = e_clase-knttp
                                    bsart = e_clase-bsart.
  IF sy-subrc = 0.
    ls_headerdata-doc_type = ls_cldoc-blart.
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
  ls_headerdata-pstng_date  = sy-datum.
  ls_headerdata-bline_date = p_entrada-bldat.
*OT Texto y Asignación
  CLEAR ls_headerdata-item_text.
  CONCATENATE p_entrada-tipodte '-' p_entrada-xblnr INTO  ls_headerdata-alloc_nmbr.
  CONDENSE ls_headerdata-alloc_nmbr NO-GAPS.
  SELECT  txz01 INTO ls_headerdata-item_text FROM ekpo UP TO 1 ROWS
    WHERE ebeln EQ p_entrada-ebeln
     AND loekz EQ space
  ORDER BY ebeln ebelp  DESCENDING.
  ENDSELECT.

  CONDENSE ls_headerdata-item_text NO-GAPS.



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

  DATA: lv_invoicedocnumber TYPE  re_belnr,
        lv_fiscalyear       TYPE  gjahr.
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
*     ADDRESSDATA          =
    IMPORTING
      invoicedocnumber     = lv_invoicedocnumber
      fiscalyear           = lv_fiscalyear
    TABLES
      selectpo             = lt_selectpo
*     SELECTDELIVERY       = lt_selectdelivery
*     SELECTBILLLADING     =
*     SELECTSERVICE        = lt_selectsv
*     SELECTPLANT          =
      taxdata              = lt_tax_data
*     WITHTAXDATA          =
*     VENDORITEMSPLITDATA  =
      return               = lt_return
*     EXTENSIONIN          =
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
*  ENDIF.

  "p_entrada-status = '3'.
ENDMETHOD.


METHOD generar_rechazo.
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

    "---Envío de e-mail rechazo
    CALL FUNCTION 'ZFI_0028DTE'
      EXPORTING
        i_lifnr   = p_entrada-lifnr
        i_bukrs   = p_entrada-bukrs
        i_xblnr   = p_entrada-xblnr
        i_tipodte = p_entrada-tipodte
        i_glosa   = p_entrada-glosa. "e_salida-estadodteglosa
  ENDIF.
ENDMETHOD.


  method GET_FECHACIERRE.

     me->lg_fechacierre = fecha_cierre.

  endmethod.


METHOD grabar.
    FREE old_ztfi_0074.
    CLEAR old_ztfi_0074.
    FREE new_ztfi_0074.
    CLEAR new_ztfi_0074.
    wa_objectid = wa_recfactprov+0(36).
    CLEAR: old_ztfi_0074[], new_ztfi_0074[].
    APPEND wa_recfactprov TO new_ztfi_0074.
    SELECT * FROM ztfi_0074 INTO TABLE old_ztfi_0074
             WHERE bukrs EQ wa_recfactprov-bukrs
               AND xblnr EQ wa_recfactprov-xblnr
               AND lifnr EQ wa_recfactprov-lifnr
               AND tipodte EQ wa_recfactprov-tipodte.
    IF sy-subrc EQ 0.
      READ TABLE old_ztfi_0074  ASSIGNING FIELD-SYMBOL(<lt2>)  INDEX 1.
      IF wa_recfactprov-status NE <lt2>-status OR
         wa_recfactprov-belnr  NE <lt2>-belnr OR
         wa_recfactprov-gjahr  NE <lt2>-gjahr OR
         wa_recfactprov-aprobador_real NE <lt2>-aprobador_real.
        CALL FUNCTION 'ZTFI_0074_WRITE_DOCUMENT'
          IN UPDATE TASK
          EXPORTING
            objectid      = wa_objectid
            tcode         = sy-tcode "'ZDTE_001'
            utime         = sy-uzeit
            udate         = sy-datum
            username      = sy-uname
            upd_ztfi_0074 = 'U'
          TABLES
            xztfi_0074    = new_ztfi_0074
            yztfi_0074    = old_ztfi_0074.
      ENDIF.
    ENDIF.
*fin. Log de Modificaciones Status (pre-contab).
*Ini. Grabar tablas.
    UPDATE ztfi_0074 FROM wa_recfactprov.
    COMMIT WORK AND WAIT.
    UPDATE ztfi_0075 FROM TABLE lt_zdte_posfact.
    COMMIT WORK AND WAIT.
*FIn. Grabar Tablas
  ENDMETHOD.


METHOD indiva_defecto.
    DATA: ls_0093 TYPE ztfi_0093.
    CLEAR ls_0093.
    p_error = 0.
    READ TABLE t_ztfi_0093 INTO ls_0093 WITH KEY cdsii = lv_cdsii.
    IF sy-subrc NE 0.
      CLEAR wa_log.
      wa_log-estado    = icon_red_light.
      wa_log-documento = p_entrada-xblnr.
      wa_log-sociedad  = p_entrada-bukrs.
      wa_log-proveedor = p_entrada-lifnr.
      wa_log-fecha     = p_entrada-bldat.
*----> 'Error'
      MESSAGE ID 'ZDTE_0001' TYPE 'E' NUMBER '034' WITH lv_cdsii
                                                  INTO wa_log-mensaje.
      CONCATENATE TEXT-410 wa_log-mensaje INTO wa_log-mensaje
                                          SEPARATED BY space.
      APPEND wa_log TO ti_log.

      p_entrada-status = '7'.

*      PERFORM generar_log USING  p_entrada        'E'
*                                 'ZDTE_0001'           '034'
*                                 lv_cdsii             space
*                                 space                space.
      msgv1 = lv_cdsii.
      me->generar_log( EXPORTING p_entrada = p_entrada
                      xmsgty = 'E'  xmsgid = 'ZDTE_0001' xmsgno = '034' xmsgv1 = msgv1
                      xmsgv2 = space xmsgv3 = space xmsgv4 = space ).
      p_error = 1.

    ELSE.
      p_lt_tax_data_tax_code = ls_0093-mwskz_cpr. "'C1'
    ENDIF.
  ENDMETHOD.


METHOD inilog.
    CLEAR wa_log. CLEAR s_tipodte.
    wa_log-documento = wa_recfactprov-xblnr.
    wa_log-sociedad = wa_recfactprov-bukrs.
    wa_log-proveedor = wa_recfactprov-lifnr.
    wa_log-fecha = wa_recfactprov-bldat.
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


    ELSEIF  s_tipodte EQ '4'. "Proveedor OC  costos ind x MM-MIRA

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


METHOD sustitucion.
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
  data: ls_lfb1  type lfb1.

  clear ls_lfb1.

  READ TABLE lt_lfb1 into ls_lfb1   WITH KEY lifnr = wa_recfactprov-lifnr
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
        SELECT * INTO wa_factmm FROM zcds_factmm UP TO 1 ROWS
        WHERE bukrs = <lt2>-bukrs AND
                              xblnr = <lt2>-folioref AND
                              lifnr = <lt2>-lifnr AND
                              tipodte = <lt2>-tiporef
                              ORDER BY bukrs xblnr.
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
*---->  Asigno tipo DTE a valiables por el tipo mixto que es 1 y 3
  s_tipodte = ls_lfb1-zzdte_tipo.

ENDMETHOD.


METHOD validacion.
  DATA: y_msgno TYPE msgno.
  CLEAR msgid.
  CLEAR msgty.
  CLEAR msgno. CLEAR  y_msgno.
  CLEAR msgv1.
  CLEAR msgv2.
  CLEAR msgv3.
  CLEAR msgv4.
  CLEAR estatus.
  CLEAR e_repid.
  READ TABLE lt_zdte_valida  ASSIGNING FIELD-SYMBOL(<lt2>) WITH  KEY bukrs = wa_recfactprov-bukrs.
  IF sy-subrc EQ 0.

    e_repid  =  <lt2>-varvalpre.
    IF e_repid IS NOT INITIAL.

      CALL FUNCTION 'ZFI_0030'
        EXPORTING
          lt_zcb_recfactprov = wa_recfactprov
          zdte_tipo          = s_tipodte
          repid              = e_repid
        TABLES
          lt_lfb1            = lt_lfb1
          lt_lfb1_oc         = lt_lfb1_oc
          lt_lfa1            = lt_lfa1
          lt_ekko            = lt_ekko
          lt_ekpo            = lt_ekpo
          lt_zdte_cldoc      = lt_zdte_cldoc
          lt_zdte_dias       = lt_zdte_dias
          lt_zdte_posfact    = lt_zdte_posfact
          lt_zdt_recref      = lt_zdt_recref
          lt_ekbe            = lt_ekbe
          lt_ekkn            = lt_ekkn
          ti_essr            = ti_essr
          ti_ekko_essr       = ti_ekko_essr
          ti_ekpo_essr       = ti_ekpo_essr
        CHANGING
          msgid              = msgid
          msgty              = msgty
          msgno              = y_msgno
          msgv1              = msgv1
          msgv2              = msgv2
          msgv3              = msgv3
          msgv4              = msgv4
          estatus            = estatus
        EXCEPTIONS
          envia_error        = 1
          OTHERS             = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
      msgno = y_msgno.
    ELSE.
      estatus = '7'.
      msgid = 'ZDTE_0001'.
      msgty = 'E'.
      msgno = '012'.
      msgv1 = TEXT-406. msgv2 = wa_recfactprov-bukrs . msgv3 = TEXT-407. msgv4 = ''.
    ENDIF.
  ELSE.
    estatus = '7'.
    msgid = 'ZDTE_0001'.
    msgty = 'E'.
    msgno = '012'.
    msgv1 = TEXT-406. msgv2 = wa_recfactprov-bukrs . msgv3 = TEXT-407. msgv4 = ''.
  ENDIF.

ENDMETHOD.


METHOD validarproveedor.
    CLEAR wa_log. CLEAR s_tipodte.
    wa_log-documento = wa_recfactprov-xblnr.
    wa_log-sociedad = wa_recfactprov-bukrs.
    wa_log-proveedor = wa_recfactprov-lifnr.
    wa_log-fecha = wa_recfactprov-bldat.
*OT Primero preguntar por Proveedor

    READ TABLE lt_lfa1 ASSIGNING FIELD-SYMBOL(<lt2>)  WITH KEY lifnr = wa_recfactprov-lifnr.
    IF sy-subrc EQ 0.
*OT Busco el tipo_proveedor X sociedad
      READ TABLE lt_lfb1 TRANSPORTING NO FIELDS WITH KEY lifnr = <lt2>-lifnr
                                  bukrs = wa_recfactprov-bukrs.
      IF sy-subrc NE 0.
        cnum = '000'.
        wa_log-estado = icon_red_light.
        MESSAGE e000(zdte_0001) WITH wa_recfactprov-bukrs  INTO wa_log-mensaje.
        me->generar_rechazo( EXPORTING lt_paval = lt_t001z  lt_lfa1 = lt_lfa1  p_glosa = wa_log-mensaje p_numero = cnum
                              CHANGING p_entrada = wa_recfactprov ). "TABLES lt_t001z  lt_lfa1 USING  wa_log-mensaje '000' CHANGING wa_recfactprov.

        APPEND  wa_log TO ti_log.
        MODIFY lt_zcb_recfactprov FROM wa_recfactprov  INDEX lv_registro.
        " CONTINUE.
        res = sy-subrc.

      ENDIF.
    ELSE.
      cnum = '000'.
      wa_log-estado = icon_red_light.
      MESSAGE e000(zdte_0001) WITH wa_recfactprov-bukrs  INTO wa_log-mensaje.
      me->generar_rechazo( EXPORTING lt_paval = lt_t001z  lt_lfa1 = lt_lfa1  p_glosa = wa_log-mensaje p_numero = cnum
                             CHANGING p_entrada = wa_recfactprov ). "TABLES lt_t001z  lt_lfa1 USING  wa_log-mensaje '000' CHANGING wa_recfactprov.

      APPEND wa_log TO ti_log.
      MODIFY lt_zcb_recfactprov FROM wa_recfactprov  INDEX lv_registro.
      "CONTINUE.
      res = sy-subrc.
    ENDIF.
    IF res NE 0.
      me->grabar( ).
      me->desbloqueo( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
