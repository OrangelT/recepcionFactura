class ZCL_DTE_MONITOR definition
  public
  create public .

public section.

  types:
    r_cpudt TYPE RANGE OF sy-datum .
  types:
    r_folio TYPE RANGE OF xblnr .
  types:
    r_tipd TYPE RANGE OF ztipodte .
  types:
    r_dtetipo TYPE RANGE OF zfi_tipo .
  types:
    r_lifnr_pr TYPE RANGE OF lifnr .
  types:
    r_nombana TYPE RANGE OF znombana .
  types:
    BEGIN OF ty_gt_sesion.
        INCLUDE TYPE ztfi_0074.
        TYPES:  usuario TYPE syuname.
    TYPES: END OF ty_gt_sesion .
  types:
    BEGIN OF t_fechasii,
        f    TYPE ztfi_0074fr,
        flag TYPE flag,
      END OF t_fechasii .
  types:
    r_lifnr TYPE RANGE OF lifnr .
  types:
    r_bldat TYPE RANGE OF sy-datum .
  types:
    r_status TYPE RANGE OF ztfi_0074-status .
  types:
    r_stcd1 TYPE RANGE OF ztfi_0074-stcd1 .
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

  data PATH type STRING .
  data:
    cpudt TYPE RANGE OF sydatum .
  data GS_VATINT type ZDTE_VARIANT .
  data DTETIPO type R_DTETIPO .
  data:
    conf_status TYPE STANDARD TABLE OF ztfi_0079 WITH NON-UNIQUE DEFAULT KEY .
  data:
    ti_mod_save TYPE STANDARD TABLE OF ty_mod .
  data:
    ti_mod_acep TYPE STANDARD TABLE OF ty_mod .
  data NOMBANA type R_NOMBANA .
  data BUKRS type BUKRS .
  data LIFNR type R_LIFNR_PR .
  data STATUS type R_STATUS .
  data STCD1 type R_STCD1 .
  data GR_ALVGRID type ref to CL_GUI_ALV_GRID .
  data GV_STRUCTURE type TABNAME value 'ZEFI_0026' ##NO_TEXT.
  data GT_TOOLBAR type TTB_BUTTON .
  data GV_MODIF type ABAP_BOOL .
  data GV_GLOSA type CHAR100 .
  data:
    conf_boton TYPE STANDARD TABLE OF ztfi_0080 WITH NON-UNIQUE DEFAULT KEY .
  data:
    conf_trans TYPE STANDARD TABLE OF ztfi_0081 WITH NON-UNIQUE DEFAULT KEY .
  data:
    bldat TYPE RANGE OF sydatum .
  data:
    datos    TYPE STANDARD TABLE OF zefi_0026 WITH NON-UNIQUE DEFAULT KEY .
  data:
    datos_or TYPE STANDARD TABLE OF zefi_0026 WITH NON-UNIQUE DEFAULT KEY .
  data:
    fechasii TYPE STANDARD TABLE OF t_fechasii .                   "ztfi_0074fr.
  data:
    gt_sesion TYPE STANDARD TABLE OF ty_gt_sesion WITH NON-UNIQUE DEFAULT KEY .
  data:
    ls_sesion LIKE LINE OF gt_sesion .
  data:
    wa_sesion LIKE LINE OF gt_sesion .
  data:
    lt_sesion  TYPE STANDARD TABLE OF ty_gt_sesion WITH NON-UNIQUE DEFAULT KEY .
  data:
    lt_sesion2 TYPE STANDARD TABLE OF ty_gt_sesion WITH NON-UNIQUE DEFAULT KEY .
  data:
    ln_sesion    LIKE LINE OF lt_sesion .
  data:
    lt_ztfi_0074 TYPE TABLE OF ztfi_0074 .
  data FOLIO type R_FOLIO .
  data TIPD type R_TIPD .
  data DIAS type ZDTE_A_EVALUATION_PERIOD .
  data ARCHV type FLAG .

  methods ENVIAR_RECHAZOACEP
    importing
      value(I_SALIDA) type ANY optional
      value(I_COMMIT) type C optional
      value(I_ENTRADA) type ZEFI_0026 optional
    exporting
      value(E_ERROR) type C .
  methods GENERAR_VARIANTE
    exporting
      value(I_VARIANT) type DISVARIANT .
  methods LIBERAROBJ .
  methods REPROC_IDOC
    exporting
      !LT_SALIDA_TEMP type T_ZEFI_0026
      !LT_SELECT_ROWS type LVC_T_ROID .
  methods CONSTRUCTOR
    importing
      !BUKRS type BUKRS optional
      !LIFNR type R_LIFNR optional
      !BLDAT type R_BLDAT optional
      !STATUS type R_STATUS optional
      !STCD1 type R_STCD1 optional
      !NOMBANA type R_NOMBANA optional
      !DTETIPO type R_DTETIPO optional
      !FOLIO type R_FOLIO optional
      !TIPD type R_TIPD optional
      !DIAS type ZDTE_A_EVALUATION_PERIOD optional
      !ARCHIVADO type FLAG optional
      !CPUDT type R_CPUDT optional .
  methods GENERAR_CATALOGO
    exporting
      !TI_CATALOGO type SLIS_T_FIELDCAT_ALV .
  methods OBTENER_DATOS
    exporting
      !TI_SALIDA type ZDTE_T_MONITOR_SALIDA .
  methods GENERAR_BOTONES
    exporting
      !TI_BOTONES type SLIS_T_EXTAB .
  methods GENERAR_CAMBIOS
    importing
      !BELNR type RBKP-BELNR
      !GJAHR type RBKP-GJAHR
      !TCODE type SYTCODE
      !BUKRS type BUKRS
      !VISUALIZAR type CHAR1 .
  methods MOSTRAR_LOG
    importing
      !BELNR type BKPF-BELNR
      !EBELN type EKKO-EBELN
      !GJAHR type BKPF-GJAHR
      !LIFNR type EKKO-LIFNR
      !XBLNR type XBLNR
      !BUKRS type BUKRS
      !BLDAT type SYDATUM
      !TIPODTE type ZTIPODTE .
  methods CIERRE_DOC
    importing
      !ENTRADA type ZEFI_0026
      !ACEPTA_RECHAZA type CHAR1
    exporting
      !ERROR type CHAR1
      !GLOSA type CHAR100
      !CORRELATIVO type CHAR10 .
  methods ACEPTA_RECHAZA_DOC
    importing
      !ENTRADA type ZEFI_0026
      !ACEPTA_RECHAZA type CHAR1
    exporting
      !ERROR type CHAR1
      !GLOSA type CHAR100
      !CORRELATIVO type CHAR10 .
  methods ACEPTA_RECHAZA_MER .
  class-methods VER_PDF
    importing
      !XBLNR type ZEFI_0026-XBLNR
      !BUKRS type ZEFI_0026-BUKRS
      !TIPODTE type ZEFI_0026-TIPODTE
      !STCD1 type ZEFI_0026-STCD1
    exporting
      !PDF type XSTRING
    raising
      CX_STATIC_CHECK .
  methods LANZAR_WF
    importing
      value(SOCIEDAD) type ZTFI_0074-BUKRS
      value(FACTURA) type ZTFI_0074-XBLNR
      value(PROVEEDOR) type ZTFI_0074-LIFNR
      value(FECHA) type ZTFI_0074-BLDAT
      value(RESPONSABLE) type XUBNAME
      value(TIPODTE) type ZTFI_0074-TIPODTE
      value(REGISTRO) type ZTFI_0074
    exporting
      value(MOD) type XFLAG
    changing
      value(STATUS) type ZSTATDTE optional .
  methods VER_DETALLE
    importing
      !SOCIEDAD type BUKRS
      !DOCUMENTO type XBLNR
      !PROVEEDOR type LIFNR
      !TIPODTE type ZTIPODTE .
  methods OBTENER_STATUS
    exporting
      !TI_STATUS type ZTFI_0079 .
  methods DOCUMENTO_MODIFICACION
    importing
      !SOCIEDAD type BUKRS
      !DOCUMENTO type XBLNR
      !PROVEEDOR type LIFNR
      !TIPODTE type ZTFI_0074-TIPODTE
      !OBJEKT type TCDOB-OBJECT .
  class-methods VER_PDF_ARCH
    importing
      !BUKRS type BUKRS
      !RUT_EMISOR type STCD1
      !RUT_RECEPTOR type ZRUTRECEP
      !TIPO_DTE type ZTIPODTE
      !XBLNR type XBLNR
      !FONDO type XFLAG optional
    exporting
      !PDF type XSTRING .
  methods VER_EVENTOS
    importing
      !SOCIEDAD type BUKRS
      !ITAB type ZDTE_FECHASII .
  methods DISPLAY
    exporting
      !R_RETURN type BAPIRET2 .
  methods SAVE .
  methods CREATE_ATTACHMENT_BOR
    importing
      !I_ZTFI_0074 type ZTFI_0074
      !I_OBJTYP type SWO_OBJTYP .
  methods GENERAR_CATALOGOOO
    exporting
      !TI_CATALOGO type LVC_T_FCAT .
  methods GENERAR_EXCLUIDOS
    exporting
      !TI_EXCLUIDOS type UI_FUNCTIONS .
  methods BUSCAR_DATOS .
  methods BUSCAR_CONF .
  methods GET_OTHER_CHDOCS
    importing
      !BUKRS type BUKRS
      !LIFNR type LIFNR
      !XBLNR type XBLNR
    returning
      value(R_VALUE) type CDREDCD_TAB .
  methods HANDLE_USER_COMMAND
    for event USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM .
  methods HANDLE_DOUBLE_CLICK
    for event DOUBLE_CLICK of CL_GUI_ALV_GRID
    importing
      !E_ROW
      !E_COLUMN
      !ES_ROW_NO .
  methods HANDLE_HOTSPOT_CLICK
    for event HOTSPOT_CLICK of CL_GUI_ALV_GRID
    importing
      !E_ROW_ID
      !E_COLUMN_ID
      !ES_ROW_NO .
  methods HANDLE_TOOLBAR
    for event TOOLBAR of CL_GUI_ALV_GRID
    importing
      !E_OBJECT
      !E_INTERACTIVE .
  methods HANDLE_DATA_CHANGED
    for event DATA_CHANGED of CL_GUI_ALV_GRID
    importing
      !ER_DATA_CHANGED .
  methods HANDLE_DATA_CHANGED_FINISHED
    for event DATA_CHANGED_FINISHED of CL_GUI_ALV_GRID
    importing
      !E_MODIFIED
      !ET_GOOD_CELLS .
  methods LIBERACION_MASIVA_MM
    importing
      !T_SELECTED_ROWS type LVC_T_ROID .
  methods GENERAR_LIBERACION_MM
    importing
      !I_GJAHR type GJAHR
      !T_BELNR type ANY TABLE .
  methods FUNCIONPDF
    importing
      value(WA_SALIDA) type ZEFI_0026 .
  methods FUNCIONCIERRE
    exporting
      !LT_SALIDA_TEMP type T_ZEFI_0026
      !LV_STATUS type ZTFI_0074-STATUS
      !LT_SELECTED_ROWS type LVC_T_ROID .
  methods FUNCIONACRE
    exporting
      !LT_SALIDA_TEMP type T_ZEFI_0026
      !LV_STATUS type ZTFI_0074-STATUS
      !LT_SELECTED_ROWS type LVC_T_ROID .
  methods FUNCIONWF
    exporting
      !LT_SALIDA_TEMP type T_ZEFI_0026
      !LT_SELECTED_ROWS type LVC_T_ROID .
  methods BLOQUEO
    importing
      !LT_ZCB_RECFACTPROV type ZEFI_0026
    returning
      value(ESTADO) type ABAP_BOOL .
  methods DESBLOQUEO
    importing
      !LT_ZCB_RECFACTPROV type ZEFI_0026 .
  methods MOSTRAR_LOG_PROC_MASI
    importing
      !LT_MOD_SAVE type T_MODLOG .
  methods SET_FIELDNAME
    importing
      !I_FIELDNAME type FIELDNAME
      !I_SELECTED_ROWS_T type LVC_T_ROID .
  methods GET_FIELDNAME
    importing
      !I_TITULO type CUA_TIT_TX
      !I_FIELDNAME type FIELDNAME
    exporting
      !E_RETURN type C
      !E_VALOR type SPO_VALUE .
  methods MOD_FECHA
    importing
      !LT_SALIDA_TEMP type T_ZEFI_0026
      !LT_SELECTED_ROWS type LVC_T_ROID .
  methods MAIN .
  methods HANDLE_CLOSE
    for event CLOSE of CL_GUI_DIALOGBOX_CONTAINER
    importing
      !SENDER .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_DTE_MONITOR IMPLEMENTATION.


  METHOD acepta_rechaza_doc.


    DATA lv_respuesta TYPE c.
    DATA: lv_acepta_rechaza TYPE char1.
    error = 'X'.
    CLEAR error.
    DATA: e_salida TYPE zst_dte_ack.

    CLEAR e_salida.

    e_salida-rutreceptor = entrada-stcd1.


    DATA: lv_paval TYPE t001z-paval.

    SELECT SINGLE paval
    FROM t001z
    INTO lv_paval
    WHERE bukrs = entrada-bukrs AND
          party = 'TAXNR'.
    IF sy-dbcnt = 1.
      REPLACE ALL OCCURRENCES OF '.' IN  lv_paval WITH space.
      CONDENSE lv_paval NO-GAPS.
      e_salida-rutrecibe = lv_paval.
      e_salida-rutreceptor = lv_paval.
    ENDIF.

    e_salida-tipodte = entrada-tipodte.
    e_salida-folio = entrada-xblnr.
    CONCATENATE entrada-bldat+0(4) '-' entrada-bldat+4(2) '-' entrada-bldat+6(2) INTO e_salida-fchemis.

    e_salida-rutemisor = entrada-stcd1.

    DATA: lv_monto TYPE string.

    "lv_monto = entrada-wrbtr.
    DATA: lv_amount TYPE bapicurr_d.
    CLEAR lv_amount.

    CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
      EXPORTING
        currency        = entrada-waers
        amount_internal = entrada-wrbtr
      IMPORTING
        amount_external = lv_amount.

    lv_monto = lv_amount.

    e_salida-mnttotal = lv_monto.

    e_salida-codenvio = entrada-docnum.
    e_salida-idrespuesta = entrada-docnum.

    e_salida-estadodte = '0'.
    e_salida-estadodteglosa = TEXT-gl2.

    IF acepta_rechaza = '6'.
      glosa = me->gv_glosa.
      e_salida-estadodte = '2'.
      IF glosa IS NOT INITIAL.
        e_salida-estadodteglosa = glosa.
      ELSE.
        e_salida-estadodteglosa = TEXT-gl1.
        glosa =  TEXT-gl1.
      ENDIF.
    ENDIF.
    e_salida-bukrs = entrada-bukrs.

*    CALL FUNCTION 'ZFI_0008DTE'
*      EXPORTING
*        acknowledgment = e_salida
*        commit         = 'X'
*      IMPORTING
*        error          = error.
    CALL METHOD me->enviar_rechazoacep
      EXPORTING
        i_salida  = e_salida
        i_entrada = entrada
        i_commit  = 'X'
      IMPORTING
        e_error   = error.

**********************************************************
* Solo se cambia el estatus a N, si se aceptA/rechaza    *
* Y el Ws dio error, si se acepta y el WS da error       *
**********************************************************

    IF error = 'W'. "AND acepta_rechaza = '6'.
      lv_acepta_rechaza = 'N'.
      e_salida-estadodteglosa = TEXT-221.
      glosa = TEXT-221.
    ELSE.
      IF  acepta_rechaza ='6'.
        IF error = '8' .
          lv_acepta_rechaza = 'N'.
          e_salida-estadodteglosa = TEXT-221.
          glosa = TEXT-221.
        ELSEIF  error = '3' .
          lv_acepta_rechaza = 'N'.
          e_salida-estadodteglosa = TEXT-221.
          glosa = TEXT-221.
        ELSEIF  error = '27' .
          lv_acepta_rechaza = 'N'.
          e_salida-estadodteglosa = TEXT-221.
          glosa = TEXT-221.
        ELSE.
          lv_acepta_rechaza = acepta_rechaza.
        ENDIF.
      ELSE.
        lv_acepta_rechaza = acepta_rechaza.
      ENDIF.
    ENDIF.

*** Log de Modificaciones Status (Monitor)
    DATA objkey TYPE cdhdr-objectid.

    DATA: old_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074.
    DATA: old_wa TYPE ztfi_0074.
    CLEAR old_ztfi_0074[].
    CLEAR old_wa.

    DATA: new_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074.
    DATA: new_wa TYPE ztfi_0074.
    CLEAR new_ztfi_0074[].
    CLEAR new_wa.

    IF entrada-status = 'S'.
      IF lv_acepta_rechaza = '6'. " Modifico estatus en S si queda rechazado
        MOVE-CORRESPONDING entrada TO old_wa.
        new_wa = old_wa.
        new_wa-status = lv_acepta_rechaza. "acepta_rechaza.
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
            tcode         = 'ZFI_0012'
            utime         = sy-uzeit
            udate         = sy-datum
            username      = sy-uname
            upd_ztfi_0074 = 'U'
          TABLES
            xztfi_0074    = new_ztfi_0074
            yztfi_0074    = old_ztfi_0074.

        MODIFY ztfi_0074 FROM  new_wa.
        COMMIT WORK AND WAIT.
      ENDIF.

    ELSE.

      SELECT SINGLE * FROM ztfi_0074 INTO old_wa
               WHERE bukrs EQ entrada-bukrs
                 AND xblnr EQ entrada-xblnr
                 AND lifnr EQ entrada-lifnr
                 AND tipodte EQ entrada-tipodte.
      IF sy-subrc EQ 0.
        new_wa = old_wa.
        new_wa-status = lv_acepta_rechaza. "acepta_rechaza.
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
            tcode         = 'ZDTE_003'
            utime         = sy-uzeit
            udate         = sy-datum
            username      = sy-uname
            upd_ztfi_0074 = 'U'
          TABLES
            xztfi_0074    = new_ztfi_0074
            yztfi_0074    = old_ztfi_0074.
      ENDIF.
***  Log de Modificaciones Status (Monitor)
      UPDATE ztfi_0074 SET status = lv_acepta_rechaza  glosa = e_salida-estadodteglosa  WHERE bukrs = entrada-bukrs AND
                                                                  xblnr = entrada-xblnr AND
                                                                  lifnr = entrada-lifnr AND
                                                                  tipodte = entrada-tipodte.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD.


  METHOD acepta_rechaza_mer.
  ENDMETHOD.


  METHOD bloqueo.
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
  ENDMETHOD.


  METHOD buscar_conf.


    FIELD-SYMBOLS:<fs> TYPE ANY TABLE.
    DATA: wa_ref TYPE REF TO data.
    CREATE DATA wa_ref TYPE STANDARD TABLE OF ztfi_0079.
    ASSIGN wa_ref->* TO <fs>.
    CHECK <fs> IS ASSIGNED.

    SELECT *
    FROM ztfi_0079
    INTO TABLE <fs>
    WHERE sociedad = me->bukrs.
    IF sy-dbcnt >= 1.
      me->conf_status[] = <fs>[].
    ENDIF.

    FREE <fs>.
    UNASSIGN <fs>.
    FREE wa_ref.

    CREATE DATA wa_ref TYPE STANDARD TABLE OF ztfi_0080.
    ASSIGN wa_ref->* TO <fs>.
    CHECK <fs> IS ASSIGNED.

    SELECT *
    FROM ztfi_0080
    INTO TABLE <fs>
    WHERE bukrs = me->bukrs.
    IF sy-dbcnt >= 1.
      me->conf_boton[] = <fs>[].
    ENDIF.

    FREE <fs>.
    UNASSIGN <fs>.
    FREE wa_ref.

    CREATE DATA wa_ref TYPE STANDARD TABLE OF  ztfi_0081.
    ASSIGN wa_ref->* TO <fs>.
    CHECK <fs> IS ASSIGNED.

    SELECT *
    FROM ztfi_0081
    INTO TABLE <fs>
    WHERE bukrs = me->bukrs.
    IF sy-dbcnt >= 1.
      me->conf_trans[] = <fs>[].
    ENDIF.


    FREE <fs>.
    UNASSIGN <fs>.
    FREE wa_ref.

*Esto es para soluciones sin CDS
*Debe ir antes q obtener_datos
*    CREATE DATA wa_ref TYPE STANDARD TABLE OF  ztfi_0074fr.
*    ASSIGN wa_ref->* TO <fs>.
*    CHECK <fs> IS ASSIGNED.
*    SELECT *
*    FROM ztfi_0074fr
*    INTO TABLE <fs>
*      WHERE bukrs = me->bukrs
*        AND bldat IN me->bldat
*        AND stcd1 IN me->stcd1
*        AND lifnr IN me->lifnr
*        AND estatus NE space.
*    IF sy-dbcnt >= 1.
*      me->fechasii[] = <fs>[].
*    ENDIF.
*    FREE <fs>.
*    UNASSIGN <fs>.
*    FREE wa_ref.


  ENDMETHOD.


  METHOD buscar_datos.
    DATA: numero  TYPE int4,
          lt_styl TYPE lvc_t_styl,
          ls_styl TYPE lvc_s_styl.
    FIELD-SYMBOLS: <fs> TYPE ANY TABLE.

    DATA: wa_ref TYPE REF TO data.
    CREATE DATA wa_ref TYPE STANDARD TABLE OF zefi_0026."zcds_0074u.
    ASSIGN wa_ref->* TO <fs>.
    CHECK <fs> IS ASSIGNED.

**//..
    TRY .

        SELECT * FROM zcds_0074u( p_dia = @sy-datum , p_totdias = @me->dias )
              INTO CORRESPONDING FIELDS OF TABLE @<fs>
        WHERE bukrs  EQ @me->bukrs
          AND zdte_tipo IN @me->dtetipo
          AND lifnr   IN @me->lifnr
          AND bldat   IN @me->bldat
          AND status  IN @me->status
          AND nombana IN @me->nombana
          AND xblnr   IN @me->folio
          AND tipodte IN @me->tipd
          AND cpudt   IN @me->cpudt
          AND stcd1   IN @me->stcd1.

        IF sy-dbcnt >= 1.
          SORT <fs> BY ('BUKRS') ('LIFNR') ('XBLNR') ('TIPODTE') DESCENDING.
          DELETE ADJACENT DUPLICATES FROM <fs>
             COMPARING ('BUKRS') ('LIFNR') ('XBLNR') ('TIPODTE').
          me->datos[] = <fs>[].
          "---> habilitar/deshabilitar campo Orden de Compra.
          LOOP AT me->datos ASSIGNING FIELD-SYMBOL(<f1>)   .
            REFRESH: lt_styl.
            CLEAR: lt_styl,
                 ls_styl.
            IF <f1>-ebeln IS INITIAL .
              ls_styl-fieldname = 'EBELN'.
              ls_styl-style = cl_gui_alv_grid=>mc_style_enabled.
              APPEND ls_styl TO lt_styl.
            ELSE.
              ls_styl-fieldname = 'EBELN'.
              ls_styl-style = cl_gui_alv_grid=>mc_style_disabled.
              APPEND ls_styl TO lt_styl.
            ENDIF.
            INSERT LINES OF lt_styl INTO TABLE <f1>-cellstyl.
          ENDLOOP.
          me->datos_or[] =  me->datos[].
        ENDIF.

      CATCH cx_sy_open_sql_db.
    ENDTRY.
  ENDMETHOD.


  METHOD cierre_doc.


    DATA lv_respuesta TYPE c.
    DATA: lv_acepta_rechaza TYPE char1.
    CLEAR error.
    glosa = me->gv_glosa.
    IF glosa IS NOT INITIAL.

    ELSE.

      glosa =  TEXT-gl3.
    ENDIF.

    lv_acepta_rechaza = 'C'.

*** Log de Modificaciones Status (Monitor)
    DATA objkey TYPE cdhdr-objectid.

    DATA: old_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074.
    DATA: old_wa TYPE ztfi_0074.
    CLEAR old_ztfi_0074[].
    CLEAR old_wa.

    DATA: new_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074.
    DATA: new_wa TYPE ztfi_0074.
    CLEAR new_ztfi_0074[].
    CLEAR new_wa.



    SELECT SINGLE * FROM ztfi_0074 INTO old_wa
             WHERE bukrs EQ entrada-bukrs
               AND xblnr EQ entrada-xblnr
               AND lifnr EQ entrada-lifnr
               AND tipodte EQ entrada-tipodte.
    IF sy-subrc EQ 0.
      new_wa = old_wa.
      new_wa-status = lv_acepta_rechaza. "Cierre de documento.
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
          tcode         = 'ZDTE_0003'
          utime         = sy-uzeit
          udate         = sy-datum
          username      = sy-uname
          upd_ztfi_0074 = 'U'
        TABLES
          xztfi_0074    = new_ztfi_0074
          yztfi_0074    = old_ztfi_0074.
    ENDIF.
***  Log de Modificaciones Status (Monitor)
    UPDATE ztfi_0074 SET status = lv_acepta_rechaza  glosa = glosa  WHERE bukrs = entrada-bukrs AND
                                                                xblnr = entrada-xblnr AND
                                                                lifnr = entrada-lifnr AND
                                                                tipodte = entrada-tipodte.
    COMMIT WORK AND WAIT.


  ENDMETHOD.


  METHOD constructor.
    IF bukrs IS NOT INITIAL.
      me->bukrs = bukrs.
      me->lifnr[] = lifnr[].
      me->status[] = status[].
      me->stcd1[] = stcd1[].
      me->bldat[] = bldat[].
      me->nombana[] = nombana[].
      me->dtetipo[] = dtetipo[].
      me->folio = folio[].
      me->tipd = tipd[].
      me->dias = dias.
      me->cpudt = cpudt[].
      me->archv = archivado.

*      me->buscar_datos( ).
*      me->buscar_conf( ).
      SELECT SINGLE vatint INTO  me->gs_vatint FROM ztfi_0078
    WHERE bukrs EQ me->bukrs.


    ENDIF.
  ENDMETHOD.


  METHOD create_attachment_bor.
    DATA: lv_pdf    TYPE xstring,
          lv_fname  TYPE string,
          lv_size   TYPE wsrm_error-wsrm_direction, "i,
          ls_obja   TYPE borident,
          ls_objb   TYPE borident,
          lv_obj    TYPE obj_record, "swc_object,
          ls_binrel TYPE gbinrel,
          lt_binatt TYPE STANDARD TABLE OF brelattr,
          lt_data   TYPE TABLE OF solix.
    DATA: co_type TYPE string.
    DATA: t_0078 TYPE STANDARD TABLE OF ztfi_0078.
    DATA: wa_0078 TYPE ztfi_0078.

    CLEAR: ls_obja,
           ls_objb,
           lv_pdf,
           lt_data.

**    INCLUDE zfir_cntn01.
**    swc_container lt_message_container.

**//.. Crear clave según tipo de objeto
    CASE i_objtyp.
      WHEN 'BKPF'. " Doc. FI
        IF i_ztfi_0074-tcode EQ 'FBV3'.
          CONCATENATE i_ztfi_0074-bukrs "company code
                      i_ztfi_0074-belnr "FI Document
                      i_ztfi_0074-gjahr "fiscal year
                      INTO ls_obja-objkey.
        ELSE.
          DATA: lv_awkey TYPE bkpf-awkey,
                lv_awtyp TYPE bkpf-awtyp VALUE 'RMRP',
                lv_belnr TYPE bkpf-belnr,
                lv_gjahr TYPE bkpf-gjahr.

          CLEAR: lv_belnr,
                 lv_gjahr,
                 lv_awkey.

          CONCATENATE i_ztfi_0074-belnr i_ztfi_0074-gjahr INTO lv_awkey.

          " Buscar Doc. FI del Doc. MM
          SELECT SINGLE belnr gjahr INTO (lv_belnr, lv_gjahr)
          FROM bkpf
          WHERE awtyp EQ lv_awtyp
            AND awkey EQ lv_awkey
            AND awsys EQ ''.
          IF sy-subrc EQ 0.
            CONCATENATE i_ztfi_0074-bukrs "company code
                        lv_belnr          "FI Document
                        lv_gjahr          "fiscal year
                        INTO ls_obja-objkey.
          ENDIF.
        ENDIF.

      WHEN 'BUS2081'. " Factura recibida
        CONCATENATE i_ztfi_0074-belnr "FI Document
                    i_ztfi_0074-gjahr "fiscal year
                    INTO ls_obja-objkey.
      WHEN OTHERS.
    ENDCASE.

    CHECK ls_obja-objkey IS NOT INITIAL.

**//.. buscar PDF en WS
    CLEAR co_type. CLEAR wa_0078. REFRESH t_0078.
    SELECT SINGLE * INTO  wa_0078 FROM ztfi_0078
       WHERE bukrs EQ i_ztfi_0074-bukrs.


    CLEAR  co_type.
    SELECT SINGLE nombre_int INTO co_type FROM ztfi_0078b
       WHERE vatint = wa_0078-vatint
       AND   codint = '0003'.

    IF co_type IS NOT INITIAL.
      CALL METHOD (co_type)=>('VER_PDF') "zcl_monitor=>ver_pdf
        EXPORTING
          xblnr   = i_ztfi_0074-xblnr
          bukrs   = i_ztfi_0074-bukrs
          tipodte = i_ztfi_0074-tipodte
          stcd1   = i_ztfi_0074-stcd1
        IMPORTING
          pdf     = lv_pdf.
    ENDIF.
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
        CONCATENATE i_ztfi_0074-stcd1   i_ztfi_0074-rutrecep
                    i_ztfi_0074-tipodte i_ztfi_0074-xblnr '.PDF'
                    INTO lv_fname.
**
**        swc_container      lv_cont.
**        swc_create_object  lv_obj  'MESSAGE'       ''.
**        swc_set_element    lv_cont 'NO_DIALOG'     'X'.
**        swc_set_element    lv_cont 'DOCUMENTTITLE' lv_fname.
**        swc_set_table      lv_cont 'Content_Hex'   lt_data.
**        swc_set_element    lv_cont 'DOCUMENTTYPE'  'PDF'. " EXT
**        swc_set_element    lv_cont 'DOCUMENTNAME' 'MESSAGE'.
**        swc_set_element    lv_cont 'DOCUMENTSIZE'  lv_size.
**        swc_refresh_object lv_obj.
**        swc_call_method    lv_obj  'CREATE'        lv_cont.
**        swc_get_object_key lv_obj  ls_objb-objkey.
**
**        ls_objb-objtype = 'MESSAGE'.   "type of attach document
**        ls_obja-objtype = u_objtyp.    "BO of SAP Document.
**
****//..
**        CALL FUNCTION 'BINARY_RELATION_CREATE_COMMIT'
**          EXPORTING
**            obj_rolea      = ls_obja
**            obj_roleb      = ls_objb
**            relationtype   = 'ATTA'
**          IMPORTING
**            binrel         = ls_binrel
**          TABLES
**            binrel_attrib  = lt_binatt
**          EXCEPTIONS
**            no_model       = 1
**            internal_error = 2
**            unknown        = 3
**            OTHERS         = 4.
**        IF sy-subrc EQ 0.
***        MESSAGE s043(sgos_msg).
**        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD desbloqueo.

    CALL FUNCTION 'DEQUEUE_EZTFI_0074'
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
*       _SCOPE         = '3'
*       _SYNCHRON      = ' '
*       _COLLECT       = ' '
      .
  ENDMETHOD.


  METHOD display.
    DATA: lc_custom_control_name TYPE scrfname VALUE 'CONTAINER',
          lr_ccontainer          TYPE REF TO cl_gui_custom_container,
          lr_docking_container   TYPE REF TO cl_gui_docking_container.
    DATA: lt_excluidos TYPE ui_functions,
          ls_variant   TYPE disvariant,
          lt_fieldcat  TYPE lvc_t_fcat, " FIELDCATALOGs
          ls_layout    TYPE lvc_s_layo. " LAYOUTs

**//.. Grid
    IF gr_alvgrid IS INITIAL.
      IF cl_gui_alv_grid=>offline( ) IS INITIAL.
**//..
        CREATE OBJECT lr_ccontainer
          EXPORTING
            container_name              = lc_custom_control_name
            style                       = cl_gui_custom_container=>ws_maximizebox
          EXCEPTIONS
            cntl_error                  = 1
            cntl_system_error           = 2
            create_error                = 3
            lifetime_error              = 4
            lifetime_dynpro_dynpro_link = 5
            OTHERS                      = 6.

        IF sy-subrc NE 0.
          MOVE: sy-msgid TO r_return-id,
                sy-msgty TO r_return-type,
                sy-msgno TO r_return-number,
                sy-msgv1 TO r_return-message_v1,
                sy-msgv2 TO r_return-message_v2,
                sy-msgv3 TO r_return-message_v3,
                sy-msgv4 TO r_return-message_v4.
          EXIT.
        ENDIF.

**//.. from here as usual..you need to specify parent as splitter part
**//..which we alloted for grid
        CREATE OBJECT gr_alvgrid
          EXPORTING
            i_parent          = cl_gui_custom_container=>default_screen
          EXCEPTIONS
            error_cntl_create = 1
            error_cntl_init   = 2
            error_cntl_link   = 3
            error_dp_create   = 4
            OTHERS            = 5.
        IF sy-subrc NE 0.
          MOVE: sy-msgid TO r_return-id,
                sy-msgty TO r_return-type,
                sy-msgno TO r_return-number,
                sy-msgv1 TO r_return-message_v1,
                sy-msgv2 TO r_return-message_v2,
                sy-msgv3 TO r_return-message_v3,
                sy-msgv4 TO r_return-message_v4.
          EXIT.
        ENDIF.

      ELSE.
        CREATE OBJECT gr_alvgrid
          EXPORTING
            i_parent          = lr_docking_container
          EXCEPTIONS
            error_cntl_create = 1
            error_cntl_init   = 2
            error_cntl_link   = 3
            error_dp_create   = 4
            OTHERS            = 5.
        IF sy-subrc NE 0.
          MOVE: sy-msgid TO r_return-id,
                sy-msgty TO r_return-type,
                sy-msgno TO r_return-number,
                sy-msgv1 TO r_return-message_v1,
                sy-msgv2 TO r_return-message_v2,
                sy-msgv3 TO r_return-message_v3,
                sy-msgv4 TO r_return-message_v4.
          EXIT.
        ENDIF.
      ENDIF.

**//.. Llamado a ALV
      ls_layout-sel_mode   = 'D'. " Selección multiple
      ls_layout-zebra      = abap_true.
      ls_layout-stylefname = 'CELLSTYL'.
      ls_layout-ctab_fname = 'COLORTAB'.
      ls_layout-info_fname = 'COLORLI'.
      ls_layout-cwidth_opt = abap_true.

      REFRESH: lt_fieldcat,
               lt_excluidos.

**//.. Generar Catálogo
      CALL METHOD me->generar_catalogooo
        IMPORTING
          ti_catalogo = lt_fieldcat.

**//.. Excluir funciones
      CALL METHOD me->generar_excluidos
        IMPORTING
          ti_excluidos = lt_excluidos.

**//.. Layout General x Pais.
*      ls_variant-report = sy-repid.
      CALL METHOD me->generar_variante
        IMPORTING
          i_variant = ls_variant.



      CALL METHOD gr_alvgrid->set_table_for_first_display
        EXPORTING
*         i_structure_name              = gv_structure
          is_layout                     = ls_layout
          it_toolbar_excluding          = lt_excluidos
          i_save                        = 'A'
          is_variant                    = ls_variant
          i_default                     = abap_true
        CHANGING
          it_fieldcatalog               = lt_fieldcat
          it_outtab                     = me->datos
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.
      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

****//.. Creating an instance for the event handler

**//.. Registering handler methods to handle ALV Grid events
      SET HANDLER me->handle_toolbar               FOR gr_alvgrid.
      SET HANDLER me->handle_user_command          FOR gr_alvgrid.
      SET HANDLER me->handle_double_click          FOR gr_alvgrid.
      SET HANDLER me->handle_hotspot_click         FOR gr_alvgrid.
      SET HANDLER me->handle_data_changed          FOR gr_alvgrid.
      SET HANDLER me->handle_data_changed_finished FOR gr_alvgrid.

**//.. Call method 'set_toolbar_interactive' to raise event TOOLBAR.
      CALL METHOD gr_alvgrid->set_toolbar_interactive.

***//.. Eventos de edición
      CALL METHOD gr_alvgrid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.

      CALL METHOD gr_alvgrid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    ELSE.
**//..
      CALL METHOD gr_alvgrid->refresh_table_display
        EXCEPTIONS
          finished = 1
          OTHERS   = 2.
      IF sy-subrc NE 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD documento_modificacion.

    DATA objkey TYPE cdhdr-objectid.
    DATA lt_cdred_str TYPE cdred_str_tab.
    DATA editpos_with_header TYPE TABLE OF cdred.
    DATA editpos_foreign TYPE cdredcd_tab.
    DATA applicationid TYPE repid.
    DATA ec TYPE i VALUE 150.
    DATA el TYPE i VALUE 20.
    DATA sc TYPE i VALUE 10.
    DATA sl TYPE i VALUE 10.

    objkey(3) = sy-mandt.
    objkey+3(4) = sociedad.
    objkey+7(16) = documento.
    objkey+23(10) = proveedor.
    objkey+33(3) = tipodte.

    CALL FUNCTION 'CHANGEDOCUMENT_READ'
      EXPORTING
        objectclass       = objekt
        objectid          = objkey
      IMPORTING
        et_cdred_str      = lt_cdred_str[]
      TABLES
        editpos           = editpos_with_header
      EXCEPTIONS
        no_position_found = 1
        OTHERS            = 2.

    editpos_foreign = get_other_chdocs( bukrs = sociedad lifnr = proveedor xblnr = documento ).

    IF NOT editpos_foreign[] IS INITIAL.
      APPEND LINES OF editpos_foreign TO editpos_with_header.
    ENDIF.

    IF sy-subrc = 0.
      applicationid = objekt.
      CALL FUNCTION 'CHANGEDOCUMENT_DISPLAY'
        EXPORTING
          i_applicationid       = applicationid
          flg_autocondense      = 'X'
          i_objectclas          = objekt
          it_cdred_str          = lt_cdred_str[]
          i_screen_start_column = sc
          i_screen_start_line   = sl
          i_screen_end_column   = ec
          i_screen_end_line     = el
        TABLES
          i_cdred               = editpos_with_header.
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
              CALL FUNCTION 'ZFI_0028DTE'
                EXPORTING
                  i_lifnr   = wa_salida-lifnr
                  i_bukrs   = wa_salida-bukrs
                  i_xblnr   = wa_salida-xblnr
                  i_tipodte = wa_salida-tipodte
                  i_glosa   = wa_salida-glosa.

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


  METHOD funcioncierre.
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
          lv_glosa1            TYPE char100.
    CLEAR lv_error_acep.
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

      lv_text = TEXT-050.
      lv_texto_popup =  lv_text.

*Pregunta  Si desea Modificar estarus
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          text_question = lv_texto_popup
          text_button_1 = 'Si'
          text_button_2 = 'No'
        IMPORTING
          answer        = lv_respuesta.
    ENDIF.

    IF lv_respuesta = '1'. " Si es la respuesta es SI
      IF  me->gv_glosa IS INITIAL.
        lv_sourcetext = TEXT-012.
        lv_titel = TEXT-229.
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
          MESSAGE TEXT-227 TYPE 'I' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
      ENDIF.

      LOOP AT lt_selected_rows INTO wa_selected_row.
        READ TABLE me->datos INTO wa_salida INDEX wa_selected_row-row_id.
        lv_registro = wa_selected_row-row_id.
        CLEAR lv_documento_mensaje. CLEAR edo_bloqueo.

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
        "-----No se pude cierre de documento
        IF wa_salida-status = 'A' OR wa_salida-status = 'S'.
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
          ENDIF.
        ENDIF.

        IF lv_flag = 'X'.
          "---> Se llama cierre de documento
          CALL METHOD me->cierre_doc
            EXPORTING
              entrada        = wa_salida
              acepta_rechaza = ' '
            IMPORTING
              error          = lv_error_acep
              glosa          = lv_glosa1.
          IF lv_error_acep IS INITIAL.
            CLEAR ln_status.
            READ TABLE  me->conf_status INTO ln_status WITH KEY estatus =  wa_salida-status.
            IF sy-subrc = 0.
              wa_salida-icon = ln_status-icono.
            ENDIF.
            MODIFY me->datos FROM wa_salida INDEX lv_registro.

            CLEAR: lv_mensaje,
            ls_mod_save.
            CONCATENATE TEXT-216 lv_documento_mensaje
                        TEXT-228 INTO lv_mensaje SEPARATED BY space.

            ls_mod_save-icono     = icon_green_light.
            ls_mod_save-documento = lv_documento_mensaje.
            ls_mod_save-mensaje   = lv_mensaje.
            APPEND ls_mod_save TO lt_mod_save.

          ELSE.
          ENDIF.
        ENDIF.
        "-----> Desbloqueo registro
        me->desbloqueo( EXPORTING lt_zcb_recfactprov = wa_salida ).
      ENDLOOP.
      "log de modificaciones o bloqueos realizados.
      IF lt_mod_save[] IS NOT INITIAL.
        me->mostrar_log_proc_masi( EXPORTING  lt_mod_save = lt_mod_save[] ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD funcionpdf.
    DATA: s_pdfwb  TYPE ztfi_0078-visupdf,
          gv_pdf   TYPE xstring,
          s_alias  TYPE ztfi_0078-direcpdf,
          s_vatint TYPE ztfi_0078-vatint.
    DATA: lt_data TYPE STANDARD TABLE OF x255.

    DATA: co_type TYPE string.
    DATA: co_met TYPE string.
    DATA: co_tiviewer TYPE string.
    DATA: t_0078 TYPE STANDARD TABLE OF ztfi_0078.
    DATA: wa_0078 TYPE ztfi_0078.
    DATA: gv_size TYPE i.

    TRY.
        CLEAR s_pdfwb.CLEAR s_alias.CLEAR s_vatint.
        SELECT SINGLE visupdf direcpdf vatint INTO (s_pdfwb, s_alias, s_vatint) FROM ztfi_0078
        WHERE bukrs = wa_salida-bukrs.
        IF sy-subrc  <> 0.
          " NO PUEDO DECIDIR
        ELSE.

          SELECT SINGLE nombre_int INTO co_type FROM ztfi_0078b
                 WHERE vatint = s_vatint
                 AND   codint = '0003'.

          SELECT SINGLE nombre_int INTO co_met FROM ztfi_0078b
                 WHERE vatint = s_vatint
                 AND   codint = '0012'.

          IF s_pdfwb = 'A'.

            CLEAR gv_pdf.
            CALL METHOD (co_type)=>('VER_PDF_ARCH') "zcl_dte_monitor=>ver_pdf_arch
              EXPORTING
                xblnr        = wa_salida-xblnr
                bukrs        = wa_salida-bukrs
                tipo_dte     = wa_salida-tipodte
                rut_emisor   = wa_salida-stcd1
                rut_receptor = wa_salida-rutrecep
              IMPORTING
                pdf          = gv_pdf.

          ELSE.

            CALL METHOD (co_type)=>(co_met) "zcl_dte_monitor=>ver_pdf
              EXPORTING
                xblnr   = wa_salida-xblnr
                bukrs   = wa_salida-bukrs
                tipodte = wa_salida-tipodte
                stcd1   = wa_salida-stcd1
              IMPORTING
                pdf     = gv_pdf.

          ENDIF.
          IF gv_pdf IS NOT INITIAL.
            CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
              EXPORTING
                buffer        = gv_pdf
              IMPORTING
                output_length = gv_size
              TABLES
                binary_tab    = lt_data.

            CLEAR co_tiviewer.
            SELECT SINGLE nombre_int INTO co_tiviewer FROM ztfi_0078b
            WHERE vatint = s_vatint
            AND   codint = '0014'.
            IF sy-subrc = 0.
              CASE co_tiviewer.
                WHEN 'P'.
*-----> Visualizar. PDF en Pantalla Preview SAP
                  CALL FUNCTION 'ZFI_0060DTE'
                    TABLES
                      binary_tab = lt_data.
                WHEN 'W'.
*-----> Visualizar. PDF en Browser
                  CALL FUNCTION 'ZFI_0061DTE'
                    TABLES
                      binary_tab = lt_data.
                WHEN 'D'.
*-----> Visualizar. Bajar PDF y visualizacion por PDF Reader
                  CALL FUNCTION 'ZFI_0062DTE'
                    EXPORTING
                      path       = me->path
                      gv_size    = gv_size
                      i_datos    = wa_salida
                    IMPORTING
                      e_path     = me->path
                    TABLES
                      binary_tab = lt_data.
                WHEN OTHERS.
              ENDCASE.
            ELSE.
*-----> Default es por Bajada y visualizacion por PDF Reader
              CALL FUNCTION 'ZFI_0062DTE'
                EXPORTING
                  path       = me->path
                  gv_size    = gv_size
                  i_datos    = wa_salida
                IMPORTING
                  e_path     = me->path
                TABLES
                  binary_tab = lt_data.
            ENDIF.
          ENDIF.
        ENDIF.
*              CATCH cx_pdf.
      CATCH cx_static_check.
        MESSAGE TEXT-212 TYPE 'I'.
    ENDTRY.

  ENDMETHOD.


  METHOD funcionwf.
    DATA:
      wa_salida            TYPE zefi_0026,
      ti_ztfi_0074_bd      TYPE STANDARD TABLE OF ztfi_0074,
      wa_selected_row      LIKE LINE OF lt_selected_rows,
      lv_registro          TYPE i,
      ln_salida_or         TYPE zefi_0026,
      lv_salida_original   TYPE zefi_0026,
      lv_flag              TYPE c,
      vl_mens              TYPE char100,
      lv_mensaje           TYPE char200,
      lv_documento_mensaje TYPE char200,
      lv_glosa(100)        TYPE c,
      lv_text              TYPE trm080-text,
      ln_status            TYPE ztfi_0079,
      lt_mod_save          TYPE STANDARD TABLE OF ty_mod,
      ls_mod_save          TYPE ty_mod,
      lv_respuesta         TYPE c,
      ls_ztfi_0074_bd      LIKE LINE OF ti_ztfi_0074_bd.
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

    ENDIF.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question = '¿Desea liberar documento / Enviar WF?'
        text_button_1 = 'Si'
        text_button_2 = 'No'
      IMPORTING
        answer        = lv_respuesta.
    CHECK lv_respuesta = '1'.

    LOOP AT lt_selected_rows INTO wa_selected_row.
      READ TABLE me->datos INTO wa_salida INDEX wa_selected_row-row_id.
      lv_registro = wa_selected_row-row_id.
      CLEAR lv_documento_mensaje.
      CONCATENATE wa_salida-bukrs '/' wa_salida-xblnr INTO lv_documento_mensaje.

      "--->registro en BD actual.
      CLEAR ls_ztfi_0074_bd.
      READ TABLE ti_ztfi_0074_bd WITH KEY bukrs = wa_salida-bukrs
                                          xblnr = wa_salida-xblnr
                                          lifnr = wa_salida-lifnr
                                          tipodte = wa_salida-tipodte
                                          INTO ls_ztfi_0074_bd.
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
      lv_flag = 'X'.
      IF ls_ztfi_0074_bd-aprobador NE  lv_salida_original-aprobador OR
        ls_ztfi_0074_bd-nombana NE lv_salida_original-nombana OR
        ls_ztfi_0074_bd-texto_libre NE lv_salida_original-texto_libre OR
        ls_ztfi_0074_bd-status NE lv_salida_original-status.
        CLEAR: lv_flag.
        "---->validación sesion.
**        IF gt_sesion[] IS NOT INITIAL.
**          FREE lt_sesion.
**          CLEAR lv_flag.CLEAR ls_sesion.
**          IMPORT gt_sesion = lt_sesion FROM DATABASE indx(zq)  CLIENT sy-mandt ID 'ZTFI0074'.
**
**          READ TABLE gt_sesion  INTO ls_sesion WITH KEY  bukrs = wa_salida-bukrs
**                                        xblnr = wa_salida-xblnr
**                                        lifnr = wa_salida-lifnr
**                                        tipodte = wa_salida-tipodte
**                                        nombana = wa_salida-nombana
**                                         texto_libre = wa_salida-texto_libre
**                                        aprobador = wa_salida-aprobador.
**          IF sy-subrc = 0.
**            READ TABLE lt_sesion INTO ln_sesion  WITH KEY  bukrs = ls_sesion-bukrs
**                                        xblnr = ls_sesion-xblnr
**                                        lifnr = ls_sesion-lifnr
**                                        tipodte = ls_sesion-tipodte
**                                        usuario = sy-uname.
**            IF sy-subrc = 0.
**              lv_flag = 'X'.
**
**            ENDIF.
**          ENDIF.
**        ENDIF.
        "--->Fin
        IF lv_flag NE 'X'.
          CLEAR: lv_mensaje,
                ls_mod_save.
          CONCATENATE TEXT-216 lv_documento_mensaje
                      TEXT-217 INTO lv_mensaje SEPARATED BY space.

          ls_mod_save-icono     = icon_red_light.
          ls_mod_save-documento = lv_documento_mensaje.
          ls_mod_save-mensaje   = lv_mensaje.
          APPEND ls_mod_save TO lt_mod_save.
*         CONCATENATE TEXT-216 lv_documento_mensaje
*                     TEXT-217  INTO lv_mensaje SEPARATED BY space.
*         MESSAGE lv_mensaje TYPE 'I' DISPLAY LIKE 'E'.
        ENDIF.
      ELSE.
        IF wa_salida-aprobador IS NOT INITIAL.
          IF wa_salida-status EQ '2' OR wa_salida-status EQ '9' OR wa_salida-status EQ 'P'.
            DATA: lv_aprobador TYPE xubname.
            lv_aprobador = wa_salida-aprobador.
            DATA: wa_registro TYPE ztfi_0074,
                  lv_mod      TYPE c.
            MOVE-CORRESPONDING wa_salida TO wa_registro.
            CALL METHOD me->lanzar_wf
              EXPORTING
                sociedad    = wa_salida-bukrs
                factura     = wa_salida-xblnr
                proveedor   = wa_salida-lifnr
                fecha       = wa_salida-bldat
                tipodte     = wa_salida-tipodte
                responsable = lv_aprobador
                registro    = wa_registro
              IMPORTING
                mod         = lv_mod
              CHANGING
                status      = wa_salida-status.
            IF lv_mod = 'X'.
*                wa_salida-status = '3'.
              wa_salida-aprobador_real = lv_aprobador.
              CLEAR ln_status.
              READ TABLE  me->conf_status INTO ln_status WITH KEY estatus =  wa_salida-status.
              IF sy-subrc = 0.
                wa_salida-icon = ln_status-icono.
              ENDIF.
              MODIFY me->datos FROM wa_salida INDEX lv_registro.
              CLEAR: lv_mensaje,
             ls_mod_save.
              CONCATENATE TEXT-216 lv_documento_mensaje
                          TEXT-223 INTO lv_mensaje SEPARATED BY space.

              ls_mod_save-icono     = icon_green_light.
              ls_mod_save-documento = lv_documento_mensaje.
              ls_mod_save-mensaje   = lv_mensaje.
              APPEND ls_mod_save TO lt_mod_save.

            ENDIF.
          ELSE.
            "--->datos de sesion
            CLEAR wa_sesion.
            CLEAR: lv_mensaje,
              ls_mod_save.
            CONCATENATE TEXT-216 lv_documento_mensaje
                        TEXT-215 INTO lv_mensaje SEPARATED BY space.

            ls_mod_save-icono     = icon_red_light.
            ls_mod_save-documento = lv_documento_mensaje.
            ls_mod_save-mensaje   = lv_mensaje.
            APPEND ls_mod_save TO lt_mod_save.

*           MESSAGE TEXT-215 TYPE 'I' DISPLAY LIKE 'E'.
*            EXIT.
          ENDIF.
        ELSE.
          CLEAR: lv_mensaje,
             ls_mod_save.
          CONCATENATE TEXT-216 lv_documento_mensaje
                      TEXT-210 INTO lv_mensaje SEPARATED BY space.

          ls_mod_save-icono     = icon_red_light.
          ls_mod_save-documento = lv_documento_mensaje.
          ls_mod_save-mensaje   = lv_mensaje.
          APPEND ls_mod_save TO lt_mod_save.
*          MESSAGE TEXT-210 TYPE 'I' DISPLAY LIKE 'E'.
*          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
    "log de modificaciones o bloqueos realizados.
    IF lt_mod_save[] IS NOT INITIAL.
      me->mostrar_log_proc_masi( EXPORTING  lt_mod_save = lt_mod_save[] ).
    ENDIF.
  ENDMETHOD.


  METHOD generar_botones.
    DATA: wa_boton LIKE  LINE OF me->conf_boton.

    DATA: lt_botones TYPE slis_t_extab,
          wa_botones LIKE LINE OF lt_botones.

    LOOP AT  me->conf_boton[] INTO wa_boton.
      IF wa_boton-visible = '0'.
        CLEAR wa_botones.
        CASE wa_boton-boton.
          WHEN '1'.
            wa_botones-fcode = '&REL'.
          WHEN '2'.
            wa_botones-fcode = '&LOG'.
          WHEN '3'.
            wa_botones-fcode = '&MOD'.
          WHEN '4'.
            wa_botones-fcode = '&PDF'.
          WHEN '5'.
            wa_botones-fcode = '&ACEP'.
          WHEN '6'.
            wa_botones-fcode = '&REC'.
          WHEN '7'.
            wa_botones-fcode = '&GLOSA'.
          WHEN '8'.
            wa_botones-fcode = '&LIB'.
          WHEN '9'.
            wa_botones-fcode = '&DETALLE'.
          WHEN '10'.
            wa_botones-fcode = '&LIBMAS'.
        ENDCASE.
        APPEND wa_botones TO lt_botones.
      ENDIF.
    ENDLOOP.
    IF lt_botones[] IS NOT INITIAL.
      ti_botones[] = lt_botones[].
    ENDIF.
  ENDMETHOD.


  METHOD generar_cambios.
*  case visualizar.
*  when '1'.
*  IF me->conf_trans[] IS NOT INITIAL.
*    DATA: wa_conf_trans LIKE LINE OF me->conf_trans.
*    READ TABLE me->conf_trans INTO wa_conf_trans WITH KEY bukrs = bukrs
*                                                          tcode_ori = tcode.
*    IF sy-subrc = 0.
*      DATA: xwfla1 TYPE c VALUE 'X'.
*      SET PARAMETER ID 'RBN' FIELD belnr.
*      SET PARAMETER ID 'GJR' FIELD gjahr.
*      SET PARAMETER ID 'CHG' FIELD 'X'.
*      CALL TRANSACTION wa_conf_trans-tcode_des AND SKIP FIRST SCREEN.
*
*
*    ENDIF.
*  ENDIF.
*  when '2'.
*      SET PARAMETER ID 'RBN' FIELD belnr.
*      SET PARAMETER ID 'GJR' FIELD gjahr.
*      CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
*
*  ENDCASE.

    CASE tcode.
      WHEN 'FBV3'.
        SET PARAMETER ID 'BUK' FIELD bukrs.
        SET PARAMETER ID 'BLP' FIELD belnr.
        SET PARAMETER ID 'GJR' FIELD gjahr.
        IF visualizar = '1'.
          SET PARAMETER ID 'CHG' FIELD 'X'.
        ENDIF.
        CALL TRANSACTION 'FBV3' AND SKIP FIRST SCREEN.
      WHEN 'MIR4'.
        SET PARAMETER ID 'RBN' FIELD belnr.
        SET PARAMETER ID 'GJR' FIELD gjahr.
        IF visualizar = '1'.
          SET PARAMETER ID 'CHG' FIELD 'X'.
        ENDIF.
        CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
    ENDCASE.
  ENDMETHOD.


  METHOD generar_catalogo.
    DATA: lt_catalogo TYPE slis_t_fieldcat_alv.

    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZEFI_0026'
      CHANGING
        ct_fieldcat      = lt_catalogo.

    IF lt_catalogo[] IS NOT INITIAL.
      DATA: lv_registro LIKE sy-tabix,
            ln_catalogo LIKE LINE OF lt_catalogo.
      READ TABLE lt_catalogo INTO ln_catalogo INDEX 1.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
        CLEAR: ln_catalogo-seltext_l,
               ln_catalogo-seltext_m,
               ln_catalogo-seltext_s,
               ln_catalogo-reptext_ddic.
        MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
      ENDIF.

      READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'GLOSA'.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
        "ln_catalogo-hotspot = 'X'.
        ln_catalogo-seltext_m = ln_catalogo-seltext_l = TEXT-300.
        MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
      ENDIF.

      READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'CORRELATIVO'.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
        ln_catalogo-hotspot = 'X'.
        ln_catalogo-seltext_m = ln_catalogo-seltext_l = TEXT-302.
        MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
      ENDIF.

      READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'NOMBANA'.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
        ln_catalogo-edit = 'X'.
        ln_catalogo-ref_tabname = 'WRF_PPW_US_USGP'.
        ln_catalogo-ref_fieldname = 'BNAME'.
        MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
      ENDIF.
      READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'APROBADOR'.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
        ln_catalogo-edit = 'X'.
        ln_catalogo-ref_tabname = 'WRF_PPW_US_USGP'.
        ln_catalogo-ref_fieldname = 'BNAME'.
        ln_catalogo-seltext_l = ln_catalogo-seltext_s = ln_catalogo-seltext_m = 'Aprobador de GTo'.
        ln_catalogo-reptext_ddic = 'Aprobador de GTo'.
        ln_catalogo-ddic_outputlen = 15.
        ln_catalogo-outputlen = 15.
        MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
      ENDIF.

      READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'DIAS_TRANS'.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
        ln_catalogo-seltext_m = ln_catalogo-seltext_l = TEXT-301.
        MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
      ENDIF.
      READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'FECHABASE'.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
        ln_catalogo-seltext_m = ln_catalogo-seltext_l = TEXT-323.
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

      READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'CENTRO'.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
        ln_catalogo-reptext_ddic = ln_catalogo-seltext_l = ln_catalogo-seltext_s = ln_catalogo-seltext_m = TEXT-320.
        MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
      ENDIF.

      READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'HES'.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
        CLEAR ln_catalogo-reptext_ddic.
        ln_catalogo-seltext_l = ln_catalogo-seltext_s = ln_catalogo-seltext_m = TEXT-321.
        MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
      ENDIF.

      READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'KONNR'.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
        CLEAR ln_catalogo-reptext_ddic.
        ln_catalogo-reptext_ddic = ln_catalogo-seltext_l = ln_catalogo-seltext_s = ln_catalogo-seltext_m = TEXT-322.
        MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
      ENDIF.

      "----->inserción de campos tipo dte.
      READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'ZDTE_TIPO'.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
        CLEAR ln_catalogo-reptext_ddic.
        ln_catalogo-reptext_ddic = ln_catalogo-seltext_l = ln_catalogo-seltext_s = ln_catalogo-seltext_m  = TEXT-310.
        MODIFY lt_catalogo FROM ln_catalogo INDEX lv_registro.
      ENDIF.
      READ TABLE lt_catalogo INTO ln_catalogo WITH KEY fieldname = 'DESC_TIPO'.
      IF sy-subrc = 0.
        lv_registro = sy-tabix.
        CLEAR ln_catalogo-reptext_ddic.
        ln_catalogo-reptext_ddic = ln_catalogo-seltext_l = ln_catalogo-seltext_s = ln_catalogo-seltext_m  = TEXT-311.
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


      DELETE lt_catalogo WHERE fieldname IN r_campos.

      "    data:lt_catalogo_temp TYPE slis_t_fieldcat_alv.


      ti_catalogo[] = lt_catalogo[].
    ENDIF.

  ENDMETHOD.


  METHOD generar_catalogooo.
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

  ENDMETHOD.


  METHOD generar_excluidos.
    DATA : ls_exclude TYPE ui_func.

    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND : ls_exclude TO ti_excluidos.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND : ls_exclude TO ti_excluidos.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND : ls_exclude TO ti_excluidos.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND : ls_exclude TO ti_excluidos.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND : ls_exclude TO ti_excluidos.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND : ls_exclude TO ti_excluidos.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND : ls_exclude TO ti_excluidos.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND : ls_exclude TO ti_excluidos.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND : ls_exclude TO ti_excluidos.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND : ls_exclude TO ti_excluidos.
    ls_exclude = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND : ls_exclude TO ti_excluidos.
    ls_exclude = cl_gui_alv_grid=>mc_fc_graph.
    APPEND : ls_exclude TO ti_excluidos.
    ls_exclude = cl_gui_alv_grid=>mc_fc_info.
    APPEND : ls_exclude TO ti_excluidos.
  ENDMETHOD.


  METHOD generar_liberacion_mm.
    DATA: lt_bdcdata           TYPE TABLE OF bdcdata,
          lr_belnr             TYPE RANGE OF belnr_d, "belnr,
          ls_belnr             LIKE LINE OF lr_belnr, "IBKK_R_BELNR.
          ls_opt               TYPE ctu_params,
          lt_mensaje           TYPE TABLE OF bdcmsgcoll,
          ls_mensaje           TYPE bdcmsgcoll,
          lv_fnam              TYPE bdcdata-fnam,
          lv_sign              TYPE rsdsselopt-sign VALUE 'I',
          lv_first_time        TYPE flag,
          lv_row_number        TYPE n LENGTH 2,
          lt_mod_save          TYPE STANDARD TABLE OF ty_mod,
          ls_mod_save          TYPE ty_mod,
          lc_belnr             TYPE belnr_d,
          lv_mensaje           TYPE char200,
          lv_documento_mensaje TYPE char200.

    CLEAR ls_mod_save. REFRESH lt_mod_save.
    LOOP AT t_belnr ASSIGNING FIELD-SYMBOL(<fs_range>).
      MOVE-CORRESPONDING <fs_range> TO ls_belnr.
      APPEND ls_belnr TO lr_belnr.
      CLEAR: ls_belnr.
    ENDLOOP.

    IF lines( lr_belnr ) EQ 0.
      lr_belnr = VALUE #( BASE lr_belnr  ( sign = 'I' option = 'EQ' low = ' ' ) ).
    ENDIF.

**//..
    LOOP AT lr_belnr INTO ls_belnr.

      CLEAR ls_mensaje. REFRESH lt_mensaje.
      CLEAR lt_bdcdata. REFRESH lt_bdcdata.
      CLEAR lv_documento_mensaje.
      CONCATENATE ls_belnr-low '/' i_gjahr  INTO lv_documento_mensaje.

      SELECT belnr INTO lc_belnr FROM rbkp UP TO 1 ROWS
       WHERE belnr EQ ls_belnr-low
         AND gjahr EQ i_gjahr
         AND rbstat EQ '3'.
      ENDSELECT.
      IF sy-subrc NE 0.
        CLEAR: lv_mensaje,
         ls_mod_save.
        CONCATENATE TEXT-216 lv_documento_mensaje
                    TEXT-404 INTO lv_mensaje SEPARATED BY space.

        ls_mod_save-icono     = icon_yellow_light.
        ls_mod_save-documento = lv_documento_mensaje.
        ls_mod_save-mensaje   = lv_mensaje.
        APPEND ls_mod_save TO lt_mod_save.

      ELSE.


        lt_bdcdata = VALUE #( BASE lt_bdcdata ( program  = 'SAPMM08N' dynpro = '0100' dynbegin = 'X' )
                              ( fnam = 'BDC_CURSOR'   fval = 'SO_BELNR-LOW' )
                              ( fnam =  'SO_BELNR-LOW' fval = ls_belnr-low )
                              ( fnam = 'SO_GJAHR-LOW' fval = i_gjahr )
                              ( fnam = 'SO_USNAM-LOW' fval = '' )
                              ( fnam = 'SO_LIFNR-LOW' fval = '' )
                              ( fnam = 'SO_BUKRS-LOW' fval = me->bukrs )
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

        LOOP AT lt_mensaje INTO ls_mensaje.
          CLEAR ls_mod_save.
          IF ls_mensaje-msgtyp = 'E'.
            ls_mod_save-icono      = icon_red_light.
          ELSEIF ls_mensaje-msgtyp = 'W'.
            ls_mod_save-icono = icon_yellow_light.
          ELSE.
            ls_mod_save-icono      = icon_green_light.
          ENDIF.

          ls_mod_save-documento = lv_documento_mensaje.
          MESSAGE ID ls_mensaje-msgid TYPE ls_mensaje-msgtyp NUMBER ls_mensaje-msgnr
              WITH ls_mensaje-msgv1 ls_mensaje-msgv2 ls_mensaje-msgv3 ls_mensaje-msgv4  INTO  ls_mod_save-mensaje.
          APPEND ls_mod_save TO lt_mod_save.

        ENDLOOP.
      ENDIF.
    ENDLOOP.

    IF lt_mod_save[] IS NOT INITIAL.
      me->mostrar_log_proc_masi( EXPORTING  lt_mod_save = lt_mod_save[] ).
    ENDIF.


  ENDMETHOD.


  METHOD generar_variante.
    i_variant-report = sy-repid.
    "-----colocar variante por Pais.

  ENDMETHOD.


  METHOD get_fieldname.
 DATA: lt_val     TYPE ty_sval,
          ls_val     TYPE sval,
          lv_check   TYPE c,
          lv_bname   TYPE usr02-bname,
          lv_retcode TYPE c.

    CLEAR: lt_val.
    lv_check         = abap_true.
    ls_val-tabname   = 'ZTFI_0074'.
    ls_val-fieldname = i_fieldname.
    ls_val-field_obl = abap_true.
    APPEND ls_val TO lt_val.

**//..
    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
*       NO_VALUE_CHECK  = ' '
        popup_title     = i_titulo
        start_column    = '25'
        start_row       = '6'
      IMPORTING
        returncode      = lv_retcode
      TABLES
        fields          = lt_val
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.

    IF sy-subrc NE 0.
* Implement suitable error handling here
      e_return = abap_true.
      EXIT.
    ENDIF.

    IF lv_retcode EQ 'A'. " cancelar
      e_return = lv_retcode.
      EXIT.
    ELSE.
      READ TABLE lt_val INTO ls_val INDEX 1.
      IF sy-subrc EQ 0.
**//.. Validar con tabla de usuarios
        SELECT SINGLE bname INTO lv_bname
        FROM usr02
        WHERE bname EQ ls_val-value.
        IF sy-subrc NE 0.
          MESSAGE ID 'ZFI_0003' TYPE 'S' NUMBER '050' DISPLAY LIKE 'E'
                                WITH ls_val-value.
          e_return = abap_true.
          EXIT.
        ELSE.
          e_valor = ls_val-value.
        ENDIF.

      ELSE.
        MESSAGE ID 'ZFI_0003' TYPE 'S' NUMBER '050' DISPLAY LIKE 'E'
                              WITH ls_val-value.
        e_return = abap_true.
        EXIT.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD get_other_chdocs.
*  bukrs, lifnr, xblnr...return r_value
    DATA: lv_objectclas TYPE cdhdr-objectclas,
          lv_objectid   TYPE cdhdr-objectid.
    DATA: lt_cdred TYPE cdredcd_tab.
    DATA: lt_result TYPE cdredcd_tab.
    DATA: lt_objectid TYPE TABLE OF cdhdr-objectid.
    DATA: lv_vars TYPE string.
*obtener cada objectid involucrado para ZTFI_0075
    lv_objectclas = 'ZTFI_0075'.

    CONCATENATE sy-mandt
                bukrs
                lifnr
                xblnr
                INTO lv_objectid SEPARATED BY space.

*  lv_vars = lv_objectid && '%'.
    CONCATENATE lv_objectid '%' INTO lv_vars.

    SELECT DISTINCT objectid INTO TABLE lt_objectid FROM cdhdr WHERE objectclas = lv_objectclas AND objectid LIKE lv_vars.

*leer todos los cchanges
    LOOP AT lt_objectid INTO lv_objectid.
      CLEAR: lt_cdred.
      CALL FUNCTION 'CHANGEDOCUMENT_READ'
        EXPORTING
          objectclass       = lv_objectclas
          objectid          = lv_objectid
*    IMPORTING
*         et_cdred_str      = lt_cdred_str[]
        TABLES
          editpos           = lt_cdred
        EXCEPTIONS
          no_position_found = 1
          OTHERS            = 2.
*unirlos todos
      APPEND LINES OF lt_cdred TO lt_result.
    ENDLOOP.

*retornar valor
    r_value = lt_result.
  ENDMETHOD.


  METHOD handle_close.
* I will free textedit control and dialogbox
    CALL METHOD sender->free.

* finally flush
    CALL METHOD cl_gui_cfw=>flush
      EXCEPTIONS
        OTHERS = 1.

    LEAVE TO SCREEN 9000.

  ENDMETHOD.


  METHOD handle_data_changed.
    DATA: ls_mod_cell TYPE lvc_s_modi,
          lv_value    TYPE lvc_value,
          sw_ok       TYPE i.
    DATA: lt_mod       TYPE lvc_t_modi,
          ls_ztfi_0074 TYPE ztfi_0074,
          wa_mod       LIKE LINE OF lt_mod.
    DATA: lv_registro LIKE sy-tabix,
          ln_salida   LIKE LINE OF me->datos.

**//..  ordenamos la lista de celdas modificadas por el numero de fila
    SORT er_data_changed->mt_mod_cells BY row_id.

    lt_mod[] = er_data_changed->mt_mod_cells[].
    CHECK lt_mod[] IS NOT INITIAL.

    READ TABLE lt_mod INTO wa_mod WITH KEY fieldname = 'APROBADOR'.
    IF sy-subrc = 0.
      lv_registro = wa_mod-row_id.
      READ TABLE me->datos INTO ln_salida INDEX lv_registro.
      IF sy-subrc = 0.
        CLEAR ls_ztfi_0074.
        ln_salida-aprobador = wa_mod-value.
        TRANSLATE ln_salida-aprobador TO UPPER CASE.

        MOVE-CORRESPONDING ln_salida TO ls_ztfi_0074.

        " verificar tabla con datos modificados
        READ TABLE me->lt_ztfi_0074 WITH KEY mandt   = ln_salida-mandt
                                             bukrs   = ln_salida-bukrs
                                             xblnr   = ln_salida-xblnr
                                             lifnr   = ln_salida-lifnr
                                             tipodte = ln_salida-tipodte
                                    ASSIGNING FIELD-SYMBOL(<fs_ztfi_0074>).
        IF sy-subrc EQ 0.
          <fs_ztfi_0074> = ls_ztfi_0074.
        ELSE.
          APPEND ls_ztfi_0074 TO me->lt_ztfi_0074.
        ENDIF.

      ENDIF.
    ENDIF.

    READ TABLE lt_mod INTO wa_mod WITH KEY fieldname = 'TEXTO_LIBRE'.
    IF sy-subrc = 0.
      lv_registro = wa_mod-row_id.
      READ TABLE me->datos INTO ln_salida INDEX lv_registro.
      IF sy-subrc = 0.
        CLEAR ls_ztfi_0074.
        ln_salida-texto_libre = wa_mod-value.
        MOVE-CORRESPONDING ln_salida TO ls_ztfi_0074.

        " verificar tabla con datos modificados
        READ TABLE me->lt_ztfi_0074 WITH KEY mandt   = ln_salida-mandt
                                             bukrs   = ln_salida-bukrs
                                             xblnr   = ln_salida-xblnr
                                             lifnr   = ln_salida-lifnr
                                             tipodte = ln_salida-tipodte
                                    ASSIGNING <fs_ztfi_0074>.
        IF sy-subrc EQ 0.
          <fs_ztfi_0074> = ls_ztfi_0074.
        ELSE.
          APPEND ls_ztfi_0074 TO me->lt_ztfi_0074.
        ENDIF.

      ENDIF.
    ENDIF.

    READ TABLE lt_mod INTO wa_mod WITH KEY fieldname = 'NOMBANA'.
    IF sy-subrc = 0.
      lv_registro = wa_mod-row_id.
      READ TABLE me->datos INTO ln_salida INDEX lv_registro.
      IF sy-subrc = 0.
        CLEAR ls_ztfi_0074.
        ln_salida-nombana = wa_mod-value.
        TRANSLATE ln_salida-nombana TO UPPER CASE.

        MOVE-CORRESPONDING ln_salida TO ls_ztfi_0074.

        " verificar tabla con datos modificados
        READ TABLE me->lt_ztfi_0074 WITH KEY mandt   = ln_salida-mandt
                                             bukrs   = ln_salida-bukrs
                                             xblnr   = ln_salida-xblnr
                                             lifnr   = ln_salida-lifnr
                                             tipodte = ln_salida-tipodte
                                    ASSIGNING <fs_ztfi_0074>.
        IF sy-subrc EQ 0.
          <fs_ztfi_0074> = ls_ztfi_0074.
        ELSE.
          APPEND ls_ztfi_0074 TO me->lt_ztfi_0074.
        ENDIF.

      ENDIF.
    ENDIF.
    READ TABLE lt_mod INTO wa_mod WITH KEY fieldname = 'ZFECHAPRO'.
    IF sy-subrc = 0.
      lv_registro = wa_mod-row_id.
      READ TABLE me->datos INTO ln_salida INDEX lv_registro.
      IF sy-subrc = 0.
        CLEAR ls_ztfi_0074.
        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
          EXPORTING
            date_external            = wa_mod-value
*           ACCEPT_INITIAL_DATE      =
          IMPORTING
            date_internal            = ln_salida-zfechapro
          EXCEPTIONS
            date_external_is_invalid = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

        MOVE-CORRESPONDING ln_salida TO ls_ztfi_0074.

        " verificar tabla con datos modificados
        READ TABLE me->lt_ztfi_0074 WITH KEY mandt   = ln_salida-mandt
                                             bukrs   = ln_salida-bukrs
                                             xblnr   = ln_salida-xblnr
                                             lifnr   = ln_salida-lifnr
                                             tipodte = ln_salida-tipodte
                                    ASSIGNING <fs_ztfi_0074>.
        IF sy-subrc EQ 0.
          <fs_ztfi_0074> = ls_ztfi_0074.
        ELSE.
          APPEND ls_ztfi_0074 TO me->lt_ztfi_0074.
        ENDIF.

      ENDIF.
    ENDIF.
    READ TABLE lt_mod INTO wa_mod WITH KEY fieldname = 'ZFECHAREC'.
    IF sy-subrc = 0.
      lv_registro = wa_mod-row_id.
      READ TABLE me->datos INTO ln_salida INDEX lv_registro.
      IF sy-subrc = 0.
        CLEAR ls_ztfi_0074.
        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
          EXPORTING
            date_external            = wa_mod-value
*           ACCEPT_INITIAL_DATE      =
          IMPORTING
            date_internal            = ln_salida-zfecharec
          EXCEPTIONS
            date_external_is_invalid = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

        MOVE-CORRESPONDING ln_salida TO ls_ztfi_0074.

        " verificar tabla con datos modificados
        READ TABLE me->lt_ztfi_0074 WITH KEY mandt   = ln_salida-mandt
                                             bukrs   = ln_salida-bukrs
                                             xblnr   = ln_salida-xblnr
                                             lifnr   = ln_salida-lifnr
                                             tipodte = ln_salida-tipodte
                                    ASSIGNING <fs_ztfi_0074>.
        IF sy-subrc EQ 0.
          <fs_ztfi_0074> = ls_ztfi_0074.
        ELSE.
          APPEND ls_ztfi_0074 TO me->lt_ztfi_0074.
        ENDIF.

      ENDIF.
    ENDIF.
    READ TABLE lt_mod INTO wa_mod WITH KEY fieldname = 'EBELN'.
    IF sy-subrc = 0.
      lv_registro = wa_mod-row_id.
      READ TABLE me->datos INTO ln_salida INDEX lv_registro.
      IF sy-subrc = 0.
        CLEAR ls_ztfi_0074.
        ln_salida-ebeln = wa_mod-value.

        MOVE-CORRESPONDING ln_salida TO ls_ztfi_0074.

        " verificar tabla con datos modificados
        READ TABLE me->lt_ztfi_0074 WITH KEY mandt   = ln_salida-mandt
                                             bukrs   = ln_salida-bukrs
                                             xblnr   = ln_salida-xblnr
                                             lifnr   = ln_salida-lifnr
                                             tipodte = ln_salida-tipodte
                                    ASSIGNING <fs_ztfi_0074>.
        IF sy-subrc EQ 0.
          <fs_ztfi_0074> = ls_ztfi_0074.
        ELSE.
          APPEND ls_ztfi_0074 TO me->lt_ztfi_0074.
        ENDIF.
      ENDIF.
    ENDIF.

*  IF v_error_in_data = c_check.
*    CALL METHOD er_data_changed->display_protocol.
*  ENDIF.
  ENDMETHOD.


  METHOD handle_data_changed_finished.
    DATA ls_good_cells TYPE lvc_s_modi.

*  LOOP AT et_good_cells INTO ls_good_cells.
*  ENDLOOP.

**//.. ALV ha sido Modificado
    IF e_modified EQ abap_true.
      MOVE e_modified TO gv_modif.
    ENDIF.
  ENDMETHOD.


  METHOD handle_double_click.
  ENDMETHOD.


  METHOD handle_hotspot_click.
    DATA: lr_docnum TYPE RANGE OF edi_docnum,
          ls_docnum LIKE LINE OF lr_docnum,
          lr_cretim TYPE RANGE OF edi_ccrtim,
          ls_cretim LIKE LINE OF lr_cretim,
          lr_updtim TYPE RANGE OF edi_updtim,
          ls_updtim LIKE LINE OF lr_updtim,
          lr_credat TYPE RANGE OF sydatum,
          ls_credat LIKE LINE OF lr_credat.

    FIELD-SYMBOLS: <fs_datos> TYPE zefi_0026.
**//..
    CASE e_column_id-fieldname.
      WHEN 'DOCNUM'. " IDoc
        "
        READ TABLE me->datos ASSIGNING <fs_datos> INDEX e_row_id-index.
        IF sy-subrc EQ 0.
          ls_docnum-sign   = 'I'.
          ls_docnum-option = 'EQ'.
          ls_docnum-low    = <fs_datos>-docnum.
          APPEND ls_docnum TO lr_docnum.

          ls_cretim-sign   = 'I'.
          ls_cretim-option = 'EQ'.
          ls_cretim-low    = '000000'.
          ls_cretim-high   = '235900'.
          APPEND ls_cretim TO lr_cretim.

          ls_credat-sign   = 'I'.
          ls_credat-option = 'BT'.
          ls_credat-low    = '18000101'.
          ls_credat-high   = '99991231'.
          APPEND ls_credat TO lr_credat.

          lr_updtim[] = lr_cretim[].

          SUBMIT rseidoc2
                 WITH cretim IN lr_cretim
                 WITH docnum IN lr_docnum
                 WITH credat IN lr_credat
                 WITH updtim IN lr_updtim AND RETURN.
        ENDIF.
      WHEN 'EBELN'. " Pedido MM
        READ TABLE me->datos ASSIGNING <fs_datos> INDEX e_row_id-index.
        IF sy-subrc EQ 0.
          IF <fs_datos>-ebeln IS NOT INITIAL.
            " Ver pedido
            CALL FUNCTION 'ME_DISPLAY_PURCHASE_DOCUMENT'
              EXPORTING
                i_ebeln              = <fs_datos>-ebeln
                i_enjoy              = 'X'
              EXCEPTIONS
                not_found            = 1
                no_authority         = 2
                invalid_call         = 3
                preview_not_possible = 4
                OTHERS               = 5.
            IF sy-subrc NE 0.
              MESSAGE ID sy-msgid TYPE   'S'
                                  NUMBER sy-msgno
                                  WITH   sy-msgv1
                                         sy-msgv2
                                         sy-msgv3
                                         sy-msgv4
                                  DISPLAY LIKE sy-msgty.
            ENDIF.
          ENDIF.
        ENDIF.
      WHEN 'ZDOCFI'. " Doc FI
        READ TABLE me->datos ASSIGNING <fs_datos> INDEX e_row_id-index.
        IF sy-subrc EQ 0.
          IF <fs_datos>-zdocfi IS NOT INITIAL.
            " Ver Documento
            SET PARAMETER ID: 'BLN' FIELD <fs_datos>-zdocfi,
                              'BUK' FIELD <fs_datos>-bukrs,
                              'GJR' FIELD <fs_datos>-zanodoc.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
        ENDIF.

      WHEN 'BELNR'.  " Doc FI ??
        READ TABLE me->datos ASSIGNING <fs_datos> INDEX e_row_id-index.
        IF sy-subrc EQ 0.
          IF <fs_datos>-belnr IS NOT INITIAL.
            IF <fs_datos>-zdte_tipo EQ '2' OR
               <fs_datos>-zdte_tipo EQ '5'.
              " Ver Documento FI
              SET PARAMETER ID: 'BLN' FIELD <fs_datos>-belnr,
                                'BUK' FIELD <fs_datos>-bukrs,
                                'GJR' FIELD <fs_datos>-gjahr.
              CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
            ELSE.
              " Ver Documento MM
              SET PARAMETER ID: 'RBN' FIELD <fs_datos>-belnr,
                                'GJR' FIELD <fs_datos>-gjahr.
              CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
      WHEN 'CORRELATIVO'.
        READ TABLE me->datos ASSIGNING <fs_datos> INDEX e_row_id-index.
        IF sy-subrc EQ 0.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD handle_toolbar.
    DATA: ls_toolbar TYPE stb_button.

    DATA: wa_boton LIKE  LINE OF me->conf_boton.

    DATA: lt_botones TYPE slis_t_extab,
          wa_botones LIKE LINE OF lt_botones.

**//.. Separador
    CLEAR ls_toolbar.
    MOVE 3            TO ls_toolbar-butn_type.
    APPEND ls_toolbar TO e_object->mt_toolbar.

**//.. Botones
    LOOP AT  me->conf_boton[] INTO wa_boton.
      IF wa_boton-visible = '0'.
        CLEAR ls_toolbar.
        MOVE 3            TO ls_toolbar-butn_type.
        APPEND ls_toolbar TO e_object->mt_toolbar.
        CLEAR ls_toolbar.
        CASE wa_boton-boton.

          WHEN '1'.
            CLEAR ls_toolbar.
            MOVE 0               TO ls_toolbar-butn_type.
            MOVE icon_refresh    TO ls_toolbar-icon.
            MOVE '&ZREFR'        TO ls_toolbar-function.
            MOVE 'Refrescar'     TO ls_toolbar-quickinfo.
            MOVE abap_true       TO ls_toolbar-disabled.
            APPEND ls_toolbar TO e_object->mt_toolbar.
          WHEN '2'.
            MOVE 0               TO ls_toolbar-butn_type.
            MOVE icon_history  TO ls_toolbar-icon.
            MOVE '&LOG'          TO ls_toolbar-function.
            MOVE abap_true              TO ls_toolbar-disabled.
            MOVE 'Log de Errores'     TO ls_toolbar-quickinfo.
            APPEND ls_toolbar TO e_object->mt_toolbar.
          WHEN '3'.
            MOVE '0'               TO ls_toolbar-butn_type.
            MOVE 'ICON_TOGGLE_DISPLAY_CHANGE' TO ls_toolbar-icon.
            MOVE '&MOD'          TO ls_toolbar-function.
            MOVE abap_true       TO ls_toolbar-disabled.
            MOVE 'Modificar/Visualizar' TO ls_toolbar-text.
            APPEND ls_toolbar TO e_object->mt_toolbar.
          WHEN '4'.
            MOVE 0               TO ls_toolbar-butn_type.
            MOVE icon_pdf        TO ls_toolbar-icon.
            MOVE '&PDF'          TO ls_toolbar-function.
            MOVE 'Ver PDF Doc.Elec'       TO ls_toolbar-quickinfo.
            MOVE abap_true       TO ls_toolbar-disabled.
            APPEND ls_toolbar TO e_object->mt_toolbar.
          WHEN '5'.
            MOVE 0               TO ls_toolbar-butn_type.
            MOVE icon_okay       TO ls_toolbar-icon.
            MOVE '&ACEP'         TO ls_toolbar-function.
            MOVE 'Aceptar Doc. en SII'       TO ls_toolbar-quickinfo.
            MOVE abap_true       TO ls_toolbar-disabled.
            APPEND ls_toolbar TO e_object->mt_toolbar.
          WHEN '6'.
            MOVE 0               TO ls_toolbar-butn_type.
            MOVE icon_cancel     TO ls_toolbar-icon.
            MOVE '&REC'          TO ls_toolbar-function.
            MOVE 'Rechazar Doc. en SII'       TO ls_toolbar-quickinfo.
            MOVE abap_true       TO ls_toolbar-disabled.
            APPEND ls_toolbar TO e_object->mt_toolbar.
          WHEN '7'.
            MOVE 0                TO ls_toolbar-butn_type.
            MOVE icon_annotation  TO ls_toolbar-icon.
            MOVE '&GLOSA'         TO ls_toolbar-function.
            MOVE 'Glosa Para rechazo'       TO ls_toolbar-quickinfo.
            MOVE abap_true        TO ls_toolbar-disabled.
            APPEND ls_toolbar TO e_object->mt_toolbar.
          WHEN '8'.
            MOVE 0               TO ls_toolbar-butn_type.
            MOVE icon_allow     TO ls_toolbar-icon.
            MOVE '&LIB'         TO ls_toolbar-function.
            MOVE 'Aprobación de Gto'       TO ls_toolbar-quickinfo.
            MOVE abap_true              TO ls_toolbar-disabled.
            APPEND ls_toolbar TO e_object->mt_toolbar.
          WHEN '9'.
            MOVE 0               TO ls_toolbar-butn_type.
            MOVE icon_detail     TO ls_toolbar-icon.
            MOVE '&DETALLE'      TO ls_toolbar-function.
            MOVE 'Detalle de Factura'       TO ls_toolbar-quickinfo.
            MOVE abap_true       TO ls_toolbar-disabled.
            APPEND ls_toolbar TO e_object->mt_toolbar.
          WHEN '10'.
            MOVE 0                 TO ls_toolbar-butn_type.
            MOVE icon_planning_in  TO ls_toolbar-icon.
            MOVE '&LIBMAS'         TO ls_toolbar-function.
            MOVE 'Lib. Masiva de MM'       TO ls_toolbar-quickinfo.
            MOVE abap_true         TO ls_toolbar-disabled.
            APPEND ls_toolbar TO e_object->mt_toolbar.
          WHEN '11'.
            MOVE 0                TO ls_toolbar-butn_type.
            MOVE icon_protocol    TO ls_toolbar-icon.
            MOVE '&MODIF'         TO ls_toolbar-function.
            MOVE 'Log de Modificaciones'       TO ls_toolbar-quickinfo.
            MOVE abap_true        TO ls_toolbar-disabled.
            APPEND ls_toolbar TO e_object->mt_toolbar.
          WHEN '12'.
            MOVE 0               TO ls_toolbar-butn_type.
            MOVE icon_its        TO ls_toolbar-icon.
            MOVE '&EVE'          TO ls_toolbar-function.
            MOVE 'Listado de Eventos en SII'       TO ls_toolbar-quickinfo.
            MOVE abap_true       TO ls_toolbar-disabled.
            APPEND ls_toolbar TO e_object->mt_toolbar.
          WHEN '13'.
            MOVE 0               TO ls_toolbar-butn_type.
            MOVE icon_hr_manager TO ls_toolbar-icon.
            MOVE '&UPDAPRO'      TO ls_toolbar-function.
            MOVE 'Act. Aprobador' TO ls_toolbar-quickinfo.
            MOVE 'Act. Aprob.'    TO ls_toolbar-text.
            MOVE abap_true       TO ls_toolbar-disabled.
            APPEND ls_toolbar    TO e_object->mt_toolbar.
          WHEN '14'.
            MOVE 0               TO ls_toolbar-butn_type.
            MOVE icon_agent      TO ls_toolbar-icon.
            MOVE '&UPDANLI'      TO ls_toolbar-function.
            MOVE 'Act. Analista' TO ls_toolbar-quickinfo.
            MOVE 'Act. Analis'   TO ls_toolbar-text.
            MOVE abap_true       TO ls_toolbar-disabled.
            APPEND ls_toolbar    TO e_object->mt_toolbar.
          WHEN '15'.
            MOVE 0               TO ls_toolbar-butn_type.
            MOVE icon_submit     TO ls_toolbar-icon.
            MOVE '&REPIDOC'      TO ls_toolbar-function.
            MOVE 'ReprocIdoc'    TO ls_toolbar-quickinfo.
            MOVE abap_true       TO ls_toolbar-disabled.
            APPEND ls_toolbar    TO e_object->mt_toolbar.
          WHEN '16'.
            MOVE 0               TO ls_toolbar-butn_type.
            MOVE icon_date     TO ls_toolbar-icon.
            MOVE '&FECHAC'       TO ls_toolbar-function.
            MOVE 'FechaContable'    TO ls_toolbar-quickinfo.
            MOVE abap_true       TO ls_toolbar-disabled.
            APPEND ls_toolbar    TO e_object->mt_toolbar.
          WHEN '17'.
            MOVE 0               TO ls_toolbar-butn_type.
            MOVE icon_locked     TO ls_toolbar-icon.
            MOVE '&CIERRE'       TO ls_toolbar-function.
            MOVE 'Cierre Doc.'   TO ls_toolbar-quickinfo.
            MOVE abap_true       TO ls_toolbar-disabled.
            APPEND ls_toolbar    TO e_object->mt_toolbar.
        ENDCASE.
      ELSE.
        CLEAR ls_toolbar.
        MOVE 3            TO ls_toolbar-butn_type.
        APPEND ls_toolbar TO e_object->mt_toolbar.
        CLEAR ls_toolbar.

        CASE wa_boton-boton.
          WHEN '1'.
            CLEAR ls_toolbar.
            MOVE 0               TO ls_toolbar-butn_type.
            MOVE icon_refresh    TO ls_toolbar-icon.
            MOVE '&ZREFR'        TO ls_toolbar-function.
            MOVE 'Refrescar'     TO ls_toolbar-quickinfo.
            MOVE abap_false      TO ls_toolbar-disabled.
            APPEND ls_toolbar TO e_object->mt_toolbar.
          WHEN '2'.
            MOVE 0                TO ls_toolbar-butn_type.
            MOVE icon_history     TO ls_toolbar-icon.
            MOVE '&LOG'           TO ls_toolbar-function.
            MOVE 'Log de Errores' TO ls_toolbar-quickinfo.
            MOVE abap_false       TO ls_toolbar-disabled.
            APPEND ls_toolbar TO e_object->mt_toolbar.
          WHEN '3'.
            MOVE 0                             TO ls_toolbar-butn_type.
            MOVE icon_toggle_display_change    TO ls_toolbar-icon.
            MOVE '&MOD'                        TO ls_toolbar-function.
            MOVE abap_false                    TO ls_toolbar-disabled.
            MOVE 'Modificar/Visualizar'        TO ls_toolbar-text.
            APPEND ls_toolbar TO e_object->mt_toolbar.
          WHEN '4'.
            MOVE 0                  TO ls_toolbar-butn_type.
            MOVE 'Ver PDF Doc.Elec' TO ls_toolbar-quickinfo.
            MOVE icon_pdf           TO ls_toolbar-icon.
            MOVE '&PDF'             TO ls_toolbar-function.
            MOVE abap_false         TO ls_toolbar-disabled.
            APPEND ls_toolbar TO e_object->mt_toolbar.
          WHEN '5'.
            MOVE 0                           TO ls_toolbar-butn_type.
            MOVE 'Aceptar Doc. en SII'       TO ls_toolbar-quickinfo.
            MOVE icon_okay                   TO ls_toolbar-icon.
            MOVE '&ACEP'                     TO ls_toolbar-function.
            AUTHORITY-CHECK OBJECT 'ZFI03'
           ID 'BUKRS' FIELD me->bukrs
           ID 'ACTVT' FIELD '37'.
            IF sy-subrc  NE 0.
              MOVE abap_true            TO ls_toolbar-disabled.
            ELSE.
              MOVE abap_false             TO ls_toolbar-disabled.
            ENDIF.

            APPEND ls_toolbar TO e_object->mt_toolbar.
          WHEN '6'.
            MOVE 0                       TO ls_toolbar-butn_type.
            MOVE 'Rechazar Doc. en SII'  TO ls_toolbar-quickinfo.
            MOVE icon_cancel             TO ls_toolbar-icon.
            MOVE '&REC'                  TO ls_toolbar-function.
            AUTHORITY-CHECK OBJECT 'ZFI03'
          ID 'BUKRS' FIELD me->bukrs
          ID 'ACTVT' FIELD '96'.
            IF sy-subrc  NE 0.
              MOVE abap_true             TO ls_toolbar-disabled.
            ELSE.
              MOVE abap_false             TO ls_toolbar-disabled.
            ENDIF.

            APPEND ls_toolbar TO e_object->mt_toolbar.
          WHEN '7'.
            MOVE 0                    TO ls_toolbar-butn_type.
            MOVE 'Glosa Para rechazo' TO ls_toolbar-quickinfo.
            MOVE icon_annotation      TO ls_toolbar-icon.
            MOVE '&GLOSA'             TO ls_toolbar-function.
            MOVE abap_false           TO ls_toolbar-disabled.
            APPEND ls_toolbar TO e_object->mt_toolbar.
          WHEN '8'.
            MOVE 0                         TO ls_toolbar-butn_type.
            MOVE 'Aprobación de Gto'       TO ls_toolbar-quickinfo.
            MOVE icon_allow                TO ls_toolbar-icon.
            MOVE '&LIB'                    TO ls_toolbar-function.
            AUTHORITY-CHECK OBJECT 'ZFI03'
         ID 'BUKRS' FIELD me->bukrs
         ID 'ACTVT' FIELD '43'.
            IF sy-subrc  NE 0.
              MOVE abap_true             TO ls_toolbar-disabled.
            ELSE.
              MOVE abap_false             TO ls_toolbar-disabled.
            ENDIF.
            APPEND ls_toolbar TO e_object->mt_toolbar.
          WHEN '9'.
            MOVE 0                         TO ls_toolbar-butn_type.
            MOVE 'Detalle de Factura'      TO ls_toolbar-quickinfo.
            MOVE icon_detail               TO ls_toolbar-icon.
            MOVE '&DETALLE'                TO ls_toolbar-function.
            MOVE abap_false                TO ls_toolbar-disabled.
            APPEND ls_toolbar TO e_object->mt_toolbar.
          WHEN '10'.
            MOVE 0                        TO ls_toolbar-butn_type.
            MOVE 'Lib. Masiva de MM'      TO ls_toolbar-quickinfo.
            MOVE icon_planning_in         TO ls_toolbar-icon.
            MOVE '&LIBMAS'                TO ls_toolbar-function.
            AUTHORITY-CHECK OBJECT 'ZFI01'
       ID 'BUKRS' FIELD me->bukrs
       ID 'ACTVT' FIELD '02'.
            IF sy-subrc  NE 0.
              MOVE abap_true             TO ls_toolbar-disabled.
            ELSE.
              MOVE abap_false             TO ls_toolbar-disabled.
            ENDIF.
            APPEND ls_toolbar TO e_object->mt_toolbar.
          WHEN '11'.
            MOVE 0                        TO ls_toolbar-butn_type.
            MOVE 'Log de Modificaciones'  TO ls_toolbar-quickinfo.
            MOVE icon_protocol            TO ls_toolbar-icon.
            MOVE '&MODIF'                 TO ls_toolbar-function.
            MOVE abap_false               TO ls_toolbar-disabled.
            APPEND ls_toolbar TO e_object->mt_toolbar.
          WHEN '12'.
            MOVE 0                           TO ls_toolbar-butn_type.
            MOVE 'Listado de Eventos en SII' TO ls_toolbar-quickinfo.
            MOVE icon_its                    TO ls_toolbar-icon.
            MOVE '&EVE'                      TO ls_toolbar-function.
            MOVE abap_false                  TO ls_toolbar-disabled.
            APPEND ls_toolbar TO e_object->mt_toolbar.
          WHEN '13'.
            MOVE 0               TO ls_toolbar-butn_type.
            MOVE icon_hr_manager TO ls_toolbar-icon.
            MOVE '&UPDAPRO'      TO ls_toolbar-function.
            MOVE 'Act. Aprobador' TO ls_toolbar-quickinfo.
            MOVE 'Act. Aprob.'    TO ls_toolbar-text.
            AUTHORITY-CHECK OBJECT 'ZFI01'
       ID 'BUKRS' FIELD me->bukrs
       ID 'ACTVT' FIELD '02'.
            IF sy-subrc  NE 0.
              MOVE abap_true             TO ls_toolbar-disabled.
            ELSE.
              MOVE abap_false             TO ls_toolbar-disabled.
            ENDIF.
            APPEND ls_toolbar    TO e_object->mt_toolbar.
          WHEN '14'.
            MOVE 0               TO ls_toolbar-butn_type.
            MOVE icon_agent      TO ls_toolbar-icon.
            MOVE '&UPDANLI'      TO ls_toolbar-function.
            MOVE 'Act. Analista' TO ls_toolbar-quickinfo.
            MOVE 'Act. Analis'   TO ls_toolbar-text.
            AUTHORITY-CHECK OBJECT 'ZFI01'
       ID 'BUKRS' FIELD me->bukrs
       ID 'ACTVT' FIELD '02'.
            IF sy-subrc  NE 0.
              MOVE abap_true             TO ls_toolbar-disabled.
            ELSE.
              MOVE abap_false             TO ls_toolbar-disabled.
            ENDIF.
            APPEND ls_toolbar    TO e_object->mt_toolbar.
          WHEN '15'.
            MOVE 0               TO ls_toolbar-butn_type.
            MOVE icon_submit     TO ls_toolbar-icon.
            MOVE '&REPIDOC'      TO ls_toolbar-function.
            MOVE 'ReprocIdoc'    TO ls_toolbar-quickinfo.
            AUTHORITY-CHECK OBJECT 'ZFI03'
        ID 'BUKRS' FIELD me->bukrs
        ID 'ACTVT' FIELD '25'.
            IF sy-subrc  NE 0.
              MOVE abap_true             TO ls_toolbar-disabled.
            ELSE.
              MOVE abap_false             TO ls_toolbar-disabled.
            ENDIF.
            APPEND ls_toolbar    TO e_object->mt_toolbar.

          WHEN '16'.
            MOVE 0               TO ls_toolbar-butn_type.
            MOVE icon_date       TO ls_toolbar-icon.
            MOVE '&FECHAC'       TO ls_toolbar-function.
            MOVE 'FechaContable' TO ls_toolbar-quickinfo.
            AUTHORITY-CHECK OBJECT 'ZFI03'
        ID 'BUKRS' FIELD me->bukrs
        ID 'ACTVT' FIELD '16'.
            IF sy-subrc  NE 0.
              MOVE abap_true             TO ls_toolbar-disabled.
            ELSE.
              MOVE abap_false             TO ls_toolbar-disabled.
            ENDIF.
            APPEND ls_toolbar    TO e_object->mt_toolbar.
          WHEN '17'.
            MOVE 0               TO ls_toolbar-butn_type.
            MOVE icon_locked      TO ls_toolbar-icon.
            MOVE '&CIERRE'      TO ls_toolbar-function.
            MOVE 'Cierre Doc.' TO ls_toolbar-quickinfo.
            AUTHORITY-CHECK OBJECT 'ZFI01'
       ID 'BUKRS' FIELD me->bukrs
       ID 'ACTVT' FIELD '02'.
            IF sy-subrc  NE 0.
              MOVE abap_true             TO ls_toolbar-disabled.
            ELSE.
              MOVE abap_false             TO ls_toolbar-disabled.
            ENDIF.
            APPEND ls_toolbar    TO e_object->mt_toolbar.
        ENDCASE.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD handle_user_command.
    DATA: lc_acep VALUE '5',
          lc_rec  VALUE '6',
          lc_cie  VALUE 'C'.
    DATA: lt_selected_rows     TYPE lvc_t_roid,
          wa_selected_row      TYPE lvc_s_roid,
          wa_salida            TYPE zefi_0026,
          ln_salida_or         TYPE zefi_0026,
          lv_salida_original   TYPE zefi_0026,
          vl_mens              TYPE char100,
          s_pdfwb              TYPE ztfi_0078-visupdf,
          s_alias              TYPE ztfi_0078-direcpdf,
          gv_pdf               TYPE xstring,
          lt_salida_temp       TYPE STANDARD TABLE OF zefi_0026,
          ti_ztfi_0074_bd      TYPE STANDARD TABLE OF ztfi_0074,
          ls_ztfi_0074_bd      TYPE ztfi_0074,
          ls_tab               TYPE zdte_fechasii,
          lv_registro          TYPE lvc_s_roid-row_id,
*          ti_mod_save          TYPE STANDARD TABLE OF ty_mod WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE,
          lv_mensaje           TYPE char200,
          lv_documento_mensaje TYPE char200,
          lv_text              TYPE trm080-text,
          lv_refr              TYPE abap_bool, " refrescar ALV??
          lv_flag              TYPE c,
          ln_status            TYPE ztfi_0079,
          lv_respuesta         TYPE c.

    CLEAR: lv_text.
**//.. Obtener las filas seleccionadas
    CALL METHOD gr_alvgrid->get_selected_rows
      IMPORTING
        et_row_no = lt_selected_rows.

**//.. Verificar si se seleccionó al menos un registro
    IF lines( lt_selected_rows ) EQ 0 AND e_ucomm NE '&ZREFR'.
      MESSAGE ID '3L' TYPE 'S' NUMBER 169 .
    ELSE.
      lv_refr = abap_true.

**//.. Boton
      CASE e_ucomm.

        WHEN '&PDF'.
          CLEAR: lv_refr.
          lv_respuesta  = 0.
*// Pregunta Unica para mostrar PDF
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              text_question = TEXT-400
              text_button_1 = 'Si'
              text_button_2 = 'No'
            IMPORTING
              answer        = lv_respuesta.


          CHECK lv_respuesta = '1'.
          LOOP AT lt_selected_rows INTO wa_selected_row.
*          READ TABLE lt_selected_rows INTO wa_selected_row INDEX 1.
            READ TABLE me->datos INTO wa_salida INDEX wa_selected_row-row_id.
            IF sy-subrc = 0.
              CALL METHOD funcionpdf( EXPORTING wa_salida = wa_salida ).
            ENDIF.
          ENDLOOP.

        WHEN '&ZREFR'.
          me->buscar_datos( ).

        WHEN '&EVE'.
          CLEAR: lv_refr.
          READ TABLE lt_selected_rows INTO wa_selected_row INDEX 1.
          READ TABLE me->datos INTO wa_salida INDEX wa_selected_row-row_id.
          ls_tab-folio = wa_salida-xblnr.
          ls_tab-tipodoc = wa_salida-tipodte.
          SPLIT wa_salida-rutemisor AT '-' INTO ls_tab-rutemisor ls_tab-dvemisor.
          CALL METHOD me->ver_eventos
            EXPORTING
              sociedad = wa_salida-bukrs
              itab     = ls_tab.

        WHEN '&LOG'.
          CLEAR: lv_refr.
          READ TABLE lt_selected_rows INTO wa_selected_row INDEX 1.
          READ TABLE me->datos INTO wa_salida INDEX wa_selected_row-row_id.
          CALL METHOD me->mostrar_log
            EXPORTING
              belnr   = wa_salida-belnr
              ebeln   = wa_salida-ebeln
              gjahr   = wa_salida-gjahr
              lifnr   = wa_salida-lifnr
              xblnr   = wa_salida-xblnr
              bukrs   = wa_salida-bukrs
              bldat   = wa_salida-bldat
              tipodte = wa_salida-tipodte.

        WHEN '&MOD'.
          READ TABLE lt_selected_rows INTO wa_selected_row INDEX 1.
          READ TABLE me->datos INTO wa_salida INDEX wa_selected_row-row_id.
          IF sy-subrc = 0.
            DATA: lv_visualizar TYPE c.
            lv_visualizar = '1'.
            IF  wa_salida-status = '4' OR wa_salida-status = '5'
             OR wa_salida-status ='6' .
              lv_visualizar = '2'.
            ENDIF.
            CALL METHOD me->generar_cambios
              EXPORTING
                belnr      = wa_salida-belnr
                gjahr      = wa_salida-gjahr
                tcode      = wa_salida-tcode
                bukrs      = wa_salida-bukrs
                visualizar = lv_visualizar.
          ENDIF.

        WHEN '&ACEP'.
          LOOP AT  lt_selected_rows INTO wa_selected_row.
            READ TABLE me->datos INTO wa_salida INDEX wa_selected_row-row_id.
            APPEND  wa_salida TO lt_salida_temp.
          ENDLOOP.

          CALL METHOD funcionacre(
            IMPORTING
              lt_salida_temp   = lt_salida_temp
              lv_status        = lc_acep
              lt_selected_rows = lt_selected_rows ).
          CLEAR me->gv_glosa. "Se borra por cada evento de rechazo/aceptacion

        WHEN '&REC'.
          LOOP AT  lt_selected_rows INTO wa_selected_row.
            READ TABLE me->datos INTO wa_salida INDEX wa_selected_row-row_id.
            APPEND  wa_salida TO lt_salida_temp.
          ENDLOOP.

          CALL METHOD funcionacre(
            IMPORTING
              lt_salida_temp   = lt_salida_temp
              lv_status        = lc_rec
              lt_selected_rows = lt_selected_rows ).
          CLEAR me->gv_glosa. "Se borra por cada evento de rechazo/aceptacion

        WHEN '&LIB'.
          "---->llamado a WF de aprobación Z
          LOOP AT  lt_selected_rows INTO wa_selected_row.
            READ TABLE me->datos INTO wa_salida INDEX wa_selected_row-row_id.
            APPEND  wa_salida TO lt_salida_temp.
          ENDLOOP.
          CALL METHOD funcionwf(
            IMPORTING
              lt_salida_temp   = lt_salida_temp
              lt_selected_rows = lt_selected_rows ).

        WHEN '&DETALLE'.
          CLEAR: lv_refr.
          LOOP AT lt_selected_rows INTO wa_selected_row.
            READ TABLE me->datos INTO wa_salida INDEX wa_selected_row-row_id.
            lv_registro = wa_selected_row-row_id.
            CALL METHOD me->ver_detalle
              EXPORTING
                sociedad  = wa_salida-bukrs
                documento = wa_salida-xblnr
                proveedor = wa_salida-lifnr
                tipodte   = wa_salida-tipodte.
          ENDLOOP.

        WHEN '&LIBMAS'.
          CALL METHOD me->liberacion_masiva_mm
            EXPORTING
              t_selected_rows = lt_selected_rows.

        WHEN '&MODIF'.
          CLEAR: lv_refr.
          LOOP AT lt_selected_rows INTO wa_selected_row.
            READ TABLE me->datos INTO wa_salida INDEX wa_selected_row-row_id.
            lv_registro = wa_selected_row-row_id.
            CALL METHOD me->documento_modificacion
              EXPORTING
                sociedad  = wa_salida-bukrs
                documento = wa_salida-xblnr
                proveedor = wa_salida-lifnr
                tipodte   = wa_salida-tipodte
                objekt    = 'ZTFI_0074'.
          ENDLOOP.

        WHEN '&GLOSA'.
          CLEAR: lv_refr,
                 lv_text.
          lv_text = gv_glosa.

          CLEAR: gv_glosa.

          CALL FUNCTION 'ADA_POPUP_TEXT_INPUT'
            EXPORTING
              sourcetext   = TEXT-403     "Ingrese Glosa de Rechazo
              titel        = TEXT-402     "'Monitor DTE
              start_column = 25
              start_row    = 6
            CHANGING
              targettext   = lv_text.
          IF lv_text IS NOT INITIAL.
            gv_glosa = lv_text.
          ENDIF.
        WHEN '&UPDAPRO'. " Actualizar Aprobador
          CLEAR: lv_refr.

          CALL METHOD me->set_fieldname
            EXPORTING
              i_fieldname       = 'APROBADOR'
              i_selected_rows_t = lt_selected_rows.

        WHEN '&UPDANLI'. " Actualizar Analista
          CLEAR: lv_refr.

          CALL METHOD me->set_fieldname
            EXPORTING
              i_fieldname       = 'NOMBANA'
              i_selected_rows_t = lt_selected_rows.

        WHEN '&REPIDOC'. "Reproceso de IDOC
          "---> reprocesar Idoc q estan en error
          LOOP AT  lt_selected_rows INTO wa_selected_row.
            READ TABLE me->datos INTO wa_salida INDEX wa_selected_row-row_id.
            APPEND  wa_salida TO lt_salida_temp.
          ENDLOOP.
          CALL METHOD me->reproc_idoc(
            IMPORTING
              lt_salida_temp = lt_salida_temp
              lt_select_rows = lt_selected_rows ).
          me->buscar_datos( ).

        WHEN '&FECHAC'. "Mod. Fecha Contable
          "---> Modificar fecha contable
          LOOP AT  lt_selected_rows INTO wa_selected_row.
            READ TABLE me->datos INTO wa_salida INDEX wa_selected_row-row_id.
            APPEND  wa_salida TO lt_salida_temp.
          ENDLOOP.
          CALL METHOD me->mod_fecha(
            EXPORTING
              lt_salida_temp   = lt_salida_temp
              lt_selected_rows = lt_selected_rows ).
          me->buscar_datos( ).

        WHEN '&CIERRE'. "Cierre de documentos
          LOOP AT  lt_selected_rows INTO wa_selected_row.
            READ TABLE me->datos INTO wa_salida INDEX wa_selected_row-row_id.
            APPEND  wa_salida TO lt_salida_temp.
          ENDLOOP.

          CALL METHOD funcioncierre(
            IMPORTING
              lt_salida_temp   = lt_salida_temp
              lv_status        = lc_cie
              lt_selected_rows = lt_selected_rows ).
          CLEAR me->gv_glosa. "Se borra por cada evento de cierre
          me->buscar_datos( ).
      ENDCASE.

**      me->buscar_datos( ).

      IF lv_refr EQ abap_true.
        CALL METHOD gr_alvgrid->refresh_table_display
          EXCEPTIONS
            finished = 1
            OTHERS   = 2.
        IF sy-subrc NE 0.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD lanzar_wf.
    DATA: lv_respuesta TYPE c,
          lv_dte_tipo  TYPE lfb1-zzdte_tipo,
          lv_status    TYPE ztfi_0074-status.
    DATA: old_ztfi_0074    TYPE STANDARD TABLE OF yztfi_0074  WITH NON-UNIQUE DEFAULT KEY,
          ln_old_ztfi_0074 LIKE LINE OF old_ztfi_0074.
    DATA: new_ztfi_0074    TYPE STANDARD TABLE OF yztfi_0074 WITH NON-UNIQUE DEFAULT KEY,
          ln_new_ztfi_0074 LIKE LINE OF new_ztfi_0074.
    DATA: wa_objectid  TYPE cdhdr-objectid.
    DATA: lv_key TYPE sweinstcou-objkey.
    DATA: lt_container TYPE STANDARD TABLE OF swcont WITH NON-UNIQUE DEFAULT KEY.
    DATA: lv_responsable TYPE char14.

    CLEAR lv_respuesta.
    CLEAR mod.


    FREE old_ztfi_0074.
    CLEAR old_ztfi_0074.

    FREE new_ztfi_0074.
    CLEAR new_ztfi_0074.



    wa_objectid(3) = sy-mandt.
    wa_objectid+3(4) = sociedad.
    wa_objectid+7(16) = factura.
    wa_objectid+23(10) = proveedor.
    wa_objectid+33(3) = tipodte.



    "---> Valido si es el mismo usuario aprobador solo pregunto y cambio estatus
    IF responsable = sy-uname.

      lv_status = '3'. "para todos los Tipos de proveedores se mueve a estatus 3.
      status = lv_status.
*      CALL FUNCTION 'POPUP_TO_CONFIRM'
*        EXPORTING
*          text_question = '¿Desea liberar el documento?'
*          text_button_1 = 'Si'
*          text_button_2 = 'No'
*        IMPORTING
*          answer        = lv_respuesta.
*      CHECK lv_respuesta = '1'.
      "---->grabación en log modificaciones.

      SELECT * FROM ztfi_0074 INTO TABLE old_ztfi_0074
                WHERE bukrs EQ sociedad
                  AND xblnr EQ factura
                  AND lifnr EQ proveedor
                  AND tipodte EQ tipodte.
      IF sy-dbcnt >= 1.
        READ TABLE old_ztfi_0074 INTO ln_old_ztfi_0074 INDEX 1.
        IF sy-subrc = 0.
          registro-status    = lv_status.
          registro-aprobador = responsable.
          registro-aprobador_real = responsable.
          APPEND registro TO new_ztfi_0074.

          CALL FUNCTION 'ZTFI_0074_WRITE_DOCUMENT'
            IN UPDATE TASK
            EXPORTING
              objectid      = wa_objectid
              tcode         = 'ZDTE_003'
              utime         = sy-uzeit
              udate         = sy-datum
              username      = sy-uname
              upd_ztfi_0074 = 'U'
            TABLES
              xztfi_0074    = new_ztfi_0074
              yztfi_0074    = old_ztfi_0074.

          "--->actualización en tabla Z
          UPDATE ztfi_0074 SET status         = lv_status
                               aprobador_real = responsable
                               fechaaprob = sy-datum
                               horaaprob = sy-uzeit
                           WHERE bukrs   EQ sociedad
                             AND xblnr   EQ factura
                             AND lifnr   EQ proveedor
                             AND tipodte EQ tipodte.
          IF sy-subrc = 0.
            COMMIT WORK AND WAIT.
          ENDIF.
          mod = 'X'.
        ENDIF.
      ENDIF.

      EXIT.
    ENDIF.

    "---->fin.


    SELECT SINGLE zzdte_tipo INTO lv_dte_tipo
      FROM lfb1
      WHERE lifnr EQ proveedor
      AND bukrs EQ sociedad.

    CASE lv_dte_tipo.
      WHEN '2'.
        lv_status = 'P'.
      WHEN '5'.
        lv_status = 'P'.
      WHEN OTHERS.
*  por defecto sera 3.
        lv_status = '2'.
    ENDCASE.
    status = lv_status.

*    CALL FUNCTION 'POPUP_TO_CONFIRM'
*      EXPORTING
*        text_question = '¿Desea enviar WF a responsable del Gasto?'
*        text_button_1 = 'Si'
*        text_button_2 = 'No'
*      IMPORTING
*        answer        = lv_respuesta.
*    CHECK lv_respuesta = '1'.

    IF lv_status NE '2'.
*  *******************************************************
      SELECT * FROM ztfi_0074 INTO TABLE old_ztfi_0074
                    WHERE bukrs EQ sociedad
                      AND xblnr EQ factura
                      AND lifnr EQ proveedor
                      AND tipodte EQ tipodte.
      IF sy-dbcnt >= 1.
        READ TABLE old_ztfi_0074 INTO ln_old_ztfi_0074 INDEX 1.
        IF sy-subrc = 0.
          registro-status    = lv_status.
          registro-aprobador = responsable.
          APPEND registro TO new_ztfi_0074.

          CALL FUNCTION 'ZTFI_0074_WRITE_DOCUMENT'
            IN UPDATE TASK
            EXPORTING
              objectid      = wa_objectid
              tcode         = 'ZDTE_003'
              utime         = sy-uzeit
              udate         = sy-datum
              username      = sy-uname
              upd_ztfi_0074 = 'U'
            TABLES
              xztfi_0074    = new_ztfi_0074
              yztfi_0074    = old_ztfi_0074.
          "--->actualización en tabla Z
          UPDATE ztfi_0074 SET status         = lv_status
*                           aprobador_real = responsable
*                           FECHAAPROB = sy-datum
*                           HORAAPROB = sy-UZEIT
                           WHERE bukrs   EQ sociedad
                             AND xblnr   EQ factura
                             AND lifnr   EQ proveedor
                             AND tipodte EQ tipodte.
          IF sy-subrc = 0.
            COMMIT WORK AND WAIT.
          ENDIF.
          mod = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.
*  *******************************************************
    CLEAR lv_key.
    CONCATENATE sociedad   factura proveedor tipodte INTO lv_key RESPECTING BLANKS.
    CONCATENATE 'US' responsable INTO lv_responsable.
    CALL FUNCTION 'SWC_ELEMENT_SET'
      EXPORTING
        element   = 'responsable'
        field     = lv_responsable
      TABLES
        container = lt_container.
    CALL FUNCTION 'SWE_EVENT_CREATE'
      EXPORTING
        objtype           = 'ZFIJP'
        objkey            = lv_key
        event             = 'Z_EJECUTA'
      TABLES
        event_container   = lt_container
      EXCEPTIONS
        objtype_not_found = 1
        OTHERS            = 2.
    COMMIT WORK.

  ENDMETHOD.


  METHOD liberacion_masiva_mm.
    DATA: ls_selected_row TYPE lvc_s_roid,
          lt_data_tmp     TYPE TABLE OF zefi_0026,
          ls_data         TYPE zefi_0026,
          lr_belnr        TYPE RANGE OF belnr_d,
          lr_gjahr        TYPE RANGE OF gjahr,
          ls_gjahr        LIKE LINE OF lr_gjahr,
          lr_status       TYPE RANGE OF ztfi_0074-status.

    lr_status = VALUE #( ( sign = 'I' option = 'BT' low = '8' high = '8' ) ).

    LOOP AT t_selected_rows INTO ls_selected_row.
      READ TABLE me->datos INTO ls_data INDEX ls_selected_row-row_id.
      IF  sy-subrc EQ 0
      AND ls_data-status IN lr_status.
        lr_gjahr = VALUE #( BASE lr_gjahr ( sign = 'I' option = 'BT' low = ls_data-gjahr ) ).

        APPEND ls_data TO lt_data_tmp.
      ENDIF.
    ENDLOOP.

    SORT lr_gjahr.
    DELETE ADJACENT DUPLICATES FROM lr_gjahr COMPARING low.

    IF lines( lr_gjahr ) GT 0.
      LOOP AT lr_gjahr INTO ls_gjahr.
        CLEAR: lr_belnr.

        LOOP AT lt_data_tmp INTO ls_data
                            WHERE gjahr EQ ls_gjahr-low.
          lr_belnr = VALUE #(  BASE lr_belnr ( sign = 'I' option = 'EQ' low = ls_data-belnr ) ).

        ENDLOOP.

        IF lines( lr_belnr ) GT 0.
          CALL METHOD me->generar_liberacion_mm
            EXPORTING
              i_gjahr = ls_gjahr-low
              t_belnr = lr_belnr.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD liberarobj.
    DATA: co_type TYPE string.
    DATA: ls_0074 TYPE ztfi_0074.
    DATA: t_0078 TYPE STANDARD TABLE OF ztfi_0078.
    DATA: wa_0078 TYPE ztfi_0078.
    CLEAR co_type. CLEAR wa_0078. REFRESH t_0078.


    SELECT SINGLE * INTO  wa_0078 FROM ztfi_0078
       WHERE bukrs EQ me->bukrs.


    CLEAR  co_type.
    SELECT SINGLE nombre_int INTO co_type FROM ztfi_0078b
       WHERE vatint = wa_0078-vatint
       AND   codint = '0005'.

    CHECK co_type IS NOT INITIAL.

    CALL FUNCTION co_type. "'ZFI_0008FDTE'.

  ENDMETHOD.


  METHOD main.
    IF me->bukrs IS NOT INITIAL.
      me->buscar_datos( ).
      me->buscar_conf( ).
    ENDIF.
  ENDMETHOD.


  METHOD mod_fecha.
    DATA:
      lt_mod_save  TYPE STANDARD TABLE OF ty_mod,
      ls_mod_save  TYPE ty_mod,
      lv_respuesta TYPE c,
      lv_fechac    TYPE sy-datum,
      ok_code      TYPE syucomm,
      lv_vatint    TYPE ztfi_0078-vatint,
      co_type      TYPE ztfi_0078b-nombre_int,
      ls_salidag   TYPE zefi_0026,
      obj          TYPE REF TO object.
    CLEAR lv_fechac . CLEAR ok_code.


    CALL FUNCTION 'ZFI_0040FECDTE'
      IMPORTING
        answer    = ok_code
      CHANGING
        ls_fechac = lv_fechac.


    CHECK lv_fechac IS NOT INITIAL.

    lv_respuesta = 0.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question = '¿Desea modificar la Fecha Contable?'
        text_button_1 = 'Si'
        text_button_2 = 'No'
      IMPORTING
        answer        = lv_respuesta.
    CHECK lv_respuesta = '1'.
* "--->Busco Clase para fecha, Cod=0013
    READ TABLE lt_salida_temp INTO ls_salidag INDEX 1.
    SELECT SINGLE vatint INTO  lv_vatint  FROM ztfi_0078
         WHERE bukrs = ls_salidag-bukrs.

    SELECT SINGLE nombre_int INTO co_type FROM ztfi_0078b
           WHERE vatint = lv_vatint
           AND   codint = '0013'.

    TRY.
        CREATE OBJECT obj  TYPE (co_type)
          EXPORTING
            fecha_c = lv_fechac.

        IF obj IS BOUND.
          CALL METHOD obj->('MUEVE_VALOR')
            EXPORTING
              lt_entra = lt_salida_temp[].
          CALL METHOD obj->('MOD_FECHA_CIERRE')
            IMPORTING
              lt_mod = lt_mod_save.
        ENDIF.
      CATCH cx_sql_exception.
    ENDTRY.
    "log de modificaciones o bloqueos realizados.
    IF lt_mod_save[] IS NOT INITIAL.
      me->mostrar_log_proc_masi( EXPORTING  lt_mod_save = lt_mod_save[] ).
    ENDIF.
    FREE obj.
  ENDMETHOD.


  METHOD mostrar_log.

    DATA: lv_belnr TYPE rbkp-belnr,
          lv_gjahr TYPE rbkp-gjahr.

    CLEAR: lv_belnr,
           lv_gjahr.

    lv_belnr = belnr.
    lv_gjahr = gjahr.

    DATA: lt_log_mrm TYPE bal_t_msgh.

    CALL FUNCTION 'MRM_APPL_LOG_MEM_DELETE'
      EXPORTING
        i_belnr = lv_belnr
        i_gjahr = lv_gjahr.

    CALL FUNCTION 'MRM_APPL_LOG_DB_READ'
      EXPORTING
        i_belnr            = lv_belnr
        i_gjahr            = lv_gjahr
        i_not_load_msg     = space
      IMPORTING
*       TE_LOG_HANDLE      =
        te_msg_handle      = lt_log_mrm
*   CHANGING
*       X_S_LOG_FILTER     =
      EXCEPTIONS
        load_error         = 1
        log_already_loaded = 2
        OTHERS             = 3.

    IF lt_log_mrm[] IS NOT INITIAL.
      "IF belnr IS NOT INITIAL.
      "---->lee objeto estándar.
      DATA: lv_mrm_rbkpv TYPE mrm_rbkpv.
      DATA: lv_rbkp TYPE rbkp.

      SELECT SINGLE *
      FROM rbkp
      INTO lv_rbkp
      WHERE belnr = belnr AND
            gjahr = gjahr.
      IF sy-dbcnt =  1.
        CLEAR lv_mrm_rbkpv.
        DATA: lv_read TYPE c VALUE 'X'.
        MOVE-CORRESPONDING lv_rbkp TO lv_mrm_rbkpv.
        CALL FUNCTION 'MRM_PROTOCOL_DISPLAY'
          EXPORTING
            i_rbkpv  = lv_mrm_rbkpv
          CHANGING
            x_dbread = lv_read.
      ENDIF.

    ELSE.

      "---->lee objeto "Z"

      DATA: p_number_of_logs LIKE sy-tabix.

      DATA: p_header_data_tab  TYPE STANDARD TABLE OF balhdr WITH NON-UNIQUE DEFAULT KEY,
            p_header_para_tab  TYPE STANDARD TABLE OF balhdrp WITH NON-UNIQUE DEFAULT KEY,
            p_message_tab      TYPE STANDARD TABLE OF balm WITH NON-UNIQUE DEFAULT KEY,
            p_message_para_tab TYPE STANDARD TABLE OF balmp WITH NON-UNIQUE DEFAULT KEY.

      DATA: lv_external_number TYPE balnrext.

      DATA: lv_stcd1 TYPE lfa1-stcd1.

      SELECT SINGLE stcd1
      FROM lfa1
      INTO lv_stcd1
      WHERE lifnr = lifnr.



      CONCATENATE bukrs lifnr xblnr tipodte INTO lv_external_number.

      DATA: lv_log_filter TYPE bal_s_lfil.

      DATA: r_extnumber  TYPE bal_r_extn,
            ln_extnumber LIKE LINE OF r_extnumber,
            r_object     TYPE bal_r_obj,
            ln_object    LIKE LINE OF r_object,
            r_subobject  TYPE bal_r_sub,
            ln_subobject LIKE LINE OF r_subobject.

      FREE: r_extnumber,
            r_object,
            r_subobject.

      CLEAR: ln_extnumber,
             ln_object,
             ln_subobject.

      ln_extnumber-sign = ln_object-sign = ln_subobject-sign = 'I'.
      ln_extnumber-option = ln_object-option = ln_subobject-option = 'EQ'.

      ln_extnumber-low =  lv_external_number.
      ln_object-low = 'ZDTE'.
      ln_subobject-low = 'RECEPCION'.

      APPEND ln_extnumber TO r_extnumber.
      APPEND ln_object TO r_object.
      APPEND ln_subobject TO r_subobject.

      lv_log_filter-extnumber[] = r_extnumber[].
      lv_log_filter-object[] = r_object[].
      lv_log_filter-subobject[] = r_subobject[].



      DATA: lt_log_header TYPE balhdr_t,
            ls_log_header TYPE balhdr.

      FREE lt_log_header.
      CALL FUNCTION 'BAL_DB_SEARCH'
        EXPORTING
*         I_CLIENT       = SY-MANDT
          i_s_log_filter = lv_log_filter
        IMPORTING
          e_t_log_header = lt_log_header.

      CHECK sy-subrc = 0.

      DATA: lt_log_handle        TYPE  bal_t_logh,
            lt_log_handle_e      TYPE  bal_t_logh,
            lt_msg_handle        TYPE  bal_t_msgh,
            ln_msg_handle        LIKE LINE OF lt_msg_handle,
            lt_log_handle_primer TYPE bal_t_logh,
            ln_log_handle_primer LIKE LINE OF lt_log_handle_primer.

      CALL FUNCTION 'BAL_DB_LOAD'
        EXPORTING
          i_t_log_header = lt_log_header
*         I_T_LOG_HANDLE =
*         I_T_LOGNUMBER  =
*         I_CLIENT       = SY-MANDT
*         I_DO_NOT_LOAD_MESSAGES        = ' '
*         I_EXCEPTION_IF_ALREADY_LOADED =
        IMPORTING
          e_t_log_handle = lt_log_handle_e
          e_t_msg_handle = lt_msg_handle.

      READ TABLE lt_log_handle_e INTO ln_log_handle_primer INDEX 1.
      IF sy-subrc = 0.
        APPEND ln_log_handle_primer TO lt_log_handle_primer.
      ELSE. "Allready loaded
**      READ TABLE lt_log_header INTO ls_log_header INDEX 1.
**      IF sy-subrc EQ 0.
**        APPEND ls_log_header-log_handle TO lt_log_handle_e.
**      ENDIF.
        LOOP AT lt_log_header INTO ls_log_header.
          INSERT ls_log_header-log_handle INTO TABLE lt_log_handle_e.
        ENDLOOP.
      ENDIF.

      DATA: lv_mensaje TYPE bal_s_msg.

      DATA: tab_errprot    TYPE STANDARD TABLE OF mrm_errprot,
            ln_tab_errprot LIKE LINE OF tab_errprot.

      LOOP AT lt_msg_handle INTO ln_msg_handle.

        CALL FUNCTION 'BAL_LOG_MSG_READ'
          EXPORTING
            i_s_msg_handle = ln_msg_handle
*           I_LANGU        = SY-LANGU
          IMPORTING
            e_s_msg        = lv_mensaje.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING lv_mensaje TO ln_tab_errprot.
          APPEND ln_tab_errprot TO tab_errprot.
        ENDIF.
      ENDLOOP.


      DATA: lv_display_profile TYPE bal_s_prof.

      CLEAR lv_display_profile.

      CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
        IMPORTING
          e_s_display_profile = lv_display_profile.

      CONCATENATE TEXT-301 space bukrs '/' xblnr INTO lv_display_profile-title.


      lv_display_profile-show_all  = 'X'.
      lv_display_profile-exp_level = '1'.

      CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
        EXPORTING
          i_s_display_profile = lv_display_profile
          i_t_log_handle      = lt_log_handle_e
          i_amodal            = ' '.
    ENDIF.
  ENDMETHOD.


  METHOD mostrar_log_proc_masi.
    "log de modificaciones o bloqueos realizados.
    IF lt_mod_save[] IS NOT INITIAL.
      DATA: lo_alv1      TYPE REF TO cl_salv_table,
            lo_header    TYPE REF TO cl_salv_form_layout_grid,
            gr_columns   TYPE REF TO cl_salv_columns_table,
            gr_column    TYPE REF TO cl_salv_column_table,
            lt_mod_save2 TYPE t_modlog.

      lt_mod_save2[] = lt_mod_save.

      TRY.
          cl_salv_table=>factory(
            IMPORTING
              r_salv_table = lo_alv1
            CHANGING
              t_table      = lt_mod_save2 ).
        CATCH cx_salv_msg.
      ENDTRY.

      DATA: lr_functions TYPE REF TO cl_salv_functions_list.
      lr_functions = lo_alv1->get_functions( ).
      lr_functions->set_all( 'X' ).

      IF lo_alv1 IS BOUND.

        lo_alv1->set_screen_popup(
          start_column = 10
          end_column  = 100
          start_line  = 5
          end_line    = 20 ).

        TRY.
            gr_columns = lo_alv1->get_columns( ).
            gr_column ?= gr_columns->get_column( 'ICONO' ).
            gr_column->set_long_text( 'Estado' ).
            gr_column->set_medium_text( 'Estado' ).
            gr_column->set_short_text( 'Estado' ).

            gr_columns = lo_alv1->get_columns( ).
            gr_column ?= gr_columns->get_column( 'DOCUMENTO' ).
            gr_column->set_long_text( 'Documento' ).
            gr_column->set_medium_text( 'Documento' ).
            gr_column->set_short_text( 'Documento' ).

            gr_columns = lo_alv1->get_columns( ).
            gr_column ?= gr_columns->get_column( 'MENSAJE' ).
            gr_column->set_long_text( 'Mensaje' ).
            gr_column->set_medium_text( 'Mensaje' ).
            gr_column->set_short_text( 'Mensaje' ).
          CATCH cx_salv_not_found.
        ENDTRY.
        lo_alv1->display( ).

      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD obtener_datos.
    FREE ti_salida.
    DATA: wa_fechasii TYPE t_fechasii.
    DATA:wa_datos       LIKE LINE OF me->datos,
         wa_salida      LIKE LINE OF ti_salida,
         wa_conf_status LIKE LINE OF me->conf_status,
         lt_domval      TYPE TABLE OF dd07v,
         ls_domval      TYPE dd07v,
         lt_dom2        TYPE TABLE OF dd07v,
         ls_dom2        TYPE  dd07v,
         numero         TYPE p,
         lr_status      TYPE r_status.

    DATA: x_awkey  TYPE bkpf-awkey,
          x_aawsys TYPE bkpf-awsys.

    DATA: lt_lfb1 TYPE STANDARD TABLE OF lfb1,
          ls_lfb1 TYPE lfb1.

    CLEAR: ls_domval,ls_dom2 .
    REFRESH: lt_domval,lt_dom2.
**//.. Determinar textos de Forma de pago
    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname    = 'ZDO_FMAPAGO'
        text       = 'X'
      TABLES
        values_tab = lt_domval.

**//.. Determina texto de tipos de proveedor
    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname    = 'ZFI_DOM_TIPO'
        text       = 'X'
      TABLES
        values_tab = lt_dom2.

**//.. Determino tipo de proveedor.
    IF me->datos[] IS NOT INITIAL.
      SELECT *
      FROM lfb1
      INTO TABLE lt_lfb1
      FOR ALL ENTRIES IN me->datos
      WHERE lifnr = me->datos-lifnr AND
            bukrs = me->datos-bukrs .
      IF sy-dbcnt >= 1.
        SORT lt_lfb1 BY lifnr bukrs.
        DELETE ADJACENT DUPLICATES FROM lt_lfb1.
      ENDIF.
    ENDIF.
**//..

    SORT me->conf_status BY sociedad estatus.
    LOOP AT me->datos INTO wa_datos.
      CLEAR wa_salida.
      MOVE-CORRESPONDING wa_datos TO wa_salida.

      "Agregar Estatus
      READ TABLE me->conf_status INTO wa_conf_status WITH KEY sociedad = wa_datos-bukrs
                                                              estatus = wa_datos-status BINARY SEARCH.
      IF sy-subrc = 0.
        wa_salida-icon = wa_conf_status-icono.
      ENDIF.
      READ TABLE lt_lfb1 INTO ls_lfb1 WITH KEY bukrs = wa_datos-bukrs
                                                lifnr = wa_datos-lifnr.
      "Agregar tipo de proveedor y desc.
      IF sy-subrc = 0.
        wa_salida-zdte_tipo = ls_lfb1-zzdte_tipo.
        READ TABLE lt_dom2 INTO ls_dom2 WITH KEY domvalue_l = ls_lfb1-zzdte_tipo.
        IF sy-subrc = 0.
          wa_salida-desc_tipo = ls_dom2-ddtext.
        ENDIF.
      ENDIF.

      " Agregar Desc.Forma de pago
      READ TABLE lt_domval WITH KEY domvalue_l = wa_salida-fmapago INTO ls_domval.
      IF sy-subrc EQ 0.
        wa_salida-formapago = ls_domval-ddtext.
      ENDIF.

      "Documentos contables, Fecha de contabilización

      IF NOT wa_salida-belnr IS INITIAL.
        IF wa_salida-zdte_tipo EQ '2'.
          wa_salida-zdocfi = wa_salida-belnr.
          wa_salida-zanodoc = wa_salida-gjahr.

          SELECT SINGLE budat INTO wa_salida-zfechacont FROM bkpf
            WHERE bukrs EQ wa_salida-bukrs
              AND belnr EQ wa_salida-belnr
              AND gjahr EQ wa_salida-gjahr.

        ELSE.
          CLEAR x_awkey. CLEAR x_aawsys.
          CONCATENATE sy-sysid 'CLNT' sy-mandt INTO x_aawsys.
          CONCATENATE wa_salida-belnr wa_salida-gjahr INTO  x_awkey.
          CLEAR wa_salida-zdocfi. CLEAR wa_salida-zanodoc.
          SELECT SINGLE belnr gjahr budat  INTO (wa_salida-zdocfi , wa_salida-zanodoc , wa_salida-zfechacont) FROM bkpf
            WHERE awkey EQ x_awkey
             AND awtyp EQ 'RMRP'
             AND ( awsys EQ x_aawsys OR awsys EQ space ).
        ENDIF.
      ENDIF.

      "Fecha de recepcion del SII y calculo Dias transcurridos.
      READ TABLE me->fechasii INTO wa_fechasii WITH KEY f-bukrs =  wa_salida-bukrs
                                                  f-xblnr =  wa_salida-xblnr
                                                  f-lifnr =  wa_salida-lifnr
                                                  f-tipodte = wa_salida-tipodte
                                                  f-stcd1 = wa_salida-stcd1.

      IF sy-subrc = 0.
        wa_salida-zfecharsii = wa_fechasii-f-fecharsii.
        "---> agrego flag de q ya esta procesada en cabecera.
        wa_fechasii-flag = 'X'.
        MODIFY me->fechasii FROM wa_fechasii INDEX sy-tabix.
      ENDIF.
*los dias transcurridos por fecha de recepción o fecha de Factura.
      IF wa_salida-zfecharsii IS INITIAL.
        CLEAR numero.
        IF wa_salida-zfechacont IS INITIAL.
          numero = sy-datum - wa_datos-bldat.
        ELSE.
          numero = wa_salida-zfechacont - wa_datos-bldat.
        ENDIF.
      ELSE.
        CLEAR numero.
        IF wa_salida-zfechacont IS INITIAL.
          numero = sy-datum - wa_salida-zfecharsii .
        ELSE.
          numero = wa_salida-zfechacont - wa_salida-zfecharsii.
        ENDIF.
      ENDIF.
      IF numero  > 99.
        wa_salida-dias_trans = 99.
      ELSE.
        wa_salida-dias_trans = numero.
      ENDIF.
      APPEND wa_salida TO ti_salida.
    ENDLOOP.


    "--->Insertar registros zt_cb_recfactprovfechasii en ztfi_0074 de no existir.
    DELETE me->fechasii WHERE flag = 'X'.
    CLEAR wa_salida.

    CHECK me->status IS INITIAL OR 'S' IN me->status.

    LOOP AT me->fechasii INTO wa_fechasii.
      CLEAR wa_salida.
      MOVE-CORRESPONDING wa_fechasii-f TO wa_salida.
      wa_salida-zfecharsii = wa_fechasii-f-fecharsii.
      SELECT * FROM ztfi_0074 INTO CORRESPONDING FIELDS OF wa_datos UP TO 1 ROWS
      WHERE bukrs  EQ wa_salida-bukrs AND
             xblnr EQ wa_salida-xblnr AND
             lifnr EQ wa_salida-lifnr AND
             tipodte EQ wa_salida-tipodte AND
             stcd1 EQ  wa_salida-stcd1
             ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF sy-subrc NE 0.


        wa_salida-status = 'S'.

        READ TABLE me->conf_status INTO wa_conf_status WITH KEY sociedad = wa_salida-bukrs
                                                        estatus = wa_salida-status BINARY SEARCH.
        IF sy-subrc = 0.
          wa_salida-icon = wa_conf_status-icono.
        ENDIF.

        IF wa_salida-zfecharsii IS INITIAL.
          CLEAR numero.
          IF wa_salida-zfechacont IS INITIAL.
            numero = sy-datum - wa_salida-bldat.
          ELSE.
            numero = wa_salida-zfechacont - wa_salida-bldat.
          ENDIF.
        ELSE.
          CLEAR numero.
          IF wa_salida-zfechacont IS INITIAL.
            numero = sy-datum - wa_salida-zfecharsii .
          ELSE.
            numero = wa_salida-zfechacont - wa_salida-zfecharsii.
          ENDIF.
        ENDIF.

        IF numero  > 99.
          wa_salida-dias_trans = 99.
        ELSE.
          wa_salida-dias_trans = numero.
        ENDIF.

        IF wa_salida-name1 IS INITIAL.
          SELECT name1 INTO wa_salida-name1  FROM lfa1 UP TO 1 ROWS
             WHERE lifnr EQ wa_salida-lifnr.
          ENDSELECT.
          SELECT zzdte_tipo INTO wa_salida-zdte_tipo FROM lfb1 UP TO 1 ROWS
             WHERE lifnr EQ wa_salida-lifnr
               AND bukrs EQ wa_salida-bukrs.
          ENDSELECT.
          "Agregar tipo de proveedor y desc.
          IF wa_salida-zdte_tipo IS NOT INITIAL.
            READ TABLE lt_dom2 INTO ls_dom2 WITH KEY domvalue_l = wa_salida-zdte_tipo.
            IF sy-subrc = 0.
              wa_salida-desc_tipo = ls_dom2-ddtext.
            ENDIF.
          ENDIF.
        ENDIF.

        APPEND  wa_salida TO ti_salida.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD obtener_status.

    "ZTFI_0079

    "ti_status[] = me->CONF_STATUS[].

  ENDMETHOD.


  METHOD reproc_idoc.
    DATA: repro                TYPE REF TO  bd_ls_mon,
          lrdoc                TYPE bdrg_doc_tab,
          lrdat                TYPE bdrg_dat_tab,
          lrmes                TYPE bdrg_mes_tab,
          lrpar                TYPE bdrg_par_tab,
          lrobj                TYPE bdrg_obj_tab,
          lrda2                TYPE bdrg_dat_tab,
          lrtim                TYPE bdrg_tim_tab,
          lrti2                TYPE bdrg_tim_tab,
          lroky                TYPE bdrg_oky_tab,
          lrsta                TYPE bdrg_sta_tab,
          ls_st                TYPE bdmon_stat,
          lv_mensaje           TYPE char200,
          lv_documento_mensaje TYPE char200,
          lt_mod_save          TYPE STANDARD TABLE OF ty_mod,
          ls_mod_save          TYPE ty_mod,
          lt_fi_0074           TYPE STANDARD TABLE OF ztfi_0074.
    DATA objkey TYPE cdhdr-objectid.
    DATA: old_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074.
    DATA: old_wa TYPE ztfi_0074.
    CLEAR old_ztfi_0074[].
    CLEAR old_wa.

    DATA: new_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074.
    DATA: new_wa TYPE ztfi_0074.
    CLEAR new_ztfi_0074[].
    CLEAR new_wa.


    CREATE OBJECT repro.
*    "---> Solo se reprocesan IDOC con estatus 'A'
    DELETE lt_salida_temp WHERE status NE 'A'.
    IF lines( lt_salida_temp ) GT 0.
      READ TABLE lt_salida_temp TRANSPORTING NO FIELDS WITH KEY lifnr = ' '. "Sin Acreedor
      IF sy-subrc = 0.
        CLEAR: lv_mensaje,
               ls_mod_save,
               lv_documento_mensaje.
        CONCATENATE ' '
                    TEXT-226 INTO lv_mensaje SEPARATED BY space.

        ls_mod_save-icono     = icon_red_light.
        ls_mod_save-documento = ' '.
        ls_mod_save-mensaje   = lv_mensaje.
        APPEND ls_mod_save TO lt_mod_save.

      ELSE.

        LOOP AT lt_salida_temp ASSIGNING FIELD-SYMBOL(<f1>).
          lrdoc  = VALUE #( BASE lrdoc  ( sign = 'I' option = 'EQ' low = <f1>-docnum ) ).
        ENDLOOP.
        lrsta = VALUE #( BASE lrsta  ( sign = 'I' option = 'EQ' low = '51' ) ).
        ls_st-status = '51'.
*   "---> Filtro
        CALL METHOD repro->set_filter
          EXPORTING
            filter_docnum  = lrdoc
            filter_credat  = lrdat
            filter_mestyp  = lrmes
            filter_partner = lrpar
            filter_objtyp  = lrobj
            filter_upddat  = lrda2
            filter_cretim  = lrtim
            filter_updtim  = lrti2
            filter_objkey  = lroky
            filter_status  = lrsta.

*   "--->Collect
        CALL METHOD repro->collect_data.
*   "--->process_idoc
        CALL METHOD repro->process_idocs
          EXPORTING
            process_mode = 'A'
            idoc_stat    = ls_st.
*   "--->guardo LogsModDoc
        SELECT * INTO TABLE lt_fi_0074
          FROM ztfi_0074
          FOR ALL ENTRIES IN lt_salida_temp
          WHERE bukrs EQ lt_salida_temp-bukrs
            AND xblnr EQ lt_salida_temp-xblnr
            AND tipodte EQ lt_salida_temp-tipodte
            AND lifnr EQ lt_salida_temp-lifnr
            ORDER BY PRIMARY KEY.

        LOOP AT lt_salida_temp  ASSIGNING FIELD-SYMBOL(<f2>).
          READ TABLE  lt_fi_0074 TRANSPORTING NO FIELDS WITH KEY bukrs = <f2>-bukrs
                     xblnr = <f2>-xblnr
                     tipodte = <f2>-tipodte
                     lifnr = <f2>-lifnr
                     status = '1'.
          IF sy-subrc = 0.
            MOVE-CORRESPONDING <f2> TO old_wa.
            new_wa = old_wa.
            new_wa-status = '1'.
            APPEND new_wa TO new_ztfi_0074.
            APPEND old_wa TO old_ztfi_0074.
            objkey(3) = sy-mandt.
            objkey+3(4) = <f2>-bukrs.
            objkey+7(16) = <f2>-xblnr.
            objkey+23(10) = <f2>-lifnr.
            objkey+33(3) = <f2>-tipodte.

            CALL FUNCTION 'ZTFI_0074_WRITE_DOCUMENT'
              IN UPDATE TASK
              EXPORTING
                objectid      = objkey
                tcode         = 'ZDTE_0003'
                utime         = sy-uzeit
                udate         = sy-datum
                username      = sy-uname
                upd_ztfi_0074 = 'U'
              TABLES
                xztfi_0074    = new_ztfi_0074
                yztfi_0074    = old_ztfi_0074.
            COMMIT WORK AND WAIT.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE.
      CLEAR: lv_mensaje,
             ls_mod_save,
             lv_documento_mensaje.
      CONCATENATE ' '
                  TEXT-225 INTO lv_mensaje SEPARATED BY space.

      ls_mod_save-icono     = icon_red_light.
      ls_mod_save-documento = ' '.
      ls_mod_save-mensaje   = lv_mensaje.
      APPEND ls_mod_save TO lt_mod_save.
    ENDIF.
    "log de modificaciones o bloqueos realizados.
    IF lt_mod_save[] IS NOT INITIAL.
      me->mostrar_log_proc_masi( EXPORTING  lt_mod_save = lt_mod_save[] ).
    ENDIF.


    FREE repro.
  ENDMETHOD.


  METHOD save.
    DATA: ti_ztfi_0074_bd TYPE STANDARD TABLE OF ztfi_0074.
    DATA: ln_salida_or LIKE LINE OF me->datos_or.
    DATA: ln_salida_act LIKE LINE OF me->datos.
    DATA: ti_mod_save TYPE STANDARD TABLE OF ty_mod,
          ls_mod_save TYPE ty_mod.
    DATA: lt_ztfi_0074_old TYPE STANDARD TABLE OF yztfi_0074 WITH NON-UNIQUE DEFAULT KEY,
          ls_ztfi_0074_old TYPE yztfi_0074.
    DATA: lt_ztfi_0074_new TYPE STANDARD TABLE OF yztfi_0074 WITH NON-UNIQUE DEFAULT KEY,
          ls_ztfi_0074_new TYPE yztfi_0074,
          lv_objectid      TYPE cdhdr-objectid.
    DATA: lv_mensaje           TYPE char200,
          edo_bloqueo          TYPE abap_bool,
          lv_documento_mensaje TYPE char200.

    IF me->gv_modif NE abap_true.
      MESSAGE s109(f5) DISPLAY LIKE 'W'.
      EXIT.
    ENDIF.

*** Log de Modificaciones Status (Monitor)
    "----->tabla interna con modificaciones realizadas en el monitor
    IF lt_ztfi_0074[] IS NOT INITIAL.

      SELECT *
      FROM ztfi_0074
      INTO TABLE ti_ztfi_0074_bd
      FOR ALL ENTRIES IN lt_ztfi_0074
      WHERE bukrs   = lt_ztfi_0074-bukrs
        AND xblnr   = lt_ztfi_0074-xblnr
        AND lifnr   = lt_ztfi_0074-lifnr
        AND tipodte = lt_ztfi_0074-tipodte.
      IF sy-dbcnt >= 1.
        SORT ti_ztfi_0074_bd BY bukrs xblnr lifnr tipodte.
        DELETE ADJACENT DUPLICATES FROM ti_ztfi_0074_bd.
      ENDIF.

      FREE ti_mod_save.
      CLEAR ls_mod_save. refresh ti_mod_save.

      LOOP AT lt_ztfi_0074 ASSIGNING FIELD-SYMBOL(<fs>).
        CLEAR lv_documento_mensaje.
*        CLEAR ti_mod_save.
        CONCATENATE <fs>-bukrs '/' <fs>-xblnr INTO lv_documento_mensaje.

        "-----Bloqueo documento.
        READ TABLE me->datos INTO ln_salida_act WITH KEY bukrs = <fs>-bukrs
                                                 xblnr = <fs>-xblnr
                                                 lifnr = <fs>-lifnr
                                                 tipodte = <fs>-tipodte.
        IF sy-subrc = 0.
          edo_bloqueo = me->bloqueo( EXPORTING lt_zcb_recfactprov = ln_salida_act ).
          IF edo_bloqueo NE 0.

            CLEAR: lv_mensaje,
            ls_mod_save.
            CONCATENATE TEXT-216 lv_documento_mensaje
                        TEXT-222 INTO lv_mensaje SEPARATED BY space.

            ls_mod_save-icono     = icon_red_light.
            ls_mod_save-documento = lv_documento_mensaje.
            ls_mod_save-mensaje   = lv_mensaje.
            APPEND ls_mod_save TO ti_mod_save.
            CONTINUE.
          ENDIF.
        ENDIF.

        "--->registro en BD actual.
        READ TABLE ti_ztfi_0074_bd INTO DATA(ls_db)
                                   WITH KEY bukrs = <fs>-bukrs
                                            xblnr = <fs>-xblnr
                                            lifnr = <fs>-lifnr
                                            tipodte = <fs>-tipodte.
        IF sy-subrc = 0.

          READ TABLE me->datos_or INTO ln_salida_or WITH KEY bukrs = <fs>-bukrs
                                                 xblnr = <fs>-xblnr
                                                 lifnr = <fs>-lifnr
                                                 tipodte = <fs>-tipodte.
          IF sy-subrc = 0.
            DATA: lv_salida_original TYPE ztfi_0074.
            MOVE-CORRESPONDING ln_salida_or TO lv_salida_original.
          ENDIF.
        ENDIF.

        IF ls_db-aprobador   EQ lv_salida_original-aprobador AND
           ls_db-nombana     EQ lv_salida_original-nombana AND
           ls_db-texto_libre EQ lv_salida_original-texto_libre AND
           ls_db-status      EQ lv_salida_original-status AND
           ls_db-zfechapro   EQ lv_salida_original-zfechapro AND
           ls_db-zfecharec   EQ lv_salida_original-zfecharec AND
           ls_db-ebeln       EQ lv_salida_original-ebeln.

          UPDATE ztfi_0074 SET aprobador = <fs>-aprobador
                               nombana = <fs>-nombana
                               texto_libre = <fs>-texto_libre
                               zfechapro = <fs>-zfechapro
                               zfecharec = <fs>-zfecharec
                               ebeln    = <fs>-ebeln
                           WHERE bukrs = <fs>-bukrs AND
                                 xblnr = <fs>-xblnr AND
                                 lifnr = <fs>-lifnr AND
                                 tipodte = <fs>-tipodte.
          IF sy-subrc = 0.
            CLEAR ls_mod_save.
            ls_mod_save-icono = icon_green_light.
            ls_mod_save-documento = lv_documento_mensaje.
            ls_mod_save-mensaje = 'OK'.
            APPEND ls_mod_save TO ti_mod_save.

**            EXPORT  gt_sesion = gt_sesion TO DATABASE indx(zq)  CLIENT sy-mandt ID 'ZTFI0074'.

            COMMIT WORK AND WAIT.

            MOVE-CORRESPONDING <fs> TO ln_salida_or.

            MODIFY me->datos_or FROM ln_salida_or
                                TRANSPORTING aprobador nombana texto_libre
                                             zfechapro zfecharec
                                WHERE bukrs   = ln_salida_or-bukrs
                                  AND xblnr   = ln_salida_or-xblnr
                                  AND lifnr   = ln_salida_or-lifnr
                                  AND tipodte = ln_salida_or-tipodte.
          ENDIF.

        ELSE.
          IF me->gt_sesion[] IS NOT INITIAL.


            DATA: lt_sesion  TYPE STANDARD TABLE OF ty_gt_sesion WITH NON-UNIQUE DEFAULT KEY,
                  lt_sesion2 TYPE STANDARD TABLE OF ty_gt_sesion WITH NON-UNIQUE DEFAULT KEY,
                  ln_sesion  LIKE LINE OF lt_sesion.
            FREE lt_sesion.

            IMPORT gt_sesion = lt_sesion FROM DATABASE indx(zq)  CLIENT sy-mandt ID 'ZTFI0074'.

            READ TABLE me->gt_sesion  INTO ls_sesion WITH KEY  bukrs = <fs>-bukrs
                                          xblnr = <fs>-xblnr
                                          lifnr = <fs>-lifnr
                                          tipodte = <fs>-tipodte
                                          nombana = <fs>-nombana
                                          texto_libre = <fs>-texto_libre
                                          aprobador = <fs>-aprobador
                                          zfechapro = <fs>-zfechapro
                                          zfecharec = <fs>-zfecharec.
            IF sy-subrc = 0.
              READ TABLE lt_sesion INTO ln_sesion  WITH KEY  bukrs = ls_sesion-bukrs
                                          xblnr = ls_sesion-xblnr
                                          lifnr = ls_sesion-lifnr
                                          tipodte = ls_sesion-tipodte
                                          usuario = sy-uname.
              IF sy-subrc = 0.

                UPDATE ztfi_0074 SET aprobador = ls_sesion-aprobador
                                nombana = <fs>-nombana
                                texto_libre = <fs>-texto_libre
                                zfechapro = <fs>-zfechapro
                                zfecharec = <fs>-zfecharec
                            WHERE bukrs = <fs>-bukrs AND
                                  xblnr = <fs>-xblnr AND
                                  lifnr = <fs>-lifnr AND
                                  tipodte = <fs>-tipodte.
                IF sy-subrc = 0.
                  CLEAR ls_mod_save.
                  ls_mod_save-icono = icon_green_light.
                  ls_mod_save-documento = lv_documento_mensaje.
                  ls_mod_save-mensaje = 'OK'.
                  APPEND ls_mod_save TO ti_mod_save.
                  COMMIT WORK AND WAIT.
                ENDIF.
                CONTINUE.
              ENDIF.
            ENDIF.
          ENDIF.

          CLEAR lv_mensaje.
          CLEAR ls_mod_save.
          CONCATENATE TEXT-216 lv_documento_mensaje
                      TEXT-217  INTO lv_mensaje SEPARATED BY space.
          ls_mod_save-icono = icon_red_light.
          ls_mod_save-documento = lv_documento_mensaje.
          ls_mod_save-mensaje = lv_mensaje.
          APPEND ls_mod_save TO ti_mod_save.
        ENDIF.
        "-----> Desbloqueo registro
        me->desbloqueo( EXPORTING lt_zcb_recfactprov = ln_salida_act ).
      ENDLOOP.

      FREE lt_ztfi_0074.
      IF <fs> IS ASSIGNED.
        UNASSIGN <fs>.
      ENDIF.

      "log de modificaciones o bloqueos realizados.
      IF ti_mod_save[] IS NOT INITIAL.
        DATA: go_alv    TYPE REF TO cl_salv_table,
              lo_header TYPE REF TO cl_salv_form_layout_grid.
        DATA: lt_mod TYPE STANDARD TABLE OF ty_mod.
        lt_mod[] = ti_mod_save[].
        TRY.
            cl_salv_table=>factory(
              IMPORTING
                r_salv_table = go_alv
              CHANGING
                t_table      = lt_mod ).
          CATCH cx_salv_msg.
        ENDTRY.

        DATA: lr_functions TYPE REF TO cl_salv_functions_list.
        lr_functions = go_alv->get_functions( ).
        lr_functions->set_all( 'X' ).
        IF go_alv IS BOUND.

          go_alv->set_screen_popup(
            start_column = 10
            end_column  = 100
            start_line  = 5
            end_line    = 20 ).

          DATA:gr_columns TYPE REF TO cl_salv_columns_table,
               gr_column  TYPE REF TO cl_salv_column_table.
          TRY.
              gr_columns = go_alv->get_columns( ).
              gr_column ?= gr_columns->get_column( 'ICONO' ).
              gr_column->set_long_text( 'Estado' ).
              gr_column->set_medium_text( 'Estado' ).
              gr_column->set_short_text( 'Estado' ).

              gr_columns = go_alv->get_columns( ).
              gr_column ?= gr_columns->get_column( 'DOCUMENTO' ).
              gr_column->set_long_text( 'Documento' ).
              gr_column->set_medium_text( 'Documento' ).
              gr_column->set_short_text( 'Documento' ).

              gr_columns = go_alv->get_columns( ).
              gr_column ?= gr_columns->get_column( 'MENSAJE' ).
              gr_column->set_long_text( 'Mensaje' ).
              gr_column->set_medium_text( 'Mensaje' ).
              gr_column->set_short_text( 'Mensaje' ).
            CATCH cx_salv_not_found.
          ENDTRY.
          go_alv->display( ).

        ENDIF.
      ENDIF.

    ENDIF.

    CLEAR: me->gv_modif.
    CLEAR: me->lt_ztfi_0074.

    me->buscar_datos( ).

    CALL METHOD gr_alvgrid->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
    IF sy-subrc NE 0.
    ENDIF.
  ENDMETHOD.


  METHOD set_fieldname.
   DATA: lv_retcode   TYPE c,
          lv_titulo    TYPE cua_tit_tx,
          lv_valor     TYPE spo_value,
          lv_nombana   TYPE ztfi_0074-nombana,
          lv_aprobador TYPE ztfi_0074-aprobador,
          ls_row       TYPE lvc_s_roid,
          lv_true      TYPE abap_bool VALUE 'X',
          ls_ztfi_0074 TYPE ztfi_0074.
    FIELD-SYMBOLS: <fs_salida> TYPE zefi_0026.
    lv_titulo = TEXT-t01.

    DATA: lt_style_cells TYPE lvc_t_modi,
          ls_style_cells TYPE lvc_s_modi,
          lt_cells       TYPE lvc_t_pos,
          ls_cells       TYPE lvc_s_pos.

**//.. Solicitar valor por pantalla
    CALL METHOD me->get_fieldname
      EXPORTING
        i_titulo    = lv_titulo
        i_fieldname = i_fieldname
      IMPORTING
        e_return    = lv_retcode
        e_valor     = lv_valor.
    IF lv_retcode IS NOT INITIAL.
      CASE lv_retcode.
        WHEN 'A'.
          MESSAGE ID '00' TYPE 'S' NUMBER 359 DISPLAY LIKE 'E'.
          EXIT.
        WHEN OTHERS.
          EXIT.
      ENDCASE.
    ENDIF.

**//..
    LOOP AT i_selected_rows_t INTO ls_row.
      READ TABLE me->datos ASSIGNING <fs_salida> INDEX ls_row-row_id.
      IF sy-subrc = 0.
        CLEAR: ls_cells,
               ls_style_cells,
               ls_ztfi_0074.

**//..
        CASE i_fieldname.
          WHEN 'NOMBANA'.
            lv_nombana = lv_valor.
            <fs_salida>-nombana = lv_nombana.

            ls_cells-fieldname  = i_fieldname.
            ls_cells-row_id     = ls_row-row_id.
            APPEND ls_cells TO lt_cells.

            ls_style_cells-row_id    = ls_row-row_id.
            ls_style_cells-fieldname = i_fieldname.
            ls_style_cells-value     = lv_nombana.
            APPEND ls_style_cells TO lt_style_cells.
          WHEN 'APROBADOR'.
            lv_aprobador = lv_valor.
            <fs_salida>-aprobador = lv_aprobador.
          WHEN OTHERS.
            CONTINUE.
        ENDCASE.

        MOVE-CORRESPONDING <fs_salida> TO ls_ztfi_0074.

        " verificar tabla con datos modificados
        READ TABLE me->lt_ztfi_0074 WITH KEY mandt   = <fs_salida>-mandt
                                             bukrs   = <fs_salida>-bukrs
                                             xblnr   = <fs_salida>-xblnr
                                             lifnr   = <fs_salida>-lifnr
                                             tipodte = <fs_salida>-tipodte
                                    ASSIGNING FIELD-SYMBOL(<fs_ztfi_0074>).
        IF sy-subrc EQ 0.
          <fs_ztfi_0074> = ls_ztfi_0074.
        ELSE.
          APPEND ls_ztfi_0074 TO me->lt_ztfi_0074.
        ENDIF.
      ENDIF.
    ENDLOOP.

**//.. Marcar cambio
    IF gv_modif IS INITIAL.
      MOVE abap_true TO gv_modif.
    ENDIF.

**//.. Refrescar tabla
    CALL METHOD gr_alvgrid->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
    IF sy-subrc NE 0.
    ENDIF.

  ENDMETHOD.


  METHOD ver_detalle.

    DATA: ti_detalle TYPE STANDARD TABLE OF ztfi_0075 WITH NON-UNIQUE DEFAULT KEY.
    SELECT *
    FROM ztfi_0075
    INTO TABLE ti_detalle
    WHERE bukrs = sociedad AND
          lifnr = proveedor AND
          xblnr = documento AND
          tipodte = tipodte.
    IF sy-dbcnt >= 1.
      SORT ti_detalle BY pos.

      DATA: lt_catalogo_det TYPE slis_t_fieldcat_alv.
      DATA: lv_layout TYPE slis_layout_alv.
      CLEAR lv_layout.
      lv_layout-colwidth_optimize = 'X'.
      lv_layout-zebra = 'X'.

      CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name = 'ZTFI_0075'
        CHANGING
          ct_fieldcat      = lt_catalogo_det.
      CHECK lt_catalogo_det[] IS NOT INITIAL.

      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          it_fieldcat           = lt_catalogo_det
          is_layout             = lv_layout
          i_screen_start_column = 10
          i_screen_start_line   = 3
          i_screen_end_column   = 100
          i_screen_end_line     = 20
        TABLES
          t_outtab              = ti_detalle.

    ENDIF.
  ENDMETHOD.


  METHOD ver_eventos.


    DATA: co_type TYPE string.
    DATA: t_0078 TYPE STANDARD TABLE OF ztfi_0078.
    DATA: wa_0078 TYPE ztfi_0078.
    DATA: gr_dte_sii TYPE REF TO object, "zcl_sii,
          lt_eventos TYPE zdte_liseve_t.

    CLEAR co_type. CLEAR wa_0078. REFRESH t_0078.
    SELECT SINGLE * INTO  wa_0078 FROM ztfi_0078
       WHERE bukrs EQ sociedad.


    CLEAR  co_type.
    SELECT SINGLE nombre_int INTO co_type FROM ztfi_0078b
       WHERE vatint = wa_0078-vatint
       AND   codint = '0002'.

    CHECK co_type IS NOT INITIAL.

    TRY.
        CREATE OBJECT gr_dte_sii TYPE (co_type).
        IF  gr_dte_sii IS BOUND.
          CALL METHOD gr_dte_sii->('DISPLAY_EVENTOS') "display_eventos
            EXPORTING
              itab    = itab
              i_bukrs = sociedad
            IMPORTING
              lt_eve  = lt_eventos.
        ENDIF.

        IF lt_eventos[] IS NOT INITIAL.

          DATA: lt_catalogo_det TYPE slis_t_fieldcat_alv.
          DATA: lv_layout TYPE slis_layout_alv.
          CLEAR lv_layout.
          lv_layout-colwidth_optimize = 'X'.
          lv_layout-zebra = 'X'.

          CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
            EXPORTING
              i_structure_name = 'ZDTE_LISEVE'
            CHANGING
              ct_fieldcat      = lt_catalogo_det.
          CHECK lt_catalogo_det[] IS NOT INITIAL.

          CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
            EXPORTING
              it_fieldcat           = lt_catalogo_det
              is_layout             = lv_layout
              i_screen_start_column = 10
              i_screen_start_line   = 3
              i_screen_end_column   = 100
              i_screen_end_line     = 20
            TABLES
              t_outtab              = lt_eventos.

        ENDIF.
    ENDTRY.
  ENDMETHOD.


  METHOD ver_pdf.

*  DATA: L_STREAM TYPE REF TO IF_IXML_OSTREAM.
*  DATA: GO_XML TYPE REF TO CL_XML_DOCUMENT.
*  DATA: LS_XML TYPE STRING,
*        LX_XML TYPE XSTRING.
*
*  DATA: LV_RUTEMPRESA_SDV(13) TYPE C,
*        LV_DV(1)              TYPE C.
*
*  DATA: GO_OBTIENE_PDF TYPE REF TO ZWSCO_OBTIENE_PDF_OUT.
*  DATA: WA_INPUT TYPE ZWSOBTENER_PDFSOAP_IN.
*  DATA: WA_OUTPUT TYPE ZWSOBTENER_PDFSOAP_OUT.
*  DATA: GS_BAL_MSG TYPE BAL_S_MSG.
*
*  DATA LV_LARGO TYPE I.
*  DATA:  LV_RUTEMPRESA(12) TYPE C.
*  DATA: OREF TYPE REF TO CX_AI_SYSTEM_FAULT,
*        IREF TYPE REF TO CX_AI_APPLICATION_FAULT,
*        TEXT TYPE STRING.
*  DATA: LV_TOYEAR  TYPE INRI-TOYEAR.
*
*  CLEAR WA_INPUT. CLEAR WA_OUTPUT.CLEAR LV_RUTEMPRESA_SDV. CLEAR LV_DV.
*  SELECT SINGLE PAVAL INTO LV_RUTEMPRESA
*  FROM T001Z
*  WHERE BUKRS = BUKRS AND PARTY = 'TAXNR'.
*
*
*
*  REPLACE ALL OCCURRENCES OF '.' IN  LV_RUTEMPRESA WITH SPACE.
*  CONDENSE LV_RUTEMPRESA NO-GAPS..
*  SPLIT LV_RUTEMPRESA AT '-' INTO LV_RUTEMPRESA_SDV LV_DV.
*  WA_INPUT-S_RUTT_RECE = LV_RUTEMPRESA_SDV.
*
*  CLEAR LV_RUTEMPRESA_SDV. CLEAR LV_DV.
*  LV_RUTEMPRESA = STCD1.
*  REPLACE ALL OCCURRENCES OF '.' IN LV_RUTEMPRESA WITH SPACE.
*  CONDENSE LV_RUTEMPRESA NO-GAPS..
*  SPLIT LV_RUTEMPRESA AT '-' INTO LV_RUTEMPRESA_SDV LV_DV.
*  WA_INPUT-S_RUTT_EMIS = LV_RUTEMPRESA_SDV.
*
*
*  WA_INPUT-S_TIPO_DOCU =  TIPODTE.
*  WA_INPUT-S_FOLI_DOCU = XBLNR.
*
*  TRY.
*      FREE GO_OBTIENE_PDF.
*      CREATE OBJECT GO_OBTIENE_PDF.
*    CATCH CX_AI_SYSTEM_FAULT.
*  ENDTRY.
*  TRY.
*      CALL METHOD GO_OBTIENE_PDF->OBTIENE_PDF_OUT
*        EXPORTING
*          OUTPUT = WA_INPUT
*        IMPORTING
*          INPUT  = WA_OUTPUT.
*    CATCH CX_AI_SYSTEM_FAULT INTO OREF.
*      TEXT = OREF->GET_TEXT( ).
*    CATCH CX_AI_APPLICATION_FAULT INTO IREF.
*      TEXT = IREF->GET_TEXT( ).
*  ENDTRY.
*  "----> Operacion NOOK
*
*  IF WA_OUTPUT-OBTENER_PDFRESULT-V_CODIGO NE 'DOK'. "NoOK
*    IF SY-BATCH NE 'X'.
*      IF WA_OUTPUT-OBTENER_PDFRESULT-V_CODIGO ='DON'.
*        MESSAGE 'No existe documento' TYPE 'E'.
*      ELSEIF  WA_OUTPUT-OBTENER_PDFRESULT-V_CODIGO ='ER0'.
*        MESSAGE 'parametro sin valor al llamar PDF' TYPE 'E'.
*      ELSEIF  WA_OUTPUT-OBTENER_PDFRESULT-V_CODIGO ='ERR1'.
*        MESSAGE 'No se encuetra PDF, no puede ser Generado' TYPE 'E'.
*      ELSEIF  WA_OUTPUT-OBTENER_PDFRESULT-V_CODIGO ='ERR2'.
*        MESSAGE 'Error en proceso de Generacion PDF' TYPE 'E'.
*      ELSE.
*        MESSAGE 'Error WS  PDF' TYPE 'E'.
*      ENDIF.
*    ENDIF.
*
*    "----> Operacion OK
*  ELSE. "OK
*
*    LS_XML = WA_OUTPUT-OBTENER_PDFRESULT-V_PDF.
*    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
*      EXPORTING
*        TEXT   = LS_XML
*      IMPORTING
*        BUFFER = LX_XML
*      EXCEPTIONS
*        FAILED = 1
*        OTHERS = 2.
*    IF SY-SUBRC <> 0.
*      MESSAGE 'Error in the PDF file' TYPE 'E'.
*    ENDIF.
*
*    CHECK LX_XML IS NOT INITIAL.
*    DATA: LS_OUTPUT TYPE SMUM_XMLTB,
*          LT_OUTPUT TYPE STANDARD TABLE OF SMUM_XMLTB,
*          LS_RETURN TYPE BAPIRET2,
*          LT_RETURN TYPE BAPIRET2_T.
*
**    CALL FUNCTION 'SMUM_XML_PARSE'
**      EXPORTING
**        xml_input = lx_xml
**      TABLES
**        xml_table = lt_output
**        return    = lt_return
**      EXCEPTIONS
**        OTHERS    = 0.
**    CHECK lt_output[] IS NOT INITIAL.
**
**    DATA: lv_cadena TYPE string.
**
**    LOOP AT lt_output INTO ls_output  WHERE cname = 'TextData'.
**
**      CONCATENATE lv_cadena  ls_output-cvalue  INTO lv_cadena.
**
**    ENDLOOP.
**
**    DATA: lv_xstring TYPE xstring.
***
**    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
**      EXPORTING
**        text   = lv_cadena
**      IMPORTING
**        buffer = lv_xstring
**      EXCEPTIONS
**        failed = 1
**        OTHERS = 2.
**
**    CHECK sy-subrc = 0.
*
*
*    DATA: CONVERTER TYPE REF TO CL_ABAP_CONV_IN_CE.
*    CALL METHOD CL_ABAP_CONV_IN_CE=>CREATE
*      EXPORTING
*        INPUT       = LX_XML "lv_xstring
*        ENCODING    = 'UTF-8'
*        REPLACEMENT = '?'
*        IGNORE_CERR = ABAP_TRUE
*      RECEIVING
*        CONV        = CONVERTER.
*
*    DATA: LV_ENCODED TYPE STRING.
*
*    TRY.
*        CALL METHOD CONVERTER->READ
*          IMPORTING
*            DATA = LV_ENCODED.
*      CATCH CX_SY_CONVERSION_CODEPAGE.
**        -- Should ignore errors in code conversions
*      CATCH CX_SY_CODEPAGE_CONVERTER_INIT.
**        -- Should ignore errors in code conversions
*      CATCH CX_PARAMETER_INVALID_TYPE.
*      CATCH CX_PARAMETER_INVALID_RANGE.
*    ENDTRY.
*
*    DATA: LV_DECODEX TYPE XSTRING.
*    TRY.
*        CALL FUNCTION 'SSFC_BASE64_DECODE'
*          EXPORTING
*            B64DATA = LV_ENCODED
*          IMPORTING
*            BINDATA = LV_DECODEX
*          EXCEPTIONS
*            OTHERS  = 8.
*      CATCH CX_SY_CONVERSION_CODEPAGE.
**        -- Should ignore errors in code conversions
*      CATCH CX_SY_CODEPAGE_CONVERTER_INIT.
**        -- Should ignore errors in code conversions
*      CATCH CX_PARAMETER_INVALID_TYPE.
*      CATCH CX_PARAMETER_INVALID_RANGE.
*    ENDTRY.
*    CHECK LV_DECODEX IS NOT INITIAL.
*    PDF = LV_DECODEX.
*
*  ENDIF.
*
  ENDMETHOD.                    "ver_pdf


  METHOD ver_pdf_arch.
    DATA: lv_respuesta TYPE c.
*    IF fondo IS INITIAL.
*      CALL FUNCTION 'POPUP_TO_CONFIRM'
*        EXPORTING
*          text_question = TEXT-400
*          text_button_1 = 'Si'
*          text_button_2 = 'No'
*        IMPORTING
*          answer        = lv_respuesta.
*    ELSE.
*      lv_respuesta = '1'.
*    ENDIF.
*
*    CHECK lv_respuesta = '1'.


    DATA: lv_exception TYPE REF TO cx_root.
    DATA: wa_tfi_0078 TYPE ztfi_0078.

    SELECT SINGLE *
    FROM ztfi_0078
    INTO wa_tfi_0078
    WHERE
          bukrs = bukrs .


    IF sy-subrc NE 0.
      "--->   Ruta no exite

    ELSE.

      TYPES: BEGIN OF ty_hex_record,
               field(1024) TYPE x,
             END OF ty_hex_record.

      DATA: lt_archivo TYPE STANDARD TABLE OF ty_hex_record,
            ln_archivo LIKE LINE OF lt_archivo.
      DATA: lv_fname TYPE string.
      DATA: lv_file TYPE string.


      DATA: lv_alias      TYPE user_dir-aliass,
            lv_directorio TYPE user_dir-dirname.

      lv_alias =  wa_tfi_0078-direcpdf.

      CHECK lv_alias IS NOT INITIAL.

      SELECT SINGLE dirname
      FROM user_dir
      INTO lv_directorio
      WHERE aliass = lv_alias.

      CHECK lv_directorio IS NOT INITIAL.

      CONCATENATE rut_emisor rut_receptor tipo_dte  xblnr  '.PDF' INTO lv_fname.
      CONCATENATE lv_directorio lv_fname INTO lv_file SEPARATED BY '\'.
      lv_fname = lv_file.
      TRY.

          OPEN DATASET lv_fname FOR INPUT IN BINARY MODE.
          IF sy-subrc EQ 0.
            DO.
              READ DATASET lv_fname INTO ln_archivo-field.
              IF sy-subrc EQ 0.
                APPEND ln_archivo TO lt_archivo.
              ELSE.
                APPEND ln_archivo TO lt_archivo.
                EXIT.
              ENDIF.
            ENDDO.
            CLOSE DATASET lv_fname.

            IF lt_archivo[] IS NOT INITIAL.
              DATA b_size TYPE i.
              DATA ultima_l TYPE i.
              ultima_l = lines( lt_archivo[] ).
              IF ultima_l > 0.
                READ TABLE lt_archivo INTO ln_archivo INDEX ultima_l.
*              b_size = b_size + strlen( ln_archivo ).
              ENDIF.
*            b_size = 1024 * ( ultima_l - 1  ).
              b_size = 1024 * ( ultima_l   ).


              CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
                EXPORTING
                  input_length = b_size
*                 FIRST_LINE   = 0
*                 LAST_LINE    = 0
                IMPORTING
                  buffer       = pdf
                TABLES
                  binary_tab   = lt_archivo[]
                EXCEPTIONS
                  failed       = 1
                  OTHERS       = 2.
              IF sy-subrc <> 0.
* Implement suitable error handling here
              ENDIF.

*          pdf = lt_archivo[].
*            DATA: lv_ruta_destino TYPE string.
*
*            CALL METHOD cl_gui_frontend_services=>get_temp_directory
*              CHANGING
*                temp_dir = lv_ruta_destino.
*
*            CALL METHOD cl_gui_cfw=>flush.
**
*            CHECK sy-subrc = 0 AND lv_ruta_destino IS NOT INITIAL.
*
*            DATA: lv_archivo_destino TYPE string.
*
**OT
*             CONCATENATE lv_ruta_destino '\' rut_emisor rut_receptor tipo_dte  xblnr  '.PDF' INTO lv_archivo_destino.
*            TRANSLATE lv_archivo_destino TO LOWER CASE.
*
*            DATA: lv_length TYPE i.
*            CALL METHOD cl_gui_frontend_services=>gui_download
*              EXPORTING
*                filename   = lv_archivo_destino
*                filetype   = 'BIN'
*              IMPORTING
*                filelength = lv_length
*              CHANGING
*                data_tab   = lt_archivo.
*
*            CHECK sy-subrc = 0.
*
*            CALL METHOD cl_gui_frontend_services=>execute
*              EXPORTING
*                document  = lv_archivo_destino
*                operation = 'OPEN'.
*
            ENDIF.
          ENDIF.
        CATCH cx_root INTO lv_exception.
          DATA: lv_message TYPE string.
          CLEAR lv_message.

          CALL METHOD lv_exception->if_message~get_text
            RECEIVING
              result = lv_message.
          IF sy-batch NE 'X'.
            MESSAGE lv_message TYPE 'I' DISPLAY LIKE 'E'.
          ENDIF.
      ENDTRY.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
