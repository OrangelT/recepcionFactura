*&---------------------------------------------------------------------*
*&  Include           ZDTE_PRECONTABILIZACION_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GENERAR_PRECONT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generar_precontnw.

  DATA: t_bstyp LIKE ekko-bstyp.
  DATA: t_pstyp LIKE ekpo-pstyp.
  DATA: lt_zcb_recfactprov TYPE STANDARD TABLE OF ztfi_0074 WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
  DATA: ls_recfactprov  TYPE   ztfi_0074 .
  DATA: ls_pos TYPE ztfi_0075.

  DATA: lt_ekko TYPE STANDARD TABLE OF ekko WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
  DATA: lt_lfb1_oc TYPE STANDARD TABLE OF lfb1 WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
  DATA: lt_ekpo TYPE STANDARD TABLE OF ekpo WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
  DATA: lt_ekkn TYPE STANDARD TABLE OF ekkn WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
  DATA: lt_afko TYPE STANDARD TABLE OF afko WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
  DATA: lt_proj TYPE STANDARD TABLE OF proj WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
  DATA: lt_ztfi_0090 TYPE STANDARD TABLE OF ztfi_0090 WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
  DATA: lt_ztfi_0093 TYPE STANDARD TABLE OF ztfi_0093 WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
  DATA: ls_ztfi_0089 TYPE ztfi_0089.
  DATA: lv_tabix     TYPE sytabix.
  DATA: wa_objectid  TYPE cdhdr-objectid.
  DATA: old_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074 WITH HEADER LINE WITH NON-UNIQUE DEFAULT KEY.
  DATA: new_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074 WITH HEADER LINE WITH NON-UNIQUE DEFAULT KEY.

  "---->selección de registros con estatus 1.
  SELECT *
  FROM ztfi_0074
  INTO TABLE lt_zcb_recfactprov
  WHERE bukrs IN s_bukrs AND
        xblnr IN s_folio AND
        belnr EQ '' AND
        status IN s_status.
  IF sy-dbcnt >= 1.
    " Bloqueos
*    LOOP AT lt_zcb_recfactprov.
*      lv_tabix = sy-tabix.
*
*
*      CALL FUNCTION 'ENQUEUE_EZTFI_0074'
*        EXPORTING
*          mode_ztfi_0074 = 'E'
*          mandt          = sy-mandt
*          bukrs          = lt_zcb_recfactprov-bukrs
*          xblnr          = lt_zcb_recfactprov-xblnr
*          lifnr          = lt_zcb_recfactprov-lifnr
*          tipodte        = lt_zcb_recfactprov-tipodte
**         X_BUKRS        = ' '
**         X_XBLNR        = ' '
**         X_LIFNR        = ' '
**         X_TIPODTE      = ' '
**         _SCOPE         = '2'
**         _WAIT          = ' '
**         _COLLECT       = ' '
*        EXCEPTIONS
*          foreign_lock   = 1
*          system_failure = 2
*          OTHERS         = 3.
*      IF sy-subrc NE 0.
** Implement suitable error handling here
*        ti_log-estado    = icon_red_light.
*        ti_log-documento = lt_zcb_recfactprov-xblnr.
*        ti_log-sociedad  = lt_zcb_recfactprov-bukrs.
*        ti_log-proveedor = lt_zcb_recfactprov-lifnr.
*        ti_log-fecha     = lt_zcb_recfactprov-bldat.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                                          WITH sy-msgv1 sy-msgv2
*                                               sy-msgv3 sy-msgv4
*                                          INTO ti_log-mensaje.
*        APPEND ti_log.
*
*        DELETE lt_zcb_recfactprov INDEX lv_tabix.
*      ELSE.
*        CLEAR ls_recfactprov.
*        "----> Releo el Registro bloqueado, para traer el ultimo valor
*        SELECT SINGLE * FROM ztfi_0074 INTO ls_recfactprov
*          WHERE bukrs EQ lt_zcb_recfactprov-bukrs AND
*                xblnr EQ lt_zcb_recfactprov-xblnr AND
*                tipodte  EQ lt_zcb_recfactprov-tipodte AND
*                lifnr EQ lt_zcb_recfactprov-lifnr.
*        lt_zcb_recfactprov =     ls_recfactprov.
*
*      ENDIF.
*    ENDLOOP.

    SORT lt_zcb_recfactprov BY bukrs tipodte xblnr lifnr bldat.
    DELETE ADJACENT DUPLICATES FROM lt_zcb_recfactprov.

    IF s_fecha[] IS NOT INITIAL.
      DELETE lt_zcb_recfactprov WHERE bldat NOT IN s_fecha.
    ENDIF.

    IF  lt_zcb_recfactprov[] IS INITIAL
    AND ti_log[] IS NOT INITIAL.
      PERFORM generar_alv.
      EXIT.
    ENDIF.

    DATA: lt_zdte_posfact TYPE STANDARD TABLE OF ztfi_0075 WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
    DATA: lt_zdte_posfact_oc TYPE STANDARD TABLE OF ztfi_0075 WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.

*cambio de lugr de busqueda por logica de bloqueo
*    SELECT *
*    FROM ztfi_0075
*    INTO TABLE lt_zdte_posfact
*    FOR ALL ENTRIES IN lt_zcb_recfactprov
*    WHERE bukrs = lt_zcb_recfactprov-bukrs AND
*          lifnr = lt_zcb_recfactprov-lifnr AND
*          xblnr = lt_zcb_recfactprov-xblnr AND
*          tipodte = lt_zcb_recfactprov-tipodte.
*    IF sy-dbcnt >= 1.
*      SORT lt_zdte_posfact BY ebeln.
*      DELETE ADJACENT DUPLICATES FROM lt_zdte_posfact.
*    ENDIF.


    DATA: lt_zdte_cldoc TYPE STANDARD TABLE OF ztfi_0001b WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
    SELECT *
    FROM ztfi_0001b
    INTO TABLE lt_zdte_cldoc
    FOR ALL ENTRIES IN lt_zcb_recfactprov
    WHERE bukrs = lt_zcb_recfactprov-bukrs.
    IF sy-dbcnt >= 1.
      SORT lt_zdte_cldoc BY bukrs.
      DELETE ADJACENT DUPLICATES FROM lt_zdte_cldoc.
    ENDIF.

    "----selección de referencias de factura.
    DATA: lt_zdt_recref TYPE STANDARD TABLE OF ztfi_0077 WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.

    SELECT *
    FROM ztfi_0077
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

    DATA: lt_t001z TYPE STANDARD TABLE OF t001z WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
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



    "---->selección de proveedores.

    DATA: lt_lfa1 TYPE STANDARD TABLE OF lfa1 WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.

    SELECT *
    FROM lfa1
    INTO TABLE lt_lfa1
    FOR ALL ENTRIES IN lt_zcb_recfactprov
    WHERE lifnr = lt_zcb_recfactprov-lifnr.
    IF sy-dbcnt >= 1.
      SORT lt_lfa1 BY lifnr.
      DELETE ADJACENT DUPLICATES FROM lt_lfa1.
    ENDIF.

    DATA: lt_lfb1 TYPE STANDARD TABLE OF lfb1 WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.

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


* Dias Permitidos por Sociedad

    DATA: lt_zdte_dias TYPE STANDARD TABLE OF ztfi_0076 WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.

    SELECT *
    FROM ztfi_0076
    INTO TABLE lt_zdte_dias
    FOR ALL ENTRIES IN lt_zcb_recfactprov
    WHERE bukrs EQ lt_zcb_recfactprov-bukrs.
    IF sy-dbcnt >= 1.
      SORT lt_zdte_dias BY bukrs.
      DELETE ADJACENT DUPLICATES FROM lt_zdte_dias.
    ENDIF.
*---> Tabla de variantes de validacion y datos de webservice.
    DATA: lt_zdte_valida TYPE STANDARD TABLE OF ztfi_0078 WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.

    SELECT *
    FROM ztfi_0078
    INTO TABLE lt_zdte_valida
    WHERE bukrs IN s_bukrs.
    IF sy-dbcnt >= 1.
      SORT lt_zdte_valida BY bukrs.
      DELETE ADJACENT DUPLICATES FROM lt_zdte_valida.
    ENDIF.


    "----->tabla imputación U.

    SELECT *
    FROM ztfi_0085
    INTO TABLE ti_ztfi_0085
    WHERE bukrs IN s_bukrs.
    IF sy-dbcnt >= 1.
      SORT ti_ztfi_0085 BY bukrs.
      DELETE ADJACENT DUPLICATES FROM ti_ztfi_0085.
    ENDIF.

    "----->sección pedido limite

    DATA: lt_ekko_lim TYPE STANDARD TABLE OF ekko WITH NON-UNIQUE DEFAULT KEY WITH  HEADER LINE.
    " Mejora del Performance
    DATA: rg_ekorg TYPE RANGE OF ekko-ekorg.
    DATA: rg_ekgrp TYPE RANGE OF ekko-ekgrp.
    DATA: rg_bedat TYPE RANGE OF ekko-bedat.
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
               AND kdatb <= lt_zcb_recfactprov-bldat
               AND kdate >= lt_zcb_recfactprov-bldat.
    IF sy-dbcnt >= 1.
      SORT lt_ekko_lim BY ebeln.
      DELETE ADJACENT DUPLICATES FROM lt_ekko_lim.

      DATA: lt_ekpo_lim TYPE STANDARD TABLE OF ekpo WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
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

        DATA: lt_ekkn_lim TYPE STANDARD TABLE OF ekkn WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.

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

            DATA: lt_afko_lim TYPE STANDARD TABLE OF afko WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.

            SELECT *
            FROM afko
            INTO TABLE lt_afko_lim
            FOR ALL ENTRIES IN lt_ekkn_lim
            WHERE aufnr = lt_ekkn_lim-nplnr.
            IF sy-dbcnt >= 1.
              SORT lt_afko_lim BY aufnr.
              DELETE ADJACENT DUPLICATES FROM lt_afko_lim.

              DATA: lt_proj_lim TYPE STANDARD TABLE OF proj WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
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
    "----->fin sección pedido limite
    "-----> Homologación de tipo de impuesto SII a código SAP
    SELECT * INTO TABLE lt_ztfi_0093
    FROM ztfi_0093.
    IF sy-subrc EQ 0.
    ENDIF.

    "-----> Otros Impuestos Facturas Procesadas
    IF lines( lt_zcb_recfactprov[] ) GT 0.
      SELECT * INTO TABLE lt_ztfi_0090
      FROM ztfi_0090
      FOR ALL ENTRIES IN lt_zcb_recfactprov
      WHERE bukrs   EQ lt_zcb_recfactprov-bukrs
        AND xblnr   EQ lt_zcb_recfactprov-xblnr
        AND lifnr   EQ lt_zcb_recfactprov-lifnr
        AND tipodte EQ lt_zcb_recfactprov-tipodte.
    ENDIF.


    DATA: lt_ekbe TYPE STANDARD TABLE OF ekbe WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
    "----->búsqueda de Hojas de Servicios.
    IF lt_zdt_recref[] IS NOT INITIAL.


      DATA: ti_essr      TYPE STANDARD TABLE OF zty_ess WITH HEADER LINE WITH NON-UNIQUE DEFAULT KEY,
            ti_ekko_essr TYPE STANDARD TABLE OF ekko WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
      READ TABLE lt_zdt_recref WITH KEY tiporef = 'HES'.
      IF sy-subrc EQ 0.

        DATA: r_lblni TYPE RANGE OF essr-lblni WITH HEADER LINE.

        CLEAR r_lblni.
        FREE r_lblni.

        r_lblni-sign = 'I'.
        r_lblni-option = 'EQ'.
        LOOP AT lt_zdt_recref WHERE tiporef = 'GES' OR
                                    tiporef = 'HES'.
          r_lblni-low = lt_zdt_recref-folioref.
          APPEND r_lblni.
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

            DATA: ti_ekpo_essr TYPE STANDARD TABLE OF ekpo WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
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





    SELECT *
    FROM vbwf16
    INTO TABLE ti_vbwf16
    FOR ALL ENTRIES IN lt_t001
    WHERE wfvar = lt_t001-wfvar.
    IF sy-dbcnt >= 1.
      SORT ti_vbwf16 BY wfvar blart.
      DELETE ADJACENT DUPLICATES FROM ti_vbwf16.
    ENDIF.
    DATA: s_tipodte LIKE lfb1-zdte_tipo.
    DATA: lv_registro LIKE sy-tabix.
    DATA: lv_registro_pos LIKE sy-tabix.
    DATA: lv_flag_lfb1_oc TYPE c.
    DATA: lv_oc_limite TYPE ekko-ebeln,
          lv_lim_lifnr TYPE ekko-lifnr,
          lv_lim_bukrs TYPE ekko-bukrs.

    DATA: wa_lt_ztfi_0075 LIKE LINE OF lt_zdte_posfact.
    DATA msgid   TYPE msgid .
    DATA msgty   TYPE msgty .
    DATA msgno   TYPE msgno .
    DATA msgv1   TYPE msgv1 .
    DATA msgv2   TYPE msgv2 .
    DATA msgv3   TYPE msgv3 .
    DATA msgv4   TYPE msgv4 .
    DATA estatus TYPE char01.
    DATA e_repid TYPE repid.


    LOOP AT lt_zcb_recfactprov.
      "____> Bloqueo de registro.
      CALL FUNCTION 'ENQUEUE_EZTFI_0074'
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
*         _SCOPE         = '2'
*         _WAIT          = ' '
*         _COLLECT       = ' '
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      IF sy-subrc NE 0.
* Implement suitable error handling here
        ti_log-estado    = icon_red_light.
        ti_log-documento = lt_zcb_recfactprov-xblnr.
        ti_log-sociedad  = lt_zcb_recfactprov-bukrs.
        ti_log-proveedor = lt_zcb_recfactprov-lifnr.
        ti_log-fecha     = lt_zcb_recfactprov-bldat.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                                          WITH sy-msgv1 sy-msgv2
                                               sy-msgv3 sy-msgv4
                                          INTO ti_log-mensaje.
        APPEND ti_log.

*       DELETE lt_zcb_recfactprov INDEX lv_tabix.
        CONTINUE.
      ELSE.
        CLEAR ls_recfactprov.CLEAR ls_pos.
        "----> Releo el Registro bloqueado, para traer el ultimo valor
        SELECT SINGLE * FROM ztfi_0074 INTO ls_recfactprov
          WHERE bukrs EQ lt_zcb_recfactprov-bukrs AND
                xblnr EQ lt_zcb_recfactprov-xblnr AND
                tipodte  EQ lt_zcb_recfactprov-tipodte AND
                lifnr EQ lt_zcb_recfactprov-lifnr AND
                belnr EQ '' AND
                status IN s_status.
        IF sy-subrc = 0 . "Busco si el Doc sigue sin Precontabilizar
          "----> Entrego ultimo valor a linea
          lt_zcb_recfactprov =     ls_recfactprov.
          SELECT * FROM ztfi_0075 INTO TABLE lt_zdte_posfact
          WHERE bukrs = lt_zcb_recfactprov-bukrs AND
                lifnr = lt_zcb_recfactprov-lifnr AND
                xblnr = lt_zcb_recfactprov-xblnr AND
                tipodte = lt_zcb_recfactprov-tipodte.
          IF sy-dbcnt >= 1.
            SORT lt_zdte_posfact BY ebeln.
            DELETE ADJACENT DUPLICATES FROM lt_zdte_posfact.
          ENDIF.
        ELSE.
          CALL FUNCTION 'DEQUEUE_EZTFI_0074'
            EXPORTING
              mode_ztfi_0074 = 'E'
              mandt          = sy-mandt
              bukrs          = lt_zcb_recfactprov-bukrs
              xblnr          = lt_zcb_recfactprov-xblnr
              lifnr          = lt_zcb_recfactprov-lifnr
              tipodte        = lt_zcb_recfactprov-tipodte
*             X_BUKRS        = ' '
*             X_XBLNR        = ' '
*             X_LIFNR        = ' '
*             X_TIPODTE      = ' '
*             _SCOPE         = '3'
*             _SYNCHRON      = ' '
*             _COLLECT       = ' '
            .
          ti_log-estado    = icon_red_light.
          ti_log-documento = lt_zcb_recfactprov-xblnr.
          ti_log-sociedad  = lt_zcb_recfactprov-bukrs.
          ti_log-proveedor = lt_zcb_recfactprov-lifnr.
          ti_log-fecha     = lt_zcb_recfactprov-bldat.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                                            WITH sy-msgv1 sy-msgv2
                                                 sy-msgv3 sy-msgv4
                                            INTO ti_log-mensaje.

          APPEND ti_log.
*        DELETE lt_zcb_recfactprov INDEX lv_tabix.
          CONTINUE.
        ENDIF.
      ENDIF.


      lv_registro = sy-tabix.
      CLEAR ti_log. CLEAR s_tipodte.
      ti_log-documento = lt_zcb_recfactprov-xblnr.
      ti_log-sociedad = lt_zcb_recfactprov-bukrs.
      ti_log-proveedor = lt_zcb_recfactprov-lifnr.
      ti_log-fecha = lt_zcb_recfactprov-bldat.
*OT Primero preguntar por Proveedor
      READ TABLE lt_lfa1 WITH KEY lifnr = lt_zcb_recfactprov-lifnr.
      IF sy-subrc EQ 0.
*OT Busco el tipo_proveedor X sociedad
        READ TABLE lt_lfb1 WITH KEY lifnr = lt_lfa1-lifnr
                                    bukrs = lt_zcb_recfactprov-bukrs.
        IF sy-subrc NE 0.
          ti_log-estado = icon_red_light.
          MESSAGE e000(zfi_0003) WITH lt_zcb_recfactprov-bukrs  INTO ti_log-mensaje.
          PERFORM generar_rechazo TABLES lt_t001z  lt_lfa1 USING  ti_log-mensaje '000' CHANGING lt_zcb_recfactprov.
          APPEND ti_log.
          MODIFY lt_zcb_recfactprov INDEX lv_registro.
          CONTINUE.

        ENDIF.
      ELSE.
        ti_log-estado = icon_red_light.
        MESSAGE e000(zfi_0003) WITH lt_zcb_recfactprov-bukrs  INTO ti_log-mensaje.
        PERFORM generar_rechazo TABLES lt_t001z  lt_lfa1 USING  ti_log-mensaje '000' CHANGING lt_zcb_recfactprov.
        APPEND ti_log.
        MODIFY lt_zcb_recfactprov INDEX lv_registro.
        CONTINUE.
      ENDIF.
*&--------------------------------------------------------------------------
* OT Sustitucion
* Se realiza sustitucion de OC y Posicion de OC
* SI el proveedor es 1  ; OC inicial -> HES -> Busca OC SAP
* SI el Proveedor es 3  ; OC incial  -> Busca  Adicional HES ->Pedido limite
*&--------------------------------------------------------------------------

      IF lt_lfb1-zdte_tipo = '1'. "Proveedor tipo Orden de compra
        IF lt_zcb_recfactprov-ebeln IS INITIAL.
*Buscar si hay HES, y de alli derivar la OC y Posicion OC.
          READ TABLE lt_zdt_recref WITH KEY  bukrs = lt_zcb_recfactprov-bukrs
                                         xblnr = lt_zcb_recfactprov-xblnr
                                         lifnr = lt_zcb_recfactprov-lifnr
                                         tipodte = lt_zcb_recfactprov-tipodte
                                         tiporef = 'HES'.
          IF sy-subrc = 0.
            READ TABLE ti_essr WITH KEY lblni = lt_zdt_recref-folioref.
            IF sy-subrc = 0.
              lt_zcb_recfactprov-ebeln = ti_essr-ebeln.
              lt_lfb1-zdte_tipo = '1'. "cambio de 1 a 3, para que entre a validacion OC(1)
              MODIFY lt_zcb_recfactprov INDEX lv_registro.
              READ TABLE lt_zdte_posfact WITH KEY  bukrs = lt_zcb_recfactprov-bukrs
                                         xblnr = lt_zcb_recfactprov-xblnr
                                         lifnr = lt_zcb_recfactprov-lifnr
                                         tipodte =  lt_zcb_recfactprov-tipodte.

              lt_zdte_posfact-ebeln = ti_essr-ebeln.
              lt_zdte_posfact-ebelp = ti_essr-ebelp.
              MODIFY lt_zdte_posfact FROM lt_zdte_posfact  TRANSPORTING ebeln ebelp WHERE  bukrs = lt_zcb_recfactprov-bukrs
                                        AND xblnr = lt_zcb_recfactprov-xblnr
                                        AND  lifnr = lt_zcb_recfactprov-lifnr
                                        AND tipodte =  lt_zcb_recfactprov-tipodte.
            ENDIF.
          ENDIF.
        ELSE. "Si ya se tiene OC, para el tipo 3, se verifica si se validarra como OC normal o OC pedido limite
          CLEAR t_bstyp.
          SELECT SINGLE bstyp INTO t_bstyp FROM ekko WHERE  ebeln = lt_zcb_recfactprov-ebeln
                                      AND  bstyp = 'F'.

          IF sy-subrc = 0.

          ELSE. "Si no encuentra ese doc como Pedido en SAP, que valide como tipo 1.

            READ TABLE lt_zdt_recref WITH KEY  bukrs = lt_zcb_recfactprov-bukrs
                        xblnr = lt_zcb_recfactprov-xblnr
                        lifnr = lt_zcb_recfactprov-lifnr
                        tipodte = lt_zcb_recfactprov-tipodte
                        tiporef = 'HES'.
            IF sy-subrc = 0.
              READ TABLE ti_essr WITH KEY lblni = lt_zdt_recref-folioref.
              IF sy-subrc = 0.
                lt_zcb_recfactprov-ebeln = ti_essr-ebeln.

                lt_lfb1-zdte_tipo = '1'. "cambio de 1 a 3, para que entre a validacion OC(1)
                MODIFY lt_zcb_recfactprov INDEX lv_registro.
                READ TABLE lt_zdte_posfact WITH KEY  bukrs = lt_zcb_recfactprov-bukrs
                                                         xblnr = lt_zcb_recfactprov-xblnr
                                                         lifnr = lt_zcb_recfactprov-lifnr
                                                         tipodte = lt_zcb_recfactprov-tipodte.

                lt_zdte_posfact-ebeln = ti_essr-ebeln.
                lt_zdte_posfact-ebelp = ti_essr-ebelp.
                MODIFY lt_zdte_posfact FROM lt_zdte_posfact  TRANSPORTING ebeln ebelp WHERE  bukrs = lt_zcb_recfactprov-bukrs
                                          AND xblnr = lt_zcb_recfactprov-xblnr
                                          AND  lifnr = lt_zcb_recfactprov-lifnr
                                          AND tipodte = lt_zcb_recfactprov-tipodte.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      IF lt_lfb1-zdte_tipo = '3'. "Proveedor tipo MIXTO
        IF lt_zcb_recfactprov-ebeln IS INITIAL.
*Buscar si hay HES, y de alli derivar la OC y Posicion OC.
          READ TABLE lt_zdt_recref WITH KEY  bukrs = lt_zcb_recfactprov-bukrs
                                         xblnr = lt_zcb_recfactprov-xblnr
                                         lifnr = lt_zcb_recfactprov-lifnr
                                         tipodte = lt_zcb_recfactprov-tipodte
                                         tiporef = 'HES'.
          IF sy-subrc = 0.
            READ TABLE ti_essr WITH KEY lblni = lt_zdt_recref-folioref.
            IF sy-subrc = 0.
              lt_zcb_recfactprov-ebeln = ti_essr-ebeln.
              lt_lfb1-zdte_tipo = '1'. "cambio de 1 a 3, para que entre a validacion OC(1)
              MODIFY lt_zcb_recfactprov INDEX lv_registro.
              READ TABLE lt_zdte_posfact WITH KEY  bukrs = lt_zcb_recfactprov-bukrs
                                         xblnr = lt_zcb_recfactprov-xblnr
                                         lifnr = lt_zcb_recfactprov-lifnr
                                         tipodte = lt_zcb_recfactprov-tipodte.

              lt_zdte_posfact-ebeln = ti_essr-ebeln.
              lt_zdte_posfact-ebelp = ti_essr-ebelp.
              MODIFY lt_zdte_posfact FROM lt_zdte_posfact  TRANSPORTING ebeln ebelp WHERE  bukrs = lt_zcb_recfactprov-bukrs
                                        AND xblnr = lt_zcb_recfactprov-xblnr
                                        AND  lifnr = lt_zcb_recfactprov-lifnr
                                        AND tipodte = lt_zcb_recfactprov-tipodte.

            ELSE. "valor Hes enviado No existe en SAP
              IF lt_lfb1-zdte_tipo = '3'. "SI es 3, busco adicionalmente Pedido limite
                LOOP AT lt_ekko_lim WHERE kdatb <= lt_zcb_recfactprov-bldat
                                                   AND kdate >= lt_zcb_recfactprov-bldat
                                                   AND lifnr EQ lt_zcb_recfactprov-lifnr.
                  READ TABLE lt_ekpo_lim WITH KEY ebeln = lt_ekko_lim-ebeln.
                  IF sy-subrc = 0.
                    lt_zcb_recfactprov-ebeln = lt_ekko_lim-ebeln.
                    MODIFY lt_zcb_recfactprov INDEX lv_registro.
                    READ TABLE lt_zdte_posfact WITH KEY  bukrs = lt_zcb_recfactprov-bukrs
                                                             xblnr = lt_zcb_recfactprov-xblnr
                                                             lifnr = lt_zcb_recfactprov-lifnr
                                                             tipodte = lt_zcb_recfactprov-tipodte.

                    lt_zdte_posfact-ebeln = lt_ekpo_lim-ebeln.
                    lt_zdte_posfact-ebelp = lt_ekpo_lim-ebelp.
                    MODIFY lt_zdte_posfact FROM lt_zdte_posfact  TRANSPORTING ebeln ebelp WHERE  bukrs = lt_zcb_recfactprov-bukrs
                                              AND xblnr = lt_zcb_recfactprov-xblnr
                                              AND  lifnr = lt_zcb_recfactprov-lifnr
                                              AND tipodte = lt_zcb_recfactprov-tipodte.
                  ENDIF.
                ENDLOOP.
              ENDIF.
            ENDIF.
          ELSE.
            IF lt_lfb1-zdte_tipo = '3'. "SI es 3, busco adicionalmente Pedido limite
              LOOP AT lt_ekko_lim WHERE kdatb <= lt_zcb_recfactprov-bldat
                                                 AND kdate >= lt_zcb_recfactprov-bldat
                                                 AND lifnr EQ lt_zcb_recfactprov-lifnr.
                READ TABLE lt_ekpo_lim WITH KEY ebeln = lt_ekko_lim-ebeln.
                IF sy-subrc = 0.
                  lt_zcb_recfactprov-ebeln = lt_ekko_lim-ebeln.

                  MODIFY lt_zcb_recfactprov INDEX lv_registro.
                  READ TABLE lt_zdte_posfact WITH KEY  bukrs = lt_zcb_recfactprov-bukrs
                                                           xblnr = lt_zcb_recfactprov-xblnr
                                                           lifnr = lt_zcb_recfactprov-lifnr
                                                           tipodte = lt_zcb_recfactprov-tipodte.

                  lt_zdte_posfact-ebeln = lt_ekpo_lim-ebeln.
                  lt_zdte_posfact-ebelp = lt_ekpo_lim-ebelp.
                  MODIFY lt_zdte_posfact FROM lt_zdte_posfact  TRANSPORTING ebeln ebelp WHERE  bukrs = lt_zcb_recfactprov-bukrs
                                            AND xblnr = lt_zcb_recfactprov-xblnr
                                            AND  lifnr = lt_zcb_recfactprov-lifnr
                                            AND tipodte = lt_zcb_recfactprov-tipodte.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ELSE. "Si ya se tiene OC, para el tipo 3, se verifica si se validarra como OC normal o OC pedido limite
          CLEAR t_bstyp.
          SELECT SINGLE bstyp INTO t_bstyp FROM ekko WHERE  ebeln = lt_zcb_recfactprov-ebeln
                                      AND  bstyp = 'F'.

          IF sy-subrc = 0.
            CLEAR  t_pstyp.
            SELECT SINGLE  pstyp INTO t_pstyp FROM ekpo WHERE ebeln = lt_zcb_recfactprov-ebeln
                                       AND  pstyp = '1'.

            IF t_pstyp NE '1'. "No es Pedido limite
              lt_lfb1-zdte_tipo = '1'.
            ELSE.
              lt_lfb1-zdte_tipo = '3'.
            ENDIF.
          ELSE. "Si no encuentra ese doc como Pedido en SAP, que valide como tipo 1.
*            lt_lfb1-zdte_tipo = '1'.
            READ TABLE lt_zdt_recref WITH KEY  bukrs = lt_zcb_recfactprov-bukrs
                        xblnr = lt_zcb_recfactprov-xblnr
                        lifnr = lt_zcb_recfactprov-lifnr
                        tipodte = lt_zcb_recfactprov-tipodte
                        tiporef = 'HES'.
            IF sy-subrc = 0.
              READ TABLE ti_essr WITH KEY lblni = lt_zdt_recref-folioref.
              IF sy-subrc = 0.
                lt_zcb_recfactprov-ebeln = ti_essr-ebeln.

                lt_lfb1-zdte_tipo = '1'. "cambio de 1 a 3, para que entre a validacion OC(1)
                MODIFY lt_zcb_recfactprov INDEX lv_registro.
                READ TABLE lt_zdte_posfact WITH KEY  bukrs = lt_zcb_recfactprov-bukrs
                                                         xblnr = lt_zcb_recfactprov-xblnr
                                                         lifnr = lt_zcb_recfactprov-lifnr
                                                         tipodte = lt_zcb_recfactprov-tipodte.

                lt_zdte_posfact-ebeln = ti_essr-ebeln.
                lt_zdte_posfact-ebelp = ti_essr-ebelp.
                MODIFY lt_zdte_posfact FROM lt_zdte_posfact  TRANSPORTING ebeln ebelp WHERE  bukrs = lt_zcb_recfactprov-bukrs
                                          AND xblnr = lt_zcb_recfactprov-xblnr
                                          AND  lifnr = lt_zcb_recfactprov-lifnr
                                          AND tipodte = lt_zcb_recfactprov-tipodte.
              ELSE. "valor Hes enviado No existe en SAP
                IF lt_lfb1-zdte_tipo = '3'. "SI es 3, busco adicionalmente Pedido limite
                  LOOP AT lt_ekko_lim WHERE kdatb <= lt_zcb_recfactprov-bldat
                                                     AND kdate >= lt_zcb_recfactprov-bldat
                                                     AND lifnr EQ lt_zcb_recfactprov-lifnr.
                    READ TABLE lt_ekpo_lim WITH KEY ebeln = lt_ekko_lim-ebeln.
                    IF sy-subrc = 0.
                      lt_zcb_recfactprov-ebeln = lt_ekko_lim-ebeln.

                      MODIFY lt_zcb_recfactprov INDEX lv_registro.
                      READ TABLE lt_zdte_posfact WITH KEY  bukrs = lt_zcb_recfactprov-bukrs
                                                               xblnr = lt_zcb_recfactprov-xblnr
                                                               lifnr = lt_zcb_recfactprov-lifnr
                                                               tipodte = lt_zcb_recfactprov-tipodte.

                      lt_zdte_posfact-ebeln = lt_ekpo_lim-ebeln.
                      lt_zdte_posfact-ebelp = lt_ekpo_lim-ebelp.
                      MODIFY lt_zdte_posfact FROM lt_zdte_posfact  TRANSPORTING ebeln ebelp WHERE  bukrs = lt_zcb_recfactprov-bukrs
                                                AND xblnr = lt_zcb_recfactprov-xblnr
                                                AND  lifnr = lt_zcb_recfactprov-lifnr
                                                AND tipodte = lt_zcb_recfactprov-tipodte.
                    ENDIF.
                  ENDLOOP.
                ENDIF.
              ENDIF.
            ELSE.
              IF lt_lfb1-zdte_tipo = '3'. "SI es 3, busco adicionalmente Pedido limite
                LOOP AT lt_ekko_lim WHERE kdatb <= lt_zcb_recfactprov-bldat
                                                   AND kdate >= lt_zcb_recfactprov-bldat
                                                   AND lifnr EQ lt_zcb_recfactprov-lifnr.
                  READ TABLE lt_ekpo_lim WITH KEY ebeln = lt_ekko_lim-ebeln.
                  IF sy-subrc = 0.
                    lt_zcb_recfactprov-ebeln = lt_ekko_lim-ebeln.

                    MODIFY lt_zcb_recfactprov INDEX lv_registro.
                    READ TABLE lt_zdte_posfact WITH KEY  bukrs = lt_zcb_recfactprov-bukrs
                                                             xblnr = lt_zcb_recfactprov-xblnr
                                                             lifnr = lt_zcb_recfactprov-lifnr
                                                             tipodte = lt_zcb_recfactprov-tipodte.

                    lt_zdte_posfact-ebeln = lt_ekpo_lim-ebeln.
                    lt_zdte_posfact-ebelp = lt_ekpo_lim-ebelp.
                    MODIFY lt_zdte_posfact FROM lt_zdte_posfact  TRANSPORTING ebeln ebelp WHERE  bukrs = lt_zcb_recfactprov-bukrs
                                              AND xblnr = lt_zcb_recfactprov-xblnr
                                              AND  lifnr = lt_zcb_recfactprov-lifnr
                                              AND tipodte = lt_zcb_recfactprov-tipodte.
                  ENDIF.
                ENDLOOP.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.
*Sustitucion de Posicion OC.
*      CLEAR lv_registro_pos.
*      LOOP AT lt_zdte_posfact WHERE ebeln = lt_zcb_recfactprov-ebeln.
*        lv_registro_pos = sy-tabix.
*        IF lt_zdte_posfact-ebelp IS INITIAL.
*          READ TABLE  lt_zdt_recref WITH KEY  bukrs = lt_zcb_recfactprov-bukrs
*                                              xblnr = lt_zcb_recfactprov-xblnr
*                                              lifnr = lt_zcb_recfactprov-lifnr
*                                              tipodte = lt_zcb_recfactprov-tipodte
*                                              tiporef = 'HES'.
*          IF sy-subrc = 0.
*            READ TABLE ti_essr WITH KEY lblni = lt_zdt_recref-folioref.
*            IF sy-subrc = 0.
*              lt_zdte_posfact-ebelp = ti_essr-ebelp.
*              MODIFY lt_zdte_posfact INDEX lv_registro_pos.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
      CLEAR lv_registro_pos.
      CLEAR lt_zdte_posfact_oc. REFRESH lt_zdte_posfact_oc.
      LOOP AT lt_zdte_posfact WHERE ebeln = lt_zcb_recfactprov-ebeln.
        lv_registro_pos = sy-tabix.
        IF lt_zdte_posfact-ebelp IS INITIAL.
          READ TABLE  lt_zdt_recref WITH KEY  bukrs = lt_zcb_recfactprov-bukrs
                                              xblnr = lt_zcb_recfactprov-xblnr
                                              lifnr = lt_zcb_recfactprov-lifnr
                                              tiporef = 'HES'.
          IF sy-subrc = 0.
            READ TABLE ti_essr WITH KEY lblni = lt_zdt_recref-folioref.
            IF sy-subrc = 0.
              lt_zdte_posfact-ebelp = ti_essr-ebelp.
              MODIFY lt_zdte_posfact INDEX lv_registro_pos .
*Solo OC y/o Pos del documneto procesado.
              APPEND lt_zdte_posfact TO lt_zdte_posfact_oc.
            ENDIF.
          ENDIF.
        ELSE.
*Solo OC y/o Pos del documneto procesado.
          APPEND lt_zdte_posfact TO lt_zdte_posfact_oc.
        ENDIF.
      ENDLOOP.
*OT Fin de Sustitucion
**Valido si lt_zdte_posfact_oc tiene posiciones.
      IF    lt_zcb_recfactprov-ebeln IS NOT INITIAL .

        IF  lt_zdte_posfact_oc[] IS INITIAL .
          CLEAR lt_zdte_posfact_oc.
          SELECT *  FROM ekpo INTO TABLE lt_ekpo
             WHERE ebeln EQ lt_zcb_recfactprov-ebeln.
          IF sy-subrc = 0.
            LOOP AT lt_ekpo ASSIGNING FIELD-SYMBOL(<f1>).
              lt_zdte_posfact_oc-mandt = lt_zcb_recfactprov-mandt.
              lt_zdte_posfact_oc-bukrs = lt_zcb_recfactprov-bukrs.
              lt_zdte_posfact_oc-xblnr = lt_zcb_recfactprov-xblnr.
              lt_zdte_posfact_oc-lifnr = lt_zcb_recfactprov-lifnr.
              lt_zdte_posfact_oc-tipodte = lt_zcb_recfactprov-tipodte.
              lt_zdte_posfact_oc-ebelp = <f1>-ebelp.
              lt_zdte_posfact_oc-ebeln = lt_zcb_recfactprov-ebeln.
              APPEND lt_zdte_posfact_oc.
            ENDLOOP.
          ENDIF.
        ELSE.

        ENDIF.
      ENDIF.

*OT Fin de Sustitucion
*---->  Asigno tipo DTE a valiables por el tipo mixto que es 1 y 3
      s_tipodte = lt_lfb1-zdte_tipo.

*    DATA: lt_ekko TYPE STANDARD TABLE OF ekko WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
      IF lt_zcb_recfactprov-ebeln IS NOT INITIAL.
        SELECT *
        FROM ekko
        INTO TABLE lt_ekko
*OT        FOR ALL ENTRIES IN lt_zcb_recfactprov
        WHERE ebeln = lt_zcb_recfactprov-ebeln.
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


*OT Validaciones

      CLEAR msgid.
      CLEAR msgty.
      CLEAR msgno.
      CLEAR msgv1.
      CLEAR msgv2.
      CLEAR msgv3.
      CLEAR msgv4.
      CLEAR estatus.
      CLEAR e_repid.
      READ TABLE lt_zdte_valida WITH  KEY bukrs = lt_zcb_recfactprov-bukrs.
      IF sy-subrc EQ 0.

        e_repid  =  lt_zdte_valida-varvalpre.
        IF e_repid IS NOT INITIAL.

          CALL FUNCTION 'ZFI_0030'
            EXPORTING
              lt_zcb_recfactprov = lt_zcb_recfactprov
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
              msgno              = msgno
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
        ELSE.
          estatus = '7'.
          msgid = 'ZDTE_0001'.
          msgty = 'E'.
          msgno = '012'.
          msgv1 = TEXT-406. msgv2 = lt_zcb_recfactprov-bukrs . msgv3 = TEXT-407. msgv4 = ''.
        ENDIF.
      ELSE.
        estatus = '7'.
        msgid = 'ZDTE_0001'.
        msgty = 'E'.
        msgno = '012'.
        msgv1 = TEXT-406. msgv2 = lt_zcb_recfactprov-bukrs . msgv3 = TEXT-407. msgv4 = ''.
      ENDIF.



      IF  s_tipodte EQ '1'. "Proveedor Siempre Con OC x MM-MIRA

*OT Envio BAPI
        IF estatus EQ '2' OR estatus EQ '3'.
          lt_zcb_recfactprov-status = estatus.
          PERFORM generar_mira TABLES   lt_ekko
                                        lt_ekpo
                                        lt_zdte_posfact_oc "lt_zdte_posfact
                                        lt_ekkn
                                        lt_afko
                                        lt_proj
                                        lt_zdte_cldoc
                               USING    s_tipodte estatus
                               CHANGING lt_zcb_recfactprov.
          MODIFY lt_zcb_recfactprov INDEX lv_registro.
        ELSE.
          lt_zcb_recfactprov-status = estatus.
          ti_log-estado = icon_red_light.
          MESSAGE ID msgid TYPE msgty NUMBER msgno
          WITH msgv1 msgv2 msgv3 msgv4  INTO ti_log-mensaje.
          APPEND ti_log.
          IF estatus = '6'.
            PERFORM generar_rechazo TABLES lt_t001z  lt_lfa1 USING  ti_log-mensaje '000' CHANGING lt_zcb_recfactprov.
            PERFORM generar_log USING lt_zcb_recfactprov msgty msgid msgno msgv1 msgv2 msgv3 msgv4.
          ELSE.
            PERFORM generar_log USING lt_zcb_recfactprov msgty msgid msgno msgv1 msgv2 msgv3 msgv4.
          ENDIF.
          MODIFY lt_zcb_recfactprov INDEX lv_registro.

        ENDIF.

      ELSEIF  s_tipodte EQ '2'. "Proveedor Sin OC x FI-FBV1
*OT Envio BAPI
        IF  estatus EQ '2' OR estatus EQ '3'.
          lt_zcb_recfactprov-status = estatus.
*          PERFORM generar_fbv1 TABLES lt_zdte_cldoc  lt_ekpo lt_ekkn lt_afko lt_proj  USING s_tipoDTE
*                                   CHANGING lt_zcb_recfactprov.
          PERFORM generar_fbv1_auto TABLES   lt_ekpo
                                                   lt_zdte_posfact
                                                   lt_ekkn
                                                   lt_afko
                                                   lt_proj
                                                   lt_lfb1
                                                   lt_zdte_cldoc
                                                   lt_ztfi_0090
*                                                   lt_ztfi_0092
                                                   lt_ztfi_0093
                                          USING    s_tipodte estatus
                                          CHANGING lt_zcb_recfactprov.
          MODIFY lt_zcb_recfactprov INDEX lv_registro.
        ELSE.
          lt_zcb_recfactprov-status = estatus.
          ti_log-estado = icon_red_light.
          MESSAGE ID msgid TYPE msgty NUMBER msgno
          WITH msgv1 msgv2 msgv3 msgv4 INTO ti_log-mensaje.
          APPEND ti_log.
          IF estatus = '6'.
            PERFORM generar_rechazo TABLES lt_t001z  lt_lfa1 USING  ti_log-mensaje '000' CHANGING lt_zcb_recfactprov.
            PERFORM generar_log USING lt_zcb_recfactprov msgty msgid msgno msgv1 msgv2 msgv3 msgv4.
          ELSE.
            PERFORM generar_log USING lt_zcb_recfactprov msgty msgid msgno msgv1 msgv2 msgv3 msgv4.
          ENDIF.
          MODIFY lt_zcb_recfactprov INDEX lv_registro.
        ENDIF.

      ELSEIF  s_tipodte EQ '3'. "Proveedor por Pedido limite MM-MIR7
*OT Envio BAPI
        IF  estatus EQ '2' OR estatus EQ '3'.
          lt_zcb_recfactprov-status = estatus.
          PERFORM generar_mir7 TABLES   lt_zdte_cldoc
                                        lt_ekko_lim
                                        lt_ekpo_lim
                                        lt_ekkn_lim
                                        lt_afko_lim
                                        lt_proj_lim
                                 USING  lt_zcb_recfactprov-ebeln
                               CHANGING lt_zcb_recfactprov.
          MODIFY lt_zcb_recfactprov INDEX lv_registro.
        ELSE.
          lt_zcb_recfactprov-status = estatus.
          ti_log-estado = icon_red_light.
          MESSAGE ID msgid TYPE msgty NUMBER msgno
          WITH msgv1 msgv2 msgv3 msgv4  INTO ti_log-mensaje.
          APPEND ti_log.
          IF estatus = '6'.
            PERFORM generar_rechazo TABLES lt_t001z  lt_lfa1 USING  ti_log-mensaje '000' CHANGING lt_zcb_recfactprov.
            PERFORM generar_log USING lt_zcb_recfactprov msgty msgid msgno msgv1 msgv2 msgv3 msgv4.
          ELSE.
            PERFORM generar_log USING lt_zcb_recfactprov msgty msgid msgno msgv1 msgv2 msgv3 msgv4.
          ENDIF.
          MODIFY lt_zcb_recfactprov INDEX lv_registro.

        ENDIF.


      ELSEIF  s_tipodte EQ '4'. "Proveedor OC  costos ind x MM-MIRA

*OT Envio BAPI
        IF estatus EQ '2' OR estatus EQ '3'.
          lt_zcb_recfactprov-status = estatus.
          PERFORM generar_mira TABLES   lt_ekko
                                        lt_ekpo
                                        lt_zdte_posfact_oc "lt_zdte_posfact
                                        lt_ekkn
                                        lt_afko
                                        lt_proj
                                        lt_zdte_cldoc
                               USING    s_tipodte estatus
                               CHANGING lt_zcb_recfactprov.
          MODIFY lt_zcb_recfactprov INDEX lv_registro.
        ELSE.
          lt_zcb_recfactprov-status = estatus.
          ti_log-estado = icon_red_light.
          MESSAGE ID msgid TYPE msgty NUMBER msgno
          WITH msgv1 msgv2 msgv3 msgv4  INTO ti_log-mensaje.
          APPEND ti_log.
          IF estatus = '6'.
            PERFORM generar_rechazo TABLES lt_t001z  lt_lfa1 USING  ti_log-mensaje '000' CHANGING lt_zcb_recfactprov.
            PERFORM generar_log USING lt_zcb_recfactprov msgty msgid msgno msgv1 msgv2 msgv3 msgv4.
          ELSE.
            PERFORM generar_log USING lt_zcb_recfactprov msgty msgid msgno msgv1 msgv2 msgv3 msgv4.
          ENDIF.
          MODIFY lt_zcb_recfactprov INDEX lv_registro.

        ENDIF.
      ELSE.

*Proveedor no clasificado 1,2,3,4.
        lt_zcb_recfactprov-status = estatus.
        ti_log-estado = icon_red_light.
        MESSAGE ID msgid TYPE msgty NUMBER msgno
        WITH msgv1 msgv2 msgv3 msgv4  INTO ti_log-mensaje.
        APPEND ti_log.
        PERFORM generar_log USING lt_zcb_recfactprov  msgty msgid msgno msgv1 msgv2 msgv3 msgv4.
        MODIFY lt_zcb_recfactprov INDEX lv_registro.

      ENDIF.
*Ini.Log de Modificaciones Status (Pre-Contab)

      FREE old_ztfi_0074.
      CLEAR old_ztfi_0074.
      FREE new_ztfi_0074.
      CLEAR new_ztfi_0074.
      wa_objectid = lt_zcb_recfactprov+0(36).
      CLEAR: old_ztfi_0074[], new_ztfi_0074[].
      APPEND lt_zcb_recfactprov TO new_ztfi_0074.
      SELECT * FROM ztfi_0074 INTO TABLE old_ztfi_0074
               WHERE bukrs EQ lt_zcb_recfactprov-bukrs
                 AND xblnr EQ lt_zcb_recfactprov-xblnr
                 AND lifnr EQ lt_zcb_recfactprov-lifnr
                 AND tipodte EQ lt_zcb_recfactprov-tipodte.
      IF sy-subrc EQ 0.
        READ TABLE old_ztfi_0074 INDEX 1.
        IF lt_zcb_recfactprov-status NE old_ztfi_0074-status OR
           lt_zcb_recfactprov-belnr  NE old_ztfi_0074-belnr OR
           lt_zcb_recfactprov-gjahr  NE old_ztfi_0074-gjahr OR
           lt_zcb_recfactprov-aprobador_real NE old_ztfi_0074-aprobador_real.
          CALL FUNCTION 'ZTFI_0074_WRITE_DOCUMENT'
            IN UPDATE TASK
            EXPORTING
              objectid      = wa_objectid
              tcode         = 'ZDTE_001'
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
      UPDATE ztfi_0074 FROM lt_zcb_recfactprov.
      COMMIT WORK AND WAIT.
      UPDATE ztfi_0075 FROM lt_zdte_posfact.
      COMMIT WORK AND WAIT.
*FIn. Grabar Tablas
*Ini.Desbloqueo
      CALL FUNCTION 'DEQUEUE_EZTFI_0074'
        EXPORTING
          mode_ztfi_0074 = 'E'
          mandt          = sy-mandt
          bukrs          = lt_zcb_recfactprov-bukrs
          xblnr          = lt_zcb_recfactprov-xblnr
          lifnr          = lt_zcb_recfactprov-lifnr
          tipodte        = lt_zcb_recfactprov-tipodte
**         X_BUKRS        = ' '
**         X_XBLNR        = ' '
**         X_LIFNR        = ' '
**         X_TIPODTE      = ' '
**         _SCOPE         = '3'
**         _SYNCHRON      = ' '
**         _COLLECT       = ' '
        .
*Fin.Desbloqueo
    ENDLOOP.
*Ini.Log de Modificaciones Status (Pre-Contab)
*    DATA: wa_objectid  TYPE cdhdr-objectid.
*    DATA: old_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074 WITH HEADER LINE WITH NON-UNIQUE DEFAULT KEY.
*    FREE old_ztfi_0074.
*    CLEAR old_ztfi_0074.
*    DATA: new_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074 WITH HEADER LINE WITH NON-UNIQUE DEFAULT KEY.
*    FREE new_ztfi_0074.
*    CLEAR new_ztfi_0074.
*
*    LOOP AT lt_zcb_recfactprov.
*      wa_objectid = lt_zcb_recfactprov+0(36).
*      CLEAR: old_ztfi_0074[], new_ztfi_0074[].
*      APPEND lt_zcb_recfactprov TO new_ztfi_0074.
*      SELECT * FROM ztfi_0074 INTO TABLE old_ztfi_0074
*               WHERE bukrs EQ lt_zcb_recfactprov-bukrs
*                 AND xblnr EQ lt_zcb_recfactprov-xblnr
*                 AND lifnr EQ lt_zcb_recfactprov-lifnr
*                 AND tipodte EQ lt_zcb_recfactprov-tipodte.
*      IF sy-subrc EQ 0.
*        READ TABLE old_ztfi_0074 INDEX 1.
*        IF lt_zcb_recfactprov-status NE old_ztfi_0074-status OR
*           lt_zcb_recfactprov-belnr  NE old_ztfi_0074-belnr OR
*           lt_zcb_recfactprov-gjahr  NE old_ztfi_0074-gjahr OR
*           lt_zcb_recfactprov-aprobador_real NE old_ztfi_0074-aprobador_real.
*          CALL FUNCTION 'ZTFI_0074_WRITE_DOCUMENT'
*            IN UPDATE TASK
*            EXPORTING
*              objectid      = wa_objectid
*              tcode         = 'ZDTE_001'
*              utime         = sy-uzeit
*              udate         = sy-datum
*              username      = sy-uname
*              upd_ztfi_0074 = 'U'
*            TABLES
*              xztfi_0074    = new_ztfi_0074
*              yztfi_0074    = old_ztfi_0074.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*Ini.Log de Modificaciones Status (Pre-Contab)

*    UPDATE ztfi_0074 FROM TABLE lt_zcb_recfactprov.
*    COMMIT WORK AND WAIT.
*    UPDATE ztfi_0075 FROM TABLE lt_zdte_posfact.
*    COMMIT WORK AND WAIT.

**//.. Eliminar bloqueos
*    LOOP AT lt_zcb_recfactprov.
*
*      CALL FUNCTION 'DEQUEUE_EZTFI_0074'
*        EXPORTING
*          mode_ztfi_0074 = 'E'
*          mandt          = sy-mandt
*          bukrs          = lt_zcb_recfactprov-bukrs
*          xblnr          = lt_zcb_recfactprov-xblnr
*          lifnr          = lt_zcb_recfactprov-lifnr
*          tipodte        = lt_zcb_recfactprov-tipodte
**         X_BUKRS        = ' '
**         X_XBLNR        = ' '
**         X_LIFNR        = ' '
**         X_TIPODTE      = ' '
**         _SCOPE         = '3'
**         _SYNCHRON      = ' '
**         _COLLECT       = ' '
*        .
*
*    ENDLOOP.


    IF ti_log[] IS NOT INITIAL.
      PERFORM generar_alv.
    ENDIF.
  ENDIF.

ENDFORM.                    " GENERAR_PRECONT
*&---------------------------------------------------------------------*
*&      Form  GENERAR_FBV1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generar_fbv1  TABLES lt_zdte_cldoc STRUCTURE ztfi_0001b
                          lt_ekpo STRUCTURE ekpo
                          lt_ekkn STRUCTURE ekkn
                          lt_afko STRUCTURE afko
                          lt_proj STRUCTURE proj
                    USING
                          e_tipodte

                    CHANGING p_entrada TYPE ztfi_0074.


  "------>homologación tipo dte <=> clase doc.
  DATA: e_clase TYPE ztfi_0001b.
  CLEAR e_clase.

  e_clase-bukrs  = p_entrada-bukrs.
  e_clase-tipodte = p_entrada-tipodte.

  "---->valor scope.
**  READ TABLE lt_ekpo WITH KEY ebeln = p_entrada-ebeln
**                              knttp = 'N'.
**  IF sy-subrc EQ 0.
**    e_clase-knttp = lt_ekpo-knttp.
**    READ TABLE lt_ekkn WITH KEY ebeln = lt_ekpo-ebeln
**                                ebelp = lt_ekpo-ebelp BINARY SEARCH.
**    IF sy-subrc = 0.
**      READ TABLE lt_afko WITH KEY aufnr = lt_ekkn-nplnr BINARY SEARCH.
**      IF sy-subrc = 0.
**        READ TABLE lt_proj WITH KEY pspnr = lt_afko-pronr BINARY SEARCH.
**        IF sy-subrc = 0.
**          e_clase-scope = lt_proj-scope.
**        ENDIF.
**      ENDIF.
**    ENDIF.
**  ELSE.
**    READ TABLE lt_ekpo WITH KEY ebeln = p_entrada-ebeln.
**    IF sy-subrc = 0.
**      e_clase-knttp = lt_ekpo-knttp.
**    ENDIF.
**  ENDIF.


  DATA: lv_blart TYPE bkpf-blart.
  READ TABLE lt_zdte_cldoc WITH KEY bukrs   = e_clase-bukrs
                                    tipop   = e_tipodte
                                    tipodte = e_clase-tipodte
                                    knttp   = e_clase-knttp
                                    bsart   = e_clase-bsart.
  IF sy-subrc = 0.
    lv_blart = lt_zdte_cldoc-blart.
  ENDIF.



  DATA: lt_bdcdata TYPE STANDARD TABLE OF bdcdata WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.

  CLEAR lt_bdcdata.
  FREE lt_bdcdata.


  DATA: lv_fecha_documento TYPE string,
        lv_mes             TYPE string.

  lv_mes = p_entrada-bldat+4(2).
  CONCATENATE p_entrada-bldat+6(2) p_entrada-bldat+4(2) p_entrada-bldat+0(4) INTO lv_fecha_documento.
  CONDENSE lv_fecha_documento NO-GAPS.

  DATA: lv_fecha_cont TYPE string.

  CONCATENATE sy-datum+6(2) sy-datum+4(2) sy-datum+0(4) INTO lv_fecha_cont.

  PERFORM generar_dynpro TABLES lt_bdcdata USING 'SAPLF040'  '0100'.

  PERFORM generar_valor TABLES lt_bdcdata USING 'BDC_CURSOR' 'RF05V-NEWKO'.
  PERFORM generar_valor TABLES lt_bdcdata USING 'BDC_OKCODE' '/00'.
  PERFORM generar_valor TABLES lt_bdcdata USING 'BKPF-BLDAT'  lv_fecha_documento.
  PERFORM generar_valor TABLES lt_bdcdata USING 'BKPF-BLART'  lv_blart.
  PERFORM generar_valor TABLES lt_bdcdata USING 'BKPF-BUKRS' p_entrada-bukrs.
  PERFORM generar_valor TABLES lt_bdcdata USING 'BKPF-BUDAT' lv_fecha_cont.
  PERFORM generar_valor TABLES lt_bdcdata USING 'BKPF-MONAT' lv_mes.
  PERFORM generar_valor TABLES lt_bdcdata USING 'BKPF-WAERS' p_entrada-waers..
  PERFORM generar_valor TABLES lt_bdcdata USING 'BKPF-XBLNR' p_entrada-xblnr.
  PERFORM generar_valor TABLES lt_bdcdata USING 'BKPF-BKTXT' p_entrada-xblnr.
  PERFORM generar_valor TABLES lt_bdcdata USING 'VBKPF-XBWAE' 'X'.
  PERFORM generar_valor TABLES lt_bdcdata USING 'FS006-DOCID' '*'.

  IF p_entrada-tipodte = '33' OR p_entrada-tipodte = '34'
  OR p_entrada-tipodte = '110'.
    PERFORM generar_valor TABLES lt_bdcdata USING 'RF05V-NEWBS' '31'.
  ELSEIF p_entrada-tipodte = '61' OR p_entrada-tipodte = '112'.
    PERFORM generar_valor TABLES lt_bdcdata USING 'RF05V-NEWBS' '21'.
  ELSEIF p_entrada-tipodte = '56' OR p_entrada-tipodte = '111'.
    PERFORM generar_valor TABLES lt_bdcdata USING 'RF05V-NEWBS' '31'.
  ENDIF.





  PERFORM generar_valor TABLES lt_bdcdata USING 'RF05V-NEWKO'  p_entrada-lifnr.

* Pantalla para CPDs
  DATA: s_xcpd LIKE lfa1-xcpdk.
  CLEAR s_xcpd.
  DATA: s_ciudad LIKE bsec-ort01.
  DATA: s_direccion LIKE bsec-stras.
  DATA: s_name1 LIKE bsec-name1.

  CLEAR s_ciudad. CLEAR s_direccion. CLEAR s_name1.

  SELECT SINGLE xcpdk INTO s_xcpd FROM lfa1
     WHERE lifnr EQ p_entrada-lifnr .

  IF  s_xcpd = 'X'.
    IF p_entrada-rznsoc IS INITIAL.
      s_name1 = p_entrada-name1.
    ELSE.
      s_name1 = p_entrada-rznsoc(35).
    ENDIF.
    IF p_entrada-dirorigen  IS INITIAL.
      s_direccion = 'SANTIAGO'.
    ELSE.
      s_direccion = p_entrada-dirorigen(35).
    ENDIF.

    IF p_entrada-ciudadorigen  IS INITIAL.
      s_direccion = 'SANTIAGO'.
    ELSE.
      s_ciudad = p_entrada-ciudadorigen(20).
    ENDIF.
  ENDIF.
  IF  s_xcpd = 'X'.
    PERFORM generar_dynpro TABLES lt_bdcdata USING 'SAPLFCPD'  '0100'.
    PERFORM generar_valor TABLES lt_bdcdata USING 'BDC_CURSOR' 'BSEC-STCD1'.
    PERFORM generar_valor TABLES lt_bdcdata USING 'BDC_OKCODE' '/00'.
    PERFORM generar_valor TABLES lt_bdcdata USING 'BSEC-SPRAS'  'ES'.
    PERFORM generar_valor TABLES lt_bdcdata USING 'BSEC-NAME1'  s_name1.
    PERFORM generar_valor TABLES lt_bdcdata USING 'BSEC-STRAS'  s_direccion.
    PERFORM generar_valor TABLES lt_bdcdata USING 'BSEC-ORT01'  s_ciudad.
    PERFORM generar_valor TABLES lt_bdcdata USING 'BSEC-LAND1' 'CL'.
    PERFORM generar_valor TABLES lt_bdcdata USING 'BSEC-BANKS' 'CL'.
    PERFORM generar_valor TABLES lt_bdcdata USING 'BSEC-STCD1' p_entrada-stcd1.
  ENDIF.


  DATA: lv_monto TYPE char20.

  WRITE p_entrada-wrbtr CURRENCY 'CLP' TO lv_monto.
  CONDENSE lv_monto NO-GAPS.
  DATA: lv_fecha_formato TYPE string.
  CONCATENATE p_entrada-fechabase+6(2) p_entrada-fechabase+4(2) p_entrada-fechabase+0(4) INTO lv_fecha_formato SEPARATED BY '.'.
  CONDENSE lv_fecha_formato NO-GAPS.

  PERFORM generar_dynpro TABLES lt_bdcdata USING 'SAPLF040'  '0302'.
  PERFORM generar_valor TABLES lt_bdcdata USING 'BDC_CURSOR'  'BSEG-ZFBDT'.
  PERFORM generar_valor TABLES lt_bdcdata USING 'BDC_OKCODE'  '/00'.
  PERFORM generar_valor TABLES lt_bdcdata USING 'BSEG-WRBTR'  lv_monto.
  PERFORM generar_valor TABLES lt_bdcdata USING 'BKPF-XMWST'  'X'.
  PERFORM generar_valor TABLES lt_bdcdata USING 'BSEG-ZFBDT'  lv_fecha_formato.

  IF  s_xcpd = 'X'.


    PERFORM generar_dynpro TABLES lt_bdcdata USING 'SAPLFCPD'  '0100'.
    PERFORM generar_valor TABLES lt_bdcdata USING 'BDC_CURSOR' 'BSEC-SPRAS'.
    PERFORM generar_valor TABLES lt_bdcdata USING 'BDC_OKCODE' '/00'.
    PERFORM generar_valor TABLES lt_bdcdata USING 'BSEC-SPRAS'  'ES'.
    PERFORM generar_valor TABLES lt_bdcdata USING 'BSEC-NAME1'  s_name1.
    PERFORM generar_valor TABLES lt_bdcdata USING 'BSEC-STRAS' s_direccion.
    PERFORM generar_valor TABLES lt_bdcdata USING 'BSEC-ORT01' s_ciudad.
    PERFORM generar_valor TABLES lt_bdcdata USING 'BSEC-LAND1' 'CL'.
    PERFORM generar_valor TABLES lt_bdcdata USING 'BSEC-BANKS' 'CL'.
    PERFORM generar_valor TABLES lt_bdcdata USING 'BSEC-STCD1' p_entrada-stcd1.
  ENDIF.

  PERFORM generar_dynpro TABLES lt_bdcdata USING 'SAPLF040'  '0302'.
  PERFORM generar_valor TABLES lt_bdcdata USING 'BDC_CURSOR'  'BSEG-WRBTR'.
  PERFORM generar_valor TABLES lt_bdcdata USING 'BDC_OKCODE'  '=BP'.
  PERFORM generar_valor TABLES lt_bdcdata USING 'BSEG-WRBTR'  lv_monto.
  PERFORM generar_valor TABLES lt_bdcdata USING 'BKPF-XMWST'  'X'.

  PERFORM generar_valor TABLES lt_bdcdata USING 'BSEG-ZFBDT'  lv_fecha_formato.

  DATA:
    lt_mensaje TYPE STANDARD TABLE OF bdcmsgcoll WITH HEADER LINE,
    lv_mode    TYPE c.

  CLEAR lt_mensaje.
  FREE lt_mensaje.

  lv_mode = 'N'.

  CALL TRANSACTION 'F-63' USING lt_bdcdata MODE lv_mode MESSAGES INTO lt_mensaje.

  IF lt_mensaje[] IS NOT INITIAL.
    LOOP AT lt_mensaje.
      IF lt_mensaje-msgtyp = 'E'.
        CLEAR ti_log.
        ti_log-estado = icon_red_light.
        ti_log-documento = p_entrada-xblnr.
        ti_log-sociedad = p_entrada-bukrs.
        ti_log-proveedor = p_entrada-lifnr.
        ti_log-fecha = p_entrada-bldat.
        DATA: lv_clase TYPE t100-arbgb,
              lv_id    TYPE t100-msgnr.

        lv_clase = lt_mensaje-msgid.
        lv_id = lt_mensaje-msgnr.

        DATA: lv_mensaje TYPE char200.

        CALL FUNCTION 'MASS_MESSAGE_GET'
          EXPORTING
*           SPRSL             = SY-LANGU
            arbgb             = lv_clase
            msgnr             = lv_id
          IMPORTING
            msgtext           = lv_mensaje
          EXCEPTIONS
            message_not_found = 1
            OTHERS            = 2.
        "----> 'Error al crear doc. trx F-63,
        CONCATENATE TEXT-400  lv_mensaje INTO ti_log-mensaje
                                                     SEPARATED BY space.

*        PERFORM generar_log USING p_entrada '012' lv_mensaje. lt_mensaje-msgtyp
        PERFORM generar_log USING  p_entrada
                lt_mensaje-msgtyp lt_mensaje-msgid lt_mensaje-msgnr lt_mensaje-msgv1 lt_mensaje-msgv2 lt_mensaje-msgv3 lt_mensaje-msgv4.
        APPEND ti_log.
        p_entrada-status = '7'.
        EXIT.
      ELSEIF lt_mensaje-msgid = 'FP' AND lt_mensaje-msgtyp = 'S'.
        ti_log-estado = icon_green_light.
        ti_log-documento = p_entrada-xblnr.
        ti_log-sociedad = p_entrada-bukrs.
        ti_log-proveedor = p_entrada-lifnr.
        ti_log-fecha = p_entrada-bldat.
        p_entrada-gjahr = sy-datum+0(4).
*----> 'Documento creado F-63: '
        CONCATENATE TEXT-401 lt_mensaje-msgv1 '/'  p_entrada-gjahr INTO ti_log-mensaje.
        APPEND ti_log.
        p_entrada-status = '2'.
        p_entrada-belnr = lt_mensaje-msgv1.
        p_entrada-tcode = 'FBV3'.
*        PERFORM generar_log USING p_entrada '012' ti_log-mensaje.
        PERFORM generar_log USING  p_entrada
                lt_mensaje-msgtyp lt_mensaje-msgid lt_mensaje-msgnr lt_mensaje-msgv1 lt_mensaje-msgv2 lt_mensaje-msgv3 lt_mensaje-msgv4.
        READ TABLE ti_ztfi_0083 WITH KEY  sociedad = p_entrada-bukrs
                                          clase_documento =  lv_blart.
        IF sy-subrc = 0.
          p_entrada-codlib = ti_ztfi_0083-cod_lib.
        ENDIF.

      ENDIF.

    ENDLOOP.
    "----> Si el B.I Falla y no presenta error.
    IF  p_entrada-belnr IS INITIAL.
      CLEAR ti_log.
      ti_log-estado = icon_red_light.
      ti_log-documento = p_entrada-xblnr.
      ti_log-sociedad = p_entrada-bukrs.
      ti_log-proveedor = p_entrada-lifnr.
      ti_log-fecha = p_entrada-bldat.

      lt_mensaje-msgid = 'ZDTE_0001'.
      lt_mensaje-msgtyp = 'E'.
      lt_mensaje-msgnr = '012'.
      lt_mensaje-msgv1 = TEXT-408. lt_mensaje-msgv2 = '' . lt_mensaje-msgv3 = ''. lt_mensaje-msgv4 = ''.

      PERFORM generar_log USING  p_entrada
              lt_mensaje-msgtyp lt_mensaje-msgid lt_mensaje-msgnr lt_mensaje-msgv1 lt_mensaje-msgv2 lt_mensaje-msgv3 lt_mensaje-msgv4.
      APPEND ti_log.
      p_entrada-status = '7'.
      EXIT.
    ENDIF.
  ENDIF.


ENDFORM.                    " GENERAR_FBV1
*&---------------------------------------------------------------------*
*&      Form  GENERAR_MIRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ZCB_RECFACTPROV  text
*----------------------------------------------------------------------*
FORM generar_mira  TABLES lt_ekko STRUCTURE ekko
                          lt_ekpo STRUCTURE ekpo
                          lt_ztfi_0075 STRUCTURE  ztfi_0075
                          lt_ekkn STRUCTURE ekkn
                          lt_afko STRUCTURE afko
                          lt_proj STRUCTURE proj
                          lt_zdte_cldoc STRUCTURE ztfi_0001b
                   USING  s_tipodte s_estatus
                  CHANGING  p_entrada TYPE ztfi_0074.


  DATA: lt_return TYPE STANDARD TABLE OF bapiret2 WITH HEADER LINE.
  DATA: ls_headerdata           LIKE  bapi_incinv_create_header,
        ls_additionalheaderdata LIKE  bapi_incinv_save_header_backgr,
        ls_refdoccategory       LIKE  bapi_incinv_fld-ref_doc_category.

  DATA: lt_tax_data TYPE STANDARD TABLE OF bapi_incinv_create_tax WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.

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
  READ TABLE lt_ekko WITH KEY ebeln = p_entrada-ebeln.
  IF sy-subrc EQ 0.
    e_clase-bsart = lt_ekko-bsart.
  ENDIF.

  "---->valor scope.
  READ TABLE lt_ekpo WITH KEY ebeln = p_entrada-ebeln
                              knttp = 'N'.
  IF sy-subrc = 0.
    e_clase-knttp = lt_ekpo-knttp.
**    READ TABLE lt_ekkn WITH KEY ebeln = lt_ekpo-ebeln
**                                ebelp = lt_ekpo-ebelp BINARY SEARCH.
**    IF sy-subrc = 0.
**      READ TABLE lt_afko WITH KEY aufnr = lt_ekkn-nplnr BINARY SEARCH.
**      IF sy-subrc = 0.
**        READ TABLE lt_proj WITH KEY pspnr = lt_afko-pronr BINARY SEARCH.
**        IF sy-subrc = 0.
**          e_clase-scope = lt_proj-scope.
**        ENDIF.
**      ENDIF.
**    ENDIF.
  ELSE.
    READ TABLE lt_ekpo WITH KEY ebeln = p_entrada-ebeln.
    IF sy-subrc = 0.
      e_clase-knttp = lt_ekpo-knttp.
    ENDIF.
  ENDIF.

  READ TABLE lt_zdte_cldoc WITH KEY bukrs = e_clase-bukrs
                                    tipodte = e_clase-tipodte
                                    knttp = e_clase-knttp
                                    bsart = e_clase-bsart.
  IF sy-subrc = 0.
    ls_headerdata-doc_type = lt_zdte_cldoc-blart.
  ENDIF.


  "---->fin homologación

  "----->determinación bloqueo para pago.
  READ TABLE lt_t001 WITH KEY bukrs = p_entrada-bukrs.
  IF sy-subrc = 0.
    READ TABLE ti_vbwf16 WITH KEY wfvar = lt_t001-wfvar
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

  DATA: lv_tax_amount LIKE lt_tax_data-tax_amount.

  CLEAR lv_tax_amount.
  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
    EXPORTING
      currency        = p_entrada-waers
      amount_internal = p_entrada-iva
    IMPORTING
      amount_external = lv_tax_amount.

  IF lv_tax_amount NE 0 AND s_tipodte = '4'. "Costos Ind/Ind.IVa
    lt_tax_data-tax_code = 'C1'.
  ENDIF.
  IF lv_tax_amount EQ 0 AND s_tipodte = '4'. "Costos Ind/ind.IVa
    lt_tax_data-tax_code = 'C0'.
  ENDIF.

  lt_tax_data-tax_amount = lv_tax_amount.
  APPEND lt_tax_data.

  DATA: lt_selectpo TYPE STANDARD TABLE OF bapi_incinv_select_po WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.

  CLEAR lt_selectpo.
  FREE lt_selectpo.
  LOOP AT lt_ztfi_0075 WHERE ebeln = p_entrada-ebeln
                        AND  bukrs = p_entrada-bukrs
                        AND  xblnr = p_entrada-xblnr
                        AND  lifnr = p_entrada-lifnr
                        AND  tipodte = p_entrada-tipodte.

    lt_selectpo-po_number = p_entrada-ebeln.
    lt_selectpo-po_item = lt_ztfi_0075-ebelp.
    APPEND lt_selectpo.
  ENDLOOP.

  SORT lt_selectpo BY po_number po_item.

  DELETE ADJACENT DUPLICATES FROM lt_selectpo COMPARING ALL FIELDS.

  DATA: lv_invoicedocnumber LIKE  bapi_incinv_fld-inv_doc_no,
        lv_fiscalyear       LIKE  bapi_incinv_fld-fisc_year.

  IF s_tipodte = '4'. "Costos Ind.-Proveedor
    ls_refdoccategory = '1'. "Por Proveedor
  ELSE.
    ls_refdoccategory = '1'. "prov.con OC
  ENDIF.

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
*     SELECTDELIVERY       =
*     SELECTBILLLADING     =
*     SELECTSERVICE        =
*     SELECTPLANT          =
      taxdata              = lt_tax_data
*     WITHTAXDATA          =
*     VENDORITEMSPLITDATA  =
      return               = lt_return
*     EXTENSIONIN          =
    .

*  IF sy-subrc = 0.
  READ TABLE lt_return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    CLEAR ti_log.
    ti_log-estado = icon_red_light.
    ti_log-documento = p_entrada-xblnr.
    ti_log-sociedad = p_entrada-bukrs.
    ti_log-proveedor = p_entrada-lifnr.
    ti_log-fecha = p_entrada-bldat.
*----> 'Error al crear doc. trx MIRA, '
    CONCATENATE TEXT-402 lt_return-message INTO ti_log-mensaje
                                                   SEPARATED BY space.
    APPEND ti_log.
    p_entrada-status = '7'.
*      PERFORM generar_log USING p_entrada '012'  ti_log-mensaje.
    PERFORM generar_log USING  p_entrada
              lt_return-type lt_return-id lt_return-number lt_return-message_v1 lt_return-message_v2
              lt_return-message_v3 lt_return-message_v3.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    CLEAR ti_log.
    ti_log-estado = icon_green_light.
    ti_log-documento = p_entrada-xblnr.
    ti_log-sociedad = p_entrada-bukrs.
    ti_log-proveedor = p_entrada-lifnr.
    ti_log-fecha = p_entrada-bldat.
*---> 'Documento creado MIRA: '
    CONCATENATE TEXT-403 lv_invoicedocnumber '/' lv_fiscalyear INTO ti_log-mensaje.
    APPEND ti_log.
    p_entrada-status = s_estatus.                           " '3'.
    p_entrada-belnr = lv_invoicedocnumber.
    p_entrada-gjahr = lv_fiscalyear.
    p_entrada-tcode = 'MIR4'.

*      PERFORM generar_log USING p_entrada '012'  ti_log-mensaje.
    PERFORM generar_log USING  p_entrada
                    lt_return-type lt_return-id lt_return-number lt_return-message_v1 lt_return-message_v2 lt_return-message_v3 lt_return-message_v4.

    READ TABLE ti_ztfi_0083 WITH KEY sociedad = e_clase-bukrs
                                clase_documento = ls_headerdata-doc_type.
    IF sy-subrc = 0.
      p_entrada-codlib = ti_ztfi_0083-cod_lib.
    ENDIF.
  ENDIF.
*  ENDIF.

  "p_entrada-status = '3'.

ENDFORM.                    " GENERAR_MIRA
*&---------------------------------------------------------------------*
*&      Form  GENERAR_MIR7
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generar_mir7 TABLES   lt_zdte_cldoc STRUCTURE ztfi_0001b
                           lt_ekko STRUCTURE ekko
                           lt_ekpo STRUCTURE ekpo
                           lt_ekkn STRUCTURE ekkn
                           lt_afko STRUCTURE afko
                           lt_proj STRUCTURE proj
                  USING    p_oc
                  CHANGING p_entrada TYPE ztfi_0074.

  DATA: ls_headerdata TYPE bapi_incinv_create_header,
        lt_itemdata   TYPE STANDARD TABLE OF bapi_incinv_create_item WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE,
        lt_return     TYPE STANDARD TABLE OF bapiret2 WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.

  CLEAR: lt_itemdata,
         lt_return.

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
    "lt_itemdata-de_cre_ind  = 'X'.
  ENDIF.

  "------->homologación tipodte <=>clase doc.

  DATA: e_clase TYPE ztfi_0001b.
  CLEAR e_clase.

  e_clase-bukrs  = p_entrada-bukrs.
  e_clase-tipodte = p_entrada-tipodte.

  "---->Buscar BSART
  READ TABLE lt_ekko WITH KEY ebeln = p_oc.
  IF sy-subrc EQ 0.
    e_clase-bsart = lt_ekko-bsart.
  ENDIF.

  "---->valor scope.
  READ TABLE lt_ekpo WITH KEY ebeln = p_oc
                              knttp = 'N'.
  IF sy-subrc = 0.
    e_clase-knttp = lt_ekpo-knttp.
**    READ TABLE lt_ekkn WITH KEY ebeln = lt_ekpo-ebeln
**                                ebelp = lt_ekpo-ebelp BINARY SEARCH.
**    IF sy-subrc = 0.
**      READ TABLE lt_afko WITH KEY aufnr = lt_ekkn-nplnr BINARY SEARCH.
**      IF sy-subrc = 0.
**        READ TABLE lt_proj WITH KEY pspnr = lt_afko-pronr BINARY SEARCH.
**        IF sy-subrc = 0.
**          e_clase-scope = lt_proj-scope.
**        ENDIF.
**      ENDIF.
**    ENDIF.
  ELSE.
    READ TABLE lt_ekpo WITH KEY ebeln = p_oc.
    IF sy-subrc = 0.
      e_clase-knttp = lt_ekpo-knttp.
    ENDIF.
  ENDIF.

  READ TABLE lt_zdte_cldoc WITH KEY bukrs = e_clase-bukrs
                                    tipodte = e_clase-tipodte
                                    knttp = e_clase-knttp
                                    bsart = e_clase-bsart.
  IF sy-subrc = 0.
    ls_headerdata-doc_type = lt_zdte_cldoc-blart.
  ENDIF.

  "----->determinación bloqueo para pago.
  READ TABLE lt_t001 WITH KEY bukrs = p_entrada-bukrs.
  IF sy-subrc = 0.
    READ TABLE ti_vbwf16 WITH KEY wfvar = lt_t001-wfvar
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
  ls_headerdata-item_text = p_entrada-texto.
  ls_headerdata-calc_tax_ind  = 'X'.
  ls_headerdata-bline_date = p_entrada-bldat.
  DATA: lv_invoice_doc_item TYPE rblgp.
  lv_invoice_doc_item  = '00001'.


  DATA:lt_impu TYPE STANDARD TABLE OF bapi_incinv_create_account WITH HEADER LINE.
  FREE lt_impu.


  DATA: lt_glimpu TYPE STANDARD TABLE OF  bapi_incinv_create_gl_account,
        ln_glimpu LIKE LINE OF lt_glimpu.

  FREE lt_glimpu.
  CLEAR ln_glimpu.
  LOOP AT lt_ekpo WHERE ebeln = p_oc AND loekz NE 'X'.
    "----->estructura itemdata.
    CLEAR lt_itemdata.
    IF p_entrada-tipodte = '56' OR p_entrada-tipodte = '111'.
      lt_itemdata-de_cre_ind  = 'X'.
    ENDIF.
    lt_itemdata-po_number = lt_ekpo-ebeln.
    lt_itemdata-po_item = lt_ekpo-ebelp.
    lt_itemdata-invoice_doc_item = lv_invoice_doc_item.
    IF lt_ekpo-knttp NE 'U'.
      lt_itemdata-item_amount = lv_amount_item.
      lt_itemdata-tax_code = lt_ekpo-mwskz.
    ELSE.
      CLEAR:  lt_itemdata-item_amount,
              lt_itemdata-tax_code.
    ENDIF.

    APPEND lt_itemdata.
    "---->si es tipo de imputación K o U
    IF lt_ekpo-knttp NE 'U'.
      "----->estructura imputaciones
      CLEAR: lt_impu.

      lt_impu-invoice_doc_item = lv_invoice_doc_item.
      READ TABLE lt_ekkn WITH KEY ebeln = lt_ekpo-ebeln.
      IF sy-subrc = 0.
        lt_impu-gl_account = lt_ekkn-sakto.
        lt_impu-costcenter = lt_ekkn-kostl.
      ENDIF.
      lt_impu-serial_no = '01'.
      lt_impu-tax_code = lt_ekpo-mwskz.

      CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
        EXPORTING
          currency        = p_entrada-waers
          amount_internal = lv_impuesto
        IMPORTING
          amount_external = lv_amount_item_imp.

      lt_impu-item_amount = lv_amount_item_imp."p_entrada-wrbtr.
      APPEND lt_impu.
      ADD 1 TO lv_invoice_doc_item.

    ELSE.
      "---->se agrega condicionante por imputación U.
      READ TABLE ti_ztfi_0085 WITH KEY bukrs = lt_ekpo-bukrs BINARY SEARCH.
      IF sy-subrc = 0.
        CLEAR ln_glimpu.
        ln_glimpu-gl_account = ti_ztfi_0085-hkont.

        CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
          EXPORTING
            currency        = p_entrada-waers
            amount_internal = lv_impuesto
          IMPORTING
            amount_external = lv_amount_item_imp.

        ln_glimpu-item_amount = lv_amount_item_imp.
        ln_glimpu-costcenter = ti_ztfi_0085-kostl.
        ln_glimpu-tax_code = lt_ekpo-mwskz.
        ln_glimpu-comp_code = lt_ekpo-bukrs.
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
      return           = lt_return.

  IF lt_return[] IS NOT INITIAL.
    LOOP AT lt_return.
      IF lt_return-type = 'E'.
        CLEAR ti_log.
        ti_log-estado = icon_red_light.
        ti_log-documento = p_entrada-xblnr.
        ti_log-sociedad = p_entrada-bukrs.
        ti_log-proveedor = p_entrada-lifnr.
        ti_log-fecha = p_entrada-bldat.
*--->  'Error al crear doc. trx MIR7, '
        CONCATENATE TEXT-404 lt_return-message INTO ti_log-mensaje
                                                     SEPARATED BY space.
        APPEND ti_log.
        p_entrada-status = '7'.
        PERFORM generar_log USING p_entrada lt_return-type lt_return-id lt_return-number lt_return-message_v1
              lt_return-message_v2 lt_return-message_v3 lt_return-message_v4.

      ENDIF.
    ENDLOOP.
  ELSEIF lt_return[] IS  INITIAL AND lv_documento IS NOT INITIAL AND lv_gjahr IS NOT INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    CLEAR ti_log.
    ti_log-estado = icon_green_light.
    ti_log-documento = p_entrada-xblnr.
    ti_log-sociedad = p_entrada-bukrs.
    ti_log-proveedor = p_entrada-lifnr.
    ti_log-fecha = p_entrada-bldat.
*--->    'Documento creado MIR7: '
    CONCATENATE TEXT-405 lv_documento '/' lv_gjahr  INTO ti_log-mensaje.
    APPEND ti_log.
    p_entrada-status = '2'.
    p_entrada-belnr  = lv_documento.
    p_entrada-gjahr = lv_gjahr.
    p_entrada-tcode = 'MIR4'.
*    PERFORM generar_log USING p_entrada '012' ti_log-mensaje.
    PERFORM generar_log USING p_entrada  lt_return-type lt_return-id lt_return-number lt_return-message_v1
             lt_return-message_v2 lt_return-message_v3 lt_return-message_v4.
    READ TABLE ti_ztfi_0083 WITH KEY sociedad = e_clase-bukrs
                                       clase_documento = ls_headerdata-doc_type.
    IF sy-subrc = 0.
      p_entrada-codlib = ti_ztfi_0083-cod_lib.
    ENDIF.
  ENDIF.
ENDFORM.                    " GENERAR_MIR7
*&---------------------------------------------------------------------*
*&      Form  GENERAR_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generar_alv .

  DATA: lt_catalogo TYPE slis_t_fieldcat_alv,
        ln_catalogo LIKE LINE OF lt_catalogo.


  FREE lt_catalogo.

  CLEAR ln_catalogo.
  ln_catalogo-fieldname = 'ESTADO'.
  ln_catalogo-tabname = 'TI_LOG'.
  ln_catalogo-seltext_s = TEXT-105."'Estado'.
  APPEND ln_catalogo TO lt_catalogo.

  CLEAR ln_catalogo.
  ln_catalogo-fieldname = 'DOCUMENTO'.
  ln_catalogo-tabname = 'TI_LOG'.
  ln_catalogo-seltext_s = TEXT-104."'Documento'.
  APPEND ln_catalogo TO lt_catalogo.

  CLEAR ln_catalogo.
  ln_catalogo-fieldname = 'SOCIEDAD'.
  ln_catalogo-tabname = 'TI_LOG'.
  ln_catalogo-seltext_s = TEXT-103."'Sociedad'.
  APPEND ln_catalogo TO lt_catalogo.

  CLEAR ln_catalogo.
  ln_catalogo-fieldname = 'PROVEEDOR'.
  ln_catalogo-tabname = 'TI_LOG'.
  ln_catalogo-seltext_s = TEXT-102."'Proveedor'.
  APPEND ln_catalogo TO lt_catalogo.

  CLEAR ln_catalogo.
  ln_catalogo-fieldname = 'FECHA'.
  ln_catalogo-tabname = 'TI_LOG'.
  ln_catalogo-seltext_s = TEXT-101."'Fecha'.
  APPEND ln_catalogo TO lt_catalogo.

  CLEAR ln_catalogo.
  ln_catalogo-fieldname = 'MENSAJE'.
  ln_catalogo-tabname = 'TI_LOG'.
  ln_catalogo-seltext_s = TEXT-100."'Mensaje'.
  APPEND ln_catalogo TO lt_catalogo.

  DATA: lv_layout TYPE slis_layout_alv.
  CLEAR lv_layout.

  lv_layout-colwidth_optimize = 'X'.
  lv_layout-zebra = 'X'.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = lv_layout
      it_fieldcat        = lt_catalogo
    TABLES
      t_outtab           = ti_log                                                                                                                                                                                                 " i_structure_name = 'TI_LOG'
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.


ENDFORM.                    " GENERAR_ALV
*&---------------------------------------------------------------------*
*&      Form  GENERAR_RECHAZO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generar_rechazo TABLES lt_paval STRUCTURE t001z
                            lt_lfa1 STRUCTURE lfa1
                    USING
                          p_glosa
                          p_numero
                    CHANGING p_entrada TYPE ztfi_0074.

  DATA: e_salida TYPE zst_dte_ack.

  CLEAR e_salida.
  e_salida-bukrs = p_entrada-bukrs.

  e_salida-rutreceptor = p_entrada-stcd1.
  READ TABLE lt_paval WITH KEY bukrs = p_entrada-bukrs.
  IF sy-subrc = 0.
    REPLACE ALL OCCURRENCES OF '.' IN lt_paval-paval WITH space.
    CONDENSE lt_paval-paval NO-GAPS.
    e_salida-rutrecibe = lt_paval-paval.
    e_salida-rutreceptor = lt_paval-paval.
  ENDIF.

  DATA:lv_addrnumber TYPE addr1_sel-addrnumber.

  READ TABLE lt_lfa1 WITH KEY lifnr = p_entrada-lifnr.
  IF sy-subrc = 0.
    lv_addrnumber = lt_lfa1-adrnr.
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

**  e_salida-tipodte = '33'.
  e_salida-tipodte = p_entrada-tipodte.
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

  CALL FUNCTION 'ZFI_0008DTE'
    EXPORTING
      acknowledgment = e_salida
      commit         = 'X'.


*  PERFORM generar_log USING p_entrada p_numero ''.
  p_entrada-status = '6'.
  p_entrada-glosa = e_salida-estadodteglosa.

  "---Envío de e-mail rechazo
  CALL FUNCTION 'ZFI_0028DTE'
    EXPORTING
      i_lifnr   = p_entrada-lifnr
      i_bukrs   = p_entrada-bukrs
      i_xblnr   = p_entrada-xblnr
      i_tipodte = p_entrada-tipodte.


ENDFORM.                    " GENERAR_RECHAZO
*&---------------------------------------------------------------------*
*&      Form  GENERAR_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generar_log USING p_entrada TYPE ztfi_0074
*                       p_numero p_variable.
                      xmsgty xmsgid xmsgno xmsgv1 xmsgv2 xmsgv3 xmsgv4.
  DATA: lv_s_log  TYPE bal_s_log,
        lv_handle TYPE balloghndl,
        lv_stcd1  TYPE lfa1-stcd1.

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
    DATA: lv_s_msg TYPE bal_s_msg.

    lv_s_msg-msgty = xmsgty.
    lv_s_msg-msgid = xmsgid.
    lv_s_msg-msgno = xmsgno.
    lv_s_msg-msgv1 = xmsgv1.
    lv_s_msg-msgv2 = xmsgv2.
    lv_s_msg-msgv3 = xmsgv3.
    lv_s_msg-msgv4 = xmsgv4.


*    IF p_numero = '012'.
*      IF strlen( p_variable ) > 50.
*        DATA: lv_line TYPE string,
*                   lv_rest TYPE string.
*
*        CLEAR: lv_line,
*               lv_rest.
*
*        CALL FUNCTION 'TEXT_SPLIT'
*          EXPORTING
*            length = '50'
*            text   = p_variable
*          IMPORTING
*            line   = lv_line
*            rest   = lv_rest.
*        lv_s_msg-msgv1 = lv_line.
*        lv_s_msg-msgv2 = lv_rest.
*      ENDIF.
*
*    ENDIF.

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


ENDFORM.                    " GENERAR_LOG
*&---------------------------------------------------------------------*
*&      Form  GENERAR_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_BDCDATA  text
*      -->P_1900   text
*      -->P_1901   text
*      -->P_1902   text
*----------------------------------------------------------------------*
FORM generar_dynpro  TABLES   p_lt_bdcdata STRUCTURE  bdcdata
                     USING   p_program
                              p_dynpro.
  CLEAR p_lt_bdcdata.
  p_lt_bdcdata-program = p_program .
  p_lt_bdcdata-dynpro  = p_dynpro .
  p_lt_bdcdata-dynbegin = 'X'.
  APPEND p_lt_bdcdata.

ENDFORM.                    " GENERAR_DYNPRO


*&---------------------------------------------------------------------*
*&      Form  generar_valor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_BDCDATA  text
*      -->P_CAMPO       text
*      -->P_VALOR       text
*----------------------------------------------------------------------*
FORM generar_valor TABLES p_lt_bdcdata STRUCTURE bdcdata
                   USING p_campo p_valor.
  CLEAR p_lt_bdcdata.
  p_lt_bdcdata-fnam = p_campo.
  p_lt_bdcdata-fval = p_valor.
  APPEND p_lt_bdcdata.


ENDFORM.                    "generar_valor
*&---------------------------------------------------------------------*
*&      Form  GENERAR_FBV1_AUTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_EKPO  text
*      -->P_LT_ZDTE_POSFACT  text
*      -->P_LT_EKKN  text
*      -->P_LT_AFKO  text
*      -->P_LT_PROJ  text
*      -->P_LT_LFB1  text
*      -->P_LT_ZDTE_CLDOC  text
*      -->P_LT_ZTFI_0090  text
*      -->P_LT_ZTFI_0092  text
*      -->P_LT_ZTFI_0093  text
*      -->P_S_TIPODTE  text
*      -->P_ESTATUS  text
*      <--P_LT_ZCB_RECFACTPROV  text
*----------------------------------------------------------------------*
FORM generar_fbv1_auto  TABLES   t_ekpo       STRUCTURE ekpo
                                 t_ztfi_0075  STRUCTURE ztfi_0075
                                 t_ekkn       STRUCTURE ekkn
                                 t_afko       STRUCTURE afko
                                 t_proj       STRUCTURE proj
                                 t_lfb1       STRUCTURE lfb1
                                 t_zdte_cldoc STRUCTURE ztfi_0001b
                                 t_ztfi_0090  STRUCTURE ztfi_0090
*                                 t_ztfi_0092  STRUCTURE ztfi_0092
                                 t_ztfi_0093  STRUCTURE ztfi_0093
                        USING    u_tipodte
                                 u_estatus
                        CHANGING c_recfactprov TYPE ztfi_0074.
  DATA: lt_bkpf     TYPE TABLE OF bkpf,
        ls_bkpf     TYPE bkpf,
        lt_bseg     TYPE TABLE OF bseg,
        ls_bseg     TYPE bseg,
        ls_lfb1     TYPE lfb1,
        ls_cladte   TYPE ztfi_0001b,
        lv_blart    TYPE bkpf-blart,
        lv_fdocto   TYPE string,
        lv_fecont   TYPE string,
        lv_mes      TYPE string,
        lv_shkzg_h  TYPE shkzg VALUE 'H',
        lv_shkzg_s  TYPE shkzg VALUE 'S',
        lv_koart_s  TYPE koart VALUE 'S', " Cuentas de mayor
        lv_koart_k  TYPE koart VALUE 'K', " Acreedores
*        lv_cdsii    TYPE ztfi_0093-cdsii VALUE '15',
        lv_buzei    TYPE bseg-buzei,
        lv_clvcta_k TYPE bseg-bschl,
        lv_clvcta_m TYPE bseg-bschl,
        ls_fs006    TYPE fs006,
        lv_xfeld    TYPE xfeld,
        lv_error    TYPE xfeld,
        lv_belnr    TYPE bkpf-belnr,
        lv_bukrs    TYPE bkpf-bukrs,
        lt_bset     TYPE STANDARD TABLE OF bset,
        lt_bsec     TYPE STANDARD TABLE OF bsec,
        lt_bsez     TYPE STANDARD TABLE OF bsez,
        lt_with     TYPE STANDARD TABLE OF with_itemx.

  "---->homologación tipo dte <=> clase doc.
  ls_cladte-bukrs   = c_recfactprov-bukrs.
  ls_cladte-tipodte = c_recfactprov-tipodte.

  "---->valor scope.
  READ TABLE t_ekpo WITH KEY ebeln = c_recfactprov-ebeln
                             knttp = 'N'.
  IF sy-subrc = 0.
    ls_cladte-knttp = t_ekpo-knttp.
**    READ TABLE t_ekkn WITH KEY ebeln = t_ekpo-ebeln
**                               ebelp = t_ekpo-ebelp BINARY SEARCH.
**    IF sy-subrc = 0.
**      READ TABLE t_afko WITH KEY aufnr = t_ekkn-nplnr BINARY SEARCH.
**      IF sy-subrc = 0.
**        READ TABLE t_proj WITH KEY pspnr = t_afko-pronr BINARY SEARCH.
**        IF sy-subrc = 0.
**          ls_cladte-scope = t_proj-scope.
**        ENDIF.
**      ENDIF.
**    ENDIF.
  ELSE.
    READ TABLE t_ekpo WITH KEY ebeln = c_recfactprov-ebeln.
    IF sy-subrc = 0.
      ls_cladte-knttp = t_ekpo-knttp.
    ENDIF.
  ENDIF.

  "-----
  READ TABLE t_zdte_cldoc WITH KEY bukrs   = ls_cladte-bukrs
                                   tipop   = u_tipodte
                                   tipodte = ls_cladte-tipodte
                                   knttp   = ls_cladte-knttp
                                   bsart   = ls_cladte-bsart.
  IF sy-subrc = 0.
    lv_blart = t_zdte_cldoc-blart.
  ELSE.
*----> 'Error'
    ti_log-estado    = icon_red_light.
    ti_log-documento = c_recfactprov-xblnr.
    ti_log-sociedad  = c_recfactprov-bukrs.
    ti_log-proveedor = c_recfactprov-lifnr.
    ti_log-fecha     = c_recfactprov-bldat.
    MESSAGE ID 'ZDTE_0001' TYPE 'E' NUMBER '035' INTO ti_log-mensaje.
    CONCATENATE TEXT-410 ti_log-mensaje INTO ti_log-mensaje
                                        SEPARATED BY space.
    APPEND ti_log.

    c_recfactprov-status = '7'.

    PERFORM generar_log USING  c_recfactprov        'E'
                               'ZDTE_0001'           '035'
                               space                space
                               space                space.
    EXIT.
  ENDIF.

  "----
  READ TABLE t_lfb1 WITH KEY bukrs = c_recfactprov-bukrs
                             lifnr = c_recfactprov-lifnr
                             INTO ls_lfb1.
  IF sy-subrc NE 0.
    " Error??
  ENDIF.

  "---- Fecha Documento
  lv_mes = c_recfactprov-bldat+4(2).
  CONCATENATE c_recfactprov-bldat+6(2) c_recfactprov-bldat+4(2)
              c_recfactprov-bldat+0(4) INTO lv_fdocto.
  CONDENSE lv_fdocto NO-GAPS.

  "---- Fecha Contabilización
  CONCATENATE sy-datum+6(2) sy-datum+4(2) sy-datum+0(4) INTO lv_fecont.

  "---- Lleno los datos cabecera
  ls_bkpf-mandt = sy-mandt.
  ls_bkpf-bukrs = c_recfactprov-bukrs.
  ls_bkpf-gjahr = sy-datum(4).
  ls_bkpf-blart = lv_blart.
  ls_bkpf-bldat = c_recfactprov-bldat. "lv_fdocto.
  ls_bkpf-wwert = c_recfactprov-bldat. "lv_fdocto.
  ls_bkpf-budat = sy-datum. "lv_fecont.
*  ls_bkpf-monat = lv_mes.
  ls_bkpf-xblnr = c_recfactprov-xblnr.
  ls_bkpf-waers = c_recfactprov-waers. "'CLP'.
  ls_bkpf-hwaer =  c_recfactprov-waers. "'CLP'.
  ls_bkpf-bktxt = 'Doc.Fact.Elect'.
  ls_bkpf-glvor = 'RFBV'.
  ls_bkpf-tcode = 'FV60'.

  "---- Determ. Clave de contabilización
  CASE c_recfactprov-tipodte.
    WHEN '33' OR '34' OR '61' OR '110' OR '111'.
      lv_clvcta_k = 31.
      lv_clvcta_m = 40.
    WHEN OTHERS.
      lv_clvcta_k = 21.
      lv_clvcta_m = 50.
  ENDCASE.

  APPEND ls_bkpf TO lt_bkpf.

  "----(1) Asiento Cuenta por Pagar
  CLEAR: lv_buzei,
         ls_bseg.
  ADD 1 TO lv_buzei.
  ls_bseg-mandt = sy-mandt.
  ls_bseg-bukrs = c_recfactprov-bukrs.
  ls_bseg-gjahr = c_recfactprov-gjahr.
  ls_bseg-buzei = lv_buzei.
  ls_bseg-bschl = lv_clvcta_k.
  ls_bseg-koart = lv_koart_k.
  ls_bseg-zfbdt = c_recfactprov-bldat.
  ls_bseg-zterm =  ls_lfb1-zterm.

  IF  lv_clvcta_k EQ 31.
    ls_bseg-shkzg = lv_shkzg_h.
  ELSE.
    ls_bseg-shkzg = lv_shkzg_s.
  ENDIF.

  ls_bseg-dmbtr = c_recfactprov-mnttotal.
  ls_bseg-wrbtr = c_recfactprov-mnttotal.
  ls_bseg-wmwst = c_recfactprov-iva.
  ls_bseg-pswsl =  c_recfactprov-waers.
  ls_bseg-hkont = ls_lfb1-akont.
  ls_bseg-lifnr = c_recfactprov-lifnr.
  APPEND ls_bseg TO lt_bseg. CLEAR: ls_bseg.

  CALL FUNCTION 'PRELIMINARY_POSTING_FB01'
    EXPORTING
      i_xcmpl       = ' '
      i_tcode       = 'FBV1'                                " 'FV60'
      fs006_fb01    = ls_fs006
    IMPORTING
      xepbbp        = lv_xfeld
    TABLES
      t_bkpf        = lt_bkpf  "header data.
      t_bseg        = lt_bseg  "line item data
      t_bsec        = lt_bsec  "one time vendor data
      t_bset        = lt_bset  "tax data
      t_bsez        = lt_bsez
      t_spltwt      = lt_with  "Withholding tax
    EXCEPTIONS
      error_message = 1.

  IF  sy-subrc EQ 0.
    MOVE: sy-msgv1 TO lv_belnr,
          sy-msgv2 TO lv_bukrs.
**//.. Commit
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

**//.. Llenar Log
    CLEAR ti_log.
    ti_log-estado    = icon_green_light.
    ti_log-documento = c_recfactprov-xblnr.
    ti_log-sociedad  = c_recfactprov-bukrs.
    ti_log-proveedor = c_recfactprov-lifnr.
    ti_log-fecha     = c_recfactprov-bldat.
*---> 'Documento creado : '
    CONCATENATE TEXT-409 lv_belnr '/' ls_bkpf-gjahr INTO ti_log-mensaje.
    APPEND ti_log.
    c_recfactprov-status = u_estatus.
    c_recfactprov-belnr  = lv_belnr.
    c_recfactprov-gjahr  = ls_bkpf-gjahr.
    c_recfactprov-tcode  = 'FBV3'.

    PERFORM generar_log USING c_recfactprov sy-msgty sy-msgid sy-msgno
                              sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ELSE.
    CLEAR ti_log.
    ti_log-estado    = icon_red_light.
    ti_log-documento = c_recfactprov-xblnr.
    ti_log-sociedad  = c_recfactprov-bukrs.
    ti_log-proveedor = c_recfactprov-lifnr.
    ti_log-fecha     = c_recfactprov-bldat.
*----> 'Error al crear doc. trx FBV1, '
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ti_log-mensaje.
    CONCATENATE TEXT-410 ti_log-mensaje INTO ti_log-mensaje
                                        SEPARATED BY space.
    APPEND ti_log.

    c_recfactprov-status = '7'.

    PERFORM generar_log USING c_recfactprov sy-msgty sy-msgid sy-msgno
                              sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " GENERAR_FBV1_AUTO
