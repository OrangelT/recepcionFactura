*&---------------------------------------------------------------------*
*&  Include           ZFIR_0058_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GENERAR_CONT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generar_cont .
  DATA: lt_zcb_recfactprov TYPE STANDARD TABLE OF ztfi_0074
                           WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE,
        lv_tabix           TYPE sytabix.

  SELECT *
  FROM ztfi_0074
  INTO TABLE lt_zcb_recfactprov
  WHERE bukrs IN s_bukrs AND
        xblnr IN s_folio AND
        belnr NE space   AND
        gjahr NE space   AND
        bldat IN s_fecha AND
        status IN s_status.
  IF sy-dbcnt >= 1.
    " Bloqueos
    LOOP AT lt_zcb_recfactprov.
      lv_tabix = sy-tabix.

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
        DELETE lt_zcb_recfactprov INDEX lv_tabix.
      ENDIF.
    ENDLOOP.

    SORT lt_zcb_recfactprov BY bukrs tipodte xblnr lifnr bldat.
    DELETE ADJACENT DUPLICATES FROM lt_zcb_recfactprov.



**    IF s_fecha[] IS NOT INITIAL.
**      DELETE lt_zcb_recfactprov WHERE bldat NOT IN s_fecha.
**    ENDIF.

    IF  lt_zcb_recfactprov[] IS INITIAL
    AND ti_log[] IS NOT INITIAL.
      PERFORM mostrar_alv.
    ENDIF.

    DATA: lt_zdte_posfact TYPE STANDARD TABLE OF ztfi_0075 WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.

    SELECT *
    FROM ztfi_0075
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

    DATA: lt_ekpo TYPE STANDARD TABLE OF ekpo WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
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
      DATA: lt_ekbe TYPE STANDARD TABLE OF ekbe WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
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

    DATA: lt_ztfi_0082 TYPE STANDARD TABLE OF ztfi_0082 WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.
    SELECT *
    FROM ztfi_0082
    INTO TABLE lt_ztfi_0082
    FOR ALL ENTRIES IN lt_zcb_recfactprov
    WHERE bukrs = lt_zcb_recfactprov-bukrs.
    IF sy-dbcnt >= 1.
      SORT lt_ztfi_0082 BY bukrs.
      DELETE ADJACENT DUPLICATES FROM lt_ztfi_0082.
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
    ENDIF.

    CLEAR ti_log.
    FREE ti_log.
    DATA: lv_registro LIKE sy-tabix,
          lv_trx      TYPE ztfi_0074-tcode.

    DATA: lv_valida_prov TYPE c,
          lv_servicio    TYPE c,
          t_pstyp        LIKE ekpo-pstyp.

    "----->referencias.
    DATA: lt_ztfi_0077 TYPE STANDARD TABLE OF ztfi_0077 WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.

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


    LOOP AT lt_zcb_recfactprov.
      lv_registro = sy-tabix.
      lv_trx = lt_zcb_recfactprov-tcode.
      CLEAR ti_log.
      CLEAR lv_valida_prov.
      CLEAR lv_servicio.
      "--->Validación periodo.
      READ TABLE lt_ztfi_0082 WITH KEY bukrs = lt_zcb_recfactprov-bukrs.
      IF sy-subrc = 0.
        DATA: r_fecha TYPE RANGE OF sydatum WITH HEADER LINE.

        CLEAR r_fecha.
        r_fecha-sign = 'I'.
        r_fecha-option = 'BT'.
        r_fecha-low = lt_ztfi_0082-begda.
        r_fecha-high = lt_ztfi_0082-endda.
        APPEND r_fecha.
        IF sy-datum  IN  r_fecha.
          MESSAGE e017(zfi_0003) INTO ti_log-mensaje.
          PERFORM generar_log USING lt_zcb_recfactprov '017' ''.
          ti_log-estado = icon_red_light.
          ti_log-documento = lt_zcb_recfactprov-xblnr.
          ti_log-mensaje = ti_log-mensaje.
          ti_log-sociedad = lt_zcb_recfactprov-bukrs.
          ti_log-proveedor = lt_zcb_recfactprov-lifnr.
          ti_log-fecha = lt_zcb_recfactprov-bldat.
          APPEND ti_log.
          CONTINUE.
        ENDIF.
      ENDIF.
      "---->fin

      "---->validación proveedor.
      READ TABLE lt_lfa1 WITH KEY lifnr = lt_zcb_recfactprov-lifnr.
      IF sy-subrc NE 0.
        READ TABLE lt_lfb1 WITH KEY lifnr = lt_lfa1-lifnr
                                    bukrs = lt_zcb_recfactprov-bukrs.
        IF sy-subrc NE 0.
          ti_log-estado = icon_red_light.

          MESSAGE e000(zfi_0003) WITH lt_zcb_recfactprov-bukrs INTO ti_log-mensaje.
          PERFORM generar_rechazo TABLES lt_t001z  lt_lfa1 USING  ti_log-mensaje '000' CHANGING lt_zcb_recfactprov.
          APPEND ti_log.
          MODIFY lt_zcb_recfactprov INDEX lv_registro.
          CONTINUE.
        ENDIF.
        ti_log-estado = icon_red_light.

        MESSAGE e000(zfi_0003) WITH lt_zcb_recfactprov-bukrs INTO ti_log-mensaje.
        PERFORM generar_rechazo TABLES lt_t001z  lt_lfa1 USING ti_log-mensaje '000' CHANGING  lt_zcb_recfactprov.
        APPEND ti_log.
        MODIFY lt_zcb_recfactprov INDEX lv_registro.
        CONTINUE.
      ELSE.
        "------ validación proveedor
        READ TABLE lt_lfb1 WITH KEY lifnr = lt_lfa1-lifnr.
        IF sy-subrc = 0.
          IF lt_lfb1-zdte_tipo = '1' OR lt_lfb1-zdte_tipo = '3' OR lt_lfb1-zdte_tipo = '4'.
            lv_valida_prov = 'X'.
          ELSEIF lt_lfb1-zdte_tipo = '2'.
            lv_valida_prov = 'C'.
          ENDIF.
        ENDIF.
      ENDIF.
      "----->pstyp  9 o distinto de 9
      IF lv_valida_prov = 'X'.

        READ TABLE lt_ekpo WITH KEY ebeln = lt_zcb_recfactprov-ebeln
                                    pstyp = '9'.
        IF sy-subrc = 0.
          lv_servicio = 'X'.
          "---->igual a 9
          DATA: lv_error TYPE c.
          CLEAR lv_error.
          LOOP AT lt_ekpo WHERE ebeln = lt_zcb_recfactprov-ebeln AND pstyp = '9'.
            READ TABLE lt_ekbe WITH KEY ebeln = lt_ekpo-ebeln
                                        ebelp = lt_ekpo-ebelp
                                        bewtp = 'D'.
            IF sy-subrc = 0.
              READ TABLE lt_ekbe WITH KEY ebeln = lt_ekpo-ebeln
                                        ebelp = lt_ekpo-ebelp
                                        bewtp = 'E'
                                        bwart = '101'.
              IF sy-subrc NE 0.
                "--->error
                lv_error = 'X'.
                lt_zcb_recfactprov-status = '8'.
                MESSAGE e014(zfi_0003) INTO ti_log-mensaje.
                PERFORM generar_log USING lt_zcb_recfactprov '014' ''.
                MODIFY lt_zcb_recfactprov INDEX lv_registro.

                ti_log-estado = icon_red_light.
                ti_log-documento = lt_zcb_recfactprov-xblnr.
                ti_log-mensaje = ti_log-mensaje.
                ti_log-sociedad = lt_zcb_recfactprov-bukrs.
                ti_log-proveedor = lt_zcb_recfactprov-lifnr.
                ti_log-fecha = lt_zcb_recfactprov-bldat.
                APPEND ti_log.


                CONTINUE.
              ENDIF.
            ELSE.
              "---->error
              lv_error = 'X'.
              lt_zcb_recfactprov-status = '8'.
              MESSAGE e015(zfi_0003) INTO ti_log-mensaje.
              PERFORM generar_log USING lt_zcb_recfactprov '015' ''.
              MODIFY lt_zcb_recfactprov INDEX lv_registro.
              "PERFORM generar_rechazo TABLES lt_t001z  lt_lfa1 USING  ti_log-mensaje '010' CHANGING lt_zcb_recfactprov.

              ti_log-estado = icon_red_light.
              ti_log-documento = lt_zcb_recfactprov-xblnr.
              ti_log-mensaje = ti_log-mensaje.
              ti_log-sociedad = lt_zcb_recfactprov-bukrs.
              ti_log-proveedor = lt_zcb_recfactprov-lifnr.
              ti_log-fecha = lt_zcb_recfactprov-bldat.
              APPEND ti_log.

              CONTINUE.
            ENDIF.
          ENDLOOP.
          IF lv_error = 'X'.
            CONTINUE.
          ENDIF.
        ELSE.
          CLEAR lv_servicio.
          CLEAR  t_pstyp.
          SELECT SINGLE  pstyp INTO t_pstyp FROM ekpo WHERE ebeln = lt_zcb_recfactprov-ebeln
                                       AND  pstyp = '1'.
          IF sy-subrc  NE 0.
            "---->distinto de 9 o 1
            CLEAR lv_error.
            LOOP AT lt_ekpo WHERE ebeln = lt_zcb_recfactprov-ebeln.

              READ TABLE lt_ekbe WITH KEY ebeln = lt_ekpo-ebeln
                                               ebelp = lt_ekpo-ebelp
                                               bewtp = 'E'
                                               bwart = '101'.
              IF sy-subrc NE 0.
                READ TABLE lt_ekbe WITH KEY ebeln = lt_ekpo-ebeln
                                              ebelp = lt_ekpo-ebelp
                                              bewtp = 'E'
                                              bwart = '105'.
                IF sy-subrc NE 0.
                  IF lt_ekpo-pstyp NE '1'.
                    "--->error.
                    lt_zcb_recfactprov-status = '8'.
                    MESSAGE e016(zfi_0003) INTO ti_log-mensaje.
                    PERFORM generar_log USING lt_zcb_recfactprov '016' ''.
                    MODIFY lt_zcb_recfactprov INDEX lv_registro.


                    ti_log-estado = icon_red_light.
                    ti_log-documento = lt_zcb_recfactprov-xblnr.
                    ti_log-mensaje = ti_log-mensaje.
                    ti_log-sociedad = lt_zcb_recfactprov-bukrs.
                    ti_log-proveedor = lt_zcb_recfactprov-lifnr.
                    ti_log-fecha = lt_zcb_recfactprov-bldat.
                    APPEND ti_log.
                    lv_error = 'X'.

                    CONTINUE.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDLOOP.
            IF lv_error = 'X'.
              CONTINUE.
            ENDIF.

          ELSE.
            lv_trx = 'MIR7'.
          ENDIF.
        ENDIF.


      ELSEIF lv_valida_prov = 'C'.
        lv_trx = 'FBV3'.
      ENDIF.

      CASE lv_trx.
        WHEN 'MIR4'.
          PERFORM generar_miro_mir4 TABLES lt_t001z  lt_lfa1 USING lv_servicio CHANGING lt_zcb_recfactprov.
        WHEN 'MIR7'.
          PERFORM generar_miro_mir7 TABLES lt_t001z  lt_lfa1 CHANGING lt_zcb_recfactprov.
        WHEN 'FBV3'.
          PERFORM generar_fbv1 TABLES lt_t001z  lt_lfa1 CHANGING lt_zcb_recfactprov.
      ENDCASE.
      MODIFY  lt_zcb_recfactprov INDEX lv_registro.
    ENDLOOP.

***Inic.Log de Modificaciones Status (Contab)
    DATA: wa_objectid  TYPE cdhdr-objectid.
    DATA: old_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074 WITH HEADER LINE WITH NON-UNIQUE DEFAULT KEY.
    FREE old_ztfi_0074.
    CLEAR old_ztfi_0074.
    DATA: new_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074 WITH HEADER LINE WITH NON-UNIQUE DEFAULT KEY.
    FREE new_ztfi_0074.
    CLEAR new_ztfi_0074.

    LOOP AT lt_zcb_recfactprov.
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
    ENDLOOP.
***Ini. Log de Modificaciones Status (Contab)

    UPDATE ztfi_0074 FROM TABLE lt_zcb_recfactprov.
    COMMIT WORK AND WAIT.

**//.. Eliminar bloqueos
    LOOP AT lt_zcb_recfactprov.

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

    ENDLOOP.


    IF ti_log[] IS NOT INITIAL.
      PERFORM mostrar_alv.
    ENDIF.
  ENDIF.

ENDFORM.                    " GENERAR_CONT
*&---------------------------------------------------------------------*
*&      Form  GENERAR_MIRO_MIR4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generar_miro_mir4 TABLES lt_t001z lt_lfa1 USING p_servicio CHANGING p_entrada TYPE ztfi_0074.

  DATA: s_bukrs TYPE RANGE OF bukrs WITH HEADER LINE,
        s_doc   TYPE RANGE OF bkpf-belnr WITH HEADER LINE,
        s_gjahr TYPE RANGE OF bkpf-gjahr WITH HEADER LINE.


  CLEAR: s_bukrs,
         s_doc,
         s_gjahr.

  FREE: s_bukrs,
        s_doc,
        s_gjahr.

  s_bukrs-sign = 'I'.
  s_bukrs-option = 'EQ'.
  s_bukrs-low = p_entrada-bukrs.
  APPEND s_bukrs.

  s_doc-sign = 'I'.
  s_doc-option = 'EQ'.
  s_doc-low = p_entrada-belnr.
  APPEND s_doc.

  s_gjahr-sign = 'I'.
  s_gjahr-option = 'EQ'.
  s_gjahr-low = p_entrada-gjahr.
  APPEND s_gjahr.

  DATA: s_user TYPE RANGE OF usnam WITH HEADER LINE.

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
      ti_log-estado = icon_red_light.
      ti_log-documento = p_entrada-xblnr.
      ti_log-mensaje = 'Error en Contabilización'.
      ti_log-sociedad = p_entrada-bukrs.
      ti_log-proveedor = p_entrada-lifnr.
      ti_log-fecha = p_entrada-bldat.
      p_entrada-status = '8'.
      APPEND ti_log.
      PERFORM generar_log USING p_entrada
                     '007' ti_log-mensaje.
    ELSE.
      ti_log-estado = icon_green_light.
      ti_log-documento = p_entrada-xblnr.
      ti_log-sociedad = p_entrada-bukrs.
      CONCATENATE 'Documento Contabilizado: ' p_entrada-belnr '/' p_entrada-gjahr INTO ti_log-mensaje.
      ti_log-proveedor = p_entrada-lifnr.
      ti_log-fecha = p_entrada-bldat.
      p_entrada-status = '5'.
      p_entrada-glosa = TEXT-gl2.
      APPEND ti_log.
      PERFORM generar_log USING p_entrada
                     '007' ti_log-mensaje.
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
*          ti_log-estado = icon_red_light.
*          ti_log-documento = p_entrada-xblnr.
*          ti_log-sociedad = p_entrada-bukrs.
*          CONCATENATE 'Documento Contabilizaco: ' p_entrada-belnr '/' p_entrada-gjahr space text-500 INTO ti_log-mensaje.
*          ti_log-proveedor = p_entrada-lifnr.
*          ti_log-fecha = p_entrada-bldat.
*        ELSE.
*          ti_log-estado = icon_green_light.
*          ti_log-documento = p_entrada-xblnr.
*          ti_log-sociedad = p_entrada-bukrs.
*          CONCATENATE 'Documento Contabilizado: ' p_entrada-belnr '/' p_entrada-gjahr space text-501 INTO ti_log-mensaje.
*          ti_log-proveedor = p_entrada-lifnr.
*          ti_log-fecha = p_entrada-bldat.
*        ENDIF.
*        APPEND ti_log.
*        PERFORM generar_log USING p_entrada
*                     '007' ti_log-mensaje.
*      ENDIF.
      "---->fin liberación de pago
    ENDIF.
  ENDIF.

  IF p_entrada-status = '5'.
    PERFORM generar_aprob TABLES lt_t001z  lt_lfa1 USING  ti_log-mensaje '012' CHANGING p_entrada.
  ENDIF.


ENDFORM.                    " GENERAR_MIRO_MIR4
*&---------------------------------------------------------------------*
*&      Form  GENERAR_MIRO_MIR7
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generar_miro_mir7 TABLES lt_t001z  lt_lfa1 CHANGING p_entrada TYPE ztfi_0074.
  DATA: lv_invoice TYPE bapi_incinv_fld-inv_doc_no,
        lv_fiscal  TYPE bapi_incinv_fld-fisc_year.
  DATA: lt_return TYPE STANDARD TABLE OF bapiret2 WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE.

  CLEAR lt_return.
  FREE lt_return.

  lv_invoice = p_entrada-belnr.
  lv_fiscal = p_entrada-gjahr.

  CALL FUNCTION 'BAPI_INCOMINGINVOICE_POST'
    EXPORTING
      invoicedocnumber = lv_invoice
      fiscalyear       = lv_fiscal
    TABLES
      return           = lt_return.

  READ TABLE lt_return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    ti_log-estado = icon_red_light.
    ti_log-documento = p_entrada-xblnr.
    ti_log-mensaje = lt_return-message.
    ti_log-sociedad = p_entrada-bukrs.
    ti_log-proveedor = p_entrada-lifnr.
    ti_log-fecha = p_entrada-bldat.
    p_entrada-status = '8'.
    APPEND ti_log.

    PERFORM generar_log USING p_entrada
                     '007' lt_return-message.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    ti_log-estado = icon_green_light.
    ti_log-documento = p_entrada-xblnr.
    ti_log-mensaje = lt_return-message.
    ti_log-sociedad = p_entrada-bukrs.
    ti_log-proveedor = p_entrada-lifnr.
    ti_log-fecha = p_entrada-bldat.
    APPEND ti_log.
    p_entrada-status = '5'.
    p_entrada-glosa = TEXT-gl2.

    PERFORM generar_log USING p_entrada
                       '007' lt_return-message.
    PERFORM generar_aprob TABLES lt_t001z  lt_lfa1 USING  ti_log-mensaje '012' CHANGING p_entrada.
  ENDIF.



ENDFORM.                    " GENERAR_MIRO_MIR7
*&---------------------------------------------------------------------*
*&      Form  GENERAR_FBV1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generar_fbv1 TABLES lt_t001z lt_lfa1 CHANGING p_entrada TYPE ztfi_0074.

  DATA: xvbkpf  TYPE STANDARD TABLE OF vbkpf WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE,
        xmsg    TYPE STANDARD TABLE OF fimsg1 WITH NON-UNIQUE DEFAULT KEY WITH HEADER LINE,
        lt_xmsg TYPE STANDARD TABLE OF msg_tab_line WITH NON-UNIQUE
                                                   DEFAULT KEY WITH HEADER LINE.
  DATA: lv_posted  TYPE boole_d VALUE 'X',
        ls_bal_log TYPE bal_s_log,
        ls_bal_msg TYPE bal_s_msg,
        ls_handle  TYPE balloghndl.
  CLEAR xvbkpf.
  FREE xvbkpf.

  CLEAR xmsg.
  FREE xmsg.

  xvbkpf-belnr = p_entrada-belnr.
  xvbkpf-bukrs = p_entrada-bukrs.
  xvbkpf-gjahr = p_entrada-gjahr.
  APPEND xvbkpf.

  DATA: lv_clase TYPE t100-arbgb,
        lv_id    TYPE t100-msgnr.
  DATA: lv_mensaje TYPE char200.

  CALL FUNCTION 'PRELIMINARY_POSTING_POST_ALL'
    EXPORTING
      nomsg   = 'X'
      synch   = 'X'
*     nocheck = 'X'
    TABLES
      t_vbkpf = xvbkpf
      t_msg   = xmsg
    EXCEPTIONS
      OTHERS  = 1.

  IF xmsg[] IS NOT INITIAL.
    LOOP AT xmsg WHERE posted NE 'X'.
      CLEAR: lv_posted.
      ti_log-estado    = icon_red_light.
      ti_log-documento = p_entrada-xblnr.
      ti_log-sociedad  = p_entrada-bukrs.
      ti_log-proveedor = p_entrada-lifnr.
      ti_log-fecha     = p_entrada-bldat.

      xmsg-msgty = 'E'.
**      lv_clase = 'E'. "xmsg-msgid.
**      lv_id    = xmsg-msgno.
      CLEAR lv_mensaje.

      CALL FUNCTION 'MASS_MESSAGE_GET'
        EXPORTING
*         SPRSL             = SY-LANGU
          arbgb             = xmsg-msgid
          msgnr             = xmsg-msgno
          msgv1             = xmsg-msgv1
          msgv2             = xmsg-msgv2
          msgv3             = xmsg-msgv3
          msgv4             = xmsg-msgv4
        IMPORTING
          msgtext           = lv_mensaje
        EXCEPTIONS
          message_not_found = 1
          OTHERS            = 2.

      CONCATENATE 'Error al contabilizar FBV0 ' lv_mensaje INTO ti_log-mensaje
                                                SEPARATED BY space.

**      PERFORM generar_log USING p_entrada '012' lv_mensaje.
      APPEND ti_log.
      p_entrada-status = '8'.

**//..
      IF ls_handle IS INITIAL.
        PERFORM crear_log USING    p_entrada
                          CHANGING ls_bal_log ls_handle.
        PERFORM add_log USING ls_handle
                              xmsg.
      ELSE.
        PERFORM add_log USING ls_handle
                              xmsg.
      ENDIF.

      AT LAST.
        PERFORM save_log USING ls_handle.
      ENDAT.
    ENDLOOP.

    IF lv_posted EQ 'X'.
      READ TABLE xmsg WITH KEY posted = lv_posted.
      IF sy-subrc EQ 0.
        ti_log-estado    = icon_green_light.
        ti_log-documento = p_entrada-xblnr.
        ti_log-sociedad  = p_entrada-bukrs.
        ti_log-proveedor = p_entrada-lifnr.
        ti_log-fecha     = p_entrada-bldat.

        CLEAR lv_mensaje.
        CALL FUNCTION 'MASS_MESSAGE_GET'
          EXPORTING
*           SPRSL             = SY-LANGU
            arbgb             = xmsg-msgid
            msgnr             = xmsg-msgno
            msgv1             = xmsg-msgv1
            msgv2             = xmsg-msgv2
            msgv3             = xmsg-msgv3
            msgv4             = xmsg-msgv4
          IMPORTING
            msgtext           = lv_mensaje
          EXCEPTIONS
            message_not_found = 1
            OTHERS            = 2.

        CONCATENATE 'Documento contabilizado FBV0 ' p_entrada-belnr '/' p_entrada-gjahr INTO ti_log-mensaje
                                                     SEPARATED BY space.
        PERFORM generar_log USING p_entrada '012' lv_mensaje.

        APPEND ti_log.

        CALL FUNCTION 'DEQUEUE_EFBKPF'
          EXPORTING
            belnr = p_entrada-belnr
            bukrs = p_entrada-bukrs
            gjahr = p_entrada-gjahr.
        p_entrada-status = '5'.
        p_entrada-glosa = TEXT-gl2.

        PERFORM generar_aprob TABLES lt_t001z  lt_lfa1 USING  ti_log-mensaje '012' CHANGING p_entrada.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " GENERAR_FBV1
*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mostrar_alv .

  DATA: lt_catalogo TYPE slis_t_fieldcat_alv,
        ln_catalogo LIKE LINE OF lt_catalogo.


  CLEAR lt_catalogo.
  FREE lt_catalogo.


  CLEAR ln_catalogo.
  ln_catalogo-fieldname = 'ESTADO'.
  ln_catalogo-tabname = 'TI_LOG'.
  ln_catalogo-seltext_s = 'Estado'.
  APPEND ln_catalogo TO lt_catalogo.

  CLEAR ln_catalogo.
  ln_catalogo-fieldname = 'DOCUMENTO'.
  ln_catalogo-tabname = 'TI_LOG'.
  ln_catalogo-seltext_s = 'Documento'.
  APPEND ln_catalogo TO lt_catalogo.

  CLEAR ln_catalogo.
  ln_catalogo-fieldname = 'SOCIEDAD'.
  ln_catalogo-tabname = 'TI_LOG'.
  ln_catalogo-seltext_s = 'Sociedad'.
  APPEND ln_catalogo TO lt_catalogo.

  CLEAR ln_catalogo.
  ln_catalogo-fieldname = 'PROVEEDOR'.
  ln_catalogo-tabname = 'TI_LOG'.
  ln_catalogo-seltext_s = 'Proveedor'.
  APPEND ln_catalogo TO lt_catalogo.

  CLEAR ln_catalogo.
  ln_catalogo-fieldname = 'FECHA'.
  ln_catalogo-tabname = 'TI_LOG'.
  ln_catalogo-seltext_s = 'Fecha'.
  APPEND ln_catalogo TO lt_catalogo.

  CLEAR ln_catalogo.
  ln_catalogo-fieldname = 'MENSAJE'.
  ln_catalogo-tabname = 'TI_LOG'.
  ln_catalogo-seltext_s = 'Mensaje'.
  APPEND ln_catalogo TO lt_catalogo.

  DATA: lv_layout TYPE slis_layout_alv.
  CLEAR lv_layout.

  lv_layout-colwidth_optimize = 'X'.
  lv_layout-zebra = 'X'.


  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      is_layout   = lv_layout
      it_fieldcat = lt_catalogo
    TABLES
      t_outtab    = ti_log.


ENDFORM.                    " MOSTRAR_ALV


*&---------------------------------------------------------------------*
*&      Form  generar_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ENTRADA  text
*      -->P_NUMERO   text
*----------------------------------------------------------------------*
FORM generar_log USING p_entrada TYPE ztfi_0074
                       p_numero  p_variable.

  DATA: lv_s_log  TYPE bal_s_log,
        lv_handle TYPE balloghndl,
        lv_stcd1  TYPE lfa1-stcd1.


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
    DATA: lv_s_msg TYPE bal_s_msg.

    lv_s_msg-msgty = 'E'.
    lv_s_msg-msgid = 'ZDTE_0001'.
    lv_s_msg-msgno = p_numero.
    lv_s_msg-msgv1 = p_variable.

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


ENDFORM.                    " GENERAR_LOG


*&---------------------------------------------------------------------*
*&      Form  generar_rechazo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LT_PAVAL   text
*      -->LT_LFA1    text
*      -->P_GLOSA    text
*      -->P_NUMERO   text
*      -->P_ENTRADA  text
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

  e_salida-tipodte = '33'.
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


  PERFORM generar_log USING p_entrada p_numero ''.
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
*&      Form  generar_aprob
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LT_PAVAL   text
*      -->LT_LFA1    text
*      -->P_GLOSA    text
*      -->P_NUMERO   text
*      -->P_ENTRADA  text
*----------------------------------------------------------------------*
FORM generar_aprob TABLES lt_paval STRUCTURE t001z
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

  e_salida-tipodte = '33'.
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

  CALL FUNCTION 'ZFI_0008DTE'
    EXPORTING
      acknowledgment = e_salida
      commit         = 'X'.



ENDFORM.                    " GENERAR_RECHAZO

*&---------------------------------------------------------------------*
*&      Form  CREAR_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_ENTRADA  text
*      <--P_LS_BAL_LOG  text
*      <--P_LS_HANDLE  text
*----------------------------------------------------------------------*
FORM crear_log  USING    p_entrada TYPE ztfi_0074
                CHANGING c_bal_log TYPE bal_s_log
                         c_handle  TYPE balloghndl.
*  DATA: c_bal_log  TYPE bal_s_log,
*        lv_handle TYPE balloghndl,
*        lv_stcd1  TYPE lfa1-stcd1.

*  CLEAR c_bal_log.

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
ENDFORM.                    " CREAR_LOG
*&---------------------------------------------------------------------*
*&      Form  ADD_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_HANDLE  text
*      -->P_XMSG  text
*      -->P_ELSE  text
*----------------------------------------------------------------------*
FORM add_log  USING    u_handle TYPE balloghndl
                       u_xmsg   TYPE fimsg1.
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
ENDFORM.                    " ADD_LOG

*&---------------------------------------------------------------------*
*&      Form  SAVE_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_HANDLE  text
*----------------------------------------------------------------------*
FORM save_log  USING    u_handle TYPE balloghndl.
  DATA: lt_log_input TYPE bal_t_logh.

  FREE lt_log_input.
  APPEND u_handle TO lt_log_input.

  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_client       = sy-mandt
      i_save_all     = 'X'
      i_t_log_handle = lt_log_input.
ENDFORM.                    " SAVE_LOG
