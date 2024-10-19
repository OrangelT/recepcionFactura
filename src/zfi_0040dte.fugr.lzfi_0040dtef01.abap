*----------------------------------------------------------------------*
***INCLUDE LZFI_0040DTEF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DATA_GENERA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_genera .

  MOVE-CORRESPONDING  ls_salidag TO lt_zcb_recfactprov.
  APPEND lt_zcb_recfactprov.
  "--> Orden
  SELECT * FROM ekko INTO TABLE lt_ekko
  WHERE ebeln EQ lt_zcb_recfactprov-ebeln
  ORDER BY PRIMARY KEY.
  "--> Posicion
  IF lines( lt_ekko ) GT 0.
    SELECT * FROM ekpo INTO TABLE lt_ekpo
    FOR ALL ENTRIES IN lt_ekko
    WHERE ebeln EQ lt_ekko-ebeln
    ORDER BY PRIMARY KEY.
  ENDIF.

  IF lines( lt_zcb_recfactprov ) GT 0.
    "--> posicion 75
    SELECT * FROM ztfi_0075 INTO TABLE lt_zdte_posfact
       FOR ALL ENTRIES IN lt_zcb_recfactprov
       WHERE bukrs  EQ lt_zcb_recfactprov-bukrs
         AND xblnr EQ   lt_zcb_recfactprov-xblnr
         AND tipodte EQ lt_zcb_recfactprov-tipodte
         AND lifnr  EQ  lt_zcb_recfactprov-lifnr
       ORDER BY PRIMARY KEY.

    "--> Clase de documento
    SELECT * FROM ztfi_0001b
     INTO TABLE lt_zdte_cldoc
     FOR ALL ENTRIES IN lt_zcb_recfactprov
     WHERE bukrs = lt_zcb_recfactprov-bukrs.
    IF sy-dbcnt >= 1.
      SORT lt_zdte_cldoc BY bukrs.
      DELETE ADJACENT DUPLICATES FROM lt_zdte_cldoc.
    ENDIF.

    "-----Datos de WF de FI
    SELECT bukrs wfvar  FROM t001
    INTO TABLE lt_t001
    WHERE bukrs EQ lt_zcb_recfactprov-bukrs.
    IF lines( lt_t001 ) GT 0.
      SELECT * FROM vbwf16
      INTO TABLE ti_vbwf16
      FOR ALL ENTRIES IN lt_t001
      WHERE wfvar = lt_t001-wfvar.
      IF sy-dbcnt >= 1.
        SORT ti_vbwf16 BY wfvar blart.
        DELETE ADJACENT DUPLICATES FROM ti_vbwf16.
      ENDIF.
    ENDIF.

    "---Referencias
    SELECT * FROM ztfi_0077 INTO TABLE lt_zdt_recref
       FOR ALL ENTRIES IN lt_zcb_recfactprov
       WHERE bukrs  EQ lt_zcb_recfactprov-bukrs
         AND xblnr EQ   lt_zcb_recfactprov-xblnr
         AND lifnr  EQ  lt_zcb_recfactprov-lifnr
         AND tipodte EQ lt_zcb_recfactprov-tipodte
       ORDER BY PRIMARY KEY.


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
      ENDIF.
    ENDIF.

  ENDIF.

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

      SELECT *
                   FROM afko
                   INTO TABLE lt_afko
                   FOR ALL ENTRIES IN lt_ekkn
                   WHERE aufnr = lt_ekkn-nplnr.
      IF sy-dbcnt >= 1.
        SORT lt_afko BY aufnr.
        DELETE ADJACENT DUPLICATES FROM lt_afko.

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
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SUTITUCION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sutitucion .
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

ENDFORM.
