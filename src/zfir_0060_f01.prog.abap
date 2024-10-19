*&---------------------------------------------------------------------*
*&  Include           ZFIR_0060_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  macro definir icono
*&---------------------------------------------------------------------*
*   -->  type 'I,E,S,W'
*   <--  ICON
*&---------------------------------------------------------------------*
DEFINE ma_icono_determ.
  case &1.
    when 'I'.
      move icon_information  to &2.
    when 'S'.
      move icon_green_light  to &2.
    when 'W'.
      move icon_yellow_light to &2.
    when 'E' or 'A'.
      move icon_red_light    to &2.
  endcase.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&  macro rangos 'WA_XXX' 'RG_XXX' 'I' 'EQ' VLOW VHIGH.
*&---------------------------------------------------------------------*
DEFINE ma_rangos.
  concatenate &1 '-SIGN' into text_r.
  assign (text_r) to <fs_a>.
  move &3 to <fs_a>.
  concatenate &1 '-OPTION' into text_r.
  assign (text_r) to <fs_a>.
  move &4 to <fs_a>.
  concatenate &1 '-LOW' into text_r.
  assign (text_r) to <fs_a>.
  move &5 to <fs_a>.
  concatenate &1 '-HIGH' into text_r.
  assign (text_r) to <fs_a>.
  move &6 to <fs_a>.
  assign (&1) to <fs_a>.
  concatenate &2 '[]' into text_r.
  assign (text_r) to <fs_b>.
  append <fs_a> to <fs_b>.
  clear text_r.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&      DEFINE ma_set_column_position
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
DEFINE ma_set_column_position.
  call method lr_columns->set_column_position
    exporting
      columnname = &1
      position   = &2.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&      Form  FO_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->U_TCODE  text
*----------------------------------------------------------------------*
FORM fo_authority_check  USING    u_tcode TYPE sy-tcode.
  AUTHORITY-CHECK OBJECT 'S_TCODE'
           ID 'TCD' FIELD sy-tcode.
  IF sy-subrc NE 0.
    MESSAGE e077(s#) WITH sy-tcode.
  ENDIF.
ENDFORM.                    " FO_AUTHORITY_CHECK

*&---------------------------------------------------------------------*
*&      Form  FO_INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fo_initialization .
**//.. Estatus a evaluar

  ma_rangos: 'SO_STATU' 'SO_STATU' 'I' 'EQ' '5' ''.
  ma_rangos: 'SO_STATU' 'SO_STATU' 'I' 'EQ' '6' ''.

ENDFORM.                    " FO_INITIALIZATION

*&---------------------------------------------------------------------*
*&      Form  FO_DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fo_display_alv USING    it_generica TYPE ANY TABLE.
  DATA: lv_title        TYPE lvc_title,
        ls_key          TYPE salv_s_layout_key,
        lr_sorts        TYPE REF TO cl_salv_sorts,
        lr_sort         TYPE REF TO cl_salv_sort,
        lr_events       TYPE REF TO cl_salv_events_table,
        lr_column       TYPE REF TO cl_salv_column,
        lr_columno      TYPE REF TO cl_salv_column_table,
        lr_columns      TYPE REF TO cl_salv_columns_table,
        lr_layout       TYPE REF TO cl_salv_layout,
        lr_content      TYPE REF TO cl_salv_form_element,
        lr_aggregations TYPE REF TO cl_salv_aggregations,
        lr_selections   TYPE REF TO cl_salv_selections,
        gr_functions    TYPE REF TO cl_salv_functions_list,
        lr_display      TYPE REF TO cl_salv_display_settings,
        lv_icono        TYPE string,
        lv_text         TYPE string,
        lv_tool         TYPE string,
        lv_funpos       TYPE salv_de_function_pos VALUE
        if_salv_c_function_position=>right_of_salv_functions.
**//..
  TRY .
      IF sy-batch EQ abap_true.
        CALL METHOD cl_salv_table=>factory
          EXPORTING
            list_display = abap_true
          IMPORTING
            r_salv_table = gr_table
          CHANGING
            t_table      = it_generica[].
      ELSE.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = gr_table
          CHANGING
            t_table      = it_generica[].
      ENDIF.

    CATCH cx_salv_msg.
  ENDTRY.

**//.. Layout
  lr_layout     = gr_table->get_layout( ).
  ls_key-report = sy-repid.
  lr_layout->set_key( ls_key ).

**//..  set usage of default Layouts
  lr_layout->set_default( abap_true ).

**//.. set Layout save restriction
  lr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
*  lr_layout->set_initial_layout( pa_vari ).
  lr_display = gr_table->get_display_settings( ).
  lr_display->set_striped_pattern( cl_salv_display_settings=>true ).

  lv_title = sy-title.
  lr_display->set_list_header( lv_title ).

**//.. Enable Generic ALV functions
  gr_functions = gr_table->get_functions( ).
  gr_functions->set_all( ).

**//.. Tipo de selección
  lr_selections = gr_table->get_selections(  ).
  lr_selections->set_selection_mode( lr_selections->none ).

**//.. optimize output
  lr_columns = gr_table->get_columns( ).
  lr_columns->set_optimize( abap_true ).

**//.. Ordenar catalogo
*  ma_set_column_position 'CAMPO'  1.


*  TRY .
**//.. Renombrar columnas
*      lr_column = lr_columns->get_column( 'CAMPO' ).
*      lr_column->set_long_text( 'Importe Factura' ).
*      lr_column->set_medium_text( 'Importe Factura' ).
*      lr_column->set_short_text( 'Impte.Fact' ).
*      lr_column->set_currency_column( 'WAERS' ).
*    CATCH cx_salv_not_found.
*  ENDTRY.
  TRY.
      lr_columno ?= lr_columns->get_column( 'RECIB' ).
      lr_columno->set_long_text( 'Documentos Recibidos' ).
      lr_columno->set_medium_text( 'Doctos. Recibidos' ).
      lr_columno->set_short_text( 'Doc.Recib.' ).
      lr_columno->set_text_column( 'Documentos Recibidos' ). "#EC NOTEXT
    CATCH cx_salv_not_found.
  ENDTRY.

  TRY.
      lr_columno ?= lr_columns->get_column( 'RECHA' ).
      lr_columno->set_long_text( 'Documentos Rechazados' ).
      lr_columno->set_medium_text( 'Doctos. Rechazados' ).
      lr_columno->set_short_text( 'Doc.Recha.' ).
      lr_columno->set_text_column( 'Documentos Rechazados' ). "#EC NOTEXT
    CATCH cx_salv_not_found.
  ENDTRY.

  TRY.
      lr_columno ?= lr_columns->get_column( 'OKCOC' ).
      lr_columno->set_long_text( 'Doctos. con éxito O/C' ).
      lr_columno->set_medium_text( 'Doctos. éxito O/C' ).
      lr_columno->set_short_text( 'Doc.Ok.O/C' ).
      lr_columno->set_text_column( 'Doc.Ok.O/C' ).          "#EC NOTEXT
    CATCH cx_salv_not_found.
  ENDTRY.

  TRY.
      lr_columno ?= lr_columns->get_column( 'TMPROM' ).
      lr_columno->set_long_text( 'Tiempo Prom. Contabilización' ).
      lr_columno->set_medium_text( 'Tiempo Prom. Contab.' ).
      lr_columno->set_short_text( 'Tp.Prom.CB' ).
      lr_columno->set_text_column( 'Tiempo Prom. Contabilización' ). "#EC NOTEXT
    CATCH cx_salv_not_found.
  ENDTRY.

  TRY.
**// Quitar Columnas del alv
      lr_column ?= lr_columns->get_column( 'TMPACU' ).
      lr_column->set_technical( abap_true ).

      lr_column ?= lr_columns->get_column( 'TMPCON' ).
      lr_column->set_technical( abap_true ).
    CATCH cx_salv_not_found.
  ENDTRY.
*Para que aparezca los ceros en el campo
*    lr_column = lr_columns->get_column( 'CAMPO ).
*    lr_column->set_leading_zero( 'X' ).

**//.. Ordenamiento
**//.. Fill the sort table
  lr_sorts = gr_table->get_sorts( ).
  TRY.
      CALL METHOD lr_sorts->add_sort
        EXPORTING
          columnname = 'BUKRS'
          position   = 1
          sequence   = if_salv_c_sort=>sort_up
          subtotal   = if_salv_c_bool_sap=>false
          group      = 1
          obligatory = if_salv_c_bool_sap=>false
        RECEIVING
          value      = lr_sort.

      CALL METHOD lr_sorts->add_sort
        EXPORTING
          columnname = 'ZDTE_TIPO'
          position   = 2
          sequence   = if_salv_c_sort=>sort_up
          subtotal   = if_salv_c_bool_sap=>false
          group      = 1
          obligatory = if_salv_c_bool_sap=>false
        RECEIVING
          value      = lr_sort.
    CATCH cx_salv_not_found .
    CATCH cx_salv_existing .
    CATCH cx_salv_data_error .
  ENDTRY.

**//.. Mostrar ALV
  gr_table->display( ).

ENDFORM.                    "fo_display_alv

*&---------------------------------------------------------------------*
*&      Form  FO_GET_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_VG_MENS  text
*----------------------------------------------------------------------*
FORM fo_get_info  CHANGING c_mens.
  DATA: lt_ztfi_0074     TYPE tt_ztfi_0074,
        lt_ztfi_0074_tp2 TYPE tt_ztfi_0074,
        lt_awkey         TYPE TABLE OF ty_awkey,
        ls_awkey         TYPE bkpf-awkey,
        lv_buzei         TYPE bseg-buzei,
        lv_awtyp         TYPE bkpf-awtyp VALUE 'RMRP',
        lv_logsys        TYPE tbdls-logsys.
  FIELD-SYMBOLS: <fs_ztfi_0074> TYPE ty_ztfi_0074.
**//..
  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
    IMPORTING
      own_logical_system             = lv_logsys
    EXCEPTIONS
      own_logical_system_not_defined = 1
      OTHERS                         = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

**//.. Buscar Facturas Procesadas
  SELECT a~bukrs a~xblnr  a~lifnr a~tipodte a~bldat a~belnr a~gjahr
         a~ebeln a~status a~cpudt a~cputm   b~zdte_tipo
    INTO TABLE gt_ztfi_0074
    FROM ztfi_0074 AS a INNER JOIN lfb1 AS b
                                ON b~lifnr EQ a~lifnr
                               AND b~bukrs EQ a~bukrs
    WHERE a~bukrs  IN so_bukrs
*     AND a~xblnr   IN s_xblnr
      AND a~lifnr     IN so_lifnr
      AND a~bldat     IN so_bldat
      AND a~stcd1     IN so_stcd1
      AND a~status    IN so_statu
      AND b~zdte_tipo IN so_dtetp.
  IF sy-subrc NE 0.
    MESSAGE i017(m5) INTO c_mens.
  ELSE.
**//.. Verificar si tiene documentos FI
    APPEND LINES OF gt_ztfi_0074 TO lt_ztfi_0074.
    DELETE lt_ztfi_0074 WHERE belnr EQ space.
    IF LINES( lt_ztfi_0074 ) GT 0.
**//.. Diferenciar documentos FI tipo proveedor 2 y diferentes de 2
      APPEND LINES OF lt_ztfi_0074 TO lt_ztfi_0074_tp2.
      DELETE lt_ztfi_0074_tp2 WHERE zdte_tipo NE '2'.
      DELETE lt_ztfi_0074     WHERE zdte_tipo EQ '2'.
**//.. Tipo DTE 2
      IF LINES( lt_ztfi_0074_tp2 ) GT 0.
        SELECT bukrs belnr gjahr bstat
               budat bldat cpudt cputm
               awtyp awkey awsys
               INTO TABLE gt_bkpf
        FROM bkpf
        FOR ALL ENTRIES IN lt_ztfi_0074_tp2
        WHERE bukrs EQ lt_ztfi_0074_tp2-bukrs
          AND belnr EQ lt_ztfi_0074_tp2-belnr
          AND gjahr EQ lt_ztfi_0074_tp2-gjahr.
      ENDIF.
**//.. Distinto Tipo DTE 2
      IF LINES( lt_ztfi_0074 ) GT 0.
        LOOP AT lt_ztfi_0074 ASSIGNING <fs_ztfi_0074>.
          CLEAR: ls_awkey.
          CONCATENATE <fs_ztfi_0074>-belnr <fs_ztfi_0074>-gjahr
                      INTO ls_awkey.
          APPEND ls_awkey TO lt_awkey.
        ENDLOOP.

**//..
        SELECT bukrs belnr gjahr bstat
               budat bldat cpudt cputm
               awtyp awkey awsys
               APPENDING TABLE gt_bkpf
        FROM bkpf
        FOR ALL ENTRIES IN lt_awkey
        WHERE awtyp   EQ lv_awtyp
          AND awkey   EQ lt_awkey-value
          AND ( awsys EQ lv_logsys
           OR   awsys EQ space ).
      ENDIF.

      IF LINES( gt_bkpf ) GT 0.
        SELECT bukrs belnr gjahr
               buzei lifnr gsber
               INTO TABLE gt_bseg
        FROM bseg
        FOR ALL ENTRIES IN gt_bkpf
        WHERE bukrs EQ gt_bkpf-bukrs
          AND belnr EQ gt_bkpf-belnr
          AND gjahr EQ gt_bkpf-gjahr
          AND buzei NE lv_buzei
          AND koart EQ co_koart.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " FO_GET_INFO

*&---------------------------------------------------------------------*
*&      Form  FO_DETERMINE_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_START_DATE  text
*      -->P_END_DATE  text
*      -->P_START_TIME  text
*      -->P_END_TIME  text
*      <--P_MINUTE  text
*----------------------------------------------------------------------*
FORM fo_determine_time  USING    start_date TYPE dats
                                 end_date   TYPE dats
                                 start_time TYPE tims
                                 end_time   TYPE tims
                        CHANGING minutes.
  DATA: lv_seconds TYPE sytabix,
        lv_factor  TYPE p DECIMALS 8,
        lv_decimal TYPE p DECIMALS 2,
        lv_minutes TYPE p DECIMALS 2.

**//..
  CALL FUNCTION 'SWI_DURATION_DETERMINE'
    EXPORTING
      start_date = start_date
      end_date   = end_date
      start_time = start_time
      end_time   = end_time
    IMPORTING
      duration   = lv_seconds.

*  minutes = ( lv_seconds MOD 3600 ) DIV 60.
  lv_factor = 60 / 3600.
  lv_minutes = lv_seconds * lv_factor.
  lv_decimal = frac( lv_minutes ).  " extraer la parte decimal
  lv_minutes = trunc( lv_minutes ). " extraer la parte entera
  lv_seconds = lv_decimal * 60.
  lv_minutes = lv_minutes + ( lv_seconds / 100 ).
  minutes    = lv_minutes.
ENDFORM.                    " FO_DETERMINE_TIME

*&---------------------------------------------------------------------*
*&      Form  FO_VALIDAR_SOCIEDAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fo_validar_sociedad .
  DATA: ls_t001 TYPE ty_t001.

**//..
  SELECT bukrs butxt INTO TABLE gt_t001
  FROM t001
  WHERE bukrs IN so_bukrs.
  IF sy-subrc EQ 0.
    LOOP AT gt_t001 INTO ls_t001.
      AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
               ID 'BUKRS' FIELD ls_t001-bukrs
               ID 'ACTVT' FIELD '01'
               ID 'ACTVT' FIELD '02'.
      IF sy-subrc NE 0.
        MESSAGE e800(fr) WITH ls_t001-bukrs.
      ENDIF.
    ENDLOOP.
  ENDIF.

*  AUTHORITY-CHECK OBJECT 'ZFI01'
*           ID 'BUKRS' FIELD pa_bukrs-low
*           ID 'ACTVT' FIELD '03'.
*  IF sy-subrc NE 0.
*    MESSAGE e460(f5) WITH pa_bukrs-low.
*  ENDIF.
ENDFORM.                    " FO_VALIDAR_SOCIEDAD

*&---------------------------------------------------------------------*
*&      Form  FO_PROC_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--C_MENS  text
*----------------------------------------------------------------------*
FORM fo_proc_info  CHANGING c_mens.
  DATA: ls_salida    TYPE ty_salida,
        ls_bkpf      TYPE ty_bkpf,
        ls_bseg      TYPE ty_bseg,
        ls_lfb1      TYPE ty_lfb1,
        lv_awkey     TYPE bkpf-awkey.

  FIELD-SYMBOLS: <fs_ztfi_0074> TYPE ty_ztfi_0074,
                 <fs_salida>    TYPE ty_salida.

**//..
  LOOP AT gt_ztfi_0074 ASSIGNING <fs_ztfi_0074>.
    CLEAR: ls_bkpf,
           lv_awkey.
    MOVE-CORRESPONDING <fs_ztfi_0074> TO ls_salida.

**//.. Verificar estatus
    CASE <fs_ztfi_0074>-status.
      WHEN '5'. " Aceptado
        ADD 1 TO ls_salida-recib.

        IF <fs_ztfi_0074>-ebeln IS NOT INITIAL.
          ADD 1 TO ls_salida-okcoc.
        ENDIF.
      WHEN '6'. " Rechazado
        ADD 1 TO ls_salida-recha.
      WHEN OTHERS.

    ENDCASE.

**//.. Verificar documento FI
    CASE <fs_ztfi_0074>-zdte_tipo.
      WHEN '2'.
        READ TABLE gt_bkpf WITH KEY bukrs = <fs_ztfi_0074>-bukrs
                                    belnr = <fs_ztfi_0074>-belnr
                                    gjahr = <fs_ztfi_0074>-gjahr
                           INTO ls_bkpf.
      WHEN OTHERS.
        CLEAR: lv_awkey.

        CONCATENATE <fs_ztfi_0074>-belnr <fs_ztfi_0074>-gjahr
                    INTO lv_awkey.

        READ TABLE gt_bkpf WITH KEY awkey = lv_awkey
                           INTO ls_bkpf.
    ENDCASE.

    IF  sy-subrc EQ 0
    AND ls_bkpf-bstat IS INITIAL.
      " Diferencia fechas Doc. FI y Llegada XML
      PERFORM fo_determine_time USING    <fs_ztfi_0074>-cpudt
                                         ls_bkpf-cpudt
                                         <fs_ztfi_0074>-cputm
                                         ls_bkpf-cputm
                                CHANGING ls_salida-tmpacu.
      ADD 1 TO ls_salida-tmpcon.

      READ TABLE gt_bseg WITH KEY bukrs = <fs_ztfi_0074>-bukrs
                                  belnr = ls_bkpf-belnr
                                  gjahr = ls_bkpf-gjahr
                                  lifnr = <fs_ztfi_0074>-lifnr
                                  INTO ls_bseg.
      IF sy-subrc EQ 0.
        MOVE ls_bseg-gsber TO ls_salida-gsber.
      ENDIF.
    ENDIF.

**//..
    COLLECT ls_salida INTO gt_salida.
    CLEAR: ls_salida.
  ENDLOOP.

**//.. Calcular promedio de contabilización
  LOOP AT gt_salida ASSIGNING <fs_salida>.
    IF <fs_salida>-tmpcon IS NOT INITIAL.
      <fs_salida>-tmprom = <fs_salida>-tmpacu / <fs_salida>-tmpcon.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " FO_PROC_INFO
