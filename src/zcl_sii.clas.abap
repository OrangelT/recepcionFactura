CLASS zcl_sii DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_docfi,
        bukrs TYPE bukrs,
        belnr TYPE belnr,
        gjahr TYPE gjahr,
        zlspr TYPE dzlspr,
        empfb TYPE empfb,
      END OF ty_docfi .
    TYPES:
      t_docfi TYPE STANDARD TABLE OF ty_docfi .
    TYPES:
      BEGIN OF tr_lines,
        out_lines(76) TYPE c,
      END OF tr_lines .
    TYPES:
      BEGIN OF ty_data,
        nro         TYPE string,
        tipo_doc    TYPE string,
        tipo_compra TYPE string,
        campo       TYPE string,
      END OF ty_data .
    TYPES:
      t_data TYPE STANDARD TABLE OF ty_data .
    TYPES:
      t_ztfi_0074fr TYPE STANDARD TABLE OF ztfi_0074fr .
    TYPES:
      tr_bukrs TYPE RANGE OF bukrs .
    TYPES:
      tr_fecha TYPE RANGE OF syst_datum .

    DATA gs_semilla TYPE string VALUE '0343434343' ##NO_TEXT.
    DATA gs_token TYPE string .
    DATA xml_out TYPE string .
    DATA lo_object TYPE REF TO cl_sec_sxml_dsignature .
    DATA lf_bindata TYPE xstring .
    DATA sxml_firmado TYPE xstring .
    DATA xml_firmado TYPE string .
    DATA lx_error TYPE REF TO cx_sec_sxml_error .
    DATA lf_msg TYPE string .
    CONSTANTS co_semilla TYPE string VALUE 'Z_DTESEMILLA' ##NO_TEXT.
    CONSTANTS co_token TYPE string VALUE 'Z_DTETOKEN' ##NO_TEXT.
    CONSTANTS co_aceptarecl TYPE string VALUE 'Z_DTEREG' ##NO_TEXT.
    CONSTANTS co_listado TYPE string VALUE 'Z_LISTADO' ##NO_TEXT.
    CONSTANTS co_factory TYPE string VALUE 'Z_FACTORY' ##NO_TEXT.
    DATA gs_rc TYPE sy-subrc VALUE 0 ##NO_TEXT.
    DATA text_error TYPE string .
    DATA firmado_arc TYPE flag VALUE '' ##NO_TEXT.
    DATA lv_file2 TYPE string .
    DATA go_log TYPE REF TO zcl_bal_log_dte .
    DATA gs_bal_log TYPE bal_s_log .
    DATA gs_bal_msg TYPE bal_s_msg .
    DATA xml_firmadostr TYPE string .
    CONSTANTS co_listarec TYPE string VALUE 'Z_DTELISREC' ##NO_TEXT.
    CONSTANTS co_listadofactrecsiinw TYPE string VALUE 'Z_DTELISRECNW' ##NO_TEXT.
    DATA gs_codcersii TYPE zcodcersii .
    DATA lg_min TYPE i .
    DATA lg_cer_ant TYPE zcodcersii .
    DATA lg_cer_a TYPE zcodcersii .
    DATA lg_hora_ant TYPE syst_uzeit .
    DATA lg_fecha_ant TYPE syst-datum .
    DATA lg_auten TYPE c .
    DATA lg_token TYPE string .
    DATA lg_seg TYPE i .

    METHODS constructor .
    METHODS put_acepreclamo
      IMPORTING
        !itab        TYPE zdte_aceprec
      EXPORTING
        !zdte_respar TYPE zdte_respar .
    METHODS write_fechasii
      IMPORTING
        VALUE(s_bukrs)                    TYPE bukrs
      CHANGING
        VALUE(lt_zcb_recfactprovfechasii) TYPE t_ztfi_0074fr .
    METHODS display_log .
    METHODS get_autenticacion
      RETURNING
        VALUE(token) TYPE string .
    METHODS display_eventos
      IMPORTING
        !itab    TYPE zdte_fechasii
        !i_bukrs TYPE bukrs
      EXPORTING
        !lt_eve  TYPE zdte_liseve_t .
    METHODS write_acepreclamo
      IMPORTING
        !itab        TYPE zdte_aceprec
        !i_bukrs     TYPE bukrs
      EXPORTING
        !zdte_respar TYPE zdte_respar .
    METHODS ws_listado_factsii
      IMPORTING
        !s_bukrs   TYPE tr_bukrs
        !p_periodo TYPE spmon .
    METHODS get_parse_listado
      IMPORTING
        !i_anytable    TYPE ANY TABLE
        !i_tabname     TYPE tabname
      EXPORTING
        VALUE(e_tabla) TYPE ANY TABLE .
    METHODS get_listadofacrecsii
      IMPORTING
        !p_bukrs             TYPE bukrs
        !p_rutemisor         TYPE string
        !p_begda             TYPE begda
        !p_endda             TYPE endda
      CHANGING
        VALUE(lt_facturasii) TYPE t_ztfi_0074fr .
    METHODS ws_listado_factrecsii
      IMPORTING
        !s_bukrs TYPE tr_bukrs
        !p_begda TYPE begda
        !p_endda TYPE endda .
    METHODS get_factorin
      IMPORTING
        !p_rutemisor         TYPE string
        !p_dvemisor          TYPE string
        !p_tipodoc           TYPE string
        !p_folio             TYPE string
        !p_rutempresa        TYPE string
        !p_dvempresa         TYPE string
      RETURNING
        VALUE(r_rut_tenedor) TYPE string .
    METHODS ws_factorin
      IMPORTING
        VALUE(s_bukrs) TYPE tr_bukrs
        VALUE(p_fecha) TYPE tr_fecha
      EXPORTING
        !e_factory_tab TYPE ztt_dte_factory .
    METHODS update_doc_fi
      IMPORTING
        !lt_docfi      TYPE ztt_dte_factory
        VALUE(go_logf) TYPE REF TO zcl_bal_log_dte OPTIONAL .
    METHODS ws_listado_factrecsiinw
      IMPORTING
        !s_bukrs   TYPE tr_bukrs
        !p_periodo TYPE spmon .
    CLASS-METHODS envio_correo
      IMPORTING
        !s_bukrs    TYPE bukrs
        !glosa      TYPE string
        !asunto     TYPE so_obj_des
        !clasetexto TYPE tdobname
        !documento  TYPE char20 .
    METHODS valida_cert
      RETURNING
        VALUE(r_token) TYPE string .
    METHODS main
      IMPORTING
        VALUE(r_bukrs)  TYPE tr_bukrs OPTIONAL
        VALUE(pa_fldir) TYPE flag OPTIONAL .
    METHODS arc_factorin
      IMPORTING
        VALUE(s_bukrs) TYPE tr_bukrs
        VALUE(p_fecha) TYPE tr_fecha
        VALUE(p_dir)   TYPE flag OPTIONAL .
    METHODS mov_archivos
      IMPORTING
        !file_o       TYPE pfeflnamel
        !file_d       TYPE pfeflnamel
        !nomb_archivo TYPE pfeflnamel
        !arch         TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA go_xml TYPE REF TO cl_xml_document .

    METHODS get_arclocfactorin
      IMPORTING
        !s_bukrs        TYPE tr_bukrs
      RETURNING
        VALUE(lt_tabla) TYPE string_t .
    METHODS proce_regarcfactorin
      IMPORTING
        !lt_tabla           TYPE string_t
        !s_bukrs            TYPE tr_bukrs
        VALUE(nomb_archivo) TYPE pfeflnamel OPTIONAL .
    METHODS get_listadofactrecsiinw
      IMPORTING
        !p_bukrs             TYPE bukrs
        !p_rutemisor         TYPE string
        !p_dvemisor          TYPE string
        !p_periodo           TYPE spmon
        !p_tipodoc           TYPE ztipodte
      CHANGING
        VALUE(lt_facturasii) TYPE t_ztfi_0074fr .
    METHODS get_hisfolio
      IMPORTING
        !itab             TYPE zdte_fechasii
      EXPORTING
        !zdte_listeventos TYPE zdte_liev01 .
    METHODS get_fechasii
      IMPORTING
        !itab               TYPE zdte_fechasii
      RETURNING
        VALUE(fecharecpsii) TYPE string .
    METHODS get_xml .
    METHODS get_token .
    METHODS get_xmlfirmado .
    METHODS get_tokenpost
      IMPORTING
        !ls_destp    TYPE rfcdest OPTIONAL
      RETURNING
        VALUE(token) TYPE string .
    METHODS get_semilla .
    METHODS get_firmado_arc .
    METHODS get_listadofactsii
      IMPORTING
        !p_bukrs             TYPE bukrs
        !p_rutemisor         TYPE string
        !p_dvemisor          TYPE string
        !p_periodo           TYPE spmon
      CHANGING
        VALUE(lt_facturasii) TYPE t_ztfi_0074fr .
    METHODS get_arcserfactorin
      IMPORTING
        !s_bukrs        TYPE tr_bukrs
      RETURNING
        VALUE(lt_tabla) TYPE string_t .
ENDCLASS.



CLASS ZCL_SII IMPLEMENTATION.


  METHOD arc_factorin.
    DATA: lt_tabla TYPE string_t.

**//..

    "---> Crea Objeto Logs de interfaz.
    CONCATENATE sy-datum sy-uzeit
                INTO gs_bal_log-extnumber.

    gs_bal_log-object     = 'ZDTE'.
    gs_bal_log-subobject  = 'FACTORIN'.
    gs_bal_log-aldate     = syst-datum.
    gs_bal_log-altime     = syst-uzeit.
    gs_bal_log-aluser     = syst-uname.
    gs_bal_log-alprog     = syst-repid.

    FREE go_log.
    CREATE OBJECT go_log
      EXPORTING
        i_s_object = gs_bal_log.

**//..
    CASE p_dir.
      WHEN abap_true. " servidor
**//.. Leer archivo de servidor obtener registros en tabla y Procesa Registros
        CALL METHOD me->get_arcserfactorin
          EXPORTING
            s_bukrs  = s_bukrs
          RECEIVING
            lt_tabla = lt_tabla.

      WHEN OTHERS.    " local pc
**//.. Leer archivo local obtener registros en tabla y Procesa registros
        CALL METHOD me->get_arclocfactorin
          EXPORTING
            s_bukrs  = s_bukrs
          RECEIVING
            lt_tabla = lt_tabla.


    ENDCASE.

  ENDMETHOD.


  METHOD constructor.
    CREATE OBJECT go_xml. "Propiedades de XML

  ENDMETHOD.


  METHOD display_eventos.
    DATA: ls_dest          TYPE rfcdest,
          ls_token         TYPE string,
          ws_string        TYPE string,
          http_status_code TYPE i,
          status_text      TYPE string,
          xml              TYPE xstring,
          xml_result       TYPE xstring,
          w_result         TYPE string,
          ls_xmltable      TYPE  smum_xmltb,
          lo_err           TYPE REF TO cx_root,
          lv_err_string    TYPE string,
          len_str          TYPE string,
          ws_resp          TYPE string,
          xml_table        TYPE STANDARD TABLE OF smum_xmltb,
          wa_s_msg         TYPE symsg,
          return           TYPE STANDARD TABLE OF  bapiret2,
          ls_eventos       TYPE zdte_liev01,
          lt_repeventos    TYPE zdte_liseve_t,
          wa_respeve       TYPE zdte_liseve,
          cont             TYPE i,
          sw               TYPE i,
          gv_token         TYPE string.


    IF go_log IS NOT BOUND.
      "---> Crea Objeto Logs de interfaz.
      CONCATENATE sy-datum sy-uzeit
                  INTO gs_bal_log-extnumber.
*   GS_BAL_LOG-EXTNUMBER  = 'BUKRSRUTEMISORRXBLNR'.
      gs_bal_log-object     = 'ZDTE'.
      gs_bal_log-subobject  = 'RECEPCION'.
      gs_bal_log-aldate     = syst-datum.
      gs_bal_log-altime     = syst-uzeit.
      gs_bal_log-aluser     = syst-uname.
      gs_bal_log-alprog     = syst-repid.
      FREE go_log.
      CREATE OBJECT go_log
        EXPORTING
          i_s_object = gs_bal_log.
    ENDIF.
    "----> Autenticar al sii
    sw = 0.
    "----> Cod.CertificadoSociedad
    CLEAR  gs_codcersii.
    SELECT codcersii INTO gs_codcersii  FROM ztfi_0078 UP TO 1 ROWS
    WHERE bukrs EQ i_bukrs.
    ENDSELECT.
    IF gs_codcersii IS INITIAL.
      gs_codcersii = 'Z_SII'.
    ENDIF.
    gv_token = gs_token.
    IF gv_token IS INITIAL.
      WHILE sw = 0. "Pedir 5 veces el token
        cont = cont + 1.
        CALL METHOD me->get_autenticacion
          RECEIVING
            token = gv_token.
        IF gv_token IS NOT INITIAL.
          sw = 1.
        ENDIF.
        IF cont = 5.
          sw = 1.
        ENDIF.
      ENDWHILE.
    ENDIF.
    IF gv_token IS NOT INITIAL.
      CALL METHOD me->get_hisfolio
        EXPORTING
          itab             = itab
        IMPORTING
          zdte_listeventos = ls_eventos.
      IF sy-subrc = 0.
        CLEAR  wa_respeve.
        IF ls_eventos IS NOT INITIAL.
          IF ls_eventos-body-listaeven-return-listaeventosdoc[] IS NOT INITIAL.
            lt_repeventos[] = ls_eventos-body-listaeven-return-listaeventosdoc[].
          ELSE.
            CLEAR wa_respeve.
            wa_respeve-codevento = ls_eventos-body-listaeven-return-codresp.
            wa_respeve-descevento = ls_eventos-body-listaeven-return-descresp.
            APPEND wa_respeve TO lt_repeventos.
          ENDIF.
        ELSE.

        ENDIF.

        lt_eve[] = lt_repeventos[].

        "--> Error en Ws
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD display_log.
    go_log->display( ).
  ENDMETHOD.


  METHOD envio_correo.
    DATA: wa_ztfi_0078 TYPE ztfi_0078.

    DATA: lv_correo         TYPE comm_id_long,
          i_text            TYPE STANDARD TABLE OF solisti1,
          r_text            TYPE solisti1,
          wa_mail           TYPE ztfi_0078-correo_texto,
          lt_mailsubject    TYPE sodocchgi1,
          lt_mailrecipients TYPE STANDARD TABLE OF somlreci1,
          ls_mailrecipients TYPE  somlreci1,
          lt_mailtxt        TYPE STANDARD TABLE OF solisti1,
          ls_mailtxt        LIKE LINE OF  lt_mailtxt,
          lv_texto          TYPE char200,
          lt_lineas         TYPE STANDARD TABLE OF tline,
          ls_lineas         LIKE LINE OF lt_lineas,
          lv_sender         TYPE soextreci1-receiver,
          "--->si existe emisor se envia el correo.
          lv_lineas_txt     TYPE i,
          lt_objpack        TYPE STANDARD TABLE OF sopcklsti1,
          wa_objpack        LIKE LINE OF lt_objpack,
          lv_tipo           TYPE  so_adr_typ.


    CLEAR wa_ztfi_0078. CLEAR  lv_correo. CLEAR ls_mailrecipients.
    CLEAR lt_mailsubject.

    SELECT SINGLE * FROM ztfi_0078 INTO wa_ztfi_0078
    WHERE bukrs = s_bukrs.

    TRANSLATE wa_ztfi_0078-correo_idoc TO LOWER CASE.
    lv_correo = wa_ztfi_0078-correo_idoc.

    TRANSLATE wa_ztfi_0078-sender TO LOWER CASE.
    "-----Envío de e-mail.


* Recipients
    ls_mailrecipients-rec_type  = 'U'.
    ls_mailrecipients-receiver = lv_correo.
    APPEND ls_mailrecipients TO  lt_mailrecipients.
    CLEAR ls_mailrecipients .
* Subject.
    CLEAR lv_texto.
    lv_texto = asunto.
    lt_mailsubject-obj_name = 'EMAIL'.
    lt_mailsubject-obj_langu = sy-langu.
    lt_mailsubject-obj_descr =  lv_texto.
    "---->texto creado

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client                  = sy-mandt
        id                      = 'ST'
        language                = sy-langu
        name                    = clasetexto
        object                  = 'TEXT'
      TABLES
        lines                   = lt_lineas
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc  = 0.
      FREE lt_mailtxt. CLEAR ls_mailtxt.
      TRANSLATE wa_ztfi_0078-url_texto TO LOWER CASE.
      TRANSLATE wa_ztfi_0078-correo_texto TO LOWER CASE.

      LOOP AT lt_lineas INTO ls_lineas.
        CLEAR ls_mailtxt.
        ls_mailtxt = ls_lineas-tdline.

        REPLACE ALL OCCURRENCES OF '(GLOSA)'  IN ls_mailtxt WITH glosa.
        REPLACE ALL OCCURRENCES OF '(DOCUMENTO)'   IN ls_mailtxt WITH documento.
        REPLACE ALL OCCURRENCES OF '(CORREO)' IN ls_mailtxt WITH wa_ztfi_0078-correo_texto.

        APPEND ls_mailtxt TO lt_mailtxt.
      ENDLOOP.
    ENDIF.
    "---->se agrega correo emisor.
    IF wa_ztfi_0078-sender IS NOT INITIAL.
      lv_sender = wa_ztfi_0078-sender.
      DESCRIBE TABLE lt_mailtxt LINES lv_lineas_txt.
      CLEAR wa_objpack-transf_bin.
      wa_objpack-head_start = 1.
      wa_objpack-head_num = 0.
      wa_objpack-body_start = 1.
      wa_objpack-body_num = lv_lineas_txt.
      wa_objpack-doc_type = 'RAW'.
      APPEND wa_objpack TO lt_objpack.

      lv_tipo = 'SMTP'.

      CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
        EXPORTING
          document_data              = lt_mailsubject
          sender_address             = lv_sender
          sender_address_type        = lv_tipo
        TABLES
          packing_list               = lt_objpack
          contents_txt               = lt_mailtxt
          receivers                  = lt_mailrecipients
        EXCEPTIONS
          too_many_receivers         = 1
          document_not_sent          = 2
          document_type_not_exist    = 3
          operation_no_authorization = 4
          parameter_error            = 5
          x_error                    = 6
          enqueue_error              = 7
          OTHERS                     = 8.
      IF sy-subrc EQ 0.
*        IF i_commit IS INITIAL.
        COMMIT WORK.
*        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_arclocfactorin.

    DATA: ls_opsystem       TYPE opsystem,
          lv_sep            TYPE char01,
          lv_existe         TYPE abap_bool,
          lv_dirname        TYPE pfeflnamel,
          lv_dirproc        TYPE pfeflnamel,
          lv_filename       TYPE string,
          lv_fname          TYPE pfeflnamel,
          lv_filenam        TYPE pfeflnamel,
          lt_return         TYPE bapireturn_t,
          ls_return         TYPE bapiret2,
          lt_file           TYPE STANDARD TABLE OF salfldir,
          lt_dir_list       TYPE STANDARD TABLE OF eps2fili,
          ls_dir_list       TYPE eps2fili,
          lt_cabarc         TYPE TABLE OF string,
          lt_listadocab     TYPE TABLE OF zdte_listafaccab,
          ls_cab            TYPE zdte_listafaccab,
          lt_listadofac     TYPE TABLE OF zdte_listafar,
          lt_fi0074fr       TYPE TABLE OF ztfi_0074fr,
          lt_fi0088         TYPE TABLE OF ztfi_0088,
          ls_fi0088         TYPE ztfi_0088,
          ls_factory        TYPE zst_dte_factory,
          lt_t001z          TYPE TABLE OF t001z,
          ls_t001z          TYPE t001z,
          ls_user_dir       TYPE user_dir,
          lv_iso            TYPE cpcodepage,
          lv_cont           TYPE i,
          lv_accion         TYPE c,
          lv_file_dir       TYPE pfeflnamel,
          lv_line           TYPE string,
          lv_pfeflname      TYPE pfeflnamel,
          lv_all_lines      TYPE string,
          ls_string         TYPE string,
          lv_err_string     TYPE string,
          lo_err            TYPE REF TO cx_sy_file_open_mode,
          lo_cx_root        TYPE REF TO cx_root,
          ls_bukrs          LIKE LINE OF s_bukrs,
          lv_rutempresa(16) TYPE c,
          lv_rutarchivo(16) TYPE c,
          lv_nom(16)        TYPE c,
          lv_error          TYPE abap_bool,
          lv_leng           TYPE i,
          go_file           TYPE REF TO  zcl_dte_file_proc,
          ls_lifnr          TYPE lifnr,
          ls_doc            TYPE char20,
          lv_glosa          TYPE string,
          e_factory_tab     TYPE ztt_dte_factory,
          lv_num            TYPE i,
          p_tipodoc         TYPE ztipodte.


    CLEAR ls_fi0088. REFRESH lt_fi0088.
    "---> Leer RUT Sociedad
    SELECT * INTO TABLE lt_t001z
    FROM t001z
    WHERE bukrs IN s_bukrs
      AND party EQ 'TAXNR'.

    "---> Leer tabla de directorios
    SELECT  * INTO TABLE lt_fi0088
    FROM ztfi_0088
    WHERE sysid EQ sy-sysid
      AND bukrs IN s_bukrs.
    IF sy-subrc NE 0.
      "---> Logs de interfaz.
      CLEAR gs_bal_msg.
      gs_bal_msg-msgty = 'E'.
      gs_bal_msg-msgid = 'ZDTE_0001'.
      gs_bal_msg-msgno = '023'.
      go_log->add_msg( i_s_msg = gs_bal_msg ).
      go_log->save( ).
      EXIT.
    ENDIF.
    LOOP AT lt_fi0088 INTO ls_fi0088.

      "Validar Directorio Procesados
      IF ls_fi0088-zdir_prcxml IS INITIAL
      OR ls_fi0088-zdir_prcxml EQ space.
        "---> Logs de interfaz.
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '026'.
        gs_bal_msg-msgv1 = ls_fi0088-zdir_prcxml.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
        CONTINUE."EXIT.
      ENDIF.

      "Customized Table ZTFI_0088 get path
      SELECT SINGLE * INTO ls_user_dir
      FROM user_dir
      WHERE aliass EQ ls_fi0088-zdir_prcxml.
      IF sy-subrc EQ 0.
        lv_dirproc = ls_user_dir-dirname.
      ELSE.
        "---> Logs de interfaz.
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '026'.
        gs_bal_msg-msgv1 = ls_fi0088-zdir_prcxml.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
        CONTINUE. "EXIT.
      ENDIF.

      FREE go_file.
      CREATE OBJECT go_file.

      CALL METHOD go_file->file_open_dialog.

      CALL METHOD go_file->get_filename
        RECEIVING
          r_value = lv_filename.
      CALL METHOD go_file->file_exist
        EXPORTING
          i_filename = lv_filename
        RECEIVING
          r_result   = lv_existe.

      IF lv_existe IS INITIAL.
        " Error
        CALL METHOD go_file->get_result
          RECEIVING
            r_value = lt_return.

        LOOP AT lt_return INTO ls_return.
          MESSAGE ID ls_return-id TYPE ls_return-type
                                  NUMBER ls_return-number
                                  WITH ls_return-message_v1 ls_return-message_v2
                                       ls_return-message_v3 ls_return-message_v4.
        ENDLOOP.
      ELSE.
**//.. Verificar nombre fichero con Rut Sociedad
        CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
          EXPORTING
            full_name     = lv_filename
          IMPORTING
            stripped_name = lv_fname
*           FILE_PATH     =
          EXCEPTIONS
            x_error       = 1
            OTHERS        = 2.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'S'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '012'.
        gs_bal_msg-msgv1 = ls_fi0088-bukrs.
        gs_bal_msg-msgv2 = '-Comienza Arch: '.
        gs_bal_msg-msgv3 = lv_fname.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).

**//.. Buscar RUT Sociedad
        READ TABLE lt_t001z  INTO ls_t001z WITH KEY  bukrs =  ls_fi0088-bukrs.
        IF sy-subrc NE 0.
          "---> Logs de interfaz.

          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'E'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '080'.
          gs_bal_msg-msgv1 = ls_fi0088-bukrs.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
          EXIT.
        ELSE.
          lv_rutempresa = ls_t001z-paval.
          REPLACE ALL OCCURRENCES OF '.' IN  lv_rutempresa WITH ''.
          CONDENSE lv_rutempresa NO-GAPS.
        ENDIF.


**//.. Importo archivo a tabla string
        CALL METHOD go_file->read_file(
          IMPORTING
            t_data = lt_tabla ).


        CALL METHOD go_file->get_result
          RECEIVING
            r_value = lt_return.

        LOOP AT lt_return INTO ls_return.
          MESSAGE ID ls_return-id TYPE ls_return-type
                                  NUMBER ls_return-number
                                  WITH ls_return-message_v1 ls_return-message_v2
                                       ls_return-message_v3 ls_return-message_v4.


        ENDLOOP.


        IF lines( lt_tabla ) GT 0.
**// obtengo archivo en string
          CLEAR ls_string.CLEAR lv_all_lines.
          LOOP AT lt_tabla INTO ls_string.
            CONCATENATE lv_all_lines ls_string cl_abap_char_utilities=>newline INTO lv_all_lines .
          ENDLOOP.
**// Muevo el archivo a procesados y lo borro de original
          CALL METHOD me->mov_archivos
            EXPORTING
              file_o       = space
              file_d       = lv_dirproc
              nomb_archivo = lv_fname
              arch         = lv_all_lines.

**//..Ini. Verificar nombre fichero con Rut Sociedad ,  "CESIONES" y Txt/TXT
          IF NOT lv_fname CS lv_rutempresa.
            "---> Logs de interfaz.
            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'E'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '087'.
            gs_bal_msg-msgv1 = lv_fname.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
            CONTINUE.
          ENDIF.

          IF NOT  lv_fname CS 'CESIONES'.
            "---> Logs de interfaz.
            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'E'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '088'.
            gs_bal_msg-msgv1 = lv_fname.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
            CONTINUE.
          ENDIF.

          IF NOT  lv_fname CS '.TXT'.
            "---> Logs de interfaz.
            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'E'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '089'.
            gs_bal_msg-msgv1 = lv_fname.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
            CONTINUE.
          ENDIF.

          IF NOT  lv_fname CS '.txt'.
            "---> Logs de interfaz.
            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'E'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '089'.
            gs_bal_msg-msgv1 = lv_fname.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
            CONTINUE.
          ENDIF.


**//..Fin Verificar nombre fichero con Rut Sociedad , "CESIONES" y TXT/txt

**//.. Encabezado Con Rut de Empresa Receptora
          CLEAR ls_string. REFRESH lt_cabarc.
          READ TABLE lt_tabla INTO ls_string  INDEX 1.
          APPEND ls_string TO lt_cabarc.

          CALL METHOD me->get_parse_listado
            EXPORTING
              i_anytable = lt_cabarc
              i_tabname  = 'ZDTE_LISTAFACCAB'
            IMPORTING
              e_tabla    = lt_listadocab.

**//.. Busco si la sociedad concuerda con archivo.
          CLEAR: ls_cab,  lv_nom, lv_rutarchivo.
          READ TABLE lt_listadocab INTO ls_cab INDEX 1.
          SPLIT ls_cab-rut AT '=' INTO  lv_nom lv_rutarchivo.
          IF lv_rutarchivo NE lv_rutempresa.
            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'E'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '081'.
            gs_bal_msg-msgv1 = lv_rutempresa.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
            CONTINUE.
          ENDIF.

**//.. Leo archivo archivo/ elimino cabeceras de archivo
          DELETE lt_tabla INDEX 1. "elimino CAbecera 1 de archivo
          IF lines( lt_tabla ) GT 0.
            DELETE lt_tabla INDEX 1. " Elimino Cabecera 2 de archivo.
          ENDIF.
        ENDIF.
      ENDIF.

**//..Procesa registros
      IF lines( lt_tabla ) EQ 0.
        CLEAR lv_num.
        "---> Sin datos de cesion
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'W'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '050'.
        gs_bal_msg-msgv1 = lv_fname.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
**//.. Reviso si hay reprocesos que realizar en la sociedad
        SELECT COUNT(*) INTO lv_num FROM ztfi_0074cd
        WHERE bukrs IN s_bukrs.
        IF lv_num EQ 0.
          CONTINUE. "EXIT.
        ELSE.
          CALL METHOD me->proce_regarcfactorin
            EXPORTING
              s_bukrs      = s_bukrs
              lt_tabla     = lt_tabla
              nomb_archivo = lv_fname.
        ENDIF.
      ELSE.
        CALL METHOD me->proce_regarcfactorin
          EXPORTING
            s_bukrs      = s_bukrs
            lt_tabla     = lt_tabla
            nomb_archivo = lv_fname.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_arcserfactorin.
    DATA: ls_opsystem       TYPE opsystem,
          lv_sep            TYPE char01,
          lv_existe         TYPE abap_bool,
          lv_dirname        TYPE pfeflnamel,
          lv_dirproc        TYPE pfeflnamel,
          lv_filename       TYPE string,
          lv_fname          TYPE string,
          lv_filenam        TYPE pfeflnamel,
          lt_return         TYPE bapireturn_t,
          ls_return         TYPE bapiret2,
          lt_file           TYPE STANDARD TABLE OF salfldir,
          lt_dir_list       TYPE STANDARD TABLE OF eps2fili,
          ls_dir_list       TYPE eps2fili,
          lt_fi0074fr       TYPE TABLE OF ztfi_0074fr,
          lt_fi0088         TYPE TABLE OF ztfi_0088,
          ls_fi0088         TYPE ztfi_0088,
          lt_t001z          TYPE TABLE OF t001z,
          ls_t001z          TYPE t001z,
          ls_user_dir       TYPE user_dir,
          lv_iso            TYPE cpcodepage,
          lv_cont           TYPE i,
          lv_accion         TYPE c,
          lv_file_dir       TYPE pfeflnamel,
          lv_line           TYPE string,
          lv_pfeflname      TYPE pfeflnamel,
          lv_all_lines      TYPE string,
          lv_err_string     TYPE string,
          lt_cabarc         TYPE TABLE OF string,
          ls_string         TYPE string,
          lo_err            TYPE REF TO cx_sy_file_open_mode,
          lo_cx_root        TYPE REF TO cx_root,
          lv_rutempresa(16) TYPE c,
          lv_rutarchivo(16) TYPE c,
          lv_nom(16)        TYPE c,
          lv_error          TYPE abap_bool,
          ls_bukrs          LIKE LINE OF s_bukrs,
          lt_bukrs          TYPE tr_bukrs,
          lt_listadocab     TYPE TABLE OF zdte_listafaccab,
          ls_cab            TYPE zdte_listafaccab,
          lv_num            TYPE i,
          lv_leng           TYPE i.

**//..
    SELECT SINGLE * INTO ls_opsystem
    FROM  opsystem
    WHERE opsys EQ sy-opsys.
    IF ls_opsystem-filesys EQ 'UNIX'.
      lv_sep = '/'.
    ELSE.
      lv_sep = '\'.
    ENDIF.
    "---> Leer RUT Sociedad
    SELECT * INTO TABLE lt_t001z
    FROM t001z
    WHERE bukrs IN s_bukrs
      AND party EQ 'TAXNR'.

    "---> Leer tabla de directorios
    SELECT  * INTO TABLE lt_fi0088
    FROM ztfi_0088
    WHERE sysid EQ sy-sysid
      AND bukrs IN s_bukrs.
    IF sy-subrc NE 0.
      "---> Logs de interfaz.
      CLEAR gs_bal_msg.
      gs_bal_msg-msgty = 'E'.
      gs_bal_msg-msgid = 'ZDTE_0001'.
      gs_bal_msg-msgno = '023'.
      go_log->add_msg( i_s_msg = gs_bal_msg ).
      go_log->save( ).
      EXIT.
    ENDIF.

**//    "--->Busco por sociedad
    LOOP AT lt_fi0088 INTO ls_fi0088.
      "Validar Directorio Adicional
      IF ls_fi0088-zdir_adic IS INITIAL
      OR ls_fi0088-zdir_adic EQ space.
        "---> Logs de interfaz.
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '026'.
        gs_bal_msg-msgv1 = ls_fi0088-zdir_adic.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
        CONTINUE."EXIT.
      ENDIF.

      "Validar Directorio Procesados
      IF ls_fi0088-zdir_prcxml IS INITIAL
      OR ls_fi0088-zdir_prcxml EQ space.
        "---> Logs de interfaz.
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '026'.
        gs_bal_msg-msgv1 = ls_fi0088-zdir_prcxml.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
        CONTINUE."EXIT.
      ENDIF.

      "Customized Table ZTFI_0088 get path
      SELECT SINGLE * INTO ls_user_dir
      FROM user_dir
      WHERE aliass EQ ls_fi0088-zdir_adic.
      IF sy-subrc EQ 0.
        lv_dirname = ls_user_dir-dirname.
      ELSE.
        "---> Logs de interfaz.
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '026'.
        gs_bal_msg-msgv1 = ls_fi0088-zdir_adic.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
        CONTINUE. "EXIT.
      ENDIF.

      "Customized Table ZTFI_0088 get path
      SELECT SINGLE * INTO ls_user_dir
      FROM user_dir
      WHERE aliass EQ ls_fi0088-zdir_prcxml.
      IF sy-subrc EQ 0.
        lv_dirproc = ls_user_dir-dirname.
      ELSE.
        "---> Logs de interfaz.
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '026'.
        gs_bal_msg-msgv1 = ls_fi0088-zdir_prcxml.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
        CONTINUE. "EXIT.
      ENDIF.

**//..
      CLEAR: lt_dir_list.

      CALL FUNCTION 'EPS2_GET_DIRECTORY_LISTING'
        EXPORTING
          iv_dir_name            = lv_dirname
        TABLES
          dir_list               = lt_dir_list
        EXCEPTIONS
          invalid_eps_subdir     = 1
          sapgparam_failed       = 2
          build_directory_failed = 3
          no_authorization       = 4
          read_directory_failed  = 5
          too_many_read_errors   = 6
          empty_directory_list   = 7
          OTHERS                 = 8.
      IF sy-subrc <> 0.

        "---> Logs de interfaz.
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        IF sy-subrc = '7'.
          gs_bal_msg-msgno = '077'.
          gs_bal_msg-msgv1 =  lv_dirname.
        ELSEIF sy-subrc =  '4'.
          gs_bal_msg-msgno = '078'.
          gs_bal_msg-msgv1 =  lv_dirname.
        ELSE.
          gs_bal_msg-msgno = '024'.
          gs_bal_msg-msgv1 =  lv_dirname.
        ENDIF.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
        CONTINUE."EXIT.
      ENDIF.

      lv_cont = 0.
      CLEAR lv_accion.

**//.. Buscar RUT Sociedad
      READ TABLE lt_t001z WITH KEY bukrs = ls_fi0088-bukrs INTO ls_t001z.
      IF sy-subrc NE 0.
        "---> Logs de interfaz.
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '080'.
        gs_bal_msg-msgv1 = ls_fi0088-bukrs.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
        CONTINUE."EXIT.
      ELSE.
        lv_rutempresa = ls_t001z-paval.
        REPLACE ALL OCCURRENCES OF '.' IN  lv_rutempresa WITH ''.
        CONDENSE lv_rutempresa NO-GAPS.
      ENDIF.

**//.. Revisar archivos
      LOOP AT lt_dir_list INTO ls_dir_list
                          WHERE name CS 'txt'
                             OR name CS 'TXT'.
        CLEAR: lv_file_dir, lv_line, lv_all_lines.
        ADD 1 TO lv_cont.
        CONCATENATE lv_dirname lv_sep ls_dir_list-name INTO lv_file_dir.


        CALL FUNCTION 'SCP_CODEPAGE_BY_EXTERNAL_NAME'
          EXPORTING
            external_name = 'ISO-8859-1'
          IMPORTING
            sap_codepage  = lv_iso
          EXCEPTIONS
            not_found     = 1
            OTHERS        = 2.
        IF sy-subrc <> 0.
          CLEAR gs_bal_msg.
          gs_bal_msg-msgty ='E'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '012'.
          gs_bal_msg-msgv1 = lv_err_string.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
          CONTINUE.
        ELSE.
          CLEAR: lt_tabla,
                 lv_err_string.
          TRY.
              " Abrir archivo servidor
              OPEN DATASET lv_file_dir FOR INPUT IN LEGACY TEXT MODE CODE PAGE lv_iso
              MESSAGE lv_err_string.
              IF sy-subrc NE 0.
                CLEAR gs_bal_msg.
                gs_bal_msg-msgty = 'E'.
                gs_bal_msg-msgid = 'ZDTE_0001'.
                gs_bal_msg-msgno = '049'.
                gs_bal_msg-msgv1 = lv_err_string.
                gs_bal_msg-msgv2 = ls_dir_list-name.
                go_log->add_msg( i_s_msg = gs_bal_msg ).
                go_log->save( ).
                CONTINUE.
              ENDIF.

              DO.
                READ DATASET lv_file_dir INTO lv_line.
                IF sy-subrc <> 0.
                  CLOSE DATASET lv_file_dir.
                  EXIT.
                ENDIF.
                APPEND lv_line TO lt_tabla.
                CONCATENATE lv_all_lines lv_line cl_abap_char_utilities=>newline INTO lv_all_lines.
                CLEAR: lv_line.
              ENDDO.

              CLEAR: lv_pfeflname.
              lv_pfeflname = ls_dir_list-name.


              "Agregar Log
              CLEAR gs_bal_msg.
              gs_bal_msg-msgty = 'S'.
              gs_bal_msg-msgid = 'ZDTE_0001'.
              gs_bal_msg-msgno = '079'.
              gs_bal_msg-msgv1 = ls_dir_list-name.
              go_log->add_msg( i_s_msg = gs_bal_msg ).
              "Al finalizar cada registro guardo el log.
              go_log->save( ).

            CATCH cx_sy_file_open_mode INTO lo_err.
              lv_err_string = lo_err->get_text( ).
              CLEAR gs_bal_msg.
              gs_bal_msg-msgty = 'E'.
              gs_bal_msg-msgid = 'ZDTE_0001'.
              gs_bal_msg-msgno = '012'.
              gs_bal_msg-msgv1 = lv_err_string.
              go_log->add_msg( i_s_msg = gs_bal_msg ).
              go_log->save( ).
              CONTINUE.
            CATCH: cx_sy_file_open,
                   cx_sy_codepage_converter_init,
                   cx_sy_conversion_codepage,
                   cx_sy_file_authority,
                   cx_sy_pipes_not_supported,
                   cx_sy_too_many_files INTO lo_cx_root.
              lv_err_string = lo_cx_root->get_text( ).
              CLEAR gs_bal_msg.
              gs_bal_msg-msgty = 'E'.
              gs_bal_msg-msgid = 'ZDTE_0001'.
              gs_bal_msg-msgno = '012'.
              gs_bal_msg-msgv1 = lv_err_string.
              go_log->add_msg( i_s_msg = gs_bal_msg ).
              go_log->save( ).
              CONTINUE.
          ENDTRY.
        ENDIF.

**// Muevo el archivo a procesados y lo borro de original
        CALL METHOD me->mov_archivos
          EXPORTING
            file_o       = lv_dirname
            file_d       = lv_dirproc
            nomb_archivo = ls_dir_list-name
            arch         = lv_all_lines.

**//Inic. Validacion de tamaño de archivo
        IF ls_dir_list-size EQ 0.
          "---> Logs de interfaz.
          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'E'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '039'.
          gs_bal_msg-msgv1 = ls_dir_list-name.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
          CONTINUE.
        ENDIF.
**//Inic. Validacion de tamaño de archivo

**//..Ini. Verificar nombre fichero con Rut Sociedad y "CESIONES"

        IF NOT ls_dir_list-name CS lv_rutempresa.
          "---> Logs de interfaz.
          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'E'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '087'.
          gs_bal_msg-msgv1 = ls_dir_list-name.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
          CONTINUE.
        ENDIF.

        IF NOT  ls_dir_list-name CS 'CESIONES'.
          "---> Logs de interfaz.
          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'E'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '088'.
          gs_bal_msg-msgv1 = ls_dir_list-name.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
          CONTINUE.
        ENDIF.
**//..Fin Verificar nombre fichero con Rut Sociedad y "CESIONES"

        IF lines( lt_tabla ) GT 0.
**//.. Encabezado Con Rut de Empresa Receptora
          CLEAR ls_string. REFRESH lt_cabarc.
          READ TABLE lt_tabla INTO ls_string  INDEX 1.
          APPEND ls_string TO lt_cabarc.

          CALL METHOD me->get_parse_listado
            EXPORTING
              i_anytable = lt_cabarc
              i_tabname  = 'ZDTE_LISTAFACCAB'
            IMPORTING
              e_tabla    = lt_listadocab.

**//.. Busco si la sociedad concuerda con archivo.
          CLEAR: ls_cab,  lv_nom, lv_rutarchivo.
          READ TABLE lt_listadocab INTO ls_cab INDEX 1.
          SPLIT ls_cab-rut AT '=' INTO  lv_nom lv_rutarchivo.
          IF lv_rutarchivo NE lv_rutempresa.
            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'E'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '081'.
            gs_bal_msg-msgv1 = lv_rutempresa.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
            CONTINUE.
          ENDIF.



**//.. Leo archivo archivo/ elimino cabeceras de archivo
          DELETE lt_tabla INDEX 1. "elimino CAbecera 1 de archivo
          IF lines( lt_tabla ) GT 0.
            CLEAR ls_string.
            READ TABLE lt_tabla INTO ls_string  INDEX 1.
            APPEND ls_string TO lt_cabarc. "Guardo encabezado.
            DELETE lt_tabla INDEX 1. " Elimino Cabecera 2 de archivo.
          ENDIF.
        ENDIF.

**//.. Procesar registros
        IF lines( lt_tabla ) EQ 0.
          CLEAR lv_num.
          "---> Sin datos de cesion
          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'W'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '050'.
          gs_bal_msg-msgv1 = ls_dir_list-name.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).

**//.. Reviso si hay reprocesos que realizar en la sociedad
          SELECT COUNT(*) INTO lv_num FROM ztfi_0074cd
          WHERE bukrs EQ  ls_fi0088-bukrs.
          IF lv_num EQ 0.
            CONTINUE.
          ELSE.
            CLEAR ls_bukrs.
            REFRESH lt_bukrs.
            ls_bukrs-low = ls_fi0088-bukrs.
            ls_bukrs-option = 'EQ'.
            ls_bukrs-sign = 'I'.
            APPEND ls_bukrs TO lt_bukrs.
            CALL METHOD me->proce_regarcfactorin
              EXPORTING
                s_bukrs      = lt_bukrs
                lt_tabla     = lt_tabla
                nomb_archivo = ls_dir_list-name.
          ENDIF.
        ELSE.
          CLEAR ls_bukrs.
          REFRESH lt_bukrs.
          ls_bukrs-low = ls_fi0088-bukrs.
          ls_bukrs-option = 'EQ'.
          ls_bukrs-sign = 'I'.
          APPEND ls_bukrs TO lt_bukrs.
          CALL METHOD me->proce_regarcfactorin
            EXPORTING
              s_bukrs      = lt_bukrs
              lt_tabla     = lt_tabla
              nomb_archivo = ls_dir_list-name.
        ENDIF.
      ENDLOOP."Archivos
    ENDLOOP. " LOOP AT lt_fi0088 INTO ls_fi0088.

  ENDMETHOD.


  METHOD get_autenticacion.
    DATA: lt_archivo TYPE STANDARD TABLE OF string.
    FREE lt_archivo.

    IF go_log IS NOT BOUND.
      "---> Crea Objeto Logs de interfaz.
      CONCATENATE sy-datum sy-uzeit
                  INTO gs_bal_log-extnumber.
*   GS_BAL_LOG-EXTNUMBER  = 'BUKRSRUTEMISORRXBLNR'.
      gs_bal_log-object     = 'ZDTE'.
      gs_bal_log-subobject  = 'RECEPCION'.
      gs_bal_log-aldate     = syst-datum.
      gs_bal_log-altime     = syst-uzeit.
      gs_bal_log-aluser     = syst-uname.
      gs_bal_log-alprog     = syst-repid.
      FREE go_log.
      CREATE OBJECT go_log
        EXPORTING
          i_s_object = gs_bal_log.
    ENDIF.

    "---> LLamo al Ws de semilla
    me->get_semilla( ).
    IF gs_rc NE 0.
      token = ''.
      EXIT.
    ENDIF.
    "---> Llamo a rutina para obtener XML
    me->get_xml( ).
    IF gs_rc NE 0.
      token = ''.
      EXIT.
    ENDIF.
    "---> firmo XML
    IF firmado_arc = 'X'.

      me->get_firmado_arc( ).

    ELSE.
      me->get_xmlfirmado( ).
      IF gs_rc NE 0.
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '012'.
        gs_bal_msg-msgv1 = 'Error en Certificado Digital, ver Tx.Strust'.
        gs_bal_msg-msgv2 =  text_error.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
        token = ''.
        EXIT.
      ENDIF.
    ENDIF.
    "---> LLamo a Es Token sII
    me->get_token( ).
    IF gs_rc NE 0.
      token = ''.
      EXIT.
    ELSE.
      token = gs_token.
      CLEAR gs_bal_msg.
      gs_bal_msg-msgty = 'S'.
      gs_bal_msg-msgid = 'ZDTE_0001'.
      gs_bal_msg-msgno = '012'.
      gs_bal_msg-msgv1 = 'Se obtuvo token'.
      go_log->add_msg( i_s_msg = gs_bal_msg ).
      go_log->save( ).
    ENDIF.
  ENDMETHOD.


  METHOD get_factorin.
    DATA: ls_dest          TYPE rfcdest,
          ls_token         TYPE string,
          ws_string        TYPE string,
          w_result         TYPE string,
          ws_resp          TYPE string,
          xml_result       TYPE xstring,
          xml_table        TYPE STANDARD TABLE OF smum_xmltb,
          ls_xmltable      TYPE smum_xmltb,
          lt_return        TYPE STANDARD TABLE OF  bapiret2,
          len_str          TYPE string,
          ls_doc           TYPE char20,
          len              TYPE i,
          lo_err           TYPE REF TO cx_root,
          lv_err_string    TYPE string,
          http_status_code TYPE i,
          status_text      TYPE string,
          ls_msgty         TYPE symsgty,
          ls_resp_escere   TYPE zdte_est_cesi_rel_resp.
*------------------------------
*llamada R3
*------------------------------
    ls_dest = me->co_factory.
    gs_rc = 0.

    CALL METHOD cl_http_client=>create_by_destination
      EXPORTING
        destination              = ls_dest
      IMPORTING
        client                   = DATA(lo_http_client)
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 6.
    IF sy-subrc = 0.
      "----> Datos de cabecera
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = '~request_method'
          value = 'POST'.

      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'Content-Type'
          value = 'text/xml;charset=UTF-8'.

      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'SOAPAction'
          value = ''.


      CONCATENATE
      '<?xml version="1.0" encoding="utf-8"?>'
      '<soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"'
      ' xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"'
      ' xmlns:def="http://DefaultNamespace">'
      '<soapenv:Body>'
        '<def:getEstCesionRelac soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">'
          '<Token xsi:type="xsd:string">' gs_token '</Token>'
          '<RutEmisor xsi:type="xsd:string">' p_rutemisor '</RutEmisor>'
          '<DVEmisor xsi:type="xsd:string">' p_dvemisor '</DVEmisor>'
          '<TipoDoc xsi:type="xsd:string">' p_tipodoc '</TipoDoc>'
          '<FolioDoc xsi:type="xsd:string">' p_folio '</FolioDoc>'
          '<RutEmpresa xsi:type="xsd:string">' p_rutempresa '</RutEmpresa>'
          '<DVEmpresa xsi:type="xsd:string">' p_dvempresa '</DVEmpresa>'
         '</def:getEstCesionRelac>'
      '</soapenv:Body>'
      '</soapenv:Envelope>'
      INTO ws_string.

      len = strlen( ws_string ).
      len_str = len.
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'Content-Length'
          value = len_str.

      CALL METHOD lo_http_client->request->set_cdata
        EXPORTING
          data   = ws_string
          offset = 0
          length = strlen( ws_string ).

*Step-2 : Send request.
      CALL METHOD lo_http_client->send
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2.

*STEP-3 :  GET HTTP RESPONSE
      CALL METHOD lo_http_client->receive
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4.

*STEP-4 : Read HTTP RETURN CODE
      CALL METHOD lo_http_client->response->get_status
        IMPORTING
          code   = http_status_code
          reason = status_text.
      IF http_status_code = '200'. "OK
*STEP-5 :  READ RESPONSE DATA
        CALL METHOD lo_http_client->response->get_cdata
          RECEIVING
            data = w_result.
        w_result = lo_http_client->response->get_cdata( ).
        CLEAR : xml_result.

*Step-6 : convert string to xstring.
        CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
          EXPORTING
            text   = w_result
*           MIMETYPE       = ' '
*           ENCODING       =
          IMPORTING
            buffer = xml_result
          EXCEPTIONS
            failed = 1
            OTHERS = 2.

*Step-7 : create xml
        CALL FUNCTION 'SMUM_XML_PARSE'
          EXPORTING
            xml_input = xml_result
          TABLES
            xml_table = xml_table
            return    = lt_return.
        CLEAR ws_resp.
        LOOP AT xml_table INTO ls_xmltable WHERE cname = 'getEstCesionRelacReturn'.
          CONCATENATE ws_resp ls_xmltable-cvalue INTO ws_resp.
        ENDLOOP.
        TRY.
            CLEAR ls_resp_escere.

            CALL TRANSFORMATION zdte_estcesionrelac
              SOURCE XML  ws_resp
              RESULT respuesta = ls_resp_escere.

          CATCH cx_root INTO lo_err.
            lv_err_string = lo_err->get_text( ).
            gs_rc = 4.
            lo_http_client->close( ).
*            CLEAR gs_bal_msg.
*            gs_bal_msg-msgty = 'E'.
*            gs_bal_msg-msgid = 'ZDTE_0001'.
*            gs_bal_msg-msgno = '012'.
*            gs_bal_msg-msgv1 = gs_rc.
*            MOVE ls_dest TO gs_bal_msg-msgv3.
*            MOVE '108 GET_FACTORY' TO gs_bal_msg-msgv4.
*            MOVE lv_err_string TO gs_bal_msg-msgv2.
*            go_log->add_msg( i_s_msg = gs_bal_msg ).
*            go_log->save( ).
            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'E'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '012'.
            gs_bal_msg-msgv1 = gs_rc.
            CONCATENATE 'WS_Fact/ErrorConver:'  lv_err_string INTO  lv_err_string.
            go_log->contenate_msg(
               EXPORTING texto = lv_err_string
                  descripcion_operacion = ''
                  IMPORTING msgv1 = gs_bal_msg-msgv1
                            msgv2 = gs_bal_msg-msgv2
                            msgv3 = gs_bal_msg-msgv3
                            msgv4 = gs_bal_msg-msgv4 ).
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
            EXIT.
        ENDTRY.
        IF ls_resp_escere-resp_body-rut_tenedor IS NOT INITIAL.
          r_rut_tenedor = ls_resp_escere-resp_body-rut_tenedor.
          gs_rc = 0.
        ELSE.
          gs_rc = 4.
        ENDIF.
        CLEAR ls_msgty.

        CASE ls_resp_escere-resp_hdr-estado.
          WHEN '0'. " Estatus OK
            CONCATENATE  'Tenedor: '   ls_resp_escere-resp_body-rut_tenedor INTO  status_text.
            CONDENSE status_text NO-GAPS.
            ls_msgty = 'S'.
          WHEN '-10'. "Estatus de no exite o no cedido
            status_text = ls_resp_escere-resp_hdr-glosa."Error Mensaje excepcion'.
            ls_msgty = 'W'.
          WHEN OTHERS.
            status_text = ls_resp_escere-resp_hdr-glosa."Error Mensaje excepcion'.
            ls_msgty = 'E'.
        ENDCASE.

        CLEAR ls_doc .
        CONCATENATE p_tipodoc '-' p_folio '-' p_rutemisor INTO ls_doc.
        CONDENSE ls_doc NO-GAPS.
*        CLEAR gs_bal_msg.
*        gs_bal_msg-msgty = ls_msgty. "'E'.
*        gs_bal_msg-msgid = 'ZDTE_0001'.
*        gs_bal_msg-msgno = '012'.
*        gs_bal_msg-msgv1 = ls_doc.
*        MOVE status_text TO gs_bal_msg-msgv2.
*        MOVE ls_dest     TO gs_bal_msg-msgv3.
*        MOVE '162 GET_FACTORY' TO gs_bal_msg-msgv4.
*        go_log->add_msg( i_s_msg = gs_bal_msg ).
*        go_log->save( ).
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '012'.
        gs_bal_msg-msgv1 = gs_rc.
        CONCATENATE 'WS_Fact/Ret:'  ls_doc status_text INTO  status_text.
        go_log->contenate_msg(
           EXPORTING texto = status_text
              descripcion_operacion = ''
              IMPORTING msgv1 = gs_bal_msg-msgv1
                        msgv2 = gs_bal_msg-msgv2
                        msgv3 = gs_bal_msg-msgv3
                        msgv4 = gs_bal_msg-msgv4 ).
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).

      ELSE.
        gs_rc = 4.
        "status_text. Texto de error de comunicación
*        CLEAR gs_bal_msg.
*        gs_bal_msg-msgty = 'E'.
*        gs_bal_msg-msgid = 'ZDTE_0001'.
*        gs_bal_msg-msgno = '012'.
*        gs_bal_msg-msgv1 = gs_rc.
*        MOVE ls_dest TO gs_bal_msg-msgv3.
*        MOVE '176 GET_FACTORY' TO gs_bal_msg-msgv4.
*        MOVE status_text TO gs_bal_msg-msgv1.
*        go_log->add_msg( i_s_msg = gs_bal_msg ).
*        go_log->save( ).
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '012'.
        gs_bal_msg-msgv1 = gs_rc.
        CONCATENATE 'WS_Fact/ErrorComuni:'  status_text INTO  status_text.
        go_log->contenate_msg(
           EXPORTING texto = status_text
              descripcion_operacion = ''
              IMPORTING msgv1 = gs_bal_msg-msgv1
                        msgv2 = gs_bal_msg-msgv2
                        msgv3 = gs_bal_msg-msgv3
                        msgv4 = gs_bal_msg-msgv4 ).
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).

      ENDIF.
      lo_http_client->close( ).
    ELSE.
* Implement suitable error handling here
      gs_rc = sy-subrc.
      CASE gs_rc.
        WHEN '1'.
          status_text = 'argument_not_found'.
        WHEN '2'.
          status_text = 'destination_not_found'.
        WHEN '3'.
          status_text = 'destination_no_authority'.
        WHEN '4'.
          status_text = 'plugin_not_active'.
        WHEN '5'.
          status_text = 'internal_error'.
        WHEN '6'.
          status_text = 'OTHERS'.
      ENDCASE.
      IF gs_rc > 0 .
*        CLEAR gs_bal_msg.
*        gs_bal_msg-msgty = 'E'.
*        gs_bal_msg-msgid = 'ZDTE_0001'.
*        gs_bal_msg-msgno = '012'.
*        gs_bal_msg-msgv1 = gs_rc.
*        MOVE ls_dest TO gs_bal_msg-msgv3.
*        MOVE '207 GET_FACTORY' TO gs_bal_msg-msgv4.
*        MOVE status_text TO gs_bal_msg-msgv1.
*        go_log->add_msg( i_s_msg = gs_bal_msg ).
*        go_log->save( ).
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '012'.
        gs_bal_msg-msgv1 = gs_rc.
        CONCATENATE 'WS_Fact/ErrorConexion:'  status_text INTO  status_text.
        go_log->contenate_msg(
           EXPORTING texto = status_text
              descripcion_operacion = ''
              IMPORTING msgv1 = gs_bal_msg-msgv1
                        msgv2 = gs_bal_msg-msgv2
                        msgv3 = gs_bal_msg-msgv3
                        msgv4 = gs_bal_msg-msgv4 ).
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).

      ENDIF.
    ENDIF.
*------------------------------
* Llamada PI
*------------------------------
* DATA: go_cedido TYPE REF TO zwsco_get_est_cesion_relac_syn,
*          wa_input  TYPE  zwsget_est_cesion_request1,
*          wa_output TYPE zwsget_est_cesion_relac_respo1.
*
*    DATA lv_largo TYPE i.
*    DATA: oref TYPE REF TO cx_ai_system_fault,
*          iref TYPE REF TO cx_ai_application_fault,
*          text TYPE string.
*    DATA: lv_toyear  TYPE inri-toyear.
*    gs_rc = 0.
*    TRY.
*        FREE go_cedido.
*        CREATE OBJECT go_cedido.
*
*      CATCH cx_ai_system_fault INTO oref.
*        text = oref->get_text( ).
*    ENDTRY.
*    CLEAR: wa_output. CLEAR wa_input.
*    wa_input-get_est_cesion_request-token = gs_token.
*    wa_input-get_est_cesion_request-rut_emisor = p_rutemisor.
*    wa_input-get_est_cesion_request-dvemisor = p_dvemisor.
*    wa_input-get_est_cesion_request-tipo_doc = p_tipodoc.
*    wa_input-get_est_cesion_request-folio_doc = p_folio.
*    wa_input-get_est_cesion_request-rut_empresa = p_rutempresa.
*    wa_input-get_est_cesion_request-dvempresa = p_dvempresa.
*
*    TRY.
*        CALL METHOD go_cedido->get_est_cesion_relac_sync_out
*          EXPORTING
*            output = wa_input
*          IMPORTING
*            input  = wa_output.
*
*      CATCH cx_ai_system_fault INTO oref.
*        text = oref->get_text( ).
*      CATCH cx_ai_application_fault INTO iref.
*        text = iref->get_text( ).
*    ENDTRY.
*    IF text IS INITIAL.
*      w_result = wa_output-get_est_cesion_relac_response-get_est_cesion_relac_return.
*      CLEAR : xml_result.
*
*      TRY.
*
*          CLEAR ls_resp_escere.
*          CALL TRANSFORMATION zdte_estcesionrelac
*            SOURCE XML  w_result "ws_resp
*            RESULT respuesta = ls_resp_escere.
*
*
*        CATCH cx_root INTO lo_err.
*          lv_err_string = lo_err->get_text( ).
*          gs_rc = 4.
*
*          IF gs_rc > 0 .
*
*            CLEAR gs_bal_msg.
*            gs_bal_msg-msgty = 'E'.
*            gs_bal_msg-msgid = 'ZDTE_0001'.
*            gs_bal_msg-msgno = '012'.
*            gs_bal_msg-msgv1 = gs_rc.
*            MOVE lv_err_string TO gs_bal_msg-msgv2.
*            go_log->add_msg( i_s_msg = gs_bal_msg ).
*            go_log->save( ).
*          ENDIF.
*          EXIT.
*      ENDTRY.
*      IF ls_resp_escere-resp_body-rut_tenedor IS NOT INITIAL.
*        r_rut_tenedor = ls_resp_escere-resp_body-rut_tenedor.
*      ELSE.
*        gs_rc = 4.
*        CLEAR ls_msgty.
*
*        CASE ls_resp_escere-resp_hdr-estado.
*          WHEN '0'. " Estatus OK
*          WHEN '-10'. "Estatus de no exite o no cedido
*            status_text = ls_resp_escere-resp_hdr-glosa."Error Mensaje excepcion'.
*            ls_msgty = 'W'.
*          WHEN OTHERS.
*            status_text = ls_resp_escere-resp_hdr-glosa."Error Mensaje excepcion'.
*            ls_msgty = 'E'.
*
*        ENDCASE.
*        CLEAR ls_doc .
*        CONCATENATE p_tipodoc '-' p_folio '-' p_rutemisor INTO ls_doc.
*        CONDENSE ls_doc NO-GAPS.
*        CLEAR gs_bal_msg.
*        gs_bal_msg-msgty = ls_msgty. "'E'.
*        gs_bal_msg-msgid = 'ZDTE_0001'.
*        gs_bal_msg-msgno = '012'.
*        gs_bal_msg-msgv1 = ls_doc.
*        MOVE status_text TO gs_bal_msg-msgv2.
*        MOVE ls_dest     TO gs_bal_msg-msgv3.
*        MOVE 'GET_FACTORY' TO gs_bal_msg-msgv4.
*        go_log->add_msg( i_s_msg = gs_bal_msg ).
*        go_log->save( ).
*      ENDIF.
*    ELSE.
*      gs_rc = 4.
*      "status_text. Texto de error de comunicación
*      IF gs_rc > 0 .
*        CLEAR gs_bal_msg.
*        gs_bal_msg-msgty = 'E'.
*        gs_bal_msg-msgid = 'ZDTE_0001'.
*        gs_bal_msg-msgno = '012'.
*        gs_bal_msg-msgv1 = gs_rc.
*        MOVE ls_dest TO gs_bal_msg-msgv3.
*        MOVE 'GET_FACTORY' TO gs_bal_msg-msgv4.
*        MOVE 'Error de comunicación get_token lin:226' TO gs_bal_msg-msgv2.
*        go_log->add_msg( i_s_msg = gs_bal_msg ).
*        go_log->save( ).
*      ENDIF.
*    ENDIF.

  ENDMETHOD.


  METHOD get_fechasii.
    DATA: ls_dest          TYPE rfcdest,
          ls_token         TYPE string,
          ws_string        TYPE string,
          http_status_code TYPE i,
          status_text      TYPE string,
          xml              TYPE xstring,
          xml_result       TYPE xstring,
          w_result         TYPE string,
          lo_err           TYPE REF TO cx_root,
          lv_err_string    TYPE string,
          len_str          TYPE string,
          xml_table        TYPE STANDARD TABLE OF smum_xmltb,
          ws_resp          TYPE string,
          ls_xmltable      TYPE smum_xmltb,
          zdte_respar      TYPE zdte_respar,
          return           TYPE STANDARD TABLE OF  bapiret2.
*--------------------------
* llamada R3
*--------------------------
    ls_dest = me->co_aceptarecl.
    CONCATENATE 'TOKEN=' gs_token INTO ls_token.

    CALL METHOD cl_http_client=>create_by_destination
      EXPORTING
        destination              = ls_dest
      IMPORTING
        client                   = DATA(lo_http_client)
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 6.
    IF sy-subrc = 0.
      "----> Datos de cabecera
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = '~request_method'
          value = 'POST'.
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'Accept-Encoding'
          value = 'gzip, deflate'.
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'Content-Type'
          value = 'txt/xml; charset=utf-8'.
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'SOAPAction'
          value = ''.

      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'cookie'
          value = ls_token.

      CALL METHOD lo_http_client->request->set_cookie
        EXPORTING
          name  = 'TOKEN'
          value = gs_token.


      CLEAR ws_string.
      CONCATENATE
  '<?xml version="1.0" encoding="utf-8"?>'
  '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"'
  ' xmlns:ws="http://ws.registroreclamodte.diii.sdi.sii.cl">'
  '<soapenv:Header/>'
  '<soapenv:Body>'
  '<ws:consultarFechaRecepcionSii>'
  '<rutEmisor>' itab-rutemisor '</rutEmisor>'
  '<dvEmisor>' itab-dvemisor '</dvEmisor>'
  '<tipoDoc>' itab-tipodoc '</tipoDoc>'
  '<folio>' itab-folio '</folio>'
  '</ws:consultarFechaRecepcionSii>'
  '</soapenv:Body>'
  '</soapenv:Envelope>'  INTO ws_string.

      DATA(len) = strlen( ws_string ).
      len_str = len.
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'Content-Length'
          value = len_str. "'377'.
      CALL METHOD lo_http_client->request->set_cdata
        EXPORTING
          data   = ws_string
          offset = 0
          length = strlen( ws_string ). "'377'.

*Step-2 : Send request.
      CALL METHOD lo_http_client->send
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2.
      CASE sy-subrc .
        WHEN 1. status_text = 'http_communication_failure'.
        WHEN 2. status_text = 'http_invalid_state'.
        WHEN OTHERS.
      ENDCASE.
      IF sy-subrc > 0 .
*        CLEAR gs_bal_msg.
*        gs_bal_msg-msgty = 'E'.
*        gs_bal_msg-msgid = 'ZDTE_0001'.
*        gs_bal_msg-msgno = '012'.
*        gs_bal_msg-msgv1 = sy-subrc.
*        MOVE status_text TO gs_bal_msg-msgv2.
*        go_log->add_msg( i_s_msg = gs_bal_msg ).
*        go_log->save( ).
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '012'.
        gs_bal_msg-msgv1 = sy-subrc.
        MOVE status_text TO gs_bal_msg-msgv2.
        CONCATENATE   'WS_FecSII/ErrorSend:' status_text INTO status_text.
        go_log->contenate_msg(
                  EXPORTING texto = status_text  descripcion_operacion = ''
                  IMPORTING msgv1 = gs_bal_msg-msgv1
                            msgv2 = gs_bal_msg-msgv2
                            msgv3 = gs_bal_msg-msgv3
                            msgv4 = gs_bal_msg-msgv4 ).

        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
      ENDIF.

*STEP-3 :  GET HTTP RESPONSE
      CALL METHOD lo_http_client->receive
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3.

      CASE sy-subrc .
        WHEN 1. status_text = 'http_communication_failure'.
        WHEN 2. status_text = 'http_invalid_state'.
        WHEN 3. status_text = 'http_processing_failed'.
        WHEN OTHERS.
      ENDCASE.
      IF sy-subrc > 0 .
*        CLEAR gs_bal_msg.
*        gs_bal_msg-msgty = 'E'.
*        gs_bal_msg-msgid = 'ZDTE_0001'.
*        gs_bal_msg-msgno = '012'.
*        gs_bal_msg-msgv1 = sy-subrc.
*        MOVE status_text TO gs_bal_msg-msgv2.
*        go_log->add_msg( i_s_msg = gs_bal_msg ).
*        go_log->save( ).
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '012'.
        gs_bal_msg-msgv1 = sy-subrc.
        CONCATENATE   'WS_FechSII/ErroReceive:' status_text INTO status_text.
        go_log->contenate_msg(
                  EXPORTING texto = status_text  descripcion_operacion = ''
                  IMPORTING msgv1 = gs_bal_msg-msgv1
                            msgv2 = gs_bal_msg-msgv2
                            msgv3 = gs_bal_msg-msgv3
                            msgv4 = gs_bal_msg-msgv4 ).
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
      ENDIF.

*STEP-4 : Read HTTP RETURN CODE
      CALL METHOD lo_http_client->response->get_status
        IMPORTING
          code   = http_status_code
          reason = status_text.
      IF http_status_code = '200'. "OK

*STEP-5 :  READ RESPONSE DATA
        w_result = lo_http_client->response->get_cdata( ).
        CLEAR : xml_result.

*Step-6 : convert string to xstring.
        CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
          EXPORTING
            text   = w_result
*           MIMETYPE       = ' '
*           ENCODING       =
          IMPORTING
            buffer = xml_result
          EXCEPTIONS
            failed = 1
            OTHERS = 2.

*Step-7 : create xml
        CALL FUNCTION 'SMUM_XML_PARSE'
          EXPORTING
            xml_input = xml_result
          TABLES
            xml_table = xml_table
            return    = return.
        CLEAR ws_resp.
        LOOP AT xml_table INTO ls_xmltable WHERE cname = 'return'.
          CONCATENATE ws_resp ls_xmltable-cvalue INTO ws_resp.
        ENDLOOP.
        IF ws_resp IS INITIAL.
          gs_rc = 4.
          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'E'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '066'.
          gs_bal_msg-msgv1 = itab-tipodoc.
          gs_bal_msg-msgv2 = itab-folio.
          gs_bal_msg-msgv3 = itab-rutemisor.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
        ELSE.
          gs_rc = 0.
          fecharecpsii = ws_resp.
        ENDIF.
      ELSE.
*STEP-5 :  READ RESPONSE DATA
        w_result = lo_http_client->response->get_cdata( ).
        CLEAR : xml_result.

*Step-6 : convert string to xstring.
        CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
          EXPORTING
            text   = w_result
*           MIMETYPE       = ' '
*           ENCODING       =
          IMPORTING
            buffer = xml_result
          EXCEPTIONS
            failed = 1
            OTHERS = 2.

*Step-7 : create xml
        CALL FUNCTION 'SMUM_XML_PARSE'
          EXPORTING
            xml_input = xml_result
          TABLES
            xml_table = xml_table
            return    = return.
        CLEAR ws_resp.
        LOOP AT xml_table INTO ls_xmltable WHERE cname = 'faultstring'.
          CONCATENATE ws_resp ls_xmltable-cvalue INTO ws_resp.
        ENDLOOP.
        gs_rc = 4.
        IF ws_resp IS NOT INITIAL.
          status_text = ws_resp. "Texto de error de comunicación
        ENDIF.
        CONCATENATE 'Fech/' status_text INTO status_text.
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '067'.
        gs_bal_msg-msgv1 = itab-tipodoc.
        gs_bal_msg-msgv2 = itab-folio.
        gs_bal_msg-msgv3 = itab-rutemisor.
        gs_bal_msg-msgv4 = status_text.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
      ENDIF.
      lo_http_client->close( ).
    ELSE.

* Implement suitable error handling here
      gs_rc = sy-subrc.
      CASE gs_rc.
        WHEN '1'.

          status_text = 'argument_not_found'.
        WHEN '2'.

          status_text = 'destination_not_found'.
        WHEN '3'.

          status_text = 'destination_no_authority'.
        WHEN '4'.
          status_text = 'plugin_not_active'.
        WHEN '5'.
          status_text = 'internal_error'.
        WHEN '6'.
          status_text = 'OTHERS'.
      ENDCASE.
*      CLEAR gs_bal_msg.
*      gs_bal_msg-msgty = 'E'.
*      gs_bal_msg-msgid = 'ZDTE_0001'.
*      gs_bal_msg-msgno = '067'.
*      gs_bal_msg-msgv1 = status_text.
*      go_log->add_msg( i_s_msg = gs_bal_msg ).
*      go_log->save( ).
      CLEAR gs_bal_msg.
      gs_bal_msg-msgty = 'E'.
      gs_bal_msg-msgid = 'ZDTE_0001'.
      gs_bal_msg-msgno = '012'.
      CONCATENATE 'WS_FecSII/ErrorConexion:'  status_text INTO  status_text.
      go_log->contenate_msg(
           EXPORTING texto =  status_text
              descripcion_operacion = ''
              IMPORTING msgv1 = gs_bal_msg-msgv1
                        msgv2 = gs_bal_msg-msgv2
                        msgv3 = gs_bal_msg-msgv3
                        msgv4 = gs_bal_msg-msgv4 ).
      go_log->add_msg( i_s_msg = gs_bal_msg ).
      go_log->save( ).
    ENDIF.
*--------------------------
* Llamada Pi
*--------------------------
*   DATA: go_fec TYPE REF TO zwsco_get_seed_sync_out,
*          wa_output  TYPE zwsget_seed_response,
*          wa_input   TYPE zwsget_seed.
*    gs_rc = 0.
*    DATA lv_largo TYPE i.
*    DATA: oref TYPE REF TO cx_ai_system_fault,
*          iref TYPE REF TO cx_ai_application_fault,
*          text TYPE string.
*    DATA: lv_toyear  TYPE inri-toyear.
*    TRY.
*        FREE go_fec.
*        CREATE OBJECT go_fec.
*
*      CATCH cx_ai_system_fault INTO oref.
*        text = oref->get_text( ).
*    ENDTRY.
*    CLEAR: wa_output.
**<<<<<< llenar wa_input >>>>>>>>>>>>
*   wa_input-rutEmisor =  itab-rutemisor.
*   wa_input-dvEmisor  = itab-dvemisor.
*   wa_input-tipoDoc   =  itab-tipodoc.
*   wa_input-folio     =  itab-folio.
*   wa_input-token     = gs_token.
*    TRY.
*        CALL METHOD go_fec->get_seed_sync_out
*          EXPORTING
*            output = wa_input
*          IMPORTING
*            input  = wa_output.
*
*      CATCH cx_ai_system_fault INTO oref.
*        text = oref->get_text( ).
*      CATCH cx_ai_application_fault INTO iref.
*        text = iref->get_text( ).
*    ENDTRY.
*    IF text IS INITIAL.
*      if  wa_output-return is not initial.
*             gs_rc = 0.
*          fecharecpsii = wa_output-return.
*      else.
*          gs_rc = 4.
*          CLEAR gs_bal_msg.
*          gs_bal_msg-msgty = 'E'.
*          gs_bal_msg-msgid = 'ZDTE_0001'.
*          gs_bal_msg-msgno = '066'.
*          gs_bal_msg-msgv1 = itab-tipodoc.
*          gs_bal_msg-msgv2 = itab-folio.
*          gs_bal_msg-msgv3 = itab-rutemisor.
*          go_log->add_msg( i_s_msg = gs_bal_msg ).
*          go_log->save( ).
*      ENDIF.
*    ELSE.
*
*      gs_rc = 4.
*      "status_text. Texto de error de comunicación
*      IF gs_rc > 0 .
*        CLEAR gs_bal_msg.
*        gs_bal_msg-msgty = 'E'.
*        gs_bal_msg-msgid = 'ZDTE_0001'.
*        gs_bal_msg-msgno = '012'.
*        gs_bal_msg-msgv1 = gs_rc.
*        MOVE ls_dest TO gs_bal_msg-msgv3.
*        MOVE '226 GET_TOKEN' TO gs_bal_msg-msgv4.
*        MOVE 'Error de comunicación get_token lin:226' TO gs_bal_msg-msgv2.
*        go_log->add_msg( i_s_msg = gs_bal_msg ).
*        go_log->save( ).
*      ENDIF.
*    ENDIF.
  ENDMETHOD.


  METHOD get_firmado_arc.
    DATA: archivo TYPE string.
    DATA: vl_rc TYPE abap_cr_lf.
    DATA: zdte_sem TYPE zdte_semilla.
    "---> Semilla
    zdte_sem-item-semilla = gs_semilla.
    "---> Contruir XML a firmar
    vl_rc = cl_abap_char_utilities=>cr_lf.

  ENDMETHOD.


  METHOD get_hisfolio.
    DATA: ls_dest          TYPE rfcdest,
          ls_token         TYPE string,
          ws_string        TYPE string,
          http_status_code TYPE i,
          status_text      TYPE string,
          xml              TYPE xstring,
          xml_result       TYPE xstring,
          w_result         TYPE string,
          lo_err           TYPE REF TO cx_root,
          lv_err_string    TYPE string,
          len_str          TYPE string,
          xml_table        TYPE STANDARD TABLE OF smum_xmltb,
*          zdte_listeventos TYPE zdte_liev01,
          ws_resp          TYPE string,
          ls_xmltable      TYPE smum_xmltb,
          zdte_respar      TYPE zdte_respar,
          return           TYPE STANDARD TABLE OF  bapiret2.
*--------------------------
* llamada R3
*--------------------------
    ls_dest = me->co_aceptarecl.
    CONCATENATE 'TOKEN=' gs_token INTO ls_token.

    CALL METHOD cl_http_client=>create_by_destination
      EXPORTING
        destination              = ls_dest
      IMPORTING
        client                   = DATA(lo_http_client)
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 6.
    IF sy-subrc = 0.
      "----> Datos de cabecera
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = '~request_method'
          value = 'POST'.
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'Accept-Encoding'
          value = 'gzip, deflate'.
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'Content-Type'
          value = 'txt/xml; charset=utf-8'.
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'SOAPAction'
          value = ''.

      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'cookie'
          value = ls_token.

      CALL METHOD lo_http_client->request->set_cookie
        EXPORTING
          name  = 'TOKEN'
          value = gs_token.


      CLEAR ws_string.
      CONCATENATE
  '<?xml version="1.0" encoding="utf-8"?>'
  '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"'
  ' xmlns:ws="http://ws.registroreclamodte.diii.sdi.sii.cl">'
  '<soapenv:Header/>'
  '<soapenv:Body>'
  '<ws:listarEventosHistDoc>'
  '<rutEmisor>' itab-rutemisor '</rutEmisor>'
  '<dvEmisor>' itab-dvemisor '</dvEmisor>'
  '<tipoDoc>' itab-tipodoc '</tipoDoc>'
  '<folio>' itab-folio '</folio>'
  '</ws:listarEventosHistDoc>'
  '</soapenv:Body>'
  '</soapenv:Envelope>'  INTO ws_string.

      DATA(len) = strlen( ws_string ).
      len_str = len.
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'Content-Length'
          value = len_str. "'377'.
      CALL METHOD lo_http_client->request->set_cdata
        EXPORTING
          data   = ws_string
          offset = 0
          length = strlen( ws_string ). "'377'.

*Step-2 : Send request.
      CALL METHOD lo_http_client->send
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2.
      CASE sy-subrc .
        WHEN 1. status_text = 'http_communication_failure'.
        WHEN 2. status_text = 'http_invalid_state'.
        WHEN OTHERS.
      ENDCASE.
      IF sy-subrc > 0 .
*        CLEAR gs_bal_msg.
*        gs_bal_msg-msgty = 'E'.
*        gs_bal_msg-msgid = 'ZDTE_0001'.
*        gs_bal_msg-msgno = '012'.
*        gs_bal_msg-msgv1 = sy-subrc.
*        MOVE status_text TO gs_bal_msg-msgv2.
*        go_log->add_msg( i_s_msg = gs_bal_msg ).
*        go_log->save( ).
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '012'.
        gs_bal_msg-msgv1 = sy-subrc.
        MOVE status_text TO gs_bal_msg-msgv2.
        CONCATENATE   'WS_Hist/ErrorSend:' status_text INTO status_text.
        go_log->contenate_msg(
                  EXPORTING texto = status_text  descripcion_operacion = ''
                  IMPORTING msgv1 = gs_bal_msg-msgv1
                            msgv2 = gs_bal_msg-msgv2
                            msgv3 = gs_bal_msg-msgv3
                            msgv4 = gs_bal_msg-msgv4 ).

        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).

      ENDIF.

*STEP-3 :  GET HTTP RESPONSE
      CALL METHOD lo_http_client->receive
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3.

      CASE sy-subrc .
        WHEN 1. status_text = 'http_communication_failure'.
        WHEN 2. status_text = 'http_invalid_state'.
        WHEN 3. status_text = 'http_processing_failed'.
        WHEN OTHERS.
      ENDCASE.
      IF sy-subrc > 0 .
*        CLEAR gs_bal_msg.
*        gs_bal_msg-msgty = 'E'.
*        gs_bal_msg-msgid = 'ZDTE_0001'.
*        gs_bal_msg-msgno = '012'.
*        gs_bal_msg-msgv1 = sy-subrc.
*        MOVE status_text TO gs_bal_msg-msgv2.
*        go_log->add_msg( i_s_msg = gs_bal_msg ).
*        go_log->save( ).
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '012'.
        gs_bal_msg-msgv1 = sy-subrc.
        CONCATENATE   'WS_Hist/ErroReceive:' status_text INTO status_text.
        go_log->contenate_msg(
                  EXPORTING texto = status_text  descripcion_operacion = ''
                  IMPORTING msgv1 = gs_bal_msg-msgv1
                            msgv2 = gs_bal_msg-msgv2
                            msgv3 = gs_bal_msg-msgv3
                            msgv4 = gs_bal_msg-msgv4 ).
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
      ENDIF.

*STEP-4 : Read HTTP RETURN CODE
      CALL METHOD lo_http_client->response->get_status
        IMPORTING
          code   = http_status_code
          reason = status_text.
      IF http_status_code = '200'. "OK

*STEP-5 :  READ RESPONSE DATA
        w_result = lo_http_client->response->get_cdata( ).
        CLEAR : xml_result.

*Step-6 : convert string to xstring.
        CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
          EXPORTING
            text   = w_result
*           MIMETYPE       = ' '
*           ENCODING       =
          IMPORTING
            buffer = xml_result
          EXCEPTIONS
            failed = 1
            OTHERS = 2.

*Step-7 : create xml
        TRY.
            CLEAR zdte_listeventos.
            CALL TRANSFORMATION zdte_listeventos
              SOURCE XML  xml_result
              RESULT envelope = zdte_listeventos.


          CATCH cx_root INTO lo_err.
            lv_err_string = lo_err->get_text( ).
            gs_rc = 4.
            lo_http_client->close( ).
            IF gs_rc > 0 .
*              CLEAR gs_bal_msg.
*              gs_bal_msg-msgty = 'E'.
*              gs_bal_msg-msgid = 'ZDTE_0001'.
*              gs_bal_msg-msgno = '012'.
*              gs_bal_msg-msgv1 = gs_rc.
*              MOVE lv_err_string TO gs_bal_msg-msgv2.
*              go_log->add_msg( i_s_msg = gs_bal_msg ).
*              go_log->save( ).
              CLEAR gs_bal_msg.
              gs_bal_msg-msgty = 'E'.
              gs_bal_msg-msgid = 'ZDTE_0001'.
              gs_bal_msg-msgno = '012'.
              gs_bal_msg-msgv1 = gs_rc.
              CONCATENATE   'WS_Hist/ErrorConver:' lv_err_string INTO lv_err_string.
              go_log->contenate_msg(
               EXPORTING texto = lv_err_string  descripcion_operacion = ''
               IMPORTING msgv1 = gs_bal_msg-msgv1
                         msgv2 = gs_bal_msg-msgv2
                         msgv3 = gs_bal_msg-msgv3
                         msgv4 = gs_bal_msg-msgv4 ).

              go_log->add_msg( i_s_msg = gs_bal_msg ).
              go_log->save( ).
            ENDIF.
            EXIT.
        ENDTRY.



      ELSE.
*STEP-5 :  READ RESPONSE DATA
        w_result = lo_http_client->response->get_cdata( ).
        CLEAR : xml_result.

*Step-6 : convert string to xstring.
        CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
          EXPORTING
            text   = w_result
*           MIMETYPE       = ' '
*           ENCODING       =
          IMPORTING
            buffer = xml_result
          EXCEPTIONS
            failed = 1
            OTHERS = 2.

*Step-7 : create xml
        CALL FUNCTION 'SMUM_XML_PARSE'
          EXPORTING
            xml_input = xml_result
          TABLES
            xml_table = xml_table
            return    = return.
        CLEAR ws_resp.
        LOOP AT xml_table INTO ls_xmltable WHERE cname = 'faultstring'.
          CONCATENATE ws_resp ls_xmltable-cvalue INTO ws_resp.
        ENDLOOP.
        gs_rc = 4.
        IF ws_resp IS NOT INITIAL.
          status_text = ws_resp. "Texto de error de comunicación
        ENDIF.
        CONCATENATE 'Hist/' status_text INTO status_text.
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '067'.
        gs_bal_msg-msgv1 = itab-tipodoc.
        gs_bal_msg-msgv2 = itab-folio.
        gs_bal_msg-msgv3 = itab-rutemisor.
        gs_bal_msg-msgv4 = status_text.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
      ENDIF.
      lo_http_client->close( ).
    ELSE.
* Implement suitable error handling here
      gs_rc = sy-subrc.
      CASE gs_rc.
        WHEN '1'.
          status_text = 'argument_not_found'.
        WHEN '2'.
          status_text = 'destination_not_found'.
        WHEN '3'.
          status_text = 'destination_no_authority'.
        WHEN '4'.
          status_text = 'plugin_not_active'.
        WHEN '5'.
          status_text = 'internal_error'.
        WHEN '6'.
          status_text = 'OTHERS'.
      ENDCASE.
*      CLEAR gs_bal_msg.
*      gs_bal_msg-msgty = 'E'.
*      gs_bal_msg-msgid = 'ZDTE_0001'.
*      gs_bal_msg-msgno = '048'.
*      gs_bal_msg-msgv1 = status_text.
*      go_log->add_msg( i_s_msg = gs_bal_msg ).
*      go_log->save( ).
      CLEAR gs_bal_msg.
      gs_bal_msg-msgty = 'E'.
      gs_bal_msg-msgid = 'ZDTE_0001'.
      gs_bal_msg-msgno = '012'.
      CONCATENATE 'WS_Hist/ErrorConexion:'  status_text INTO  status_text.
      go_log->contenate_msg(
           EXPORTING texto =  status_text
              descripcion_operacion = ''
              IMPORTING msgv1 = gs_bal_msg-msgv1
                        msgv2 = gs_bal_msg-msgv2
                        msgv3 = gs_bal_msg-msgv3
                        msgv4 = gs_bal_msg-msgv4 ).
      go_log->add_msg( i_s_msg = gs_bal_msg ).
      go_log->save( ).
    ENDIF.
*--------------------------
* llamada Pi
*--------------------------
*  DATA: go_hist  TYPE REF TO zwsco_get_token_sync_out,
*        wa_input  TYPE zwsget_token,
*        wa_output TYPE zwsget_token_response.
*
*  DATA lv_largo TYPE i.
*  DATA: oref TYPE REF TO cx_ai_system_fault,
*        iref TYPE REF TO cx_ai_application_fault,
*        text TYPE string.
*  DATA: lv_toyear  TYPE inri-toyear.
*  gs_rc = 0.
*  TRY.
*      FREE go_hist.
*      CREATE OBJECT go_hist.
*
*    CATCH cx_ai_system_fault INTO oref.
*      text = oref->get_text( ).
*  ENDTRY.
*  CLEAR: wa_output. CLEAR wa_input.
*
**<<<<<< llenar wa_input >>>>>>>>>>>>
*   wa_input-rutEmisor =  itab-rutemisor.
*   wa_input-dvEmisor  = itab-dvemisor.
*   wa_input-tipoDoc   =  itab-tipodoc.
*   wa_input-folio     =  itab-folio.
*   wa_input-token     = gs_token.
*
*  TRY.
*      CALL METHOD go_hist->get_token_sync_out "->get_seed_sync_out
*        EXPORTING
*          OUTPUT = wa_input
*        IMPORTING
*          input  = wa_output.
*
*    CATCH cx_ai_system_fault INTO oref.
*      text = oref->get_text( ).
*    CATCH cx_ai_application_fault INTO iref.
*      text = iref->get_text( ).
*  ENDTRY.
*  IF text IS INITIAL.
*    w_result = wa_output-get_token_return.
*    CLEAR : xml_result.
*
*
*    TRY.
*            CLEAR zdte_listeventos.
*            CALL TRANSFORMATION zdte_listeventos
*              SOURCE XML  xml_result
*              RESULT envelope = zdte_listeventos.

*
*      CATCH cx_root INTO lo_err.
*        lv_err_string = lo_err->get_text( ).
*        gs_rc = 4.
*
*        IF gs_rc > 0 .
*          CLEAR gs_bal_msg.
*          gs_bal_msg-msgty = 'e'.
*          gs_bal_msg-msgid = 'zdte_0001'.
*          gs_bal_msg-msgno = '012'.
*          gs_bal_msg-msgv1 = gs_rc.
*          MOVE lv_err_string TO gs_bal_msg-msgv2.
*          go_log->add_msg( i_s_msg = gs_bal_msg ).
*          go_log->save( ).
*        ENDIF.
*        EXIT.
*    ENDTRY.

*  ELSE.
*
*    gs_rc = 4.
*    "status_text. texto de error de comunicación
*    if gs_rc > 0 .
*    CLEAR gs_bal_msg.
*    gs_bal_msg-msgty = 'e'.
*    gs_bal_msg-msgid = 'zdte_0001'.
*    gs_bal_msg-msgno = '012'.
*    gs_bal_msg-msgv1 = gs_rc.
*    MOVE ls_dest TO gs_bal_msg-msgv3.
*    MOVE 'get_hist' TO gs_bal_msg-msgv4.
*    MOVE 'error de comunicación get_hist' TO gs_bal_msg-msgv2.
*    go_log->add_msg( i_s_msg = gs_bal_msg ).
*    go_log->save( ).
*  ENDIF.
*ENDIF.


  ENDMETHOD.


  METHOD get_listadofacrecsii.
    DATA: ls_dest          TYPE rfcdest,
          ls_token         TYPE string,
          ws_string        TYPE string,
          http_status_code TYPE i,
          status_text      TYPE string,
          xml              TYPE xstring,
          xml_result       TYPE xstring,
          w_result         TYPE string,
          lo_err           TYPE REF TO cx_root,
          lv_err_string    TYPE string,
          len_str          TYPE string,
          xml_table        TYPE STANDARD TABLE OF smum_xmltb,
          ws_resp          TYPE string,
          ls_xmltable      TYPE smum_xmltb,
          zdte_respar      TYPE zdte_respar,
          return           TYPE STANDARD TABLE OF  bapiret2,
          lo_rest_client   TYPE REF TO cl_rest_http_client,
          lo_response      TYPE REF TO if_rest_entity,
          lv_result_url    TYPE string,
          response         TYPE string,
          lt_data          TYPE string_t,
          lv_fecha         TYPE string.

    DATA: lt_listado     TYPE zdte_listarec_t,
          ls_listado     TYPE zdte_listarec,
          ls_ztfi_0074fr TYPE ztfi_0074fr,
          lv_fecharec    TYPE char10,
          lv_horarec     TYPE char08.

    ls_dest = me->co_listarec.


    CONCATENATE 'TOKEN=' gs_token INTO ls_token.

    CALL METHOD cl_http_client=>create_by_destination
      EXPORTING
        destination              = ls_dest
      IMPORTING
        client                   = DATA(lo_http_client)
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 6.
    IF sy-subrc = 0.
      "----> Datos de cabecera
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = '~request_method'
          value = 'GET'.
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'Accept-Encoding'
          value = 'gzip, deflate'.
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'Content-Type'
          value = 'txt/csv; charset=utf-8'.

      CALL METHOD lo_http_client->request->set_cookie
        EXPORTING
          name  = 'TOKEN'
          value = gs_token.

      CONCATENATE p_begda+6(2) p_begda+4(2) p_begda(4)
                 INTO lv_fecha SEPARATED BY '-'.
      CALL METHOD lo_http_client->request->set_form_field
        EXPORTING
          name  = 'DESDE'
          value = lv_fecha. " 'DD-MM-YYYY'

      CONCATENATE p_endda+6(2) p_endda+4(2) p_endda(4)
                       INTO lv_fecha SEPARATED BY '-'.
      CALL METHOD lo_http_client->request->set_form_field
        EXPORTING
          name  = 'HASTA'
          value = lv_fecha.

      CALL METHOD lo_http_client->request->set_form_field
        EXPORTING
          name  = 'RUT'
          value = p_rutemisor.
      CALL METHOD lo_http_client->request->set_form_field
        EXPORTING
          name  = 'TIPO_CONSULTA'
          value = '" "'.

      CALL METHOD lo_http_client->send
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2.
      CASE sy-subrc .
        WHEN 1. status_text = 'http_communication_failure'.
        WHEN 2. status_text = 'http_invalid_state'.
        WHEN OTHERS.
      ENDCASE.
      IF sy-subrc > 0 .
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '012'.
        gs_bal_msg-msgv1 = sy-subrc.
        MOVE status_text TO gs_bal_msg-msgv2.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
      ENDIF.

*STEP-3 :  GET HTTP RESPONSE
      CALL METHOD lo_http_client->receive
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3.

      CASE sy-subrc .
        WHEN 1. status_text = 'http_communication_failure'.
        WHEN 2. status_text = 'http_invalid_state'.
        WHEN 3. status_text = 'http_processing_failed'.
        WHEN OTHERS.
      ENDCASE.
      IF sy-subrc > 0 .
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '012'.
        gs_bal_msg-msgv1 = sy-subrc.
        MOVE status_text TO gs_bal_msg-msgv2.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
      ENDIF.

*STEP-4 : Read HTTP RETURN CODE
      CALL METHOD lo_http_client->response->get_status
        IMPORTING
          code   = http_status_code
          reason = status_text.
      IF http_status_code = '200'. "OK
* instantiate rest client
        CREATE OBJECT lo_rest_client
          EXPORTING
            io_http_client = lo_http_client.

* Get Response data
        lo_response = lo_rest_client->if_rest_client~get_response_entity( ).

* Get string data
        response = lo_response->get_string_data( ).

        DATA(cr_lf) = cl_abap_char_utilities=>newline.
        SPLIT response AT cr_lf INTO TABLE lt_data.
        DELETE lt_data INDEX 1. " Linea de encabezado

        IF lines( lt_data ) GT 0.

          CALL METHOD me->get_parse_listado
            EXPORTING
              i_anytable = lt_data
              i_tabname  = 'ZDTE_LISTAREC'
            IMPORTING
              e_tabla    = lt_listado.

          LOOP AT lt_listado INTO ls_listado.
            CLEAR: ls_ztfi_0074fr,
                   lv_fecharec,
                   lv_horarec.
            ls_ztfi_0074fr-bukrs     = p_bukrs.
            ls_ztfi_0074fr-xblnr     = ls_listado-xblnr.
            CONDENSE ls_listado-tipodte.

            CASE ls_listado-tipodte.
              WHEN 'Factura Electronica'.
                ls_ztfi_0074fr-tipodte = '33'.
              WHEN 'Factura No Afecta o Exenta Ele'.
                ls_ztfi_0074fr-tipodte = '34'.
              WHEN 'Nota de Credito Electronica'.
                ls_ztfi_0074fr-tipodte = '61'.
              WHEN 'Nota de Debito Electronica'.
                ls_ztfi_0074fr-tipodte = '56'.
              WHEN OTHERS.
                ls_ztfi_0074fr-tipodte = '??'.
            ENDCASE.
            ls_ztfi_0074fr-stcd1     = ls_listado-rutemisor.
            ls_ztfi_0074fr-estatus   = abap_true.

            " Buscar proveedor
            SELECT SINGLE lifnr INTO ls_ztfi_0074fr-lifnr
            FROM lfa1
            WHERE stcd1 EQ ls_listado-rutemisor.
            IF sy-subrc EQ 0.
            ENDIF.

            " Formatear fecha YYYY-MM-DD
            CONCATENATE ls_listado-fecha_emi+0(4)
                        ls_listado-fecha_emi+5(2)
                        ls_listado-fecha_emi+8(2) INTO ls_ztfi_0074fr-bldat.

            " Formatear fecha/hora recepción YYYY-MM-DD
            SPLIT ls_listado-fecha_rec AT space INTO lv_fecharec
                                                     lv_horarec.
            CONCATENATE lv_fecharec+0(4)
                        lv_fecharec+5(2)
                        lv_fecharec+8(2) INTO ls_ztfi_0074fr-fecharsii.

            REPLACE ALL OCCURRENCES OF ':' IN lv_horarec WITH ''.
            CONDENSE lv_horarec.
            ls_ztfi_0074fr-horarsii = lv_horarec.
            ls_ztfi_0074fr-cpudt    = sy-datum.
            ls_ztfi_0074fr-cputm    = sy-uzeit.
            OVERLAY ls_ztfi_0074fr-horarsii WITH '000000'.

            APPEND ls_ztfi_0074fr TO lt_facturasii.
          ENDLOOP.

        ELSE. " NO hay registros
          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'W'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '012'.
          gs_bal_msg-msgv1 = 'No tiene DTE recibidos en fecha,'.
          gs_bal_msg-msgv2 = 'o no tiene Permiso'.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
        ENDIF.
      ELSE.
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '012'.
        gs_bal_msg-msgv1 = sy-subrc.
        MOVE status_text TO gs_bal_msg-msgv2.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
      ENDIF.
      CALL METHOD lo_http_client->close
        EXCEPTIONS
          http_invalid_state = 1
          OTHERS             = 2.
      IF sy-subrc <> 0.

      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_listadofactrecsiinw.
    DATA: lo_http_client TYPE REF TO if_http_client,
          mo_rest_client TYPE REF TO cl_rest_http_client,
          lo_request     TYPE REF TO if_rest_entity,
          lo_response    TYPE REF TO if_rest_entity,
          ls_return      TYPE bapiret2,
          lv_content     TYPE string,
          ls_dest        TYPE rfcdest,
          lv_token       TYPE string,
          lv_body        TYPE string,
          lv_status      TYPE string,
          lo_err         TYPE REF TO cx_rest_client_exception,
          lv_err_string  TYPE string.
*--------------------------
* Llamado R3
*--------------------------
    DATA: lr_json_deserializer TYPE REF TO cl_trex_json_deserializer,
          lr_json_serializer   TYPE REF TO cl_trex_json_serializer.


    DATA: t_datos TYPE t_data.

    TYPES: BEGIN OF ty_json_req,
             tipodoc    TYPE string,
             rut        TYPE string,
             dv         TYPE string,
             periodo    TYPE string,
             operacion  TYPE string,
             derrcodigo TYPE string,
             refncd     TYPE string,
           END OF ty_json_req,

           BEGIN OF ty_metadata,
             namespace      TYPE string,
             conversationid TYPE string,
             transactionid  TYPE string,
             data           TYPE ty_json_req,
           END OF ty_metadata,

           BEGIN OF ty_req,
             metadata TYPE ty_metadata,
           END OF ty_req,

           BEGIN OF ty_metadataresp,
             conversationid TYPE string,
             transactionid  TYPE string,
             namespace      TYPE string,
             info           TYPE string,
             errors         TYPE string,
             page           TYPE string,
           END OF ty_metadataresp,

           BEGIN OF ty_respestado,
*           consulta_no_valida  TYPE string,
*           sin_datos           TYPE string,
*           error_de_aplicacion TYPE string,
*           error_de_negocio    TYPE string,
*           ok                  TYPE string,
             codrespuesta  TYPE string,
             msgerespuesta TYPE string,
             coderror      TYPE string,
           END OF ty_respestado,

           BEGIN OF ty_resp,
             data          TYPE string_t,
             nombrearchivo TYPE string,
             metadata      TYPE ty_metadataresp,
             respestado    TYPE ty_respestado,
           END OF ty_resp.

    DATA: json_req  TYPE ty_req,
          json_resp TYPE ty_resp.


    DATA: lt_listado     TYPE zdte_listarecnw_t,
          ls_listado     TYPE zdte_listarecnw,
          ls_ztfi_0074fr TYPE ztfi_0074fr,
          lv_fecharec    TYPE char10,
          lv_horarec     TYPE char08.

    ls_dest = co_listarec.
**//.. Create http client
    CALL METHOD cl_http_client=>create_by_destination
      EXPORTING
        destination              = ls_dest
      IMPORTING
        client                   = lo_http_client
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 6.
    IF lo_http_client IS NOT BOUND.
*      RAISE error_http_client.
    ELSE.
      CONCATENATE 'TOKEN=' gs_token INTO lv_token.

      CALL METHOD lo_http_client->request->set_cookie
        EXPORTING
          name  = 'TOKEN'
          value = gs_token.

      TRY.
          mo_rest_client  = NEW cl_rest_http_client( io_http_client = lo_http_client ) .
        CATCH cx_rest_client_exception INTO lo_err.
          lv_err_string = lo_err->get_text( ).
          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'E'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '007'.
          gs_bal_msg-msgv1 = lv_err_string.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
          EXIT.

      ENDTRY.

**//.. Set Payload or body ( JSON or XML)
* ABAP to JSON
      lo_request = mo_rest_client->if_rest_client~create_request_entity( ).

      lo_request->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).

      json_req-metadata-conversationid    = gs_token.
      json_req-metadata-transactionid     = gs_token.
      json_req-metadata-namespace         = 'cl.sii.sdi.lob.diii.consemitidos.data.api.interfaces.FacadeService/getDetalleExportarRecibidos'.
      json_req-metadata-data-tipodoc      = p_tipodoc.
      json_req-metadata-data-rut          = p_rutemisor.
      json_req-metadata-data-dv           = p_dvemisor.
      json_req-metadata-data-periodo      = p_periodo.
      CONCATENATE  json_req-metadata-data-periodo(4) '-'  json_req-metadata-data-periodo+4(2) INTO json_req-metadata-data-periodo.
      CONDENSE  json_req-metadata-data-periodo NO-GAPS.
      json_req-metadata-data-operacion    = '2'.
      json_req-metadata-data-derrcodigo   = p_tipodoc.
      json_req-metadata-data-refncd       = 0.

      CREATE OBJECT lr_json_serializer
        EXPORTING
          data = json_req.

      lr_json_serializer->serialize( ).
      lv_body = lr_json_serializer->get_data( ).

**//.. Reformatear el JSON
      REPLACE ALL OCCURRENCES OF '{'  IN lv_body WITH '{"'.
      REPLACE ALL OCCURRENCES OF ':'  IN lv_body WITH '":'.
      REPLACE ALL OCCURRENCES OF ', ' IN lv_body WITH ',"'.
      CONDENSE lv_body NO-GAPS.
      REPLACE ALL OCCURRENCES OF '}}}' IN lv_body WITH '}}'.
      REPLACE ALL OCCURRENCES OF ',"data' IN lv_body WITH '},"data'.
      REPLACE ALL OCCURRENCES OF 'metadata' IN lv_body WITH 'metaData'.
      REPLACE ALL OCCURRENCES OF 'conversationid' IN lv_body WITH 'conversationId'.
      REPLACE ALL OCCURRENCES OF 'tipodoc' IN lv_body WITH 'tipoDoc'.
      REPLACE ALL OCCURRENCES OF 'derrcodigo' IN lv_body WITH 'derrCodigo'.
      REPLACE ALL OCCURRENCES OF 'refncd' IN lv_body WITH 'refNCD'.
*+//..Nvo campos
      REPLACE ALL OCCURRENCES OF 'transactionid' IN lv_body WITH 'transactionId'.
**//..
      lo_request->set_string_data( lv_body ).

      " POST
      TRY.
          mo_rest_client->if_rest_resource~post( lo_request ).
        CATCH cx_rest_client_exception INTO lo_err.
          lv_err_string = lo_err->get_text( ).
          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'E'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '007'.
          gs_bal_msg-msgv1 = lv_err_string.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
          EXIT.

      ENDTRY.

      lo_response = mo_rest_client->if_rest_client~get_response_entity( ).
      lv_status = lo_response->get_header_field( '~status_code' ).

      lv_content = lo_response->get_string_data( ).
      CONDENSE lv_content.

      "---> Parsea Response
      CALL METHOD cl_fdt_json=>json_to_data
        EXPORTING
          iv_json = lv_content
        CHANGING
          ca_data = json_resp.
**//..
      IF json_resp-respestado-codrespuesta EQ '0'.
        DELETE json_resp-data INDEX 1. " Linea de encabezado
        IF lines( json_resp-data ) GT 0.
*          DELETE json_resp-data INDEX 1. " Linea de encabezado

          CALL METHOD me->get_parse_listado
            EXPORTING
              i_anytable = json_resp-data
              i_tabname  = 'ZDTE_LISTARECNW'
            IMPORTING
              e_tabla    = lt_listado.

          LOOP AT lt_listado INTO ls_listado.
            CLEAR: ls_ztfi_0074fr,
                    lv_fecharec,
                    lv_horarec.
            ls_ztfi_0074fr-bukrs     = p_bukrs.
            ls_ztfi_0074fr-xblnr     = ls_listado-xblnr.
            ls_ztfi_0074fr-tipodte   = p_tipodoc. "ls_listado-tipo_doc.
            ls_ztfi_0074fr-stcd1     = ls_listado-rutemisor.
            ls_ztfi_0074fr-estatus   = abap_true.
            ls_ztfi_0074fr-waers    = 'CLP'.
            ls_ztfi_0074fr-mntoexe  = ls_listado-mntoexe / 100.
            ls_ztfi_0074fr-mntneto  = ls_listado-mntneto  / 100.
            ls_ztfi_0074fr-ivarec   = ls_listado-ivarec  / 100.
            ls_ztfi_0074fr-mnttotal = ls_listado-mnttotal  / 100.
** Ini.Ajuste para guardar info LC
*        ls_ztfi_0074fr-estatuslc = abap_true.
*        ls_ztfi_0074fr-waers    = 'CLP'.
*        ls_ztfi_0074fr-mntoexe  = ls_listado-mntoexe / 100.
*        ls_ztfi_0074fr-mntneto  = ls_listado-mntneto  / 100.
*        ls_ztfi_0074fr-ivarec   = ls_listado-ivarec  / 100.
*        ls_ztfi_0074fr-ivanorec = ls_listado-ivanorec  / 100.
*        ls_ztfi_0074fr-mnttotal = ls_listado-mnttotal  / 100.
*        ls_ztfi_0074fr-codiva   = ls_listado-codiva.
*        ls_ztfi_0074fr-cpudtlc  = sy-datum.
*        ls_ztfi_0074fr-cputmlc  = sy-uzeit.
*        ls_ztfi_0074fr-periodo  = p_periodo.
*        ls_ztfi_0074fr-rznsoc   = ls_listado-rznsocemisor(35).
** Fin.Ajuste para guardar info LC
            " Buscar proveedor
            SELECT SINGLE lfa1~lifnr
            INTO ls_ztfi_0074fr-lifnr
            FROM lfa1 INNER JOIN lfb1 ON lfa1~lifnr = lfb1~lifnr
              WHERE lfb1~bukrs EQ  p_bukrs
                AND   lfa1~stcd1 EQ ls_listado-rutemisor
                AND   lfa1~sperr EQ space
                AND   lfa1~sperq EQ space
                AND   lfb1~sperr EQ space
                AND   lfa1~loevm EQ space
                AND   lfb1~loevm EQ space.

            " Formatear fecha DD-MM-YYYY.
            CONCATENATE ls_listado-fecha_doc+6(4)
                        ls_listado-fecha_doc+3(2)
                        ls_listado-fecha_doc+0(2) INTO ls_ztfi_0074fr-bldat.

            " Formatear fecha/hora recepción DD-MM-YYYY.
            SPLIT ls_listado-fecha_rec AT space INTO lv_fecharec
                                                     lv_horarec.
            CONCATENATE lv_fecharec+6(4)
                        lv_fecharec+3(2)
                        lv_fecharec+0(2) INTO ls_ztfi_0074fr-fecharsii.

            REPLACE ALL OCCURRENCES OF ':' IN lv_horarec WITH ''.
            CONDENSE lv_horarec.
            ls_ztfi_0074fr-horarsii = lv_horarec.
            ls_ztfi_0074fr-cpudt    = sy-datum.
            ls_ztfi_0074fr-cputm    = sy-uzeit.
            OVERLAY ls_ztfi_0074fr-horarsii WITH '000000'.


            APPEND ls_ztfi_0074fr TO lt_facturasii.
          ENDLOOP.
        ELSE.
          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'W'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '012'.
          gs_bal_msg-msgv1 = p_bukrs.
          gs_bal_msg-msgv2 = '-No hay registros para tipoDTE:'.
          gs_bal_msg-msgv3 = p_tipodoc.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
        ENDIF.
      ELSE.
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '012'.
        gs_bal_msg-msgv1 = p_bukrs.
        gs_bal_msg-msgv2 = json_resp-respestado-msgerespuesta.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
      ENDIF.
    ENDIF.
*--------------------------
* Llamada Pi
*--------------------------
*DATA: lt_listado     TYPE zdte_listarecnw_t,
*          ls_listado     TYPE zdte_listarecnw,
*          ls_ztfi_0074fr TYPE ztfi_0074fr,
*          lv_fecharec    TYPE char10,
*          lv_horarec     TYPE char08.
*
*    DATA: go_listado TYPE REF TO zwsco_listado_sync_dteout,
*          wa_input   TYPE zwslistado_dterequest1,
*          wa_output  TYPE zwslistado_dteresponse1.
*
*    DATA lv_largo TYPE i.
*    DATA: oref TYPE REF TO cx_ai_system_fault,
*          iref TYPE REF TO cx_ai_application_fault,
*          text TYPE string.
*    DATA: lv_toyear  TYPE inri-toyear.
***    CONCATENATE 'TOKEN=' gs_token INTO ls_token.
*    ls_token = gs_token.
*    gs_rc = 0.
*
*    TRY.
*        FREE go_listado.
*        CREATE OBJECT go_listado.
*
*      CATCH cx_ai_system_fault INTO oref.
*        text = oref->get_text( ).
*    ENDTRY.
*
*    CLEAR: wa_output,
*           wa_input,
*           text.
*
*    " HEADER
*    wa_input-listado_dterequest-header-token  = ls_token.
*
*    " DTEREQUEST
*    CONCATENATE p_periodo(4) p_periodo+4(2)
*                INTO lv_periodo SEPARATED BY '-'.
*    CONDENSE lv_periodo NO-GAPS.
*
*    wa_input-listado_dterequest-dterequest-tipo_doc    = p_tipodoc.
*    wa_input-listado_dterequest-dterequest-rut         = p_rutemisor.
*    wa_input-listado_dterequest-dterequest-dv          = p_dvemisor.
*    wa_input-listado_dterequest-dterequest-periodo     = lv_periodo.
*    wa_input-listado_dterequest-dterequest-operacion   = '2'.
*    wa_input-listado_dterequest-dterequest-derr_codigo = p_tipodoc.
*    wa_input-listado_dterequest-dterequest-ref_ncd     = 0.
*
*    TRY.
*        CALL METHOD go_listado->listado_sync_dteout
*          EXPORTING
*            output = wa_input
*          IMPORTING
*            input  = wa_output.
*
*      CATCH cx_ai_system_fault INTO oref.
*        text = oref->get_text( ).
*      CATCH cx_ai_application_fault INTO iref.
*        text = iref->get_text( ).
*    ENDTRY.
*
*    IF text IS INITIAL.
***      IF wa_output-listado_dteresponse-http_status = '200'.
*      IF wa_output-listado_dteresponse-http_status IS INITIAL.
*
***        DATA(cr_lf) = cl_abap_char_utilities=>newline.
***        response = wa_output-listado_dteresponse-data.
***        SPLIT response AT cr_lf INTO TABLE lt_data.
*
*        lt_data = wa_output-listado_dteresponse-data.
*
*        DELETE lt_data INDEX 1. " Linea de encabezado
*
*        IF lines( lt_data ) GT 0.
*
*          CALL METHOD me->get_parse_listado
*            EXPORTING
*              i_anytable = lt_data
*              i_tabname  = 'ZDTE_LISTARECNW'
*            IMPORTING
*              e_tabla    = lt_listado.
*
*          LOOP AT lt_listado INTO ls_listado.
*            CLEAR: ls_ztfi_0074fr,
*                   lv_fecharec,
*                   lv_horarec.
*
*            ls_ztfi_0074fr-bukrs     = p_bukrs.
*            ls_ztfi_0074fr-xblnr     = ls_listado-xblnr.
*            ls_ztfi_0074fr-tipodte   = p_tipodoc. "ls_listado-tipo_doc.
*            ls_ztfi_0074fr-stcd1     = ls_listado-rutemisor.
*            ls_ztfi_0074fr-estatus   = abap_true.
*** Ini.Ajuste para guardar info LC
**        ls_ztfi_0074fr-estatuslc = abap_true.
**        ls_ztfi_0074fr-waers    = 'CLP'.
**        ls_ztfi_0074fr-mntoexe  = ls_listado-mntoexe / 100.
**        ls_ztfi_0074fr-mntneto  = ls_listado-mntneto  / 100.
**        ls_ztfi_0074fr-ivarec   = ls_listado-ivarec  / 100.
**        ls_ztfi_0074fr-ivanorec = ls_listado-ivanorec  / 100.
**        ls_ztfi_0074fr-mnttotal = ls_listado-mnttotal  / 100.
**        ls_ztfi_0074fr-codiva   = ls_listado-codiva.
**        ls_ztfi_0074fr-cpudtlc  = sy-datum.
**        ls_ztfi_0074fr-cputmlc  = sy-uzeit.
**        ls_ztfi_0074fr-periodo  = p_periodo.
**        ls_ztfi_0074fr-rznsoc   = ls_listado-rznsocemisor(35).
*** Fin.Ajuste para guardar info LC
*            " Buscar proveedor
*            SELECT SINGLE lfa1~lifnr
*            INTO ls_ztfi_0074fr-lifnr
*            FROM lfa1 INNER JOIN lfb1 ON lfa1~lifnr = lfb1~lifnr
*              WHERE lfb1~bukrs EQ p_bukrs
*                AND lfa1~stcd1 EQ ls_listado-rutemisor
***                AND lfa1~sperr EQ space
***                AND lfa1~sperq EQ space
***                AND lfb1~sperr EQ space
***                AND lfa1~loevm EQ space
***                AND lfb1~loevm EQ space
*              .
*            IF sy-subrc EQ 0.
*            ENDIF.
*
*            " Formatear fecha DD-MM-YYYY.
*            CONCATENATE ls_listado-fecha_doc+6(4)
*                        ls_listado-fecha_doc+3(2)
*                        ls_listado-fecha_doc+0(2) INTO ls_ztfi_0074fr-bldat.
*
*            " Formatear fecha/hora recepción DD-MM-YYYY.
*            SPLIT ls_listado-fecha_rec AT space INTO lv_fecharec
*                                                     lv_horarec.
*            CONCATENATE lv_fecharec+6(4)
*                        lv_fecharec+3(2)
*                        lv_fecharec+0(2) INTO ls_ztfi_0074fr-fecharsii.
*
*            REPLACE ALL OCCURRENCES OF ':' IN lv_horarec WITH ''.
*            CONDENSE lv_horarec.
*
*            ls_ztfi_0074fr-horarsii = lv_horarec.
***            ls_ztfi_0074fr-cpudt    = sy-datum.
***            ls_ztfi_0074fr-cputm    = sy-uzeit.
*            OVERLAY ls_ztfi_0074fr-horarsii WITH '000000'.
*
*            APPEND ls_ztfi_0074fr TO lt_facturasii.
*          ENDLOOP.
*
*        ELSE. " NO hay registros
*          CLEAR gs_bal_msg.
*          gs_bal_msg-msgty = 'W'.
*          gs_bal_msg-msgid = 'ZDTE_0001'.
*          gs_bal_msg-msgno = '012'.
*          gs_bal_msg-msgv1 = 'No tiene DTE recibidos en período,'.
*          gs_bal_msg-msgv2 = 'o no tiene Permiso'.
*          go_log->add_msg( i_s_msg = gs_bal_msg ).
*          go_log->save( ).
*        ENDIF.
*      ELSE.
*        status_text = wa_output-listado_dteresponse-http_status_text.
*
*        CLEAR gs_bal_msg.
*        gs_bal_msg-msgty = 'E'.
*        gs_bal_msg-msgid = 'ZDTE_0001'.
*        gs_bal_msg-msgno = '012'.
*        gs_bal_msg-msgv1 = sy-subrc.
*        gs_bal_msg-msgv2 = status_text.
*
*        MOVE status_text TO gs_bal_msg-msgv2.
*        go_log->add_msg( i_s_msg = gs_bal_msg ).
*        go_log->save( ).
*      ENDIF.
*    ELSE.
*      CLEAR gs_bal_msg.
*      gs_bal_msg-msgty = 'E'.
*      gs_bal_msg-msgid = 'ZDTE_0001'.
*      gs_bal_msg-msgno = '012'.
*      gs_bal_msg-msgv1 = text.
*      go_log->add_msg( i_s_msg = gs_bal_msg ).
*      go_log->save( ).
*    ENDIF.

  ENDMETHOD.


  METHOD get_listadofactsii.
    DATA: lo_http_client TYPE REF TO if_http_client,
          mo_rest_client TYPE REF TO cl_rest_http_client,
          lo_request     TYPE REF TO if_rest_entity,
          lo_response    TYPE REF TO if_rest_entity,
          ls_return      TYPE bapiret2,
          lv_content     TYPE string,
          ls_dest        TYPE rfcdest,
          lv_token       TYPE string,
          lv_body        TYPE string,
          lv_status      TYPE string.
    DATA: lr_json_deserializer TYPE REF TO cl_trex_json_deserializer,
          lr_json_serializer   TYPE REF TO cl_trex_json_serializer.

    DATA: t_datos TYPE t_data.

    TYPES: BEGIN OF ty_json_req,
             rutemisor    TYPE string,
             dvemisor     TYPE string,
             ptributario  TYPE string,
             estadocontab TYPE string,
             codtipodoc   TYPE string,
             operacion    TYPE string,
           END OF ty_json_req,

           BEGIN OF ty_metadata,
             conversationid TYPE string,
             transactionid  TYPE string,
             namespace      TYPE string,
             data           TYPE ty_json_req,
           END OF ty_metadata,

           BEGIN OF ty_req,
             metadata TYPE ty_metadata,
           END OF ty_req,

           BEGIN OF ty_metadataresp,
             conversationid TYPE string,
             transactionid  TYPE string,
             namespace      TYPE string,
             info           TYPE string,
             errors         TYPE string,
             page           TYPE string,
           END OF ty_metadataresp,

           BEGIN OF ty_respestado,
             consulta_no_valida  TYPE string,
             sin_datos           TYPE string,
             error_de_aplicacion TYPE string,
             error_de_negocio    TYPE string,
             ok                  TYPE string,
             codrespuesta        TYPE string,
             msgerespuesta       TYPE string,
             coderror            TYPE string,
           END OF ty_respestado,

           BEGIN OF ty_resp,
             data          TYPE string_t,
             nombrearchivo TYPE string,
             metadata      TYPE ty_metadataresp,
             respestado    TYPE ty_respestado,
           END OF ty_resp.

    DATA: json_req  TYPE ty_req,
          json_resp TYPE ty_resp.


    DATA: lt_listado     TYPE zdte_listafac_t,
          ls_listado     TYPE zdte_listafac,
          ls_ztfi_0074fr TYPE ztfi_0074fr,
          lv_fecharec    TYPE char10,
          lv_horarec     TYPE char08.

    ls_dest = co_listado.
**//.. Create http client
    CALL METHOD cl_http_client=>create_by_destination
      EXPORTING
        destination              = ls_dest
      IMPORTING
        client                   = lo_http_client
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 6.
    IF lo_http_client IS NOT BOUND.
*      RAISE error_http_client.
    ELSE.
      CONCATENATE 'TOKEN=' gs_token INTO lv_token.

      CALL METHOD lo_http_client->request->set_cookie
        EXPORTING
          name  = 'TOKEN'
          value = gs_token.

      mo_rest_client  = NEW cl_rest_http_client( io_http_client = lo_http_client ) .

**//.. Set Payload or body ( JSON or XML)
* ABAP to JSON
      lo_request = mo_rest_client->if_rest_client~create_request_entity( ).

      lo_request->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).

      json_req-metadata-conversationid    = gs_token.
      json_req-metadata-transactionid     = gs_token.
      json_req-metadata-namespace         = 'cl.sii.sdi.lob.diii.consdcv.data.api.interfaces.FacadeService/getDetalleCompraExport'.
      json_req-metadata-data-rutemisor    = p_rutemisor.
      json_req-metadata-data-dvemisor     = p_dvemisor.
      json_req-metadata-data-ptributario  = p_periodo. "p_begda(6).
      json_req-metadata-data-estadocontab = 'REGISTRO'.
      json_req-metadata-data-codtipodoc   = 0.
      json_req-metadata-data-operacion    = 'COMPRA'.

      CREATE OBJECT lr_json_serializer
        EXPORTING
          data = json_req.

      lr_json_serializer->serialize( ).
      lv_body = lr_json_serializer->get_data( ).

**//.. Reformatear el JSON
      REPLACE ALL OCCURRENCES OF '{'  IN lv_body WITH '{"'.
      REPLACE ALL OCCURRENCES OF ':'  IN lv_body WITH '":'.
      REPLACE ALL OCCURRENCES OF ', ' IN lv_body WITH ',"'.
      CONDENSE lv_body NO-GAPS.
      REPLACE ALL OCCURRENCES OF '}}}' IN lv_body WITH '}}'.
      REPLACE ALL OCCURRENCES OF ',"data' IN lv_body WITH '},"data'.
      REPLACE ALL OCCURRENCES OF 'metadata' IN lv_body WITH 'metaData'.
      REPLACE ALL OCCURRENCES OF 'conversationid' IN lv_body WITH 'conversationId'.
      REPLACE ALL OCCURRENCES OF 'rutemisor' IN lv_body WITH 'rutEmisor'.
      REPLACE ALL OCCURRENCES OF 'dvemisor' IN lv_body WITH 'dvEmisor'.
      REPLACE ALL OCCURRENCES OF 'estadocontab' IN lv_body WITH 'estadoContab'.
      REPLACE ALL OCCURRENCES OF 'codtipodoc' IN lv_body WITH 'codTipoDoc'.
*+//..Nvo campos
      REPLACE ALL OCCURRENCES OF 'transactionid' IN lv_body WITH 'transactionId'.
**//..
      lo_request->set_string_data( lv_body ).

      " POST
      mo_rest_client->if_rest_resource~post( lo_request ).

      lo_response = mo_rest_client->if_rest_client~get_response_entity( ).
      lv_status = lo_response->get_header_field( '~status_code' ).

      lv_content = lo_response->get_string_data( ).
      CONDENSE lv_content.

      "---> Parsea Response
      CALL METHOD cl_fdt_json=>json_to_data
        EXPORTING
          iv_json = lv_content
        CHANGING
          ca_data = json_resp.
**//..
      IF lines( json_resp-data ) GT 0.
        DELETE json_resp-data INDEX 1. " Linea de encabezado

        CALL METHOD me->get_parse_listado
          EXPORTING
            i_anytable = json_resp-data
            i_tabname  = 'ZDTE_LISTAFAC'
          IMPORTING
            e_tabla    = lt_listado.

        LOOP AT lt_listado INTO ls_listado.
          CLEAR: ls_ztfi_0074fr,
                  lv_fecharec,
                  lv_horarec.
          ls_ztfi_0074fr-bukrs     = p_bukrs.
          ls_ztfi_0074fr-xblnr     = ls_listado-xblnr.
          ls_ztfi_0074fr-tipodte   = ls_listado-tipo_doc.
          ls_ztfi_0074fr-stcd1     = ls_listado-rutemisor.
          ls_ztfi_0074fr-estatus   = abap_true.
** Ini.Ajuste para guardar info LC
          ls_ztfi_0074fr-estatuslc = abap_true.
          ls_ztfi_0074fr-waers    = 'CLP'.
          ls_ztfi_0074fr-mntoexe  = ls_listado-mntoexe / 100.
          ls_ztfi_0074fr-mntneto  = ls_listado-mntneto  / 100.
          ls_ztfi_0074fr-ivarec   = ls_listado-ivarec  / 100.
          ls_ztfi_0074fr-ivanorec = ls_listado-ivanorec  / 100.
          ls_ztfi_0074fr-mnttotal = ls_listado-mnttotal  / 100.
          ls_ztfi_0074fr-codiva   = ls_listado-codiva.
          ls_ztfi_0074fr-cpudtlc  = sy-datum.
          ls_ztfi_0074fr-cputmlc  = sy-uzeit.
          ls_ztfi_0074fr-periodo  = p_periodo.
          ls_ztfi_0074fr-rznsoc   = ls_listado-rznsocemisor(35).
** Fin.Ajuste para guardar info LC
          " Buscar proveedor

          SELECT SINGLE lfa1~lifnr
          INTO ls_ztfi_0074fr-lifnr
          FROM lfa1 INNER JOIN lfb1 ON lfa1~lifnr = lfb1~lifnr
            WHERE lfb1~bukrs EQ  p_bukrs
              AND   lfa1~stcd1 EQ ls_listado-rutemisor
              AND   lfa1~sperr EQ space
              AND   lfa1~sperq EQ space
              AND   lfb1~sperr EQ space
              AND   lfa1~loevm EQ space
              AND   lfb1~loevm EQ space.


          " Formatear fecha DD-MM-YYYY.
          CONCATENATE ls_listado-fecha_doc+6(4)
                      ls_listado-fecha_doc+3(2)
                      ls_listado-fecha_doc+0(2) INTO ls_ztfi_0074fr-bldat.

          " Formatear fecha/hora recepción DD-MM-YYYY.
          SPLIT ls_listado-fecha_rec AT space INTO lv_fecharec
                                                   lv_horarec.
          CONCATENATE lv_fecharec+6(4)
                      lv_fecharec+3(2)
                      lv_fecharec+0(2) INTO ls_ztfi_0074fr-fecharsii.

          REPLACE ALL OCCURRENCES OF ':' IN lv_horarec WITH ''.
          CONDENSE lv_horarec.
          ls_ztfi_0074fr-horarsii = lv_horarec.
          ls_ztfi_0074fr-cpudt    = sy-datum.
          ls_ztfi_0074fr-cputm    = sy-uzeit.
          OVERLAY ls_ztfi_0074fr-horarsii WITH '000000'.


          APPEND ls_ztfi_0074fr TO lt_facturasii.
        ENDLOOP.
      ELSE.
*        RAISE error_data.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_parse_listado.
    DATA: lt_split  TYPE STANDARD TABLE OF string,
          ldo_data  TYPE REF TO data,
          ldo_data1 TYPE REF TO data.

    FIELD-SYMBOLS: <f_field> TYPE any,
                   <wa>      TYPE any,
                   <table>   TYPE STANDARD TABLE,
                   <lt>      TYPE STANDARD TABLE.
    DATA: lt_data TYPE TABLE OF string.

    lt_data = i_anytable.
* Se crea una estructura generica
    CREATE DATA ldo_data  TYPE STANDARD TABLE OF (i_tabname).
    CREATE DATA ldo_data1 TYPE (i_tabname).
    ASSIGN ldo_data->*  TO <table>.
    ASSIGN ldo_data1->* TO <wa>.

    LOOP AT lt_data INTO DATA(ls_data).
      CLEAR <wa>.
      SPLIT ls_data AT ';' INTO TABLE lt_split.
      LOOP AT lt_split INTO DATA(ls_split).
        ASSIGN COMPONENT sy-tabix
            OF STRUCTURE <wa> TO <f_field>.
        IF sy-subrc EQ 0.
          MOVE ls_split TO <f_field>.
        ENDIF.
      ENDLOOP.

      APPEND <wa> TO <table>.
    ENDLOOP.

    IF <table> IS ASSIGNED.
      MOVE <table> TO e_tabla.
    ENDIF.

  ENDMETHOD.


  METHOD get_semilla.
    DATA: ls_dest          TYPE rfcdest,
          ws_string        TYPE string,
          ws_resp          TYPE string,
          http_status_code TYPE i,
          status_text      TYPE string,
          xml              TYPE xstring,
          xml_result       TYPE xstring,
          w_result         TYPE string,
          xml_table        TYPE STANDARD TABLE OF smum_xmltb,
          lo_err           TYPE REF TO cx_root,
          lv_err_string    TYPE string,
          ls_xmltable      TYPE smum_xmltb,
          len_str          TYPE string,
          zdte_semillaresp TYPE zdte_semilla_resp,
          return           TYPE STANDARD TABLE OF  bapiret2.
*--------------------------------
* Llamado R3
*--------------------------------
    ls_dest = me->co_semilla.
    gs_rc = 0.
    CALL METHOD cl_http_client=>create_by_destination
      EXPORTING
        destination              = ls_dest
      IMPORTING
        client                   = DATA(lo_http_client)
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 6.
    IF sy-subrc = 0.
      "----> Datos de cabecera
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = '~request_method'
          value = 'POST'.

      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'Content-Type'
          value = 'text/xml;charset=UTF-8'. "text/xml;charset=UTF-8'.application/soap+xml

      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'SOAPAction'
          value = 'getSeed'.


      CONCATENATE
  '<?xml version="1.0" encoding="utf-8"?>'
  '<soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:def="http://DefaultNamespace">'
   '<soapenv:Body>'
      '<def:getSeed soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"/>'
   '</soapenv:Body>'
  '</soapenv:Envelope>'

   INTO ws_string.

      DATA(len) = strlen( ws_string ).
      len_str = len.
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'Content-Length'
          value = len_str. "'377'.

      CALL METHOD lo_http_client->request->set_cdata
        EXPORTING
          data   = ws_string
          offset = 0
          length = strlen( ws_string ). "'377'.

*Step-2 : Send request.
      CALL METHOD lo_http_client->send
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2.

*STEP-3 :  GET HTTP RESPONSE
      CALL METHOD lo_http_client->receive
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4.

*STEP-4 : Read HTTP RETURN CODE
      CALL METHOD lo_http_client->response->get_status
        IMPORTING
          code   = http_status_code
          reason = status_text.
      IF http_status_code = '200'. "OK

*STEP-5 :  READ RESPONSE DATA
        CALL METHOD lo_http_client->response->get_cdata
          RECEIVING
            data = w_result.
        w_result = lo_http_client->response->get_cdata( ).
        CLEAR : xml_result.

*Step-6 : convert string to xstring.
        CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
          EXPORTING
            text   = w_result
*           MIMETYPE       = ' '
*           ENCODING       =
          IMPORTING
            buffer = xml_result
          EXCEPTIONS
            failed = 1
            OTHERS = 2.

*Step-7 : create xml
        CALL FUNCTION 'SMUM_XML_PARSE'
          EXPORTING
            xml_input = xml_result
          TABLES
            xml_table = xml_table
            return    = return.
        CLEAR ws_resp.
        LOOP AT xml_table INTO ls_xmltable WHERE cname = 'getSeedReturn'.
          CONCATENATE ws_resp ls_xmltable-cvalue INTO ws_resp.
        ENDLOOP.
        TRY.
            CLEAR zdte_semillaresp.
            CALL TRANSFORMATION zdte_semillaresp
              SOURCE XML  ws_resp
              RESULT respuesta = zdte_semillaresp.

          CATCH cx_root INTO lo_err.
            lv_err_string = lo_err->get_text( ).
            gs_rc = 4.
            lo_http_client->close( ).

            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'E'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '012'.
            gs_bal_msg-msgv1 = gs_rc.
            CONCATENATE 'WS_Semilla/ErrorConver:'  lv_err_string INTO  lv_err_string.
            go_log->contenate_msg(
               EXPORTING texto = lv_err_string
                  descripcion_operacion = ''
                  IMPORTING msgv1 = gs_bal_msg-msgv1
                            msgv2 = gs_bal_msg-msgv2
                            msgv3 = gs_bal_msg-msgv3
                            msgv4 = gs_bal_msg-msgv4 ).
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
            EXIT.
*            CLEAR gs_bal_msg.
*            gs_bal_msg-msgty = 'E'.
*            gs_bal_msg-msgid = 'ZDTE_0001'.
*            gs_bal_msg-msgno = '012'.
*            gs_bal_msg-msgv1 = gs_rc.
*            MOVE ls_dest TO gs_bal_msg-msgv3.
*            MOVE '108 GET_SEMILLA' TO gs_bal_msg-msgv4.
*            MOVE lv_err_string TO gs_bal_msg-msgv2.
*            go_log->add_msg( i_s_msg = gs_bal_msg ).
*            go_log->save( ).
*            EXIT.
        ENDTRY.
        IF zdte_semillaresp-resp_body-semilla IS NOT INITIAL.
          gs_semilla = zdte_semillaresp-resp_body-semilla.
        ELSE.
          gs_rc = 4.
          gs_semilla = ''.
          CASE zdte_semillaresp-resp_hdr-estado.
            WHEN '-1'.
              status_text = zdte_semillaresp-resp_hdr-glosa."Error Mensaje excepcion'.
            WHEN '-2'.
              status_text = zdte_semillaresp-resp_hdr-glosa."Error Retorno'.
          ENDCASE.
          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'E'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '012'.
          gs_bal_msg-msgv1 = gs_rc.
          CONCATENATE 'WS_Semilla/ErrorRetorno:'  status_text INTO  status_text.
          go_log->contenate_msg(
             EXPORTING texto = status_text
                descripcion_operacion = ''
                IMPORTING msgv1 = gs_bal_msg-msgv1
                          msgv2 = gs_bal_msg-msgv2
                          msgv3 = gs_bal_msg-msgv3
                          msgv4 = gs_bal_msg-msgv4 ).
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
*          CLEAR gs_bal_msg.
*          gs_bal_msg-msgty = 'E'.
*          gs_bal_msg-msgid = 'ZDTE_0001'.
*          gs_bal_msg-msgno = '012'.
*          gs_bal_msg-msgv1 = gs_rc.
*          MOVE ls_dest TO gs_bal_msg-msgv3.
*          MOVE '162 GET_SEMILLA' TO gs_bal_msg-msgv4.
*          MOVE status_text TO gs_bal_msg-msgv1.
*          go_log->add_msg( i_s_msg = gs_bal_msg ).
*          go_log->save( ).
        ENDIF.
      ELSE.
        gs_rc = 4.
        "status_text. Texto de error de comunicación
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '012'.
        gs_bal_msg-msgv1 = gs_rc.
        CONCATENATE 'WS_Semilla/ErrorComuni:'  status_text INTO  status_text.
        go_log->contenate_msg(
           EXPORTING texto = status_text
              descripcion_operacion = ''
              IMPORTING msgv1 = gs_bal_msg-msgv1
                        msgv2 = gs_bal_msg-msgv2
                        msgv3 = gs_bal_msg-msgv3
                        msgv4 = gs_bal_msg-msgv4 ).
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
*        CLEAR gs_bal_msg.
*        gs_bal_msg-msgty = 'E'.
*        gs_bal_msg-msgid = 'ZDTE_0001'.
*        gs_bal_msg-msgno = '012'.
*        gs_bal_msg-msgv1 = gs_rc.
*        MOVE ls_dest TO gs_bal_msg-msgv3.
*        MOVE '176 GET_SEMILLA' TO gs_bal_msg-msgv4.
*        MOVE status_text TO gs_bal_msg-msgv1.
*        go_log->add_msg( i_s_msg = gs_bal_msg ).
*        go_log->save( ).

      ENDIF.
      lo_http_client->close( ).
    ELSE.
* Implement suitable error handling here
      gs_rc = sy-subrc.
      CASE gs_rc.
        WHEN '1'.
          status_text = 'argument_not_found'.
        WHEN '2'.
          status_text = 'destination_not_found'.
        WHEN '3'.
          status_text = 'destination_no_authority'.
        WHEN '4'.
          status_text = 'plugin_not_active'.
        WHEN '5'.
          status_text = 'internal_error'.
        WHEN '6'.
          status_text = 'OTHERS'.
      ENDCASE.
      IF gs_rc > 0 .
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '012'.
        gs_bal_msg-msgv1 = gs_rc.
        CONCATENATE 'WS_Semilla/ErrorConexion:'  status_text INTO  status_text.
        go_log->contenate_msg(
           EXPORTING texto = status_text
              descripcion_operacion = ''
              IMPORTING msgv1 = gs_bal_msg-msgv1
                        msgv2 = gs_bal_msg-msgv2
                        msgv3 = gs_bal_msg-msgv3
                        msgv4 = gs_bal_msg-msgv4 ).
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
*        CLEAR gs_bal_msg.
*        gs_bal_msg-msgty = 'E'.
*        gs_bal_msg-msgid = 'ZDTE_0001'.
*        gs_bal_msg-msgno = '012'.
*        gs_bal_msg-msgv1 = gs_rc.
*        MOVE ls_dest TO gs_bal_msg-msgv3.
*        MOVE '207 GET_SEMILLA' TO gs_bal_msg-msgv4.
*        MOVE status_text TO gs_bal_msg-msgv1.
*        go_log->add_msg( i_s_msg = gs_bal_msg ).
*        go_log->save( ).
      ENDIF.
    ENDIF.
**---------------------------------------------------*
** Llamado PI                                        *
**---------------------------------------------------*
*    gs_rc = 0.
*    DATA lv_largo TYPE i.
*    DATA: oref TYPE REF TO cx_ai_system_fault,
*          iref TYPE REF TO cx_ai_application_fault,
*          text TYPE string.
*    DATA: lv_toyear  TYPE inri-toyear.
*    TRY.
*        FREE go_semilla.
*        CREATE OBJECT go_semilla.
*
*      CATCH cx_ai_system_fault INTO oref.
*        text = oref->get_text( ).
*    ENDTRY.
*    CLEAR: wa_output.
*    TRY.
*        CALL METHOD go_semilla->get_seed_sync_out
*          EXPORTING
*            output = wa_input
*          IMPORTING
*            input  = wa_output.
*
*      CATCH cx_ai_system_fault INTO oref.
*        text = oref->get_text( ).
*      CATCH cx_ai_application_fault INTO iref.
*        text = iref->get_text( ).
*    ENDTRY.
*    IF text IS INITIAL.
*      w_result = wa_output-get_seed_return.
*
*      TRY.
*          CLEAR zdte_semillaresp.
*          CALL TRANSFORMATION zdte_semillaresp
*            SOURCE XML  w_result
*            RESULT respuesta = zdte_semillaresp.
*
*        CATCH cx_root INTO lo_err.
*          lv_err_string = lo_err->get_text( ).
*          gs_rc = 4.

*        CLEAR gs_bal_msg.
*        gs_bal_msg-msgty = 'E'.
*        gs_bal_msg-msgid = 'ZDTE_0001'.
*        gs_bal_msg-msgno = '012'.
*        gs_bal_msg-msgv1 = gs_rc.
*        CONCATENATE 'WS_Semilla/ErrorConver:'  lv_err_string INTO  lv_err_string.
*        go_log->contenate_msg(
*         EXPORTING texto = lv_err_string
*          descripcion_operacion = ''
*          IMPORTING msgv1 = gs_bal_msg-msgv1
*                    msgv2 = gs_bal_msg-msgv2
*                    msgv3 = gs_bal_msg-msgv3
*                    msgv4 = gs_bal_msg-msgv4 ).
*        go_log->add_msg( i_s_msg = gs_bal_msg ).
*        go_log->save( ).
*        EXIT.

*      ENDTRY.
*      IF zdte_semillaresp-resp_body-semilla IS NOT INITIAL.
*        gs_semilla = zdte_semillaresp-resp_body-semilla.
*      ELSE.
*        gs_rc = 4.
*        gs_semilla = ''.
*        CASE zdte_semillaresp-resp_hdr-estado.
*          WHEN '-1'.
*            status_text = zdte_semillaresp-resp_hdr-glosa."Error Mensaje excepcion'.
*          WHEN '-2'.
*            status_text = zdte_semillaresp-resp_hdr-glosa."Error Retorno'.
*        ENDCASE.
*        CLEAR gs_bal_msg.
*        gs_bal_msg-msgty = 'E'.
*        gs_bal_msg-msgid = 'ZDTE_0001'.
*        gs_bal_msg-msgno = '012'.
*        gs_bal_msg-msgv1 = gs_rc.
*        CONCATENATE 'WS_Semilla/ErrorRetorno:'  status_text INTO  status_text.
*        go_log->contenate_msg(
*         EXPORTING texto = status_text
*            descripcion_operacion = ''
*            IMPORTING msgv1 = gs_bal_msg-msgv1
*                      msgv2 = gs_bal_msg-msgv2
*                      msgv3 = gs_bal_msg-msgv3
*                      msgv4 = gs_bal_msg-msgv4 ).
*         go_log->add_msg( i_s_msg = gs_bal_msg ).
*         go_log->save( ).
*      ENDIF.
*    ELSE.
*
*      gs_rc = 4.
*      gs_bal_msg-msgty = 'E'.
*      gs_bal_msg-msgid = 'ZDTE_0001'.
*      gs_bal_msg-msgno = '012'.
*      CONCATENATE 'WS_Semilla/ErrorComu:' text INTO text.
*       go_log->contenate_msg(
*         EXPORTING texto = text
*            descripcion_operacion = ''
*            IMPORTING msgv1 = gs_bal_msg-msgv1
*                      msgv2 = gs_bal_msg-msgv2
*                      msgv3 = gs_bal_msg-msgv3
*                      msgv4 = gs_bal_msg-msgv4 ).
*       go_log->add_msg( i_s_msg = gs_bal_msg ).
*       go_log->save( ).
*    ENDIF.
  ENDMETHOD.


  METHOD get_token.
    DATA: ls_dest          TYPE rfcdest,
          ws_string        TYPE string,
          http_status_code TYPE i,
          status_text      TYPE string,
          xml              TYPE xstring,
          xml_result       TYPE xstring,
          w_result         TYPE string,
          xml_table        TYPE STANDARD TABLE OF smum_xmltb,
          ls_xmltable      TYPE  smum_xmltb,
          lo_err           TYPE REF TO cx_root,
          lv_err_string    TYPE string,
          len_str          TYPE string,
          ws_resp          TYPE string,
          zdte_tokenresp   TYPE zdte_token_resp,
          vl_rc            TYPE  abap_cr_lf,
          fin_linea(6)     TYPE  c,
          fin_certif(22)   TYPE c,
          return           TYPE STANDARD TABLE OF  bapiret2.
*--------------------------------
* Llamado R3
*--------------------------------
    ls_dest = me->co_token.

    CALL METHOD cl_http_client=>create_by_destination
      EXPORTING
        destination              = ls_dest
      IMPORTING
        client                   = DATA(lo_http_client)
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 6.
    IF sy-subrc = 0.
      "----> Datos de cabecera
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = '~request_method'
          value = 'POST'.
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'Content-Type'
          value = 'text/xml; charset=utf-8'.
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'Cache-Control'
          value = 'no-cache'.

      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'SOAPAction'
          value = ''.

*XML coding
      IF firmado_arc = space.
        xml_firmado = cl_http_utility=>escape_html( unescaped = xml_firmado ).
*        CLEAR fin_linea.CLEAR fin_certif.
*        vl_rc = cl_abap_char_utilities=>NEWLINE.
*        CONCATENATE '&gt;' vl_rc INTO fin_linea.
*        CONCATENATE vl_rc  '&lt;/X509Certificate' INTO fin_certif.
*        REPLACE ALL  OCCURRENCES OF '&gt;' IN xml_firmado WITH fin_linea.
*        REPLACE '&lt;/X509Certificate' IN xml_firmado WITH  fin_certif.
      ENDIF.
      CLEAR  ws_string.
      vl_rc = cl_abap_char_utilities=>cr_lf.
      CONCATENATE
'<?xml version="1.0" encoding="utf-8" ?>'
'<soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"'
' xmlns:xsd="http://www.w3.org/2001/XMLSchema"'
' xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"'
' xmlns:def="http://DefaultNamespace">'
'<soapenv:Body>'
'<def:getToken soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">'
'<pszXml xsi:type="xsd:string">*</pszXml>'
'</def:getToken>'
'</soapenv:Body>'
'</soapenv:Envelope>' INTO  ws_string.
      REPLACE '*' IN ws_string WITH xml_firmado.
      DATA(len) = strlen( ws_string ).
      len_str = len.
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'Content-Length'
          value = len_str. "'377'.

      CALL METHOD lo_http_client->request->set_cdata
        EXPORTING
          data   = ws_string
          offset = 0
          length = strlen( ws_string ). "'377'.

*Step-2 : Send request.
      CALL METHOD lo_http_client->send
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2.

      CASE sy-subrc .
        WHEN 1. status_text = 'http_communication_failure'.
        WHEN 2. status_text = 'http_invalid_state'.
        WHEN OTHERS.
      ENDCASE.
      IF sy-subrc > 0 .
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '012'.
        gs_bal_msg-msgv1 = sy-subrc.
        MOVE status_text TO gs_bal_msg-msgv2.
        CONCATENATE   'WS_Token/ErrorSend:' status_text INTO status_text.
        go_log->contenate_msg(
                  EXPORTING texto = status_text  descripcion_operacion = ''
                  IMPORTING msgv1 = gs_bal_msg-msgv1
                            msgv2 = gs_bal_msg-msgv2
                            msgv3 = gs_bal_msg-msgv3
                            msgv4 = gs_bal_msg-msgv4 ).

        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
      ENDIF.

*STEP-3 :  GET HTTP RESPONSE
      CALL METHOD lo_http_client->receive
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3.
      CASE sy-subrc .
        WHEN 1. status_text = 'http_communication_failure'.
        WHEN 2. status_text = 'http_invalid_state'.
        WHEN 3. status_text = 'http_processing_failed'.
        WHEN OTHERS.
      ENDCASE.
      IF sy-subrc > 0 .
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '012'.
        gs_bal_msg-msgv1 = sy-subrc.
        CONCATENATE   'WS_Token/ErroReceive:' status_text INTO status_text.
        go_log->contenate_msg(
                  EXPORTING texto = status_text  descripcion_operacion = ''
                  IMPORTING msgv1 = gs_bal_msg-msgv1
                            msgv2 = gs_bal_msg-msgv2
                            msgv3 = gs_bal_msg-msgv3
                            msgv4 = gs_bal_msg-msgv4 ).
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
      ENDIF.
*STEP-4 : Read HTTP RETURN CODE
      CALL METHOD lo_http_client->response->get_status
        IMPORTING
          code   = http_status_code
          reason = status_text.
      IF http_status_code = '200'. "OK

*STEP-5 :  READ RESPONSE DATA
        w_result = lo_http_client->response->get_cdata( ).
        CLEAR : xml_result.

*Step-6 : convert string to xstring.
        CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
          EXPORTING
            text   = w_result
*           MIMETYPE       = ' '
*           ENCODING       =
          IMPORTING
            buffer = xml_result
          EXCEPTIONS
            failed = 1
            OTHERS = 2.

*Step-7 : create xml
        CALL FUNCTION 'SMUM_XML_PARSE'
          EXPORTING
            xml_input = xml_result
          TABLES
            xml_table = xml_table
            return    = return.
        CLEAR ws_resp.
        LOOP AT xml_table INTO ls_xmltable WHERE cname = 'getTokenReturn'.
          CONCATENATE ws_resp ls_xmltable-cvalue INTO ws_resp.
        ENDLOOP.
        TRY.
            CLEAR zdte_tokenresp.
            CALL TRANSFORMATION zdte_tokenresp
              SOURCE XML  ws_resp
              RESULT respuesta = zdte_tokenresp.

          CATCH cx_root INTO lo_err.
            lv_err_string = lo_err->get_text( ).
            gs_rc = 4.
            lo_http_client->close( ).
            IF gs_rc > 0 .
*              CLEAR gs_bal_msg.
*              gs_bal_msg-msgty = 'E'.
*              gs_bal_msg-msgid = 'ZDTE_0001'.
*              gs_bal_msg-msgno = '012'.
*              gs_bal_msg-msgv1 = gs_rc.
*              MOVE lv_err_string TO gs_bal_msg-msgv2.
*              go_log->add_msg( i_s_msg = gs_bal_msg ).
*              go_log->save( ).
              CLEAR gs_bal_msg.
              gs_bal_msg-msgty = 'E'.
              gs_bal_msg-msgid = 'ZDTE_0001'.
              gs_bal_msg-msgno = '012'.
              gs_bal_msg-msgv1 = gs_rc.
              CONCATENATE   'WS_Token/ErrorConver:' lv_err_string INTO lv_err_string.
              go_log->contenate_msg(
               EXPORTING texto = lv_err_string  descripcion_operacion = ''
               IMPORTING msgv1 = gs_bal_msg-msgv1
                         msgv2 = gs_bal_msg-msgv2
                         msgv3 = gs_bal_msg-msgv3
                         msgv4 = gs_bal_msg-msgv4 ).

              go_log->add_msg( i_s_msg = gs_bal_msg ).
              go_log->save( ).
            ENDIF.
            EXIT.
        ENDTRY.

        IF zdte_tokenresp-resp_body-token IS NOT INITIAL.
          gs_token = zdte_tokenresp-resp_body-token.
        ELSE.
          gs_rc = 4.
          gs_token = ''.
          status_text = zdte_tokenresp-resp_hdr-glosa."Error Mensaje excepcion'.
          IF gs_rc > 0 .
*            CLEAR gs_bal_msg.
*            gs_bal_msg-msgty = 'E'.
*            gs_bal_msg-msgid = 'ZDTE_0001'.
*            gs_bal_msg-msgno = '012'.
*            gs_bal_msg-msgv1 = gs_rc.
*            MOVE status_text TO gs_bal_msg-msgv2.
*            go_log->add_msg( i_s_msg = gs_bal_msg ).
*            go_log->save( ).
            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'E'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '012'.
            gs_bal_msg-msgv1 = gs_rc.
            CONCATENATE 'WS_Token/ErrorRetorno:'  status_text INTO  status_text.
            go_log->contenate_msg(
               EXPORTING texto = status_text
                  descripcion_operacion = ''
                  IMPORTING msgv1 = gs_bal_msg-msgv1
                            msgv2 = gs_bal_msg-msgv2
                            msgv3 = gs_bal_msg-msgv3
                            msgv4 = gs_bal_msg-msgv4 ).
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
          ENDIF.
        ENDIF.

      ELSE.
        gs_rc = 4.
        "status_text. Texto de error de comunicación
        IF gs_rc > 0 .
          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'E'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '012'.
          CONCATENATE 'WS_Token/ErrorComuni:'  status_text INTO  status_text.
          go_log->contenate_msg(
               EXPORTING texto =  status_text
                  descripcion_operacion = ''
                  IMPORTING msgv1 = gs_bal_msg-msgv1
                            msgv2 = gs_bal_msg-msgv2
                            msgv3 = gs_bal_msg-msgv3
                            msgv4 = gs_bal_msg-msgv4 ).
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
        ENDIF.
      ENDIF.
      lo_http_client->close( ).
    ELSE.
* Implement suitable error handling here
      gs_rc = sy-subrc.
      CASE gs_rc.
        WHEN '1'.
          status_text = 'argument_not_found'.
        WHEN '2'.
          status_text = 'destination_not_found'.
        WHEN '3'.
          status_text = 'destination_no_authority'.
        WHEN '4'.
          status_text = 'plugin_not_active'.
        WHEN '5'.
          status_text = 'internal_error'.
        WHEN '6'.
          status_text = 'OTHERS'.
      ENDCASE.
      IF gs_rc > 0 .
*        CLEAR gs_bal_msg.
*        gs_bal_msg-msgty = 'E'.
*        gs_bal_msg-msgid = 'ZDTE_0001'.
*        gs_bal_msg-msgno = '012'.
*        gs_bal_msg-msgv1 = gs_rc.
*        MOVE status_text TO gs_bal_msg-msgv2.
*        MOVE ls_dest TO gs_bal_msg-msgv3.
*        MOVE '257 GET_TOKEN' TO gs_bal_msg-msgv4.
*        go_log->add_msg( i_s_msg = gs_bal_msg ).
*        go_log->save( ).
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '012'.
        CONCATENATE 'WS_Token/ErrorConexion:'  status_text INTO  status_text.
        go_log->contenate_msg(
             EXPORTING texto =  status_text
                descripcion_operacion = ''
                IMPORTING msgv1 = gs_bal_msg-msgv1
                          msgv2 = gs_bal_msg-msgv2
                          msgv3 = gs_bal_msg-msgv3
                          msgv4 = gs_bal_msg-msgv4 ).
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
      ENDIF.
    ENDIF.
*----------------------------------------------------*
* LLamada de Pi                                      *
*----------------------------------------------------*
*  DATA: go_token  TYPE REF TO zwsco_get_token_sync_out,
*        wa_input  TYPE zwsget_token,
*        wa_output TYPE zwsget_token_response.
*
*  DATA lv_largo TYPE i.
*  DATA: oref TYPE REF TO cx_ai_system_fault,
*        iref TYPE REF TO cx_ai_application_fault,
*        text TYPE string.
*  DATA: lv_toyear  TYPE inri-toyear.
*  gs_rc = 0.
*  TRY.
*      FREE go_token.
*      CREATE OBJECT go_token.
*
*    CATCH cx_ai_system_fault INTO oref.
*      text = oref->get_text( ).
*  ENDTRY.
*  CLEAR: wa_output. CLEAR wa_input.
*
*  wa_input-psz_xml = xml_firmado.
*
*  TRY.
*      CALL METHOD go_token->get_token_sync_out "->get_seed_sync_out
*        EXPORTING
*          OUTPUT = wa_input
*        IMPORTING
*          input  = wa_output.
*
*    CATCH cx_ai_system_fault INTO oref.
*      text = oref->get_text( ).
*    CATCH cx_ai_application_fault INTO iref.
*      text = iref->get_text( ).
*  ENDTRY.
*  IF text IS INITIAL.
*    w_result = wa_output-get_token_return.
*    CLEAR : xml_result.
*
*
*    TRY.
*        CLEAR zdte_tokenresp.
*        CALL TRANSFORMATION zdte_tokenresp
*          SOURCE XML  w_result
*          RESULT respuesta = zdte_tokenresp.
*
*      CATCH cx_root INTO lo_err.
*        lv_err_string = lo_err->get_text( ).
*        gs_rc = 4.

*        IF gs_rc > 0 .
*          CLEAR gs_bal_msg.
*          gs_bal_msg-msgty = 'E'.
*          gs_bal_msg-msgid = 'ZDTE_0001'.
*          gs_bal_msg-msgno = '012'.
*          gs_bal_msg-msgv1 = gs_rc.
*          CONCATENATE   'WS_Token/ErrorConver:' lv_err_string INTO lv_err_string.
*          go_log->contenate_msg(
*           EXPORTING texto = lv_err_string  descripcion_operacion = ''
*           IMPORTING msgv1 = gs_bal_msg-msgv1
*                     msgv2 = gs_bal_msg-msgv2
*                     msgv3 = gs_bal_msg-msgv3
*                     msgv4 = gs_bal_msg-msgv4 ).
*
*          go_log->add_msg( i_s_msg = gs_bal_msg ).
*          go_log->save( ).
*        ENDIF.
*        EXIT.
*    ENDTRY.
*
*    IF zdte_tokenresp-resp_body-token IS NOT INITIAL.
*      gs_token = zdte_tokenresp-resp_body-token.
*    ELSE.
*      gs_rc = 4.
*      gs_token = ''.
*      status_text = zdte_tokenresp-resp_hdr-glosa."error mensaje excepcion'.
*      CLEAR gs_bal_msg.
*      gs_bal_msg-msgty = 'E'.
*      gs_bal_msg-msgid = 'ZDTE_0001'.
*      gs_bal_msg-msgno = '012'.
*      gs_bal_msg-msgv1 = gs_rc.
*      CONCATENATE 'WS_Token/ErrorRetorno:'  status_text INTO  status_text.
*      go_log->contenate_msg(
*         EXPORTING texto = status_text
*            descripcion_operacion = ''
*            IMPORTING msgv1 = gs_bal_msg-msgv1
*                      msgv2 = gs_bal_msg-msgv2
*                      msgv3 = gs_bal_msg-msgv3
*                      msgv4 = gs_bal_msg-msgv4 ).
*      go_log->add_msg( i_s_msg = gs_bal_msg ).
*      go_log->save( ).
*    ENDIF.
*  ELSE.
*    gs_rc = 4.
*    gs_bal_msg-msgty = 'E'.
*    gs_bal_msg-msgid = 'ZDTE_0001'.
*    gs_bal_msg-msgno = '012'.
*    CONCATENATE 'WS_Token/ErrorComu:' text INTO text.
*    go_log->contenate_msg(
*         EXPORTING texto = text
*            descripcion_operacion = ''
*            IMPORTING msgv1 = gs_bal_msg-msgv1
*                      msgv2 = gs_bal_msg-msgv2
*                      msgv3 = gs_bal_msg-msgv3
*                      msgv4 = gs_bal_msg-msgv4 ).
*    go_log->add_msg( i_s_msg = gs_bal_msg ).
*    go_log->save( ).
*
*  ENDIF.
*ENDIF.
  ENDMETHOD.


  METHOD get_tokenpost.

    CALL METHOD cl_http_client=>create_by_destination
      EXPORTING
        destination              = ls_destp
      IMPORTING
        client                   = DATA(lo_http_client)
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 6.
    IF sy-subrc = 0.
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = if_rest_request=>gc_header_csrf_token
          value = 'Fetch'.
      token = lo_http_client->request->get_header_field( name = if_rest_request=>gc_header_csrf_token ).
    ELSE.
    ENDIF.
  ENDMETHOD.


  METHOD get_xml.
    DATA: zdte_sem TYPE zdte_semilla.
    "---> Semilla
    zdte_sem-item-semilla = gs_semilla.
    "---> Contruir XML a firmar
    CONCATENATE  '<?xml version="1.0" encoding="utf-8"?>'
    '<getToken><item><Semilla>' zdte_sem-item-semilla INTO xml_out.
    CONDENSE xml_out.
    CONCATENATE xml_out '</Semilla></item></getToken>' INTO xml_out.
    CONDENSE xml_out.
    .
    "---> String a xstring
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = xml_out
      IMPORTING
        buffer = lf_bindata
      EXCEPTIONS
        failed = 1
        OTHERS = 2.
    IF sy-subrc <> 0.
      gs_rc = sy-subrc.
      text_error = 'Error in the XML file'.
    ENDIF.


  ENDMETHOD.


  METHOD get_xmlfirmado.
    DATA: gf_psect       TYPE psecontext,
          gf_pseap       TYPE ssfappl,
          gf_hmac        TYPE string,
          gf_xtyp        TYPE sec_xml_dsig_type,
          gf_xtra        TYPE sec_xml_dsig_transform,
          gf_xc14        TYPE sec_xml_dsig_canonicalization,
          gf_xmet        TYPE sec_xml_dsig_method,
          gf_xhash       TYPE ssfhash,
          gf_xid         TYPE string,
          gf_xdsig       TYPE string,
          gf_rfuri       TYPE abap_bool,
          gf_xkinf       TYPE abap_bool,
          gf_xha         TYPE sec_xml_dsig_hash,
          lf_mac_key     TYPE xstring,
          gf_xpath       TYPE string,
          gf_xele        TYPE abap_bool,
          gf_xel         TYPE string,
          gf_xnsp        TYPE string,
          gf_xan         TYPE string,
          gf_xemb        TYPE abap_bool,
          lf_result      TYPE xstring,
          ls_signer      TYPE ssfinfo,
          gf_xemn        TYPE string,
          gf_xend        TYPE abap_bool,
          gf_xch         TYPE abap_bool,
          xml_table      TYPE STANDARD TABLE OF smum_xmltb,
          ls_xmltable    TYPE smum_xmltb,
          text_tab02     TYPE STANDARD TABLE OF zchar76,
          lt_str         TYPE STANDARD TABLE OF string,
          wa_str         TYPE string,
          ws_resp        TYPE string,
          sw             TYPE i,
          cont           TYPE i,
          co_76          TYPE i VALUE 76,
          co_78          TYPE i VALUE 60,
          limite         TYPE i,
          v_len          TYPE i,
          nro            TYPE i,
          fin_linea(3)   TYPE  c,
          fin_certif(19) TYPE c,

          return         TYPE STANDARD TABLE OF bapiret2,
          vl_rc          TYPE abap_cr_lf.


    gf_psect = 'SSFA'.
    gf_xtyp = 'ENV'.
    gf_xtra = 'ENV'.
    gf_xc14 = 'EX'.
    gf_xpath = space.
    gf_rfuri = 'X'.
    gf_hmac = space.
    gf_xele  = space.
    gf_xdsig = space.
    gf_xid = space.
    gf_xel = space.
    gf_xan = 'ID'.
* gf_pseap = 'Z_SII'.
    gf_pseap = gs_codcersii.
    gf_xemb = 'X'.
    gf_xch = 'X'.
    gf_xend = 'X'.
    gf_xemn = space.
    gf_xkinf = 'X'.
    gf_xha = 'SHA1'.
    gf_xhash = 'SHA1'.
    gf_xmet = 'RSA'."space.
    TRY.
        lo_object = cl_sec_sxml_dsignature=>create_reader_instance(
                        if_input        =  lf_bindata
                        if_input_base64 = space )."lf_bfile64 ).

        lo_object->m_pse_context              =  gf_psect.
        lo_object->m_signature_type           =  gf_xtyp.
        lo_object->m_signature_transformation =  gf_xtra. "ENV
        lo_object->m_canonicalization         =  gf_xc14.
        lo_object->m_signature_ns_prefix      =  gf_xdsig.
        lo_object->m_signature_id             =  gf_xid.
        lo_object->m_dsig_hash_algorithm      =  gf_xha.
        lo_object->m_ssf_hash_algorithm       =  gf_xhash.
        lo_object->m_dsig_method              =  gf_xmet.
        IF gf_xtra = cl_sec_sxml_dsignature=>co_transform_xpath.
          lo_object->m_xpath_expression         = gf_xpath.
        ELSEIF gf_xtra = cl_sec_sxml_dsignature=>co_transform_base64.
          lo_object->m_signature_reference_uri  = gf_xpath.
        ELSE.
          IF gf_rfuri = abap_true.
            lo_object->m_signature_reference_uri  = '<none>'. "#EC *
          ENDIF.
        ENDIF.

        IF gf_hmac IS NOT INITIAL.
          lf_mac_key = cl_secxml_helper=>string_2_utf8( gf_hmac ).
        ENDIF.
        IF gf_xele = abap_true.
          lo_object->set_attributes( if_element_name = gf_xel
                                     if_attribute_namespace = gf_xnsp ).
        ELSE.
          lo_object->set_attributes( if_attribute_name = gf_xan
                           if_attribute_namespace = gf_xnsp ).
        ENDIF.
        "IF gf_xsig = abap_true.
        IF gf_pseap IS INITIAL.
          gf_pseap = '<SYST>'.                              "#EC *
        ENDIF.
*        lf_msg = 'Sign'.                                    "#EC NOTEXT
        IF gf_xemb = abap_false.
          IF gf_xtyp = lo_object->co_type_detached.
            lo_object->sign_xml(
                EXPORTING
                  if_ssf_app        = gf_pseap
                  if_add_keyinfo    = gf_xkinf
                  if_add_keyinfo_ex = gf_xkinf
                  if_signer_mackey  = lf_mac_key
                IMPORTING
                  ef_signature_xml  = lf_result ).
          ELSE.
            lo_object->sign_xml(
                EXPORTING
                  if_ssf_app        = gf_pseap
                  if_add_keyinfo    = gf_xkinf
                  if_add_keyinfo_ex = gf_xkinf
                  if_signer_mackey  = lf_mac_key
                IMPORTING
                  ef_signed_xml     = lf_result ).
          ENDIF.
        ELSE.
          lo_object->sign_xml(
              EXPORTING
                if_ssf_app        = gf_pseap
                if_add_keyinfo    = gf_xkinf
                if_add_keyinfo_ex = gf_xkinf
                if_signer_mackey  = lf_mac_key
              IMPORTING
                es_signer        = ls_signer
                ef_signature_xml = lf_result ).
          lo_object->embed_signature(
            EXPORTING
              if_xml            = lf_bindata
              if_signature      = lf_result
              if_embed_as_child = gf_xch
              if_embed_at_end   = gf_xend
              if_embed_into     = gf_xemn
              is_signer         = ls_signer
            IMPORTING
              ef_result         = lf_result ).
        ENDIF.
        sxml_firmado = lf_result.
        CALL FUNCTION 'HR_KR_XSTRING_TO_STRING'
          EXPORTING
            from_codepage = 'UTF-8'
            in_xstring    = lf_result
          IMPORTING
            out_string    = xml_firmado.
        xml_firmado = translate( val = xml_firmado from = 'áéíóúàèìòùñÜüöÖ&' to = 'aeiouaeiounUuoOy' ).
        IF xml_firmado IS INITIAL.
          gs_rc = '4'.
        ENDIF.

        CLEAR fin_linea.CLEAR fin_certif.
        vl_rc = cl_abap_char_utilities=>cr_lf.
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN xml_firmado WITH ''.
        CONCATENATE '>' vl_rc INTO fin_linea.
        CONCATENATE vl_rc  '</X509Certificate' INTO fin_certif.
        REPLACE ALL  OCCURRENCES OF '>' IN xml_firmado WITH fin_linea.
        REPLACE '</X509Certificate' IN xml_firmado WITH  fin_certif.
        CONCATENATE vl_rc  '</Modulus' INTO fin_certif.
        REPLACE '</Modulus' IN xml_firmado WITH  fin_certif.
        CONCATENATE '<Semilla>' vl_rc  INTO fin_certif.
        REPLACE fin_certif IN xml_firmado WITH  '<Semilla>'.

        SPLIT xml_firmado AT vl_rc INTO TABLE lt_str.


        CLEAR ws_resp.
        nro = co_78.
        limite = co_78.

        LOOP AT lt_str INTO wa_str.

          xml_firmadostr = wa_str.
          vl_rc = cl_abap_char_utilities=>cr_lf.

          IF wa_str EQ '<X509Certificate>' OR
             wa_str EQ '<Modulus>' OR
              wa_str EQ '<SignatureValue>'
             .

            nro = co_76.
            limite = co_76.

          ENDIF.
          v_len = strlen( xml_firmadostr ).
          sw = 0.
          cont = 0.

          IF v_len >= 13.
            IF wa_str(13) EQ '<?xml version'.
              CONTINUE.
            ENDIF.
          ENDIF.
          DO.

            IF sw = 0 AND v_len <= limite.

              CONCATENATE  ws_resp wa_str+sw(v_len)  vl_rc   INTO ws_resp.
              sw = 0.
              EXIT.
            ELSE.

              IF  v_len <= ( sw + limite ).
                nro = nro - ( ( sw + limite ) - v_len ) .
                cont = 1.
              ENDIF.
              CONCATENATE  ws_resp wa_str+sw(nro)  vl_rc  INTO ws_resp.
              sw = sw + limite.
              IF cont = 1.
                sw = 0.
                nro = limite.
                EXIT.
              ENDIF.
            ENDIF.

          ENDDO.

        ENDLOOP.

        xml_firmado =  ws_resp.
        CONCATENATE '</getToken>' vl_rc INTO fin_certif.
        REPLACE  fin_certif IN xml_firmado WITH '</getToken>'.
        CONCATENATE  vl_rc '</X509Certificate>'  INTO fin_certif.
        REPLACE  fin_certif IN xml_firmado WITH '</X509Certificate>'.

        CONCATENATE   '<X509Certificate>'   vl_rc  INTO fin_certif.
        REPLACE  fin_certif IN xml_firmado WITH '<X509Certificate>'.


      CATCH cx_sec_sxml_error INTO lx_error.
        lf_msg = lx_error->get_text( ).
        lf_result = lf_bindata.
        gs_rc = '4'.
        text_error = lf_msg.
    ENDTRY.
  ENDMETHOD.


  METHOD main.
    TYPES: BEGIN OF z_sociedad,
             bukrs TYPE bukrs,
           END OF z_sociedad.
    DATA: cant TYPE i.
    DATA: lt_zcb_recfactprovfechasii TYPE STANDARD TABLE OF ztfi_0074fr.
    DATA: lt_zcb_recfactprovfechasii_soc TYPE STANDARD TABLE OF ztfi_0074fr.
    DATA: wa TYPE  ztfi_0074fr.
    DATA: itab TYPE STANDARD TABLE OF z_sociedad.
    DATA: wa_itab TYPE z_sociedad.

    cant = 1.
    IF pa_fldir = 'X' .
      cant = 500.
    ELSE.
      cant = 100.
    ENDIF.
    SELECT * FROM ztfi_0074fr UP TO cant ROWS
    INTO TABLE lt_zcb_recfactprovfechasii
    WHERE bukrs IN r_bukrs
      AND estatus EQ space
      AND tipodte NE '56' AND tipodte NE '61'
      ORDER BY PRIMARY KEY.

    IF lt_zcb_recfactprovfechasii[] IS NOT INITIAL.
      "----> Selecciono sociedades
      LOOP AT lt_zcb_recfactprovfechasii INTO wa.
        wa_itab-bukrs  = wa-bukrs.
        COLLECT wa_itab INTO itab.
      ENDLOOP.

      "----> Busco los registros por soc.y los envio a buscar Fecha SII
      LOOP AT itab INTO wa_itab.
        CLEAR wa. REFRESH lt_zcb_recfactprovfechasii_soc.
        LOOP AT lt_zcb_recfactprovfechasii INTO wa WHERE bukrs = wa_itab-bukrs.
          APPEND  wa TO  lt_zcb_recfactprovfechasii_soc.
        ENDLOOP.

        CALL METHOD me->write_fechasii
          EXPORTING
            s_bukrs                    = wa_itab-bukrs
          CHANGING
            lt_zcb_recfactprovfechasii = lt_zcb_recfactprovfechasii_soc[].
      ENDLOOP.
    ELSE.
      IF go_log IS NOT BOUND.
        CLEAR: gs_bal_log, gs_bal_msg.
        "---> Crea Objeto Logs de interfaz.
        CONCATENATE sy-datum sy-uzeit
                    INTO gs_bal_log-extnumber.

        gs_bal_log-object     = 'ZDTE'.
        gs_bal_log-subobject  = 'RECEPCION'.
        gs_bal_log-aldate     = syst-datum.
        gs_bal_log-altime     = syst-uzeit.
        gs_bal_log-aluser     = syst-uname.
        gs_bal_log-alprog     = syst-repid.
        FREE go_log.
        CREATE OBJECT go_log
          EXPORTING
            i_s_object = gs_bal_log.

        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'S'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '046'.
        gs_bal_msg-msgv1 = ''.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD mov_archivos.
    DATA: lv_file_path   TYPE string,
          lv_file_path_o TYPE string,
          lv_err_string  TYPE string,
          lv_sep         TYPE char1,
          ls_opsystem    TYPE  opsystem,
          lo_err         TYPE REF TO cx_sy_file_open_mode.


    SELECT SINGLE * INTO ls_opsystem
    FROM  opsystem
    WHERE opsys EQ sy-opsys.
    IF ls_opsystem-filesys EQ 'UNIX'.
      lv_sep = '/'.
    ELSE.
      lv_sep = '\'.
    ENDIF.

    CLEAR lv_file_path. CLEAR lv_file_path_o.

    CONCATENATE file_d  lv_sep nomb_archivo  INTO lv_file_path.
    CONDENSE lv_file_path NO-GAPS.

    CONCATENATE file_o  lv_sep nomb_archivo  INTO lv_file_path_o.
    CONDENSE lv_file_path_o NO-GAPS.

    IF file_d IS NOT INITIAL. " Si no envio Destino es porq no se debe copiar
      TRY.
          OPEN DATASET lv_file_path FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
          IF sy-subrc = 0.
            TRANSFER arch TO lv_file_path.
            CLOSE DATASET lv_file_path.
            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'S'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '025'.
            gs_bal_msg-msgv1 = lv_file_path.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
          ELSE.
            gs_bal_msg-msgty = 'E'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '041'.
            gs_bal_msg-msgv1 = lv_file_path.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
          ENDIF.
        CATCH  cx_sy_file_open_mode INTO lo_err.
          lv_err_string = lo_err->get_text( ).
          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'E'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '012'.
          gs_bal_msg-msgv1 = lv_err_string.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
          EXIT.
      ENDTRY.
    ENDIF.
    IF file_o IS NOT INITIAL. "Si no envio Directorio de Origen , es porque no se debe borrar
      TRY.
          DELETE DATASET lv_file_path_o.
          IF sy-subrc <> 0.
            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'E'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '012'.
            gs_bal_msg-msgv1 = lv_err_string.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
            EXIT.
          ELSE.
            CLOSE DATASET lv_file_path_o.
          ENDIF.
        CATCH  cx_sy_file_open_mode INTO lo_err.
          lv_err_string = lo_err->get_text( ).
          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'E'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '012'.
          gs_bal_msg-msgv1 = lv_err_string.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
          EXIT.
      ENDTRY.

    ENDIF.


  ENDMETHOD.


  METHOD proce_regarcfactorin.
    DATA: lt_cabarc     TYPE TABLE OF string,
          lt_listadocab TYPE TABLE OF zdte_listafaccab,
          lt_listadofac TYPE TABLE OF zdte_listafar,
          ls_lifnr      TYPE lifnr,
          ls_doc        TYPE char20,
          ls_bukrs      LIKE LINE OF s_bukrs,
          lv_glosa      TYPE string,
          e_factory_tab TYPE ztt_dte_factory,
          ls_factory    TYPE zst_dte_factory,
          go_logf       TYPE REF TO zcl_bal_log_dte,
          ls_0074cd     TYPE ztfi_0074cd,
          lt_0074cd     TYPE TABLE OF ztfi_0074cd,
          ls_lista      TYPE zdte_listafar,
          p_tipodoc     TYPE ztipodte.
    DATA: amount_external TYPE  bapicurr-bapicurr.


    CLEAR amount_external.

    IF lines( lt_tabla ) GT 0.
      CALL METHOD me->get_parse_listado
        EXPORTING
          i_anytable = lt_tabla
          i_tabname  = 'ZDTE_LISTAFAR'
        IMPORTING
          e_tabla    = lt_listadofac.
    ENDIF.

    READ TABLE s_bukrs INTO ls_bukrs INDEX 1.

    "---> Busca reprocesos
    SELECT * FROM ztfi_0074cd INTO TABLE lt_0074cd
    WHERE bukrs IN s_bukrs.

    CLEAR ls_0074cd.
    LOOP AT lt_0074cd INTO ls_0074cd.
      CLEAR  ls_lista.
      READ TABLE lt_listadofac  INTO ls_lista WITH KEY folio_doc = ls_0074cd-xblnr
                                                          deudor = ls_0074cd-rutrecep
                                                         cedente = ls_0074cd-stcd1
                                                        tipo_doc = ls_0074cd-tipodte.

      IF sy-subrc NE 0.
        ls_lista-folio_doc = ls_0074cd-xblnr .
        ls_lista-tipo_doc =  ls_0074cd-tipodte .
        ls_lista-vendedor =  ls_0074cd-stcd1 .
        ls_lista-deudor =    ls_0074cd-rutrecep .
        ls_lista-cesionario = ls_0074cd-rutcesionario .
        ls_lista-rz_cesiona = ls_0074cd-zrzcesionario .
        ls_lista-rz_cedente = ls_0074cd-zrzcedente .
        ls_lista-cedente =   ls_0074cd-cedente.
        CONCATENATE ls_0074cd-bldat(4) '-'  ls_0074cd-bldat+4(2) '-'  ls_0074cd-bldat+6(2) INTO ls_lista-fch_emis.
        CONDENSE ls_lista-fch_emis NO-GAPS.

        SET COUNTRY 'CL'.
        WRITE: ls_0074cd-mnttotal TO ls_lista-mnt_total CURRENCY 'CLP'.
        CONDENSE ls_lista-mnt_total NO-GAPS.
        REPLACE ALL OCCURRENCES OF '.' IN  ls_lista-mnt_total WITH ''.
        CONDENSE ls_lista-mnt_total NO-GAPS.

        APPEND  ls_lista TO lt_listadofac .
      ELSE.
        DELETE FROM ztfi_0074cd   WHERE xblnr = ls_0074cd-xblnr AND
                              rutrecep = ls_0074cd-rutrecep AND
                                 stcd1 = ls_0074cd-stcd1 AND
                               tipodte = ls_0074cd-tipodte.
      ENDIF.
    ENDLOOP.

    IF lines( lt_listadofac ) GT 0.

      LOOP AT lt_listadofac  ASSIGNING FIELD-SYMBOL(<lt1>).

        CLEAR ls_factory.CLEAR ls_0074cd .
        ls_factory-xblnr = <lt1>-folio_doc.
        ls_factory-tipodte = <lt1>-tipo_doc.
        ls_factory-rut_emisor = <lt1>-vendedor.
        ls_factory-rut_empresa = <lt1>-deudor.
        ls_factory-rut_tenedor = <lt1>-cesionario.
        ls_0074cd-xblnr =  ls_factory-xblnr.
        ls_0074cd-tipodte =  ls_factory-tipodte.
        ls_0074cd-stcd1 = ls_factory-rut_emisor.
        ls_0074cd-rutrecep = ls_factory-rut_empresa.
        ls_0074cd-rutcesionario = ls_factory-rut_tenedor.
        ls_0074cd-zrzcesionario = <lt1>-rz_cesiona.
        ls_0074cd-zrzcedente = <lt1>-rz_cedente.
        ls_0074cd-cedente = <lt1>-cedente.

        "---> Caso archivos reprocesos / se borra y se asigna el archivo de donde viene originalmente
        SELECT SINGLE nomb_archivo INTO ls_0074cd-nomb_archivo FROM ztfi_0074cd
        WHERE xblnr = ls_0074cd-xblnr AND
           rutrecep = ls_0074cd-rutrecep AND
             stcd1 = ls_0074cd-stcd1 AND
           tipodte = ls_0074cd-tipodte.

        IF sy-subrc = 0.
          DELETE FROM ztfi_0074cd   WHERE xblnr = ls_0074cd-xblnr AND
                                rutrecep = ls_0074cd-rutrecep AND
                                   stcd1 = ls_0074cd-stcd1 AND
                                 tipodte = ls_0074cd-tipodte.
        ELSE.
          ls_0074cd-nomb_archivo = nomb_archivo.
        ENDIF.

        CONCATENATE <lt1>-fch_emis(4) <lt1>-fch_emis+5(2) <lt1>-fch_emis+8(2) INTO ls_0074cd-bldat.
        ls_0074cd-waers = 'CLP'.
        amount_external =   <lt1>-mnt_total.
        CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
          EXPORTING
            currency             = ls_0074cd-waers
            amount_external      = amount_external
            max_number_of_digits = 23
          IMPORTING
            amount_internal      = ls_0074cd-mnttotal.

        "---> Crea Objeto Logs por Documento.
        CONCATENATE ls_factory-tipodte '-' ls_factory-xblnr '-' ls_factory-rut_emisor
        INTO gs_bal_log-extnumber.

        gs_bal_log-object     = 'ZDTE'.
        gs_bal_log-subobject  = 'FACTORIN'.
        gs_bal_log-aldate     = syst-datum.
        gs_bal_log-altime     = syst-uzeit.
        gs_bal_log-aluser     = syst-uname.
        gs_bal_log-alprog     = syst-repid.

        FREE go_logf.
        CREATE OBJECT go_logf
          EXPORTING
            i_s_object = gs_bal_log.

        gs_bal_msg-msgty = 'W'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '012'.
        CONCATENATE  'Doc.' ls_factory-tipodte '-' ls_factory-xblnr INTO gs_bal_msg-msgv1.
        CONDENSE gs_bal_msg-msgv1.
        CONCATENATE ' de Soc ' ls_factory-bukrs INTO  gs_bal_msg-msgv2.
        gs_bal_msg-msgv3 = 'Con archivo: '.
        gs_bal_msg-msgv4 = nomb_archivo.
        go_logf->add_msg( i_s_msg = gs_bal_msg ).
        go_logf->save( ).

        "--->Busco Sociedad
        SELECT SINGLE bukrs  INTO ls_factory-bukrs FROM ztfi_0078
        WHERE suscriber EQ  ls_factory-rut_empresa.
        IF ls_factory-bukrs IS INITIAL.
          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'E'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '075'.
          gs_bal_msg-msgv1 = ls_factory-rut_empresa.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
          go_logf->add_msg( i_s_msg = gs_bal_msg ).
          go_logf->save( ).
          EXIT.
        ELSE.
          ls_0074cd-bukrs = ls_factory-bukrs.
        ENDIF.
        "--->Busco Cod.proveedor
        SELECT SINGLE lifnr INTO ls_factory-lifnr FROM lfa1
         WHERE stcd1 EQ ls_factory-rut_emisor.
        IF ls_factory-lifnr IS INITIAL.
          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'W'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '012'.
          CONCATENATE  'Doc.' ls_factory-tipodte '-' ls_factory-xblnr INTO gs_bal_msg-msgv1.
          CONDENSE gs_bal_msg-msgv1.
          CONCATENATE ' de Soc ' ls_factory-bukrs INTO  gs_bal_msg-msgv2.
          gs_bal_msg-msgv3 = 'Proveedor de Doc. No exite en Maestro'.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
          go_logf->add_msg( i_s_msg = gs_bal_msg ).
          go_logf->save( ).
          ls_0074cd-glosa = 'Proveedor de Doc. No exite en Maestro'.
          MODIFY ztfi_0074cd FROM ls_0074cd.
          CONTINUE.
        ELSE.
          ls_0074cd-lifnr = ls_factory-lifnr.
        ENDIF.

        "--->Busco cod. Sap de  tenedor
        SELECT SINGLE lifnr INTO ls_factory-empfb FROM lfa1
         WHERE stcd1 EQ ls_factory-rut_tenedor.
        IF ls_factory-empfb IS INITIAL.
          "----> No Esta creado Proveedor en SAP
          CLEAR ls_doc .
          CONCATENATE  ls_factory-tipodte '-'  ls_factory-xblnr '-'  ls_factory-lifnr INTO
          ls_doc.
          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'E'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '070'.
          gs_bal_msg-msgv1 = ls_doc.
          gs_bal_msg-msgv2 =  ls_factory-bukrs.
          gs_bal_msg-msgv3 = ls_factory-rut_tenedor.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
          go_logf->add_msg( i_s_msg = gs_bal_msg ).
          go_logf->save( ).

          ls_0074cd-glosa = 'Tenedor No tiene Cod.SAp en Maestro'.
          MODIFY ztfi_0074cd FROM ls_0074cd.

          "---> Envio de correo
          CLEAR lv_glosa.
          MESSAGE ID gs_bal_msg-msgid TYPE gs_bal_msg-msgty NUMBER gs_bal_msg-msgno
                          WITH gs_bal_msg-msgv1 gs_bal_msg-msgv2
                               gs_bal_msg-msgv3 gs_bal_msg-msgv4
          INTO lv_glosa.
          CALL METHOD zcl_sii=>envio_correo
            EXPORTING
              s_bukrs    = ls_factory-bukrs
              glosa      = lv_glosa
              asunto     = 'Error en Proceso Factorin'
              clasetexto = 'ZDTE_FACTORIN'
              documento  = ls_doc.
          CONTINUE.
        ENDIF.



        "--->Busco si el tenedor esta asociado al proveedor, si es diferente a proveedor
        IF ls_factory-empfb NE  ls_factory-lifnr.
          CLEAR ls_lifnr.
          SELECT SINGLE lifnr INTO ls_lifnr FROM lfza
             WHERE lifnr EQ ls_factory-lifnr
               AND empfk EQ ls_factory-empfb.
          IF sy-subrc NE 0.
            "----> No Esta asociado Proveedor con receptor de pago
            CLEAR ls_doc .
            CONCATENATE  ls_factory-tipodte  '-' ls_factory-xblnr '-' ls_factory-lifnr INTO
            ls_doc.
            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'W'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '071'.
            gs_bal_msg-msgv1 = ls_doc.
            gs_bal_msg-msgv2 = ls_factory-bukrs.
            gs_bal_msg-msgv3 = ls_factory-rut_tenedor.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
            go_logf->add_msg( i_s_msg = gs_bal_msg ).
            go_logf->save( ).

            "---> Envio de correo
            CLEAR lv_glosa.
            MESSAGE ID gs_bal_msg-msgid TYPE gs_bal_msg-msgty NUMBER gs_bal_msg-msgno
                            WITH gs_bal_msg-msgv1 gs_bal_msg-msgv2
                                 gs_bal_msg-msgv3 gs_bal_msg-msgv4
            INTO lv_glosa.
            CALL METHOD zcl_sii=>envio_correo
              EXPORTING
                s_bukrs    = ls_factory-bukrs
                glosa      = lv_glosa
                asunto     = 'Error en Proceso Factorin'
                clasetexto = 'ZDTE_FACTORIN'
                documento  = ls_doc.
          ENDIF.
        ENDIF.

        "---> Busco Doc.Contable y año no pagados/compensados
*        SELECT  belnr ,  gjahr   INTO ( @ls_factory-belnr , @ls_factory-gjahr ) UP TO 1 ROWS
        SELECT  zdocfi , zanodoc   INTO ( @ls_factory-belnr , @ls_factory-gjahr ) UP TO 1 ROWS
*        FROM zcdsced( p_dia = @sy-datum, p_totdias = 365 )"nvsacc 021024 se cambian las vistas por entidades
          FROM zddl_i_ced( p_dia = @sy-datum, p_totdias = 365 ) "nvsacc 021024
        WHERE bukrs EQ @ls_factory-bukrs
         AND xblnr EQ  @ls_factory-xblnr
         AND tipodte EQ  @ls_factory-tipodte
         AND lifnr EQ   @ls_factory-lifnr.
        ENDSELECT.
        IF sy-subrc NE 0.
          CLEAR gs_bal_msg.
          CLEAR ls_doc .
          CONCATENATE  ls_factory-tipodte  '-' ls_factory-xblnr '-' ls_factory-lifnr INTO
          ls_doc.
          gs_bal_msg-msgty = 'E'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '076'.
          gs_bal_msg-msgv1 = ls_doc.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
          go_logf->add_msg( i_s_msg = gs_bal_msg ).
          go_logf->save( ).
          ls_0074cd-glosa = 'Folio no tiene Doc.Contabilizado o esta Compensado'.
          MODIFY ztfi_0074cd FROM ls_0074cd.
        ELSE.
          REFRESH e_factory_tab. "solo proceso 1, pero envio tabla
          APPEND ls_factory TO e_factory_tab.  "Solo se llena con Facturas Con Doc.Contable

          "---> Llamo a modificar DocumentosFI (lo llamo por cada registro para dejar el log en cada doc).
          CALL METHOD me->update_doc_fi
            EXPORTING
              lt_docfi = e_factory_tab
              go_logf  = go_logf.

        ENDIF.

      ENDLOOP.
      IF e_factory_tab[] IS INITIAL.

        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'W'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '068'.
        gs_bal_msg-msgv1 = ls_bukrs-low.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
      ENDIF.
    ELSE.
      "---> Sin datos de cesion
      CLEAR gs_bal_msg.
      gs_bal_msg-msgty = 'W'.
      gs_bal_msg-msgid = 'ZDTE_0001'.
      gs_bal_msg-msgno = '050'.
      gs_bal_msg-msgv1 = nomb_archivo.
      go_log->add_msg( i_s_msg = gs_bal_msg ).
      go_log->save( ).
    ENDIF.
  ENDMETHOD.


  METHOD put_acepreclamo.
    DATA: ls_dest          TYPE rfcdest,
          ls_token         TYPE string,
          ws_string        TYPE string,
          http_status_code TYPE i,
          status_text      TYPE string,
          xml              TYPE xstring,
          xml_result       TYPE xstring,
          w_result         TYPE string,
          ls_xmltable      TYPE  smum_xmltb,
          lo_err           TYPE REF TO cx_root,
          lv_err_string    TYPE string,
          len_str          TYPE string,
          ws_resp          TYPE string,
          xml_table        TYPE STANDARD TABLE OF smum_xmltb,
          return           TYPE STANDARD TABLE OF  bapiret2.
*-------------------------
* llamado R3
*-------------------------

    ls_dest = me->co_aceptarecl.
    CONCATENATE 'TOKEN=' gs_token INTO ls_token.

    CALL METHOD cl_http_client=>create_by_destination
      EXPORTING
        destination              = ls_dest
      IMPORTING
        client                   = DATA(lo_http_client)
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 6.
    IF sy-subrc = 0.
      "----> Datos de cabecera
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = '~request_method'
          value = 'POST'.
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'Content-Type'
          value = 'application/soap+xml; charset=utf-8'.
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'SOAPAction'
          value = ''.
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'auth-token'
          value = me->get_tokenpost( EXPORTING ls_destp = ls_dest ).
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'cookie'
          value = ls_token.
      CALL METHOD lo_http_client->request->set_cookie
        EXPORTING
          name  = 'TOKEN'
          value = gs_token.

      CONCATENATE
  '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"'
  ' xmlns:ws="http://ws.registroreclamodte.diii.sdi.sii.cl">'
  '<soapenv:Body>'
  '<ws:ingresarAceptacionReclamoDoc>'
  '<rutEmisor>' itab-rutemisor '</rutEmisor>'
  '<dvEmisor>' itab-dvemisor '</dvEmisor>'
  '<tipoDoc>' itab-tipodoc '</tipoDoc>'
  '<folio>' itab-folio '</folio>'
  '<accionDoc>' itab-acciondoc '</accionDoc>'
  '</ws:ingresarAceptacionReclamoDoc>'
  '</soapenv:Body>'
  '</soapenv:Envelope>'
       INTO ws_string.

      DATA(len) = strlen( ws_string ).
      len_str = len.
      CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
          name  = 'Content-Length'
          value = len_str. "'377'.
      CALL METHOD lo_http_client->request->set_cdata
        EXPORTING
          data   = ws_string
          offset = 0
          length = strlen( ws_string ). "'377'.

*Step-2 : Send request.
      CALL METHOD lo_http_client->send
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2.
      CASE sy-subrc .
        WHEN 1.
          status_text = 'http_communication_failure'.
        WHEN 2.
          status_text = 'http_invalid_state'.
        WHEN OTHERS.
      ENDCASE.
      IF sy-subrc > 0 .
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '012'.
        gs_bal_msg-msgv1 = sy-subrc.
        MOVE status_text TO gs_bal_msg-msgv2.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
      ENDIF.

*STEP-3 :  GET HTTP RESPONSE
      CALL METHOD lo_http_client->receive
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3.
      CASE sy-subrc .
        WHEN 1. status_text = 'http_communication_failure'.
        WHEN 2. status_text = 'http_invalid_state'.
        WHEN 3. status_text = 'http_processing_failed'.
        WHEN OTHERS.
      ENDCASE.
      IF sy-subrc > 0 .
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '012'.
        gs_bal_msg-msgv1 = sy-subrc.
        gs_bal_msg-msgv4 = '128 PUT_ACEPRECLAMO'.
        MOVE status_text TO gs_bal_msg-msgv2.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
      ENDIF.
*STEP-4 : Read HTTP RETURN CODE
      CALL METHOD lo_http_client->response->get_status
        IMPORTING
          code   = http_status_code
          reason = status_text.
      IF http_status_code = '200'. "OK

*STEP-5 :  READ RESPONSE DATA
        w_result = lo_http_client->response->get_cdata( ).
        CLEAR : xml_result.

*Step-6 : convert string to xstring.
        CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
          EXPORTING
            text   = w_result
*           MIMETYPE       = ' '
*           ENCODING       =
          IMPORTING
            buffer = xml_result
          EXCEPTIONS
            failed = 1
            OTHERS = 2.

*Step-7 : create xml
        CALL FUNCTION 'SMUM_XML_PARSE'
          EXPORTING
            xml_input = xml_result
          TABLES
            xml_table = xml_table
            return    = return.
        CLEAR ws_resp.
        CLEAR zdte_respar.
        LOOP AT xml_table INTO ls_xmltable WHERE cname = 'codResp'
                                              OR cname = 'descResp'.
          CASE ls_xmltable-cname.
            WHEN 'codResp'.
              zdte_respar-codresp = ls_xmltable-cvalue.
            WHEN 'descResp'.
              zdte_respar-desrep = ls_xmltable-cvalue.
            WHEN OTHERS.
          ENDCASE.
*          CONCATENATE ws_resp ls_xmltable-cvalue INTO ws_resp.
        ENDLOOP.

**        TRY.
**            CLEAR zdte_respar.
**            CALL TRANSFORMATION zdte_respar
**              SOURCE XML  ws_resp
**              RESULT respuesta = zdte_respar.
**
**          CATCH cx_root INTO lo_err.
**            lv_err_string = lo_err->get_text( ).
**            gs_rc = 4.
**
**            IF sy-subrc > 0 .
**              CLEAR gs_bal_msg.
**              gs_bal_msg-msgty = 'E'.
**              gs_bal_msg-msgid = 'ZDTE_0001'.
**              gs_bal_msg-msgno = '012'.
**              gs_bal_msg-msgv1 = gs_rc.
**              gs_bal_msg-msgv4 = '174 PUT_ACEPRECLAMO'.
**              MOVE status_text TO gs_bal_msg-msgv2.
**              go_log->add_msg( i_s_msg = gs_bal_msg ).
**              go_log->save( ).
**            ENDIF.
**
**            EXIT.
**        ENDTRY.
**
**        IF zdte_respar-codresp IS NOT INITIAL.
**          gs_rc = 0.
**        ELSE.
**          gs_rc = 4.
**        ENDIF.

        CASE zdte_respar-codresp.
          WHEN '0' OR '1'. " OK
            gs_rc = 0.

            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'S'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '012'.
            gs_bal_msg-msgv1 = zdte_respar-desrep.
            gs_bal_msg-msgv4 = '174 PUT_ACEPRECLAMO'.
            MOVE status_text TO gs_bal_msg-msgv2.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
          WHEN OTHERS.     " NOK
            gs_rc = 4.

            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'E'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '012'.
            gs_bal_msg-msgv1 = zdte_respar-codresp.
            gs_bal_msg-msgv2 = zdte_respar-desrep.
            gs_bal_msg-msgv4 = '174 PUT_ACEPRECLAMO'.
            MOVE status_text TO gs_bal_msg-msgv3.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
        ENDCASE.

      ELSE.
        gs_rc = 4.
        IF gs_rc > 0 .
          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'E'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '012'.
          gs_bal_msg-msgv1 = gs_rc.
          gs_bal_msg-msgv4 = '196 PUT_ACEPRECLAMO'.
          MOVE 'CodResp vacio' TO gs_bal_msg-msgv2.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
          zdte_respar-desrep = status_text.
          zdte_respar-codresp = 'E'.
          "status_text. Texto de error de comunicación
        ENDIF.
      ENDIF.
      lo_http_client->close( ).
    ELSE.
* Implement suitable error handling here
      gs_rc = sy-subrc.
      CASE gs_rc.
        WHEN '1'.
          status_text = 'argument_not_found'.
        WHEN '2'.
          status_text = 'destination_not_found'.
        WHEN '3'.
          status_text = 'destination_no_authority'.
        WHEN '4'.
          status_text = 'plugin_not_active'.
        WHEN '5'.
          status_text = 'internal_error'.
        WHEN '6'.
          status_text = 'OTHERS'.
      ENDCASE.
      IF gs_rc <> 0 .
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '012'.
        gs_bal_msg-msgv1 = gs_rc.
        gs_bal_msg-msgv2 = status_text.
        gs_bal_msg-msgv4 = '227 PUT_ACEPRECLAMO'.
        MOVE ls_dest TO gs_bal_msg-msgv3.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( )..
        zdte_respar-desrep = status_text.
        zdte_respar-codresp = 'E'.

      ENDIF.
    ENDIF.
*-------------------------
* Llamado PI
*-------------------------

*   DATA: go_ar TYPE REF TO zwsco_get_seed_sync_out,
*          wa_output  TYPE zwsget_seed_response,
*          wa_input   TYPE zwsget_seed.
*    gs_rc = 0.
*    DATA lv_largo TYPE i.
*    DATA: oref TYPE REF TO cx_ai_system_fault,
*          iref TYPE REF TO cx_ai_application_fault,
*          text TYPE string.
*    DATA: lv_toyear  TYPE inri-toyear.
*    TRY.
*        FREE go_ar.
*        CREATE OBJECT go_ar.
*
*      CATCH cx_ai_system_fault INTO oref.
*        text = oref->get_text( ).
*    ENDTRY.
*    CLEAR: wa_output, wa_input.
**<<<<<< llenar wa_input >>>>>>>>>>>>
*   wa_input-rutEmisor =  itab-rutemisor.
*   wa_input-dvEmisor  = itab-dvemisor.
*   wa_input-tipoDoc   =  itab-tipodoc.
*   wa_input-folio     =  itab-folio.
*   wa_input-accionDoc = itab-acciondoc.
*   wa_input-token     = gs_token.
*    TRY.
*        CALL METHOD go_ar->get_seed_sync_out
*          EXPORTING
*            output = wa_input
*          IMPORTING
*            input  = wa_output.
*
*      CATCH cx_ai_system_fault INTO oref.
*        text = oref->get_text( ).
*      CATCH cx_ai_application_fault INTO iref.
*        text = iref->get_text( ).
*    ENDTRY.
*    IF text IS INITIAL.
*        CASE wa_output-return-codresp .
*        WHEN '0' OR '1'. " OK
*          gs_rc = 0.
*          zdte_respar-codresp =  wa_output-return-codresp.
*          zdte_respar-desrep  =  wa_output-return-desrep.
*          CLEAR gs_bal_msg.
*          gs_bal_msg-msgty = 'S'.
*          gs_bal_msg-msgid = 'ZDTE_0001'.
*          gs_bal_msg-msgno = '012'.
*          gs_bal_msg-msgv1 = zdte_respar-desrep.
*          gs_bal_msg-msgv4 = '174 PUT_ACEPRECLAMO'.
*          MOVE status_text TO gs_bal_msg-msgv2.
*          go_log->add_msg( i_s_msg = gs_bal_msg ).
*          go_log->save( ).
*        WHEN OTHERS.     " NOK
*          clear zdte_respar.
*          gs_rc = 4.
*          CLEAR gs_bal_msg.
*          gs_bal_msg-msgty = 'E'.
*          gs_bal_msg-msgid = 'ZDTE_0001'.
*          gs_bal_msg-msgno = '012'.
*          gs_bal_msg-msgv1 = zdte_respar-codresp.
*          gs_bal_msg-msgv2 = zdte_respar-desrep.
*          gs_bal_msg-msgv4 = '174 PUT_ACEPRECLAMO'.
*          MOVE status_text TO gs_bal_msg-msgv3.
*          go_log->add_msg( i_s_msg = gs_bal_msg ).
*          go_log->save( ).
*      ENDCASE.
*
*    ELSE.
*
*      gs_rc = 4.
*      "status_text. Texto de error de comunicación
*      IF gs_rc > 0 .
*        CLEAR gs_bal_msg.
*        gs_bal_msg-msgty = 'E'.
*        gs_bal_msg-msgid = 'ZDTE_0001'.
*        gs_bal_msg-msgno = '012'.
*        gs_bal_msg-msgv1 = gs_rc.
*        MOVE ls_dest TO gs_bal_msg-msgv3.
*        MOVE '226 GET_TOKEN' TO gs_bal_msg-msgv4.
*        MOVE 'Error de comunicación get_token lin:226' TO gs_bal_msg-msgv2.
*        go_log->add_msg( i_s_msg = gs_bal_msg ).
*        go_log->save( ).
*      ENDIF.
*    ENDIF.
  ENDMETHOD.


  METHOD update_doc_fi.
    DATA: change_bkpf TYPE STANDARD TABLE OF bkpf,
          change_bkdf TYPE STANDARD TABLE OF bkdf,
          change_bseg TYPE STANDARD TABLE OF bseg,
          change_bsec TYPE STANDARD TABLE OF bsec,
          change_bsed TYPE STANDARD TABLE OF bsed,
          change_bset TYPE STANDARD TABLE OF bset.
    DATA: dummy_bkdf      TYPE bkdf,
          dummy_bkpf      TYPE bkpf,
          dummy_bseg      TYPE STANDARD TABLE OF fbseg,
          bseg_o          TYPE STANDARD TABLE OF fbseg,
          dummy_bsed      TYPE STANDARD TABLE OF fbsed,
          dummy_bsec      TYPE STANDARD TABLE OF fbsec,
          dummy_bset      TYPE STANDARD TABLE OF fbset,
          dummy_xbseg_add TYPE STANDARD TABLE OF fbseg_add,
          dummy_ybseg_add TYPE STANDARD TABLE OF fbseg_add,
          news_text       TYPE string.
    DATA: ls_doc TYPE char30.
    DATA: objectid TYPE cdhdr-objectid.
    DATA: old_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074.
    DATA: new_ztfi_0074 TYPE STANDARD TABLE OF ztfi_0074.
    DATA: lv_status TYPE zstatdte.

    CLEAR: change_bkpf[],
                     change_bkdf[],
                     change_bseg[],
                     change_bsec[],
                     change_bsed[],
                     change_bset[],
                     old_ztfi_0074[],
                     new_ztfi_0074[],
                     lv_status.

    IF lt_docfi[] IS NOT INITIAL.

      LOOP AT lt_docfi ASSIGNING FIELD-SYMBOL(<f1>).

        SELECT * FROM bkpf INTO TABLE change_bkpf
                           WHERE bukrs = <f1>-bukrs
                           AND belnr = <f1>-belnr
                           AND gjahr = <f1>-gjahr.

        SELECT * FROM bkdf INTO TABLE change_bkdf
                           WHERE bukrs = <f1>-bukrs
                           AND belnr = <f1>-belnr
                           AND gjahr = <f1>-gjahr.

        SELECT * FROM bseg INTO TABLE change_bseg
                           WHERE bukrs = <f1>-bukrs
                           AND belnr = <f1>-belnr
                           AND gjahr = <f1>-gjahr.

        SELECT * FROM bsec INTO TABLE change_bsec
                           WHERE bukrs = <f1>-bukrs
                           AND belnr = <f1>-belnr
                           AND gjahr = <f1>-gjahr.

        SELECT * FROM bsed INTO TABLE change_bsed
                           WHERE bukrs = <f1>-bukrs
                           AND belnr = <f1>-belnr
                           AND gjahr = <f1>-gjahr.

        SELECT * FROM bset INTO TABLE change_bset
                           WHERE bukrs = <f1>-bukrs
                           AND belnr = <f1>-belnr
                           AND gjahr = <f1>-gjahr.

        SELECT *  FROM ztfi_0074 INTO TABLE old_ztfi_0074
             WHERE bukrs EQ <f1>-bukrs
                               AND tipodte EQ <f1>-tipodte
                               AND xblnr EQ <f1>-xblnr
                               AND lifnr EQ <f1>-lifnr.

        "___> modifico bloqueo de pago y Recpetor de pago alternativo.
        REFRESH bseg_o.
        bseg_o[] = change_bseg[].

        LOOP AT change_bseg ASSIGNING FIELD-SYMBOL(<f2>) WHERE koart = 'K'.

          IF  <f1>-empfb EQ  <f1>-lifnr. "Si el cod.Proveedor igual a Tenedor, se deja tenedor en blanco
            <f2>-zlspr = space.
            <f2>-empfb =  space.
          ELSE.
            <f2>-zlspr = <f1>-zlspr.
            <f2>-empfb =  <f1>-empfb.
          ENDIF.
*   Update FI document
          CALL FUNCTION 'CHANGE_DOCUMENT'
            TABLES
              t_bkdf = change_bkdf
              t_bkpf = change_bkpf
              t_bsec = change_bsec
              t_bsed = change_bsed
              t_bseg = change_bseg
              t_bset = change_bset
            EXCEPTIONS
              OTHERS = 1.
          IF sy-subrc <> 0.
            "---> log de errorres de mod.
            CLEAR news_text.
            CALL FUNCTION 'MESSAGE_TEXT_BUILD'
              EXPORTING
                msgid               = 'ICC_CN'
                msgnr               = '870'
                msgv1               = ''
                msgv2               = ''
              IMPORTING
                message_text_output = news_text.
            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'E'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '012'.
            gs_bal_msg-msgv1 = news_text.
            gs_bal_msg-msgv2 = <f2>-belnr.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
            IF go_logf IS BOUND.
              go_logf->add_msg( i_s_msg = gs_bal_msg ).
              go_logf->save( ).
            ENDIF.
          ELSE.
            CLEAR ls_doc .
            CONCATENATE <f1>-tipodte '-' <f1>-xblnr '-' <f1>-lifnr INTO
            ls_doc.
            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'S'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '072'.
            gs_bal_msg-msgv1 = ls_doc.
            gs_bal_msg-msgv3 = <f2>-belnr.
            gs_bal_msg-msgv2 = <f2>-bukrs.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
            IF go_logf IS BOUND.
              go_logf->add_msg( i_s_msg = gs_bal_msg ).
              go_logf->save( ).
            ENDIF.

            "---> Log que se grabo.
*     register changes
            objectid(3)    = sy-mandt.
            objectid+3(4)  = <f1>-bukrs.
            objectid+7(10) = <f1>-belnr.
            objectid+17(4) = <f1>-gjahr.

            CLEAR: dummy_bseg, dummy_bsec, dummy_bsed, dummy_bset, dummy_bkdf, dummy_bkpf.
            "----> Coloco la nueva Bseg
            dummy_bseg[] = change_bseg[].
            CALL FUNCTION 'BELEG_WRITE_DOCUMENT'
              EXPORTING
                objectid  = objectid
                tcode     = sy-tcode
                utime     = sy-uzeit
                udate     = sy-datum
                username  = sy-uname
                n_bkdf    = dummy_bkdf
                o_bkdf    = dummy_bkdf
                n_bkpf    = dummy_bkpf       "new state
                o_bkpf    = dummy_bkpf   "old state
                upd_bkpf  = 'U'
              TABLES
                xbsec     = dummy_bsec
                ybsec     = dummy_bsec
                xbsed     = dummy_bsed
                ybsed     = dummy_bsed
                xbseg     = bseg_o
                ybseg     = dummy_bseg
                xbset     = dummy_bset
                ybset     = dummy_bset
                xbseg_add = dummy_xbseg_add
                ybseg_add = dummy_ybseg_add
              EXCEPTIONS
                OTHERS    = 1.
            IF sy-subrc NE 0.
*              MESSAGE w874(icc_cn).
            ENDIF.
            CLEAR lv_status.
            IF  <f1>-empfb EQ  <f1>-lifnr.
              lv_status = '5'. "La factura fue devuelta al Proveedor (factorin = Proveedor)
            ELSE.
              lv_status = 'F'. "La factura esta en factorin.
            ENDIF.

            UPDATE ztfi_0074 SET status = lv_status WHERE bukrs EQ <f1>-bukrs
                               AND tipodte EQ <f1>-tipodte
                               AND xblnr EQ <f1>-xblnr
                               AND lifnr EQ <f1>-lifnr.
            CLEAR objectid.


            objectid(3)    = sy-mandt.
            objectid+3(4)  = <f1>-bukrs.
            objectid+7(16) = <f1>-xblnr.
            objectid+23(10) = <f1>-lifnr.
            objectid+33(3) = <f1>-tipodte.

            new_ztfi_0074[] = old_ztfi_0074[].
            LOOP AT new_ztfi_0074 ASSIGNING FIELD-SYMBOL(<f3>).
              <f3>-status = lv_status. "'F'.
            ENDLOOP.

            CALL FUNCTION 'ZTFI_0074_WRITE_DOCUMENT'
              IN UPDATE TASK
              EXPORTING
                objectid      = objectid
                tcode         = sy-tcode
                utime         = sy-uzeit
                udate         = sy-datum
                username      = sy-uname
                upd_ztfi_0074 = 'U'
              TABLES
                xztfi_0074    = new_ztfi_0074
                yztfi_0074    = old_ztfi_0074.

            COMMIT WORK.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD valida_cert.
    CLEAR  lg_cer_a.
    lg_cer_a = gs_codcersii.
    IF  lg_cer_a IS INITIAL.
      lg_cer_a = 'Z_SII'.
    ENDIF.
    IF lg_hora_ant IS INITIAL.
      lg_min = 6.
    ELSE.
      lg_min = ( sy-datum - lg_fecha_ant ) * 24 * 60 + ( ( sy-uzeit - lg_hora_ant ) / 60 ).
    ENDIF.
**/  "---> Se autentica cada 5min/si cambia de certificado/Error En auten.Anterior
    CLEAR lg_auten.CLEAR lg_seg.
    IF lg_min GE 5.
      lg_auten = 'X'.
      lg_seg = 0.
    ENDIF.
    IF   ( lg_cer_ant NE lg_cer_a ) AND lg_cer_ant IS NOT INITIAL.
      lg_auten = 'X'.
      lg_seg = 2.
    ENDIF.
    IF lg_token IS INITIAL AND lg_hora_ant IS NOT INITIAL.
      lg_auten = 'X'.
      lg_seg = 2.
    ENDIF.
    IF lg_auten = 'X'.
      IF lg_seg NE 0.
        WAIT UP TO lg_seg SECONDS.
      ENDIF.

      lg_cer_ant = lg_cer_a.
      lg_hora_ant = sy-uzeit.
      lg_fecha_ant = sy-datum.
      lg_min = 0.
      CLEAR lg_token.
    ELSE.
      lg_token = gs_token.
    ENDIF.
    r_token = lg_token.

  ENDMETHOD.


  METHOD write_acepreclamo.
    DATA: ls_dest          TYPE rfcdest,
          ls_token         TYPE string,
          ws_string        TYPE string,
          http_status_code TYPE i,
          status_text      TYPE string,
          xml              TYPE xstring,
          xml_result       TYPE xstring,
          w_result         TYPE string,
          ls_xmltable      TYPE  smum_xmltb,
          lo_err           TYPE REF TO cx_root,
          lv_err_string    TYPE string,
          len_str          TYPE string,
          ws_resp          TYPE string,
          xml_table        TYPE STANDARD TABLE OF smum_xmltb,
          wa_s_msg         TYPE symsg,
          return           TYPE STANDARD TABLE OF  bapiret2,
          cont             TYPE i,
          sw               TYPE i,
          gv_token         TYPE string.


    IF go_log IS NOT BOUND.
      "---> Crea Objeto Logs de interfaz.
      CONCATENATE sy-datum sy-uzeit
                  INTO gs_bal_log-extnumber.
*   GS_BAL_LOG-EXTNUMBER  = 'BUKRSRUTEMISORRXBLNR'.
      gs_bal_log-object     = 'ZDTE'.
      gs_bal_log-subobject  = 'RECEPCION'.
      gs_bal_log-aldate     = syst-datum.
      gs_bal_log-altime     = syst-uzeit.
      gs_bal_log-aluser     = syst-uname.
      gs_bal_log-alprog     = syst-repid.
      FREE go_log.
      CREATE OBJECT go_log
        EXPORTING
          i_s_object = gs_bal_log.
    ENDIF.
*    "----> Autenticar al sii
*    sw = 0.
*    "----> Cod.CertificadoSociedad
*    CLEAR  gs_codcersii.
*    SELECT codcersii INTO gs_codcersii  FROM ztfi_0078 UP TO 1 ROWS
*    WHERE bukrs EQ i_bukrs.
*    ENDSELECT.
*    IF gs_codcersii IS INITIAL.
*      gs_codcersii = 'Z_SII'.
*    ENDIF.
*
*    WHILE sw = 0. "Pedir 5 veces el token
*      cont = cont + 1.
*      CALL METHOD me->get_autenticacion
*        RECEIVING
*          token = gv_token.
*      IF gv_token IS NOT INITIAL.
*        sw = 1.
*      ENDIF.
*      IF cont = 5.
*        sw = 1.
*      ENDIF.
*    ENDWHILE.
    gv_token = gs_token.
    IF gv_token IS NOT INITIAL.
      CALL METHOD me->put_acepreclamo
        EXPORTING
          itab        = itab
        IMPORTING
          zdte_respar = zdte_respar.

    ELSE.
      zdte_respar-codresp ='E'.
      zdte_respar-desrep = 'Error al Autenticarse SII'.
    ENDIF.

  ENDMETHOD.


  METHOD write_fechasii.
    "----> Fecha de recepción de SII

    FIELD-SYMBOLS: <wa_0074fr> TYPE ztfi_0074fr.
    DATA: cont TYPE i.
    DATA: sw TYPE i.
    DATA: gv_token TYPE string.
    DATA: gv_fecharecpsii TYPE string.
    DATA: itab TYPE zdte_fechasii.
    DATA: fecha_c(10) TYPE c.
    DATA: hora_c(8) TYPE c.

    "---> Crea Objeto Logs de interfaz.
    CONCATENATE sy-datum sy-uzeit
                INTO gs_bal_log-extnumber.
*   GS_BAL_LOG-EXTNUMBER  = 'BUKRSRUTEMISORRXBLNR'.
    gs_bal_log-object     = 'ZDTE'.
    gs_bal_log-subobject  = 'RECEPCION'.
    gs_bal_log-aldate     = syst-datum.
    gs_bal_log-altime     = syst-uzeit.
    gs_bal_log-aluser     = syst-uname.
    gs_bal_log-alprog     = syst-repid.
    FREE go_log.
    CREATE OBJECT go_log
      EXPORTING
        i_s_object = gs_bal_log.

    IF lt_zcb_recfactprovfechasii[] IS NOT INITIAL.

      "----> Autenticar al sii
      sw = 0.
      "----> Cod.CertificadoSociedad
      CLEAR  gs_codcersii.
      SELECT codcersii INTO gs_codcersii  FROM ztfi_0078 UP TO 1 ROWS
      WHERE bukrs EQ s_bukrs.
      ENDSELECT.
      IF gs_codcersii IS INITIAL.
        gs_codcersii = 'Z_SII'.
      ENDIF.
**//** Validacion de token por Certificado y tiempo
      gv_token = me->valida_cert( ).
      IF gv_token IS INITIAL.
        WHILE sw = 0. "Pedir 5 veces el token
          cont = cont + 1.
          CALL METHOD me->get_autenticacion
            RECEIVING
              token = gv_token.
          IF gv_token IS NOT INITIAL.
            sw = 1.
          ENDIF.
          IF cont = 5.
            sw = 1.
          ENDIF.
        ENDWHILE.
**//** Validacion de token por Certificado y tiempo
        lg_token = gv_token.
      ENDIF.
      IF gv_token IS NOT INITIAL.
        "---->buscar fechasii
        LOOP AT lt_zcb_recfactprovfechasii ASSIGNING <wa_0074fr>.

          "----> Llamar a WS de fecha de SII
          SPLIT <wa_0074fr>-stcd1 AT '-' INTO itab-rutemisor itab-dvemisor.
          CONDENSE itab-rutemisor NO-GAPS. CONDENSE itab-dvemisor NO-GAPS.
*          itab-rutemisor = <wa_0074fr>-stcd1(8).
*          itab-dvemisor = <wa_0074fr>-stcd1+9(1).
          itab-tipodoc   = <wa_0074fr>-tipodte.
          itab-folio   = <wa_0074fr>-xblnr.

          CALL METHOD me->get_fechasii
            EXPORTING
              itab         = itab
            RECEIVING
              fecharecpsii = gv_fecharecpsii.

          "----> Modificar tabla ztfi_0074FR con fecha
          IF gv_fecharecpsii IS NOT INITIAL.
            CLEAR fecha_c. CLEAR hora_c.
            CONCATENATE gv_fecharecpsii+6(4) gv_fecharecpsii+3(2) gv_fecharecpsii(2) INTO fecha_c.
            CONCATENATE gv_fecharecpsii+11(2) gv_fecharecpsii+14(2) gv_fecharecpsii+17(2) INTO hora_c.
            <wa_0074fr>-fecharsii =  fecha_c.
            <wa_0074fr>-horarsii = hora_c.
            <wa_0074fr>-estatus = 'X'.
            MODIFY ztfi_0074fr FROM <wa_0074fr>.
          ENDIF.
        ENDLOOP.
        IF gv_fecharecpsii IS NOT INITIAL. "Actualizo al menos uno
          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'E'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '065'.
          gs_bal_msg-msgv1 = ''.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
        ENDIF.
      ELSE.

      ENDIF.
    ELSE.
      CLEAR gs_bal_msg.
      gs_bal_msg-msgty = 'E'.
      gs_bal_msg-msgid = 'ZDTE_0001'.
      gs_bal_msg-msgno = '046'.
      gs_bal_msg-msgv1 = ''.
      go_log->add_msg( i_s_msg = gs_bal_msg ).
      go_log->save( ).
    ENDIF.

  ENDMETHOD.


  METHOD ws_factorin.
    DATA:
*          lt_ztfi_0074 TYPE STANDARD TABLE OF zcdsced,"nvsacc 021024 se cambian las vistas por entidades
          lt_ztfi_0074 TYPE STANDARD TABLE OF zddl_i_ced, "nvsacc 021024
          ls_ztfi_0074 LIKE LINE OF lt_ztfi_0074,
          ls_factory   TYPE zst_dte_factory.
    DATA: lt_facturasii TYPE STANDARD TABLE OF ztfi_0074fr.
    DATA: lt_ztfi_0078 TYPE STANDARD TABLE OF ztfi_0078.
    DATA: cont TYPE i.
    DATA: sw TYPE i.
    DATA: ls_doc TYPE char20.
    DATA: lv_glosa TYPE string.
    DATA: ls_lifnr TYPE lifnr.
    DATA: gv_token TYPE string.
    DATA: gv_fecharecpsii TYPE string.
    DATA: itab TYPE zdte_fechasii.
    DATA: fecha_c(10) TYPE c.
    DATA: hora_c(8)  TYPE c.
    DATA: lv_rutfull TYPE string,
          lv_rut     TYPE string,
          lv_dver    TYPE string,
          lv_rutemi  TYPE string,
          lv_dveremi TYPE string,
          lv_folio   TYPE string,
          lv_tipodte TYPE string,
          lv_taxnr   TYPE t001z-party VALUE 'TAXNR'.
    DATA: x_awkey      TYPE bkpf-awkey,
          x_aawsys     TYPE bkpf-awsys,
          ls_zdte_tipo TYPE lfb1-zzdte_tipo.


    CLEAR e_factory_tab. REFRESH e_factory_tab.

    "---> Crea Objeto Logs de interfaz.
    CONCATENATE sy-datum sy-uzeit
                INTO gs_bal_log-extnumber.
*   GS_BAL_LOG-EXTNUMBER  = 'BUKRSRUTEMISORRXBLNR'.
    gs_bal_log-object     = 'ZDTE'.
    gs_bal_log-subobject  = 'RECEPCION'.
    gs_bal_log-aldate     = syst-datum.
    gs_bal_log-altime     = syst-uzeit.
    gs_bal_log-aluser     = syst-uname.
    gs_bal_log-alprog     = syst-repid.
    FREE go_log.
    CREATE OBJECT go_log
      EXPORTING
        i_s_object = gs_bal_log.

    SELECT * FROM ztfi_0078 INTO TABLE lt_ztfi_0078
      WHERE bukrs IN s_bukrs.

    IF lt_ztfi_0078[] IS NOT INITIAL.


      LOOP AT lt_ztfi_0078 ASSIGNING FIELD-SYMBOL(<lt2>).
        "---> Buscar RUT Sociedad
        CLEAR: lv_rutfull,
               lv_rut,
               lv_dver.

        "----> Autenticar al sii
        sw = 0.
        "----> Cod.CertificadoSociedad
        CLEAR gs_codcersii.
        gs_codcersii = <lt2>-codcersii.
        IF gs_codcersii IS INITIAL.
          gs_codcersii = 'Z_SII'.
        ENDIF.

        WHILE sw = 0. "Pedir 5 veces el token
          cont = cont + 1.
          CALL METHOD me->get_autenticacion
            RECEIVING
              token = gv_token.
          IF gv_token IS NOT INITIAL.
            sw = 1.
          ENDIF.
          IF cont = 5.
            sw = 1.
          ENDIF.
        ENDWHILE.
        IF gv_token IS NOT INITIAL.

          CLEAR lv_rutfull. CLEAR  lv_rut. CLEAR lv_dver.

          SELECT SINGLE paval INTO lv_rutfull
          FROM t001z
          WHERE bukrs EQ <lt2>-bukrs
            AND party EQ lv_taxnr.
          IF sy-subrc EQ 0.
            REPLACE ALL OCCURRENCES OF '.' IN lv_rutfull WITH ''.
            SPLIT lv_rutfull AT '-' INTO lv_rut lv_dver.
            CONDENSE lv_rut NO-GAPS.
            CONDENSE lv_dver NO-GAPS.
          ENDIF.

          "---> Buscar tabla 74 de la soc.
          REFRESH lt_ztfi_0074.
          REFRESH lt_facturasii.

          SELECT * INTO CORRESPONDING FIELDS OF TABLE  @lt_ztfi_0074
*          FROM zcdsced( p_dia = @sy-datum, p_totdias = 365 ) "nvsacc 021024 se cambian las vistas por entidades
          FROM zddl_i_ced( p_dia = @sy-datum, p_totdias = 365 ) "nvsacc 021024
          WHERE bukrs EQ @<lt2>-bukrs
            AND  bldat IN @p_fecha.
          IF sy-subrc NE 0.
            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'S'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '069'.
            gs_bal_msg-msgv1 = <lt2>-bukrs.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).

          ELSE.
            LOOP AT lt_ztfi_0074 INTO ls_ztfi_0074.
              CLEAR: lv_rutemi,
                     lv_dveremi,
                     lv_folio,
                     lv_tipodte,
                     ls_factory.
              lv_rutemi = ls_ztfi_0074-stcd1.
              REPLACE ALL OCCURRENCES OF '.' IN lv_rutemi WITH ''.
              SPLIT lv_rutemi AT '-' INTO lv_rutemi lv_dveremi.
              CONDENSE lv_rutemi NO-GAPS.
              CONDENSE lv_dveremi NO-GAPS.

              lv_folio   = ls_ztfi_0074-xblnr.
              lv_tipodte = ls_ztfi_0074-tipodte.

              " Consumir WS para capturar el retenedor
              CALL METHOD me->get_factorin
                EXPORTING
                  p_rutemisor   = lv_rutemi
                  p_dvemisor    = lv_dveremi
                  p_tipodoc     = lv_tipodte
                  p_folio       = lv_folio
                  p_rutempresa  = lv_rut
                  p_dvempresa   = lv_dver
                RECEIVING
                  r_rut_tenedor = ls_factory-rut_tenedor.

              IF ls_factory-rut_tenedor IS NOT INITIAL.

                SELECT SINGLE lifnr INTO ls_factory-empfb FROM lfa1
                  WHERE stcd1 EQ ls_factory-rut_tenedor.
                IF sy-subrc NE 0.
                  "----> No Esta asociado Proveedor con receptor de pago
                  CLEAR ls_doc .
                  CONCATENATE ls_ztfi_0074-tipodte '-' ls_ztfi_0074-xblnr '-' ls_ztfi_0074-lifnr INTO
                  ls_doc.
                  CLEAR gs_bal_msg.
                  gs_bal_msg-msgty = 'E'.
                  gs_bal_msg-msgid = 'ZDTE_0001'.
                  gs_bal_msg-msgno = '070'.
                  gs_bal_msg-msgv1 = ls_doc.
                  gs_bal_msg-msgv2 = ls_ztfi_0074-bukrs.
                  gs_bal_msg-msgv3 = ls_factory-rut_tenedor.
                  go_log->add_msg( i_s_msg = gs_bal_msg ).
                  go_log->save( ).

                  "---> Envio de correo
                  CLEAR lv_glosa.
                  MESSAGE ID gs_bal_msg-msgid TYPE gs_bal_msg-msgty NUMBER gs_bal_msg-msgno
                                  WITH gs_bal_msg-msgv1 gs_bal_msg-msgv2
                                       gs_bal_msg-msgv3 gs_bal_msg-msgv4
                  INTO lv_glosa.
                  CALL METHOD zcl_sii=>envio_correo
                    EXPORTING
                      s_bukrs    = ls_ztfi_0074-bukrs
                      glosa      = lv_glosa
                      asunto     = 'Error en Proceso Factorin'
                      clasetexto = 'ZDTE_FACTORIN'
                      documento  = ls_doc.
                ELSE.
                  CLEAR ls_lifnr.
                  SELECT SINGLE lifnr INTO ls_lifnr FROM lfza
                     WHERE lifnr EQ ls_ztfi_0074-lifnr
                       AND empfk EQ ls_factory-empfb.
                  IF sy-subrc NE 0.
                    "----> No Esta asociado Proveedor con receptor de pago
                    CLEAR ls_doc .
                    CONCATENATE ls_ztfi_0074-tipodte '-' ls_ztfi_0074-xblnr '-' ls_ztfi_0074-lifnr INTO
                    ls_doc.
                    CLEAR gs_bal_msg.
                    gs_bal_msg-msgty = 'E'.
                    gs_bal_msg-msgid = 'ZDTE_0001'.
                    gs_bal_msg-msgno = '071'.
                    gs_bal_msg-msgv1 = ls_doc.
                    gs_bal_msg-msgv2 = ls_ztfi_0074-bukrs.
                    gs_bal_msg-msgv3 = ls_factory-rut_tenedor.
                    go_log->add_msg( i_s_msg = gs_bal_msg ).
                    go_log->save( ).

                    "---> Envio de correo
                    CLEAR lv_glosa.
                    MESSAGE ID gs_bal_msg-msgid TYPE gs_bal_msg-msgty NUMBER gs_bal_msg-msgno
                                    WITH gs_bal_msg-msgv1 gs_bal_msg-msgv2
                                         gs_bal_msg-msgv3 gs_bal_msg-msgv4
                    INTO lv_glosa.
                    CALL METHOD zcl_sii=>envio_correo
                      EXPORTING
                        s_bukrs    = ls_ztfi_0074-bukrs
                        glosa      = lv_glosa
                        asunto     = 'Error en Proceso Factorin'
                        clasetexto = 'ZDTE_FACTORIN'
                        documento  = ls_doc.
                  ENDIF.

                  " Datos return factory
                  ls_factory-bukrs       = ls_ztfi_0074-bukrs.
                  ls_factory-xblnr       = ls_ztfi_0074-xblnr.
                  ls_factory-lifnr       = ls_ztfi_0074-lifnr.
                  ls_factory-tipodte     = ls_ztfi_0074-tipodte.
                  ls_factory-rut_emisor  = ls_ztfi_0074-stcd1.
                  ls_factory-rut_empresa = lv_rutfull.
                  ls_factory-zlspr = 'Z'.
                  ls_factory-belnr = ls_ztfi_0074-zdocfi.
                  ls_factory-gjahr = ls_ztfi_0074-zanodoc.

                  APPEND ls_factory TO e_factory_tab.

                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ELSE.
          " error token
        ENDIF.
      ENDLOOP.
      IF e_factory_tab[] IS NOT INITIAL.
        "---> Llamo a modificar DocumentosFI
        CALL METHOD me->update_doc_fi
          EXPORTING
            lt_docfi = e_factory_tab.
      ELSE.
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'W'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '068'.
        gs_bal_msg-msgv1 = <lt2>-bukrs.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
      ENDIF.


    ENDIF.

  ENDMETHOD.


  METHOD ws_listado_factrecsii.
    DATA: lt_ztfi_0074fr TYPE STANDARD TABLE OF ztfi_0074fr.
    DATA: lt_facturasii TYPE STANDARD TABLE OF ztfi_0074fr.
    DATA: lt_ztfi_0078 TYPE STANDARD TABLE OF ztfi_0078.
    DATA: cont TYPE i.
    DATA: sw TYPE i.
    DATA: gv_token TYPE string.
    DATA: gv_fecharecpsii TYPE string.
    DATA: itab TYPE zdte_fechasii.
    DATA: fecha_c(10) TYPE c.
    DATA: hora_c(8)  TYPE c.
    DATA: lv_rutfull TYPE string,
          lv_rut     TYPE string,
          lv_dver    TYPE string,
          lv_taxnr   TYPE t001z-party VALUE 'TAXNR'.

    "---> Crea Objeto Logs de interfaz.
    CONCATENATE sy-datum sy-uzeit
                INTO gs_bal_log-extnumber.
*   GS_BAL_LOG-EXTNUMBER  = 'BUKRSRUTEMISORRXBLNR'.
    gs_bal_log-object     = 'ZDTE'.
    gs_bal_log-subobject  = 'RECEPCION'.
    gs_bal_log-aldate     = syst-datum.
    gs_bal_log-altime     = syst-uzeit.
    gs_bal_log-aluser     = syst-uname.
    gs_bal_log-alprog     = syst-repid.
    FREE go_log.
    CREATE OBJECT go_log
      EXPORTING
        i_s_object = gs_bal_log.

    SELECT * FROM ztfi_0078 INTO TABLE lt_ztfi_0078
      WHERE bukrs IN s_bukrs.

    IF lt_ztfi_0078[] IS NOT INITIAL.

      LOOP AT lt_ztfi_0078 ASSIGNING FIELD-SYMBOL(<lt2>).
        "---> Buscar RUT Sociedad
        CLEAR: lv_rutfull,
               lv_rut,
               lv_dver.
        "----> Autenticar al sii
        sw = 0.
        "----> Cod.CertificadoSociedad
        CLEAR gs_codcersii.
        gs_codcersii = <lt2>-codcersii.
        IF gs_codcersii IS INITIAL.
          gs_codcersii = 'Z_SII'.
        ENDIF.
        WHILE sw = 0. "Pedir 5 veces el token
          cont = cont + 1.
          CALL METHOD me->get_autenticacion
            RECEIVING
              token = gv_token.
          IF gv_token IS NOT INITIAL.
            sw = 1.
          ENDIF.
          IF cont = 5.
            sw = 1.
          ENDIF.
        ENDWHILE.
        IF gv_token IS NOT INITIAL.

          SELECT SINGLE paval INTO lv_rutfull
          FROM t001z
          WHERE bukrs EQ <lt2>-bukrs
            AND party EQ lv_taxnr.
          IF sy-subrc EQ 0.
            REPLACE ALL OCCURRENCES OF '.' IN lv_rutfull WITH ''.
            SPLIT lv_rutfull AT '-' INTO lv_rut lv_dver.
            CONDENSE lv_rut NO-GAPS.
          ENDIF.

          "---> Buscar 74fr de la soc.
          REFRESH lt_ztfi_0074fr.
          REFRESH lt_facturasii.


          CALL METHOD me->get_listadofacrecsii
            EXPORTING
              p_bukrs       = <lt2>-bukrs
              p_rutemisor   = lv_rutfull
              p_begda       = p_begda
              p_endda       = p_endda
            CHANGING
              lt_facturasii = lt_facturasii.
          IF lt_facturasii[] IS NOT INITIAL. "Si tengo facturas
            SELECT * FROM ztfi_0074fr INTO TABLE lt_ztfi_0074fr
            FOR ALL ENTRIES IN lt_facturasii
                WHERE bukrs EQ lt_facturasii-bukrs
                  AND xblnr EQ lt_facturasii-xblnr
                  AND tipodte EQ lt_facturasii-tipodte
                  AND stcd1 EQ lt_facturasii-stcd1.

            IF lt_ztfi_0074fr[] IS NOT INITIAL.
              "----> elimino los registro que ya tiene fechaSII y estatus X
              LOOP AT lt_ztfi_0074fr ASSIGNING FIELD-SYMBOL(<f1>).
                READ TABLE lt_facturasii ASSIGNING FIELD-SYMBOL(<f2>) WITH KEY bukrs = <f1>-bukrs
                                           xblnr = <f1>-xblnr
                                           tipodte = <f1>-tipodte
                                           stcd1   = <f1>-stcd1.
                IF sy-subrc = 0.
                  IF <f1>-estatus = 'X'.
                    DELETE lt_facturasii WHERE bukrs = <f1>-bukrs
                                                AND xblnr = <f1>-xblnr
                                                AND tipodte = <f1>-tipodte
                                                AND stcd1   = <f1>-stcd1.
                  ELSE.
                    <f2>-exml = <f1>-exml.
                    <f2>-docnum = <f1>-docnum.
                  ENDIF.
                ENDIF.

              ENDLOOP.
            ENDIF.
            IF lt_facturasii[] IS NOT INITIAL.
              MODIFY ztfi_0074fr FROM TABLE lt_facturasii.
              COMMIT WORK AND WAIT.
              CLEAR gs_bal_msg.
              gs_bal_msg-msgty = 'S'.
              gs_bal_msg-msgid = 'ZDTE_0001'.
              gs_bal_msg-msgno = '065'.
              gs_bal_msg-msgv1 = <lt2>-bukrs.
              go_log->add_msg( i_s_msg = gs_bal_msg ).
              go_log->save( ).

            ELSE.
              CLEAR gs_bal_msg.
              gs_bal_msg-msgty = 'S'.
              gs_bal_msg-msgid = 'ZDTE_0001'.
              gs_bal_msg-msgno = '064'.
              gs_bal_msg-msgv1 = <lt2>-bukrs.
              go_log->add_msg( i_s_msg = gs_bal_msg ).
              go_log->save( ).
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD ws_listado_factrecsiinw.
    DATA: lt_ztfi_0074fr TYPE STANDARD TABLE OF ztfi_0074fr.
    DATA: lt_facturasii TYPE STANDARD TABLE OF ztfi_0074fr.
    DATA: lt_facturasii_1  TYPE STANDARD TABLE OF ztfi_0074fr.
    DATA: lt_ztfi_0078 TYPE STANDARD TABLE OF ztfi_0078.
    DATA: cont TYPE i.
    DATA: p_tipodoc TYPE ztipodte.
    DATA: sw TYPE i.
    DATA: gv_token TYPE string.
    DATA: gv_fecharecpsii TYPE string.
    DATA: itab TYPE zdte_fechasii.
    DATA: fecha_c(10) TYPE c.
    DATA: hora_c(8)  TYPE c.
    DATA: lv_rutfull TYPE t001z-paval,
          lv_rut     TYPE string,
          lv_dver    TYPE string,
          lv_taxnr   TYPE t001z-party VALUE 'TAXNR'.

    "---> Crea Objeto Logs de interfaz.
    CONCATENATE sy-datum sy-uzeit
                INTO gs_bal_log-extnumber.
*   GS_BAL_LOG-EXTNUMBER  = 'BUKRSRUTEMISORRXBLNR'.
    gs_bal_log-object     = 'ZDTE'.
    gs_bal_log-subobject  = 'RECEPCION'.
    gs_bal_log-aldate     = syst-datum.
    gs_bal_log-altime     = syst-uzeit.
    gs_bal_log-aluser     = syst-uname.
    gs_bal_log-alprog     = syst-repid.
    FREE go_log.
    CREATE OBJECT go_log
      EXPORTING
        i_s_object = gs_bal_log.

    SELECT * FROM ztfi_0078 INTO TABLE lt_ztfi_0078
      WHERE bukrs IN s_bukrs.

    IF lt_ztfi_0078[] IS NOT INITIAL.

      LOOP AT lt_ztfi_0078 ASSIGNING FIELD-SYMBOL(<lt2>).
        "---> Buscar RUT Sociedad
        CLEAR: lv_rutfull,
               lv_rut,
               lv_dver.
        "----> Autenticar al sii
        sw = 0.
        "----> Cod.CertificadoSociedad
        CLEAR gs_codcersii.
        gs_codcersii = <lt2>-codcersii.
        IF gs_codcersii IS INITIAL.
          gs_codcersii = 'Z_SII'.
        ENDIF.
**//** Validacion de token por Certificado y tiempo
        gv_token = me->valida_cert( ).
        IF gv_token IS INITIAL.
          WHILE sw = 0. "Pedir 5 veces el token
            cont = cont + 1.
            CALL METHOD me->get_autenticacion
              RECEIVING
                token = gv_token.
            IF gv_token IS NOT INITIAL.
              sw = 1.
            ENDIF.
            IF cont = 5.
              sw = 1.
            ENDIF.
          ENDWHILE.
**//** Validacion de token por Certificado y tiempo
          lg_token = gv_token.
        ENDIF.
        IF gv_token IS NOT INITIAL.
          SELECT SINGLE paval INTO lv_rutfull
          FROM t001z
          WHERE bukrs EQ <lt2>-bukrs
            AND party EQ lv_taxnr.
          IF sy-subrc EQ 0.
            SPLIT lv_rutfull AT '-' INTO lv_rut lv_dver.
            REPLACE ALL OCCURRENCES OF '.' IN lv_rut WITH ''.
            CONDENSE lv_rut NO-GAPS.
          ENDIF.

          "---> Buscar 74fr de la soc.
          REFRESH lt_ztfi_0074fr.
          REFRESH lt_facturasii.

          sw = 0.
          cont = 0.
          WHILE sw = 0.
            REFRESH lt_facturasii_1.
            cont = cont + 1.
            CASE cont.
              WHEN '1'.
                p_tipodoc = '33'.
              WHEN '2'.
                p_tipodoc = '34'.
              WHEN '3'.
                p_tipodoc = '56'.
              WHEN '4'.
                p_tipodoc = '61'.
              WHEN '5'.
                p_tipodoc = '46'.
            ENDCASE.

            IF cont = 5.
              sw = 1.
            ENDIF.

            CALL METHOD me->get_listadofactrecsiinw
              EXPORTING
                p_bukrs       = <lt2>-bukrs
                p_rutemisor   = lv_rut
                p_dvemisor    = lv_dver
                p_periodo     = p_periodo
                p_tipodoc     = p_tipodoc
              CHANGING
                lt_facturasii = lt_facturasii_1.
            APPEND LINES OF lt_facturasii_1 TO lt_facturasii.
          ENDWHILE.
          IF lt_facturasii[] IS NOT INITIAL. "Si tengo facturas
* "----> Busco los registro que ya tiene fechaSII (estatus X) y info LC (estatusLC X)
*        de los registros de sii
            SELECT * FROM ztfi_0074fr INTO TABLE lt_ztfi_0074fr
              FOR ALL ENTRIES IN lt_facturasii
                  WHERE bukrs EQ lt_facturasii-bukrs
                    AND xblnr EQ lt_facturasii-xblnr
                    AND tipodte EQ lt_facturasii-tipodte
                    AND stcd1 EQ lt_facturasii-stcd1.
*                    AND estatus EQ 'X'.

            IF lt_ztfi_0074fr[] IS NOT INITIAL.
              "----> elimino los registro que ya tiene fechaSII y estatus X
              LOOP AT lt_ztfi_0074fr ASSIGNING FIELD-SYMBOL(<f1>).
                READ TABLE lt_facturasii ASSIGNING FIELD-SYMBOL(<f2>) WITH KEY bukrs = <f1>-bukrs
                                           xblnr = <f1>-xblnr
                                           tipodte = <f1>-tipodte
                                           stcd1   = <f1>-stcd1.
                IF sy-subrc = 0.
                  IF <f1>-estatus EQ 'X'.
                    DELETE lt_facturasii WHERE bukrs = <f1>-bukrs
                                                AND xblnr = <f1>-xblnr
                                                AND tipodte = <f1>-tipodte
                                                AND stcd1   = <f1>-stcd1.
                  ELSE.
                    <f2>-cpudt = <f1>-cpudt.
                    <f2>-cputm = <f1>-cputm.
                    <f2>-exml = <f1>-exml.
                    <f2>-docnum = <f1>-docnum.
                  ENDIF.
                ENDIF.
              ENDLOOP.
            ENDIF.
            IF lt_facturasii[] IS NOT INITIAL.
              MODIFY ztfi_0074fr FROM TABLE lt_facturasii.
              COMMIT WORK AND WAIT.
              CLEAR gs_bal_msg.
              gs_bal_msg-msgty = 'S'.
              gs_bal_msg-msgid = 'ZDTE_0001'.
              gs_bal_msg-msgno = '065'.
              gs_bal_msg-msgv1 = <lt2>-bukrs.
              go_log->add_msg( i_s_msg = gs_bal_msg ).
              go_log->save( ).

            ELSE.
              CLEAR gs_bal_msg.
              gs_bal_msg-msgty = 'S'.
              gs_bal_msg-msgid = 'ZDTE_0001'.
              gs_bal_msg-msgno = '064'.
              gs_bal_msg-msgv1 = <lt2>-bukrs.
              go_log->add_msg( i_s_msg = gs_bal_msg ).
              go_log->save( ).
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD ws_listado_factsii.
    DATA: lt_ztfi_0074fr TYPE STANDARD TABLE OF ztfi_0074fr.
    DATA: lt_facturasii TYPE STANDARD TABLE OF ztfi_0074fr.
    DATA: lt_ztfi_0078 TYPE STANDARD TABLE OF ztfi_0078.
    DATA: cont TYPE i.
    DATA: sw TYPE i.
    DATA: gv_token TYPE string.
    DATA: gv_fecharecpsii TYPE string.
    DATA: itab TYPE zdte_fechasii.
    DATA: fecha_c(10) TYPE c.
    DATA: hora_c(8)  TYPE c.
    DATA: lv_rutfull TYPE t001z-paval,
          lv_rut     TYPE string,
          lv_dver    TYPE string,
          lv_taxnr   TYPE t001z-party VALUE 'TAXNR'.

    "---> Crea Objeto Logs de interfaz.
    CONCATENATE sy-datum sy-uzeit
                INTO gs_bal_log-extnumber.
*   GS_BAL_LOG-EXTNUMBER  = 'BUKRSRUTEMISORRXBLNR'.
    gs_bal_log-object     = 'ZDTE'.
    gs_bal_log-subobject  = 'RECEPCION'.
    gs_bal_log-aldate     = syst-datum.
    gs_bal_log-altime     = syst-uzeit.
    gs_bal_log-aluser     = syst-uname.
    gs_bal_log-alprog     = syst-repid.
    FREE go_log.
    CREATE OBJECT go_log
      EXPORTING
        i_s_object = gs_bal_log.

    SELECT * FROM ztfi_0078 INTO TABLE lt_ztfi_0078
      WHERE bukrs IN s_bukrs.

    IF lt_ztfi_0078[] IS NOT INITIAL.


      LOOP AT lt_ztfi_0078 ASSIGNING FIELD-SYMBOL(<lt2>).
        "---> Buscar RUT Sociedad
        CLEAR: lv_rutfull,
               lv_rut,
               lv_dver.
        "----> Autenticar al sii
        sw = 0.
        "----> Registro de cond_certificado SII
        CLEAR  gs_codcersii.
        gs_codcersii = <lt2>-codcersii.
        IF gs_codcersii IS INITIAL.
          gs_codcersii = 'Z_SII'.
        ENDIF.
        WHILE sw = 0. "Pedir 5 veces el token
          cont = cont + 1.
          CALL METHOD me->get_autenticacion
            RECEIVING
              token = gv_token.
          IF gv_token IS NOT INITIAL.
            sw = 1.
          ENDIF.
          IF cont = 5.
            sw = 1.
          ENDIF.
        ENDWHILE.
        IF gv_token IS NOT INITIAL.

          SELECT SINGLE paval INTO lv_rutfull
          FROM t001z
          WHERE bukrs EQ <lt2>-bukrs
            AND party EQ lv_taxnr.
          IF sy-subrc EQ 0.
            SPLIT lv_rutfull AT '-' INTO lv_rut lv_dver.
            REPLACE ALL OCCURRENCES OF '.' IN lv_rut WITH ''.
            CONDENSE lv_rut NO-GAPS.
          ENDIF.

          "---> Buscar 74fr de la soc.
          REFRESH lt_ztfi_0074fr.
          REFRESH lt_facturasii.

          CALL METHOD me->get_listadofactsii
            EXPORTING
              p_bukrs       = <lt2>-bukrs
              p_rutemisor   = lv_rut
              p_dvemisor    = lv_dver
              p_periodo     = p_periodo
            CHANGING
              lt_facturasii = lt_facturasii.
          IF lt_facturasii[] IS NOT INITIAL. "Si tengo facturas
* "----> Busco los registro que ya tiene fechaSII (estatus X) y info LC (estatusLC X)
*        de los registros de sii
            SELECT * FROM ztfi_0074fr INTO TABLE lt_ztfi_0074fr
              FOR ALL ENTRIES IN lt_facturasii
                  WHERE bukrs EQ lt_facturasii-bukrs
                    AND xblnr EQ lt_facturasii-xblnr
                    AND tipodte EQ lt_facturasii-tipodte
                    AND stcd1 EQ lt_facturasii-stcd1.
*                    AND estatuslc EQ 'X' "Si se adiciona datos del LC en tabla74fr
*                    AND estatus EQ 'X'.

            IF lt_ztfi_0074fr[] IS NOT INITIAL.
              "----> elimino los registro que ya tiene fechaSII y estatus X
              LOOP AT lt_ztfi_0074fr ASSIGNING FIELD-SYMBOL(<f1>).
                READ TABLE lt_facturasii ASSIGNING FIELD-SYMBOL(<f2>) WITH KEY bukrs = <f1>-bukrs
                                           xblnr = <f1>-xblnr
                                           tipodte = <f1>-tipodte
                                           stcd1   = <f1>-stcd1.
                IF sy-subrc = 0.
                  IF <f1>-estatuslc EQ 'X' AND   <f1>-estatus EQ 'X'.
                    DELETE lt_facturasii WHERE bukrs = <f1>-bukrs
                                                AND xblnr = <f1>-xblnr
                                                AND tipodte = <f1>-tipodte
                                                AND stcd1   = <f1>-stcd1.
                  ELSE.
                    <f2>-exml = <f1>-exml.
                    <f2>-docnum = <f1>-docnum.
                  ENDIF.
                ENDIF.
              ENDLOOP.
            ENDIF.
            IF lt_facturasii[] IS NOT INITIAL.
              MODIFY ztfi_0074fr FROM TABLE lt_facturasii.
              COMMIT WORK AND WAIT.
              CLEAR gs_bal_msg.
              gs_bal_msg-msgty = 'S'.
              gs_bal_msg-msgid = 'ZDTE_0001'.
              gs_bal_msg-msgno = '065'.
              gs_bal_msg-msgv1 = <lt2>-bukrs.
              go_log->add_msg( i_s_msg = gs_bal_msg ).
              go_log->save( ).

            ELSE.
              CLEAR gs_bal_msg.
              gs_bal_msg-msgty = 'S'.
              gs_bal_msg-msgid = 'ZDTE_0001'.
              gs_bal_msg-msgno = '064'.
              gs_bal_msg-msgv1 = <lt2>-bukrs.
              go_log->add_msg( i_s_msg = gs_bal_msg ).
              go_log->save( ).
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
