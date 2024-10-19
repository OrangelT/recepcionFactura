class ZCL_DTE_RECEPCION definition
  public
  create public .

public section.

  types:
    t_fecha TYPE RANGE OF sy-datum .
  types:
    tt_edidd  TYPE STANDARD TABLE OF edidd .
  types:
    ty_cb_recfact_t2 TYPE STANDARD TABLE OF z1mm_dte_encabezado2 .
  types:
    ty_x255 TYPE STANDARD TABLE OF x255 .
  types:
    tr_bukrs TYPE RANGE OF bukrs .
  types:
    ty_cb_recfact_t TYPE STANDARD TABLE OF z1mm_dte_encabezado .
  types:
    ty_dt_recfact_t TYPE STANDARD TABLE OF z1mm_dte_detalle .
  types:
    ty_rf_recfact_t TYPE STANDARD TABLE OF z1mm_dte_refer .
  types:
    ty_im_recfact_t TYPE STANDARD TABLE OF z1mm_dte_impret .
  types:
    ty_dt_recfact_t2 TYPE STANDARD TABLE OF z1mm_dte_det_codl .
  types:
    ty_td_recfact_t TYPE STANDARD TABLE OF z1mm_dte_ted .
  types:
    BEGIN OF ty_dt_recfact2,
        tabix  TYPE sy-tabix,
        tabixp TYPE sy-tabix.
        INCLUDE TYPE z1mm_dte_det_codl.
    TYPES: END OF ty_dt_recfact2 .
  types:
    tt_dt_recfact_t2 TYPE STANDARD TABLE OF ty_dt_recfact2 .
  types:
    BEGIN OF ty_dt_recfact,
        tabix  TYPE sy-tabix,
        tabixp TYPE sy-tabix.
        INCLUDE TYPE z1mm_dte_detalle.
    TYPES: END   OF ty_dt_recfact .
  types:
    tt_dt_recfact TYPE STANDARD TABLE OF ty_dt_recfact .
  types:
    tt_fi0074fr TYPE STANDARD TABLE OF ztfi_0074fr .
  types:
    BEGIN OF ty_file_attr,
        dirname(255) TYPE c, " name of directory. (possibly truncated.)
        name(255)    TYPE c, " name of entry. (possibly truncated.)
        type(10)     TYPE c, " type of entry.
        len(8)       TYPE p DECIMALS 0, " length in bytes.
        owner(8)     TYPE c, " owner of the entry.
        mtime(6)     TYPE p DECIMALS 0, " last modification date,seconds since 1970
        mode(9)      TYPE c, " like "rwx-r-x--x": protection mode.
        mod_date     TYPE d,
        mod_time(8)  TYPE c, " hh:mm:ss
        subrc(4)     TYPE c,
        size         TYPE epsf-epsfilsiz,
      END OF ty_file_attr .

  constants CO_RCVPRN type EDIDC-RCVPRN value 'OPENDTE' ##NO_TEXT.
  constants CO_RCVJUSTME type EDIDC-RCVPRN value 'TCCCLNT400' ##NO_TEXT.
  constants CO_RCVPRT type EDIDC-RCVPRT value 'LS' ##NO_TEXT.
  constants CO_RCVPOR type EDIDC-RCVPOR value 'A000000002' ##NO_TEXT.
  constants CO_SNDPRT type EDIDC-SNDPRT value 'LS' ##NO_TEXT.
  constants CO_OUTPUT type C value '1' ##NO_TEXT.
  constants CO_INPUT type EDIDC-DIRECT value '2' ##NO_TEXT.
  constants CO_Z_DTE_FELEC type EDIDC-MESTYP value 'ZDTE_FELEC' ##NO_TEXT.
  constants CO_ZID_DTE_FELEC type EDIDC-IDOCTP value 'ZID_DTE_FELEC' ##NO_TEXT.
  constants CO_ZID_IDOC type EDIDC-IDOCTP value 'ZID_DTE_FELEC' ##NO_TEXT.
  data GO_LOG type ref to ZCL_BAL_LOG_DTE .
  data GS_BAL_LOG type BAL_S_LOG .
  data GS_BAL_MSG type BAL_S_MSG .
  constants CO_Z1MM_DTE_ENCABEZADO type EDIDD-SEGNAM value 'Z1MM_DTE_ENCABEZADO' ##NO_TEXT.
  constants CO_Z1MM_DTE_ENCABEZADO2 type EDIDD-SEGNAM value 'Z1MM_DTE_ENCABEZADO2' ##NO_TEXT.
  constants CO_Z1MM_DTE_DETALLE type EDIDD-SEGNAM value 'Z1MM_DTE_DETALLE' ##NO_TEXT.
  constants CO_Z1MM_DTE_DET_CODL type EDIDD-SEGNAM value 'Z1MM_DTE_DET_CODL' ##NO_TEXT.
  constants CO_Z1MM_DTE_REFER type EDIDD-SEGNAM value 'Z1MM_DTE_REFER' ##NO_TEXT.
  constants CO_Z1MM_DTE_SUBTOT type EDIDD-SEGNAM value 'Z1MM_DTE_SUBTOT' ##NO_TEXT.
  constants CO_Z1MM_DTE_MONTOPAGO type EDIDD-SEGNAM value 'Z1MM_DTE_MONTOPAGO' ##NO_TEXT.
  constants CO_Z1MM_DTE_IMPRET type EDIDD-SEGNAM value 'Z1MM_DTE_IMPRET' ##NO_TEXT.
  constants CO_Z1MM_DTE_IMPOTRMON type EDIDD-SEGNAM value 'Z1MM_DTE_IMPOTRMON' ##NO_TEXT.
  constants CO_Z1MM_DTE_RESPONSE type EDIDD-SEGNAM value 'Z1MM_DTE_RESPONSE' ##NO_TEXT.
  data CO_Z1MM_DTE_FILENAME type EDIDD-SEGNAM value 'Z1MM_DTE_FILENAME' ##NO_TEXT.
  constants CO_Z1MM_DTE_TED type EDIDD-SEGNAM value 'Z1MM_DTE_TED' ##NO_TEXT.
  constants CO_FPNAME type FPNAME value 'ZDTE_FORM' ##NO_TEXT.
  data GS_XML2 type SMUM_XMLTB .
  data GS_XML type SMUM_XMLTB .
  data:
    gt_xml TYPE TABLE OF smum_xmltb .
  data GT_BUKRS type TR_BUKRS .
  data GS_RECFACT type Z1MM_DTE_ENCABEZADO .
  data GS_RECFACT2 type Z1MM_DTE_ENCABEZADO2 .
  data GS_DTRECFACT2 type Z1MM_DTE_DET_CODL .
  data GS_DTRECFACT type Z1MM_DTE_DETALLE .
  data GS_RFRECFACT type Z1MM_DTE_REFER .
  data GT_RFRECFACT type TY_RF_RECFACT_T .
  data GT_DTRECFACT2 type TY_DT_RECFACT_T2 .
  data GT_DTRECFACT type TY_DT_RECFACT_T .
  data GS_ENCABEZADO2 type Z1MM_DTE_ENCABEZADO2 .
  data GS_FILENAME type Z1MM_DTE_FILENAME .
  data GT_FLAGDIR type FLAG .
  data GT_IMRECFACT type TY_IM_RECFACT_T .
  data GS_IMRECFACT type Z1MM_DTE_IMPRET .
  data GS_DTRECFACT_ID type TY_DT_RECFACT .
  data GT_DTRECFACT_ID type TT_DT_RECFACT .
  data GS_DTRECFACT2_ID type TY_DT_RECFACT2 .
  data GT_DTRECFACT2_ID type TT_DT_RECFACT_T2 .
  data GS_TDRECFACT type Z1MM_DTE_TED .
  data NOMBREXML type PFEFLNAME .
  data:
    gt_fi0074fr TYPE TABLE OF ztfi_0074fr .
  data GT_SXML type XSTRING .
  data GT_SSXML type STRING .
  data GS_VARINT type ZVARINT .
  data LS_NAMEROOT type STRING .
  data GT_DATOS type TT_EDIDD .
  data GO_XML type ref to CL_XML_DOCUMENT .
  data GO_FILE type ref to ZCL_DTE_FILE_PROC .

  methods NOMBRE_PDF
    exporting
      value(NAME) type EPS2FILNAM .
  methods NOMBRE_SFP
    importing
      value(I_BUKRS) type BUKRS optional
    exporting
      !NOMBRE type FPNAME .
  methods MAPPING_LC
    importing
      !T_DATA type STANDARD TABLE
      !P_BUKRS type BUKRS
      !P_PERIODO type SPMON .
  methods CONSTRUCTOR
    importing
      value(I_BUKRS_R) type TR_BUKRS
      value(I_FLAGDIR) type FLAG .
  methods IMPORT_FILE .
  methods DISPLAY_LOG .
  methods WS_IMPORT_DOCUMENTS
    importing
      value(S_BUKRS) type TR_BUKRS optional
      value(S_FECHA) type T_FECHA optional
      value(P_CREASOL) type FLAG optional .
  methods WS_LOAD_PDF
    importing
      !I_REGISTRO type BAPIRETURN
      !I_RUTEMPRESA type CHAR12
    exporting
      !PDF type XSTRING .
  methods WS_AUTDBNET
    importing
      !USUARIO type ZTFI_0078-USUARIO
      !PASSWORD type ZTFI_0078-PASSWORD
      !NOM_WS type CHAR3 .
  methods DIGITO_VER
    importing
      !NUMBER_PART type STRING
    exporting
      !CHECK_DIGIT type CHAR1 .
  methods IMPORT_LC
    importing
      !P_PERIODO type SPMON .
  methods IMPORT_CUADRATURA .
  methods MAPPING_CUADRATURA
    importing
      !T_DATA type STANDARD TABLE
      !P_BUKRS type BUKRS .
  methods DELETE_PRCXML .
  methods CHECK_CUADRATURA
    importing
      !P_BUKRS type BUKRS
      !P_FNAME type STRING
    returning
      value(R_ERROR) type ABAP_BOOL .
  methods IMPORT_CUADRATURANW
    importing
      !P_PERIODO type SPMON .
  methods MAPPING_CUADRATURANW
    importing
      !T_DATA type STANDARD TABLE
      !P_BUKRS type BUKRS
      !P_TIPODOC type ZTIPODTE
      !P_PERIODO type SPMON .
  methods VARIANTE_INTERFAZ
    importing
      value(I_BUKRS) type BUKRS optional .
  methods MAPPINGXML_TO_STRUCH
    importing
      value(PARAMETRO) type ZETFI_0027
      value(LS_TABIX) type SY-TABIX
      value(NODO) type ref to IF_IXML_NODE .
  methods LOAD_SAP_DATAH
    importing
      value(PARAMETRO) type ZEFI_0030
      value(LS_TABIX) type SY-TABIX .
  methods SAVE_LC
    importing
      !P_BUKRS type BUKRS .
  methods LOAD_PDF_FORM_XML
    importing
      value(LOCAL) type FLAG optional
      value(I_BUKRS) type BUKRS optional
    exporting
      !E_PDF_XSTRING type XSTRING
      !E_PDF_STRING type STRING .
  methods LOAD_PDF_FORM
    importing
      value(I_BUKRS) type BUKRS optional
    exporting
      !E_PDF_XSTRING type XSTRING
      !E_PDF_STRING type STRING .
  methods MAPPING_TO_STRUC_1
    importing
      !FILENAME type PFEFLNAMEL optional .
  methods MAPPING_TO_STRUC_2
    importing
      !FILENAME type PFEFLNAMEL optional .
  methods MAPPING_TO_STRUC
    importing
      !FILENAME type PFEFLNAMEL optional .
  methods PARSE_XML_TO_TAB_V2
    importing
      value(FILENAME) type PFEFLNAMEL optional .
  methods LOAD_SAP_DATA_1 .
  methods LOAD_SAP_DATA_2 .
  methods LOAD_SAP_DATA .
  methods GET_NODO_XML_TO_TAB
    importing
      !NODE type ref to IF_IXML_NODE
    exporting
      !T_RETURN type BAPIRET2_T
      !T_XMLTB type STANDARD TABLE .
  methods MOV_ARCHIVOSERROR
    importing
      !FILE_O type SAPB-SAPPFAD
      !FILE_D type SAPB-SAPPFAD
      !NOMB_ARCHIVO type PFEFLNAMEL .
  methods WS_LOAD_XML
    importing
      !I_REGISTRO type BAPIRETURN
      !I_RUTEMPRESA type CHAR12
    exporting
      !XML type STRING .
  methods MOVE_FILE
    importing
      !TYPE_FILE type CHAR3 optional
      !ZPATH type CHAR75 optional
      !XML type STRING optional
      !PDF type XSTRING optional
      !I_REGISTRO type ZDTE_MENSAJE
      !E_TAB type TY_X255 optional
      !I_RUTEMPRESA type CHAR16
    exporting
      !ESTADO type SY-SUBRC .
  methods WS_MARCAR
    importing
      !I_REGISTRO type ANY
      !I_MAIL type ZTFI_0078-SENDER
      !I_RUTEMPRESA type CHAR12 .
  methods SAVE_DATESII
    importing
      !I_REGISTRO type ZDTE_MENSAJE
      !I_BUKRS01 type ZTFI_0088-BUKRS
    exporting
      !CODE_RETURN type SY-SUBRC .
  methods ENVIA_CORREO
    importing
      !I_S_MSG type BAL_S_MSG
      !I_BUKRS type BUKRS .
  methods SAVE_CUADRATURA
    importing
      !P_BUKRS type BUKRS .
  methods GET_DATE_TIME_TZ
    importing
      !I_MTIME type ANY
    exporting
      !E_MOD_TIME type ANY
      !E_MOD_DATE type ANY .
  methods SAVE_PDF_FORM
    importing
      !I_BUKRS type BUKRS optional .
  methods GET_FIRMA_TED
    importing
      !I_DTE_TED type Z1MM_DTE_TED
    returning
      value(R_RESULT) type STRING .
  methods MOVE_FILEXML
    importing
      !FILE_O type SAPB-SAPPFAD
      !FILE_D type SAPB-SAPPFAD
      !NOMB_ARCHIVO type PFEFLNAMEL .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_DTE_RECEPCION IMPLEMENTATION.


  METHOD check_cuadratura.
    CLEAR: r_error.
**//..
    LOOP AT gt_fi0074fr ASSIGNING FIELD-SYMBOL(<fs_fi0074fr>).

**//.. Verificar Formatos de Fechas
      CALL FUNCTION 'RP_CHECK_DATE'
        EXPORTING
          date         = <fs_fi0074fr>-bldat
        EXCEPTIONS
          date_invalid = 1
          OTHERS       = 2.
      IF sy-subrc NE 0.
* Implement suitable error handling here
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'S'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '082'.
        gs_bal_msg-msgv1 = p_bukrs.
        gs_bal_msg-msgv2 = p_fname.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
        r_error = abap_true.
        EXIT.
      ENDIF.

      CALL FUNCTION 'RP_CHECK_DATE'
        EXPORTING
          date         = <fs_fi0074fr>-fecharsii
        EXCEPTIONS
          date_invalid = 1
          OTHERS       = 2.
      IF sy-subrc NE 0.
* Implement suitable error handling here
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'S'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '082'.
        gs_bal_msg-msgv1 = p_bukrs.
        gs_bal_msg-msgv2 = p_fname.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
        r_error = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD constructor.

    "Inicializar objeto xml
    CREATE OBJECT go_xml.

    "Inicializar objeto File
    CREATE OBJECT go_file.

    "Set activity to read xml
    me->gt_bukrs = i_bukrs_r.
    me->gt_flagdir = i_flagdir.
  ENDMETHOD.


  METHOD delete_prcxml.
    DATA: ls_opsystem   TYPE opsystem,
          lv_sep        TYPE char01,
          lv_dirname    TYPE pfeflnamel,
          lv_filename   TYPE string,
          lv_filenam    TYPE pfeflnamel,
          lt_file       TYPE STANDARD TABLE OF salfldir,
          lt_dir_list   TYPE STANDARD TABLE OF eps2fili,
          ls_dir_list   TYPE eps2fili,
          lt_fi0088     TYPE TABLE OF ztfi_0088,
          ls_fi0088     TYPE ztfi_0088,
          ls_user_dir   TYPE user_dir,
          lv_iso        TYPE cpcodepage,
          lv_file_dir   TYPE pfeflnamel,
          lv_line       TYPE string,
          lv_pfeflname  TYPE pfeflnamel,
          lv_all_lines  TYPE string,
          lv_err_string TYPE string,
          lo_err        TYPE REF TO cx_sy_file_open_mode,
          lo_cx_root    TYPE REF TO cx_root.
    DATA: lv_filnam    TYPE epsf-epsfilnam,
          lv_dirnam    TYPE epsf-epsdirnam,
          ls_file_attr TYPE ty_file_attr,
          lv_first     TYPE sy-datum,
          lv_last      TYPE sy-datum.

**//..
    SELECT SINGLE * INTO ls_opsystem
    FROM  opsystem
    WHERE opsys EQ sy-opsys.
    IF ls_opsystem-filesys EQ 'UNIX'.
      lv_sep = '/'.
    ELSE.
      lv_sep = '\'.
    ENDIF.

**//..
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

    "---> Leer tabla de directorios
    SELECT  * INTO TABLE lt_fi0088
    FROM ztfi_0088
    WHERE sysid EQ sy-sysid
      AND bukrs IN me->gt_bukrs.
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

    "Determinar rango fecha inicio-fin
    CONCATENATE sy-datum(6) '01' INTO lv_first.

    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = lv_first
      IMPORTING
        last_day_of_month = lv_last
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    "Busco por sociedad
    LOOP AT lt_fi0088 INTO ls_fi0088.
      "Customized Table ZTFI_0088 get path
      SELECT SINGLE * INTO ls_user_dir
      FROM user_dir
      WHERE aliass EQ ls_fi0088-zdir_prcxml.
      IF sy-subrc EQ 0.
        lv_dirname = ls_user_dir-dirname.
      ELSE.
        "---> Logs de interfaz.
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '026'.
        gs_bal_msg-msgv1 = ls_fi0088-zdir_prcxml.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
        EXIT.
      ENDIF.

**///..
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
        gs_bal_msg-msgno = '024'.
        gs_bal_msg-msgv1 =  lv_dirname.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
        EXIT.
      ENDIF.

      LOOP AT lt_dir_list INTO ls_dir_list
                          WHERE name CS 'xml'
                             OR name CS 'XML'.
        CLEAR: lv_file_dir, lv_line, lv_all_lines.
        CONCATENATE lv_dirname lv_sep ls_dir_list-name
                    INTO lv_file_dir.
**//..
        CLEAR: lv_filnam,
               lv_dirnam,
               ls_file_attr.

        lv_filnam = ls_dir_list-name.
        lv_dirnam = lv_dirname.

**//.. Leer atributos del archivo
        CALL FUNCTION 'EPS_GET_FILE_ATTRIBUTES'
          EXPORTING
            file_name              = lv_filnam
            dir_name               = lv_dirnam
          IMPORTING
            file_size              = ls_file_attr-size
            file_owner             = ls_file_attr-owner
            file_mode              = ls_file_attr-mode
            file_type              = ls_file_attr-type
            file_mtime             = ls_file_attr-mtime
            file_size_long         = ls_file_attr-len
          EXCEPTIONS
            read_directory_failed  = 1
            read_attributes_failed = 2
            OTHERS                 = 3.
        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.

**//.. Validar fecha
*        PERFORM p6_to_date_time_tz(rstr0400) USING ls_file_attr-mtime
*                                                   ls_file_attr-mod_time
*                                                   ls_file_attr-mod_date.
        CALL METHOD me->get_date_time_tz
          EXPORTING
            i_mtime    = ls_file_attr-mtime
          IMPORTING
            e_mod_time = ls_file_attr-mod_time
            e_mod_date = ls_file_attr-mod_date.
        IF ls_file_attr-mod_date BETWEEN lv_first AND lv_last.
          CONTINUE.
        ENDIF.

**//..
        TRY.
            DELETE DATASET lv_file_dir.
            IF sy-subrc NE 0.
              CLEAR gs_bal_msg.
              gs_bal_msg-msgty = 'E'.
              gs_bal_msg-msgid = 'PS'.
              gs_bal_msg-msgno = '083'.
              gs_bal_msg-msgv1 = ls_dir_list-name.
              go_log->add_msg( i_s_msg = gs_bal_msg ).
              go_log->save( ).
            ELSE.
              CLEAR gs_bal_msg.
              gs_bal_msg-msgty = 'S'.
              gs_bal_msg-msgid = 'PS'.
              gs_bal_msg-msgno = '084'.
              gs_bal_msg-msgv1 = ls_dir_list-name.
              go_log->add_msg( i_s_msg = gs_bal_msg ).
              go_log->save( ).
            ENDIF.
          CATCH cx_sy_file_authority INTO lo_cx_root.
            lv_err_string = lo_cx_root->get_text( ).
            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'E'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '012'.
            gs_bal_msg-msgv1 = lv_err_string.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
          CATCH cx_sy_file_open INTO lo_cx_root.
            lv_err_string = lo_cx_root->get_text( ).
            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'E'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '012'.
            gs_bal_msg-msgv1 = lv_err_string.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
        ENDTRY.

      ENDLOOP."Archivos

    ENDLOOP. " LOOP AT lt_fi0088 INTO ls_fi0088.

  ENDMETHOD.


  METHOD digito_ver.
    DATA: length  TYPE i, mult1 TYPE n, mult2 TYPE n VALUE 2,
          prod(2) TYPE n, addi TYPE p VALUE 0, modu(2) TYPE n, rest TYPE p.
    DATA: work_string(50) TYPE c.

    work_string = number_part.

    length = strlen( work_string ).

    SHIFT work_string RIGHT DELETING TRAILING space.

    DO length TIMES.
      SHIFT work_string RIGHT CIRCULAR.
      WRITE work_string(1) TO mult1.
      prod = mult1 * mult2.
      addi = addi  + prod.
      mult2 = mult2 + 1.
      IF mult2 = 8.
        mult2 = 2.
      ENDIF.
    ENDDO.

    rest = addi MOD 11.
    modu =  11 - rest.

    IF modu = 10.
      check_digit  = 'K'.
    ELSEIF modu = 11.
      check_digit  = '0'.
    ELSE.
      check_digit = modu+1(1).
    ENDIF.

  ENDMETHOD.


  METHOD display_log.
    go_log->display( ).
  ENDMETHOD.


  METHOD envia_correo.
    DATA: wa_ztfi_0078 TYPE ztfi_0078.

    DATA: lv_correo TYPE comm_id_long,
          i_text    TYPE STANDARD TABLE OF solisti1,
          r_text    TYPE solisti1,
          wa_mail   TYPE ztfi_0078-correo_texto.

    SELECT SINGLE * FROM ztfi_0078 INTO wa_ztfi_0078
    WHERE bukrs = i_bukrs.

    TRANSLATE wa_ztfi_0078-correo_idoc TO LOWER CASE.
    lv_correo = wa_ztfi_0078-correo_idoc.

    TRANSLATE wa_ztfi_0078-sender TO LOWER CASE.

    r_text = 'Estimados'.
    APPEND r_text TO i_text.
    CLEAR r_text.
    APPEND r_text TO i_text.
    CONCATENATE 'El proceso del Webservice DBNET '
    ' ha finalizado con error.' INTO r_text.
    APPEND r_text TO i_text.

    CLEAR r_text.
    APPEND r_text TO i_text.
    r_text = 'Detalle de errores del WS:'.
    APPEND r_text TO i_text.

    CLEAR r_text.
    APPEND r_text TO i_text.
    r_text = i_s_msg-msgv1.
    APPEND r_text TO i_text.

    CLEAR r_text.
    r_text = i_s_msg-msgv2.
    APPEND r_text TO i_text.

    CLEAR r_text.
    r_text = i_s_msg-msgv3.
    APPEND r_text TO i_text.

    CLEAR r_text.
    r_text = i_s_msg-msgv4.
    APPEND r_text TO i_text.

    CLEAR r_text.
    APPEND r_text TO i_text.
    r_text =
    'Este correo ha sido generado automaticamente. Por favor no responder'
    .
    APPEND r_text TO i_text.
    "-----Envío de e-mail.
    DATA: lt_mailsubject     TYPE sodocchgi1.
    DATA: lt_mailrecipients  TYPE STANDARD TABLE OF somlreci1.
    DATA: ls_mailrecipients TYPE somlreci1.

* Recipients
    CLEAR ls_mailrecipients. REFRESH lt_mailrecipients.
    ls_mailrecipients-rec_type  = 'U'.
    ls_mailrecipients-receiver = lv_correo.
    APPEND ls_mailrecipients TO lt_mailrecipients.
    CLEAR ls_mailrecipients .
* Subject.
    DATA: lv_texto TYPE char200.
    CLEAR lv_texto.
    CONCATENATE 'Error WS DBNET del dia: ' sy-datum INTO lv_texto.
    lt_mailsubject-obj_name = 'EMAIL'.
    lt_mailsubject-obj_langu = sy-langu.
    lt_mailsubject-obj_descr =  lv_texto.
*Emisor
    DATA: lv_sender TYPE soextreci1-receiver.

    lv_sender = wa_ztfi_0078-sender.
* Texto
    DATA: lv_lineas_txt TYPE i.
    DATA: lt_objpack TYPE STANDARD TABLE OF sopcklsti1,
          wa_objpack LIKE LINE OF lt_objpack.

    DESCRIBE TABLE i_text LINES lv_lineas_txt.
    CLEAR wa_objpack-transf_bin.
    wa_objpack-head_start = 1.
    wa_objpack-head_num = 0.
    wa_objpack-body_start = 1.
    wa_objpack-body_num = lv_lineas_txt.
    wa_objpack-doc_type = 'RAW'.
    APPEND wa_objpack TO lt_objpack.

    DATA: lv_tipo TYPE  soextreci1-adr_typ.
    lv_tipo = 'SMTP'.

    CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
      EXPORTING
        document_data              = lt_mailsubject
        sender_address             = lv_sender
        sender_address_type        = lv_tipo
      TABLES
        packing_list               = lt_objpack
        contents_txt               = i_text
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
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD get_date_time_tz.
    DATA: opcode         TYPE x,
          unique, not_found,
          timestamp      TYPE i,
          date           TYPE d,
          time           TYPE t,
          tz             LIKE sy-zonlo,
          timestring(10),
          abapstamp(14),
          abaptstamp     TYPE timestamp.

    timestamp = i_mtime.

    IF sy-zonlo = space.
* Der Benutzer hat keine Zeitzone gepflegt: nehme lokale des App. Srv.
      CALL FUNCTION 'TZON_GET_OS_TIMEZONE'
        IMPORTING
          ef_timezone   = tz
          ef_not_unique = unique
          ef_not_found  = not_found.
      IF unique = 'X' OR not_found = 'X'.
        tz = sy-tzone.
        CONCATENATE 'UTC+' tz INTO tz.
      ENDIF.
    ELSE.
      tz = sy-zonlo.
    ENDIF.

* wandle den Timestamp in ABAP Format um und lass den ABAP konvertieren
    opcode = 3.
    CALL 'RstrDateConv'
      ID 'OPCODE' FIELD opcode
      ID 'TIMESTAMP' FIELD timestamp
      ID 'ABAPSTAMP' FIELD abapstamp.
    abaptstamp = abapstamp.
    CONVERT TIME STAMP abaptstamp TIME ZONE tz INTO DATE date
      TIME time.
    IF sy-subrc <> 0.
      date = abapstamp(8).
      time = abapstamp+8.
    ENDIF.

    WRITE: time(2) TO timestring(2),
           ':' TO timestring+2(1),
           time+2(2) TO timestring+3(2),
           ':' TO timestring+5(1),
           time+4(2) TO timestring+6(2).
    MOVE timestring TO e_mod_time.
    MOVE date TO e_mod_date.
  ENDMETHOD.


  METHOD get_firma_ted.
    DATA: lv_strg1 TYPE string,
          lv_strg2 TYPE string.

    lv_strg1 = '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'.
    lv_strg2 = '<TED version="1.0">'.
    CONCATENATE lv_strg1 lv_strg2 INTO lv_strg1.
    CONCATENATE lv_strg1 '<DD>'   INTO lv_strg1.

    CONCATENATE lv_strg1 '<RE>'  i_dte_ted-re  '</RE>'  INTO lv_strg1.
    CONCATENATE lv_strg1 '<TD>'  i_dte_ted-td  '</TD>'  INTO lv_strg1.
    CONCATENATE lv_strg1 '<F>'   i_dte_ted-f   '</F>'   INTO lv_strg1.
    CONCATENATE lv_strg1 '<FE>'  i_dte_ted-fe  '</FE>'  INTO lv_strg1.
    CONCATENATE lv_strg1 '<RR>'  i_dte_ted-rr  '</RR>'  INTO lv_strg1.
    CONCATENATE lv_strg1 '<RSR>' i_dte_ted-rsr '</RSR>' INTO lv_strg1.
    CONCATENATE lv_strg1 '<MNT>' i_dte_ted-mnt '</MNT>' INTO lv_strg1.
    CONCATENATE lv_strg1 '<IT1>' i_dte_ted-it1 '</IT1>' INTO lv_strg1.

    lv_strg2 = '<CAF version="1.0">'.
    CONCATENATE lv_strg1 lv_strg2 INTO lv_strg1.
    CONCATENATE lv_strg1 '<DA>'   INTO lv_strg1.

    CONCATENATE lv_strg1 '<RE>'   i_dte_ted-re  '</RE>' INTO lv_strg1.
    CONCATENATE lv_strg1 '<RS>'   i_dte_ted-rs  '</RS>' INTO lv_strg1.
    CONCATENATE lv_strg1 '<TD>'   i_dte_ted-td  '</TD>' INTO lv_strg1.

    CONCATENATE lv_strg1 '<RNG>'  INTO lv_strg1.
    CONCATENATE lv_strg1 '<D>'    i_dte_ted-d   '</D>'  INTO lv_strg1.
    CONCATENATE lv_strg1 '<H>'    i_dte_ted-h   '</H>'  INTO lv_strg1.
    CONCATENATE lv_strg1 '</RNG>' INTO lv_strg1.

    CONCATENATE lv_strg1 '<FA>'   i_dte_ted-fa  '</FA>' INTO lv_strg1.

    CONCATENATE lv_strg1 '<RSAPK>' INTO lv_strg1.
    CONCATENATE lv_strg1 '<M>'     i_dte_ted-m   '</M>'  INTO lv_strg1.
    CONCATENATE lv_strg1 '<E>'     i_dte_ted-e   '</E>'  INTO lv_strg1.
    CONCATENATE lv_strg1 '</RSAPK>' INTO lv_strg1.

    CONCATENATE lv_strg1 '<IDK>'   i_dte_ted-idk  '</IDK>' INTO lv_strg1.

    CONCATENATE lv_strg1 '</DA>'   INTO lv_strg1.
    CONCATENATE lv_strg1 '<FRMA algoritmo="SHA1withRSA">' i_dte_ted-frma '</FRMA>' INTO lv_strg1.

    CONCATENATE lv_strg1 '</CAF>'   INTO lv_strg1.
    CONCATENATE lv_strg1 '<TSTED>'  i_dte_ted-tsted  '</TSTED>' INTO lv_strg1.

    CONCATENATE lv_strg1 '</DD>'   INTO lv_strg1.
    CONCATENATE lv_strg1 '<FRMT algoritmo="SHA1withRSA">' i_dte_ted-frmt '</FRMT>' INTO lv_strg1.

    CONCATENATE lv_strg1 '</TED>'  INTO lv_strg1.
    r_result = lv_strg1.
  ENDMETHOD.


  METHOD get_nodo_xml_to_tab.
    DATA: lo_xml    TYPE REF TO cl_xml_document_base,
          lx_xml    TYPE xstring,
          ls_xml    TYPE string,
          ls_return TYPE bapiret2,
          lt_return TYPE bapiret2_t.

**    lo_xml ?= go_xml->get_node_subtree( EXPORTING node = node ).
    CALL METHOD go_xml->get_node_subtree
      EXPORTING
        node    = node
*       embedded_only = SPACE
      RECEIVING
        subtree = lo_xml.

    lo_xml->render_2_string( IMPORTING stream = ls_xml ).

    IF lo_xml->parse_string( ls_xml ) EQ 0.
      CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
        EXPORTING
          text   = ls_xml
        IMPORTING
          buffer = lx_xml
        EXCEPTIONS
          failed = 1
          OTHERS = 2.
      IF sy-subrc <> 0.
        MESSAGE 'Error in the XML file' TYPE 'E'.
      ELSE.
* This function module is used to parse the XML and get the
* data in the form of a table
        CALL FUNCTION 'SMUM_XML_PARSE'
          EXPORTING
            xml_input = lx_xml
          TABLES
            xml_table = t_xmltb
            return    = lt_return
          EXCEPTIONS
            OTHERS    = 0.
*"If XML parsing is not successful, return table will contain error messages
        READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
        IF sy-subrc EQ 0.
          MESSAGE 'Error converting the input XML file' TYPE 'E'.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD import_cuadratura.
    DATA: ls_opsystem       TYPE opsystem,
          lv_sep            TYPE char01,
          lv_existe         TYPE abap_bool,
          lv_dirname        TYPE pfeflnamel,
          lv_filename       TYPE string,
          lv_fname          TYPE string,
          lv_filenam        TYPE pfeflnamel,
          lt_return         TYPE bapireturn_t,
          ls_return         TYPE bapiret2,
          lt_file           TYPE STANDARD TABLE OF salfldir,
          lt_dir_list       TYPE STANDARD TABLE OF eps2fili,
          ls_dir_list       TYPE eps2fili,
          lt_tabla          TYPE TABLE OF string,
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
          lo_err            TYPE REF TO cx_sy_file_open_mode,
          lo_cx_root        TYPE REF TO cx_root,
          ls_bukrs          LIKE LINE OF gt_bukrs,
          lv_rutempresa(10) TYPE c,
          lv_error          TYPE abap_bool,
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

    "---> Leer RUT Sociedad
    SELECT * INTO TABLE lt_t001z
    FROM t001z
    WHERE bukrs IN me->gt_bukrs
      AND party EQ 'TAXNR'.
**//..
    CASE gt_flagdir.
      WHEN abap_true. " servidor
        "---> Leer tabla de directorios
        SELECT  * INTO TABLE lt_fi0088
        FROM ztfi_0088
        WHERE sysid EQ sy-sysid
          AND bukrs IN me->gt_bukrs.
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

        "Busco por sociedad
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
            EXIT.
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
            EXIT.
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
            gs_bal_msg-msgno = '024'.
            gs_bal_msg-msgv1 =  lv_dirname.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
            EXIT.
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
            EXIT.
          ELSE.
            lv_rutempresa = ls_t001z-paval.
            REPLACE ALL OCCURRENCES OF '.' IN  lv_rutempresa WITH ''.
            CONDENSE lv_rutempresa NO-GAPS.
          ENDIF.

**//.. Revisar archios
          LOOP AT lt_dir_list INTO ls_dir_list
                              WHERE name CS 'csv'
                                 OR name CS 'CSV'.
            CLEAR: lv_file_dir, lv_line, lv_all_lines.
            ADD 1 TO lv_cont.
            CONCATENATE lv_dirname lv_sep ls_dir_list-name INTO lv_file_dir.
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

**//.. Verificar nombre fichero con Rut Sociedad
            lv_leng = strlen( ls_dir_list-name ).
            IF lv_leng GT 10.
              IF ls_dir_list-name(10) NE lv_rutempresa.
                "---> Logs de interfaz.
                CLEAR gs_bal_msg.
                gs_bal_msg-msgty = 'E'.
                gs_bal_msg-msgid = 'ZDTE_0001'.
                gs_bal_msg-msgno = '081'.
                gs_bal_msg-msgv1 = ls_fi0088-bukrs.
                go_log->add_msg( i_s_msg = gs_bal_msg ).
                go_log->save( ).
                EXIT.
              ENDIF.
            ELSE.
              "---> Logs de interfaz.
              CLEAR gs_bal_msg.
              gs_bal_msg-msgty = 'E'.
              gs_bal_msg-msgid = 'ZDTE_0001'.
              gs_bal_msg-msgno = '081'.
              gs_bal_msg-msgv1 = ls_fi0088-bukrs.
              go_log->add_msg( i_s_msg = gs_bal_msg ).
              go_log->save( ).
              EXIT.
            ENDIF.

**//..
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
              EXIT.
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
                    CLEAR: lv_line.
                    CONCATENATE lv_all_lines lv_line INTO lv_all_lines.
                  ENDDO.

                  CLEAR: lv_pfeflname.
                  lv_pfeflname = ls_dir_list-name.

                  me->mapping_cuadratura( EXPORTING
                                            t_data = lt_tabla
                                            p_bukrs = ls_fi0088-bukrs ).
                  .
                  CLEAR: lv_fname.
                  lv_fname = ls_dir_list-name.
                  lv_error = me->check_cuadratura( EXPORTING
                                                     p_bukrs = ls_fi0088-bukrs
                                                     p_fname = lv_fname ).

                  CHECK lv_error NE abap_true.

                  me->save_cuadratura( EXPORTING p_bukrs = ls_fi0088-bukrs ).

                  "Agregar Log
                  CLEAR gs_bal_msg.
                  gs_bal_msg-msgty = 'S'.
                  gs_bal_msg-msgid = 'ZDTE_0001'.
                  gs_bal_msg-msgno = '025'.
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
                  EXIT.
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
                  EXIT.
              ENDTRY.
            ENDIF.
**//..
            TRY.
                DELETE DATASET lv_file_dir.
                IF sy-subrc <> 0.
                  CLEAR gs_bal_msg.
                  gs_bal_msg-msgty = 'E'.
                  gs_bal_msg-msgid = 'ZDTE_0001'.
                  gs_bal_msg-msgno = '012'.
                  gs_bal_msg-msgv1 = lv_err_string.
                  go_log->add_msg( i_s_msg = gs_bal_msg ).
                  go_log->save( ).
                  EXIT.
                ENDIF.
              CATCH cx_sy_file_authority INTO lo_cx_root.
                lv_err_string = lo_cx_root->get_text( ).
                CLEAR gs_bal_msg.
                gs_bal_msg-msgty = 'E'.
                gs_bal_msg-msgid = 'ZDTE_0001'.
                gs_bal_msg-msgno = '012'.
                gs_bal_msg-msgv1 = lv_err_string.
                go_log->add_msg( i_s_msg = gs_bal_msg ).
                go_log->save( ).
                EXIT.
              CATCH cx_sy_file_open INTO lo_cx_root.
                lv_err_string = lo_cx_root->get_text( ).
                CLEAR gs_bal_msg.
                gs_bal_msg-msgty = 'E'.
                gs_bal_msg-msgid = 'ZDTE_0001'.
                gs_bal_msg-msgno = '012'.
                gs_bal_msg-msgv1 = lv_err_string.
                go_log->add_msg( i_s_msg = gs_bal_msg ).
                go_log->save( ).
                EXIT.
            ENDTRY.

          ENDLOOP."Archivos
        ENDLOOP. " LOOP AT lt_fi0088 INTO ls_fi0088.

      WHEN OTHERS.    " local pc
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
**//.. Buscar RUT Sociedad
          READ TABLE lt_t001z INTO ls_t001z INDEX 1.
          IF sy-subrc NE 0.
            "---> Logs de interfaz.
            READ TABLE gt_bukrs INTO ls_bukrs INDEX 1.
            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'E'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '080'.
            gs_bal_msg-msgv1 = ls_bukrs-low.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
            EXIT.
          ELSE.
            lv_rutempresa = ls_t001z-paval.
            REPLACE ALL OCCURRENCES OF '.' IN  lv_rutempresa WITH ''.
            CONDENSE lv_rutempresa NO-GAPS.
          ENDIF.

**//.. Verificar nombre fichero con Rut Sociedad
          CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
            EXPORTING
              full_name     = lv_filename
            IMPORTING
              stripped_name = lv_fname
*             FILE_PATH     =
            EXCEPTIONS
              x_error       = 1
              OTHERS        = 2.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.

          lv_leng = strlen( lv_fname ).
          IF lv_leng GT 10.
            IF lv_fname(10) NE lv_rutempresa.
              "---> Logs de interfaz.
              CLEAR gs_bal_msg.
              gs_bal_msg-msgty = 'E'.
              gs_bal_msg-msgid = 'ZDTE_0001'.
              gs_bal_msg-msgno = '081'.
              gs_bal_msg-msgv1 = ls_t001z-bukrs.
              go_log->add_msg( i_s_msg = gs_bal_msg ).
              go_log->save( ).
              EXIT.
            ENDIF.
          ELSE.
            "---> Logs de interfaz.
            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'E'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '081'.
            gs_bal_msg-msgv1 = ls_t001z-bukrs.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
            EXIT.
          ENDIF.

          me->go_file->read_file( IMPORTING
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

          READ TABLE gt_bukrs INTO ls_bukrs INDEX 1.
          me->mapping_cuadratura( EXPORTING
                                    t_data = lt_tabla
                                    p_bukrs = ls_bukrs-low ).

          lv_error = me->check_cuadratura( EXPORTING
                                             p_bukrs = ls_bukrs-low
                                             p_fname = lv_fname ).

          CHECK lv_error NE abap_true.

          me->save_cuadratura( EXPORTING p_bukrs = ls_bukrs-low ).
        ENDIF.

    ENDCASE.
  ENDMETHOD.


  METHOD import_cuadraturanw.
    DATA: ls_opsystem       TYPE opsystem,
          lv_sep            TYPE char01,
          lv_existe         TYPE abap_bool,
          lv_dirname        TYPE pfeflnamel,
          lv_filename       TYPE string,
          lv_fname          TYPE string,
          lv_filenam        TYPE pfeflnamel,
          lt_return         TYPE bapireturn_t,
          ls_return         TYPE bapiret2,
          lt_file           TYPE STANDARD TABLE OF salfldir,
          lt_dir_list       TYPE STANDARD TABLE OF eps2fili,
          ls_dir_list       TYPE eps2fili,
          lt_tabla          TYPE TABLE OF string,
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
          lo_err            TYPE REF TO cx_sy_file_open_mode,
          lo_cx_root        TYPE REF TO cx_root,
          ls_bukrs          LIKE LINE OF gt_bukrs,
          lv_rutempresa(10) TYPE c,
          lv_error          TYPE abap_bool,
          lv_leng           TYPE i,
          p_tipodoc         TYPE ztipodte.

**//..
    SELECT SINGLE * INTO ls_opsystem
    FROM  opsystem
    WHERE opsys EQ sy-opsys.
    IF ls_opsystem-filesys EQ 'UNIX'.
      lv_sep = '/'.
    ELSE.
      lv_sep = '\'.
    ENDIF.

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

    "---> Leer RUT Sociedad
    SELECT * INTO TABLE lt_t001z
    FROM t001z
    WHERE bukrs IN me->gt_bukrs
      AND party EQ 'TAXNR'.
**//..
    CASE gt_flagdir.
      WHEN abap_true. " servidor
        "---> Leer tabla de directorios
        SELECT  * INTO TABLE lt_fi0088
        FROM ztfi_0088
        WHERE sysid EQ sy-sysid
          AND bukrs IN me->gt_bukrs.
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

        "Busco por sociedad
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
            EXIT.
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
            EXIT.
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
            gs_bal_msg-msgno = '024'.
            gs_bal_msg-msgv1 =  lv_dirname.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
            EXIT.
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
            EXIT.
          ELSE.
            lv_rutempresa = ls_t001z-paval.
            REPLACE ALL OCCURRENCES OF '.' IN  lv_rutempresa WITH ''.
            CONDENSE lv_rutempresa NO-GAPS.
          ENDIF.

**//.. Revisar archios
          LOOP AT lt_dir_list INTO ls_dir_list
                              WHERE name CS 'csv'
                                 OR name CS 'CSV'.
            CLEAR: lv_file_dir, lv_line, lv_all_lines.
            ADD 1 TO lv_cont.
            CONCATENATE lv_dirname lv_sep ls_dir_list-name INTO lv_file_dir.
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

**//.. Verificar nombre fichero con Rut Sociedad
            lv_leng = strlen( ls_dir_list-name ).
            IF lv_leng GT 10.
              IF NOT ls_dir_list-name CS lv_rutempresa.
                "---> Logs de interfaz.
                CLEAR gs_bal_msg.
                gs_bal_msg-msgty = 'E'.
                gs_bal_msg-msgid = 'ZDTE_0001'.
                gs_bal_msg-msgno = '081'.
                gs_bal_msg-msgv1 = ls_fi0088-bukrs.
                go_log->add_msg( i_s_msg = gs_bal_msg ).
                go_log->save( ).
                EXIT.
              ENDIF.

              IF NOT  ls_dir_list-name CS p_periodo.
                "---> Logs de interfaz.
                CLEAR gs_bal_msg.
                gs_bal_msg-msgty = 'E'.
                gs_bal_msg-msgid = 'ZDTE_0001'.
                gs_bal_msg-msgno = '086'.
                gs_bal_msg-msgv1 = ls_fi0088-bukrs.
                go_log->add_msg( i_s_msg = gs_bal_msg ).
                go_log->save( ).
                EXIT.
              ENDIF.
              IF NOT  ls_dir_list-name CS '_33.'.
                IF   NOT  ls_dir_list-name CS '_34.'.
                  IF   NOT  ls_dir_list-name CS '_61.'.
                    IF   NOT  ls_dir_list-name CS '_56.'.
                      IF   NOT  ls_dir_list-name CS '_46.'.
                        CLEAR gs_bal_msg.
                        gs_bal_msg-msgty = 'E'.
                        gs_bal_msg-msgid = 'ZDTE_0001'.
                        gs_bal_msg-msgno = '085'.
                        gs_bal_msg-msgv1 = ls_fi0088-bukrs.
                        go_log->add_msg( i_s_msg = gs_bal_msg ).
                        go_log->save( ).
                        EXIT.
                      ELSE.
                        p_tipodoc = '46'.
                      ENDIF.
                    ELSE.
                      p_tipodoc = '56'.
                    ENDIF.
                  ELSE.
                    p_tipodoc = '61'.
                  ENDIF.
                ELSE.
                  p_tipodoc = '34'.
                ENDIF.
              ELSE.
                p_tipodoc = '33'.
              ENDIF.
            ELSE.
              "---> Logs de interfaz.
              CLEAR gs_bal_msg.
              gs_bal_msg-msgty = 'E'.
              gs_bal_msg-msgid = 'ZDTE_0001'.
              gs_bal_msg-msgno = '081'.
              gs_bal_msg-msgv1 = ls_fi0088-bukrs.
              go_log->add_msg( i_s_msg = gs_bal_msg ).
              go_log->save( ).
              EXIT.
            ENDIF.

**//..
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
              EXIT.
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
                    CLEAR: lv_line.
                    CONCATENATE lv_all_lines lv_line INTO lv_all_lines.
                  ENDDO.

                  CLEAR: lv_pfeflname.
                  lv_pfeflname = ls_dir_list-name.

                  me->mapping_cuadraturanw( EXPORTING
                                  t_data = lt_tabla
                                  p_bukrs = ls_fi0088-bukrs
                                  p_tipodoc = p_tipodoc
                                  p_periodo = p_periodo ).
                  .
                  CLEAR: lv_fname.
                  lv_fname = ls_dir_list-name.
                  lv_error = me->check_cuadratura( EXPORTING
                                                     p_bukrs = ls_fi0088-bukrs
                                                     p_fname = lv_fname ).

                  CHECK lv_error NE abap_true.

                  me->save_cuadratura( EXPORTING p_bukrs = ls_fi0088-bukrs ).

                  "Agregar Log
                  CLEAR gs_bal_msg.
                  gs_bal_msg-msgty = 'S'.
                  gs_bal_msg-msgid = 'ZDTE_0001'.
                  gs_bal_msg-msgno = '025'.
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
                  EXIT.
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
                  EXIT.
              ENDTRY.
            ENDIF.
**//..
            TRY.
                DELETE DATASET lv_file_dir.
                IF sy-subrc <> 0.
                  CLEAR gs_bal_msg.
                  gs_bal_msg-msgty = 'E'.
                  gs_bal_msg-msgid = 'ZDTE_0001'.
                  gs_bal_msg-msgno = '012'.
                  gs_bal_msg-msgv1 = lv_err_string.
                  go_log->add_msg( i_s_msg = gs_bal_msg ).
                  go_log->save( ).
                  EXIT.
                ENDIF.
              CATCH cx_sy_file_authority INTO lo_cx_root.
                lv_err_string = lo_cx_root->get_text( ).
                CLEAR gs_bal_msg.
                gs_bal_msg-msgty = 'E'.
                gs_bal_msg-msgid = 'ZDTE_0001'.
                gs_bal_msg-msgno = '012'.
                gs_bal_msg-msgv1 = lv_err_string.
                go_log->add_msg( i_s_msg = gs_bal_msg ).
                go_log->save( ).
                EXIT.
              CATCH cx_sy_file_open INTO lo_cx_root.
                lv_err_string = lo_cx_root->get_text( ).
                CLEAR gs_bal_msg.
                gs_bal_msg-msgty = 'E'.
                gs_bal_msg-msgid = 'ZDTE_0001'.
                gs_bal_msg-msgno = '012'.
                gs_bal_msg-msgv1 = lv_err_string.
                go_log->add_msg( i_s_msg = gs_bal_msg ).
                go_log->save( ).
                EXIT.
            ENDTRY.

          ENDLOOP."Archivos
        ENDLOOP. " LOOP AT lt_fi0088 INTO ls_fi0088.

      WHEN OTHERS.    " local pc
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
**//.. Buscar RUT Sociedad
          READ TABLE lt_t001z INTO ls_t001z INDEX 1.
          IF sy-subrc NE 0.
            "---> Logs de interfaz.
            READ TABLE gt_bukrs INTO ls_bukrs INDEX 1.
            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'E'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '080'.
            gs_bal_msg-msgv1 = ls_bukrs-low.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
            EXIT.
          ELSE.
            lv_rutempresa = ls_t001z-paval.
            REPLACE ALL OCCURRENCES OF '.' IN  lv_rutempresa WITH ''.
            CONDENSE lv_rutempresa NO-GAPS.
          ENDIF.

**//.. Verificar nombre fichero con Rut Sociedad
          CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
            EXPORTING
              full_name     = lv_filename
            IMPORTING
              stripped_name = lv_fname
*             FILE_PATH     =
            EXCEPTIONS
              x_error       = 1
              OTHERS        = 2.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.

          lv_leng = strlen( lv_fname ).
          IF lv_leng GT 30.
            IF NOT  lv_fname CS lv_rutempresa.
              "---> Logs de interfaz.
              CLEAR gs_bal_msg.
              gs_bal_msg-msgty = 'E'.
              gs_bal_msg-msgid = 'ZDTE_0001'.
              gs_bal_msg-msgno = '081'.
              gs_bal_msg-msgv1 = ls_t001z-bukrs.
              go_log->add_msg( i_s_msg = gs_bal_msg ).
              go_log->save( ).
              EXIT.
            ENDIF.
            IF NOT lv_fname CS p_periodo.
              "---> Logs de interfaz.
              CLEAR gs_bal_msg.
              gs_bal_msg-msgty = 'E'.
              gs_bal_msg-msgid = 'ZDTE_0001'.
              gs_bal_msg-msgno = '086'.
              gs_bal_msg-msgv1 = ls_t001z-bukrs.
              go_log->add_msg( i_s_msg = gs_bal_msg ).
              go_log->save( ).
              EXIT.
            ENDIF.
            IF NOT  lv_fname CS '_33.'.
              IF   NOT  lv_fname  CS '_34.'.
                IF   NOT  lv_fname  CS '_61.'.
                  IF   NOT  lv_fname  CS '_56.'.
                    IF   NOT  lv_fname CS '_46.'.
                      CLEAR gs_bal_msg.
                      gs_bal_msg-msgty = 'E'.
                      gs_bal_msg-msgid = 'ZDTE_0001'.
                      gs_bal_msg-msgno = '085'.
                      gs_bal_msg-msgv1 = ls_t001z-bukrs.
                      go_log->add_msg( i_s_msg = gs_bal_msg ).
                      go_log->save( ).
                      EXIT.
                    ELSE.
                      p_tipodoc = '46'.
                    ENDIF.
                  ELSE.
                    p_tipodoc = '56'.
                  ENDIF.
                ELSE.
                  p_tipodoc = '61'.
                ENDIF.
              ELSE.
                p_tipodoc = '34'.
              ENDIF.
            ELSE.
              p_tipodoc = '33'.
            ENDIF.
          ELSE.
            "---> Logs de interfaz.
            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'E'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '081'.
            gs_bal_msg-msgv1 = ls_t001z-bukrs.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
            EXIT.
          ENDIF.

          me->go_file->read_file( IMPORTING
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

          READ TABLE gt_bukrs INTO ls_bukrs INDEX 1.
          me->mapping_cuadraturanw( EXPORTING
                          t_data = lt_tabla
                          p_bukrs = ls_bukrs-low
                          p_tipodoc = p_tipodoc
                          p_periodo = p_periodo ).

          lv_error = me->check_cuadratura( EXPORTING
                                             p_bukrs = ls_bukrs-low
                                             p_fname = lv_fname ).

          CHECK lv_error NE abap_true.

          me->save_cuadratura( EXPORTING p_bukrs = ls_bukrs-low ).

        ENDIF.

    ENDCASE.


  ENDMETHOD.


  METHOD import_file.
    DATA ls_dte_xml TYPE ztfi_0088.
    DATA lv_file TYPE pfeflnamel.
    DATA lv_file2 TYPE localfile.
    DATA lv_file3 TYPE pfeflnamel.
    DATA lv_filenam TYPE pfeflnamel.
    DATA lv_subrc TYPE sysubrc.
    DATA lv_err_string TYPE string.
    DATA lv_line TYPE string.
    DATA all_lines TYPE string.
    DATA lo_err TYPE REF TO cx_sy_file_open_mode.
    DATA lo_cx_root TYPE REF TO cx_root.
    DATA lt_file TYPE STANDARD TABLE OF salfldir.
    DATA lt_file2 TYPE STANDARD TABLE OF eps2fili.
    DATA ls_file TYPE salfldir.
    DATA ls_file2 TYPE  eps2fili.
    DATA cont TYPE i.
    DATA: iso TYPE cpcodepage.
*OT
    DATA: lv_phys_filename TYPE sapb-sappfad.
    DATA: lv_accion TYPE c.
    DATA: ls_dte_xml_acc TYPE ztfi_0088.
    DATA: ti_dte_xml TYPE TABLE OF ztfi_0088.
    DATA: lv_file_acc TYPE user_dir-dirname.
    DATA: lv_file_err TYPE user_dir-dirname.
    DATA: lv_file_d TYPE sapb-sappfad.
    DATA: lv_file_o TYPE sapb-sappfad.
    DATA: ls_opsystem TYPE opsystem.
    DATA: lv_sep       TYPE char01,
          lv_pfeflname TYPE pfeflnamel,
          ls_bukrs     LIKE LINE OF gt_bukrs.
*OT
    DATA: BEGIN OF isearchpoints,
            dirname(75) TYPE c,            " name of directory.
            aliass(75)  TYPE c,            " alias for directory.
            svrname(75) TYPE c,            " svr where directory is availabl
            sp_name(75) TYPE c,            " name of entry. (may end with *)
            sp_cs(10)   TYPE c,            " ContainsString pattern for name
          END OF isearchpoints.
**//..
    SELECT SINGLE * INTO ls_opsystem
    FROM  opsystem
    WHERE opsys EQ sy-opsys.
    IF ls_opsystem-filesys EQ 'UNIX'.
      lv_sep = '/'.
    ELSE.
      lv_sep = '\'.
    ENDIF.

**//..
    IF gt_flagdir = space.
      CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
        EXPORTING
          static        = 'X'
        CHANGING
          file_name     = lv_file2
        EXCEPTIONS
          mask_too_long = 1
          OTHERS        = 2.

      CALL METHOD go_xml->import_from_file
        EXPORTING
          filename = lv_file2
        RECEIVING
          retcode  = lv_subrc.

      IF lv_subrc <> 0.

        IF lv_subrc = 2.
          "'Arch. No existe o Path muy Largo'
          MESSAGE e051(zdte_0001) WITH lv_file2.
        ELSE.
          "Error al leer el archivo'
          MESSAGE e052(zdte_0001) WITH lv_file2.
        ENDIF.
      ELSE.

        me->variante_interfaz(  ).
        me->parse_xml_to_tab_v2( ).
        lv_filenam = lv_file2.
        me->mapping_to_struc( filename = lv_filenam ).
        me->load_sap_data( ).

        READ TABLE gt_bukrs INTO ls_bukrs INDEX 1.
        me->save_pdf_form( EXPORTING
                             i_bukrs = ls_bukrs-low ).
      ENDIF.

    ELSE.
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

      "---> Leer tabla de directorios
      SELECT  * INTO TABLE ti_dte_xml FROM ztfi_0088
      WHERE sysid EQ sy-sysid
        AND bukrs IN gt_bukrs.
      IF sy-subrc NE 0.
        "---> Logs de interfaz.
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '012'.
        gs_bal_msg-msgv1 = lv_err_string.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
        EXIT.
      ENDIF.

      LOOP AT  ti_dte_xml INTO ls_dte_xml. "Busco por sociedad

        "---> Busco variante de integracion
        me->variante_interfaz( i_bukrs = ls_dte_xml-bukrs  ).

        "Customized Table ZTFI_0088 get path
        SELECT SINGLE * FROM user_dir INTO isearchpoints
          WHERE aliass = ls_dte_xml-zdir_recxml.
        IF sy-subrc EQ 0.
          lv_file = isearchpoints-dirname.
        ELSE.
          "---> Logs de interfaz.
          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'E'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '026'.
          gs_bal_msg-msgv1 = ls_dte_xml-zdir_recxml.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
          CONTINUE.
        ENDIF.

        "Directorio de procesados
        SELECT SINGLE * FROM user_dir INTO isearchpoints
           WHERE aliass = ls_dte_xml-zdir_prcxml.
        IF sy-subrc EQ 0.
          lv_file_acc =  isearchpoints-dirname.
        ELSE.
          "---> Logs de interfaz.
          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'E'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '027'.
          gs_bal_msg-msgv1 = ls_dte_xml-zdir_prcxml.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
          CONTINUE.
        ENDIF.

        "Directorio de error
        SELECT SINGLE * FROM user_dir INTO isearchpoints
           WHERE aliass = ls_dte_xml-zdir_errxml.
        IF sy-subrc EQ 0.
          lv_file_err =  isearchpoints-dirname.
        ELSE.
          "---> Logs de interfaz.
          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'E'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '027'.
          gs_bal_msg-msgv1 = ls_dte_xml-zdir_errxml.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
          CONTINUE.
        ENDIF.

        FREE: lt_file.

        CLEAR lt_file2.  REFRESH lt_file2.
        CALL FUNCTION 'EPS2_GET_DIRECTORY_LISTING'
          EXPORTING
            iv_dir_name            = lv_file
          TABLES
            dir_list               = lt_file2
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
          gs_bal_msg-msgno = '024'.
          gs_bal_msg-msgv1 =  lv_file.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
          CONTINUE.
        ENDIF.
        cont = 0.
        CLEAR lv_accion.
        CLEAR ls_dte_xml_acc.
*      CLEAR lv_file_acc.
*      LOOP AT lt_file INTO ls_file
        LOOP AT lt_file2 INTO ls_file2
                       WHERE name CS'xml'
                          OR name CS 'XML'.


          FREE: lv_file3, lv_line, all_lines.
          ADD 1 TO cont.
          CONCATENATE lv_file lv_sep ls_file2-name INTO lv_file3.
          IF ls_file2-size = 0.

            "---> Logs de interfaz.
            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'E'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '039'.
            gs_bal_msg-msgv1 = ls_file2-name.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
            CONTINUE.
          ENDIF.
**//..
          TRY .
              CALL FUNCTION 'SCP_CODEPAGE_BY_EXTERNAL_NAME'
                EXPORTING
                  external_name = 'ISO-8859-1'
                IMPORTING
                  sap_codepage  = iso
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
                EXIT.
              ELSE.
                CLEAR lv_err_string.
                OPEN DATASET lv_file3 FOR INPUT IN LEGACY TEXT MODE CODE PAGE iso
                MESSAGE lv_err_string.
                IF sy-subrc NE 0.
                  CLEAR gs_bal_msg.
                  gs_bal_msg-msgty = 'E'.
                  gs_bal_msg-msgid = 'ZDTE_0001'.
                  gs_bal_msg-msgno = '049'.
                  gs_bal_msg-msgv1 = lv_err_string.
                  gs_bal_msg-msgv2 = ls_file2-name.
                  go_log->add_msg( i_s_msg = gs_bal_msg ).
                  go_log->save( ).
                  CONTINUE.
                ENDIF.
                DO.
                  READ DATASET lv_file3 INTO lv_line.
                  IF sy-subrc <> 0.
                    EXIT.
                  ENDIF.
                  CONCATENATE all_lines lv_line INTO all_lines.
                ENDDO.

                CLEAR: lv_pfeflname.
*              lv_pfeflname = ls_file-name.
                lv_pfeflname = ls_file2-name.
                CLOSE DATASET lv_file3.
                go_xml->parse_string( stream = all_lines ).
                me->parse_xml_to_tab_v2( filename = ls_file2-name )."lv_filenam ).
                IF gt_xml IS INITIAL. "Si hay error del arhivo
                  CLEAR lv_file_o. CLEAR lv_phys_filename. CLEAR lv_file_d.
                  lv_phys_filename = ls_file2-name.

                  CONCATENATE lv_file lv_sep     INTO lv_file_o.
                  CONCATENATE lv_file_err lv_sep INTO lv_file_d.

                  me->mov_archivoserror( file_o =  lv_file_o
                                        file_d = lv_file_d
                                        nomb_archivo =  ls_file2-name ).


                  CONTINUE.
                ENDIF.
                me->mapping_to_struc( filename = lv_pfeflname ).

                "Se crea log dentro del load_sap_data
                me->load_sap_data( ).


                me->save_pdf_form( EXPORTING
                                     i_bukrs = ls_dte_xml-bukrs ).

                "Agregar Log
                CLEAR gs_bal_msg.
                gs_bal_msg-msgty = 'S'.
                gs_bal_msg-msgid = 'ZDTE_0001'.
                gs_bal_msg-msgno = '025'.
*              gs_bal_msg-msgv1 = ls_file-name.
                gs_bal_msg-msgv1 = ls_file2-name.
                go_log->add_msg( i_s_msg = gs_bal_msg ).
                "Al finalizar cada registro guardo el log.
                go_log->save( ).

              ENDIF.

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
          ENDTRY.
**//..
          CLEAR lv_file_o. CLEAR lv_phys_filename. CLEAR lv_file_d.
*        lv_phys_filename = ls_file-name.
          lv_phys_filename = ls_file2-name.
          CONCATENATE lv_file lv_sep     INTO lv_file_o.
          CONCATENATE lv_file_acc lv_sep INTO lv_file_d.

          me->move_filexml( file_o =  lv_file_o
                            file_d = lv_file_d
                            nomb_archivo =  ls_file2-name ).


        ENDLOOP."Archivos

      ENDLOOP."Sociedades

    ENDIF.

  ENDMETHOD.


  METHOD import_lc.
    DATA: ls_opsystem       TYPE opsystem,
          lv_sep            TYPE char01,
          lv_existe         TYPE abap_bool,
          lv_dirname        TYPE pfeflnamel,
          lv_filename       TYPE string,
          lv_fname          TYPE string,
          lv_filenam        TYPE pfeflnamel,
          lt_return         TYPE bapireturn_t,
          ls_return         TYPE bapiret2,
          lt_file           TYPE STANDARD TABLE OF salfldir,
          lt_dir_list       TYPE STANDARD TABLE OF eps2fili,
          ls_dir_list       TYPE eps2fili,
          lt_tabla          TYPE TABLE OF string,
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
          lo_err            TYPE REF TO cx_sy_file_open_mode,
          lo_cx_root        TYPE REF TO cx_root,
          ls_bukrs          LIKE LINE OF gt_bukrs,
          lv_rutempresa(10) TYPE c,
          lv_error          TYPE abap_bool,
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

    "---> Leer RUT Sociedad
    SELECT * INTO TABLE lt_t001z
    FROM t001z
    WHERE bukrs IN me->gt_bukrs
      AND party EQ 'TAXNR'.
**//..
    CASE gt_flagdir.
      WHEN abap_true. " servidor
        "---> Leer tabla de directorios
        SELECT  * INTO TABLE lt_fi0088
        FROM ztfi_0088
        WHERE sysid EQ sy-sysid
          AND bukrs IN me->gt_bukrs.
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

        "Busco por sociedad
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
            EXIT.
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
            EXIT.
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
            gs_bal_msg-msgno = '024'.
            gs_bal_msg-msgv1 =  lv_dirname.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
            EXIT.
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
            EXIT.
          ELSE.
            lv_rutempresa = ls_t001z-paval.
            REPLACE ALL OCCURRENCES OF '.' IN  lv_rutempresa WITH ''.
            CONDENSE lv_rutempresa NO-GAPS.
          ENDIF.

**//.. Revisar archios
          LOOP AT lt_dir_list INTO ls_dir_list
                              WHERE name CS 'csv'
                                 OR name CS 'CSV'.
            CLEAR: lv_file_dir, lv_line, lv_all_lines.
            ADD 1 TO lv_cont.
            CONCATENATE lv_dirname lv_sep ls_dir_list-name INTO lv_file_dir.
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

**//.. Verificar nombre fichero con Rut Sociedad
            lv_leng = strlen( ls_dir_list-name ).
            IF lv_leng GT 10.
              IF NOT ls_dir_list-name CS lv_rutempresa.
                "---> Logs de interfaz.
                CLEAR gs_bal_msg.
                gs_bal_msg-msgty = 'E'.
                gs_bal_msg-msgid = 'ZDTE_0001'.
                gs_bal_msg-msgno = '081'.
                gs_bal_msg-msgv1 = ls_fi0088-bukrs.
                go_log->add_msg( i_s_msg = gs_bal_msg ).
                go_log->save( ).
                EXIT.
              ENDIF.

              IF NOT  ls_dir_list-name CS p_periodo.
                "---> Logs de interfaz.
                CLEAR gs_bal_msg.
                gs_bal_msg-msgty = 'E'.
                gs_bal_msg-msgid = 'ZDTE_0001'.
                gs_bal_msg-msgno = '085'.
                gs_bal_msg-msgv1 = ls_fi0088-bukrs.
                go_log->add_msg( i_s_msg = gs_bal_msg ).
                go_log->save( ).
                EXIT.
              ENDIF.
            ELSE.
              "---> Logs de interfaz.
              CLEAR gs_bal_msg.
              gs_bal_msg-msgty = 'E'.
              gs_bal_msg-msgid = 'ZDTE_0001'.
              gs_bal_msg-msgno = '081'.
              gs_bal_msg-msgv1 = ls_fi0088-bukrs.
              go_log->add_msg( i_s_msg = gs_bal_msg ).
              go_log->save( ).
              EXIT.
            ENDIF.

**//..
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
              EXIT.
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
                    CLEAR: lv_line.
                    CONCATENATE lv_all_lines lv_line INTO lv_all_lines.
                  ENDDO.

                  CLEAR: lv_pfeflname.
                  lv_pfeflname = ls_dir_list-name.

                  me->mapping_lc( EXPORTING
                                  t_data = lt_tabla
                                  p_bukrs = ls_fi0088-bukrs
                                  p_periodo = p_periodo ).
                  .
                  CLEAR: lv_fname.
                  lv_fname = ls_dir_list-name.
                  lv_error = me->check_cuadratura( EXPORTING
                                                     p_bukrs = ls_fi0088-bukrs
                                                     p_fname = lv_fname ).

                  CHECK lv_error NE abap_true.

                  me->save_lc( EXPORTING p_bukrs = ls_fi0088-bukrs ).

                  "Agregar Log
                  CLEAR gs_bal_msg.
                  gs_bal_msg-msgty = 'S'.
                  gs_bal_msg-msgid = 'ZDTE_0001'.
                  gs_bal_msg-msgno = '025'.
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
                  EXIT.
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
                  EXIT.
              ENDTRY.
            ENDIF.
**//..
            TRY.
                DELETE DATASET lv_file_dir.
                IF sy-subrc <> 0.
                  CLEAR gs_bal_msg.
                  gs_bal_msg-msgty = 'E'.
                  gs_bal_msg-msgid = 'ZDTE_0001'.
                  gs_bal_msg-msgno = '012'.
                  gs_bal_msg-msgv1 = lv_err_string.
                  go_log->add_msg( i_s_msg = gs_bal_msg ).
                  go_log->save( ).
                  EXIT.
                ENDIF.
              CATCH cx_sy_file_authority INTO lo_cx_root.
                lv_err_string = lo_cx_root->get_text( ).
                CLEAR gs_bal_msg.
                gs_bal_msg-msgty = 'E'.
                gs_bal_msg-msgid = 'ZDTE_0001'.
                gs_bal_msg-msgno = '012'.
                gs_bal_msg-msgv1 = lv_err_string.
                go_log->add_msg( i_s_msg = gs_bal_msg ).
                go_log->save( ).
                EXIT.
              CATCH cx_sy_file_open INTO lo_cx_root.
                lv_err_string = lo_cx_root->get_text( ).
                CLEAR gs_bal_msg.
                gs_bal_msg-msgty = 'E'.
                gs_bal_msg-msgid = 'ZDTE_0001'.
                gs_bal_msg-msgno = '012'.
                gs_bal_msg-msgv1 = lv_err_string.
                go_log->add_msg( i_s_msg = gs_bal_msg ).
                go_log->save( ).
                EXIT.
            ENDTRY.

          ENDLOOP."Archivos
        ENDLOOP. " LOOP AT lt_fi0088 INTO ls_fi0088.

      WHEN OTHERS.    " local pc
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
**//.. Buscar RUT Sociedad
          READ TABLE lt_t001z INTO ls_t001z INDEX 1.
          IF sy-subrc NE 0.
            "---> Logs de interfaz.
            READ TABLE gt_bukrs INTO ls_bukrs INDEX 1.
            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'E'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '080'.
            gs_bal_msg-msgv1 = ls_bukrs-low.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
            EXIT.
          ELSE.
            lv_rutempresa = ls_t001z-paval.
            REPLACE ALL OCCURRENCES OF '.' IN  lv_rutempresa WITH ''.
            CONDENSE lv_rutempresa NO-GAPS.
          ENDIF.

**//.. Verificar nombre fichero con Rut Sociedad
          CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
            EXPORTING
              full_name     = lv_filename
            IMPORTING
              stripped_name = lv_fname
*             FILE_PATH     =
            EXCEPTIONS
              x_error       = 1
              OTHERS        = 2.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.

          lv_leng = strlen( lv_fname ).
          IF lv_leng GT 10.
            IF NOT  lv_fname CS lv_rutempresa.
              "---> Logs de interfaz.
              CLEAR gs_bal_msg.
              gs_bal_msg-msgty = 'E'.
              gs_bal_msg-msgid = 'ZDTE_0001'.
              gs_bal_msg-msgno = '081'.
              gs_bal_msg-msgv1 = ls_t001z-bukrs.
              go_log->add_msg( i_s_msg = gs_bal_msg ).
              go_log->save( ).
              EXIT.
            ENDIF.
            IF NOT lv_fname CS p_periodo.
              "---> Logs de interfaz.
              CLEAR gs_bal_msg.
              gs_bal_msg-msgty = 'E'.
              gs_bal_msg-msgid = 'ZDTE_0001'.
              gs_bal_msg-msgno = '085'.
              gs_bal_msg-msgv1 = ls_t001z-bukrs.
              go_log->add_msg( i_s_msg = gs_bal_msg ).
              go_log->save( ).
              EXIT.
            ENDIF.
          ELSE.
            "---> Logs de interfaz.
            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'E'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '081'.
            gs_bal_msg-msgv1 = ls_t001z-bukrs.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
            EXIT.
          ENDIF.

          me->go_file->read_file( IMPORTING
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

          READ TABLE gt_bukrs INTO ls_bukrs INDEX 1.
          me->mapping_lc( EXPORTING
                          t_data = lt_tabla
                          p_bukrs = ls_bukrs-low
                          p_periodo = p_periodo ).

          lv_error = me->check_cuadratura( EXPORTING
                                             p_bukrs = ls_bukrs-low
                                             p_fname = lv_fname ).

          CHECK lv_error NE abap_true.

          me->save_lc( EXPORTING p_bukrs = ls_bukrs-low ).

        ENDIF.

    ENDCASE.
  ENDMETHOD.


  METHOD load_pdf_form.
    DATA: ls_recfact   TYPE zst_dte_encabezado,
          ls_recfact2  TYPE zst_dte_encabezado2,
          ls_rfrecfact TYPE zst_dte_refer,
          lt_rfrecfact TYPE ztt_dte_refer.

*OT: Datos para formulario
    DATA: lv_fname      TYPE fpname.
    DATA: lv_funcname   TYPE funcname.
    DATA: ls_formoutput TYPE fpformoutput.
    DATA: ls_docparams  TYPE sfpdocparams.
    DATA: itab_line     TYPE tsftext.
    DATA: result        TYPE sfpjoboutput.
    DATA: ls_itab       TYPE zdte.
    DATA: ls_det        TYPE Z1MM_DTE_DETALLE.
    DATA: ls_detid TYPE ty_dt_recfact .
    DATA: itab          TYPE STANDARD TABLE OF zdte.
    DATA: lt_domval TYPE TABLE OF dd07v,
          ls_domval TYPE dd07v.

**//.. Determinar textos de Forma de pago
    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname    = 'ZDO_FMAPAGO'
        text       = 'X'
      TABLES
        values_tab = lt_domval.

**//..
    lv_fname             = co_fpname.

    MOVE-CORRESPONDING: gs_recfact  TO ls_recfact,
                        gs_recfact2 TO ls_recfact2.

    CASE ls_recfact-tipodte.
      WHEN '33'.
        ls_recfact-desc_tipodte = TEXT-001.
      WHEN '34'.
        ls_recfact-desc_tipodte = TEXT-002.
      WHEN '56'.
        ls_recfact-desc_tipodte = TEXT-003.
      WHEN '61'.
        ls_recfact-desc_tipodte = TEXT-004.
      WHEN OTHERS.
    ENDCASE.

    " Agregar Desc.Forma de pago
    READ TABLE lt_domval WITH KEY domvalue_l = ls_recfact2-fmapago
                         INTO ls_domval.
    IF sy-subrc EQ 0.
      ls_recfact2-desc_fmapago = ls_domval-ddtext.
    ENDIF.

**//..
    LOOP AT gt_rfrecfact INTO gs_rfrecfact.
      MOVE-CORRESPONDING gs_rfrecfact TO ls_rfrecfact.

      CASE ls_rfrecfact-tpodocref.
        WHEN 'HES'.
          ls_rfrecfact-desc_tpodocref = TEXT-005.
        WHEN '801'.
          ls_rfrecfact-desc_tpodocref = TEXT-006.
        WHEN '802'.
          ls_rfrecfact-desc_tpodocref = TEXT-007.
        WHEN '803'.
          ls_rfrecfact-desc_tpodocref = TEXT-008.
        WHEN '804'.
          ls_rfrecfact-desc_tpodocref = TEXT-009.
        WHEN '805'.
          ls_rfrecfact-desc_tpodocref = TEXT-010.
        WHEN OTHERS.
      ENDCASE.

      APPEND ls_rfrecfact TO lt_rfrecfact.
      CLEAR: ls_rfrecfact.
    ENDLOOP.

    IF lines( gt_dtrecfact[] ) GT 0.
      ls_itab-detalle[] = gt_dtrecfact[].
    ELSE.
      LOOP AT gt_dtrecfact_id  INTO ls_detid.
        MOVE-CORRESPONDING ls_detid TO ls_det.
        APPEND ls_det TO ls_itab-detalle.
        CLEAR ls_det.
      ENDLOOP.
    ENDIF.

    ls_itab-cabecera     = ls_recfact.
    ls_itab-cabecera2    = ls_recfact2.
*    ls_itab-MONTOPAGO
    ls_itab-impretenidos = gt_imrecfact.
    ls_itab-motoimpotrmn = gt_dtrecfact2.
    ls_itab-referencia   = lt_rfrecfact.

*    ls_itab-SUBTOTAL
    ls_itab-nombrearc    = gs_filename.
    ls_itab-ted          = gs_tdrecfact.
    ls_itab-firma        = me->get_firma_ted( EXPORTING
                                                i_dte_ted = gs_tdrecfact ).


**//..
    CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
      EXPORTING
        i_name     = lv_fname
      IMPORTING
        e_funcname = lv_funcname.

    CHECK lv_funcname IS NOT INITIAL.

    "----> prepara el formulario.
    DATA: ls_outputparams TYPE sfpoutputparams.

    ls_outputparams-nodialog   = abap_true.
*   ls_outputparams-preview    = ' '.
*   ls_outputparams-getpdl     = ' '.
    ls_outputparams-getpdf     = abap_true.
    ls_outputparams-dest       = 'LPDF'.
    ls_outputparams-connection = 'ADS'.

**//..
    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = ls_outputparams
      EXCEPTIONS
        cancel          = 1
        usage_error     = 2
        system_error    = 3
        internal_error  = 4
        OTHERS          = 5.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    ls_docparams-langu    = sy-langu.
*   ls_docparams-DYNAMIC  = 'X'.
    ls_docparams-fillable = space.

**//..
    CALL FUNCTION lv_funcname "'/1BCDWB/SM00000011'
      EXPORTING
        /1bcdwb/docparams  = ls_docparams
        itab               = ls_itab
      IMPORTING
        /1bcdwb/formoutput = ls_formoutput
      EXCEPTIONS
        usage_error        = 1
        system_error       = 2
        internal_error     = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      CLEAR gs_bal_msg.
      gs_bal_msg-msgty = 'E'.
      gs_bal_msg-msgid = sy-msgid.
      gs_bal_msg-msgno = sy-msgno.
      gs_bal_msg-msgv1 = sy-msgv1.
      gs_bal_msg-msgv2 = sy-msgv2.
      gs_bal_msg-msgv3 = sy-msgv3.
      gs_bal_msg-msgv4 = sy-msgv4.
      go_log->add_msg( i_s_msg = gs_bal_msg ).
      go_log->save( ).
      EXIT.
    ELSE.
      e_pdf_xstring = ls_formoutput-pdf.
    ENDIF.

**//..
    CALL FUNCTION 'FP_JOB_CLOSE'
      IMPORTING
        e_result       = result
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
      CLEAR gs_bal_msg.
      gs_bal_msg-msgty = 'E'.
      gs_bal_msg-msgid = sy-msgty.
      gs_bal_msg-msgno = sy-msgno.
      gs_bal_msg-msgv1 = sy-msgv1.
      gs_bal_msg-msgv2 = sy-msgv2.
      gs_bal_msg-msgv3 = sy-msgv3.
      gs_bal_msg-msgv4 = sy-msgv4.
      go_log->add_msg( i_s_msg = gs_bal_msg ).
      go_log->save( ).
      EXIT.
    ENDIF.

***//..
*    DATA: lv_string  TYPE string,
*          lv_xstring TYPE xstring,
*          lr_conv    TYPE REF TO cl_abap_conv_in_ce.
*
***//..
*    TRY.
*        CALL METHOD cl_abap_conv_in_ce=>create
*          EXPORTING
*            input = ls_formoutput-pdf
*          RECEIVING
*            conv  = lr_conv.
*      CATCH cx_parameter_invalid_range .
*      CATCH cx_sy_codepage_converter_init .
*    ENDTRY.
*
***//..
*    TRY.
*        CALL METHOD lr_conv->read
*          IMPORTING
*            data = lv_string.
*        e_pdf_string = lv_string.
*      CATCH cx_sy_conversion_codepage .
*      CATCH cx_sy_codepage_converter_init .
*      CATCH cx_parameter_invalid_type .
*      CATCH cx_parameter_invalid_range .
*    ENDTRY.
  ENDMETHOD.


  METHOD load_pdf_form_xml.
    DATA: ls_recfact   TYPE zst_dte_encabezado,
          ls_recfact2  TYPE zst_dte_encabezado2,
          ls_rfrecfact TYPE zst_dte_refer,
          lt_rfrecfact TYPE ztt_dte_refer.

*OT: Datos para formulario
    DATA: lv_fname      TYPE fpname.
    DATA: lv_funcname   TYPE funcname.
    DATA: ls_formoutput TYPE fpformoutput.
    DATA: ls_docparams  TYPE sfpdocparams.
    DATA: itab_line     TYPE tsftext.
    DATA: result        TYPE sfpjoboutput.
    DATA: ls_itab       TYPE zdte.
    DATA: itab          TYPE STANDARD TABLE OF zdte.
    DATA: lt_domval TYPE TABLE OF dd07v,
          ls_domval TYPE dd07v,
          ls_xml    TYPE string,
          lx_xml    TYPE xstring.
    DATA: lv_file2 TYPE localfile,
          lv_subrc TYPE subrc.
    DATA gt_sxml TYPE xstring .
    DATA gt_ssxml TYPE string .
    DATA: ls_output TYPE smum_xmltb,
          lt_output TYPE STANDARD TABLE OF smum_xmltb,
          ls_return TYPE bapiret2,
          lt_return TYPE bapiret2_t.


**//..




    IF local = 'X'.
      CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
        EXPORTING
          static        = 'X'
        CHANGING
          file_name     = lv_file2
        EXCEPTIONS
          mask_too_long = 1
          OTHERS        = 2.

      CALL METHOD go_xml->import_from_file
        EXPORTING
          filename = lv_file2
        RECEIVING
          retcode  = lv_subrc.
    ENDIF.

    go_xml->set_encoding( charset = 'utf-8' ).
    go_xml->render_2_xstring( IMPORTING stream = lx_xml ).
    gt_sxml = lx_xml.
*// Esto se hace porque los XML si vienen de origen con utf-16 generan error
*    go_xml->render_2_string( IMPORTING stream = ls_xml ).
*    gt_ssxml = ls_xml.
*    REPLACE 'encoding="utf-16"' WITH 'encoding="utf-8"' INTO gt_ssxml.
*
*    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
*      EXPORTING
*        text   = gt_ssxml
**       ENCODING = 4110
*      IMPORTING
*        buffer = gt_sxml
*      EXCEPTIONS
*        failed = 1
*        OTHERS = 2.
*//..

*//.. Determina Nombre de Formulacio por TipoDte
    me->nombre_sfp( EXPORTING i_bukrs = i_bukrs
                   IMPORTING nombre = lv_fname ).
    CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
      EXPORTING
        i_name     = lv_fname
      IMPORTING
        e_funcname = lv_funcname.

    CHECK lv_funcname IS NOT INITIAL.

    "----> prepara el formulario.
    DATA: ls_outputparams TYPE sfpoutputparams.

    ls_outputparams-nodialog   = abap_true.
*   ls_outputparams-preview    = ' '.
*   ls_outputparams-getpdl     = ' '.
    ls_outputparams-getpdf     = abap_true.
    ls_outputparams-dest       = 'LPDF'.
    ls_outputparams-connection = 'ADS'.

**//..
    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = ls_outputparams
      EXCEPTIONS
        cancel          = 1
        usage_error     = 2
        system_error    = 3
        internal_error  = 4
        OTHERS          = 5.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    ls_docparams-langu    = sy-langu.
*   ls_docparams-DYNAMIC  = 'X'.
    ls_docparams-fillable = space.

**//..
    CALL FUNCTION lv_funcname "'/1BCDWB/SM00000011'
      EXPORTING
        /1bcdwb/docparams  = ls_docparams
        /1bcdwb/docxml     = gt_sxml
      IMPORTING
        /1bcdwb/formoutput = ls_formoutput
      EXCEPTIONS
        usage_error        = 1
        system_error       = 2
        internal_error     = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      CLEAR gs_bal_msg.
      gs_bal_msg-msgty = 'E'.
      gs_bal_msg-msgid = sy-msgid.
      gs_bal_msg-msgno = sy-msgno.
      gs_bal_msg-msgv1 = sy-msgv1.
      gs_bal_msg-msgv2 = sy-msgv2.
      gs_bal_msg-msgv3 = sy-msgv3.
      gs_bal_msg-msgv4 = sy-msgv4.
      go_log->add_msg( i_s_msg = gs_bal_msg ).
      go_log->save( ).
      EXIT.
    ELSE.
      e_pdf_xstring = ls_formoutput-pdf.
    ENDIF.

**//..
    CALL FUNCTION 'FP_JOB_CLOSE'
      IMPORTING
        e_result       = result
      EXCEPTIONS
        usage_error    = 1
        system_error   = 2
        internal_error = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
      CLEAR gs_bal_msg.
      gs_bal_msg-msgty = 'E'.
      gs_bal_msg-msgid = sy-msgty.
      gs_bal_msg-msgno = sy-msgno.
      gs_bal_msg-msgv1 = sy-msgv1.
      gs_bal_msg-msgv2 = sy-msgv2.
      gs_bal_msg-msgv3 = sy-msgv3.
      gs_bal_msg-msgv4 = sy-msgv4.
      go_log->add_msg( i_s_msg = gs_bal_msg ).
      go_log->save( ).
      EXIT.
    ENDIF.


  ENDMETHOD.


  METHOD load_sap_data.


    me->load_sap_data_1( ).


  ENDMETHOD.


  METHOD load_sap_datah.
    FIELD-SYMBOLS: <tab>       TYPE STANDARD TABLE,
                   <datos>     TYPE tt_edidd,
                   <wa>        TYPE any,
                   <wa1>       TYPE any,
                   <tcampo>    TYPE table,
                   <ls_struct> TYPE any,
                   <ld_fld>    TYPE any.
    DATA: ldo_data     TYPE REF TO data.
    DATA: it_comm   TYPE TABLE OF  edidc,
          ls_datos  TYPE edidd,
          it_datos  TYPE TABLE OF  edidd,
          lv_tabix  TYPE sy-tabix,
          ls_tabixp TYPE sy-tabix.

    DATA: lt_parametro TYPE STANDARD TABLE OF ztfi_0078c,
          wa_parametro TYPE ztfi_0078c.

    ASSIGN (parametro-atributo)   TO   <tab>.
    ASSIGN (parametro-waatributo) TO   <wa>.
    ASSIGN gt_datos TO <datos>.

    " Create dynamic structure
    CREATE DATA ldo_data TYPE (parametro-estructura).
    ASSIGN ldo_data->* TO <ls_struct>.

    CLEAR  <ls_struct>.

    "---> busco Hijos del registo actual
    SELECT * FROM ztfi_0078c INTO  TABLE  lt_parametro
      WHERE vatint EQ gs_varint "'CL'
      AND nodop EQ parametro-nodo.

    IF parametro-clase = 'C'.
      CLEAR ls_datos.
      ls_datos-segnam = parametro-estructura. "co_z1mm_dte_XXXXXX.
      ls_datos-hlevel = parametro-nivelj.
      MOVE-CORRESPONDING   <wa>  TO  <ls_struct>."Para No tener Tabix/tabixP q no existe en IDOC
      ls_datos-sdata =  <ls_struct>.
      APPEND  ls_datos TO <datos>.

    ELSE.
      CLEAR  <wa>.
      LOOP AT <tab> INTO <wa>.
        ASSIGN COMPONENT 'tabixp' OF STRUCTURE <wa> TO <ld_fld>.
        IF <ld_fld> IS ASSIGNED.
          CHECK <ld_fld> = ls_tabix.
          UNASSIGN <ld_fld>.
        ENDIF.

        ASSIGN COMPONENT 'tabix' OF STRUCTURE <wa> TO <ld_fld>.
        IF <ld_fld> IS ASSIGNED.
          lv_tabix = <ld_fld> .
        ENDIF.
        CLEAR ls_datos.
        ls_datos-segnam = parametro-estructura. "co_z1mm_dte_XXXXXX.
        ls_datos-hlevel = parametro-nivelj.
        MOVE-CORRESPONDING   <wa>  TO  <ls_struct>. "Para No tener Tabix/tabixP q no existe en IDOC
        ls_datos-sdata =  <ls_struct>.
        APPEND  ls_datos TO <datos>.

        LOOP AT lt_parametro INTO wa_parametro.
          load_sap_datah( parametro = wa_parametro ls_tabix = lv_tabix ).
        ENDLOOP.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD LOAD_SAP_DATA_1.
    DATA: lt_control      TYPE TABLE OF edidc,
          ls_control      TYPE edidc,
          ls_inbound_proc TYPE tede2,
          ls_ediadmin     TYPE swhactor.
    DATA: ls_datos    TYPE edidd,
          lv_docnum   TYPE edidc-docnum,
          lv_statproc LIKE sy-subrc.

* tablas internas locales .
    DATA: it_comm  TYPE TABLE OF  edidc,
          it_datos TYPE TABLE OF  edidd.

    "-->datos de control.
    ls_control-mestyp   = co_z_dte_felec.
    ls_control-idoctp   = co_zid_dte_felec.
    ls_control-direct   = co_input.
    ls_control-serial   = sy-datum.
    ls_control-serial+8 = sy-uzeit.
*  ls_control-rcvpor  = co_rcvpor.

    " Buscar interlocutor remitente
    ls_control-sndprt = co_sndprt.
*  ls_control-rcvprn = co_rcvprn. "iterlocutor
    SELECT SINGLE sndprn INTO ls_control-sndprn
    FROM edp21
    WHERE sndprt EQ co_sndprt
      AND mestyp EQ co_z_dte_felec.
    IF sy-subrc EQ 0.
      CONCATENATE 'SAP' ls_control-rcvprn(3) INTO ls_control-sndpor.
    ENDIF.

    " Intelocutor destinatario
    CONCATENATE sy-sysid 'CLNT' sy-mandt INTO ls_control-rcvprn.
    CONCATENATE 'SAP' sy-sysid INTO ls_control-rcvpor.
    ls_control-rcvprt = co_sndprt.
    ls_control-outmod = '2'.
    ls_control-status = '53'.

    "--> Cabecera
    CLEAR ls_datos.
    ls_datos-segnam = co_z1mm_dte_encabezado.
    ls_datos-hlevel = 1.
    ls_datos-sdata =  gs_recfact.
    APPEND  ls_datos TO it_datos  .

    "--> Cabecera2
    CLEAR ls_datos.
    ls_datos-segnam = co_z1mm_dte_encabezado2.
    ls_datos-sdata =  gs_recfact2.
    ls_datos-hlevel = 1.
    APPEND  ls_datos TO it_datos  .

    "-->Otros Impuestos
    CLEAR ls_datos.
    ls_datos-segnam = co_z1mm_dte_impret.
    ls_datos-hlevel = 2.
    LOOP AT gt_imrecfact INTO gs_imrecfact.
      ls_datos-sdata =  gs_imrecfact.
      APPEND  ls_datos TO it_datos. CLEAR: gs_imrecfact.
    ENDLOOP.

    "-->Referencia
    CLEAR ls_datos.
    ls_datos-segnam = co_z1mm_dte_refer.
    ls_datos-hlevel = 2.
    LOOP AT gt_rfrecfact INTO gs_rfrecfact.
      ls_datos-sdata =  gs_rfrecfact.
      APPEND  ls_datos TO it_datos. CLEAR: gs_rfrecfact.
    ENDLOOP.

    "-->Detalle
    LOOP AT gt_dtrecfact_id INTO gs_dtrecfact_id.
      CLEAR ls_datos.
      ls_datos-segnam = co_z1mm_dte_detalle.
      ls_datos-hlevel = 2.
      MOVE-CORRESPONDING gs_dtrecfact_id TO gs_dtrecfact.
      ls_datos-sdata =  gs_dtrecfact.
      APPEND  ls_datos TO it_datos. CLEAR: gs_dtrecfact.

      "-->Detalle2
      CLEAR ls_datos.
      ls_datos-segnam = co_z1mm_dte_det_codl.
      ls_datos-hlevel = 3.
      LOOP AT gt_dtrecfact2_id INTO gs_dtrecfact2_id
                                WHERE tabix EQ gs_dtrecfact_id-tabix.
        CLEAR ls_datos-sdata.
        MOVE-CORRESPONDING gs_dtrecfact2_id TO gs_dtrecfact2.
        ls_datos-sdata =  gs_dtrecfact2.
        APPEND  ls_datos TO it_datos. CLEAR: gs_dtrecfact2.
      ENDLOOP.
    ENDLOOP.

     "--> Timbre electronico Digital
    CLEAR ls_datos.
    ls_datos-segnam = co_z1mm_dte_ted.
    ls_datos-sdata  = gs_tdrecfact.
    ls_datos-hlevel = 3.
    APPEND ls_datos TO it_datos  .

    "--> FileName
    IF gs_filename IS NOT INITIAL.
      CLEAR ls_datos.
      ls_datos-segnam = co_z1mm_dte_filename.
      ls_datos-hlevel = 3.
      ls_datos-sdata =  gs_filename.
      APPEND  ls_datos TO it_datos.
    ENDIF.



    "-->
    CASE ls_control-direct.
      WHEN co_input. " Entrada
        CLEAR: lv_docnum.
        "-->
        CALL FUNCTION 'IDOC_INBOUND_WRITE_TO_DB'
          EXPORTING
            pi_do_handle_error      = abap_true
            pi_return_data_flag     = abap_false
          IMPORTING
            pe_idoc_number          = lv_docnum
            pe_state_of_processing  = lv_statproc
            pe_inbound_process_data = ls_inbound_proc
          TABLES
            t_data_records          = it_datos
          CHANGING
            pc_control_record       = ls_control
          EXCEPTIONS
            idoc_not_saved          = 1
            OTHERS                  = 2.
        IF sy-subrc EQ 0.
          COMMIT WORK.

          CLEAR: lt_control.
          APPEND ls_control TO lt_control.

          "--> pass idoc into application
          CALL FUNCTION 'IDOC_START_INBOUND'
            EXPORTING
              pi_inbound_process_data = ls_inbound_proc
              pi_called_online        = 'X'
              pi_org_unit             = ls_ediadmin
              succ_show_flag          = 'X'
            TABLES
              t_control_records       = lt_control
            EXCEPTIONS
              OTHERS                  = 1.
          IF sy-subrc EQ 0.
            IF go_log IS BOUND.
              CLEAR gs_bal_msg.
              gs_bal_msg-msgid = 'EA'.
              gs_bal_msg-msgty = 'S'.
              gs_bal_msg-msgno = '709'.
              gs_bal_msg-msgv1 = lv_docnum.
              go_log->add_msg( i_s_msg = gs_bal_msg ).
              go_log->save( ).
            ENDIF.
          ELSE.
            IF go_log IS BOUND.
              CLEAR gs_bal_msg.
              gs_bal_msg-msgid = 'EA'.
              gs_bal_msg-msgty = 'I'.
              gs_bal_msg-msgno = '424'.
              gs_bal_msg-msgv1 = lv_docnum.
              go_log->add_msg( i_s_msg = gs_bal_msg ).
              go_log->save( ).
            ENDIF.
          ENDIF.
        ELSE.
          " Error
          IF go_log IS BOUND.
            CLEAR gs_bal_msg.
            gs_bal_msg-msgid = sy-msgid.
            gs_bal_msg-msgty = 'W'.
            gs_bal_msg-msgno = sy-msgno.
            gs_bal_msg-msgv1 = sy-msgv1.
            gs_bal_msg-msgv2 = sy-msgv2.
            gs_bal_msg-msgv3 = sy-msgv3.
            gs_bal_msg-msgv4 = sy-msgv4.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
          ENDIF.
        ENDIF.
      WHEN co_output." Salida
        "-->
        CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE' IN UPDATE TASK
          EXPORTING
            master_idoc_control            = ls_control
          TABLES
            communication_idoc_control     = it_comm
            master_idoc_data               = it_datos
          EXCEPTIONS
            error_in_idoc_control          = 1
            error_writing_idoc_status      = 2
            error_in_idoc_data             = 3
            sending_logical_system_unknown = 4
            OTHERS                         = 5.

        IF sy-subrc IS INITIAL.
          COMMIT WORK.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

    CLEAR it_comm[].
    CLEAR it_datos[].
  ENDMETHOD.


  METHOD load_sap_data_2.
    DATA: lt_control      TYPE TABLE OF edidc,
          ls_control      TYPE edidc,
          ls_inbound_proc TYPE tede2,
          ls_ediadmin     TYPE swhactor.
    DATA: ls_datos         TYPE edidd,
          lv_docnum        TYPE edidc-docnum,
          ls_zid_dte_felec TYPE edidc-idoctp,
          ls_z_dte_felec   TYPE edidc-mestyp,
          lv_statproc      LIKE sy-subrc.

* tablas internas locales .
    DATA: it_comm      TYPE TABLE OF  edidc,
          it_datos     TYPE TABLE OF  edidd,
          ls_tabixp    TYPE sy-tabix,
          wa_parametro TYPE ztfi_0078c,
          wa_0078d     TYPE ztfi_0078d,
          lt_0078d     TYPE STANDARD TABLE OF ztfi_0078d,
          lt_parametro TYPE STANDARD TABLE OF ztfi_0078c.

    "-->datos de control.
    SELECT nombre_int INTO ls_zid_dte_felec FROM ztfi_0078b UP TO 1 ROWS
    WHERE vatint = gs_varint
      AND codint = '0009'
      ORDER BY PRIMARY KEY.
    ENDSELECT.
    SELECT nombre_int INTO ls_z_dte_felec FROM ztfi_0078b UP TO 1 ROWS
    WHERE vatint = gs_varint
     AND codint = '0010'
    ORDER BY PRIMARY KEY.
    ENDSELECT.

    ls_control-mestyp   = ls_z_dte_felec. "co_z_dte_felec.
    ls_control-idoctp   = ls_zid_dte_felec. "co_zid_dte_felec.
    ls_control-direct   = co_input.
    ls_control-serial   = sy-datum.
    ls_control-serial+8 = sy-uzeit.
*  ls_control-rcvpor  = co_rcvpor.

    " Buscar interlocutor remitente
    ls_control-sndprt = co_sndprt.
*  ls_control-rcvprn = co_rcvprn. "iterlocutor
    SELECT SINGLE sndprn INTO ls_control-sndprn
    FROM edp21
    WHERE sndprt EQ co_sndprt
      AND mestyp EQ ls_z_dte_felec. "co_z_dte_felec.
    IF sy-subrc EQ 0.
      CONCATENATE 'SAP' ls_control-rcvprn(3) INTO ls_control-sndpor.
    ENDIF.

    " Intelocutor destinatario
    CONCATENATE sy-sysid 'CLNT' sy-mandt INTO ls_control-rcvprn.
    CONCATENATE 'SAP' sy-sysid INTO ls_control-rcvpor.
    ls_control-rcvprt = co_sndprt.
    ls_control-outmod = '2'.
    ls_control-status = '53'.
    REFRESH  gt_datos. CLEAR  gt_datos.

*    SELECT * FROM ztfi_0078c INTO TABLE  lt_parametro
*       WHERE vatint = gs_varint
*        AND  nodop = ' '
*        AND  nivelj NE ' '. "Se deja en blanco los q van al misma estructura IDOC, solo para tomar 1
    REFRESH lt_0078d .  REFRESH lt_parametro. CLEAR wa_parametro. CLEAR  wa_0078d.

    SELECT * FROM ztfi_0078d INTO TABLE lt_0078d
    WHERE  vatint = gs_varint.

    LOOP AT lt_0078d INTO wa_0078d.
      SELECT  * INTO wa_parametro  FROM ztfi_0078c  UP TO 1 ROWS
        WHERE vatint = gs_varint
         AND  nodop = ' '
         AND  tipo  =  wa_0078d-tipo
       ORDER BY vatint sec.
      ENDSELECT.
      APPEND wa_parametro TO lt_parametro.
    ENDLOOP.

    SORT lt_parametro BY sec.
    LOOP AT lt_parametro INTO wa_parametro.

      load_sap_datah( parametro = wa_parametro ls_tabix = 0 ).
    ENDLOOP.
    "--> FileName
    IF gs_filename IS NOT INITIAL.
      CLEAR ls_datos.
      ls_datos-segnam = co_z1mm_dte_filename.
      ls_datos-hlevel = 3.
      ls_datos-sdata =  gs_filename.
      APPEND  ls_datos TO  gt_datos.
    ENDIF.

    it_datos[] = gt_datos[].

    "-->
    CASE ls_control-direct.
      WHEN co_input. " Entrada
        CLEAR: lv_docnum.
        "-->
        CALL FUNCTION 'IDOC_INBOUND_WRITE_TO_DB'
          EXPORTING
            pi_do_handle_error      = abap_true
            pi_return_data_flag     = abap_false
          IMPORTING
            pe_idoc_number          = lv_docnum
            pe_state_of_processing  = lv_statproc
            pe_inbound_process_data = ls_inbound_proc
          TABLES
            t_data_records          = it_datos
          CHANGING
            pc_control_record       = ls_control
          EXCEPTIONS
            idoc_not_saved          = 1
            OTHERS                  = 2.
        IF sy-subrc EQ 0.
          COMMIT WORK.

          CLEAR: lt_control.
          APPEND ls_control TO lt_control.

          "--> pass idoc into application
          CALL FUNCTION 'IDOC_START_INBOUND'
            EXPORTING
              pi_inbound_process_data = ls_inbound_proc
              pi_called_online        = 'X'
              pi_org_unit             = ls_ediadmin
              succ_show_flag          = 'X'
            TABLES
              t_control_records       = lt_control
            EXCEPTIONS
              OTHERS                  = 1.
          IF sy-subrc EQ 0.
            IF go_log IS BOUND.
              CLEAR gs_bal_msg.
              gs_bal_msg-msgid = 'EA'.
              gs_bal_msg-msgty = 'S'.
              gs_bal_msg-msgno = '709'.
              gs_bal_msg-msgv1 = lv_docnum.
              go_log->add_msg( i_s_msg = gs_bal_msg ).
              go_log->save( ).
            ENDIF.
          ELSE.
            IF go_log IS BOUND.
              CLEAR gs_bal_msg.
              gs_bal_msg-msgid = 'EA'.
              gs_bal_msg-msgty = 'I'.
              gs_bal_msg-msgno = '424'.
              gs_bal_msg-msgv1 = lv_docnum.
              go_log->add_msg( i_s_msg = gs_bal_msg ).
              go_log->save( ).
            ENDIF.
          ENDIF.
        ELSE.
          " Error
          IF go_log IS BOUND.
            CLEAR gs_bal_msg.
            gs_bal_msg-msgid = sy-msgid.
            gs_bal_msg-msgty = 'W'.
            gs_bal_msg-msgno = sy-msgno.
            gs_bal_msg-msgv1 = sy-msgv1.
            gs_bal_msg-msgv2 = sy-msgv2.
            gs_bal_msg-msgv3 = sy-msgv3.
            gs_bal_msg-msgv4 = sy-msgv4.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
          ENDIF.
        ENDIF.
      WHEN co_output." Salida
        "-->
        CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE' IN UPDATE TASK
          EXPORTING
            master_idoc_control            = ls_control
          TABLES
            communication_idoc_control     = it_comm
            master_idoc_data               = it_datos
          EXCEPTIONS
            error_in_idoc_control          = 1
            error_writing_idoc_status      = 2
            error_in_idoc_data             = 3
            sending_logical_system_unknown = 4
            OTHERS                         = 5.

        IF sy-subrc IS INITIAL.
          COMMIT WORK.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

    CLEAR it_comm[].
    CLEAR it_datos[].

  ENDMETHOD.


  METHOD mappingxml_to_struch.
    FIELD-SYMBOLS: <tab>       TYPE STANDARD TABLE,
                   <wa>        TYPE any,
                   <tcampo>    TYPE table,
                   <ls_struct> TYPE any,
                   <ld_fld>    TYPE any.

    DATA: ldo_data     TYPE REF TO data.
    DATA: ldo_data2     TYPE REF TO data.

    DATA: lv_value    TYPE string,
          lv_name     TYPE string,
          ls_stream   TYPE string,
          lt_return   TYPE bapiret2_t,
          lt_xmltb    TYPE STANDARD TABLE OF smum_xmltb,
          lf_node     TYPE REF TO if_ixml_node,
          lv_tabix    LIKE sy-tabix,
          lv_tabixc   LIKE sy-tabix,
          ls_campo    TYPE zcb_recfactmap, "estas estructienen los mismos campo
          lt_campo    TYPE  STANDARD TABLE OF zrecfactmap, "zcb_recfactmap,
          lf_iterator TYPE REF TO if_ixml_node_iterator,
          lf_filter   TYPE REF TO if_ixml_node_filter,
          ls_name     TYPE string,
          sw          TYPE int4,
          lo_id       TYPE i.

    DATA: lt_parametro  TYPE STANDARD TABLE OF ztfi_0078c,
          wa_parametro  TYPE ztfi_0078c,
          lt_parametroh TYPE STANDARD TABLE OF ztfi_0078c,
          wa_parametroh TYPE  ztfi_0078c.



    READ TABLE  parametro INDEX '1' INTO wa_parametro.

    ASSIGN (wa_parametro-atributo)   TO   <tab>.
    ASSIGN (wa_parametro-waatributo) TO   <wa>.


    " Create dynamic structure
    CREATE DATA ldo_data TYPE (wa_parametro-estructura).
    ASSIGN ldo_data->* TO <ls_struct>.


    CLEAR wa_parametro.

    SORT parametro BY sec.

    LOOP AT parametro INTO wa_parametro.

      "SEARCH FOR MAPPING CUSTOMIZED.

      SELECT * INTO TABLE lt_campo FROM (wa_parametro-tabcampo)
      WHERE vatint EQ  gs_varint
      AND clave EQ wa_parametro-clave
      ORDER BY vatint clave.

      IF sy-subrc <> 0.
        "RAISE MAPPING.
      ENDIF.

      sw = 0. REFRESH lt_parametro.
      SELECT * FROM ztfi_0078c INTO  TABLE  lt_parametro
       WHERE vatint EQ gs_varint "'CL'
       AND nodop EQ wa_parametro-nodo.

      CLEAR: lv_tabix.
      lf_node = nodo.
      "---> nodos por cada parametrizacion de parametros
      IF lf_node IS INITIAL.
        CLEAR ls_name.
        ls_name =  wa_parametro-nodoruta.
        IF  ls_name = '.' OR ls_name IS INITIAL.
          lf_node = go_xml->m_document.
        ELSE.
          IF ls_name CA '/'. "Si se va por Path colocar concatenar el root.
            CONCATENATE '//' ls_nameroot ls_name INTO ls_name.
            CONDENSE ls_name NO-GAPS.
          ENDIF.
          lf_node = go_xml->find_node( name = ls_name ).
        ENDIF.
      ENDIF.

      IF  lf_node IS NOT INITIAL.
        ls_name = wa_parametro-nodo.
        lf_filter = lf_node->create_filter_name( name = ls_name ).
        lf_iterator = lf_node->create_iterator_filtered( filter = lf_filter ).
*       lf_iterator = lf_node->create_iterator(  ).
        lf_node = lf_iterator->get_next( ).
        WHILE NOT lf_node IS INITIAL.
          lv_name = lf_node->get_name( ).
          lv_value = lf_node->get_value( ).
          CASE lv_name.
            WHEN  wa_parametro-nodo.
              ADD 1 TO lv_tabix.
              CLEAR: lt_xmltb.
              me->get_nodo_xml_to_tab( EXPORTING node = lf_node
                                       IMPORTING t_return = lt_return
                                                 t_xmltb  = lt_xmltb ).
              LOOP AT lt_xmltb INTO gs_xml.
                READ TABLE lt_campo INTO ls_campo
                                           WITH KEY xml_property = gs_xml-cname.
                IF sy-subrc EQ 0.
                  ASSIGN COMPONENT ls_campo-str_property
                                   OF STRUCTURE <ls_struct> TO <ld_fld>.
                  <ld_fld> = gs_xml-cvalue.
                ENDIF.
              ENDLOOP.

              IF sy-subrc        EQ 0
              AND <ls_struct> IS ASSIGNED.
                MOVE-CORRESPONDING  <ls_struct> TO <wa>.
                sw = 1. "entroValor
                IF wa_parametro-clase = 'P'. "solo en posicion
                  ASSIGN COMPONENT 'tabix' OF STRUCTURE <wa> TO <ld_fld>.
                  IF  <ld_fld> IS ASSIGNED.
                    CLEAR  lv_tabixc.
                    lv_tabixc = ( ( ( lv_tabix * 10 ) + ls_tabix )  * 10 ).
                    <ld_fld>    =  lv_tabixc. "lv_tabix.
                  ENDIF.
                  UNASSIGN <ld_fld>.
                  ASSIGN COMPONENT 'tabixp' OF STRUCTURE <wa> TO <ld_fld>.
                  IF  <ld_fld> IS ASSIGNED.
                    <ld_fld>    =  ls_tabix.
                  ENDIF.
                  UNASSIGN <ld_fld>.

*                  IF  <ld_fld> IS ASSIGNED.
*                    IF ls_tabix = 0.
*                      <ld_fld>    = lv_tabix.
*                    ELSE.
*                      <ld_fld>    = ls_tabix. "CAMBIE el LV_tabix por el q manda el padre
*                    ENDIF.
*                  ENDIF.

                  APPEND <wa> TO <tab>.
                ENDIF.
              ENDIF.
              IF lines( lt_parametro ) GT 0. "envio si tengo Nodos hijos
                LOOP AT lt_parametro INTO wa_parametroh.
                  REFRESH lt_parametroh.
                  APPEND wa_parametroh TO lt_parametroh.
*                me->mappingxml_to_struch( EXPORTING nodo = lf_node parametro = lt_parametro  ls_tabix = lv_tabixc ).
                  me->mappingxml_to_struch( EXPORTING nodo = lf_node parametro = lt_parametroh  ls_tabix = lv_tabixc ).
                ENDLOOP.
              ENDIF.
          ENDCASE.
          lf_node = lf_iterator->get_next( ).
          IF sw = 1 AND wa_parametro-clase = 'C'. " si es cabecera y tiene ya entro a buscar los datos Salgo
            CLEAR lf_node.
          ENDIF.

        ENDWHILE.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD mapping_cuadratura.
    DATA: lt_linea       TYPE TABLE OF string,
          ls_linea       TYPE string,
          ls_resto       TYPE string,
          ls_ztfi_0074fr TYPE ztfi_0074fr,
          lt_ztfi0074fr  TYPE TABLE OF ztfi_0074fr,
          lv_fecha       TYPE char10,
*          lo_sii         TYPE REF TO zcl_sii,
          lo_file        TYPE REF TO zcl_dte_file_proc,
          lv_fecharec    TYPE char10,
          lv_horarec     TYPE char08.

    DATA: lt_listado TYPE zdte_listarec_t.
    CLEAR:     gt_fi0074fr.
    lt_linea = t_data.

    CREATE OBJECT lo_file.

    CALL METHOD lo_file->get_parse_table
      EXPORTING
        i_separator = ';'
        i_anytable  = t_data
        i_tabname   = 'ZDTE_LISTAREC'
      IMPORTING
        e_table     = lt_listado.

    "--> Borrar registro cabecera
    DELETE lt_listado INDEX 1.

    LOOP AT lt_listado ASSIGNING FIELD-SYMBOL(<f2>).

      CLEAR: ls_ztfi_0074fr,
                   lv_fecharec,
                   lv_horarec.


      ls_ztfi_0074fr-bukrs     = p_bukrs.
      ls_ztfi_0074fr-xblnr     = <f2>-xblnr.
      CONDENSE <f2>-tipodte.

      CASE <f2>-tipodte.
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
      ls_ztfi_0074fr-stcd1     = <f2>-rutemisor.
      ls_ztfi_0074fr-estatus   = abap_true.

      " Buscar proveedor
      SELECT SINGLE lifnr INTO ls_ztfi_0074fr-lifnr
      FROM lfa1
      WHERE stcd1 EQ <f2>-rutemisor.
      IF sy-subrc EQ 0.
      ENDIF.

      " Formatear fecha YYYY-MM-DD
      CONCATENATE <f2>-fecha_emi+0(4)
                  <f2>-fecha_emi+5(2)
                  <f2>-fecha_emi+8(2) INTO ls_ztfi_0074fr-bldat.

      " Formatear fecha/hora recepción YYYY-MM-DD
      SPLIT <f2>-fecha_rec AT space INTO lv_fecharec
                                          lv_horarec.
      CONCATENATE lv_fecharec+0(4)
                  lv_fecharec+5(2)
                  lv_fecharec+8(2) INTO ls_ztfi_0074fr-fecharsii.

      REPLACE ALL OCCURRENCES OF ':' IN lv_horarec WITH ''.
      CONDENSE lv_horarec.
      ls_ztfi_0074fr-horarsii = lv_horarec.
      OVERLAY ls_ztfi_0074fr-horarsii WITH '000000'.

      "--> Fecha y hora de mod.
      ls_ztfi_0074fr-cpudt = sy-datum.
      ls_ztfi_0074fr-cputm = sy-uzeit.

      APPEND ls_ztfi_0074fr TO lt_ztfi0074fr.
    ENDLOOP.

    IF lines( lt_ztfi0074fr ) GT 0.
      CLEAR: gt_fi0074fr.
      gt_fi0074fr = lt_ztfi0074fr.
    ENDIF.
  ENDMETHOD.


  METHOD mapping_cuadraturanw.
    DATA: lt_linea       TYPE TABLE OF string,
          ls_linea       TYPE string,
          ls_resto       TYPE string,
          ls_ztfi_0074fr TYPE ztfi_0074fr,
          lt_ztfi0074fr  TYPE TABLE OF ztfi_0074fr,
          lv_fecha       TYPE char10,
*          lo_sii         TYPE REF TO zcl_sii,
          lo_file        TYPE REF TO zcl_dte_file_proc,
          lv_fecharec    TYPE char10,
          lv_horarec     TYPE char08.

    DATA: lt_listado TYPE zdte_listarecnw_t.
    CLEAR:     gt_fi0074fr.
    lt_linea = t_data.

    CREATE OBJECT lo_file.

    CALL METHOD lo_file->get_parse_table
      EXPORTING
        i_separator = ';'
        i_anytable  = t_data
        i_tabname   = 'ZDTE_LISTARECNW'
      IMPORTING
        e_table     = lt_listado.

    "--> Borrar registro cabecera
    DELETE lt_listado INDEX 1.

    LOOP AT lt_listado ASSIGNING FIELD-SYMBOL(<f2>).

      CLEAR: ls_ztfi_0074fr,
                   lv_fecharec,
                   lv_horarec.
      ls_ztfi_0074fr-bukrs     = p_bukrs.
      ls_ztfi_0074fr-xblnr     = <f2>-xblnr.
      ls_ztfi_0074fr-tipodte   = p_tipodoc.
      ls_ztfi_0074fr-stcd1     = <f2>-rutemisor.
      ls_ztfi_0074fr-estatus   = abap_true.
** Ini.Ajuste para guardar info LC
*      ls_ztfi_0074fr-estatuslc = abap_true.
*      ls_ztfi_0074fr-waers    = 'CLP'.
*      ls_ztfi_0074fr-mntoexe  = <f2>-mntoexe / 100.
*      ls_ztfi_0074fr-mntneto  = <f2>-mntneto  / 100.
*      ls_ztfi_0074fr-ivarec   = <f2>-ivarec  / 100.
*      ls_ztfi_0074fr-ivanorec = <f2>-ivanorec  / 100.
*      ls_ztfi_0074fr-mnttotal = <f2>-mnttotal  / 100.
*      ls_ztfi_0074fr-codiva   = <f2>-codiva.
*      ls_ztfi_0074fr-cpudtlc  = sy-datum.
*      ls_ztfi_0074fr-cputmlc  = sy-uzeit.
*      ls_ztfi_0074fr-periodo  = p_periodo.
*      ls_ztfi_0074fr-rznsoc = <f2>-rznsocemisor(35).
** Fin.Ajuste para guardar info LC
        " Buscar proveedor
        SELECT SINGLE lfa1~lifnr
        INTO ls_ztfi_0074fr-lifnr
        FROM lfa1 INNER JOIN lfb1 ON lfa1~lifnr = lfb1~lifnr
          WHERE lfb1~bukrs EQ  p_bukrs
            AND   lfa1~stcd1 EQ  <f2>-rutemisor
            AND   lfa1~sperr EQ space
            AND   lfa1~sperq EQ space
            AND   lfb1~sperr EQ space
            AND   lfa1~loevm EQ space
            AND   lfb1~loevm EQ space.


      " Formatear fecha DD-MM-YYYY.
      CONCATENATE <f2>-fecha_doc+6(4)
                  <f2>-fecha_doc+3(2)
                  <f2>-fecha_doc+0(2) INTO ls_ztfi_0074fr-bldat.

      " Formatear fecha/hora recepción DD-MM-YYYY.
      SPLIT <f2>-fecha_rec AT space INTO lv_fecharec
                                               lv_horarec.
      CONCATENATE lv_fecharec+6(4)
                  lv_fecharec+3(2)
                  lv_fecharec+0(2) INTO ls_ztfi_0074fr-fecharsii.


      REPLACE ALL OCCURRENCES OF ':' IN lv_horarec WITH ''.
      CONDENSE lv_horarec.
      ls_ztfi_0074fr-horarsii = lv_horarec.
      OVERLAY ls_ztfi_0074fr-horarsii WITH '000000'.

      ls_ztfi_0074fr-cpudt    = sy-datum.
      ls_ztfi_0074fr-cputm    = sy-uzeit.


      APPEND ls_ztfi_0074fr TO lt_ztfi0074fr.

    ENDLOOP.

    IF lines( lt_ztfi0074fr ) GT 0.
      CLEAR: gt_fi0074fr.
      gt_fi0074fr = lt_ztfi0074fr.
    ENDIF.


  ENDMETHOD.


  METHOD mapping_lc.
    DATA: lt_linea       TYPE TABLE OF string,
          ls_linea       TYPE string,
          ls_resto       TYPE string,
          ls_ztfi_0074fr TYPE ztfi_0074fr,
          lt_ztfi0074fr  TYPE TABLE OF ztfi_0074fr,
          lv_fecha       TYPE char10,
*          lo_sii         TYPE REF TO zcl_sii,
          lo_file        TYPE REF TO zcl_dte_file_proc,
          lv_fecharec    TYPE char10,
          lv_horarec     TYPE char08.

    DATA: lt_listado TYPE zdte_listafac_t.
    CLEAR:     gt_fi0074fr.
    lt_linea = t_data.

    CREATE OBJECT lo_file.

    CALL METHOD lo_file->get_parse_table
      EXPORTING
        i_separator = ';'
        i_anytable  = t_data
        i_tabname   = 'ZDTE_LISTAFAC'
      IMPORTING
        e_table     = lt_listado.

    "--> Borrar registro cabecera
    DELETE lt_listado INDEX 1.

    LOOP AT lt_listado ASSIGNING FIELD-SYMBOL(<f2>).

      CLEAR: ls_ztfi_0074fr,
                   lv_fecharec,
                   lv_horarec.
      ls_ztfi_0074fr-bukrs     = p_bukrs.
      ls_ztfi_0074fr-xblnr     = <f2>-xblnr.
      ls_ztfi_0074fr-tipodte   = <f2>-tipo_doc.
      ls_ztfi_0074fr-stcd1     = <f2>-rutemisor.
      ls_ztfi_0074fr-estatus   = abap_true.
** Ini.Ajuste para guardar info LC
      ls_ztfi_0074fr-estatuslc = abap_true.
      ls_ztfi_0074fr-waers    = 'CLP'.
      ls_ztfi_0074fr-mntoexe  = <f2>-mntoexe / 100.
      ls_ztfi_0074fr-mntneto  = <f2>-mntneto  / 100.
      ls_ztfi_0074fr-ivarec   = <f2>-ivarec  / 100.
      ls_ztfi_0074fr-ivanorec = <f2>-ivanorec  / 100.
      ls_ztfi_0074fr-mnttotal = <f2>-mnttotal  / 100.
      ls_ztfi_0074fr-codiva   = <f2>-codiva.
      ls_ztfi_0074fr-cpudtlc  = sy-datum.
      ls_ztfi_0074fr-cputmlc  = sy-uzeit.
      ls_ztfi_0074fr-periodo  = p_periodo.
      ls_ztfi_0074fr-rznsoc = <f2>-rznsocemisor(35).
** Fin.Ajuste para guardar info LC

        " Buscar proveedor
        SELECT SINGLE lfa1~lifnr
        INTO ls_ztfi_0074fr-lifnr
        FROM lfa1 INNER JOIN lfb1 ON lfa1~lifnr = lfb1~lifnr
          WHERE lfb1~bukrs EQ  p_bukrs
            AND   lfa1~stcd1 EQ  <f2>-rutemisor
            AND   lfa1~sperr EQ space
            AND   lfa1~sperq EQ space
            AND   lfb1~sperr EQ space
            AND   lfa1~loevm EQ space
            AND   lfb1~loevm EQ space.


      " Formatear fecha DD-MM-YYYY.
      CONCATENATE <f2>-fecha_doc+6(4)
                  <f2>-fecha_doc+3(2)
                  <f2>-fecha_doc+0(2) INTO ls_ztfi_0074fr-bldat.

      " Formatear fecha/hora recepción DD-MM-YYYY.
      SPLIT <f2>-fecha_rec AT space INTO lv_fecharec
                                               lv_horarec.
      CONCATENATE lv_fecharec+6(4)
                  lv_fecharec+3(2)
                  lv_fecharec+0(2) INTO ls_ztfi_0074fr-fecharsii.


      REPLACE ALL OCCURRENCES OF ':' IN lv_horarec WITH ''.
      CONDENSE lv_horarec.
      ls_ztfi_0074fr-horarsii = lv_horarec.
      OVERLAY ls_ztfi_0074fr-horarsii WITH '000000'.

      ls_ztfi_0074fr-cpudt    = sy-datum.
      ls_ztfi_0074fr-cputm    = sy-uzeit.


      APPEND ls_ztfi_0074fr TO lt_ztfi0074fr.

    ENDLOOP.

    IF lines( lt_ztfi0074fr ) GT 0.
      CLEAR: gt_fi0074fr.
      gt_fi0074fr = lt_ztfi0074fr.
    ENDIF.
  ENDMETHOD.


  METHOD mapping_to_struc.


   CALL METHOD me->mapping_to_struc_1
      EXPORTING
       filename = filename
       .


  ENDMETHOD.


  METHOD MAPPING_TO_STRUC_1.


    DATA: ls_recfact       TYPE z1mm_dte_encabezado,
          ls_recfact2      TYPE z1mm_dte_encabezado2,
          ls_dtrecfact     TYPE z1mm_dte_detalle,
          ls_dtrecfact2    TYPE z1mm_dte_det_codl,
          ls_imrecfact     TYPE z1mm_dte_impret,
          ls_struc         TYPE string VALUE 'LS_RECFACT',

          ls_recfactmap    TYPE zcb_recfactmap,      "header
          ls_recfactmap2   TYPE zcb_recfactmap2,     "header2
          ls_dtrecfactmap  TYPE zdt_recfactmap,      "detail
          ls_dtrecfactmap2 TYPE zdt_recfactmap2,     "detailcodigo
          ls_rfrecfactmap  TYPE zrf_recfactmap,      "reference
          ls_imrecfactmap  TYPE zim_recfactmap,      "OtrosImpuestos
          ls_tdrecfactmap  TYPE ztd_recfactmap,      "TimbreElect.Dig

          lt_recfactmap    TYPE STANDARD TABLE OF zcb_recfactmap, "header
          lt_recfactmap2   TYPE STANDARD TABLE OF zcb_recfactmap2, "header2
          lt_dtrecfactmap  TYPE STANDARD TABLE OF zdt_recfactmap, "detail
          lt_dtrecfactmap2 TYPE STANDARD TABLE OF zdt_recfactmap2, "detail codigo
          lt_rfrecfactmap  TYPE STANDARD TABLE OF zrf_recfactmap, "reference
          lt_imrecfactmap  TYPE STANDARD TABLE OF zim_recfactmap, "OtrosImpuestos
          lt_tdrecfactmap  TYPE STANDARD TABLE OF ztd_recfactmap, "TimbreElect.Dig

          ls_xml           LIKE me->gs_xml,
          ls_xml2          LIKE me->gs_xml,
          lv_tabix         LIKE sy-tabix,
          lv_tabix2        LIKE sy-tabix.

    DATA:
      lv_count     TYPE i,
      ldo_data     TYPE REF TO data,
      ldo_data2    TYPE REF TO data,
      ldo_data_dt  TYPE REF TO data,
      ldo_data_dt2 TYPE REF TO data,
      ldo_data_rf  TYPE REF TO data,
      ldo_data_im  TYPE REF TO data,
      ldo_data_fl  TYPE REF TO data,
      ldo_data_td  TYPE REF TO data,
      lo_typedescr TYPE REF TO cl_abap_typedescr,
      ld_fldname   TYPE string.

    FIELD-SYMBOLS:
      <ls_struct>     TYPE any,
      <ls_struct2>    TYPE any,
      <ls_struct_dt>  TYPE any,
      <ls_struct_dt2> TYPE any,
      <ls_struct_rf>  TYPE any,
      <ls_struct_im>  TYPE any,
      <ls_struct_fl>  TYPE any,
      <ls_struct_td>  TYPE any,
      <ld_fld>        TYPE any.

    CLEAR: gs_recfact,
           gs_recfact2,
           gt_dtrecfact,
           gt_dtrecfact2,
           gt_rfrecfact,
           gt_imrecfact,
           gt_dtrecfact_id,
           gt_dtrecfact2_id,
           gs_tdrecfact.
    REFRESH: gt_dtrecfact,
             gt_dtrecfact2,
             gt_rfrecfact,
             gt_imrecfact,
             gt_dtrecfact_id,
             gt_dtrecfact2_id.

    "SEARCH FOR MAPPING CUSTOMIZED.
    SELECT * INTO TABLE lt_recfactmap FROM zcb_recfactmap.
    IF sy-subrc <> 0.
      "RAISE MAPPING.
    ENDIF.

    "SEARCH FOR MAPPING CUSTOMIZED.
    SELECT * INTO TABLE lt_recfactmap2 FROM zcb_recfactmap2.
    IF sy-subrc <> 0.
      "RAISE MAPPING.
    ENDIF.


    "SEARCH FOR MAPPING CUSTOMIZED DETAIL STRUCTURE.
    SELECT * INTO TABLE lt_dtrecfactmap FROM zdt_recfactmap.
    IF sy-subrc <> 0.
      "RAISE MAPPING.
    ENDIF.

    "SEARCH FOR MAPPING CUSTOMIZED DETAIL Codigo STRUCTURE.
    SELECT * INTO TABLE lt_dtrecfactmap2 FROM zdt_recfactmap2.
    IF sy-subrc <> 0.
      "RAISE MAPPING.
    ENDIF.


    "SEARCH FOR MAPPING CUSTOMIZED REFERENCE STRUCTURE.
    SELECT * INTO TABLE lt_rfrecfactmap FROM zrf_recfactmap.
    IF sy-subrc <> 0.
      "RAISE MAPPING.
    ENDIF.

    "SEARCH FOR MAPPING CUSTOMIZED OTHERSTAX STRUCTURE.
    SELECT * INTO TABLE lt_imrecfactmap FROM zim_recfactmap.
    IF sy-subrc <> 0.
      "RAISE MAPPING.
    ENDIF.

    "SEARCH FOR MAPPING CUSTOMIZED OTHERSTAX STRUCTURE.
    SELECT * INTO TABLE lt_tdrecfactmap FROM ztd_recfactmap.
    IF sy-subrc <> 0.
      "RAISE MAPPING.
    ENDIF.

    " Create dynamic structure
    CREATE DATA ldo_data TYPE z1mm_dte_encabezado.
    ASSIGN ldo_data->* TO <ls_struct>.

    " Create dynamic structure
    CREATE DATA ldo_data2 TYPE z1mm_dte_encabezado2.
    ASSIGN ldo_data2->* TO <ls_struct2>.

    "CREATE DYNAMIC STRUCTURE DETAIL
    CREATE DATA ldo_data_dt TYPE z1mm_dte_detalle.
    ASSIGN ldo_data_dt->* TO <ls_struct_dt>.

    "CREATE DYNAMIC STRUCTURE DETAIL
    CREATE DATA ldo_data_dt2 TYPE z1mm_dte_det_codl.
    ASSIGN ldo_data_dt2->* TO <ls_struct_dt2>.


    "Create dynamic structure Reference
    CREATE DATA ldo_data_rf TYPE z1mm_dte_refer.
    ASSIGN ldo_data_rf->* TO <ls_struct_rf>.

    "Create dynamic structure Reference
    CREATE DATA ldo_data_im TYPE z1mm_dte_impret.
    ASSIGN ldo_data_im->* TO <ls_struct_im>.

    "Create dynamic structure Reference
    CREATE DATA ldo_data_fl TYPE z1mm_dte_filename.
    ASSIGN ldo_data_fl->* TO <ls_struct_fl>.

    "Create dynamic structure Reference
    CREATE DATA ldo_data_td TYPE z1mm_dte_ted.
    ASSIGN ldo_data_td->* TO <ls_struct_td>.

    "DO CUSTOMIZED MAPPING. HEADER
    LOOP AT lt_recfactmap INTO ls_recfactmap.

      READ TABLE gt_xml INTO gs_xml WITH KEY cname = ls_recfactmap-xml_property.
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT ls_recfactmap-str_property OF STRUCTURE <ls_struct> TO <ld_fld>.
        <ld_fld> = gs_xml-cvalue.
      ENDIF.
    ENDLOOP.
    gs_recfact = <ls_struct>.

    "DO CUSTOMIZED MAPPING. HEADER2
    LOOP AT lt_recfactmap2 INTO ls_recfactmap2.

      READ TABLE gt_xml INTO gs_xml WITH KEY cname = ls_recfactmap2-xml_property.
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT ls_recfactmap2-str_property OF STRUCTURE <ls_struct2> TO <ld_fld>.
        <ld_fld> = gs_xml-cvalue.
      ENDIF.
    ENDLOOP.
    gs_recfact2 = <ls_struct2>.
**  gs_recfact2-dirrecep    = gs_recfact-dirorigen.
**  gs_recfact2-girorecep   = gs_recfact-giroemis.
**  gs_recfact2-cmnarecep   = gs_recfact-cmnaorigen.
**  gs_recfact2-ciudadrecep = gs_recfact-ciudadorigen.

    "DO CUSTOMIZED MAPPING FOR DETAILS
    DATA: lv_value    TYPE string,
          lv_name     TYPE string,
          ls_stream   TYPE string,
          lt_return   TYPE bapiret2_t,
          lt_xmltb    TYPE STANDARD TABLE OF smum_xmltb,
          lf_node     TYPE REF TO if_ixml_node,
          lf_iterator TYPE REF TO if_ixml_node_iterator.

    go_xml->render_2_string( IMPORTING stream = ls_stream ).

    IF go_xml->parse_string( ls_stream ) EQ 0.
      CLEAR: lv_tabix.
      lf_node = go_xml->m_document.
      IF  lf_node IS NOT INITIAL.
        lf_iterator = lf_node->create_iterator( ).
        lf_node = lf_iterator->get_next( ).
        WHILE NOT lf_node IS INITIAL.
          lv_name = lf_node->get_name( ).
          lv_value = lf_node->get_value( ).
          CASE lv_name.
            WHEN 'Detalle'.
              ADD 1 TO lv_tabix.
              CLEAR: lt_xmltb.
              me->get_nodo_xml_to_tab( EXPORTING node = lf_node
                                       IMPORTING t_return = lt_return
                                                 t_xmltb  = lt_xmltb ).
              LOOP AT lt_xmltb INTO gs_xml.
                READ TABLE lt_dtrecfactmap INTO ls_dtrecfactmap
                                           WITH KEY xml_property = gs_xml-cname.
                IF sy-subrc EQ 0.
                  ASSIGN COMPONENT ls_dtrecfactmap-str_property
                                   OF STRUCTURE <ls_struct_dt> TO <ld_fld>.
                  <ld_fld> = gs_xml-cvalue.
                ENDIF.
              ENDLOOP.

              IF sy-subrc        EQ 0
              AND <ls_struct_dt> IS ASSIGNED.
                gs_dtrecfact = <ls_struct_dt>.
                MOVE-CORRESPONDING gs_dtrecfact TO gs_dtrecfact_id.
                gs_dtrecfact_id-tabix = lv_tabix.
                APPEND gs_dtrecfact    TO gt_dtrecfact.    CLEAR: gs_dtrecfact.
                APPEND gs_dtrecfact_id TO gt_dtrecfact_id. CLEAR: gs_dtrecfact.
              ENDIF.
            WHEN 'CdgItem'.
              CLEAR: lt_xmltb.
              me->get_nodo_xml_to_tab( EXPORTING node = lf_node
                                       IMPORTING t_return = lt_return
                                                 t_xmltb  = lt_xmltb ).

              LOOP AT lt_xmltb INTO gs_xml.
                READ TABLE lt_dtrecfactmap2 INTO ls_dtrecfactmap2
                                            WITH KEY xml_property = gs_xml-cname.
                IF sy-subrc EQ 0.
                  ASSIGN COMPONENT ls_dtrecfactmap2-str_property
                                   OF STRUCTURE <ls_struct_dt2> TO <ld_fld>.
                  <ld_fld> = gs_xml-cvalue.
                ENDIF.
              ENDLOOP.

              IF  sy-subrc        EQ 0
              AND <ls_struct_dt2> IS ASSIGNED.
                gs_dtrecfact2 = <ls_struct_dt2>.
                MOVE-CORRESPONDING gs_dtrecfact2 TO gs_dtrecfact2_id.
                gs_dtrecfact2_id-tabix = lv_tabix.
                APPEND gs_dtrecfact2 TO gt_dtrecfact2.
                CLEAR: gs_dtrecfact2.
                APPEND gs_dtrecfact2_id TO gt_dtrecfact2_id.
                CLEAR: gs_dtrecfact2_id.
              ENDIF.
            WHEN 'Referencia'.
              CLEAR: lt_xmltb.
              me->get_nodo_xml_to_tab( EXPORTING node = lf_node
                                       IMPORTING t_return = lt_return
                                                 t_xmltb  = lt_xmltb ).

              LOOP AT lt_xmltb INTO ls_xml.
                READ TABLE lt_rfrecfactmap INTO ls_rfrecfactmap WITH KEY xml_property = ls_xml-cname.
                IF sy-subrc EQ 0.
                  ASSIGN COMPONENT ls_rfrecfactmap-str_property OF STRUCTURE <ls_struct_rf> TO <ld_fld>.
                  <ld_fld> = ls_xml-cvalue.
                ENDIF.
              ENDLOOP.

              IF  sy-subrc EQ 0
              AND <ls_struct_rf> IS ASSIGNED.
                gs_rfrecfact = <ls_struct_rf>.
                APPEND gs_rfrecfact TO gt_rfrecfact. CLEAR: gs_rfrecfact.
              ENDIF.
            WHEN 'ImptoReten'.
              CLEAR: lt_xmltb.
              me->get_nodo_xml_to_tab( EXPORTING node = lf_node
                                       IMPORTING t_return = lt_return
                                                 t_xmltb  = lt_xmltb ).

              LOOP AT lt_xmltb INTO ls_xml.
                READ TABLE lt_imrecfactmap INTO ls_imrecfactmap WITH KEY xml_property = ls_xml-cname.
                IF sy-subrc EQ 0.
                  ASSIGN COMPONENT ls_imrecfactmap-str_property OF STRUCTURE <ls_struct_im> TO <ld_fld>.
                  <ld_fld> = ls_xml-cvalue.
                ENDIF.
              ENDLOOP.

              IF  sy-subrc       EQ 0
              AND <ls_struct_im> IS ASSIGNED.
                gs_imrecfact = <ls_struct_im>.
                APPEND gs_imrecfact TO gt_imrecfact. CLEAR: gs_imrecfact.
              ENDIF.
            WHEN 'TED'.
              CLEAR: lt_xmltb.
              me->get_nodo_xml_to_tab( EXPORTING node = lf_node
                                       IMPORTING t_return = lt_return
                                                 t_xmltb  = lt_xmltb ).

              LOOP AT lt_xmltb INTO ls_xml.
                READ TABLE lt_tdrecfactmap INTO ls_tdrecfactmap WITH KEY xml_property = ls_xml-cname.
                IF sy-subrc EQ 0.
                  ASSIGN COMPONENT ls_tdrecfactmap-str_property OF STRUCTURE <ls_struct_td> TO <ld_fld>.
                  <ld_fld> = ls_xml-cvalue.
                ENDIF.
              ENDLOOP.

              IF  sy-subrc       EQ 0
              AND <ls_struct_td> IS ASSIGNED.
                gs_tdrecfact = <ls_struct_td>.
              ENDIF.
            WHEN OTHERS.
          ENDCASE.
          lf_node = lf_iterator->get_next( ).
        ENDWHILE.
      ENDIF.
    ENDIF.

    "DO CUSTOMIZED MAPPING FOR FILE NAME
    IF filename IS NOT INITIAL.
      CLEAR: gs_filename.
      gs_filename-filename = filename.
    ENDIF.

  ENDMETHOD.


  METHOD mapping_to_struc_2.

    DATA: wa_parametro TYPE ztfi_0078c,
          lt_parametro TYPE STANDARD TABLE OF ztfi_0078c,
          lt_tipo      TYPE STANDARD TABLE OF  ztfi_0078d,
          wa_tipo      TYPE  ztfi_0078d,
          wa_nodos     TYPE zefi_0028,
          lt_nodos     TYPE STANDARD TABLE OF zefi_0028,
          lv_tabix     TYPE sy-tabix,
          lf_node      TYPE REF TO if_ixml_node,
          ls_root      TYPE REF TO if_ixml_element,
          lf_iterator  TYPE REF TO if_ixml_node_iterator,
          lf_filter    TYPE REF TO if_ixml_node_filter,
          ls_stream    TYPE string,
          lx_stream    TYPE xstring,
          lx_ant       TYPE xstring,
          ls_name      TYPE string.
    FIELD-SYMBOLS: <tab1> TYPE STANDARD TABLE,
                   <wa1>  TYPE any.

    "---> Inicializa atributos.
    SELECT * FROM ztfi_0078c INTO TABLE  lt_parametro
             WHERE vatint = gs_varint. "'CL'.
    IF sy-subrc = 0.
      LOOP AT  lt_parametro INTO wa_parametro.
        ASSIGN (wa_parametro-atributo)   TO   <tab1>.
        ASSIGN (wa_parametro-waatributo) TO   <wa1>.
        IF  <tab1> IS ASSIGNED.
          REFRESH  <tab1>. CLEAR <tab1>.
        ENDIF.
        IF  <wa1> IS ASSIGNED.
          CLEAR  <wa1>.
        ENDIF.

      ENDLOOP.
      IF  <tab1> IS ASSIGNED.
        UNASSIGN <tab1>.
      ENDIF.
      IF  <wa1> IS ASSIGNED.
        UNASSIGN  <wa1>.
      ENDIF.

    ENDIF.


    "---> tipo de Nodos
    SELECT * FROM ztfi_0078d INTO  TABLE  lt_tipo
    WHERE vatint EQ gs_varint. "'CL'.

    "---> Solo nodos principales
    LOOP AT lt_tipo INTO wa_tipo.
      SELECT * FROM ztfi_0078c INTO TABLE  lt_parametro
       WHERE vatint = wa_tipo-vatint
        AND  tipo EQ wa_tipo-tipo
        AND  nodop = ' '.


      MOVE-CORRESPONDING wa_tipo TO wa_nodos.
      wa_nodos-parametro[] = lt_parametro[].
      APPEND wa_nodos TO lt_nodos.

    ENDLOOP.

    CLEAR wa_parametro. REFRESH lt_parametro. CLEAR ls_nameroot.
    SORT lt_nodos BY vatint sec.

    go_xml->set_encoding( charset = 'utf-8' ).
    go_xml->render_2_xstring( IMPORTING stream = lx_stream ).


    IF lx_stream IS NOT INITIAL.
      lx_ant =  lx_stream.
      CALL TRANSFORMATION z_dteelimns "Elimino NameSpaces generan error en if_ixml_node
       SOURCE XML lx_stream
       RESULT XML lx_stream.
      IF go_xml->parse_xstring( lx_stream ) EQ 0.
      ENDIF.
    ENDIF.

    IF go_xml->parse_xstring( lx_stream ) EQ 0.
      ls_root = go_xml->m_document->get_root_element( ).
      ls_nameroot = ls_root->get_name( ).
      CLEAR: lv_tabix.
      LOOP AT lt_nodos INTO wa_nodos.

        CLEAR wa_parametro.
        IF lines( wa_nodos-parametro ) EQ 1.
          READ TABLE  wa_nodos-parametro INDEX '1' INTO wa_parametro.
          CLEAR ls_name.
          ls_name =  wa_parametro-nodoruta.
          IF  ls_name = '.' OR ls_name IS INITIAL.
            lf_node = go_xml->m_document.
          ELSE.
            IF ls_name CA go_xml->c_path_sep. "Si se va por Path colocar concatenar el root.
              CONCATENATE '//' ls_nameroot ls_name INTO ls_name.
              CONDENSE ls_name NO-GAPS.
            ENDIF.
            lf_node = go_xml->find_node( name = ls_name ).
          ENDIF.
        ELSE.
          CLEAR lf_node. "Va inicial, el nodo se determina por cada  Parametro
        ENDIF.

        me->mappingxml_to_struch( EXPORTING nodo = lf_node parametro =  wa_nodos-parametro[]  ls_tabix = lv_tabix ).

      ENDLOOP.
      IF filename IS NOT INITIAL.
        CLEAR: gs_filename.
        gs_filename-filename = filename.
      ENDIF.

    ENDIF.
    IF lx_ant IS NOT INITIAL.
      IF go_xml->parse_xstring( lx_ant ) EQ 0.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD move_file.

    DATA: lv_directory    TYPE user_dir-dirname,
          lv_tipo_dte_str TYPE string,
          lv_folio_str    TYPE string,
          lv_file_path    TYPE string,
          arch_string     TYPE string,
          extens(4)       TYPE c,
          ls_solix        TYPE x255.

    SELECT SINGLE dirname FROM user_dir
            INTO lv_directory
            WHERE aliass = zpath.
    IF sy-subrc NE 0.
      gs_bal_msg-msgty = 'E'.
      gs_bal_msg-msgid = 'ZDTE_0001'.
      gs_bal_msg-msgno = '040'.
      gs_bal_msg-msgv1 = ''.
      go_log->add_msg( i_s_msg = gs_bal_msg ).
      go_log->save( ).

    ELSE.

      lv_tipo_dte_str = i_registro-tipo_doc.
      lv_folio_str = i_registro-folio_doc.

      IF type_file = 'XML'.
        arch_string = xml.
        extens = '.XML'.
      ELSE.
        extens = '.PDF'.
      ENDIF.

      IF type_file = 'XML'.
        CONCATENATE lv_directory '/' i_registro-rutt_emis
    i_rutempresa lv_tipo_dte_str lv_folio_str extens
    INTO lv_file_path.
        CONDENSE lv_file_path NO-GAPS.

        OPEN DATASET lv_file_path FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
        IF sy-subrc = 0.
          TRANSFER arch_string TO lv_file_path.
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

      ELSE.

        CONCATENATE lv_directory '\' i_registro-rutt_emis
        i_rutempresa lv_tipo_dte_str lv_folio_str extens
        INTO lv_file_path.

        CONDENSE lv_file_path NO-GAPS.
        OPEN DATASET lv_file_path FOR OUTPUT IN BINARY MODE.
        IF sy-subrc = 0.
          LOOP AT e_tab INTO ls_solix.
            TRANSFER ls_solix TO lv_file_path.
          ENDLOOP.
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
      ENDIF.
    ENDIF.
    estado = sy-subrc.

  ENDMETHOD.


  METHOD move_filexml.
    DATA: ls_stream      TYPE string,
          lv_file_path   TYPE string,
          lv_file_path_o TYPE string,
          lv_err_string  TYPE string,
          lo_cx_root     TYPE REF TO cx_root,
          lo_err         TYPE REF TO cx_sy_file_open_mode.

    go_xml->render_2_string( IMPORTING stream = ls_stream ).

    CLEAR lv_file_path. CLEAR lv_file_path_o.

    CONCATENATE file_d nomb_archivo  INTO lv_file_path.
    CONDENSE lv_file_path NO-GAPS.

    CONCATENATE file_o nomb_archivo  INTO lv_file_path_o.
    CONDENSE lv_file_path_o NO-GAPS.
    TRY.
        OPEN DATASET lv_file_path FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
        IF sy-subrc = 0.
          TRANSFER ls_stream TO lv_file_path.
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
        EXIT.
    ENDTRY.
    IF sy-subrc = 0. "Si puede moverlo, luego lo borro.
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
          EXIT.
      ENDTRY.

    ENDIF.
  ENDMETHOD.


  METHOD mov_archivoserror.
    DATA: ls_stream      TYPE string,
          lv_file_path   TYPE string,
          lv_file_path_o TYPE string,
          lv_err_string  TYPE string,
          lo_err         TYPE REF TO cx_sy_file_open_mode.

    go_xml->render_2_string( IMPORTING stream = ls_stream ).

    CLEAR lv_file_path. CLEAR lv_file_path_o.

    CONCATENATE file_d nomb_archivo  INTO lv_file_path.
    CONDENSE lv_file_path NO-GAPS.

    CONCATENATE file_o nomb_archivo  INTO lv_file_path_o.
    CONDENSE lv_file_path_o NO-GAPS.
    TRY.
        OPEN DATASET lv_file_path FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
        IF sy-subrc = 0.
          TRANSFER ls_stream TO lv_file_path.
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
    IF sy-subrc = 0. "Si puede moverlo, luego lo borro.
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


  METHOD nombre_pdf.
    CONCATENATE gs_recfact-rutemisor
                   gs_recfact-rutrecep
                   gs_recfact-tipodte
                   gs_recfact-folio
*                  gs_recfact-fchemis
                   '.pdf' INTO name.
  ENDMETHOD.


  METHOD nombre_sfp.
    DATA: lf_node      TYPE REF TO if_ixml_node,
          lc_doc       TYPE REF TO if_ixml_document,
          lf_element   TYPE REF TO if_ixml_element,
          lx_xml       TYPE xstring,
          ls_value     TYPE string,
          xml_result   TYPE xstring,
          w_result     TYPE string,
          xml_table    TYPE STANDARD TABLE OF smum_xmltb,
          ls_xmltable  TYPE  smum_xmltb,
          return       TYPE STANDARD TABLE OF  bapiret2,
          ls_vatint    TYPE  ztfi_0078b-vatint,
          lc_campo     TYPE ztfi_0078b-nombre_int.

* nombre = co_fpname.
    go_xml->render_2_xstring( IMPORTING stream = lx_xml ).
    CALL FUNCTION 'SMUM_XML_PARSE'
      EXPORTING
        xml_input = lx_xml
      TABLES
        xml_table = xml_table
        return    = return.
    CLEAR ls_value.
*Busca el nombre tipo dte
**//.. leer tabla configuración
    CLEAR lc_campo. CLEAR ls_vatint.
    IF gs_varint IS INITIAL.
      me->variante_interfaz( i_bukrs = i_bukrs  ).
    ENDIF.
    IF  gs_varint IS INITIAL.
      nombre =  co_fpname. "Coloco por defecto
      "---> Logs de interfaz.
      CLEAR gs_bal_msg.
      gs_bal_msg-msgty = 'E'.
      gs_bal_msg-msgid = 'ZDTE_0001'.
      gs_bal_msg-msgno = '012'.
      gs_bal_msg-msgv1 = 'Sociedad no configurada Variante de Integracion '.
      go_log->add_msg( i_s_msg = gs_bal_msg ).
      go_log->save( ).
    ELSE.
      CLEAR lc_campo.
      SELECT nombre_int INTO lc_campo FROM ztfi_0078b UP TO 1 ROWS
      WHERE vatint  EQ gs_varint
      AND    codint EQ '0008'
      ORDER BY PRIMARY KEY.
      ENDSELECT.
      LOOP AT xml_table INTO ls_xmltable WHERE  cname = lc_campo.
        ls_value = ls_xmltable-cvalue.
      ENDLOOP.
      CLEAR nombre.
      SELECT fname  INTO nombre  FROM ztfi_0078a UP TO 1 ROWS
      WHERE vatint  EQ gs_varint
      AND   tipodte EQ ls_value
      ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF nombre IS INITIAL.
        nombre =  co_fpname.
      ENDIF.

*      IF ls_value = 'P'.
*        nombre = 'ZEDOC_MX_PMNT_FORM'.
*      ELSE.
*        nombre = 'ZEDOC_MX_INV_FORM'. "co_fpname.
*      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD parse_xml_to_tab_v2.
    DATA: ls_xml TYPE string,
          lx_xml TYPE xstring.

    DATA: ls_output TYPE smum_xmltb,
          lt_output TYPE STANDARD TABLE OF smum_xmltb,
          ls_return TYPE bapiret2,
          lt_return TYPE bapiret2_t.

    FREE: me->gt_xml, me->gs_xml.
**********************************************************************
    " Edwar Soto Novis 11.09.2018.
    " Reparar error Dump.
    IF me->go_log IS NOT BOUND.
      "---> Crea Objeto Logs de interfaz.
      CLEAR gs_bal_log.
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

    ENDIF.
**********************************************************************
    go_xml->render_2_string( IMPORTING stream = ls_xml ).

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = ls_xml
      IMPORTING
        buffer = lx_xml
      EXCEPTIONS
        failed = 1
        OTHERS = 2.
    IF sy-subrc <> 0.
*     MESSAGE 'Error in the XML file' TYPE 'E'.
*    MESSAGE 'Error in the XML file' TYPE 'W' DISPLAY LIKE 'E'.
      "Agregar Log
      CLEAR gs_bal_msg.
      gs_bal_msg-msgty = 'E'.
      gs_bal_msg-msgid = 'ZDTE_0001'.
      gs_bal_msg-msgno = '028'.
      gs_bal_msg-msgv1 = filename.
      go_log->add_msg( i_s_msg = gs_bal_msg ).
      "Al finalizar cada registro guardo el log.
      go_log->save( ).
    ELSE.
      CLEAR gt_sxml.CLEAR gt_ssxml.
*    gt_sxml = lx_xml.
      gt_ssxml = ls_xml.
      REPLACE 'encoding="utf-16"' WITH 'encoding="utf-8"' INTO gt_ssxml.

      CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
        EXPORTING
          text   = gt_ssxml
*         ENCODING = 4110
        IMPORTING
          buffer = gt_sxml
        EXCEPTIONS
          failed = 1
          OTHERS = 2.



* This function module is used to parse the XML and get the
* data in the form of a table
      CALL FUNCTION 'SMUM_XML_PARSE'
        EXPORTING
          xml_input = lx_xml
        TABLES
          xml_table = lt_output
          return    = lt_return
        EXCEPTIONS
          OTHERS    = 0.
*"If XML parsing is not successful, return table will contain error messages
      READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
      IF sy-subrc EQ 0.
*     MESSAGE 'Error converting the input XML file' TYPE 'E'.
*      MESSAGE 'Error converting the input XML file' TYPE 'W' DISPLAY LIKE 'E'.
        "Agregar Log
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '029'.
        gs_bal_msg-msgv1 = filename.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        "Al finalizar cada registro guardo el log.
        go_log->save( ).
      ELSE.
        REFRESH lt_return.
      ENDIF.

      IF lt_output IS NOT INITIAL.
        gt_xml = lt_output.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD save_cuadratura.
    DATA: ls_fi0074fr    TYPE ztfi_0074fr,
          lt_ztfi_0074fr TYPE TABLE OF ztfi_0074fr.

    IF gt_fi0074fr[] IS NOT INITIAL. "Si tengo facturas

    SELECT * FROM ztfi_0074fr INTO TABLE lt_ztfi_0074fr
    FOR ALL ENTRIES IN gt_fi0074fr
         WHERE bukrs EQ gt_fi0074fr-bukrs
           AND xblnr EQ  gt_fi0074fr-xblnr
           AND tipodte EQ  gt_fi0074fr-tipodte
           AND stcd1 EQ  gt_fi0074fr-stcd1.

      IF lt_ztfi_0074fr[] IS NOT INITIAL.
        "----> elimino los registro que ya tiene fechaSII y estatus X
        LOOP AT lt_ztfi_0074fr ASSIGNING FIELD-SYMBOL(<f1>).
          READ TABLE gt_fi0074fr ASSIGNING FIELD-SYMBOL(<f2>) WITH KEY bukrs = <f1>-bukrs
                                     xblnr = <f1>-xblnr
                                     tipodte = <f1>-tipodte
                                     stcd1   = <f1>-stcd1.
          IF sy-subrc = 0.
            IF <f1>-estatus = 'X'.
              DELETE gt_fi0074fr WHERE bukrs = <f1>-bukrs
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
      IF gt_fi0074fr[] IS NOT INITIAL.
        MODIFY ztfi_0074fr FROM TABLE gt_fi0074fr.
        COMMIT WORK AND WAIT.
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'S'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '065'.
        gs_bal_msg-msgv1 = p_bukrs.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).

      ELSE.
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'S'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '064'.
        gs_bal_msg-msgv1 = p_bukrs.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD save_datesii.

    DATA: wa_0074fr TYPE ztfi_0074fr.
    DATA: lv_fecha(20) TYPE c.
    DATA: lv_fecha2(10) TYPE c.
    DATA: lv_fechaf(10) TYPE c.
    DATA: lv_hora(8) TYPE c.
    DATA: lv_horas(6) TYPE c.
    DATA: lv_doc(50) TYPE c.
    DATA: lv_folio_dte(10) TYPE c.
    DATA: lv_dv(1) TYPE c.
    DATA: lv_number_part(8) TYPE c.
    DATA: lv_tipo_dte(3) TYPE c.
    DATA: lv_toyear  TYPE inri-toyear.
    DATA: lv_err_string TYPE string.
**//.. OJO I_REGISTRO
**  CLEAR wa_0074fr. CLEAR lv_tipo_dte.
**  CLEAR lv_folio_dte.CLEAR lv_doc.CLEAR lv_dv.CLEAR lv_number_part.
**
**  WRITE: i_registro-fecha_recepcion_sii TO lv_fecha.
**
**  lv_fecha2 = lv_fecha(10).
**  CONCATENATE lv_fecha(4) lv_fecha+5(2) lv_fecha+8(2) INTO lv_fechaf.
**  lv_hora = lv_fecha+11(8).
**  CONCATENATE lv_hora(2) lv_hora+3(2) lv_hora+6(2) INTO lv_horas.
**  CONDENSE lv_horas NO-GAPS.
**
*** wa_0074fr-filename = nombrexml.
**  wa_0074fr-bukrs = i_bukrs01.
**  wa_0074fr-xblnr = i_registro-folio.
**  CONDENSE wa_0074fr-xblnr NO-GAPS.
**  wa_0074fr-tipodte = i_registro-tipo.
**
**  wa_0074fr-stcd1 = i_registro-rut_emisor.
**  wa_0074fr-fecharsii = lv_fechaf.
**  wa_0074fr-horarsii = lv_horas.
***  wa_0074fr-estatus = i_registro-estado_sii.
***  wa_0074fr-bldat = i_registro-fecha_emision.
***  wa_0074fr-rznsoc = i_registro-razon_social_emisor.
***  wa_0074fr-wrbtr = i_registro-monto_total.
***  wa_0074fr-waers = 'CLP'.
***  wa_0074fr-iva =  i_registro-monto_iva.
***  wa_0074fr-estatusxml = i_registro-estado_recepcion.
***  gs_estarepxml =  i_registro-estado_recepcion.
**
**  SELECT SINGLE lfa1~lifnr "lfa1~name1
**  INTO (wa_0074fr-lifnr) ",  wa_0074fr-name1)
**  FROM lfa1 INNER JOIN lfb1 ON lfa1~lifnr = lfb1~lifnr
**   WHERE lfb1~bukrs EQ i_bukrs01
**   AND   lfa1~stcd1 EQ i_registro-rut_emisor "wa_0074fr-stcd1
**   AND   lfa1~sperr EQ space
**   AND   lfa1~sperq EQ space
**   AND   lfb1~sperr EQ space.
**
**
**  MODIFY ztfi_0074fr FROM wa_0074fr.
**  code_return = sy-subrc.
**  IF sy-subrc NE 0.
**    gs_bal_msg-msgty = 'E'.
**    gs_bal_msg-msgid = 'ZDTE_0001'.
**    gs_bal_msg-msgno = '043'.
**    lv_folio_dte = i_registro-folio.
**    lv_tipo_dte = i_registro-tipo.
**    CONCATENATE lv_tipo_dte '/' lv_folio_dte '/rut:'
**    i_registro-rut_emisor INTO lv_doc.
**    CONDENSE lv_doc NO-GAPS.
**
**    gs_bal_msg-msgv1 = lv_doc.
**    go_log->add_msg( i_s_msg = gs_bal_msg ).
**    go_log->save( ).
**  ELSE.
**    CLEAR gs_bal_msg.
**    gs_bal_msg-msgty = 'S'.
**    gs_bal_msg-msgid = 'ZDTE_0001'.
**    gs_bal_msg-msgno = '048'.
**    lv_folio_dte = i_registro-folio.
**    lv_tipo_dte = i_registro-tipo.
**    CONCATENATE lv_tipo_dte '/' lv_folio_dte '/rut:'
**    i_registro-rut_emisor INTO lv_doc.
**    CONDENSE lv_doc NO-GAPS.
**    gs_bal_msg-msgv1 = lv_doc.
**    go_log->add_msg( i_s_msg = gs_bal_msg ).
**    go_log->save( ).
**  ENDIF.
  ENDMETHOD.


  METHOD save_lc.
    DATA: ls_fi0074fr    TYPE ztfi_0074fr,
          lt_ztfi_0074fr TYPE TABLE OF ztfi_0074fr.

    IF gt_fi0074fr[] IS NOT INITIAL. "Si tengo facturas

*-->Busco las facturas que ya tienen registro fechaaii y LC
*   De las seleccionadas desde SII.
      SELECT * FROM ztfi_0074fr INTO TABLE lt_ztfi_0074fr
      FOR ALL ENTRIES IN gt_fi0074fr
           WHERE bukrs EQ gt_fi0074fr-bukrs
             AND xblnr EQ  gt_fi0074fr-xblnr
             AND tipodte EQ  gt_fi0074fr-tipodte
             AND stcd1 EQ  gt_fi0074fr-stcd1.
*            AND estatuslc EQ 'X' "Si se activa el llenado del estatus LC
*             AND estatus EQ 'X'.


      IF lt_ztfi_0074fr[] IS NOT INITIAL.
        "----> elimino los registro que ya tiene fechaSII y estatus X y estatuslC X
        LOOP AT lt_ztfi_0074fr ASSIGNING FIELD-SYMBOL(<f1>).
          READ TABLE gt_fi0074fr ASSIGNING FIELD-SYMBOL(<f2>)  WITH KEY bukrs = <f1>-bukrs
                                     xblnr = <f1>-xblnr
                                     tipodte = <f1>-tipodte
                                     stcd1   = <f1>-stcd1.
          IF sy-subrc = 0.
            IF <f1>-estatus EQ 'X' AND <f1>-estatuslc  EQ 'X'.
              DELETE gt_fi0074fr WHERE bukrs = <f1>-bukrs
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
      IF gt_fi0074fr[] IS NOT INITIAL.
        MODIFY ztfi_0074fr FROM TABLE gt_fi0074fr.
        COMMIT WORK AND WAIT.
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'S'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '065'.
        gs_bal_msg-msgv1 = p_bukrs.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).

      ELSE.
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'S'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '064'.
        gs_bal_msg-msgv1 = p_bukrs.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD save_pdf_form.
    DATA: ls_ztfi_0078  TYPE ztfi_0078,
          ls_opsystem   TYPE opsystem,
          lv_sep        TYPE char01,
          lv_iso        TYPE cpcodepage,
          lv_err_string TYPE string,
          lv_dir        TYPE pfeflnamel,
          lv_fname      TYPE eps2filnam,
          lv_file       TYPE pfeflnamel,
          lv_pdf_strg   TYPE string,
          lv_pdf_xstrg  TYPE xstring,
          lv_msg        TYPE string,
          lt_dir_list   TYPE STANDARD TABLE OF eps2fili,
          lo_err        TYPE REF TO cx_sy_file_open_mode,
          lo_cx_root    TYPE REF TO cx_root.

**//..
    CHECK i_bukrs IS NOT INITIAL.

**//..
    SELECT SINGLE * INTO ls_opsystem
    FROM  opsystem
    WHERE opsys EQ sy-opsys.
    IF ls_opsystem-filesys EQ 'UNIX'.
      lv_sep = '/'.
    ELSE.
      lv_sep = '\'.
    ENDIF.

    "---> Crea Objeto Logs de interfaz.
    IF go_log IS NOT BOUND.
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
    ENDIF.

**//.. Leer tabla configuración
    SELECT SINGLE * INTO ls_ztfi_0078
    FROM ztfi_0078
    WHERE bukrs EQ i_bukrs.
    IF sy-subrc NE 0.
      "---> Logs de interfaz.
      CLEAR gs_bal_msg.
      gs_bal_msg-msgty = 'E'.
      gs_bal_msg-msgid = 'ZDTE_0001'.
      gs_bal_msg-msgno = '012'.
      gs_bal_msg-msgv1 = lv_err_string.
      go_log->add_msg( i_s_msg = gs_bal_msg ).
      go_log->save( ).
      EXIT.
    ELSE.
      CHECK ls_ztfi_0078-genepdf EQ abap_true.

**//.. Generar PDF
      CLEAR: lv_pdf_strg.

      CALL METHOD me->load_pdf_form
        EXPORTING
          i_bukrs       = i_bukrs
        IMPORTING
          e_pdf_xstring = lv_pdf_xstrg.

      CHECK lv_pdf_xstrg IS NOT INITIAL.

**//.. Buscar direcctorio
      SELECT SINGLE dirname INTO lv_dir
      FROM user_dir
        WHERE aliass = ls_ztfi_0078-direcpdf.
      IF sy-subrc NE 0.
        "---> Logs de interfaz.
        CLEAR gs_bal_msg.
        gs_bal_msg-msgty = 'E'.
        gs_bal_msg-msgid = 'ZDTE_0001'.
        gs_bal_msg-msgno = '026'.
        gs_bal_msg-msgv1 = ls_ztfi_0078-direcpdf.
        go_log->add_msg( i_s_msg = gs_bal_msg ).
        go_log->save( ).
        EXIT.
      ENDIF.

**//.. Nombre del archivo PDF
      me->nombre_pdf( IMPORTING name = lv_fname ).

*      CONCATENATE gs_recfact-rutemisor
*                  gs_recfact-rutrecep
*                  gs_recfact-tipodte
*                  gs_recfact-folio
*                  '.pdf'
*                  INTO lv_fname.
      CONCATENATE lv_dir lv_fname INTO lv_file SEPARATED BY '\'.

**//..
      TRY .
          OPEN DATASET lv_file FOR OUTPUT MESSAGE lv_msg
                       IN BINARY MODE.
          IF sy-subrc EQ 0.
            TRANSFER lv_pdf_xstrg TO lv_file.
            CLOSE DATASET lv_file.
          ELSE.
            CLEAR gs_bal_msg.
            gs_bal_msg-msgty = 'E'.
            gs_bal_msg-msgid = 'ZDTE_0001'.
            gs_bal_msg-msgno = '045'.
            gs_bal_msg-msgv1 = lv_msg.
            gs_bal_msg-msgv1 = lv_fname.
            go_log->add_msg( i_s_msg = gs_bal_msg ).
            go_log->save( ).
            EXIT.
          ENDIF.
        CATCH cx_sy_file_open               INTO lo_cx_root.
          lv_err_string = lo_cx_root->get_text( ).
          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'E'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '012'.
          gs_bal_msg-msgv1 = lv_err_string.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
          EXIT.
        CATCH cx_sy_codepage_converter_init INTO lo_cx_root.
          lv_err_string = lo_cx_root->get_text( ).
          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'E'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '012'.
          gs_bal_msg-msgv1 = lv_err_string.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
          EXIT.
        CATCH cx_sy_conversion_codepage     INTO lo_cx_root.
          lv_err_string = lo_cx_root->get_text( ).
          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'E'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '012'.
          gs_bal_msg-msgv1 = lv_err_string.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
          EXIT.
        CATCH cx_sy_file_authority          INTO lo_cx_root.
          lv_err_string = lo_cx_root->get_text( ).
          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'E'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '012'.
          gs_bal_msg-msgv1 = lv_err_string.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
          EXIT.
        CATCH cx_sy_pipes_not_supported     INTO lo_cx_root.
          lv_err_string = lo_cx_root->get_text( ).
          CLEAR gs_bal_msg.
          gs_bal_msg-msgty = 'E'.
          gs_bal_msg-msgid = 'ZDTE_0001'.
          gs_bal_msg-msgno = '012'.
          gs_bal_msg-msgv1 = lv_err_string.
          go_log->add_msg( i_s_msg = gs_bal_msg ).
          go_log->save( ).
          EXIT.
        CATCH cx_sy_too_many_files          INTO lo_cx_root.
          lv_err_string = lo_cx_root->get_text( ).
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


  METHOD variante_interfaz.
    IF i_bukrs IS INITIAL.
      "---> Busco variante de integracion
      SELECT SINGLE vatint INTO gs_varint FROM ztfi_0078
         WHERE bukrs IN gt_bukrs.

      IF gs_varint IS INITIAL.
        gs_varint = 'CL'.
      ENDIF.
    ELSE.
      "---> Busco variante de integracion
      SELECT SINGLE vatint INTO gs_varint FROM ztfi_0078
         WHERE bukrs EQ i_bukrs.

      IF gs_varint IS INITIAL.
        gs_varint = 'CL'.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD ws_autdbnet.
    DATA: ws_header TYPE REF TO if_wsprotocol_ws_header.

* get ws_header protocol
**//.. OJO
**  IF nom_ws = 'DTE'.
**    ws_header ?= go_dterecep->get_protocol('IF_WSPROTOCOL_WS_HEADER').
**  ELSE.
**    ws_header ?= go_obtiene_pdf->get_protocol('IF_WSPROTOCOL_WS_HEADER').
**  ENDIF.
***************************************************************************
* set somehow header as iXML-DOM tree
    DATA: ixml         TYPE REF TO if_ixml,
          xml_document TYPE REF TO if_ixml_document,
          xml_root     TYPE REF TO if_ixml_element,
          xml_element  TYPE REF TO if_ixml_element,
          xml_node     TYPE REF TO if_ixml_node,
          name         TYPE string,
          namespace    TYPE string.
    DATA l_xstring TYPE xstring.
    DATA l_string TYPE string.
    DATA fecha_c(24) TYPE c.
    DATA hora_c(10) TYPE c.

    CLEAR fecha_c. CLEAR hora_c.

    CONCATENATE sy-datum(4) '-' sy-datum+4(2) '-'  sy-datum+6(2) INTO fecha_c.
    CONDENSE fecha_c NO-GAPS.
    WRITE: sy-uzeit TO hora_c USING EDIT MASK 'T__:__:__Z'.
    CONDENSE hora_c NO-GAPS.
    CONCATENATE fecha_c hora_c INTO fecha_c.
    CONDENSE fecha_c NO-GAPS.

    CONCATENATE
  '<soapenv:Header>'
  '<wsse:Security soapenv:mustUnderstand="1" xmlns:wsse="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-secext-1.0.xsd"'
  'xmlns:wsu="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-wssecurity-utility-1.0.xsd"'
  'xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">'
  '<wsse:UsernameToken>'
  '<wsse:Username>' usuario '</wsse:Username>'
  '<wsse:Password Type="http://docs.oasis-open.org/wss/2004/01/oasis-200401-wss-username-token-profile-'
  '1.0#PasswordText">' password '</wsse:Password>'
  '<wsse:Nonce>HuoTyp2613MK1BVOqIhF0g==</wsse:Nonce>'
  '<wsu:Created>' fecha_c '</wsu:Created>'
  '</wsse:UsernameToken>'
  '</wsse:Security>'
  '</soapenv:Header>'
      INTO l_string.
* convert to xstring
    l_xstring = cl_proxy_service=>cstring2xstring( l_string ).
    IF NOT l_string IS INITIAL.
* create ixml dom document from xml xstring
      CALL FUNCTION 'SDIXML_XML_TO_DOM'
        EXPORTING
          xml           = l_xstring
        IMPORTING
          document      = xml_document
        EXCEPTIONS
          invalid_input = 1
          OTHERS        = 2.
      IF sy-subrc = 0 AND NOT xml_document IS INITIAL.
        xml_root = xml_document->get_root_element( ).
        xml_element ?= xml_root->get_first_child( ).
* add header element by element to soap header
        WHILE NOT xml_element IS INITIAL.
          name = xml_element->get_name( ).
          namespace = xml_element->get_namespace_uri( ).
          ws_header->set_request_header( name = name namespace = namespace dom = xml_element ).
          xml_element ?= xml_element->get_next( ).
        ENDWHILE.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "WS_AUTDBNET


  METHOD ws_import_documents.
    .

*  DATA: wa_input  TYPE zwssupplier_tras_etdsoap_in.
*  DATA: wa_output TYPE zwssupplier_tras_etdsoap_out.
*
*  DATA: wa_registro TYPE zwsmensaje.
*  DATA: lv_mail(30) TYPE c.
*  DATA: lv_rutempresa(12) TYPE c.
*  DATA: lv_rutempresa_sdv(12) TYPE c.
*  DATA: lv_err_string TYPE string.
*  DATA: lt_0088  TYPE STANDARD TABLE OF ztfi_0088.
*  DATA: wa_0088  TYPE ztfi_0088.
*  DATA: wa_0078 TYPE ztfi_0078.
*  DATA: lv_dv(1) TYPE c.
*  DATA: code_return TYPE sy-subrc.
*  DATA lv_largo TYPE i.
*  DATA: oref TYPE REF TO cx_ai_system_fault,
*        iref TYPE REF TO cx_ai_application_fault,
*        text TYPE string.
*  DATA: xml TYPE string.
*  DATA: pdf TYPE xstring.
*  DATA: lv_pdf TYPE xstring.
*  DATA: lv_estado TYPE sy-subrc.
*  DATA: tab_pdf TYPE ty_x255.
*  DATA: sw TYPE i.
*  "---> Crea Objeto Logs de interfaz.
*  CONCATENATE sy-datum sy-uzeit
*              INTO gs_bal_log-extnumber.
**   GS_BAL_LOG-EXTNUMBER  = 'BUKRSRUTEMISORRXBLNR'.
*  gs_bal_log-object     = 'ZDTE'.
*  gs_bal_log-subobject  = 'RECEPCION'.
*  gs_bal_log-aldate     = syst-datum.
*  gs_bal_log-altime     = syst-uzeit.
*  gs_bal_log-aluser     = syst-uname.
*  gs_bal_log-alprog     = syst-repid.
*  FREE go_log.
*  CREATE OBJECT go_log
*    EXPORTING
*      i_s_object = gs_bal_log.
*
*  TRY.
*      FREE go_dterecep.
*      CREATE OBJECT go_dterecep.
*    CATCH cx_ai_system_fault.
*  ENDTRY.
*
*  "---> Leer tabla de directorios
*  SELECT * FROM ztfi_0088 INTO CORRESPONDING FIELDS OF TABLE lt_0088
*  WHERE sysid EQ sy-sysid
*    AND bukrs IN gt_bukrs.
*  IF sy-subrc NE 0.
*    "---> Logs de interfaz.
*    CLEAR gs_bal_msg.
*    gs_bal_msg-msgty = 'E'.
*    gs_bal_msg-msgid = 'ZDTE_0001'.
*    gs_bal_msg-msgno = '012'.
*    gs_bal_msg-msgv1 = lv_err_string.
*    go_log->add_msg( i_s_msg = gs_bal_msg ).
*    go_log->save( ).
*    EXIT.
*  ENDIF.
*
**Busco sociedades
*  LOOP AT lt_0088 INTO wa_0088.
*    CLEAR wa_input. CLEAR wa_output.CLEAR text.CLEAR gs_bal_msg.
*    SELECT SINGLE paval INTO lv_rutempresa FROM t001z
*    WHERE bukrs = wa_0088-bukrs AND party = 'TAXNR'.
*
*
*
*    REPLACE ALL OCCURRENCES OF '.' IN  lv_rutempresa WITH space.
*    CONDENSE lv_rutempresa NO-GAPS.
*
*
*    SPLIT lv_rutempresa AT '-' INTO lv_rutempresa_sdv lv_dv.
*    wa_input-rut = lv_rutempresa_sdv.
*
*    SELECT SINGLE *  INTO
*        CORRESPONDING FIELDS OF wa_0078 FROM ztfi_0078
*    WHERE bukrs = wa_0088-bukrs.
*    sw = 0.
*    WHILE sw = 0. "Tiene Offset de documentos
**0. llamar a ProxyPI Obtiene XML
*      TRY.
*
*          CALL METHOD go_dterecep->obtiene_xml_out
*            EXPORTING
*              output = wa_input
*            IMPORTING
*              input  = wa_output.
*        CATCH cx_ai_system_fault INTO oref.
*          text = oref->get_text( ).
*        CATCH cx_ai_application_fault INTO iref.
*          text = iref->get_text( ).
*      ENDTRY.
*      "----> Operacion NOOK
*      IF   wa_output-supplier_tras_etdresult-codigo NE 'DOK'.
*
*        IF text IS NOT INITIAL.
*          CONCATENATE '(' wa_0088-bukrs ')-' text INTO text.
*        ELSE.
*          CONCATENATE '(' wa_0088-bukrs ')-' wa_output-supplier_tras_etdresult-mensajes INTO wa_output-supplier_tras_etdresult-mensajes.
*        ENDIF.
***********************************************************************
**   Edwar Soto / Novis / 11.09.2018
**        intenta conectarse a travez de WS y descarga de
**        la suite del custodio todos los archivos y los coloca en SAP
**        en carpetas en la AL11.
**        como no hay archivos manda mensaje de error de que no hay
**         archivos. quieren que quede en un warning.
***********************************************************************
*        gs_bal_msg-msgty = 'E'.
*        IF text cs 'No existen documentos a pendientes'.
*          gs_bal_msg-msgty = 'W'.
*        ENDIF.
*
*        gs_bal_msg-msgid = 'ZDTE_0001'.
*        gs_bal_msg-msgno = '012'.
*        IF wa_output-supplier_tras_etdresult-tipo_doc IS NOT INITIAL.
*          CONCATENATE wa_output-supplier_tras_etdresult-tipo_doc '/' wa_output-supplier_tras_etdresult-folio_doc '/'
*          wa_output-supplier_tras_etdresult-rutt_emis '/' wa_output-supplier_tras_etdresult-mensajes INTO
*          wa_output-supplier_tras_etdresult-mensajes.
*        ENDIF.
*
*        go_log->contenate_msg(
*            EXPORTING texto = text
*              descripcion_operacion = wa_output-supplier_tras_etdresult-mensajes
*            IMPORTING msgv1 = gs_bal_msg-msgv1
*                      msgv2 = gs_bal_msg-msgv2
*                      msgv3 = gs_bal_msg-msgv3
*                      msgv4 = gs_bal_msg-msgv4 ).
*        go_log->add_msg( i_s_msg = gs_bal_msg ).
*        go_log->save( ).
*        "-----> Envio de correo cuando Ws esta no OK, menos cuando no hay documentos pendientes
*        IF wa_output-supplier_tras_etdresult-codigo NE 'DON'.
*          me->envia_correo(
*          EXPORTING i_s_msg = gs_bal_msg
*                    i_bukrs = wa_0088-bukrs ).
*        ENDIF.
*        IF wa_output-supplier_tras_etdresult-cant_dte EQ 0 OR
*          wa_output-supplier_tras_etdresult-cant_dte EQ space.
*          sw = 1.
*        ENDIF.
*        "----> Operacion OK
*      ELSE. "OK
*        IF wa_output-supplier_tras_etdresult-cant_dte EQ 0.
*          sw = 1.
*        ENDIF.
*        IF wa_output-supplier_tras_etdresult-dte IS INITIAL.
*        ELSE.
*          wa_registro = wa_output-supplier_tras_etdresult.
*
*
*
**0. calcular digito verificador
**          CLEAR lv_dv.
**          me->digito_ver( EXPORTING number_part = wa_registro-rutt_emis
**              IMPORTING check_digit = lv_dv ).
**          CONCATENATE wa_registro-rutt_emis '-' lv_dv INTO wa_registro-rutt_emis.
**          CONDENSE wa_registro-rutt_emis NO-GAPS.
**
**1. Decodifico XML Base64
*          CHECK wa_registro-dte IS NOT INITIAL.
*          TRY.
*              "---->deco
*
*              DATA: ti_caracter TYPE STANDARD TABLE OF char100,
*                    ln_caracter LIKE LINE OF ti_caracter.
*
*
*              SPLIT wa_registro-dte AT CL_ABAP_CHAR_UTILITIES=>newline inTO TABLE ti_caracter.
*
*              DATA: lv_linea_xml TYPE string,
*                    wa_xml TYPE string.
*
*              CLEAR lv_linea_xml.
*              CLEAR wa_xml.
*              LOOP AT ti_caracter INTO ln_caracter.
*                CLEAR xml.
*                lv_linea_xml = ln_caracter.
*                CALL METHOD cl_http_utility=>if_http_utility~decode_base64
*                  EXPORTING
*                    encoded = lv_linea_xml
*                  RECEIVING
*                    decoded = xml.
*                .
*                CONCATENATE wa_xml xml INTO wa_xml.
*              ENDLOOP.
*              "--->fin.
*              xml = wa_xml.
*
*            CATCH cx_static_check.
*          ENDTRY.
*
*          CHECK xml IS NOT INITIAL.
*          CLEAR lv_estado.
**2. Grabo XMLdecodificado solo si tengo XML
*          me->move_file( EXPORTING i_registro = wa_registro
*                                   type_file = 'XML'
*                                   zpath = wa_0088-zdir_recxml
*                                   i_rutempresa = lv_rutempresa
*                                   xml = xml
*                          IMPORTING estado = lv_estado ).
*
*        ENDIF.
*      ENDIF.
*    ENDWHILE.
*  ENDLOOP.
*
  ENDMETHOD.


  METHOD ws_load_pdf.
**//.. OJO I_REGISTRO
**//.. import I_REGISTRO type ZWSDOCUMENTO
**  DATA: l_stream TYPE REF TO if_ixml_ostream.
**  DATA: go_xml TYPE REF TO cl_xml_document.
**  DATA: ls_xml TYPE string,
**        lx_xml TYPE xstring.
**
***  DATA: wa_input TYPE zwsobtener_pdfsoap_in.
***  DATA: wa_output TYPE zwsobtener_pdfsoap_out.
**
**  DATA lv_largo TYPE i.
**  DATA: oref TYPE REF TO cx_ai_system_fault,
**        iref TYPE REF TO cx_ai_application_fault,
**        text TYPE string.
**  DATA: lv_toyear  TYPE inri-toyear.
**
**  CLEAR wa_input. CLEAR wa_output.
**  wa_input-s_rutt_rece = i_rutempresa.
**  wa_input-s_rutt_emis = i_registro-rut_emisor.
**  wa_input-s_tipo_docu =  i_registro-tipo.
**  wa_input-s_foli_docu = i_registro-folio.
**  TRY.
**      CALL METHOD go_obtiene_pdf->obtener_pdf
**        EXPORTING
**          input  = wa_input
**        IMPORTING
**          output = wa_output.
**    CATCH cx_ai_system_fault INTO oref.
**      text = oref->get_text( ).
**    CATCH cx_ai_application_fault INTO iref.
**      text = iref->get_text( ).
**  ENDTRY.
****//.. OJO I_REGISTRO
**  "----> Operacion NOOK
**  IF wa_output-obtener_pdfresult-v_codigo NE 'DOK'. "NoOK
**    gs_bal_msg-msgty = 'E'.
**    gs_bal_msg-msgid = 'ZDTE_0001'.
**    gs_bal_msg-msgno = '012'.
**
**    go_log->contenate_msg(
**        EXPORTING texto = text
**          descripcion_operacion = wa_output-obtener_pdfresult-v_mensaje
**        IMPORTING msgv1 = gs_bal_msg-msgv1
**                  msgv2 = gs_bal_msg-msgv2
**                  msgv3 = gs_bal_msg-msgv3
**                  msgv4 = gs_bal_msg-msgv4 ).
**    go_log->add_msg( i_s_msg = gs_bal_msg ).
**    go_log->save( ).
**    "----> Operacion OK
**  ELSE. "OK
**
**    ls_xml = wa_output-obtener_pdfresult-v_pdf.
**    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
**      EXPORTING
**        text   = ls_xml
**      IMPORTING
**        buffer = lx_xml
**      EXCEPTIONS
**        failed = 1
**        OTHERS = 2.
**    IF sy-subrc <> 0.
**      MESSAGE 'Error in the PDF file' TYPE 'E'.
**    ENDIF.
**
**    CHECK lx_xml IS NOT INITIAL.
**    DATA: ls_output TYPE smum_xmltb,
**           lt_output TYPE STANDARD TABLE OF smum_xmltb,
**           ls_return TYPE bapiret2,
**           lt_return TYPE bapiret2_t.
**
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
**
**
**    DATA: converter TYPE REF TO cl_abap_conv_in_ce.
**    CALL METHOD cl_abap_conv_in_ce=>create
**      EXPORTING
**        input       = lv_xstring
**        encoding    = 'UTF-8'
**        replacement = '?'
**        ignore_cerr = abap_true
**      RECEIVING
**        conv        = converter.
**
**    DATA: lv_encoded TYPE string.
**
**    TRY.
**        CALL METHOD converter->read
**          IMPORTING
**            data = lv_encoded.
**      CATCH cx_sy_conversion_codepage.
***        -- Should ignore errors in code conversions
**      CATCH cx_sy_codepage_converter_init.
***        -- Should ignore errors in code conversions
**      CATCH cx_parameter_invalid_type.
**      CATCH cx_parameter_invalid_range.
**    ENDTRY.
**
**    DATA: lv_decodex TYPE xstring.
**    TRY.
**        CALL FUNCTION 'SSFC_BASE64_DECODE'
**          EXPORTING
**            b64data = lv_encoded
**          IMPORTING
**            bindata = lv_decodex
**          EXCEPTIONS
**            OTHERS  = 8.
**      CATCH cx_sy_conversion_codepage.
***        -- Should ignore errors in code conversions
**      CATCH cx_sy_codepage_converter_init.
***        -- Should ignore errors in code conversions
**      CATCH cx_parameter_invalid_type.
**      CATCH cx_parameter_invalid_range.
**    ENDTRY.
**    CHECK lv_decodex IS NOT INITIAL.
**    pdf = lv_decodex.
**
**  ENDIF.

  ENDMETHOD.


  METHOD ws_load_xml.
**//.. I_REGISTRO type ZWSDOCUMENTO
    DATA: l_stream TYPE REF TO if_ixml_ostream.
    DATA: go_xml TYPE REF TO cl_xml_document.
    DATA: ls_xml TYPE string,
          lx_xml TYPE xstring.

*  DATA: wa_input TYPE zwsrescata_xmldtesoap_in.
*  DATA: wa_output TYPE zwsrescata_xmldtesoap_out.
    DATA lv_largo TYPE i.
    DATA: oref TYPE REF TO cx_ai_system_fault,
          iref TYPE REF TO cx_ai_application_fault,
          text TYPE string.
    DATA: lv_toyear  TYPE inri-toyear.
**//.. OJO I_REGISTRO
**  CLEAR wa_input. clear wa_output.
**  wa_input-rut_empresa = i_rutempresa.
**  wa_input-rut_emisor = i_registro-rut_emisor.
**  wa_input-tipo =  i_registro-tipo.
**  wa_input-folio = i_registro-folio.
**
**  TRY.
**      CALL METHOD go_dterecep->rescata_xmldte
**        EXPORTING
**          input  = wa_input
**        IMPORTING
**          output = wa_output.
**    CATCH cx_ai_system_fault INTO oref.
**      text = oref->get_text( ).
**    CATCH cx_ai_application_fault INTO iref.
**      text = iref->get_text( ).
**  ENDTRY.
**
**  "----> Operacion NOOK
**  IF wa_output-rescata_xmldteresult-codigo NE 'DOK'. "NoOK
**    gs_bal_msg-msgty = 'E'.
**    gs_bal_msg-msgid = 'ZDTE_0001'.
**    gs_bal_msg-msgno = '012'.
**
**    go_log->contenate_msg(
**        EXPORTING texto = text
**          descripcion_operacion = wa_output-rescata_xmldteresult-mensajes
**        IMPORTING msgv1 = gs_bal_msg-msgv1
**                  msgv2 = gs_bal_msg-msgv2
**                  msgv3 = gs_bal_msg-msgv3
**                  msgv4 = gs_bal_msg-msgv4 ).
**    go_log->add_msg( i_s_msg = gs_bal_msg ).
**    go_log->save( ).
**    "----> Operacion OK
**  ELSE. "OK
**
**    xml = wa_output-rescata_xmldteresult-xmldocumento.
**
**    CONCATENATE wa_output-rescata_xmldteresult-rutt_emisor wa_output-rescata_xmldteresult-rutt_receptor
**    wa_output-rescata_xmldteresult-tipo_documento
**    wa_output-rescata_xmldteresult-folio_documento INTO nombrexml.
**    CONDENSE nombrexml NO-GAPS.
**
**  ENDIF.
**//.. OJO I_REGISTRO
  ENDMETHOD.


  METHOD ws_marcar.
**//.. I_REGISTRO type ZWSDOCUMENTO
**  DATA: wa_input TYPE zwsactualiza_estado_dtesoap_in.
**  DATA: wa_output TYPE zwsactualiza_estado_dtesoap_ou.
    DATA lv_largo TYPE i.
    DATA: oref        TYPE REF TO cx_ai_system_fault,
          iref        TYPE REF TO cx_ai_application_fault,
          text        TYPE string,
          lv_folio    TYPE  string,
          lv_tipo_dte TYPE string.

**  CLEAR wa_input. CLEAR wa_output.
**//.. OJO I_REGISTRO
**  wa_input-rut_empresa = i_rutempresa.
**  wa_input-rut_emisor = i_registro-rut_emisor.
**  wa_input-tipo =  i_registro-tipo.
**  wa_input-folio = i_registro-folio.
**  wa_input-estado = 'TRA'.
**  TRY.
**      CALL METHOD go_dterecep->actualiza_estado_dte
**        EXPORTING
**          input  = wa_input
**        IMPORTING
**          output = wa_output.
**    CATCH cx_ai_system_fault INTO oref.
**      text = oref->get_text( ).
**    CATCH cx_ai_application_fault INTO iref.
**      text = iref->get_text( ).
**  ENDTRY.

**  IF wa_output-actualiza_estado_dteresult-codigo NE 'DOK'. "NoOK
**
**    gs_bal_msg-msgty = 'W'.
**    gs_bal_msg-msgid = 'ZDTE_0001'.
**    gs_bal_msg-msgno = '012'.
**
**    go_log->contenate_msg(
**        EXPORTING texto = text
**          descripcion_operacion = wa_output-actualiza_estado_dteresult-mensajes
**        IMPORTING msgv1 = gs_bal_msg-msgv1
**                  msgv2 = gs_bal_msg-msgv2
**                  msgv3 = gs_bal_msg-msgv3
**                  msgv4 = gs_bal_msg-msgv4 ).
**    CLEAR lv_folio. CLEAR lv_tipo_dte.
**    lv_folio = i_registro-folio. lv_tipo_dte = i_registro-tipo.
**    CONCATENATE   lv_tipo_dte '/' lv_folio '/rut:' i_registro-rut_emisor
**        INTO gs_bal_msg-msgv4  .
**    CONDENSE gs_bal_msg-msgv4 NO-GAPS.
**    go_log->add_msg( i_s_msg = gs_bal_msg ).
**    go_log->save( ).
**    "----> Operacion OK
**  ELSE. "OK
**  ENDIF.

  ENDMETHOD.
ENDCLASS.
