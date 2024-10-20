class ZCL_DTE_WSACEPTA definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .

  types:
    BEGIN OF ty_response,
             code    TYPE string,
             mensaje TYPE string,
           END OF ty_response .
  types:
    BEGIN OF ty_registro,
             rutemisor TYPE string,
             rutrecep  TYPE string,
             tipodte   TYPE string,
             folio     TYPE string,
           END OF ty_registro .

  data LS_RESPONSE type TY_RESPONSE .
  data LS_REGISTRO type TY_REGISTRO .
  data LV_XML type XSTRING .

  methods DIGITO_VER
    importing
      !NUMBER_PART type STRING
    exporting
      !CHECK_DIGIT type CHAR1 .
  methods FORMATO_RUT .
  methods MOVE_FILE
    importing
      !TYPE_FILE type CHAR3 optional
      !ZPATH type ZTFI_0088-ZDIR_RECXML optional
      !ARCH type XSTRING
      !I_RUTEMPRESA type CHAR16
    exporting
      !ESTADO type SY-SUBRC .
  methods XML_D
    importing
      !XML type XSTRING
    exporting
      !PARSE type ref to IF_IXML_PARSER
      !DOCU type ref to IF_IXML_DOCUMENT .
  methods NOMB_ARC
    importing
      !XML type XSTRING .
  methods NOMB_ARC_EX
    importing
      value(XML) type XSTRING optional
      value(DOCUMENTO_PA) type ref to IF_IXML_DOCUMENT optional .
  PROTECTED SECTION.
private section.

  data ZPATHXML type ZTFI_0088-ZDIR_RECXML .
  data ZPATHPDF type ZTFI_0078-DIRECPDF .

  methods LEC_ELEMENTO
    importing
      !DOCUMENTO_PA type ref to IF_IXML_DOCUMENT
      !SERVER type ref to IF_HTTP_SERVER
      !NAME type STRING .
ENDCLASS.



CLASS ZCL_DTE_WSACEPTA IMPLEMENTATION.


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


  METHOD formato_rut.
    DATA: s_dig TYPE char1.

    CLEAR s_dig.
    me->digito_ver( EXPORTING number_part =  ls_registro-rutrecep
                                           IMPORTING  check_digit = s_dig ).
    ls_registro-rutrecep = |{ ls_registro-rutrecep }-{ s_dig }|.
    CONDENSE  ls_registro-rutrecep NO-GAPS.

    CLEAR  s_dig.
    me->digito_ver( EXPORTING number_part = ls_registro-rutemisor
       IMPORTING  check_digit = s_dig ).
    ls_registro-rutemisor = |{ ls_registro-rutemisor }-{ s_dig }|.
    CONDENSE  ls_registro-rutemisor NO-GAPS.


  ENDMETHOD.


  METHOD if_http_extension~handle_request.
    DATA: lv_response TYPE string.
    DATA(lv_method) = server->request->get_method( ).
    DATA(lv_query) =  server->request->get_form_field( '~query_string' ).
    DATA(lv_body) =  server->request->get_cdata(  ).
*---> Convertiendo en xstring
    lv_xml = cl_abap_codepage=>convert_to( lv_body ).
    ls_response-code = '1'.
    ls_response-mensaje = 'Evento Recibido'.

    CASE lv_method.
      WHEN 'POST' OR 'GET'.
        me->xml_d( EXPORTING xml = lv_xml
        IMPORTING parse = DATA(parser_pa)
                   docu =  DATA(document_pa) ).
        DATA(parsing_check) = parser_pa->parse( ).
        IF parsing_check = 0.
          DATA(root_element) = document_pa->get_root_element( ).
          DATA(ls_nameroot) =  root_element->get_name(  ).
          IF ls_nameroot = 'test'.
            "---> Envio test Ok
            ls_response-code = '1'.
            ls_response-mensaje = 'Comunicacion OK'.
          ELSEIF ls_nameroot = 'RecepcionDocumento'.
*---> Obtengo nombre de archivo.
            me->nomb_arc_ex( EXPORTING  documento_pa = document_pa ).
            IF ls_response-code = '1'.
*---> Busco XML / Guardo en Directorio
              lec_elemento(  EXPORTING documento_pa = document_pa
                                       server = server
                                       name = 'XML_DTE' ).
*---> Busco PDF y Guardo en Directorios
              IF ls_response-code = '1'. "No hay error en XML
                lec_elemento(  EXPORTING documento_pa = document_pa
                                         server = server
                                         name = 'PDF' ).
              ENDIF.
            ENDIF.
          ELSE.
            server->response->set_status( code = '500'
                  reason = 'BAD REQUEST-XML NO es RecepcionDocumento/test' ).
            ls_response-code = '4'.
            ls_response-mensaje = 'XML No es RecepcionDocumento/test'.
          ENDIF.

        ELSE.
          server->response->set_status( code = '500'
                reason = 'BAD REQUEST-XML mal Formado' ).
          ls_response-code = '4'.
          ls_response-mensaje = 'XML mal Formado'.
        ENDIF.
      WHEN OTHERS.
        ls_response-code = '4'.
        ls_response-mensaje = 'BAD REQUEST-Solo se acepta metodo POST/GET'.
        server->response->set_status( code = '500'
         reason = 'BAD REQUEST-Solo se acepta metodo POST/GET' ).
    ENDCASE.
    IF ls_response-code IS NOT INITIAL.
      CALL TRANSFORMATION zdte_acepta_response SOURCE respuesta = ls_response
                            RESULT XML DATA(xml_tab_a).
      lv_response = cl_abap_codepage=>convert_from( xml_tab_a ).
      server->response->set_cdata( lv_response ).
    ENDIF.
  ENDMETHOD.


  METHOD lec_elemento.
    DATA: lv_xml1 TYPE xstring.
    DATA: archivo TYPE string.
    DATA(r_element) = documento_pa->find_from_name_ns( name = name ).
    IF r_element IS NOT INITIAL.
*--> Obtengo archivo y decodifico
      archivo =  r_element->get_value(  ).
      TRY.
          CALL FUNCTION 'SSFC_BASE64_DECODE'
            EXPORTING
              b64data = archivo
            IMPORTING
              bindata = lv_xml1
            EXCEPTIONS
              OTHERS  = 8.
        CATCH cx_sy_conversion_codepage.
*        -- Should ignore errors in code conversions
        CATCH cx_sy_codepage_converter_init.
*        -- Should ignore errors in code conversions
        CATCH cx_parameter_invalid_type.
        CATCH cx_parameter_invalid_range.
      ENDTRY.

      IF lv_xml1 IS NOT INITIAL.
        IF name = 'XML_DTE'.
*--> Obtengo FieldKeys y Busco directorios Recibidos y PDF
*          me->nomb_arc( EXPORTING xml =  lv_xml1 ).
          DATA(zpath) = me->zpathxml.
          DATA(tipo) = 'XML'.
        ELSE.
          zpath = me->zpathpdf.
          tipo = 'PDF'.
        ENDIF.
*        IF ls_response-code NE '4'.
        me->move_file( type_file    = tipo
                       zpath        = zpath
                       i_rutempresa =  CONV stcd1( ls_registro-rutrecep )
                       arch         = lv_xml1 ).
*        ENDIF.
      ELSE.
        ls_response-code = '4'.
        ls_response-mensaje = |Error No se envio { name }|.
      ENDIF.
    ELSE.
      ls_response-code = '4'.
      ls_response-mensaje = |No se Envio TAG { name }|.
    ENDIF.

  ENDMETHOD.


  METHOD move_file.
    DATA: lv_directory TYPE user_dir-dirname,
          lv_file_path TYPE string,
          arch_string  TYPE string,
          extens(4)    TYPE c,
          lv_sep       TYPE char01,
          lv_msg       TYPE string.


    SELECT SINGLE * INTO @DATA(ls_opsystem)
    FROM  opsystem
    WHERE opsys EQ @sy-opsys.
    IF ls_opsystem-filesys EQ 'UNIX'.
      lv_sep = '/'.
    ELSE.
      lv_sep = '\'.
    ENDIF.

    SELECT SINGLE dirname FROM user_dir
            INTO lv_directory
            WHERE aliass = zpath.
    IF sy-subrc NE 0.
      ls_response-code = '4'.
* Estoy probando si se mueve rapido
      ls_response-mensaje = |No se ha creado el Directorio { zpath } En FileServer|.
    ELSE.

      lv_directory = |{ lv_directory }{ lv_sep }|.
      IF type_file = 'XML'.
        extens = '.XML'.
      ELSE.
        extens = '.PDF'.
      ENDIF.

      "----> Creo nombre de arvhivo
      lv_file_path = |{ lv_directory }{ ls_registro-rutemisor }|.
      lv_file_path = |{ lv_file_path }{ ls_registro-rutrecep }|.
      lv_file_path = |{ lv_file_path }{ ls_registro-tipodte }|.
      lv_file_path = |{ lv_file_path }{ ls_registro-folio }|.
      lv_file_path = |{ lv_file_path }{ extens }|.

      "---->Guardo en Directorio
      IF type_file = 'XML'.
        "----> coloco a string
        arch_string = cl_abap_codepage=>convert_from( arch ).

        TRY.
            OPEN DATASET lv_file_path FOR OUTPUT MESSAGE lv_msg IN TEXT MODE ENCODING UTF-8.
            IF sy-subrc = 0.
              TRANSFER arch_string TO lv_file_path.
              CLOSE DATASET lv_file_path.
            ELSE.
              ls_response-code = '4'.
              IF lv_msg IS INITIAL.
                ls_response-mensaje = 'No se puedo guardar PDF'.
              ELSE.
                ls_response-mensaje = lv_msg.
              ENDIF.
            ENDIF.
          CATCH  cx_sy_file_open_mode INTO DATA(lo_err).
            DATA(lv_err_string) = lo_err->get_text( ).
          CATCH: cx_sy_file_open,
                          cx_sy_codepage_converter_init,
                          cx_sy_conversion_codepage,
                          cx_sy_file_authority,
                          cx_sy_pipes_not_supported,
                          cx_sy_too_many_files INTO DATA(lo_cx_root).
            lv_err_string = lo_cx_root->get_text( ).
        ENDTRY.
      ELSE.

        TRY.
            OPEN DATASET lv_file_path FOR OUTPUT MESSAGE lv_msg IN BINARY MODE.
            IF sy-subrc = 0.
              TRANSFER arch TO lv_file_path.
              CLOSE DATASET lv_file_path.
            ELSE.
              ls_response-code = '4'.
              IF lv_msg IS INITIAL.
                ls_response-mensaje = 'No se puedo guardar PDF'.
              ELSE.
                ls_response-mensaje = lv_msg.
              ENDIF.
            ENDIF.
          CATCH  cx_sy_file_open_mode INTO lo_err.
            lv_err_string = lo_err->get_text( ).
          CATCH: cx_sy_file_open,
                          cx_sy_codepage_converter_init,
                          cx_sy_conversion_codepage,
                          cx_sy_file_authority,
                          cx_sy_pipes_not_supported,
                          cx_sy_too_many_files INTO lo_cx_root.
            lv_err_string = lo_cx_root->get_text( ).
        ENDTRY.
        IF lv_err_string IS NOT INITIAL.
          ls_response-code = '4'.
          ls_response-mensaje = lv_err_string.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD nomb_arc.
    DATA: r_elemno       TYPE REF TO if_ixml_element.

    me->xml_d(  EXPORTING xml   = xml
                IMPORTING parse = DATA(parser_pa1)
                           docu = DATA(document_pa1) ).

    DATA(parsing_check) = parser_pa1->parse( ).
    IF parsing_check = 0.
      DATA(root_element) = document_pa1->get_root_element( ).
      DATA(ls_nameroot) =  root_element->get_name(  ).
      CLEAR r_elemno.
      r_elemno = document_pa1->find_from_name_ns( name = 'RUTEmisor' ).
      IF r_elemno IS NOT INITIAL.
        ls_registro-rutemisor = r_elemno->get_value(  ).
      ENDIF.

      CLEAR r_elemno.
      r_elemno = document_pa1->find_from_name_ns( name = 'RUTRecep' ).
      IF r_elemno IS NOT INITIAL.
        ls_registro-rutrecep = r_elemno->get_value(  ).
      ENDIF.

      CLEAR r_elemno.
      r_elemno = document_pa1->find_from_name_ns( name = 'TipoDTE' ).
      IF r_elemno IS NOT INITIAL.
        ls_registro-tipodte = r_elemno->get_value(  ).
      ENDIF.

      CLEAR r_elemno.
      r_elemno = document_pa1->find_from_name_ns( name = 'Folio' ).
      IF r_elemno IS NOT INITIAL.
        ls_registro-folio = r_elemno->get_value(  ).
      ENDIF.

      SELECT  * FROM ztfi_0078 INTO @DATA(ls_0078) UP TO 1 ROWS
      WHERE suscriber EQ @ls_registro-rutrecep
      ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF sy-subrc NE 0.
        ls_response-code = '4'.
        ls_response-mensaje = |rut de receptor { ls_registro-rutrecep } no Tiene Parametrizacion MontFacElectronica|.
      ELSE.
        me->zpathpdf = ls_0078-direcpdf.
        SELECT  * FROM ztfi_0088 INTO @DATA(ls_0088) UP TO 1 ROWS
        WHERE bukrs EQ @ls_0078-bukrs
        ORDER BY PRIMARY KEY.
        ENDSELECT.
        IF sy-subrc EQ 0.
          me->zpathxml = ls_0088-zdir_recxml.
        ELSE.
          ls_response-code = '4'.
          ls_response-mensaje = |rut de receptor { ls_registro-rutrecep } no Tiene Parametrizacion MontFacElectronica|.
        ENDIF.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD nomb_arc_ex.
    DATA: r_elemno       TYPE REF TO if_ixml_element.

*    me->xml_d(  EXPORTING xml   = xml
*                IMPORTING parse = DATA(parser_pa1)
*                           docu = DATA(document_pa1) ).
*
*    DATA(parsing_check) = parser_pa1->parse( ).
*    IF parsing_check = 0.

    DATA(root_element) = documento_pa->get_root_element( ).
    DATA(ls_nameroot) =  root_element->get_name(  ).
    CLEAR r_elemno.
    r_elemno = documento_pa->find_from_name_ns( name = 'RUT_EMISOR' ).
    IF r_elemno IS NOT INITIAL.
      ls_registro-rutemisor = r_elemno->get_value(  ).
    ENDIF.

    CLEAR r_elemno.
    r_elemno = documento_pa->find_from_name_ns( name = 'RUT_RECEPTOR' ).
    IF r_elemno IS NOT INITIAL.
      ls_registro-rutrecep = r_elemno->get_value(  ).
    ENDIF.

    CLEAR r_elemno.
    r_elemno = documento_pa->find_from_name_ns( name = 'TIPO_DTE' ).
    IF r_elemno IS NOT INITIAL.
      ls_registro-tipodte = r_elemno->get_value(  ).
    ENDIF.

    CLEAR r_elemno.
    r_elemno = documento_pa->find_from_name_ns( name = 'FOLIO' ).
    IF r_elemno IS NOT INITIAL.
      ls_registro-folio = r_elemno->get_value(  ).
    ENDIF.

*   "---> Digito verificador
    me->formato_rut( ).
*   "---> busco Configuracion de Empresa y Path xml/pdf
    SELECT  * FROM ztfi_0078 INTO @DATA(ls_0078) UP TO 1 ROWS
    WHERE suscriber EQ @ls_registro-rutrecep
    ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc NE 0.
      ls_response-code = '4'.
      ls_response-mensaje = |rut de receptor { ls_registro-rutrecep } no Tiene Parametrizacion MontFacElectronica|.
    ELSE.
      me->zpathpdf = ls_0078-direcpdf.
      SELECT  * FROM ztfi_0088 INTO @DATA(ls_0088) UP TO 1 ROWS
      WHERE bukrs EQ @ls_0078-bukrs
      ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF sy-subrc EQ 0.
        me->zpathxml = ls_0088-zdir_recxml.
      ELSE.
        ls_response-code = '4'.
        ls_response-mensaje = |rut de receptor { ls_registro-rutrecep } no Tiene Parametrizacion MontFacElectronica|.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD xml_d.
    DATA(ixml_pa) = cl_ixml=>create( ).
    DATA(stream_factory_pa) = ixml_pa->create_stream_factory( ).
    DATA(document_pa) = ixml_pa->create_document( ).
    DATA(parser_pa) = ixml_pa->create_parser(
                  istream = stream_factory_pa->create_istream_xstring( string = xml )
                  document = document_pa
                  stream_factory = stream_factory_pa ).
    parse =  parser_pa.
    docu =   document_pa.

  ENDMETHOD.
ENDCLASS.
