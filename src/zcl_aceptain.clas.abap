CLASS zcl_aceptain DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_service_extension .
    TYPES: BEGIN OF ty_response,
             code    TYPE string,
             mensaje TYPE string,
           END OF ty_response.
    DATA: ls_response TYPE ty_response.
    DATA: lv_xml TYPE xstring.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aceptain IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.
    DATA: lv_response TYPE string.
    DATA(lv_method) = request->get_method( ).
    DATA(lv_query) = request->get_form_field( '~query_string' ).
    DATA(lv_body) = request->get_text(  ).
    "--> Convertiendo en xstring
    lv_xml = cl_abap_conv_codepage=>create_out( )->convert( lv_body ).

    CASE lv_method.
      WHEN 'POST'.
        DATA(ixml_pa) = cl_ixml_core=>create( ).
        DATA(stream_factory_pa) = ixml_pa->create_stream_factory( ).
        DATA(document_pa) = ixml_pa->create_document( ).
        DATA(parser_pa) = ixml_pa->create_parser(
                      istream = stream_factory_pa->create_istream_xstring( string = lv_xml )
                      document = document_pa
                      stream_factory = stream_factory_pa ).
        DATA(parsing_check) = parser_pa->parse( ).
        IF parsing_check = 0.
          DATA(root_element) = document_pa->get_root_element( ).
          DATA(ls_nameroot) =  root_element->get_name(  ).
          IF ls_nameroot = 'test'.
            "---> Envio test Ok
            ls_response-code = '0'.
            ls_response-mensaje = 'Comunicacion OK'.
            CALL TRANSFORMATION id SOURCE itab = ls_response
                          RESULT XML DATA(xml_tab_a).
            lv_response = cl_abap_conv_codepage=>create_in( )->convert( xml_tab_a ).

          ELSE.
            "---> Leo XML/PDF y dejo en directorios
            lv_response = | encontre el otro|.
          ENDIF.
        ENDIF.

        "      lv_response = |prueba { lv_body }|.
      WHEN 'GET'.
        lv_response = |prueba { lv_query }|.


    ENDCASE.
    IF lv_response IS NOT INITIAL.
*      response->set_content_type( 'aplication/xml' ).
      response->set_text( lv_response ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
