class ZCL_DTE_RECEPCION_CL definition
  public
  inheriting from ZCL_DTE_RECEPCION
  final
  create public .

public section.

  methods IMPORT_FILE
    redefinition .
  methods LOAD_SAP_DATA
    redefinition .
  methods MAPPING_TO_STRUC
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DTE_RECEPCION_CL IMPLEMENTATION.


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
*OT manejo de multiples DTE
    DATA: lf_node     TYPE REF TO if_ixml_node,
          lf_iterator TYPE REF TO if_ixml_node_iterator,
          lv_value    TYPE string,
          lv_name     TYPE string,
          lo_xml      TYPE REF TO cl_xml_document.
*OT
    DATA: BEGIN OF isearchpoints,
            dirname(120) TYPE c,            " name of directory. "nvsacc 031024(75)
            aliass(120)  TYPE c,            " alias for directory."nvsacc 031024(75)
            svrname(120) TYPE c,            " svr where directory is availabl"nvsacc 031024(75)
            sp_name(120) TYPE c,            " name of entry. (may end with *)"nvsacc 031024(75)
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
    CREATE OBJECT lo_xml.

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
*OT Manejo de multiples DTE en solo XML
*      CALL METHOD go_xml->import_from_file
      CALL METHOD lo_xml->import_from_file
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
*        me->variante_interfaz(  ).
*        me->parse_xml_to_tab_v2( ).
*        lv_filenam = lv_file2.
*        me->mapping_to_struc( filename = lv_filenam ).
*        me->load_sap_data( ).
*
*        READ TABLE gt_bukrs INTO ls_bukrs INDEX 1.
*        me->save_pdf_form( EXPORTING
*                             i_bukrs = ls_bukrs-low ).

        me->variante_interfaz(  ).

        lv_filenam = lv_file2.
        lf_node = lo_xml->m_document.
        IF  lf_node IS NOT INITIAL.
          lf_iterator = lf_node->create_iterator( ).
          lf_node = lf_iterator->get_next( ).
          WHILE NOT lf_node IS INITIAL.
            CLEAR: lv_name, lv_value.
            lv_name = lf_node->get_name( ).
            lv_value = lf_node->get_value( ).
            CASE lv_name.
              WHEN 'DTE' OR 'Dte'.
                CALL METHOD go_xml->get_node_subtree
                  EXPORTING
                    node    = lf_node
                  RECEIVING
                    subtree = go_xml.
                me->parse_xml_to_tab_v2( ).
                me->mapping_to_struc( filename = lv_filenam ).
                me->load_sap_data( ).

                READ TABLE gt_bukrs INTO ls_bukrs INDEX 1.
                me->save_pdf_form( EXPORTING
                                     i_bukrs = ls_bukrs-low ).

              WHEN OTHERS.
            ENDCASE.
            lf_node = lf_iterator->get_next( ).
          ENDWHILE.
        ENDIF.
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
*OT mod Multiples DTE en un mismo Archivo
                lo_xml->parse_string( stream = all_lines ).
                lf_node = lo_xml->m_document.
                IF  lf_node IS NOT INITIAL.
                  lf_iterator = lf_node->create_iterator( ).
                  lf_node = lf_iterator->get_next( ).
                  WHILE NOT lf_node IS INITIAL.
                    CLEAR: lv_name, lv_value.
                    lv_name = lf_node->get_name( ).
                    lv_value = lf_node->get_value( ).
                    CASE lv_name.
                      WHEN 'DTE' OR 'Dte'.
                        CALL METHOD go_xml->get_node_subtree
                          EXPORTING
                            node    = lf_node
                          RECEIVING
                            subtree = go_xml.
*                  go_xml->parse_string( stream = all_lines ).
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
                      WHEN OTHERS.
                    ENDCASE.
                    lf_node = lf_iterator->get_next( ).
                  ENDWHILE.
                ENDIF.
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
*OT se guarda el archivo con el Original q contiene todos los dtes
          go_xml = lo_xml.

          me->move_filexml( file_o =  lv_file_o
                            file_d = lv_file_d
                            nomb_archivo =  ls_file2-name ).


        ENDLOOP."Archivos

      ENDLOOP."Sociedades

    ENDIF.

  ENDMETHOD.


METHOD load_sap_data.
*CALL METHOD SUPER->LOAD_SAP_DATA
*    .
    me->load_sap_data_2( ).
  ENDMETHOD.


METHOD mapping_to_struc.
*CALL METHOD SUPER->MAPPING_TO_STRUC
**  EXPORTING
**    filename =
*    .
    me->mapping_to_struc_2( filename = filename ).
  ENDMETHOD.
ENDCLASS.
