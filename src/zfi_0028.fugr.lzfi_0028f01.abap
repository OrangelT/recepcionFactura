*----------------------------------------------------------------------*
***INCLUDE LZFI_0028F01.
*----------------------------------------------------------------------*

*{   INSERT         DECK910595                                        1
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_DOC  text
*      -->P_I_CORREO  text
*      -->P_I_PDF  text
*      -->P_I_XML  text
*      -->P_I_CSENDER  text
*      -->P_I_CODTEXTOC  text
*      <--P_E_RC  text
*      <--P_E_MSG  text
*----------------------------------------------------------------------*
FORM send_email  USING    i_doc
                          i_correo TYPE char100
                          i_pdf TYPE xstring
                          i_xml TYPE xstring
                          i_csender TYPE char100
                          i_codtextoc  TYPE char20
                          CHANGING e_rc e_msg.
  DATA: lv_seg         TYPE i VALUE '2',
        ls_msg         TYPE bapiret2,
        lv_message     TYPE bapi_msg,
        lv_send        TYPE os_boolean,
        p_template_id  TYPE smtg_tmpl_id,
        lv_remitente   TYPE ad_smtpadr,
        lv_pdf_size    TYPE so_obj_len,
        lv_pdf_name    TYPE sood-objdes,
        lv_pdf_content TYPE solix_tab,
        u_email        TYPE ad_smtpadr.
  TYPES: BEGIN OF ty_gs_data_key,
           name  TYPE string,
           value TYPE string,
         END   OF ty_gs_data_key.
  TYPES ty_gt_data_key TYPE STANDARD TABLE OF ty_gs_data_key WITH EMPTY KEY.


  DATA: o_cx_send_req_bcs TYPE REF TO cx_send_req_bcs,
        o_cx_bcs          TYPE REF TO cx_bcs,
        o_cl_bcs          TYPE REF TO cl_bcs.

  DATA: cl_send_request TYPE REF TO cl_bcs,
        cl_document     TYPE REF TO cl_document_bcs,
        cl_recipient    TYPE REF TO if_recipient_bcs,
        bcs_exception   TYPE REF TO cx_bcs,
        cx_smtg         TYPE REF TO cx_smtg_email_common,

        cl_o_sender     TYPE REF TO cl_cam_address_bcs.

  DATA: i_docre TYPE zst_dte_ack.

  CLASS cx_bcs DEFINITION LOAD.
  CLASS cx_address_bcs DEFINITION LOAD.
  CLASS cx_document_bcs DEFINITION LOAD.
  CLASS cx_send_req_bcs DEFINITION LOAD.
  CLASS cx_smtg_email_common  DEFINITION LOAD.

  WAIT UP TO lv_seg SECONDS.
  u_email = i_correo.
  i_docre = i_doc.
  TRY.
**//.. Crea objeto de envio de email
      cl_send_request = cl_bcs=>create_persistent( ).

**//.. Prepara CDS
      p_template_id = i_codtextoc.
      TRY.
          DATA(lo_email_api) = cl_smtg_email_api=>get_instance( iv_template_id = p_template_id  ).

        CATCH cx_smtg_email_common INTO cx_smtg.
          e_rc = 4.
          lv_message = cx_smtg->get_text( ).
          e_msg = lv_message.
          RETURN.
      ENDTRY.
      DATA(lt_cds_key) = VALUE ty_gt_data_key( ( name = 'bukrs' value = i_docre-bukrs )
                                               ( name = 'tipodte' value =  i_docre-tipodte )
                                               ( name = 'xblnr'  value =  i_docre-folio )
                                               ( name = 'stcd1' value =  i_docre-rutemisor ) ).
      TRY.
          lo_email_api->render_bcs( io_bcs = cl_send_request iv_language = sy-langu it_data_key = lt_cds_key ).
        CATCH cx_smtg_email_common INTO cx_smtg.
          e_rc = 4.
          lv_message = cx_smtg->get_text( ).
          e_msg = lv_message.
          RETURN.
      ENDTRY.
**//.. Remitente
      IF  i_csender IS NOT INITIAL.
        lv_remitente = i_csender.
      ELSE.
        lv_remitente = 'no-reply@embonor.cl'.
      ENDIF.

      cl_o_sender = cl_cam_address_bcs=>create_internet_address( lv_remitente ).
      cl_send_request->set_sender( i_sender = cl_o_sender ).

***//.. Cuerpo y asunto del correo
      cl_document ?=  cl_send_request->document( ).
      IF i_pdf IS NOT INITIAL.
**//.. Get PDF xstring and convert it to BCS format
        lv_pdf_size = xstrlen( i_pdf ).
        CONDENSE lv_pdf_size NO-GAPS.
        lv_pdf_content = cl_document_bcs=>xstring_to_solix( ip_xstring = i_pdf ).
        lv_pdf_name = |{ i_docre-bukrs }{ i_docre-tipodte }{ i_docre-folio }.pdf|.

**//.. add the spread sheet as attachment to document object
        cl_document->add_attachment( i_attachment_type    = 'PDF' "#EC NOTEXT
                                     i_attachment_subject = lv_pdf_name "#EC NOTEXT
                                     i_attachment_size    = lv_pdf_size  "Size
                                     i_att_content_hex    = lv_pdf_content ).

      ENDIF.
      IF i_xml IS NOT INITIAL.
**//.. Get XML xstring and convert it to BCS format
        lv_pdf_size = xstrlen( i_xml ).
        CONDENSE lv_pdf_size NO-GAPS.
        lv_pdf_content = cl_document_bcs=>xstring_to_solix( ip_xstring = i_xml ).
        lv_pdf_name = |{ i_docre-bukrs }{ i_docre-tipodte }{ i_docre-folio }.xml|.

**//.. add the spread sheet as attachment to document object
        cl_document->add_attachment( i_attachment_type    = 'XML' "#EC NOTEXT
                                     i_attachment_subject = lv_pdf_name "#EC NOTEXT
                                     i_attachment_size    = lv_pdf_size  "Size
                                     i_att_content_hex    = lv_pdf_content ).
      ENDIF.

**//.. Agregar Documento al Email
      cl_send_request->set_document( cl_document ).
**//.. Agrega el destinatario
      cl_recipient = cl_cam_address_bcs=>create_internet_address( u_email ).
      cl_send_request->add_recipient( cl_recipient ).
**//.. Indica para enviar inmediatamente
      cl_send_request->set_send_immediately( 'X' ).

      CLEAR lv_send.
**//.. Envía email
      lv_send = cl_send_request->send( ).

      COMMIT WORK.
**//..
      IF lv_send IS INITIAL.
        lv_message = 'No  se envio Mensaje'.
        e_rc = 4.
      ELSE.
        lv_message = 'Envio Mensaje'.
        e_rc = 0.
      ENDIF.

    CATCH cx_bcs INTO bcs_exception.
      e_rc = 4.
      PERFORM fo_error_handling USING    bcs_exception->error_type
                                CHANGING lv_message.
      e_msg = lv_message.
  ENDTRY.



ENDFORM.
*}   INSERT
*{   INSERT         DECK910595                                        2
*&---------------------------------------------------------------------*
*&      Form  FO_ERROR_HANDLING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_BCS_EXCEPTION_>ERROR_TYPE  text
*      <--P_LV_MESSAGE  text
*----------------------------------------------------------------------*
FORM fo_error_handling USING u_error_type TYPE bcs_cxerr
                        CHANGING p_message    TYPE bapi_msg.
  TYPE-POOLS: skwgd, skwfc.
***CASE
  CASE u_error_type.
*CX_BCS
    WHEN cx_bcs=>internal_error.
      MESSAGE ID 'SKWG_ERRS' TYPE 'E' NUMBER skwgd_e_internal_error INTO p_message.
    WHEN cx_bcs=>os_exception.
      MESSAGE ID 'SKWG_ERRS' TYPE 'E' NUMBER skwgd_e_os_exception INTO p_message.
    WHEN cx_bcs=>x_error.
      MESSAGE ID 'SKWG_ERRS' TYPE 'E' NUMBER skwgd_e_x_error INTO p_message.
    WHEN cx_bcs=>parameter_error.
      MESSAGE ID 'SKWG_ERRS' TYPE 'E' NUMBER skwgd_e_parameter_error INTO p_message.
    WHEN cx_bcs=>invalid_value.
      MESSAGE ID 'SKWG_ERRS' TYPE 'E' NUMBER skwgd_e_invalid_value INTO p_message.
    WHEN cx_bcs=>creation_failed.
      MESSAGE ID 'SKWG_ERRS' TYPE 'E' NUMBER skwgd_e_creation_failed INTO p_message.
    WHEN cx_bcs=>no_authorization.
      MESSAGE ID 'SKWG_ERRS' TYPE 'E' NUMBER skwgd_e_no_authorization INTO p_message.
    WHEN cx_bcs=>locked.
      MESSAGE ID 'SKWG_ERRS' TYPE 'E' NUMBER skwgd_e_locked INTO p_message.
    WHEN cx_bcs=>cast_error.
      MESSAGE ID 'SKWG_ERRS' TYPE 'E' NUMBER skwgd_e_cast_error INTO p_message.
*CX_ADDRESS
    WHEN cx_address_bcs=>not_found.
      MESSAGE ID 'SKWG_ERRS' TYPE 'E' NUMBER skwgd_e_not_found INTO p_message.
    WHEN cx_address_bcs=>cannot_resolve.
      MESSAGE ID 'SKWG_ERRS' TYPE 'E' NUMBER skwgd_e_cannot_resolve INTO p_message.
*CX_DOCUMENT_BCS
    WHEN cx_document_bcs=>object_not_exists.
      MESSAGE ID 'SKWG_ERRS' TYPE 'E' NUMBER skwgd_e_object_not_exists INTO p_message.
    WHEN cx_document_bcs=>kpro_error.
      MESSAGE ID 'SKWG_ERRS' TYPE 'E' NUMBER skwgd_e_kpro_error INTO p_message.
    WHEN cx_document_bcs=>mime_not_available.
      MESSAGE ID 'SKWG_ERRS' TYPE 'E' NUMBER skwgd_e_mime_not_available INTO p_message.
*CX_SEND_REQ_BCS
    WHEN cx_send_req_bcs=>allready_released.
      MESSAGE ID 'SKWG_ERRS' TYPE 'E' NUMBER skwgd_e_allready_released INTO p_message.
    WHEN cx_send_req_bcs=>recipient_exists.
      MESSAGE ID 'SKWG_ERRS' TYPE 'E' NUMBER skwgd_e_recipient_exists INTO p_message.
    WHEN cx_send_req_bcs=>document_not_exists.
      MESSAGE ID 'SKWG_ERRS' TYPE 'E' NUMBER skwgd_e_document_not_exists INTO p_message.
    WHEN cx_send_req_bcs=>document_not_sent.
      MESSAGE ID 'SKWG_ERRS' TYPE 'E' NUMBER skwgd_e_document_not_sent INTO p_message.
    WHEN cx_send_req_bcs=>not_submited.
      MESSAGE ID 'SKWG_ERRS' TYPE 'E' NUMBER skwgd_e_not_submited INTO p_message.
    WHEN cx_send_req_bcs=>foreign_lock.
      MESSAGE ID 'SKWG_ERRS' TYPE 'E' NUMBER skwgd_e_foreign_lock INTO p_message.
    WHEN cx_send_req_bcs=>too_many_receivers.
      MESSAGE ID 'SKWG_ERRS' TYPE 'E' NUMBER skwgd_e_too_many_receivers INTO p_message.
    WHEN cx_send_req_bcs=>no_instance.
      MESSAGE ID 'SKWG_ERRS' TYPE 'E' NUMBER skwgd_e_no_instance INTO p_message.
    WHEN cx_send_req_bcs=>cancelled.
      MESSAGE ID 'SKWG_ERRS' TYPE 'E' NUMBER skwgd_e_cancelled INTO p_message.
    WHEN cx_send_req_bcs=>not_released.
      MESSAGE ID 'SKWG_ERRS' TYPE 'E' NUMBER skwgd_e_not_released INTO p_message.
    WHEN cx_send_req_bcs=>mime_problems.
      MESSAGE ID 'SKWG_ERRS' TYPE 'E' NUMBER skwgd_e_mime_problems INTO p_message.
    WHEN cx_send_req_bcs=>no_recipients.
      MESSAGE ID 'SKWG_ERRS' TYPE 'E' NUMBER skwgd_e_os_exception INTO p_message.
    WHEN cx_send_req_bcs=>sender_problems.
      MESSAGE ID 'SKWG_ERRS' TYPE 'E' NUMBER skwgd_e_sender_problems INTO p_message.
  ENDCASE.

ENDFORM.
*}   INSERT
