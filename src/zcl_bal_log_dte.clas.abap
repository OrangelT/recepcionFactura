class ZCL_BAL_LOG_DTE definition
  public
  final
  create public .

public section.

  data GS_LOG type BAL_S_LOG .
  data GV_LOG_HANDLE type BALLOGHNDL .
  data GT_LOG_HEADER type BALHDR_T .
  data GS_LOG_FILTER type BAL_S_LFIL .
  data GS_OBJ type BAL_S_OBJ .

  methods CONSTRUCTOR
    importing
      value(I_S_OBJECT) type BAL_S_LOG optional .
  methods ADD_MSG
    importing
      !I_S_MSG type BAL_S_MSG .
  class-methods SAVE .
  methods SEARCH
    importing
      value(I_S_ALDATE) type BAL_S_DATE optional
      value(I_S_LOG_FILTER) type BAL_S_LFIL optional .
  methods DISPLAY .
  methods CREATE .
  methods DELETE_ALL .
  methods CONTENATE_MSG
    importing
      !TEXTO type STRING optional
      !DESCRIPCION_OPERACION type STRING optional
    exporting
      !MSGV1 type SYMSGV
      !MSGV2 type SYMSGV
      !MSGV3 type SYMSGV
      !MSGV4 type SYMSGV .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BAL_LOG_DTE IMPLEMENTATION.


method ADD_MSG.

  CALL FUNCTION 'BAL_LOG_MSG_ADD'
   EXPORTING
     i_log_handle             = me->gv_log_handle
     i_s_msg                  = i_s_msg
   EXCEPTIONS
     log_not_found            = 1
     msg_inconsistent         = 2
     log_is_full              = 3
     OTHERS                   = 4.

IF SY-SUBRC <> 0.
 MESSAGE ID SY-MSGID TYPE 'X' NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

endmethod.


method CONSTRUCTOR.
* HOW TO USE THIS CLASS EXAMPLE:
* GOR_BAL TYPE REF TO ZCL_BAL_LOG_DTE,
*
**  Declaración de Objetos de Datos
*  DATA:
**   Log aplicación: Datos de cabecera de log
*    LWA_LOG  TYPE BAL_S_LOG.
*
** DEFINE SOME HEADER DATA OF THIS LOG
*  LWA_LOG-EXTNUMBER  = 'BUKRSLIFNRXBLNR'.
*  LWA_LOG-OBJECT     = 'ZDTE'.
*  LWA_LOG-SUBOBJECT  = 'RECEPCION'.
*  LWA_LOG-ALDATE     = SYST-DATUM.
*  LWA_LOG-ALTIME     = SYST-UZEIT.
*  LWA_LOG-ALUSER     = SYST-UNAME.
*  LWA_LOG-ALPROG     = SYST-REPID.
*
*  CREATE OBJECT GOR_BAL
*    EXPORTING
*      I_S_OBJECT = LWA_LOG.
*
  me->gs_log = i_s_object.

  IF me->gs_log-object is not INITIAL.

  me->gs_obj-option = 'EQ'.
  me->gs_obj-sign   = 'I'.
  me->gs_obj-low    = me->gs_log-object.

  me->create( ).

  ENDIF.

endmethod.


method CONTENATE_MSG.

  data: lv_largo type i.
  data: lv_descripcion_operacion type string.

  clear lv_descripcion_operacion.
  lv_descripcion_operacion = descripcion_operacion.

  if ( descripcion_operacion is initial  ) and ( texto is
 not initial ).
    lv_descripcion_operacion = texto.
  endif.
  lv_largo = strlen( lv_descripcion_operacion ).
  msgv1 = lv_descripcion_operacion.

  if lv_largo > 50 and lv_largo < 101.
    lv_largo = lv_largo - 50.
    msgv2 = lv_descripcion_operacion+50(lv_largo).
  elseif lv_largo > 100.
    msgv2 = lv_descripcion_operacion+50(50).
  endif.

  if lv_largo > 100 and lv_largo < 151.
    lv_largo = lv_largo - 100.
    msgv3 = lv_descripcion_operacion+100(lv_largo).
  elseif lv_largo > 150.
    msgv3 = lv_descripcion_operacion+100(50).
  endif.

  if lv_largo > 150 and lv_largo < 201.
    lv_largo = lv_largo - 150.
    msgv4 = lv_descripcion_operacion+150(lv_largo).
  elseif lv_largo > 200.
    msgv4 = lv_descripcion_operacion+150(50).
  endif.
endmethod.


method CREATE.

  data: msg type string.

  IF me->gs_log is not initial .

  CALL FUNCTION 'BAL_LOG_CREATE'
  EXPORTING
    i_s_log                 = me->gs_log
  IMPORTING
    e_log_handle            = me->gv_log_handle
  EXCEPTIONS
    log_header_inconsistent = 1
    OTHERS                  = 2.

  IF sy-subrc <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ENDIF.

*  else.
*   CONCATENATE gs_log-object gs_log-subobject into msg SEPARATED BY space.
*      RAISE EXCEPTION TYPE ZCX_BAL_LOG
*        EXPORTING
*          pass = msg.
  ENDIF.

endmethod.


METHOD delete_all.

  "----->se buscan los anteriores.
  DATA: lt_log_header TYPE balhdr_t,
        lv_log_filter TYPE bal_s_lfil.

  DATA:r_external TYPE bal_r_extn,
                 r_object TYPE bal_r_obj,
                 r_subobject  TYPE bal_r_sub.

  DATA: ln_external LIKE LINE OF r_external,
                ln_object LIKE LINE OF r_object,
                ln_subobject LIKE LINE OF r_subobject.
  CLEAR:  ln_external,
          ln_object,
          ln_subobject.

  FREE: r_external,
        r_object,
        r_subobject.

  ln_external-sign = ln_object-sign = ln_subobject-sign = 'I'.
  ln_external-option = ln_object-option = ln_subobject-option = 'EQ'.
  ln_external-low =  me->gs_log-extnumber.
  ln_object-low = 'ZDTE'.
  ln_subobject-low = 'RECEPCION'.

  APPEND ln_external TO r_external.
  APPEND ln_object TO r_object.
  APPEND ln_subobject TO r_subobject.

  lv_log_filter-extnumber[] = r_external[].
  lv_log_filter-object[] = r_object[].
  lv_log_filter-subobject[] = r_subobject[].

  free lt_log_header.
  CALL FUNCTION 'BAL_DB_SEARCH'
    EXPORTING
      i_s_log_filter = lv_log_filter
    IMPORTING
      e_t_log_header = lt_log_header
   EXCEPTIONS
     LOG_NOT_FOUND            = 1
     NO_FILTER_CRITERIA       = 2
     OTHERS                   = 3
            .
  IF sy-subrc = 0 AND lt_log_header[] IS NOT INITIAL.
    CALL FUNCTION 'BAL_DB_DELETE'
      EXPORTING
        i_t_logs_to_delete = lt_log_header.
  ENDIF.
ENDMETHOD.


METHOD display.

  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
    EXPORTING
*     I_S_DISPLAY_PROFILE  =
*     I_T_LOG_HANDLE       =
*     I_T_MSG_HANDLE       =
      i_s_log_filter       = me->gs_log_filter
*     I_S_MSG_FILTER       =
*     I_T_LOG_CONTEXT_FILTER       =
*     I_T_MSG_CONTEXT_FILTER       =
*     I_AMODAL             = ' '
*     I_SRT_BY_TIMSTMP     = ' '
* IMPORTING
*     E_S_EXIT_COMMAND     =
    EXCEPTIONS
      profile_inconsistent = 1
      internal_error       = 2
      no_data_available    = 3
      no_authority         = 4
      OTHERS               = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDMETHOD.


method SAVE.



  CALL FUNCTION 'BAL_DB_SAVE'
  EXPORTING
    i_save_all = 'X'
  EXCEPTIONS
    LOG_NOT_FOUND    = 1
    SAVE_NOT_ALLOWED = 2
    NUMBERING_ERROR  = 3
    OTHERS           = 4.

IF sy-subrc <> 0.
 MESSAGE ID SY-MSGID TYPE 'X' NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

endmethod.


method SEARCH.

IF me->GS_OBJ-LOW IS NOT INITIAL.
  APPEND me->GS_OBJ TO i_s_log_filter-object.
ENDIF.


  IF i_s_aldate is NOT INITIAL.
      APPEND i_s_aldate TO i_s_log_filter-aldate.
  ENDIF.


  me->gs_log_filter = i_s_log_filter.

  CALL FUNCTION 'BAL_DB_SEARCH'
  EXPORTING
    i_s_log_filter     = me->gs_log_filter
  IMPORTING
    e_t_log_header     = me->gt_log_header
  EXCEPTIONS
    log_not_found      = 1
    no_filter_criteria = 2.

IF sy-subrc <> 0.

 MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
else.

* load these messages into memory
CALL FUNCTION 'BAL_DB_LOAD'
  EXPORTING
    i_t_log_header     = me->gt_log_header
  EXCEPTIONS
    no_logs_specified  = 1
    log_not_found      = 2
    log_already_loaded = 3.

  IF sy-subrc <> 0.
     MESSAGE ID SY-MSGID TYPE 'X' NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

  ENDIF.

ENDIF.

endmethod.
ENDCLASS.
