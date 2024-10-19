*----------------------------------------------------------------------*
***INCLUDE LZGF_ZVFI_0087F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  OBTAIN_DATA_RECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM obtain_data_record .
  CLEAR <table1>.
  CLEAR: ztfi_0087, formid.
  repid_inc = 'ZVRFE_?1'.
  formid    = 'FORM VALIDACION_?2 '.
  LOOP AT total.
    CHECK <mark> NE original.
    zvfi_0087 = total.
    REPLACE '?1' WITH zvfi_0087-grpno INTO repid_inc.
    REPLACE '?2' WITH zvfi_0087-grpno INTO formid.
    SELECT SINGLE * FROM tadir WHERE pgmid    = 'R3TR'
                                 AND object   = 'PROG'
                                 AND obj_name = repid_inc.
    IF sy-subrc = 0.
      EDITOR-CALL FOR REPORT repid_inc DISPLAY-MODE.
    ELSE.
*Se crea Include de la nva Validacion.
      REFRESH include_report.
      CLEAR include_report.
      CONCATENATE formid 'TABLES ' est_lfb1
             INTO include_report-lines
      SEPARATED BY space.
      APPEND include_report. CLEAR include_report.

      WRITE est_lfb1_oc TO include_report-lines+27.
      APPEND include_report. CLEAR include_report.

      WRITE est_lfa1 TO include_report-lines+27.
      APPEND include_report. CLEAR include_report.

      WRITE est_ekko TO include_report-lines+27.
      APPEND include_report. CLEAR include_report.

      WRITE est_ekpo TO include_report-lines+27.
      APPEND include_report. CLEAR include_report.

      WRITE est_zdte_cldoc  TO include_report-lines+27.
      APPEND include_report. CLEAR include_report.

      WRITE est_zdte_dias   TO include_report-lines+27.
      APPEND include_report. CLEAR include_report.

      WRITE est_zdte_posfact   TO include_report-lines+27.
      APPEND include_report. CLEAR include_report.

      WRITE est_ekbe   TO include_report-lines+27.
      APPEND include_report. CLEAR include_report.

      WRITE est_ekkn   TO include_report-lines+27.
      APPEND include_report. CLEAR include_report.

      WRITE est_essr   TO include_report-lines+27.
      APPEND include_report. CLEAR include_report.

      WRITE est_ekko_essr   TO include_report-lines+27.
      APPEND include_report. CLEAR include_report.

      WRITE est_ekpo_essr   TO include_report-lines+27.
      APPEND include_report. CLEAR include_report.

      CONCATENATE 'USING' est_zcb_recfactprov  INTO include_report-lines+21
      SEPARATED BY space.
      APPEND include_report. CLEAR include_report.

      WRITE est_ztfi_0087st_ok   TO include_report-lines+27.
      APPEND include_report. CLEAR include_report.

      WRITE est_ztfi_0087st_nok   TO include_report-lines+27.
      APPEND include_report. CLEAR include_report.

      WRITE 'CHANGING MSGID' TO include_report-lines+18.
      APPEND include_report. CLEAR include_report.

      WRITE 'MSGTY' TO include_report-lines+27.
      APPEND include_report. CLEAR include_report.

      WRITE 'MSGNO' TO include_report-lines+27.
      APPEND include_report. CLEAR include_report.

      WRITE 'MSGV1' TO include_report-lines+27.
      APPEND include_report. CLEAR include_report.

      WRITE 'MSGV2' TO include_report-lines+27.
      APPEND include_report. CLEAR include_report.

      WRITE 'MSGV3' TO include_report-lines+27.
      APPEND include_report. CLEAR include_report.

      WRITE 'MSGV4' TO include_report-lines+27.
      APPEND include_report. CLEAR include_report.

      WRITE 'ESTATUS' TO include_report-lines+27.
      CONCATENATE include_report '.' INTO include_report.
      APPEND include_report. CLEAR include_report.

      APPEND 'ENDFORM.' TO include_report.

      CALL FUNCTION 'SYSTEM_STATUS'
        IMPORTING
          status = xi_status.
*
      tadir-devclass  = 'Z001'.
      tadir-pgmid     = 'R3TR'.
      tadir-object    = 'PROG'.
      tadir-obj_name  = repid_inc.
      tadir-srcsystem = 'SAP'.
      tadir-srcdep    = space.
      tadir-genflag   = 'X'.
      tadir-edtflag   = 'X'.
      tadir-author    = sy-uname.
*
      CALL FUNCTION 'TR_TADIR_INTERFACE'
        EXPORTING
          wi_delete_tadir_entry    = ' '
          wi_remove_repair_flag    = ' '
          wi_set_repair_flag       = ' '
          wi_test_modus            = ' '
          wi_tadir_pgmid           = tadir-pgmid
          wi_tadir_object          = tadir-object
          wi_tadir_obj_name        = tadir-obj_name
          wi_tadir_korrnum         = ' '
          wi_tadir_srcsystem       = tadir-srcsystem
          wi_tadir_author          = tadir-author
          wi_tadir_devclass        = tadir-devclass
          wi_tadir_masterlang      = sy-langu
          wi_remove_genflag        = ' '
          wi_set_genflag           = tadir-genflag
          wi_read_only             = ' '
        IMPORTING
          new_tadir_entry          = xi_new_tadir_entry
        EXCEPTIONS
          tadir_entry_not_existing = 01
          tadir_entry_ill_type     = 02
          no_systemname            = 03
          no_systemtype            = 04
          devclass_not_existing    = 05
          devclass_not_specified   = 06
          OTHERS                   = 07.

      IF sy-subrc NE 0.
        sy-msgty = 'A'.
        CALL FUNCTION 'TR_TADIR_INTERFACE'
          EXPORTING
            wi_delete_tadir_entry = 'X'
            wi_test_modus         = ' '
            wi_tadir_pgmid        = tadir-pgmid
            wi_tadir_object       = tadir-object
            wi_tadir_obj_name     = tadir-obj_name
          EXCEPTIONS
            OTHERS                = 0.
        MESSAGE i068(tk).
        EXIT.
      ENDIF.

      INSERT REPORT repid_inc FROM include_report PROGRAM TYPE 'I'.
      CLEAR xi_error.

      READ REPORT repid_form INTO include_report.
      CONCATENATE 'INCLUDE' repid_inc '.' INTO repid_txt
        SEPARATED BY space.
      APPEND repid_txt TO include_report.
      INSERT REPORT repid_form FROM include_report.

      CALL FUNCTION 'TR_SYS_PARAMS'
        IMPORTING
          systemtype    = t_status
        EXCEPTIONS
          no_systemname = 1
          no_systemtype = 2.

      IF sy-subrc <> 0.
        MESSAGE i105(tk) WITH t_status.
        xi_error = 'Y'.
        EXIT.
      ENDIF.
      CLEAR rfcu2-tbkey.
      rfcu2-tbkey = '?1?2'.
      REPLACE '?1' WITH 'VRFE' INTO rfcu2-tbkey.
      REPLACE '?2' WITH zvfi_0087-grpno INTO rfcu2-tbkey.

      IF NOT repid_inc IS INITIAL.

        FREE itab_ko200.

        CLEAR itab_ko200.
        itab_ko200-pgmid    = 'R3TR'.
        itab_ko200-object   = 'PROG'.
        itab_ko200-obj_name = repid_inc.
        APPEND itab_ko200.

        PERFORM insert_object_task.

        CLEAR itab_ko200.
        itab_ko200-pgmid    = 'R3TR'.
        itab_ko200-object   = 'PROG'.
        itab_ko200-obj_name = repid_form.
        APPEND itab_ko200.

        PERFORM insert_object_task.

      ENDIF.     "t_repname initial

      REFRESH itab_ko200.
      CLEAR itab_ko200.
      itab_ko200-pgmid    = 'R3TR'.
      itab_ko200-object   = 'TABU'.
      itab_ko200-obj_name = 'ZFRM'.
      itab_ko200-objfunc  = 'K'.
      APPEND itab_ko200.

      CLEAR itab_ko200.
      itab_ko200-pgmid    = 'R3TR'.
      itab_ko200-object   = 'TABU'.
      itab_ko200-obj_name = 'ZFRMT'.
      itab_ko200-objfunc  = 'K'.
      APPEND itab_ko200.

* specified keys for TABU entries
      REFRESH itab_e071k.
      CLEAR itab_e071k.
      itab_e071k-pgmid      = 'R3TR'.
      itab_e071k-object     = 'TABU'.
      itab_e071k-objname    = 'ZFRM'.
      itab_e071k-tabkey     = rfcu2-tbkey.
      itab_e071k-mastertype = 'TABU'.
      itab_e071k-mastername = 'ZFRM'.
      APPEND itab_e071k.

      CLEAR itab_e071k.
      itab_e071k-pgmid       = 'R3TR'.
      itab_e071k-object      = 'TABU'.
      itab_e071k-objname     = 'ZFRMT'.
      itab_e071k-tabkey(1)   = sy-langu.
      itab_e071k-tabkey+1(7) = rfcu2-tbkey.
      itab_e071k-mastertype  = 'TABU'.
      itab_e071k-mastername  = 'ZFRMT'.
      APPEND itab_e071k.
* Check and insert TABU entries
      CALL FUNCTION 'TR_OBJECTS_CHECK'
        EXPORTING
          iv_no_show_option       = 'X'
        TABLES
          wt_ko200                = itab_ko200
        EXCEPTIONS
          cancel_edit_other_error = 01
          show_only_other_error   = 02.

      IF sy-subrc = 0.
        CALL FUNCTION 'TR_OBJECTS_INSERT'
          EXPORTING
            wi_order                = v_order
          TABLES
            wt_ko200                = itab_ko200
            wt_e071k                = itab_e071k
          EXCEPTIONS
            cancel_edit_other_error = 01
            show_only_other_error   = 02
            OTHERS                  = 03.
* not possible to append PROG entry to transport request.
* Inform user but continue processing.
        IF sy-subrc <> 0.
          MESSAGE s107(tk) WITH v_order.
        ENDIF.
      ELSE.
* inculde template already has been created but user should not be
* able to modify it without fitting object key.
        xi_error = 'Y'.
        MESSAGE i224(tk).
        EXIT.
      ENDIF.
      EDITOR-CALL FOR REPORT repid_inc.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " OBTAIN_DATA_RECORD

*&---------------------------------------------------------------------*
*&      Form  INSERT_OBJECT_TASK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM insert_object_task .

* check PROG/REPS object first, ask for object registration key
  CALL FUNCTION 'TR_OBJECT_CHECK'
    EXPORTING
      wi_ko200                = itab_ko200
      iv_no_show_option       = 'X'
    IMPORTING
      we_ko200                = itab_ko200
    EXCEPTIONS
      cancel_edit_other_error = 1
      show_only_other_error   = 2.

* insert entry, ask for transport request
  IF sy-subrc = 0.
    CALL FUNCTION 'TR_OBJECT_INSERT'
      EXPORTING
        wi_ko200                = itab_ko200
        iv_no_show_option       = 'X'
      IMPORTING
        we_order                = v_order
        we_task                 = v_task
      EXCEPTIONS
        cancel_edit_other_error = 1
        show_only_other_error   = 2.
    IF sy-subrc <> 0.
      MESSAGE s107(tk) WITH v_order.
    ENDIF.
  ELSE.
    MESSAGE i244(tk).
    xi_error = 'Y'.
*    EXIT.
  ENDIF.   "object checked sucessfully

ENDFORM.                    " INSERT_OBJECT_TASK
