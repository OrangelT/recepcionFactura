*----------------------------------------------------------------------*
***INCLUDE LZGF_ZVFI_0074F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  FO_NEW_ENTRY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FO_NEW_ENTRY.

  PERFORM FO_AUTHORITY_CHECK USING    ZVFI_0074-BUKRS
                             CHANGING SY-SUBRC.
  IF SY-SUBRC NE 0.
    MESSAGE E800(FR) WITH ZVFI_0074-BUKRS.
  ENDIF.
ENDFORM. " FO_NEW_ENTRY
*&---------------------------------------------------------------------*
*&      Form  FO_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZVFI_0074_BUKRS  text
*      <--P_SY_SUBRC  text
*----------------------------------------------------------------------*
FORM FO_AUTHORITY_CHECK  USING    P_ZVFI_0074_BUKRS
                         CHANGING P_SY_SUBRC.
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
         ID 'BUKRS' FIELD P_ZVFI_0074_BUKRS
         ID 'ACTVT' FIELD '01'
         ID 'ACTVT' FIELD '02'.
  P_SY_SUBRC = SY-SUBRC.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  fo_table_get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FO_TABLE_GET_DATA.
  DATA: LS_DATA_EXTRACT TYPE TY_EXTRACT.
  FIELD-SYMBOLS: <FS_DATA_TOTAL> TYPE ANY.

  PERFORM GET_DATA_ZVFI_0074.
  PERFORM ELIMABLOQ.

  LOOP AT TOTAL.
    ASSIGN TOTAL TO <FS_DATA_TOTAL>.
    LS_DATA_EXTRACT = <FS_DATA_TOTAL>.


    PERFORM FO_AUTHORITY_CHECK USING    LS_DATA_EXTRACT-BUKRS
                               CHANGING SY-SUBRC.
    IF SY-SUBRC NE 0.
      DELETE TOTAL.
    ELSE.
      APPEND  LS_DATA_EXTRACT TO TAB_FI_0074_OLD.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "fo_table_get_data

FORM FO_BLOQUEO.
  DATA: LS_DATA_EXTRACT TYPE TY_EXTRACT.
  DATA: L_SUBRC TYPE SY-SUBRC.
  FIELD-SYMBOLS: <FS_DATA_TOTAL> TYPE ANY.

  LOOP AT TOTAL.
    ASSIGN TOTAL TO <FS_DATA_TOTAL>.
    LS_DATA_EXTRACT = <FS_DATA_TOTAL>.
    IF LS_DATA_EXTRACT-ACTION = 'U'.
      L_SUBRC = 0.
      PERFORM BLOQUEO USING LS_DATA_EXTRACT CHANGING L_SUBRC.
      IF L_SUBRC = '4' OR L_SUBRC = '5'.
        IF L_SUBRC = '4'.
        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            TITEL = text-001
            TXT1  = text-101
            TXT2  = ''.
          MESSAGE S398(00) WITH TEXT-101 SPACE SPACE SPACE.
        ELSE.
        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            TITEL = text-001
            TXT1  = text-102
            TXT2  = ''.
          MESSAGE S398(00) WITH TEXT-102 SPACE SPACE SPACE.
        ENDIF.
        SY-SUBRC = '4'.
        VIM_ABORT_SAVING = 'X'.
        EXIT.
      ENDIF.
      APPEND LS_DATA_EXTRACT TO TAB_FI_0074_MD.
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM FO_DESBLOQUEO.
  DATA: LS_DATA_EXTRACT TYPE TY_EXTRACT.
  FIELD-SYMBOLS: <FS_DATA_TOTAL> TYPE ANY.

  LOOP AT TAB_FI_0074_MD ASSIGNING <FS_DATA_TOTAL>.
    LS_DATA_EXTRACT = <FS_DATA_TOTAL>.
    IF LS_DATA_EXTRACT-ACTION = 'U'.
      PERFORM DESBLOQUEO USING LS_DATA_EXTRACT.
    ENDIF.
  ENDLOOP.
  REFRESH TAB_FI_0074_MD.
  TAB_FI_0074_OLD[] = TOTAL[].
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BLOQUEO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_DATA_EXTRACT  text
*----------------------------------------------------------------------*
FORM BLOQUEO  USING    LS_DATA_EXTRACT TYPE TY_EXTRACT  CHANGING L_SUBRC TYPE SY-SUBRC.

  DATA: P_EBELN TYPE EKKO-EBELN.
  DATA: LS_OLD TYPE TY_EXTRACT.
  CLEAR P_EBELN. CLEAR LS_OLD.
  SELECT SINGLE EBELN INTO P_EBELN FROM ZTFI_0074
  WHERE BUKRS = LS_DATA_EXTRACT-BUKRS AND
  XBLNR = LS_DATA_EXTRACT-XBLNR AND
  LIFNR = LS_DATA_EXTRACT-LIFNR AND
  TIPODTE = LS_DATA_EXTRACT-TIPODTE.

  READ TABLE TAB_FI_0074_OLD INTO LS_OLD WITH KEY BUKRS = LS_DATA_EXTRACT-BUKRS
                                                XBLNR = LS_DATA_EXTRACT-XBLNR
                                                LIFNR = LS_DATA_EXTRACT-LIFNR
                                                TIPODTE = LS_DATA_EXTRACT-TIPODTE.

  IF  P_EBELN EQ  LS_OLD-EBELN.
    CALL FUNCTION 'ENQUEUE_EZTFI_0074'
      EXPORTING
        MODE_ZTFI_0074 = 'E'
        MANDT          = SY-MANDT
        BUKRS          = LS_DATA_EXTRACT-BUKRS
        XBLNR          = LS_DATA_EXTRACT-XBLNR
        LIFNR          = LS_DATA_EXTRACT-LIFNR
        TIPODTE        = LS_DATA_EXTRACT-TIPODTE
*       X_BUKRS        = ' '
*       X_XBLNR        = ' '
*       X_LIFNR        = ' '
*       X_TIPODTE      = ' '
*       _SCOPE         = '2'
*       _WAIT          = ' '
*       _COLLECT       = ' '
      EXCEPTIONS
        FOREIGN_LOCK   = 1
        SYSTEM_FAILURE = 2
        OTHERS         = 3.
    IF SY-SUBRC NE 0.
      L_SUBRC = 4.
      MESSAGE S398(00) WITH TEXT-101 SPACE SPACE SPACE.
      EXIT.
    ENDIF.
  ELSE.
    L_SUBRC = 5.
    MESSAGE S398(00) WITH TEXT-102 SPACE SPACE SPACE.
    EXIT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DESBLOQUEO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_DATA_EXTRACT  text
*----------------------------------------------------------------------*
FORM DESBLOQUEO  USING    LS_DATA_EXTRACT TYPE TY_EXTRACT.
  CALL FUNCTION 'DEQUEUE_EZTFI_0074'
    EXPORTING
      MODE_ZTFI_0074 = 'E'
      MANDT          = SY-MANDT
      BUKRS          = LS_DATA_EXTRACT-BUKRS
      XBLNR          = LS_DATA_EXTRACT-XBLNR
      LIFNR          = LS_DATA_EXTRACT-LIFNR
      TIPODTE        = LS_DATA_EXTRACT-TIPODTE
**         X_BUKRS        = ' '
**         X_XBLNR        = ' '
**         X_LIFNR        = ' '
**         X_TIPODTE      = ' '
**         _SCOPE         = '3'
**         _SYNCHRON      = ' '
**         _COLLECT       = ' '
    .

ENDFORM.
FORM ELIMABLOQ.
  DATA: ENQ TYPE STANDARD TABLE OF SEQG3.
  DATA: L_ENQ TYPE SEQG3.
  DATA: RC TYPE SY-SUBRC.
  CALL FUNCTION 'ENQUEUE_READ'
    EXPORTING
      GCLIENT = SY-MANDT
      GNAME   = 'RSTABLE'
      GARG    = 'ZTFI_0074*'
*     GUNAME  = SY-UNAME
*     LOCAL   = ' '
*     FAST    = ' '
*     GARGNOWC                    = ' '
* IMPORTING
*     NUMBER  =
*     SUBRC   =
    TABLES
      ENQ     = ENQ
* EXCEPTIONS
*     COMMUNICATION_FAILURE       = 1
*     SYSTEM_FAILURE              = 2
*     OTHERS  = 3
    .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
  IF SY-SUBRC = 0.
    CALL FUNCTION 'ENQUE_DELETE'
      EXPORTING
        SUPPRESS_SYSLOG_ENTRY = 'X'
      IMPORTING
        SUBRC                 = RC
      TABLES
        ENQ                   = ENQ.
  ENDIF.
ENDFORM.
