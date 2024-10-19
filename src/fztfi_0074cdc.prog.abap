FORM CD_CALL_ZTFI_0074.
  IF   ( UPD_ZTFI_0074 NE SPACE )
    OR ( UPD_ICDTXT_ZTFI_0074 NE SPACE )
  .
    CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
    CALL FUNCTION 'ZTFI_0074_WRITE_DOCUMENT' IN UPDATE TASK
        EXPORTING
          OBJECTID                = OBJECTID
          TCODE                   = TCODE
          UTIME                   = UTIME
          UDATE                   = UDATE
          USERNAME                = USERNAME
          PLANNED_CHANGE_NUMBER   = PLANNED_CHANGE_NUMBER
          OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
          PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
          NO_CHANGE_POINTERS      = CDOC_NO_CHANGE_POINTERS
* updateflag of ZTFI_0074
          UPD_ZTFI_0074
                      = UPD_ZTFI_0074
          UPD_ICDTXT_ZTFI_0074
                      = UPD_ICDTXT_ZTFI_0074
        TABLES
          ICDTXT_ZTFI_0074
                      = ICDTXT_ZTFI_0074
          XZTFI_0074
                      = XZTFI_0074
          YZTFI_0074
                      = YZTFI_0074
    .
  ENDIF.
  CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
