*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGF_ZTFI_0092
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGF_ZTFI_0092      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
