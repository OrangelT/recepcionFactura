*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTFI_0089.......................................*
DATA:  BEGIN OF STATUS_ZTFI_0089                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFI_0089                     .
CONTROLS: TCTRL_ZTFI_0089
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZTFI_0089                     .
TABLES: ZTFI_0089                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
