*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTFI_0087.......................................*
DATA:  BEGIN OF STATUS_ZTFI_0087                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFI_0087                     .
CONTROLS: TCTRL_ZTFI_0087
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZTFI_0087                     .
TABLES: ZTFI_0087                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
