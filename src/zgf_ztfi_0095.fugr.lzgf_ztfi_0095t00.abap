*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTFI_0095.......................................*
DATA:  BEGIN OF STATUS_ZTFI_0095                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFI_0095                     .
CONTROLS: TCTRL_ZTFI_0095
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZTFI_0095                     .
TABLES: ZTFI_0095                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
