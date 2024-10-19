*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTFI_0090.......................................*
DATA:  BEGIN OF STATUS_ZTFI_0090                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFI_0090                     .
CONTROLS: TCTRL_ZTFI_0090
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZTFI_0090                     .
TABLES: ZTFI_0090                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
