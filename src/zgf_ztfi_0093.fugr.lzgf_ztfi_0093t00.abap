*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTFI_0093.......................................*
DATA:  BEGIN OF STATUS_ZTFI_0093                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFI_0093                     .
CONTROLS: TCTRL_ZTFI_0093
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZTFI_0093                     .
TABLES: ZTFI_0093                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
