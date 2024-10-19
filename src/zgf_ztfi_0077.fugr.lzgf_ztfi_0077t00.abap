*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTFI_0077.......................................*
DATA:  BEGIN OF STATUS_ZTFI_0077                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFI_0077                     .
CONTROLS: TCTRL_ZTFI_0077
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZTFI_0077                     .
TABLES: ZTFI_0077                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
