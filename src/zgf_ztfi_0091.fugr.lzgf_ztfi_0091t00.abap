*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTFI_0091.......................................*
DATA:  BEGIN OF STATUS_ZTFI_0091                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFI_0091                     .
CONTROLS: TCTRL_ZTFI_0091
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZTFI_0091                     .
TABLES: ZTFI_0091                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
