*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTFI_0080.......................................*
DATA:  BEGIN OF STATUS_ZTFI_0080                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFI_0080                     .
CONTROLS: TCTRL_ZTFI_0080
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZTFI_0080                     .
TABLES: ZTFI_0080                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
