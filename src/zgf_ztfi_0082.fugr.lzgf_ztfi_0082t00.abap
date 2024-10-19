*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTFI_0082.......................................*
DATA:  BEGIN OF STATUS_ZTFI_0082                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFI_0082                     .
CONTROLS: TCTRL_ZTFI_0082
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZTFI_0082                     .
TABLES: ZTFI_0082                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
