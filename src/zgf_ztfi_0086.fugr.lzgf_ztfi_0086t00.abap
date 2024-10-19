*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTFI_0086.......................................*
DATA:  BEGIN OF STATUS_ZTFI_0086                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFI_0086                     .
CONTROLS: TCTRL_ZTFI_0086
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZTFI_0086                     .
TABLES: ZTFI_0086                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
