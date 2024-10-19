*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTFI_0092.......................................*
DATA:  BEGIN OF STATUS_ZTFI_0092                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFI_0092                     .
CONTROLS: TCTRL_ZTFI_0092
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZTFI_0092                     .
TABLES: ZTFI_0092                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
