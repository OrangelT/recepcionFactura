*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTFI_0078E......................................*
DATA:  BEGIN OF STATUS_ZTFI_0078E                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFI_0078E                    .
CONTROLS: TCTRL_ZTFI_0078E
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZTFI_0078E                    .
TABLES: ZTFI_0078E                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
