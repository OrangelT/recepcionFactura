*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTFI_0078C......................................*
DATA:  BEGIN OF STATUS_ZTFI_0078C                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFI_0078C                    .
CONTROLS: TCTRL_ZTFI_0078C
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZTFI_0078C                    .
TABLES: ZTFI_0078C                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
