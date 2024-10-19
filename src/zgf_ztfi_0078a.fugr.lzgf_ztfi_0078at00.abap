*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTFI_0078A......................................*
DATA:  BEGIN OF STATUS_ZTFI_0078A                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFI_0078A                    .
CONTROLS: TCTRL_ZTFI_0078A
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZTFI_0078A                    .
TABLES: ZTFI_0078A                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
