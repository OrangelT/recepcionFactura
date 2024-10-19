*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTFI_0078B......................................*
DATA:  BEGIN OF STATUS_ZTFI_0078B                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFI_0078B                    .
CONTROLS: TCTRL_ZTFI_0078B
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZTFI_0078B                    .
TABLES: ZTFI_0078B                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
