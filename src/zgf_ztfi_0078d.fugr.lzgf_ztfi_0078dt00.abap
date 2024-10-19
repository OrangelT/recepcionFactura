*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTFI_0078D......................................*
DATA:  BEGIN OF STATUS_ZTFI_0078D                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFI_0078D                    .
CONTROLS: TCTRL_ZTFI_0078D
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZTFI_0078D                    .
TABLES: ZTFI_0078D                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
