*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZRECFACTMAP.....................................*
DATA:  BEGIN OF STATUS_ZRECFACTMAP                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZRECFACTMAP                   .
CONTROLS: TCTRL_ZRECFACTMAP
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: *ZRECFACTMAP                   .
TABLES: ZRECFACTMAP                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
