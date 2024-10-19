FUNCTION-POOL ZGF_ZVFI_0087              MESSAGE-ID SV.

**//..
TABLES: rfcu2.

DATA formid(30)    TYPE c.
DATA repid_inc     LIKE sy-repid.
DATA repid_txt(60) TYPE c.

DATA: BEGIN OF include_report OCCURS 100,
        lines(80),
      END OF include_report.

CONSTANTS est_lfb1(22) TYPE c         VALUE 'lt_lfb1 STRUCTURE lfb1'.
CONSTANTS est_lfb1_oc(25) TYPE c      VALUE 'lt_lfb1_oc STRUCTURE lfb1'.
CONSTANTS est_lfa1(22) TYPE c         VALUE 'lt_lfa1 STRUCTURE lfa1'.
CONSTANTS est_ekko(22) TYPE c         VALUE 'lt_ekko STRUCTURE ekko'.
CONSTANTS est_ekpo(22) TYPE c         VALUE 'lt_ekpo STRUCTURE ekpo'.
CONSTANTS est_zdte_cldoc(34)   TYPE c VALUE 'lt_zdte_cldoc STRUCTURE ztfi_0001'.
CONSTANTS est_zdte_dias(34)    TYPE c VALUE 'lt_zdte_dias STRUCTURE  ztfi_0076'.
CONSTANTS est_zdte_posfact(35) TYPE c VALUE 'lt_zdte_posfact STRUCTURE ztfi_0075'.
CONSTANTS est_zdt_recref(34)   TYPE c VALUE 'lt_zdt_recref STRUCTURE ztfi_0077'.
CONSTANTS est_ekbe(22) TYPE c         VALUE 'lt_ekbe STRUCTURE ekbe'.
CONSTANTS est_ekkn(22) TYPE c         VALUE 'lt_ekkn STRUCTURE ekkn'.
CONSTANTS est_essr(27) TYPE c         VALUE 'TI_ESSR structure zty_ess'.
CONSTANTS est_ekko_essr(27) TYPE c    VALUE 'ti_ekko_essr structure ekko'.
CONSTANTS est_ekpo_essr(27) TYPE c    VALUE 'ti_ekpo_essr structure ekpo'.
CONSTANTS est_zcb_recfactprov(40) TYPE c  VALUE 'lt_zcb_recfactprov STRUCTURE  ztfi_0074'.
CONSTANTS est_ztfi_0087st_ok(27) TYPE c   VALUE 'ztfi_0087-st_ok'.
CONSTANTS est_ztfi_0087st_nok(27) TYPE c  VALUE 'ztfi_0087-st_nok'.
CONSTANTS repid_form   LIKE sy-repid      VALUE 'ZFIR_0062_FORM'.

DATA xi_status          LIKE itcda-tdstatus.
* INCLUDE LZVD_ZTFI_0086D...                 " Local class definition
DATA xi_new_tadir_entry LIKE tadir.
DATA xi_error(1)        TYPE c.
DATA t_status           LIKE sy-sysid.
DATA v_order            TYPE e071-trkorr.
DATA v_task             TYPE e071-trkorr.

DATA: BEGIN OF itab_e071k OCCURS 10.
        INCLUDE STRUCTURE e071k.
DATA: END OF itab_e071k.

DATA: BEGIN OF itab_ko200 OCCURS 10.
        INCLUDE STRUCTURE ko200.
DATA: END OF itab_ko200.

**//..

* INCLUDE LZGF_ZVFI_0087D...                 " Local class definition
  INCLUDE LSVIMDAT                                . "general data decl.
  INCLUDE LZGF_ZVFI_0087T00                       . "view rel. data dcl.
