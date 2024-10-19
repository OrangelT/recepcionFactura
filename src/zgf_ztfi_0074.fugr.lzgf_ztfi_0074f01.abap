*----------------------------------------------------------------------*
***INCLUDE LZGF_ZTFI_0074F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  fo_table_get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fo_table_get_data.
  CONSTANTS maxsellines TYPE i VALUE 500.
  DATA: tgd_sellist LIKE vimsellist OCCURS 10, tgd_sel LIKE vimsellist,
        tgd_sellangu LIKE vimsellist,
        short_sellist LIKE vimsellist OCCURS 10,
        short_sel LIKE vimsellist,
        tgd_ind TYPE i, tgd_field LIKE vimnamtab-viewfield, "#EC NEEDED
        selnumber TYPE i, selindex TYPE i, selcut TYPE i,
        selpieces TYPE i.
  FIELD-SYMBOLS: <text_key>.                                "#EC *
  DATA: primtab TYPE REF TO data, texttab TYPE REF TO data,
        w_texttab_save TYPE REF TO data, w_texttab TYPE REF TO data,
        text_keyflds TYPE vim_flds_tab_type.
  DATA: append_flag(1) TYPE c.

  FIELD-SYMBOLS: <primtab> TYPE STANDARD TABLE,
                 <texttab> TYPE SORTED TABLE, <w_texttab> TYPE ANY,
                 <w_texttab_save> TYPE ANY, <textline_x> TYPE x.

  REFRESH total. CLEAR total.
  IF x_header-selection NE space.
    DESCRIBE TABLE dba_sellist LINES selnumber.
    IF selnumber > maxsellines.  "fragmentation of too large sellists
      CLEAR selpieces.
      CLEAR selindex.
      CREATE DATA primtab TYPE STANDARD TABLE OF (x_header-maintview).
      ASSIGN primtab->* TO <primtab>.
      WHILE selindex < selnumber.
        selpieces = selpieces + maxsellines.
        REFRESH short_sellist.
        CLEAR selcut.
        WHILE selcut EQ 0 AND selindex < selnumber.
          selindex = selindex + 1.
          READ TABLE dba_sellist INTO short_sel INDEX selindex.
          APPEND short_sel TO short_sellist.
          IF selindex > selpieces AND short_sel-and_or NE 'AND'.
            selcut = 1.
          ENDIF.
        ENDWHILE.
        CLEAR short_sel-and_or.      "last line without logic operation
        MODIFY short_sellist FROM short_sel INDEX selindex.
        CALL FUNCTION 'VIEW_FILL_WHERETAB'
           EXPORTING
                tablename               = x_header-maintview
*           ONLY_CNDS_FOR_KEYFLDS   = 'X' "use default SPACE
           TABLES
                sellist                 = short_sellist
                wheretab                = vim_wheretab
                x_namtab                = x_namtab
           EXCEPTIONS                                       "#EC *
                no_conditions_for_table = 01.
*       read data from database with morer wheretabs...................*
        SELECT * FROM (x_header-maintview) APPENDING TABLE <primtab>
                                          WHERE (vim_wheretab).
        CLEAR selcut.
      ENDWHILE.
    ELSE.                                  "selnumber > maxsellines
      CALL FUNCTION 'VIEW_FILL_WHERETAB'
           EXPORTING
                tablename               = x_header-maintview
*         ONLY_CNDS_FOR_KEYFLDS   = 'X' "use default SPACE
           TABLES
                sellist                 = dba_sellist
                wheretab                = vim_wheretab
                x_namtab                = x_namtab
           EXCEPTIONS                                       "#EC *
                no_conditions_for_table = 01.
*   read data from database with one wheretab..........................*
      CREATE DATA primtab TYPE STANDARD TABLE OF (x_header-maintview)."UCb
      ASSIGN primtab->* TO <primtab>.
      SELECT * FROM (x_header-maintview) INTO TABLE <primtab>
                                          WHERE (vim_wheretab).
    ENDIF.                                   "if selnumber > maxsellines
  ELSE.                                  "if x_header-selection NE space
    REFRESH vim_wheretab.
*   read data from database without wheretab...........................*
    CREATE DATA primtab TYPE STANDARD TABLE OF (x_header-maintview)."UCb
    ASSIGN primtab->* TO <primtab>.
    SELECT * FROM (x_header-maintview) INTO TABLE <primtab>.
  ENDIF.                                "if x_header-selection NE space

**//.. Filtrar datos por autorizarión
  DATA: lt_ztfi_0074 TYPE TABLE OF ztfi_0074,
        ls_ztfi_0074 TYPE ztfi_0074.

  LOOP AT <primtab> INTO <vim_total_struc>.
    ls_ztfi_0074 = <vim_total_struc>.

    PERFORM fo_authority_check USING    ls_ztfi_0074-bukrs
                               CHANGING sy-subrc.
    IF sy-subrc NE 0.
      DELETE <primtab>.
    ENDIF.

  ENDLOOP.
**//..

  IF x_header-texttbexst EQ space.
* no texttable
    LOOP AT <primtab> INTO <vim_total_struc>.
      APPEND total.
    ENDLOOP.                                                "UCe
    SORT total BY <vim_xtotal_key>. <status>-alr_sorted = 'R'.
    IF x_header-selection EQ space AND x_header-delmdtflag NE space.
* time dependence
      PERFORM build_mainkey_tab_0.
    ENDIF.
    LOOP AT total.
      CLEAR: <action>, <mark>.
      MODIFY total.
      IF x_header-selection EQ space AND x_header-delmdtflag NE space.
        PERFORM build_mainkey_tab_1.
      ENDIF.
    ENDLOOP.
    IF x_header-selection EQ space AND x_header-delmdtflag NE space.
      PERFORM build_mainkey_tab_2.
    ENDIF.
  ELSE.
* texttable exists
    PERFORM vim_get_text_keyflds USING x_header-texttab
                                 CHANGING text_keyflds.
    CREATE DATA texttab TYPE SORTED TABLE OF (x_header-texttab)
     WITH UNIQUE KEY (text_keyflds).                        "UCb
    ASSIGN texttab->* TO <texttab>.
    IF x_header-selection NE space.
* get selection for texttable
*      READ TABLE dba_sellist INTO dpl_sellist INDEX 1.
      DESCRIBE TABLE dba_sellist LINES selnumber.
      selindex = 0.
      WHILE selindex < selnumber.
        selindex = selindex + 1.
        READ TABLE dba_sellist INTO tgd_sel INDEX selindex.
        READ TABLE x_namtab WITH KEY
          viewfield = tgd_sel-viewfield texttabfld = space. "#EC *
        CHECK x_namtab-keyflag = 'X'.        " key fields for texttab only
        tgd_sel-viewfield = x_namtab-txttabfldn.
        READ TABLE x_namtab WITH KEY
          viewfield = tgd_sel-viewfield texttabfld = 'X'.
        tgd_sel-tabix = sy-tabix.
        CLEAR append_flag.
        IF sy-subrc EQ 0."Otherwise keyfld in tab not in txttab HW696310
          append_flag = 'X'.
        ENDIF.
        IF tgd_sel-and_or NE 'AND' OR selindex = 1.       "Langufield
          READ TABLE x_namtab WITH KEY primtabkey = 0 keyflag = 'X'. "#EC *
          tgd_sellangu-viewfield = x_namtab-viewfield.
          tgd_sellangu-tabix     = sy-tabix.
          tgd_sellangu-operator = 'EQ'.
          tgd_sellangu-value = sy-langu.
          tgd_sellangu-and_or = 'AND'.
          IF tgd_sellangu-value EQ space.
            tgd_sellangu-initial = 'X'.
          ENDIF.
          tgd_sellangu-cond_kind = dpl_sellist-cond_kind.
          CLEAR tgd_sellangu-converted.
          APPEND tgd_sellangu TO tgd_sellist.
        ENDIF.
        IF append_flag EQ 'X'.
          APPEND tgd_sel TO tgd_sellist.
        ENDIF.
*      Did not work for sellist to describe more than one dataset in
*      transport request                                          "HCG
*      LOOP AT x_namtab WHERE keyflag NE space    "fill sellist for
*                         AND texttabfld NE space. "texttab
*        tgd_field = x_namtab-viewfield.
*        tgd_ind   = sy-tabix.
*        IF x_namtab-primtabkey EQ 0.   "langufield
*          tgd_sel-viewfield = tgd_field.
*          tgd_sel-tabix     = tgd_ind.
*          tgd_sel-operator = 'EQ'.
*          tgd_sel-value = sy-langu.
*          tgd_sel-and_or = 'AND'.
*          IF tgd_sel-value EQ space.
*            tgd_sel-initial = 'X'.
*          ENDIF.
*          tgd_sel-cond_kind = dpl_sellist-cond_kind.
*          clear tgd_sel-converted.
*          APPEND tgd_sel TO tgd_sellist.
*        ELSE.
*          READ TABLE x_namtab INDEX x_namtab-primtabkey.
*          LOOP AT dba_sellist WHERE viewfield EQ x_namtab-viewfield.
*            tgd_sel = dba_sellist.
*            tgd_sel-viewfield = tgd_field.
*            tgd_sel-tabix     = tgd_ind.
*            IF tgd_sel-and_or EQ space.
*              tgd_sel-and_or = 'AND'.
*            ENDIF.
*            APPEND tgd_sel TO tgd_sellist.
*          ENDLOOP.
*        ENDIF.
*      ENDLOOP.
      ENDWHILE.
      DESCRIBE TABLE tgd_sellist.
      READ TABLE tgd_sellist INDEX sy-tfill INTO tgd_sel.
      IF tgd_sel-and_or NE space.
        CLEAR tgd_sel-and_or.
        MODIFY tgd_sellist INDEX sy-tfill FROM tgd_sel.
      ENDIF.
    ELSE.
* no selection for primary table: fill selection with langu-field only
      LOOP AT x_namtab WHERE keyflag NE space    "fill sellist with
                         AND texttabfld NE space  "language condition
                         AND primtabkey EQ 0.
        tgd_sel-viewfield = x_namtab-viewfield.
        tgd_sel-tabix     = sy-tabix.
        tgd_sel-operator = 'EQ'.
        tgd_sel-value = sy-langu.
        tgd_sel-and_or = space.
        IF tgd_sel-value EQ space.
          tgd_sel-initial = 'X'.
        ENDIF.
        APPEND tgd_sel TO tgd_sellist.
        EXIT.
      ENDLOOP.
    ENDIF.
*    CALL FUNCTION 'VIEW_FILL_WHERETAB'
*      EXPORTING
*        tablename               = x_header-texttab
*        only_cnds_for_keyflds   = 'X'
*      TABLES
*        sellist                 = tgd_sellist
*        wheretab                = vim_wheretab
*        x_namtab                = x_namtab
*      EXCEPTIONS
*        no_conditions_for_table = 01.
** read texttable from database
*    SELECT * FROM (x_header-texttab) INTO TABLE <texttab>
*                                      WHERE (vim_wheretab).
    DESCRIBE TABLE tgd_sellist LINES selnumber.
    IF selnumber > maxsellines.  "fragmentation of too large sellists
      CLEAR selpieces.
      CLEAR selindex.
      WHILE selindex < selnumber.
        selpieces = selpieces + maxsellines.
        REFRESH short_sellist.
        CLEAR selcut.
        WHILE selcut EQ 0 AND selindex < selnumber.
          selindex = selindex + 1.
          READ TABLE tgd_sellist INTO short_sel INDEX selindex.
          APPEND short_sel TO short_sellist.
          IF selindex > selpieces AND short_sel-and_or NE 'AND'.
            selcut = 1.
          ENDIF.
        ENDWHILE.
        CLEAR short_sel-and_or.      "last line without logic operation
        MODIFY short_sellist FROM short_sel INDEX selindex.
        CALL FUNCTION 'VIEW_FILL_WHERETAB'
          EXPORTING
            tablename               = x_header-texttab
            only_cnds_for_keyflds   = 'X'
          TABLES
            sellist                 = short_sellist
            wheretab                = vim_wheretab
            x_namtab                = x_namtab
          EXCEPTIONS                                        "#EC *
            no_conditions_for_table = 01.
*       read data from database with morer wheretabs...................*
        SELECT * FROM (x_header-texttab) APPENDING TABLE <texttab>
                                          WHERE (vim_wheretab).
        CLEAR selcut.
      ENDWHILE.
    ELSE.                                  "selnumber > maxsellines
      CALL FUNCTION 'VIEW_FILL_WHERETAB'
        EXPORTING
          tablename               = x_header-texttab
          only_cnds_for_keyflds   = 'X'
        TABLES
          sellist                 = tgd_sellist
          wheretab                = vim_wheretab
          x_namtab                = x_namtab
        EXCEPTIONS                                          "#EC *
          no_conditions_for_table = 01.
*   read data from database with one wheretab..........................*
      SELECT * FROM (x_header-texttab) INTO TABLE <texttab>
                                        WHERE (vim_wheretab).
    ENDIF.                                   "if selnumber > maxsellines
    IF x_header-selection EQ space AND x_header-delmdtflag NE space.
      PERFORM build_mainkey_tab_0.
    ENDIF.
    CREATE DATA w_texttab_save TYPE (x_header-texttab).
    CREATE DATA w_texttab TYPE (x_header-texttab).
    ASSIGN: w_texttab->* TO <w_texttab>,
            w_texttab_save->* TO <w_texttab_save>,
            <w_texttab_save> TO <textline_x> CASTING.
    LOOP AT <primtab> INTO <vim_total_struc>.
*       hier aufbauen schlüssel texttabelle in feld text_key
      CLEAR <w_texttab>.
      PERFORM fill_texttab_key_uc USING <vim_total_struc>
                                  CHANGING <w_texttab>.
      IF <w_texttab> NE <w_texttab_save>.
        READ TABLE <texttab> INTO <w_texttab_save>
         FROM <w_texttab>.
        IF sy-subrc = 0.
*          MOVE <w_texttab_save> TO <w_textline>.
*          MOVE textline(x_header-texttablen) TO <total_text>.
        ELSE.
          MOVE <text_initial> TO <w_texttab_save>.
        ENDIF.
*      ELSE.
*        MOVE <w_texttab_save> TO <w_textline>.
*        MOVE textline(x_header-texttablen) TO <total_text>.
      ENDIF.
      MOVE <textline_x> TO <vim_xtotal_text>.
      CLEAR: <action>, <mark>, <action_text>.
      APPEND total.
      IF x_header-selection EQ space AND x_header-delmdtflag NE space.
        PERFORM build_mainkey_tab_1.
      ENDIF.
    ENDLOOP.
    SORT total BY <vim_xtotal_key>. <status>-alr_sorted = 'R'.
    IF x_header-selection EQ space AND x_header-delmdtflag NE space.
      PERFORM build_mainkey_tab_2.
    ENDIF.
  ENDIF.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF x_header-selection NE space.
    PERFORM check_dynamic_select_options.
  ENDIF.
ENDFORM.                    "fo_table_get_data

*&---------------------------------------------------------------------*
*&      Form  FO_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_U_BUKRS  text
*      <--P_SY_SUBRC  text
*----------------------------------------------------------------------*
FORM fo_authority_check  USING    u_bukrs TYPE bukrs
                         CHANGING c_subrc TYPE sysubrc.
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
           ID 'BUKRS' FIELD u_bukrs
           ID 'ACTVT' FIELD '01'
           ID 'ACTVT' FIELD '02'.
  c_subrc = sy-subrc.
ENDFORM.                    " FO_AUTHORITY_CHECK

*&---------------------------------------------------------------------*
*&      Form  FO_NEW_ENTRY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fo_new_entry.

  PERFORM fo_authority_check USING    ztfi_0074-bukrs
                             CHANGING sy-subrc.
  IF sy-subrc NE 0.
    MESSAGE e800(fr) WITH ztfi_0074-bukrs.
  ENDIF.
ENDFORM. " FO_NEW_ENTRY
