*&---------------------------------------------------------------------*
*& Include          ZCOR0230F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SET_SCREEN
*&---------------------------------------------------------------------*
FORM SET_SCREEN .

  LOOP AT SCREEN.
*    IF SCREEN-NAME = 'PA_KOKRS' OR SCREEN-NAME = 'PA_VERSN'.
    IF SCREEN-NAME = 'PA_KOKRS'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F4_PDGR
*&---------------------------------------------------------------------*
FORM F4_PDGR  CHANGING P_PDGR.

  DATA: HELP_SETNR     LIKE RGSMH-SETNR,
        HELP_SEARCHFLD LIKE RGSMH-SEARCHFLD,
        HELP_SET       LIKE RGSBS-SETNR,
        HELP_SETCLASS  LIKE RGSMH-CLASS.

  MOVE PA_KOKRS TO HELP_SEARCHFLD.

  CALL FUNCTION 'K_GROUP_SELECT'
    EXPORTING
      CLASS           = '0110'
      FIELD_NAME      = 'POSID'
      SEARCHFLD       = HELP_SEARCHFLD
      SEARCHFLD_INPUT = ' '
      SET             = HELP_SET
    IMPORTING
      SET_NAME        = HELP_SETNR
    EXCEPTIONS
      NO_SET_PICKED   = 1.

  IF SY-SUBRC = 0.
    P_PDGR = HELP_SETNR.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_BL1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM CHECK_BL1 .

  IF PA_PERBL < 1 OR PA_PERBL > 12.
    SET CURSOR FIELD 'PA_PERBL'.
    MESSAGE E023  WITH TEXT-003.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_BL2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM CHECK_BL2 .
  DATA LS_RETURN TYPE BAPIRET2.

  IF SO_POSID[] IS NOT INITIAL AND
     PA_PDGR IS NOT INITIAL.
    SET CURSOR FIELD 'SO_POSID-LOW'.
    MESSAGE E026  WITH TEXT-E01.
  ENDIF.

  IF SO_POSID[] IS NOT INITIAL.

    SELECT SINGLE * FROM PRPS
      INTO @DATA(LS_PRPS)
     WHERE POSID IN @SO_POSID.

    IF SY-SUBRC <> 0.
      SET CURSOR FIELD 'SO_POSID-LOW'.
      MESSAGE E027  WITH TEXT-E02.
    ENDIF.

  ENDIF.

  IF PA_PDGR IS NOT INITIAL.

    PERFORM GET_CHECK_GROUP USING '0110'
                                  PA_PDGR
                            CHANGING LS_RETURN.

    IF LS_RETURN-TYPE = 'E'.
      SET CURSOR FIELD 'PA_PDGR'.
      MESSAGE E027  WITH TEXT-E03.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_CHECK_GROUP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM GET_CHECK_GROUP  USING    VALUE(P_SETCLASS)
                                        P_GROUP
*                                        P_CHRT_ACCTS
                               CHANGING PS_RETURN STRUCTURE BAPIRET2.

  CLEAR PS_RETURN.

  DATA LV_GROUPNAME TYPE GRPNAME.
  DATA LV_SETCLASS  TYPE SETCLASS.
  DATA LT_SETHIER TYPE TABLE OF SETHIER_CO WITH HEADER LINE.

  MOVE: P_GROUP     TO LV_GROUPNAME,
        P_SETCLASS  TO LV_SETCLASS.

  CALL FUNCTION 'K_GROUP_REMOTE_READ'
    EXPORTING
      SETCLASS   = LV_SETCLASS
      CO_AREA    = PA_KOKRS
      CHRT_ACCTS = GC_KTOPL
      GROUPNAME  = LV_GROUPNAME
    IMPORTING
      RETURN     = PS_RETURN
    TABLES
      ET_SETHIER = LT_SETHIER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
FORM SELECTED_DATA_RTN .
  CLEAR : GT_DISPLAY, GT_DISPLAY[].

  PERFORM SET_RANGES_VALUE.
  PERFORM SET_RANGES_OBJNR.

  IF PA_RAD1 IS NOT INITIAL.
    PERFORM GET_SALE_DATA.
    PERFORM SET_SALE_DATA.

    PERFORM GET_COST_DATA.
    PERFORM SET_COST_DATA.
  ELSEIF PA_RAD2 IS NOT INITIAL.
    PERFORM GET_SALE_DATA.
    PERFORM SET_SALE_DATA.
  ELSE.
    PERFORM GET_COST_DATA.
    PERFORM SET_COST_DATA.
  ENDIF.

  PERFORM MERGE_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_RANGES_VALUE
*&---------------------------------------------------------------------*
FORM SET_RANGES_VALUE .
  CLEAR: R_SALE, R_SALE[], R_COST, R_COST[].

  MOVE: 'I'          TO R_SALE-SIGN,
        'BT'         TO R_SALE-OPTION,
        '0401101001' TO R_SALE-LOW,
        '0409999999' TO R_SALE-HIGH.

  APPEND R_SALE.
  CLEAR  R_SALE.

  MOVE: 'E'          TO R_SALE-SIGN,
        'EQ'         TO R_SALE-OPTION,
        '0405101004' TO R_SALE-LOW.

  APPEND R_SALE.
  CLEAR  R_SALE.

  MOVE: 'I'          TO R_COST-SIGN,
        'BT'         TO R_COST-OPTION,
*        '0501101001' TO R_COST-LOW,
        '0500101001' TO R_COST-LOW,
        '0503599999' TO R_COST-HIGH.

  APPEND R_COST.
  CLEAR  R_COST.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_RANGES_OBJNR
*&---------------------------------------------------------------------*
FORM SET_RANGES_OBJNR .
  CLEAR: GT_VALUES, GT_VALUES[],
         R_OBJNR,   R_OBJNR[].

  IF SO_POSID[] IS NOT INITIAL.

    LOOP AT SO_POSID.

      MOVE: SO_POSID-SIGN   TO R_OBJNR-SIGN,
            SO_POSID-OPTION TO R_OBJNR-OPTION.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
        EXPORTING
          INPUT     = SO_POSID-LOW
        IMPORTING
          OUTPUT    = R_OBJNR-LOW
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.

      R_OBJNR-LOW  = 'PR' && R_OBJNR-LOW.

      IF SO_POSID-HIGH IS NOT INITIAL.

        CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
          EXPORTING
            INPUT     = SO_POSID-HIGH
          IMPORTING
            OUTPUT    = R_OBJNR-HIGH
          EXCEPTIONS
            NOT_FOUND = 1
            OTHERS    = 2.

        R_OBJNR-HIGH  = 'PR' && R_OBJNR-HIGH.

      ENDIF.

      COLLECT R_OBJNR.
      CLEAR   R_OBJNR.

    ENDLOOP.

  ENDIF.

  IF PA_PDGR IS NOT INITIAL.

    PERFORM READ_HIERARCHY_TABLES TABLES GT_VALUES
                                  USING '0110'
                                        PA_PDGR.   "WBS 요소 그룹

    LOOP AT GT_VALUES.

      MOVE: 'I'   TO R_OBJNR-SIGN,
            'BT'  TO R_OBJNR-OPTION.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
        EXPORTING
          INPUT     = GT_VALUES-VFROM
        IMPORTING
          OUTPUT    = R_OBJNR-LOW
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
        EXPORTING
          INPUT     = GT_VALUES-VTO
        IMPORTING
          OUTPUT    = R_OBJNR-HIGH
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.

      R_OBJNR-LOW   = 'PR' && R_OBJNR-LOW.
      R_OBJNR-HIGH  = 'PR' && R_OBJNR-HIGH.

      COLLECT R_OBJNR.
      CLEAR   R_OBJNR.

    ENDLOOP.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form READ_HIERARCHY_TABLES
*&---------------------------------------------------------------------*
FORM READ_HIERARCHY_TABLES TABLES PT_VALUES STRUCTURE GRPVALUES
                            USING PV_CLASS TYPE SETCLASS
                                  PV_SETID.

  DATA: LV_SETID     LIKE SETHIER-SETID,
        LV_OVERWRITE LIKE SY-DATAR,
        LT_INFO      LIKE GRPHINFO OCCURS 0 WITH HEADER LINE,
        LT_NODES     LIKE GRPOBJECTS OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'G_SET_ENCRYPT_SETID'
    EXPORTING
      SETCLASS  = PV_CLASS
      SHORTNAME = PV_SETID  "코스트센터그룹
    IMPORTING
      SETID     = LV_SETID.

  IF SY-SUBRC = 0.

    CALL FUNCTION 'K_HIERARCHY_TABLES_READ'
      EXPORTING
        E_CLASS                     = PV_CLASS
        E_SETID                     = LV_SETID
        E_KOKRS                     = PA_KOKRS
      TABLES
        T_NODES                     = LT_NODES
        T_VALUES                    = PT_VALUES
      CHANGING
        C_INFO                      = LT_INFO
        C_OVERWRITE                 = LV_OVERWRITE
      EXCEPTIONS
        NO_CONTROLLING_AREA         = 1
        NO_CHART_OF_ACCOUNT         = 2
        DIFFERENT_CONTROLLING_AREAS = 3
        DIFFERENT_CHART_OF_ACCOUNTS = 4
        SET_NOT_FOUND               = 5
        ILLEGAL_FIELD_REPLACEMENT   = 6
        ILLEGAL_TABLE_REPLACEMENT   = 7
        FM_RAISE                    = 8
        CONVERT_ERROR               = 9
        NO_OVERWRITE_STANDARD_HIER  = 10
        NO_BUKRS_FOR_KOKRS          = 11
        OTHERS                      = 12.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_SALE_DATA
*&---------------------------------------------------------------------*
FORM GET_SALE_DATA .
  DATA: LV_WRTTP TYPE CO_WRTTP.

  CLEAR: GS_SALE, GT_SALE, GT_SALE[].

  IF PA_VERSN EQ '000'.
    LV_WRTTP = '04'.
  ELSE.
    LV_WRTTP = '01'.
  ENDIF.

  SELECT B~POSID, B~POST1, 'KRW' AS TWAER,
         SUM( A~WKG001 ) AS WKG001,
         SUM( A~WKG002 ) AS WKG002,
         SUM( A~WKG003 ) AS WKG003,
         SUM( A~WKG004 ) AS WKG004,
         SUM( A~WKG005 ) AS WKG005,
         SUM( A~WKG006 ) AS WKG006,
         SUM( A~WKG007 ) AS WKG007,
         SUM( A~WKG008 ) AS WKG008,
         SUM( A~WKG009 ) AS WKG009,
         SUM( A~WKG010 ) AS WKG010,
         SUM( A~WKG011 ) AS WKG011,
         SUM( A~WKG012 ) AS WKG012
    FROM COSP AS A JOIN PRPS AS B
                     ON A~OBJNR = B~OBJNR
   WHERE A~LEDNR  = '00'
     AND A~GJAHR  = @PA_GJAHR
     AND A~VERSN  = @PA_VERSN
     AND A~WRTTP  = @LV_WRTTP
     AND A~OBJNR IN @R_OBJNR
     AND A~KSTAR IN @R_SALE
     AND B~PBUKR  = @PA_BUKRS
   GROUP BY B~POSID, B~POST1
  UNION ALL
  SELECT B~POSID, B~POST1, 'KRW' AS TWAER,
         SUM( A~WKG001 ) AS WKG001,
         SUM( A~WKG002 ) AS WKG002,
         SUM( A~WKG003 ) AS WKG003,
         SUM( A~WKG004 ) AS WKG004,
         SUM( A~WKG005 ) AS WKG005,
         SUM( A~WKG006 ) AS WKG006,
         SUM( A~WKG007 ) AS WKG007,
         SUM( A~WKG008 ) AS WKG008,
         SUM( A~WKG009 ) AS WKG009,
         SUM( A~WKG010 ) AS WKG010,
         SUM( A~WKG011 ) AS WKG011,
         SUM( A~WKG012 ) AS WKG012
    FROM COSS AS A JOIN PRPS AS B
                     ON A~OBJNR = B~OBJNR
   WHERE A~LEDNR  = '00'
     AND A~GJAHR  = @PA_GJAHR
     AND A~VERSN  = @PA_VERSN
     AND A~WRTTP  = @LV_WRTTP
     AND A~OBJNR IN @R_OBJNR
     AND A~KSTAR IN @R_SALE
     AND B~PBUKR  = @PA_BUKRS
   GROUP BY B~POSID, B~POST1
    INTO TABLE @GT_SALE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SALE_DATA
*&---------------------------------------------------------------------*
FORM SET_SALE_DATA .
  DATA: LV_SALE_TOT  TYPE WKGXXX,
        LV_SUM_TOT   TYPE WKGXXX,
        LV_FIELDNAME TYPE FIELDNAME.

  DATA: LS_SALE LIKE GS_DISPLAY,
        LT_SALE LIKE TABLE OF GS_DISPLAY.

  FIELD-SYMBOLS: <FS_TOT> TYPE ANY.

  LOOP AT GT_SALE ASSIGNING FIELD-SYMBOL(<FS_DATA>).
    LS_SALE-POSID = <FS_DATA>-POSID.
    LS_SALE-POST1 = <FS_DATA>-POST1.
    LS_SALE-TWAER = <FS_DATA>-TWAER.

    LV_FIELDNAME = '<FS_DATA>-WKG' && PA_PERBL.
    ASSIGN (LV_FIELDNAME) TO <FS_TOT>.

    <FS_TOT> = <FS_TOT> * -1.
    LS_SALE-SALE = <FS_TOT>.

    IF <FS_TOT> > 0.
      LV_SALE_TOT = LV_SALE_TOT + <FS_TOT>.
    ENDIF.

    IF <FS_TOT> <> 0.
      APPEND LS_SALE TO LT_SALE.
    ENDIF.

    CLEAR: LS_SALE, <FS_DATA>.
  ENDLOOP.

  LOOP AT LT_SALE INTO LS_SALE.

    IF LS_SALE-SALE > 0.
      LS_SALE-SALE_R = ( LS_SALE-SALE / LV_SALE_TOT ) * 100.
    ENDIF.

    LV_SUM_TOT = LV_SUM_TOT + LS_SALE-SALE_R.

    APPEND LS_SALE TO GT_SALE_SUM.
    CLEAR LS_SALE.
  ENDLOOP.

  SORT GT_SALE_SUM BY SALE_R DESCENDING.

  IF LV_SUM_TOT <> 0.
    READ TABLE GT_SALE_SUM INTO GS_DISPLAY INDEX 1.
    GS_DISPLAY-SALE_R = GS_DISPLAY-SALE_R + ( 100 - LV_SUM_TOT ).
    MODIFY GT_SALE_SUM FROM GS_DISPLAY INDEX 1 TRANSPORTING SALE_R.
    CLEAR GS_DISPLAY.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_COST_DATA
*&---------------------------------------------------------------------*
FORM GET_COST_DATA .
  DATA: LV_WRTTP TYPE CO_WRTTP.

  CLEAR: GS_COST, GT_COST, GT_COST[].

  IF PA_VERSN EQ '000'.
    LV_WRTTP = '04'.
  ELSE.
    LV_WRTTP = '01'.
  ENDIF.

  SELECT B~POSID, B~POST1, 'KRW' AS TWAER,
         SUM( A~WKG001 ) AS WKG001,
         SUM( A~WKG002 ) AS WKG002,
         SUM( A~WKG003 ) AS WKG003,
         SUM( A~WKG004 ) AS WKG004,
         SUM( A~WKG005 ) AS WKG005,
         SUM( A~WKG006 ) AS WKG006,
         SUM( A~WKG007 ) AS WKG007,
         SUM( A~WKG008 ) AS WKG008,
         SUM( A~WKG009 ) AS WKG009,
         SUM( A~WKG010 ) AS WKG010,
         SUM( A~WKG011 ) AS WKG011,
         SUM( A~WKG012 ) AS WKG012
    FROM COSP AS A JOIN PRPS AS B
                     ON A~OBJNR = B~OBJNR
   WHERE A~LEDNR  = '00'
     AND A~GJAHR  = @PA_GJAHR
     AND A~VERSN  = @PA_VERSN
     AND A~WRTTP  = @LV_WRTTP
     AND A~OBJNR IN @R_OBJNR
     AND A~KSTAR IN @R_COST
     AND B~PBUKR  = @PA_BUKRS
   GROUP BY B~POSID, B~POST1
  UNION ALL
  SELECT B~POSID, B~POST1, 'KRW' AS TWAER,
         SUM( A~WKG001 ) AS WKG001,
         SUM( A~WKG002 ) AS WKG002,
         SUM( A~WKG003 ) AS WKG003,
         SUM( A~WKG004 ) AS WKG004,
         SUM( A~WKG005 ) AS WKG005,
         SUM( A~WKG006 ) AS WKG006,
         SUM( A~WKG007 ) AS WKG007,
         SUM( A~WKG008 ) AS WKG008,
         SUM( A~WKG009 ) AS WKG009,
         SUM( A~WKG010 ) AS WKG010,
         SUM( A~WKG011 ) AS WKG011,
         SUM( A~WKG012 ) AS WKG012
    FROM COSS AS A JOIN PRPS AS B
                     ON A~OBJNR = B~OBJNR
   WHERE A~LEDNR  = '00'
     AND A~GJAHR  = @PA_GJAHR
     AND A~VERSN  = @PA_VERSN
     AND A~WRTTP  = @LV_WRTTP
     AND A~OBJNR IN @R_OBJNR
     AND A~KSTAR IN @R_COST
     AND B~PBUKR  = @PA_BUKRS
   GROUP BY B~POSID, B~POST1
    INTO TABLE @GT_COST.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_COST_DATA
*&---------------------------------------------------------------------*
FORM SET_COST_DATA .
  DATA: LV_COST_TOT  TYPE WKGXXX,
        LV_SUM_TOT   TYPE WKGXXX,
        LV_FIELDNAME TYPE FIELDNAME.

  DATA: LS_COST LIKE GS_DISPLAY,
        LT_COST LIKE TABLE OF GS_DISPLAY.

  FIELD-SYMBOLS: <FS_TOT> TYPE ANY.

  LOOP AT GT_COST ASSIGNING FIELD-SYMBOL(<FS_DATA>).
    LS_COST-POSID = <FS_DATA>-POSID.
    LS_COST-POST1 = <FS_DATA>-POST1.
    LS_COST-TWAER = <FS_DATA>-TWAER.

    LV_FIELDNAME = '<FS_DATA>-WKG' && PA_PERBL.
    ASSIGN (LV_FIELDNAME) TO <FS_TOT>.

*    <FS_TOT> = <FS_TOT> * -1.
    LS_COST-COST = <FS_TOT>.

    IF <FS_TOT> > 0.
      LV_COST_TOT = LV_COST_TOT + <FS_TOT>.
    ENDIF.

    IF <FS_TOT> <> 0.
      APPEND LS_COST TO LT_COST.
    ENDIF.

    CLEAR: LS_COST, <FS_DATA>.
  ENDLOOP.

  LOOP AT LT_COST INTO LS_COST.

    IF LS_COST-COST > 0.
      LS_COST-COST_R = ( LS_COST-COST / LV_COST_TOT ) * 100.
    ENDIF.

    LV_SUM_TOT = LV_SUM_TOT + LS_COST-COST_R.

    APPEND LS_COST TO GT_COST_SUM.
    CLEAR LS_COST.
  ENDLOOP.

  SORT GT_COST_SUM BY COST_R DESCENDING.

  IF LV_SUM_TOT <> 0.
    READ TABLE GT_COST_SUM INTO GS_DISPLAY INDEX 1.
    GS_DISPLAY-COST_R = GS_DISPLAY-COST_R + ( 100 - LV_SUM_TOT ).
    MODIFY GT_COST_SUM FROM GS_DISPLAY INDEX 1 TRANSPORTING COST_R.
    CLEAR GS_DISPLAY.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MERGE_DATA
*&---------------------------------------------------------------------*
FORM MERGE_DATA .

  IF PA_RAD1 IS NOT INITIAL.
    PERFORM MERGE_SALE_DATA.
    PERFORM MERGE_COST_DATA.
  ELSEIF PA_RAD2 IS NOT INITIAL.
    PERFORM MERGE_SALE_DATA.
  ELSE.
    PERFORM MERGE_COST_DATA.
  ENDIF.

  SORT GT_DISPLAY BY POSID.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MERGE_SALE_DATA
*&---------------------------------------------------------------------*
FORM MERGE_SALE_DATA .

  GT_DISPLAY[] = GT_SALE_SUM[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MERGE_COST_DATA
*&---------------------------------------------------------------------*
FORM MERGE_COST_DATA .
  DATA: LS_DISPLAY LIKE GS_DISPLAY.

  IF PA_RAD3 IS INITIAL.

    LOOP AT GT_COST_SUM INTO GS_DISPLAY.

      READ TABLE GT_DISPLAY INTO LS_DISPLAY WITH KEY POSID = GS_DISPLAY-POSID.
      IF SY-SUBRC = 0.

        LS_DISPLAY-COST   = GS_DISPLAY-COST.
        LS_DISPLAY-COST_R = GS_DISPLAY-COST_R.

        MODIFY GT_DISPLAY FROM LS_DISPLAY INDEX SY-TABIX
                          TRANSPORTING COST COST_R.

      ELSE.

        APPEND GS_DISPLAY TO GT_DISPLAY.

      ENDIF.
      CLEAR: GS_DISPLAY, LS_DISPLAY.
    ENDLOOP.

  ELSE.

    GT_DISPLAY[] = GT_COST_SUM[].

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_INSTANCE_0100
*&---------------------------------------------------------------------*
FORM CREATE_INSTANCE_0100 .
*-- 1. customer container

*  CREATE OBJECT GR_CON1
*    EXPORTING
*      CONTAINER_NAME = GV_CONTAINER. "USER가 정의한 CONTAINER
*
*  CREATE OBJECT GR_GRID1
*    EXPORTING
*      I_PARENT = GR_CON1.

*-- 2. full screen
  CREATE OBJECT GR_SPLITTER1
    EXPORTING
      ROWS    = 2
      COLUMNS = 1
      PARENT  = CL_GUI_SPLITTER_CONTAINER=>SCREEN0.

*== get container instance
*-- 1. top of page
  GR_PARENT_HTML = GR_SPLITTER1->GET_CONTAINER(
      ROW       = 1
      COLUMN    = 1 ).

  GR_DATA_CONTAINER = GR_SPLITTER1->GET_CONTAINER(
      ROW       = 2
      COLUMN    = 1 ).

  CALL METHOD GR_SPLITTER1->SET_ROW_HEIGHT
    EXPORTING
      ID     = 1
      HEIGHT = 7.

  CALL METHOD GR_SPLITTER1->SET_ROW_HEIGHT
    EXPORTING
      ID     = 2
      HEIGHT = 50.

  CREATE OBJECT GR_GRID1
    EXPORTING
      I_PARENT = GR_DATA_CONTAINER.

ENDFORM.                    " CREATE_INSTANCE_0100
*&---------------------------------------------------------------------*
*&      Form  INIT_LAYOUT_0100
*&---------------------------------------------------------------------*
FORM INIT_LAYOUT_0100.

  CLEAR GS_LAYOUT.

*  GS_LAYOUT-EDIT_MODE  = ABAP_TRUE.
  GS_LAYOUT-ZEBRA      = ABAP_TRUE.
*  GS_LAYOUT-CWIDTH_OPT = ABAP_TRUE.
  GS_LAYOUT-SEL_MODE   = SPACE.     "B:단일,C:복수,D:셀,A:행/열
  GS_LAYOUT-BOX_FNAME  = SPACE.
  GS_LAYOUT-NO_ROWMARK = SPACE.

*  GS_LAYOUT-STYLEFNAME = 'STYLE'.
*  GS_LAYOUT-CTAB_FNAME = 'COLOR'.
*  GS_LAYOUT-INFO_FNAME = 'INFO'.

**  "alv title
**  GS_LAYOUT-GRID_TITLE = TEXT-GT1.

ENDFORM.                    " INIT_LAYOUT_0100
*&---------------------------------------------------------------------*
*&      Form  SET_GRID_EXCLUDE_0100
*&---------------------------------------------------------------------*
FORM SET_GRID_EXCLUDE_0100 .

  DATA: LS_EXCLUDE LIKE LINE OF GT_EXCLUDE.
  REFRESH: GT_EXCLUDE.

  "-- DEFINE _SET_EX
  DEFINE _SET_EX.
    CLEAR: ls_exclude.
    ls_exclude = &1.
    APPEND ls_exclude TO gt_exclude.
  END-OF-DEFINITION.


  _SET_EX:
*   CL_GUI_ALV_GRID=>MC_FC_FIND,
*   CL_GUI_ALV_GRID=>MC_FC_SORT_ASC,
*   CL_GUI_ALV_GRID=>MC_FC_SORT_DSC,
*   CL_GUI_ALV_GRID=>MC_MB_SUBTOT,
*   CL_GUI_ALV_GRID=>MC_MB_SUM,

    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,
    CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,
    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE,
    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO,
    CL_GUI_ALV_GRID=>MC_FC_CHECK,

*   CL_GUI_ALV_GRID=>MC_FC_DETAIL,
*   CL_GUI_ALV_GRID=>MC_FC_FILTER,
    CL_GUI_ALV_GRID=>MC_FC_GRAPH,
    CL_GUI_ALV_GRID=>MC_FC_HTML,
    CL_GUI_ALV_GRID=>MC_FC_INFO,
    CL_GUI_ALV_GRID=>MC_FC_REFRESH,

*   CL_GUI_ALV_GRID=>MC_FC_VIEWS,
*   CL_GUI_ALV_GRID=>MC_FC_LOAD_VARIANT,
*   CL_GUI_ALV_GRID=>MC_FC_PRINT,
*   CL_GUI_ALV_GRID=>MC_MB_VARIANT,
*   CL_GUI_ALV_GRID=>MC_MB_EXPORT,

    CL_GUI_ALV_GRID=>MC_FC_VIEW_CRYSTAL,
    CL_GUI_ALV_GRID=>MC_FC_VIEW_EXCEL,
    CL_GUI_ALV_GRID=>MC_FC_VIEW_GRID,
    CL_GUI_ALV_GRID=>MC_FC_VIEW_LOTUS,
    CL_GUI_ALV_GRID=>MC_FC_EXPCRDATA,
    CL_GUI_ALV_GRID=>MC_FC_EXPCRDESIG,
    CL_GUI_ALV_GRID=>MC_FC_EXPCRTEMPL,
    CL_GUI_ALV_GRID=>MC_FC_CALL_ABC,
    CL_GUI_ALV_GRID=>MC_FC_CALL_CRBATCH.

ENDFORM. " SET_GRID_EXCLUDE_0100
*&---------------------------------------------------------------------*
*&      Form  ALV_SORT_0100
*&---------------------------------------------------------------------*
FORM ALV_SORT_0100 .

  CLEAR: GS_SORT, GT_SORT.
  REFRESH: GT_SORT.

ENDFORM.                    " ALV_SORT_0100
*&---------------------------------------------------------------------*
*&      Form  APPEND_FIELDCAT_0100
*&---------------------------------------------------------------------*
FORM APPEND_FIELDCAT_0100 .

  "-- field catalog data
  "   field catalog merge or set fieldcatalog를 사용할 수 있음.

  "{ FIELDCATLOG MERGE 사용
  PERFORM GET_FIELDCATLOG_DATA.

  PERFORM MODIFY_FIELDCATLOG_DATA.
  "}

  "{ SET FIELDCATLOG 사용
*  PERFORM SET_FIELDCATLOG_DATA.
  "}

ENDFORM.                    " APPEND_FIELDCAT_0100
*&---------------------------------------------------------------------*
*&      Form  GET_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
FORM GET_FIELDCATLOG_DATA .

  DATA: LT_FIELDCAT TYPE KKBLO_T_FIELDCAT.

  CALL FUNCTION 'K_KKB_FIELDCAT_MERGE'
    EXPORTING
      I_CALLBACK_PROGRAM     = SY-REPID
      I_STRUCNAME            = 'ZCOS0230' "ABAP DIC. 정의된 STRUCTURE
      I_BYPASSING_BUFFER     = ABAP_TRUE
      I_INCLNAME             = SY-REPID
    CHANGING
      CT_FIELDCAT            = LT_FIELDCAT[]
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      OTHERS                 = 2.

  IF SY-SUBRC EQ 0.

    "-- Trasnfer LVC.
    CALL FUNCTION 'LVC_TRANSFER_FROM_KKBLO'
      EXPORTING
        IT_FIELDCAT_KKBLO = LT_FIELDCAT[]
      IMPORTING
        ET_FIELDCAT_LVC   = GT_FIELDCAT[]
      EXCEPTIONS
        IT_DATA_MISSING   = 1.
  ELSE.

    MESSAGE E020.

  ENDIF.

ENDFORM.                    " GET_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
FORM MODIFY_FIELDCATLOG_DATA .

  DATA:  LV_TEXT(50).

  LOOP AT GT_FIELDCAT INTO GS_FIELDCAT.

    CLEAR: LV_TEXT.

    CASE GS_FIELDCAT-FIELDNAME.
      WHEN 'POSID'.
        LV_TEXT               = TEXT-C01.
        GS_FIELDCAT-EMPHASIZE = 'C112'.

      WHEN 'POST1'.
        LV_TEXT = TEXT-C02.

      WHEN 'SALE'.
        LV_TEXT = TEXT-C03.
        GS_FIELDCAT-OUTPUTLEN = 20.
        GS_FIELDCAT-DO_SUM = ABAP_TRUE.
        IF PA_RAD1 IS INITIAL AND PA_RAD2 IS INITIAL.
          GS_FIELDCAT-NO_OUT = ABAP_TRUE.
        ENDIF.

      WHEN 'SALE_R'.
        LV_TEXT = TEXT-C04.
        GS_FIELDCAT-OUTPUTLEN = 10.
        GS_FIELDCAT-DO_SUM = ABAP_TRUE.
        IF PA_RAD1 IS INITIAL AND PA_RAD2 IS INITIAL.
          GS_FIELDCAT-NO_OUT = ABAP_TRUE.
        ENDIF.

      WHEN 'COST'.
        LV_TEXT = TEXT-C05.
        GS_FIELDCAT-OUTPUTLEN = 20.
        GS_FIELDCAT-DO_SUM = ABAP_TRUE.
        IF PA_RAD1 IS INITIAL AND PA_RAD3 IS INITIAL.
          GS_FIELDCAT-NO_OUT = ABAP_TRUE.
        ENDIF.

      WHEN 'COST_R'.
        LV_TEXT = TEXT-C06.
        GS_FIELDCAT-OUTPUTLEN = 10.
        GS_FIELDCAT-DO_SUM = ABAP_TRUE.
        IF PA_RAD1 IS INITIAL AND PA_RAD3 IS INITIAL.
          GS_FIELDCAT-NO_OUT = ABAP_TRUE.
        ENDIF.

      WHEN OTHERS.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
    ENDCASE.

    "-- Common attribute
    IF LV_TEXT IS NOT INITIAL.
      GS_FIELDCAT-COLTEXT   = LV_TEXT.
      GS_FIELDCAT-SCRTEXT_L = LV_TEXT.
      GS_FIELDCAT-SCRTEXT_M = LV_TEXT.
      GS_FIELDCAT-SCRTEXT_S = LV_TEXT.
    ENDIF.

    MODIFY GT_FIELDCAT FROM GS_FIELDCAT.

  ENDLOOP.

ENDFORM.                    " MODIFY_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
FORM SET_FIELDCATLOG_DATA.

  CLEAR GT_FIELDCAT[].

  PERFORM FILL_FIELD_CATEGORY USING :
        'S' 'FIELDNAME'   'GJAHR',
        ' ' 'OUTPUTLEN'   '4',
        'E' 'COLTEXT'     '적용연도',

        'S' 'FIELDNAME'   'CARRID',
        ' ' 'OUTPUTLEN'   '3',
        ' ' 'EDIT'        'X',
        'E' 'COLTEXT'     '항공사 ID'.

ENDFORM.                    " SET_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
*&      Form  fill_field_category
*&---------------------------------------------------------------------*
FORM FILL_FIELD_CATEGORY USING PV_GUB PV_FNAME PV_CON.

  IF PV_GUB = 'S'.
    CLEAR GS_FIELDCAT.
  ENDIF.

* 속성 MOVE
  DATA LV_COL(40).
  FIELD-SYMBOLS <FS>.
  CONCATENATE 'GS_FIELDCAT-' PV_FNAME  INTO LV_COL.
  ASSIGN      (LV_COL)       TO        <FS>.
  MOVE         PV_CON        TO        <FS>.

  IF PV_GUB = 'E'.
    APPEND GS_FIELDCAT TO GT_FIELDCAT.
  ENDIF.
ENDFORM. " fill_field_category
*&---------------------------------------------------------------------*
*& Form TOP_OF_PAGE_CREATE_OBJECT_0100
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE_CREATE_OBJECT_0100 .

* Create TOP-Document
  CREATE OBJECT GR_TOP_DOCUMENT
    EXPORTING
      STYLE = 'ALV_GRID'.

* Initialize
  CALL METHOD GR_TOP_DOCUMENT->INITIALIZE_DOCUMENT( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_TOP_OF_PAGE_DATA_0100
*&---------------------------------------------------------------------*
FORM MAKE_TOP_OF_PAGE_DATA_0100 .

  DATA: LT_TEXTS TYPE SDYDO_TEXT_TABLE,
        LV_TEXT  TYPE SDYDO_TEXT_ELEMENT.

  CONCATENATE TEXT-001 ':' PA_KOKRS
        INTO LV_TEXT SEPARATED BY SPACE.

  CALL METHOD GR_TOP_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
      SAP_EMPHASIS = CL_DD_AREA=>KEY
      STYLE_CLASS  = SPACE.

  CALL METHOD GR_TOP_DOCUMENT->NEW_LINE.
*
  CONCATENATE TEXT-002 ':' PA_GJAHR
        INTO LV_TEXT SEPARATED BY SPACE.

  CALL METHOD GR_TOP_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
      SAP_EMPHASIS = CL_DD_AREA=>KEY
      STYLE_CLASS  = SPACE.

  CALL METHOD GR_TOP_DOCUMENT->NEW_LINE.

  CONCATENATE TEXT-003 ':' PA_PERBL
        INTO LV_TEXT SEPARATED BY SPACE.

  CALL METHOD GR_TOP_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
      SAP_EMPHASIS = CL_DD_AREA=>KEY
      STYLE_CLASS  = SPACE.

  CALL METHOD GR_TOP_DOCUMENT->NEW_LINE.

  CONCATENATE TEXT-004 ':' PA_VERSN
        INTO LV_TEXT SEPARATED BY SPACE.

  CALL METHOD GR_TOP_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
      SAP_EMPHASIS = CL_DD_AREA=>KEY
      STYLE_CLASS  = SPACE.

  CALL METHOD GR_TOP_DOCUMENT->NEW_LINE.

  CONCATENATE TEXT-008 ` / ` TEXT-009 ` / ` TEXT-010
        INTO LV_TEXT SEPARATED BY SPACE.

  CALL METHOD GR_TOP_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
      SAP_EMPHASIS = CL_DD_AREA=>KEY
      STYLE_CLASS  = SPACE.

  CALL METHOD GR_TOP_DOCUMENT->NEW_LINE
    EXPORTING
      REPEAT = 1.

*  CALL METHOD GR_TOP_DOCUMENT->ADD_GAP
*    EXPORTING
*      WIDTH = 20.

  " Get Ready
  CALL METHOD GR_TOP_DOCUMENT->MERGE_DOCUMENT.

*" Display TOP document
  CALL METHOD GR_TOP_DOCUMENT->DISPLAY_DOCUMENT
    EXPORTING
      REUSE_CONTROL      = 'X'
      PARENT             = GR_PARENT_HTML
    EXCEPTIONS
      HTML_DISPLAY_ERROR = 1.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  REGIST_ALV_EVENT_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM REGIST_ALV_EVENT_0100 USING PR_GRID TYPE REF TO CL_GUI_ALV_GRID.

* REGISTER EVENT
  CALL METHOD PR_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
*
  CALL METHOD PR_GRID->SET_READY_FOR_INPUT
    EXPORTING
      I_READY_FOR_INPUT = 1.

*-- GR_EVENT_RECEIVER
  IF GR_EVENT_RECEIVER IS INITIAL.
    CREATE OBJECT GR_EVENT_RECEIVER.
  ENDIF.

* Handler Event
*  SET HANDLER:
*    GR_EVENT_RECEIVER->HANDLE_TOOLBAR       FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_DATA_CHANGED  FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_DATA_CHANGED_FINISHED
*      FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_USER_COMMAND  FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_HOTSPOT_CLICK FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_DOUBLE_CLICK  FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_ONF4          FOR ALL INSTANCES.

ENDFORM.                    " REGIST_ALV_EVENT_0100
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_TITLE_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DISPLAY_ALV_TITLE_0100 .

  DATA: LV_TITLE TYPE LVC_TITLE.

  LV_TITLE = TEXT-GT1.

  CALL METHOD GR_GRID1->SET_GRIDTITLE
    EXPORTING
      I_GRIDTITLE = LV_TITLE.

ENDFORM.                    " DISPLAY_ALV_TITLE_0100
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_GRID_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DISPLAY_ALV_GRID_0100 .

  GS_VARIANT-REPORT = SY-REPID.

  GV_SAVE = 'A'.

  "*-- Build field catalog for the alv control
  CALL METHOD GR_GRID1->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_DEFAULT                     = ABAP_TRUE
      IS_LAYOUT                     = GS_LAYOUT
      IS_VARIANT                    = GS_VARIANT
      I_SAVE                        = GV_SAVE
      IT_TOOLBAR_EXCLUDING          = GT_EXCLUDE
    CHANGING
      IT_FIELDCATALOG               = GT_FIELDCAT
      IT_SORT                       = GT_SORT
      IT_OUTTAB                     = GT_DISPLAY[]
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3.

  IF SY-SUBRC NE 0.
    MESSAGE E000(0K) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " DISPLAY_ALV_GRID_0100
*&---------------------------------------------------------------------*
*&      Form  REFRESH_GRID_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM REFRESH_GRID_0100 .

  GS_STABLE-ROW = ABAP_TRUE. "Row
  GS_STABLE-COL = ABAP_TRUE. "column

  CALL METHOD GR_GRID1->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = GS_STABLE
      I_SOFT_REFRESH = SPACE.

  CALL METHOD CL_GUI_CFW=>FLUSH.

ENDFORM.                    " REFRESH_GRID_0100
*&---------------------------------------------------------------------*
*& Form SET_INIT_HELP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_INIT_HELP .

  "__ Function Key
  DATA: LS_FUNTXT TYPE SMP_DYNTXT.

  LS_FUNTXT-ICON_ID   = ICON_INFORMATION.
  LS_FUNTXT-QUICKINFO = 'Program Help'.

  SSCRFIELDS-FUNCTXT_01 = LS_FUNTXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCR_USER_COMMAND_HELP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SCR_USER_COMMAND_HELP .

  CASE SSCRFIELDS-UCOMM.
    WHEN 'FC01'.
      PERFORM CALL_POPUP_HELP(ZCAR9000) USING SY-REPID SY-DYNNR SY-LANGU ''.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
