*&---------------------------------------------------------------------*
*& Include          ZCOR0240F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITAIL
*&---------------------------------------------------------------------*
FORM INITAIL .
  GV_REPID = SY-REPID.

  SELECT SINGLE BEZEI INTO @PA_KTXT
    FROM TKA01
   WHERE KOKRS = @PA_KOKRS.

  SET CURSOR FIELD 'PA_POSID'.

  "__ 20191223 BSGSM_FCM ADD default cac
  SET PARAMETER ID 'CAC' FIELD PA_KOKRS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SCREEN
*&---------------------------------------------------------------------*
FORM SET_SCREEN .

  LOOP AT SCREEN.
    IF SCREEN-NAME = 'PA_KOKRS' OR SCREEN-NAME = 'PA_VERSN'.
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
FORM CHECK_BL1 .
  DATA LS_RETURN TYPE BAPIRET2.

  IF SO_POSID[] IS NOT INITIAL AND
     PA_PDGR IS NOT INITIAL.
    SET CURSOR FIELD 'SO_POSID-LOW'.
    MESSAGE E026  WITH TEXT-E02.
  ENDIF.

  IF SO_POSID[] IS NOT INITIAL.

    SELECT SINGLE * FROM PRPS
      INTO @DATA(LS_PRPS)
     WHERE POSID IN @SO_POSID.

    IF SY-SUBRC <> 0.
      SET CURSOR FIELD 'SO_POSID-LOW'.
      MESSAGE E027  WITH TEXT-E03.
    ENDIF.

  ENDIF.

  IF PA_PDGR IS NOT INITIAL.

    PERFORM GET_CHECK_GROUP USING '0110'
                                  PA_PDGR
                            CHANGING LS_RETURN.

    IF LS_RETURN-TYPE = 'E'.
      SET CURSOR FIELD 'PA_PDGR'.
      MESSAGE E027  WITH TEXT-E04.
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

  PERFORM AUTHORITY_CHECK.
  PERFORM SET_RANGES_OBJNR.
  PERFORM GET_DEFAULT_DATA.
  PERFORM SELECTED_MAIN_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM AUTHORITY_CHECK .

  DATA: LV_TYPE    TYPE BAPI_MTYPE,
        LV_MESSAGE TYPE BAPI_MSG.

  DATA: LT_0070  LIKE TABLE OF ZCAS0070,
        LS_0070  LIKE ZCAS0070,
        LV_CLASS TYPE ZCAT0031-CD_CLASS,
        LV_CODE  TYPE ZCAT0031-CD_CODE.

  CLEAR: R_PRCTR1, R_PRCTR1[],
         R_BUKRS1, R_BUKRS1[],
         R_PSPID1, R_PSPID1[].

  LV_CLASS = 'CASUSR'.
  LV_CODE  = SY-UNAME.

  "__ SUPER USER ID 체크
  PERFORM CALL_F4_VALUES(ZCAR9000) TABLES LT_0070
                                    USING LV_CLASS LV_CODE LS_0070.
  IF LT_0070[] IS NOT INITIAL.
    EXIT.
  ELSE.
    LV_CLASS = 'CASUCO'.
    LV_CODE  = SY-UNAME.

    "__ SUPER USER ID 체크
    PERFORM CALL_F4_VALUES(ZCAR9000) TABLES LT_0070
                                      USING LV_CLASS LV_CODE LS_0070.
    IF LT_0070[] IS NOT INITIAL.
      EXIT.
    ENDIF.
  ENDIF.

  IF PA_PRCTR IS INITIAL AND
     PA_BUKRS IS INITIAL AND
     ( SO_POSID[] IS INITIAL AND PA_PDGR IS INITIAL ).

    MESSAGE S000 WITH TEXT-E01 DISPLAY LIKE 'E'.
    STOP.

  ENDIF.

  CALL FUNCTION 'ZCA_PRG_AUTH_CHECK'
    EXPORTING
      I_MODULE     = 'CO'
      I_PRCTR_CO   = PA_PRCTR
      I_BUKRS_CO   = PA_BUKRS
      I_POSIDGR_CO = PA_PDGR
    IMPORTING
      E_TYPE       = LV_TYPE
      E_MESSAGE    = LV_MESSAGE
    TABLES
      IT_POSID_CO  = SO_POSID[].

  IF LV_TYPE = 'E'.
    MESSAGE S000 WITH LV_MESSAGE DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  SELECT * INTO TABLE @DATA(LT_ZCOT0320)
    FROM ZCOT0320
   WHERE BNAME EQ @SY-UNAME.

  LOOP AT LT_ZCOT0320 INTO DATA(LS_ZCOT0320).

    IF LS_ZCOT0320-PRCTR1 IS NOT INITIAL.

      MOVE: LS_ZCOT0320-PRCTR1 TO R_PRCTR1-LOW,
            'I'                TO R_PRCTR1-SIGN,
            'EQ'               TO R_PRCTR1-OPTION.

      COLLECT R_PRCTR1.
      CLEAR   R_PRCTR1.

    ENDIF.

    IF LS_ZCOT0320-BUKRS IS NOT INITIAL.

      MOVE: LS_ZCOT0320-BUKRS TO R_BUKRS1-LOW,
            'I'               TO R_BUKRS1-SIGN,
            'EQ'              TO R_BUKRS1-OPTION.

      COLLECT R_BUKRS1.
      CLEAR   R_BUKRS1.

    ENDIF.

    IF LS_ZCOT0320-PSPID IS NOT INITIAL.

      MOVE: LS_ZCOT0320-PSPID TO R_PSPID1-LOW,
            'I'               TO R_PSPID1-SIGN,
            'EQ'              TO R_PSPID1-OPTION.

      COLLECT R_PSPID1.
      CLEAR   R_PSPID1.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_RANGES_OBJNR
*&---------------------------------------------------------------------*
FORM SET_RANGES_OBJNR .
  CLEAR: GT_VALUES, GT_VALUES[],
         R_PRCTR, R_PRCTR[],
         R_BUKRS, R_BUKRS[],
         R_OBJNR, R_OBJNR[].

  IF PA_PRCTR IS NOT INITIAL.
    MOVE: 'I'   TO R_PRCTR-SIGN,
          'EQ'  TO R_PRCTR-OPTION.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = PA_PRCTR
      IMPORTING
        OUTPUT = R_PRCTR-LOW.

    APPEND R_PRCTR.
    CLEAR R_PRCTR.

  ENDIF.

  IF PA_BUKRS IS NOT INITIAL.
    MOVE: 'I'      TO R_BUKRS-SIGN,
          'EQ'     TO R_BUKRS-OPTION,
          PA_BUKRS TO R_BUKRS-LOW.

    APPEND R_BUKRS.
    CLEAR R_BUKRS.

  ENDIF.

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

  SELECT A~OBJNR INTO TABLE @DATA(GT_PRPS)
    FROM PRPS AS A INNER JOIN PROJ AS B
                           ON A~PSPHI EQ B~PSPNR
   WHERE A~PKOKR EQ @PA_KOKRS
     AND A~PBUKR IN @R_BUKRS
     AND A~PRCTR IN @R_PRCTR
     AND A~OBJNR IN @R_OBJNR
     AND A~PBUKR IN @R_BUKRS1
     AND A~PRCTR IN @R_PRCTR1
     AND B~PSPID IN @R_PSPID1.

  CLEAR: R_OBJNR, R_OBJNR[].

  LOOP AT GT_PRPS ASSIGNING FIELD-SYMBOL(<FS_PRPS>).
    MOVE: 'I'             TO R_OBJNR-SIGN,
          'EQ'            TO R_OBJNR-OPTION,
          <FS_PRPS>-OBJNR TO R_OBJNR-LOW.
    COLLECT R_OBJNR.
    CLEAR   R_OBJNR.
  ENDLOOP.


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
*& Form GET_DEFAULT_DATA
*&---------------------------------------------------------------------*
FORM GET_DEFAULT_DATA .
  CLEAR: GV_KTEXT, GV_BUTXT.

  SELECT SINGLE B~KTEXT INTO @GV_KTEXT
    FROM CSKA AS A JOIN CSKU AS B
                     ON B~SPRAS = @SY-LANGU
                    AND A~KTOPL = B~KTOPL
                    AND A~KSTAR = B~KSTAR
   WHERE A~KTOPL = @GC_KTOPL
     AND A~KSTAR = @GC_KSTAR.

  SELECT SINGLE BUTXT INTO @GV_BUTXT
    FROM T001
   WHERE BUKRS = @PA_BUKRS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_MAIN_DATA
*&---------------------------------------------------------------------*
FORM SELECTED_MAIN_DATA .
  CLEAR: GT_DISPLAY, GT_DISPLAY[].

  DATA: LV_MONTH     TYPE N LENGTH 2,
        LV_FIELDNAME TYPE FIELDNAME,
        LV_AMAUNT    TYPE WKFXXX.

  FIELD-SYMBOLS: <FS_CNT>  TYPE ANY.

  SELECT A~POSID,
         A~POST1,
         @GC_KSTAR AS KSTAR,
         @GV_KTEXT AS KTEXT,
         'KRW'     AS WAERS,
         B~WKF01   AS WKF01,
         B~WKF02   AS WKF02,
         B~WKF03   AS WKF03,
         B~WKF04   AS WKF04,
         B~WKF05   AS WKF05,
         B~WKF06   AS WKF06,
         B~WKF07   AS WKF07,
         B~WKF08   AS WKF08,
         B~WKF09   AS WKF09,
         B~WKF10   AS WKF10,
         B~WKF11   AS WKF11,
         B~WKF12   AS WKF12
    INTO CORRESPONDING FIELDS OF TABLE @GT_DISPLAY
    FROM PRPS AS A LEFT OUTER JOIN ZCOT0240 AS B
                                ON B~POSID = A~POSID
                               AND B~KOKRS = @PA_KOKRS
                               AND B~BUKRS = A~PBUKR
                               AND B~GJAHR = @PA_GJAHR
                               AND B~VERSN = @PA_VERSN
                               AND B~KSTAR = @GC_KSTAR
   WHERE A~OBJNR IN @R_OBJNR.
*     AND A~PBUKR IN @R_BUKRS
*     AND A~PRCTR IN @R_PRCTR.

  LOOP AT GT_DISPLAY INTO GS_DISPLAY.

    LV_MONTH = '01'.

    DO 12 TIMES.

      LV_FIELDNAME = 'GS_DISPLAY-WKF' && LV_MONTH.
      ASSIGN (LV_FIELDNAME) TO <FS_CNT>.

      LV_AMAUNT = LV_AMAUNT + <FS_CNT>.

      ADD 1 TO LV_MONTH.

    ENDDO.

    GS_DISPLAY-TOTAL = LV_AMAUNT.

    MODIFY GT_DISPLAY FROM GS_DISPLAY.
    CLEAR: GS_DISPLAY, LV_AMAUNT.
  ENDLOOP.

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
      HEIGHT = 5.

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
*       text
*----------------------------------------------------------------------*
FORM INIT_LAYOUT_0100.

*  GS_LAYOUT-EDIT_MODE  = ABAP_TRUE.
  GS_LAYOUT-ZEBRA      = ABAP_TRUE.
*  GS_LAYOUT-CWIDTH_OPT = ABAP_TRUE.
  GS_LAYOUT-STYLEFNAME = 'STYLE'.
  GS_LAYOUT-SEL_MODE   = SPACE.     "B:단일,C:복수,D:셀,A:행/열
  GS_LAYOUT-BOX_FNAME  = SPACE.
  GS_LAYOUT-NO_ROWMARK = SPACE.

*  GS_LAYOUT-CTAB_FNAME = 'COLOR'.
*  GS_LAYOUT-INFO_FNAME = 'INFO'.

**  "alv title
**  GS_LAYOUT-GRID_TITLE = TEXT-GT1.

ENDFORM.                    " INIT_LAYOUT_0100
*&---------------------------------------------------------------------*
*&      Form  SET_GRID_EXCLUDE_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SET_GRID_EXCLUDE_0100 .

  DATA: LS_EXCLUDE LIKE LINE OF GT_EXCLUDE.
  REFRESH: GT_EXCLUDE.

  "-- DEFINE _SET_EX
  DEFINE _SET_EX.
    CLEAR: LS_EXCLUDE.
    LS_EXCLUDE = &1.
    APPEND LS_EXCLUDE TO GT_EXCLUDE.
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
*       text
*----------------------------------------------------------------------*
FORM ALV_SORT_0100 .

  CLEAR: GS_SORT, GT_SORT.
  REFRESH: GT_SORT.

  GS_SORT-FIELDNAME = 'POSID'.
  APPEND GS_SORT TO GT_SORT.

  GS_SORT-FIELDNAME = 'KSTAR'.
  APPEND GS_SORT TO GT_SORT.

ENDFORM.                    " ALV_SORT_0100
*&---------------------------------------------------------------------*
*&      Form  APPEND_FIELDCAT_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
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
      I_STRUCNAME            = 'ZCOS0190' "ABAP DIC. 정의된 STRUCTURE
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

  DATA: LV_TEXT(50),
        LV_POS    TYPE I VALUE 7.

  LOOP AT GT_FIELDCAT INTO GS_FIELDCAT.

    CLEAR: LV_TEXT.

    CASE GS_FIELDCAT-FIELDNAME.

      WHEN 'POSID'.
        LV_TEXT               = TEXT-C01.
        GS_FIELDCAT-COL_POS   = 1.
        GS_FIELDCAT-EMPHASIZE = 'C112'.
        GS_FIELDCAT-OUTPUTLEN = 10.

      WHEN 'POST1'.
        LV_TEXT               = TEXT-C02.
        GS_FIELDCAT-COL_POS   = 2.
        GS_FIELDCAT-EMPHASIZE = 'C112'.
        GS_FIELDCAT-OUTPUTLEN = 20.

      WHEN 'KSTAR'.
        LV_TEXT               = TEXT-C03.
        GS_FIELDCAT-COL_POS   = 3.
        GS_FIELDCAT-EMPHASIZE = 'C112'.
        GS_FIELDCAT-OUTPUTLEN = 10.

      WHEN 'KTEXT'.
        LV_TEXT               = TEXT-C04.
        GS_FIELDCAT-COL_POS   = 4.
        GS_FIELDCAT-EMPHASIZE = 'C112'.
        GS_FIELDCAT-OUTPUTLEN = 20.

      WHEN 'KSTXT'.
        GS_FIELDCAT-NO_OUT    = ABAP_TRUE.

      WHEN 'TOTAL'.
        LV_TEXT               = TEXT-C06.
        GS_FIELDCAT-COL_POS   = 6.
        GS_FIELDCAT-OUTPUTLEN = 10.

      WHEN 'WAERS'.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.

      WHEN 'MESSAGE'.
        LV_TEXT               = TEXT-C07.
        GS_FIELDCAT-OUTPUTLEN = 50.

    ENDCASE.

    IF GS_FIELDCAT-FIELDNAME CP 'WKF*'.

      CONCATENATE GS_FIELDCAT-FIELDNAME+3(2) '월' INTO LV_TEXT.
      GS_FIELDCAT-COL_POS   = LV_POS.
      GS_FIELDCAT-OUTPUTLEN = 10.
      GS_FIELDCAT-EDIT      = ABAP_TRUE.
      ADD 1 TO LV_POS.

    ENDIF.

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

  CONCATENATE TEXT-004 ':' PA_BUKRS '(' GV_BUTXT ')'
         INTO LV_TEXT SEPARATED BY SPACE.

  CALL METHOD GR_TOP_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
      SAP_EMPHASIS = CL_DD_AREA=>KEY
      STYLE_CLASS  = SPACE.

  CALL METHOD GR_TOP_DOCUMENT->NEW_LINE.

  CONCATENATE TEXT-002 ':' PA_GJAHR
        INTO LV_TEXT SEPARATED BY SPACE.

  CALL METHOD GR_TOP_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
      SAP_EMPHASIS = CL_DD_AREA=>KEY
      STYLE_CLASS  = SPACE.

  CALL METHOD GR_TOP_DOCUMENT->NEW_LINE.

  CONCATENATE TEXT-003 ':' PA_VERSN
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
  SET HANDLER:
*    GR_EVENT_RECEIVER->HANDLE_TOOLBAR       FOR ALL INSTANCES,
    GR_EVENT_RECEIVER->HANDLE_DATA_CHANGED  FOR ALL INSTANCES.
*    GR_EVENT_RECEIVER->HANDLE_USER_COMMAND  FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_HOTSPOT_CLICK FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_DOUBLE_CLICK  FOR ALL INSTANCES,
*    GR_EVENT_RECEIVER->HANDLE_ONF4          FOR ALL INSTANCES.

ENDFORM.                    " REGIST_ALV_EVENT_0100
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

ENDFORM.                    " REFRESH_GRID_0100
*&---------------------------------------------------------------------*
*&      Form  EVENT_DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM EVENT_DATA_CHANGED
       USING PR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
             PV_ONF4          TYPE CHAR01
             PV_ONF4_BEFORE   TYPE CHAR01
             PV_ONF4_AFTER    TYPE CHAR01
             PV_UCOMM         TYPE SY-UCOMM
             PR_SENDER       TYPE REF TO CL_GUI_ALV_GRID.


* - Prefix 정의 - Form Parameter
*   1. PT_  : Internal Table or Ranges
*   2. PS_  : Structure
*   3. PV_  : Variables
*   4. PR_  : Reference Variables

*--- Begin or Example
  DATA: LS_MOD_CELLS TYPE LVC_S_MODI.

  DEFINE _MODIFY_CELL.

    CALL METHOD PR_DATA_CHANGED->MODIFY_CELL
      EXPORTING
        I_FIELDNAME = &1
        I_ROW_ID    = &2
        I_VALUE     = &3.

  END-OF-DEFINITION.
*
*  DEFINE _GET_CELL_VALUE.
*    CALL METHOD PR_DATA_CHANGED->GET_CELL_VALUE
*      EXPORTING
*        I_FIELDNAME = &1
*        I_ROW_ID    = &2
*      IMPORTING
*        E_VALUE     = &3.
*  END-OF-DEFINITION.
*
*  DEFINE _ADD_PROTOCOL.
*    CALL METHOD PR_DATA_CHANGED->ADD_PROTOCOL_ENTRY
*      EXPORTING
*        I_FIELDNAME = &1
*        I_ROW_ID    = &2
*        I_MSGID     = 'ZMSTD'
*        I_MSGTY     = &3
*        I_MSGNO     = &4
*        I_MSGV1     = &5
*        I_MSGV2     = &6
*        I_MSGV3     = &7
*        I_MSGV4     = &8.
*  END-OF-DEFINITION.
*
*--- End of Example

  DATA: LV_INDEX     TYPE I,
        LV_MONTH     TYPE N LENGTH 2,
        LV_FIELDNAME TYPE FIELDNAME,
        LV_FIELD     TYPE FIELDNAME.

  FIELD-SYMBOLS: <FS_CNT>   TYPE ANY,
                 <FS_CNT_T> TYPE ANY..

  CASE PR_SENDER.
    WHEN GR_GRID1.
*      LOOP AT PR_DATA_CHANGED->MT_MOD_CELLS INTO LS_MOD_CELLS.
      LOOP AT PR_DATA_CHANGED->MT_GOOD_CELLS INTO LS_MOD_CELLS.

        IF LS_MOD_CELLS-FIELDNAME(3) CP 'WKF'.

          READ TABLE GT_DISPLAY INDEX LS_MOD_CELLS-ROW_ID ASSIGNING FIELD-SYMBOL(<LS_DISPLAY>).

          LV_MONTH = '01'.

          DO 12 TIMES.

            LV_FIELDNAME = '<LS_DISPLAY>-WKF' && LV_MONTH.
            ASSIGN (LV_FIELDNAME) TO <FS_CNT>.

            LV_FIELD = 'WKF' && LV_MONTH.

            IF LV_FIELD = LS_MOD_CELLS-FIELDNAME.

              <FS_CNT> = LS_MOD_CELLS-VALUE.

            ENDIF.

            IF LV_MONTH = '01'.
              CLEAR <LS_DISPLAY>-TOTAL.
            ENDIF.

            ASSIGN COMPONENT 'TOTAL' OF STRUCTURE <LS_DISPLAY> TO <FS_CNT_T>.
            <FS_CNT_T> = <FS_CNT_T> + <FS_CNT>.

            ADD 1 TO LV_MONTH.

          ENDDO.

        ENDIF.
      ENDLOOP.

      "-- ALV Refresh
      PERFORM REFRESH_GRID_0100.

    WHEN OTHERS.

  ENDCASE.

ENDFORM.                    " EVENT_DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM POPUP_TO_CONFIRM.

  "-- call popup
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR       = TEXT-PT1
*     DIAGNOSE_OBJECT             = ' '
      TEXT_QUESTION  = TEXT-QT1
*     TEXT_BUTTON_1  = 'Ja'(001)
*     ICON_BUTTON_1  = ' '
*     TEXT_BUTTON_2  = 'Nein'(002)
*     ICON_BUTTON_2  = ' '
*     DEFAULT_BUTTON = '1'
*     DISPLAY_CANCEL_BUTTON       = 'X'
*     USERDEFINED_F1_HELP         = ' '
*     START_COLUMN   = 25
*     START_ROW      = 6
*     POPUP_TYPE     =
*     IV_QUICKINFO_BUTTON_1       = ' '
*     IV_QUICKINFO_BUTTON_2       = ' '
    IMPORTING
      ANSWER         = GV_ANSWER
*   TABLES
*     PARAMETER      =
    EXCEPTIONS
      TEXT_NOT_FOUND = 1
      OTHERS         = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*& Form BAPI_COSTACTPLN_POSTPRIMCOST
*&---------------------------------------------------------------------*
FORM BAPI_COSTACTPLN_POSTPRIMCOST .
  DATA: LR_ERROR   TYPE REF TO CX_SY_SQL_ERROR,
        LV_MESSAGE TYPE STRING.

  DATA LV_FNAME TYPE STFNA.
  DATA LV_MONTH TYPE N LENGTH 2.

  FIELD-SYMBOLS: <FS1> TYPE ANY,
                 <FS2> TYPE ANY.

*  CLEAR: GV_ERROR, GV_SUCESS, GV_TOTAL.

  DATA : LS_HEADERINFO  LIKE  BAPIPLNHDR.

  DATA : LT_INDEXSTRUCTURE LIKE BAPIACPSTRU OCCURS 0 WITH HEADER LINE,
         LT_COOBJECT       LIKE BAPIPCPOBJ OCCURS 0 WITH HEADER LINE,
         LT_PERVALUE       LIKE BAPIPCPVAL OCCURS 0 WITH HEADER LINE.

  LOOP AT GT_DISPLAY INTO GS_DISPLAY.

    CLEAR: GT_RETURN, GT_RETURN[].

    CLEAR: LS_HEADERINFO, LT_INDEXSTRUCTURE,
                          LT_INDEXSTRUCTURE[],
                          LT_COOBJECT,
                          LT_COOBJECT[],
                          LT_PERVALUE,
                          LT_PERVALUE[].
    "Planning ( CJR2 )
*-- Header Data
    LS_HEADERINFO-CO_AREA     = PA_KOKRS.     "관리 회계영역
    LS_HEADERINFO-FISC_YEAR   = PA_GJAHR.     "회계연도
    LS_HEADERINFO-PERIOD_FROM = 1.            "기간 시작
    LS_HEADERINFO-PERIOD_TO   = 12.           "기간 종료
    LS_HEADERINFO-VERSION     = PA_VERSN.     "버전

*  CONCATENATE PA_GJAHR '년 사업계획' INTO LS_HEADERINFO-DOC_HDR_TX.

*-- 전표 헤더 텍스트
    LS_HEADERINFO-PLAN_CURRTYPE = 'C'. "통화

*-- CO-계획: 액티비티투입 & 주요지표 계획 BAPIs
    LT_INDEXSTRUCTURE-OBJECT_INDEX = 1.
    LT_INDEXSTRUCTURE-VALUE_INDEX  = 1.
    APPEND LT_INDEXSTRUCTURE.

*-- CO 계획: 1차 원가 BAPI에 대한 오브젝트
    LT_COOBJECT-OBJECT_INDEX = 1.
    LT_COOBJECT-WBS_ELEMENT  = GS_DISPLAY-POSID.
    APPEND LT_COOBJECT.

*-- CO 계획: 1차 원가 BAPI에 대한 값
    LT_PERVALUE-VALUE_INDEX  = 1.
    LT_PERVALUE-COST_ELEM    = GS_DISPLAY-KSTAR.   "원가요소
    LT_PERVALUE-TRANS_CURR   = GS_DISPLAY-WAERS.

    LV_MONTH = '01'.

    DO 12 TIMES.

      LV_FNAME = 'GS_DISPLAY-WKF' && LV_MONTH.
      ASSIGN (LV_FNAME) TO <FS1>.

      LV_FNAME = 'LT_PERVALUE-FIX_VAL_PER' && LV_MONTH.
      ASSIGN (LV_FNAME) TO <FS2>.

      IF GS_DISPLAY-WAERS = 'KRW'.
        <FS2> = <FS1> * 100.
      ELSE.
        <FS2> = <FS1>.
      ENDIF.

      ADD 1 TO LV_MONTH.

    ENDDO.

    APPEND LT_PERVALUE.

    CALL FUNCTION 'BAPI_COSTACTPLN_POSTPRIMCOST'
      EXPORTING
        HEADERINFO     = LS_HEADERINFO
      TABLES
        INDEXSTRUCTURE = LT_INDEXSTRUCTURE
        COOBJECT       = LT_COOBJECT
        PERVALUE       = LT_PERVALUE
        RETURN         = GT_RETURN.

    READ TABLE GT_RETURN WITH KEY TYPE = 'E'.
    IF SY-SUBRC EQ 0 .

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.


      PERFORM BUILD_MESSAGE USING    GT_RETURN
                            CHANGING LV_MESSAGE.

      GS_DISPLAY-MESSAGE = LV_MESSAGE.
      GV_EXIT = 'X'.

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = 'X'.

      GS_DISPLAY-MESSAGE = TEXT-S01.
    ENDIF.

    MODIFY GT_DISPLAY FROM GS_DISPLAY.
    CLEAR GS_DISPLAY.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_MESSAGE
*&---------------------------------------------------------------------*
FORM BUILD_MESSAGE  USING    PS_MESSAGE STRUCTURE BAPIRET2
                     CHANGING PV_TEXT.

  CLEAR PV_TEXT.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      MSGID               = PS_MESSAGE-ID
      MSGNR               = PS_MESSAGE-NUMBER
      MSGV1               = PS_MESSAGE-MESSAGE_V1
      MSGV2               = PS_MESSAGE-MESSAGE_V2
      MSGV3               = PS_MESSAGE-MESSAGE_V3
      MSGV4               = PS_MESSAGE-MESSAGE_V4
    IMPORTING
      MESSAGE_TEXT_OUTPUT = PV_TEXT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SAVE_DATA_RTN .
  DATA: LS_ZCOT0240 TYPE ZCOT0240.

  LOOP AT GT_DISPLAY INTO GS_DISPLAY.

    MOVE-CORRESPONDING GS_DISPLAY TO LS_ZCOT0240.
    LS_ZCOT0240-KOKRS = PA_KOKRS.
    LS_ZCOT0240-BUKRS = PA_BUKRS.
    LS_ZCOT0240-GJAHR = PA_GJAHR.
    LS_ZCOT0240-VERSN = PA_VERSN.

    LS_ZCOT0240-ERDAT = SY-DATUM.
    LS_ZCOT0240-ERZET = SY-UZEIT.
    LS_ZCOT0240-ERNAM = SY-UNAME.
    LS_ZCOT0240-AEDAT = SY-DATUM.
    LS_ZCOT0240-AEZET = SY-UZEIT.
    LS_ZCOT0240-AENAM = SY-UNAME.

    SELECT SINGLE * INTO @DATA(LS_ITAB)
      FROM ZCOT0240
     WHERE KOKRS = @PA_KOKRS
       AND BUKRS = @PA_BUKRS
       AND GJAHR = @PA_GJAHR
       AND VERSN = @PA_VERSN
       AND POSID = @GS_DISPLAY-POSID
       AND KSTAR = @GS_DISPLAY-KSTAR.
    IF SY-SUBRC EQ 0.
      LS_ZCOT0240-ERDAT = LS_ITAB-ERDAT.
      LS_ZCOT0240-ERZET = LS_ITAB-ERZET.
      LS_ZCOT0240-ERNAM = LS_ITAB-ERNAM.

      MODIFY ZCOT0240 FROM LS_ZCOT0240.

    ELSE.

      INSERT ZCOT0240 FROM LS_ZCOT0240.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " SAVE_DATA_RTN
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
