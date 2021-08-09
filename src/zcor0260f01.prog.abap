*&---------------------------------------------------------------------*
*& Include          ZCOR0160F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIAL_SET
*&---------------------------------------------------------------------*
FORM INITIAL_SET .

  SELECT SINGLE BEZEI, WAERS INTO (@PA_KTXT, @GV_WAERS)
    FROM TKA01
   WHERE KOKRS = '1000'.

**  SELECT SINGLE VTEXT INTO @PA_VTXT
**    FROM TKVS AS A
**    LEFT JOIN TKVST AS B
**      ON A~VERSI = B~VERSI
**     AND B~SPRAS = @SY-LANGU
**   WHERE A~VERSI = 'P0'.

  SELECT SINGLE B~TXT INTO @PA_VTXT
    FROM TKA09 AS A INNER JOIN TKT09 AS B
                       ON B~KOKRS EQ A~KOKRS
                      AND B~VERSN EQ A~VERSN
                      AND B~LANGU EQ @SY-LANGU
   WHERE A~KOKRS EQ @PA_KOKRS
     AND A~VERSN EQ @PA_VERSN.

  SELECT SINGLE VERSN_T INTO @PA_PVERT
    FROM ZCOT0150
   WHERE VERSN = @PA_PVER.

  "__ 20191223 BSGSM_FCM ADD default cac
  SET PARAMETER ID 'CAC' FIELD PA_KOKRS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_SELECTION_SCREEN
*&---------------------------------------------------------------------*
FORM CHECK_SELECTION_SCREEN .

  DATA LS_SETLEAF TYPE SETLEAF.

  IF PA_SPERL > PA_EPERL AND
      PA_EPERL IS NOT INITIAL.
    SET CURSOR FIELD 'PA_SPERL'.
    MESSAGE E018  WITH TEXT-E03.
  ENDIF.

**  SELECT SINGLE VTEXT INTO @PA_VTXT
**    FROM TKVS AS A
**    LEFT JOIN TKVST AS B
**      ON A~VERSI = B~VERSI
**     AND B~SPRAS = @SY-LANGU
**   WHERE A~VERSI = @PA_VERSN.


  SELECT SINGLE B~TXT INTO @PA_VTXT
    FROM TKA09 AS A INNER JOIN TKT09 AS B
                       ON B~KOKRS EQ A~KOKRS
                      AND B~VERSN EQ A~VERSN
                      AND B~LANGU EQ @SY-LANGU
   WHERE A~KOKRS EQ @PA_KOKRS
     AND A~VERSN EQ @PA_VERSN.

  IF SY-SUBRC <> 0.
    SET CURSOR FIELD 'PA_VERSN'.
    MESSAGE E027  WITH TEXT-002.
  ENDIF.

  SELECT SINGLE VERSN_T INTO @PA_PVERT
    FROM ZCOT0150
   WHERE KOKRS  = @PA_KOKRS
     AND VERSN  = @PA_PVER.

  SELECT SINGLE VERSN_T INTO @PA_PVERT
    FROM ZCOT0150
   WHERE KOKRS  = @PA_KOKRS
     AND VERSN  = @PA_PVER.

  SELECT SINGLE VERSN INTO @DATA(LV_VERSN)
    FROM ZCOT0150
   WHERE KOKRS  = @PA_KOKRS
     AND VERSN  = @PA_PVER.

  IF SY-SUBRC <> 0.
    SET CURSOR FIELD 'PA_PVER'.
    MESSAGE E027  WITH TEXT-E02.
  ENDIF.

  IF SO_POSID[] IS NOT INITIAL AND
     PA_PDGR IS NOT INITIAL.
    SET CURSOR FIELD 'SO_POSID-LOW'.
    MESSAGE E026  WITH TEXT-E04.
  ENDIF.

  IF SO_POSID[] IS NOT INITIAL.

    SELECT SINGLE * FROM PRPS
      INTO @DATA(LS_PRPS)
     WHERE POSID IN @SO_POSID.

    IF SY-SUBRC <> 0.
      SET CURSOR FIELD 'SO_POSID-LOW'.
      MESSAGE E027  WITH TEXT-E07.
    ENDIF.

  ENDIF.

  IF PA_PDGR IS NOT INITIAL.

    SELECT SINGLE * FROM SETLEAF
      INTO @LS_SETLEAF
     WHERE SETCLASS = '0110'
       AND SETNAME  = @PA_PDGR.

    IF SY-SUBRC <> 0.
      SET CURSOR FIELD 'PA_PDGR'.
      MESSAGE E027  WITH TEXT-E08.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
FORM SELECTED_DATA_RTN .

  DATA LT_DATA   TYPE TABLE OF TY_DATA   WITH HEADER LINE.
  DATA LT_ACDOCA TYPE TABLE OF TY_ACDOCA WITH HEADER LINE.

  DATA LV_WHERE TYPE STRING.

  DATA LV_WRTTP TYPE WRTTP.

  CASE PA_VERSN.

    WHEN '000'.
      LV_WRTTP = '04'.

    WHEN OTHERS.
      LV_WRTTP = '01'.

  ENDCASE.

  CONCATENATE  'A~VERSN'  '=' '@PA_VERSN' 'AND'  'A~WRTTP' '='
               '@LV_WRTTP'
          INTO LV_WHERE SEPARATED BY SPACE.

  SELECT * FROM ZCOT0150
    INTO TABLE @GT_ZCOT0150
   WHERE KOKRS = @PA_KOKRS
     AND VERSN = @PA_PVER.

  IF SY-SUBRC <> 0.
    MESSAGE S004 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  SELECT A~KSTAR, B~KTEXT
    INTO TABLE @GT_CSKA
    FROM CSKA AS A
    LEFT JOIN CSKU AS B
      ON A~KTOPL = B~KTOPL
     AND A~KSTAR = B~KSTAR
     AND B~SPRAS = @SY-LANGU
   WHERE A~KTOPL = @GC_KTOPL.

  SELECT * FROM SETNODE
    INTO TABLE @GT_SETNODE
   WHERE SETCLASS = '0102'
     AND SUBCLASS = @PA_KOKRS.

  CLEAR: R_KSTAR, R_KSTAR[].

  LOOP AT GT_ZCOT0150 WHERE GUBUN = '2'.
    PERFORM SET_KSTAR_RANGES TABLES R_KSTAR
                             USING GT_ZCOT0150-FKAGRU
                                   GT_ZCOT0150-TKAGRU.
  ENDLOOP.

  SELECT A~VERSN, A~WRTTP, A~GJAHR, A~OBJNR, A~KSTAR, B~KTEXT,
         SUM( A~WKG001 ) AS WKG001, SUM( A~WKG002 ) AS WKG002,
         SUM( A~WKG003 ) AS WKG003, SUM( A~WKG004 ) AS WKG004,
         SUM( A~WKG005 ) AS WKG005, SUM( A~WKG006 ) AS WKG006,
         SUM( A~WKG007 ) AS WKG007, SUM( A~WKG008 ) AS WKG008,
         SUM( A~WKG009 ) AS WKG009, SUM( A~WKG010 ) AS WKG010,
         SUM( A~WKG011 ) AS WKG011, SUM( A~WKG012 ) AS WKG012
    FROM COSP AS A
    LEFT JOIN CSKU AS B
      ON A~KSTAR = B~KSTAR
     AND B~KTOPL = @GV_KTOPL
     AND B~SPRAS = @SY-LANGU
   WHERE A~LEDNR = '00'
     AND A~GJAHR = @PA_GJAHR
     AND A~KSTAR IN @R_KSTAR
     AND A~OBJNR IN @R_OBJNR
     AND ( (LV_WHERE) OR
           ( A~VERSN = '000'  AND A~WRTTP = '01' AND
             A~KSTAR = '0984000010' ) )
   GROUP BY A~VERSN, A~WRTTP, A~GJAHR, A~OBJNR, A~KSTAR, B~KTEXT
   UNION ALL
  SELECT A~VERSN, A~WRTTP, A~GJAHR, A~OBJNR, A~KSTAR, B~KTEXT,
         SUM( A~WKG001 ) AS WKG001, SUM( A~WKG002 ) AS WKG002,
         SUM( A~WKG003 ) AS WKG003, SUM( A~WKG004 ) AS WKG004,
         SUM( A~WKG005 ) AS WKG005, SUM( A~WKG006 ) AS WKG006,
         SUM( A~WKG007 ) AS WKG007, SUM( A~WKG008 ) AS WKG008,
         SUM( A~WKG009 ) AS WKG009, SUM( A~WKG010 ) AS WKG010,
         SUM( A~WKG011 ) AS WKG011, SUM( A~WKG012 ) AS WKG012
    FROM COSS AS A
    LEFT JOIN CSKU AS B
      ON A~KSTAR = B~KSTAR
     AND B~KTOPL = @GV_KTOPL
     AND B~SPRAS = @SY-LANGU
   WHERE A~LEDNR = '00'
     AND A~GJAHR = @PA_GJAHR
     AND A~KSTAR IN @R_KSTAR
     AND A~OBJNR IN @R_OBJNR
     AND ( (LV_WHERE) OR
           ( A~VERSN = '000'  AND A~WRTTP = '01' AND
             A~KSTAR = '0984000010' ) )
   GROUP BY A~VERSN, A~WRTTP, A~GJAHR, A~OBJNR, A~KSTAR, B~KTEXT

    UNION ALL
  SELECT A~RVERS  AS VERSN, A~WRTTP, A~RYEAR AS GJAHR,
         A~ROBJNR AS OBJNR, A~RKSTAR AS KSTAR, B~KTEXT,
        SUM( A~HSL01 ) AS WKG001, SUM( A~HSL02 ) AS WKG002,
        SUM( A~HSL03 ) AS WKG003, SUM( A~HSL04 ) AS WKG004,
        SUM( A~HSL05 ) AS WKG005, SUM( A~HSL06 ) AS WKG006,
        SUM( A~HSL07 ) AS WKG007, SUM( A~HSL08 ) AS WKG008,
        SUM( A~HSL09 ) AS WKG009, SUM( A~HSL10 ) AS WKG010,
        SUM( A~HSL11 ) AS WKG011, SUM( A~HSL12 ) AS WKG012
    FROM ZCOT1180 AS A
    LEFT JOIN CSKU AS B
      ON A~RKSTAR = B~KSTAR
     AND B~KTOPL  = @GV_KTOPL
     AND B~SPRAS  = @SY-LANGU
   WHERE A~RVERS  = '000'
     AND A~WRTTP  = '04'
     AND A~RKOKRS = @PA_KOKRS
     AND A~RYEAR  = @PA_GJAHR
     AND A~RKSTAR IN @R_KSTAR
     AND A~ROBJNR IN @R_OBJNR
   GROUP BY A~RVERS, A~WRTTP, A~RYEAR,
            A~ROBJNR, A~RKSTAR, B~KTEXT

    INTO TABLE @GT_DATA.

*-- 관계사간 거래 ADD
  SELECT A~SPMON, A~SAKNR, A~TXT20, B~OBJNR,
         SUM( A~DMBTR ) AS DMBTR
    INTO TABLE @DATA(LT_ZFIT0620)
    FROM ZFIT0620 AS A
    LEFT JOIN PRPS AS B
      ON A~POSID = B~POSID
   WHERE A~POSID IN @R_POSID2
     AND A~SPMON IN @R_SPMON
   GROUP BY A~SPMON, A~SAKNR, A~TXT20,
            B~OBJNR.

  LOOP AT LT_ZFIT0620 INTO DATA(LS_ZFIT0620).

    MOVE: '000'              TO GT_DATA-VERSN,
          '04'               TO GT_DATA-WRTTP,
          PA_GJAHR           TO GT_DATA-GJAHR,
          LS_ZFIT0620-OBJNR  TO GT_DATA-OBJNR,
          LS_ZFIT0620-SAKNR  TO GT_DATA-KSTAR,
          LS_ZFIT0620-TXT20  TO GT_DATA-KTEXT.

    DATA(LV_FIELDNAME) = 'WKG0' && LS_ZFIT0620-SPMON+4(2).

    ASSIGN COMPONENT LV_FIELDNAME
      OF STRUCTURE GT_DATA TO FIELD-SYMBOL(<FS_WKG>).

    MOVE LS_ZFIT0620-DMBTR TO <FS_WKG>.

    COLLECT GT_DATA.
    CLEAR   GT_DATA.

  ENDLOOP.

  DELETE GT_DATA WHERE WKG001 = 0
                   AND WKG002 = 0
                   AND WKG003 = 0
                   AND WKG004 = 0
                   AND WKG005 = 0
                   AND WKG006 = 0
                   AND WKG007 = 0
                   AND WKG008 = 0
                   AND WKG009 = 0
                   AND WKG010 = 0
                   AND WKG011 = 0
                   AND WKG012 = 0.

*-- 금액이 0인 WBS 삭제 처리
  LOOP AT GT_PRPS.

    CLEAR: LT_DATA[], LT_ACDOCA[].

    LOOP AT GT_DATA WHERE OBJNR = GT_PRPS-OBJNR.
      MOVE GT_DATA TO LT_DATA.
      CLEAR: LT_DATA-VERSN, LT_DATA-WRTTP,
             LT_DATA-GJAHR, LT_DATA-KSTAR,
             LT_DATA-KTEXT.
      COLLECT LT_DATA.
    ENDLOOP.

    IF LT_DATA[] IS INITIAL.
      DELETE GT_PRPS WHERE OBJNR = GT_PRPS-OBJNR.
    ENDIF.

  ENDLOOP.

  IF GT_PRPS[] IS INITIAL.
    MESSAGE S004 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  PERFORM SET_TREE_DATA TABLES R_POSID.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SCREEN
*&---------------------------------------------------------------------*
FORM SET_SCREEN .

  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'MG1'.
      SCREEN-INPUT = 0 .
    ENDIF.
    MODIFY SCREEN.
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
*& Form SET_RANGES_OBJNR
*&---------------------------------------------------------------------*
FORM SET_RANGES_OBJNR .

  DATA LV_MONTH TYPE N LENGTH 2.

  CLEAR: R_OBJNR,   R_OBJNR[],
         R_KSTAR,   R_KSTAR[],
         R_SPMON,   R_SPMON[],
         R_POSID2,  R_POSID2[].

  CLEAR: GT_MONTH, GT_MONTH[].

  RANGES R_POSID FOR PRPS-POSID.

  PERFORM AUTHORITY_CHECK.

  IF SO_POSID[] IS NOT INITIAL.

    R_POSID[] = SO_POSID[].

  ENDIF.

  IF PA_PDGR IS NOT INITIAL.

    PERFORM READ_HIERARCHY_TABLES TABLES GT_VALUES
                                  USING '0110'
                                        PA_PDGR.   "WBS 요소 그룹
    LOOP AT GT_VALUES.

      MOVE: 'I'   TO R_POSID-SIGN,
            'BT'  TO R_POSID-OPTION.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
        EXPORTING
          INPUT     = GT_VALUES-VFROM
        IMPORTING
          OUTPUT    = R_POSID-LOW
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
        EXPORTING
          INPUT     = GT_VALUES-VTO
        IMPORTING
          OUTPUT    = R_POSID-HIGH
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.

      COLLECT R_POSID.
      CLEAR   R_POSID.

    ENDLOOP.

  ENDIF.

  IF PA_EPERL IS NOT INITIAL.

    MOVE: 'I'   TO R_SPMON-SIGN,
          'BT'  TO R_SPMON-OPTION.

    R_SPMON-LOW  = PA_GJAHR && PA_SPERL+1(2).
    R_SPMON-HIGH = PA_GJAHR && PA_EPERL+1(2).
    APPEND R_SPMON.

  ELSE.

    MOVE: 'I'   TO R_SPMON-SIGN,
          'EQ'  TO R_SPMON-OPTION.

    R_SPMON-LOW  = PA_GJAHR && PA_SPERL+1(2).
    APPEND R_SPMON.

  ENDIF.

  SELECT B~PSPID, B~POST1 AS PSPTX,
         A~OBJNR, A~PSPNR, A~POSID, A~POST1
    FROM PRPS AS A
   INNER JOIN PROJ AS B
      ON A~PSPHI = B~PSPNR
    INTO TABLE @GT_PRPS
   WHERE A~OBJNR IN @R_OBJNR
     AND A~PBUKR IN @SO_BUKRS
     AND A~POSID IN @R_POSID
     AND B~PSPID IN @SO_PSPID
     AND A~PRCTR IN @SO_PRCTR
     AND A~LOEVM = @SPACE
     AND B~LOEVM = @SPACE
     AND A~PKOKR = @PA_KOKRS
     AND A~PBUKR IN @R_BUKRS
     AND A~PRCTR IN @R_PRCTR1
     AND B~PSPID IN @R_PSPID
     AND A~ZZSCT IN @SO_ZZSCT
     AND A~ZZPHA IN @SO_ZZPHA
     AND A~ZZWBT IN @SO_ZZWBT
     AND A~ZZBGU IN @SO_ZZBGU
     AND A~ZZBGD IN @SO_ZZBGD
     AND A~ZZPRG IN @SO_ZZPRG
     AND A~ZZADT IN @SO_ZZADT
     AND A~ZZHWB IN @SO_ZZHWB
     AND A~ZZBAG IN @SO_ZZBAG
     AND A~ZZIVC IN @SO_ZZIVC
     AND A~ZZCOP IN @SO_ZZCOP
    ORDER BY B~PSPID, A~POSID.

  IF SY-SUBRC <> 0 .
    MESSAGE S004 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  CLEAR: R_OBJNR,  R_OBJNR[],
         R_POSID2, R_POSID[].

  LOOP AT GT_PRPS.

    MOVE: 'I'           TO R_OBJNR-SIGN,
          'EQ'          TO R_OBJNR-OPTION,
          GT_PRPS-OBJNR TO R_OBJNR-LOW.

    COLLECT R_OBJNR.
    CLEAR   R_OBJNR.

    MOVE: 'I'           TO R_POSID2-SIGN,
          'EQ'          TO R_POSID2-OPTION,
          GT_PRPS-POSID TO R_POSID2-LOW.

    COLLECT R_POSID2.
    CLEAR   R_POSID2.


  ENDLOOP.

  LV_MONTH = PA_SPERL+1(2).

  DO .
    MOVE LV_MONTH TO GT_MONTH-V1.
    APPEND GT_MONTH.

    ADD 1 TO LV_MONTH.
    IF LV_MONTH > PA_EPERL+1(2).
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form READ_HIERARCHY_TABLES
*&---------------------------------------------------------------------*
FORM READ_HIERARCHY_TABLES  TABLES PT_VALUES STRUCTURE GRPVALUES
                            USING PV_CLASS TYPE SETCLASS
                                  PV_SETID.

  DATA: LV_SETID     LIKE SETHIER-SETID,
        LV_OVERWRITE LIKE SY-DATAR,
        LT_INFO      LIKE GRPHINFO OCCURS 0 WITH HEADER LINE,
        LT_NODES     LIKE GRPOBJECTS OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'G_SET_ENCRYPT_SETID'
    EXPORTING
      SETCLASS  = PV_CLASS
      SHORTNAME = PV_SETID
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
*& Form CREATE_INSTANCE_0100
*&---------------------------------------------------------------------*
FORM CREATE_INSTANCE_0100 .

  CREATE OBJECT GR_SPLITTER1
    EXPORTING
      ROWS    = 1
      COLUMNS = 2
      PARENT  = CL_GUI_SPLITTER_CONTAINER=>SCREEN0.

*-- 1. LEFT data
  GR_LEFT_CONTAINER = GR_SPLITTER1->GET_CONTAINER(
      ROW       = 1
      COLUMN    = 1 ).

*-- 2. data
  GR_DATA_CONTAINER = GR_SPLITTER1->GET_CONTAINER(
      ROW       = 1
      COLUMN    = 2 ).

*-- 1. Left data
  CALL METHOD GR_SPLITTER1->SET_COLUMN_WIDTH
    EXPORTING
      ID    = 1
      WIDTH = 20.

*-- 2. Left data
  CREATE OBJECT GR_TREE1
    EXPORTING
      PARENT                      = GR_LEFT_CONTAINER
      NODE_SELECTION_MODE         = 0
      ITEM_SELECTION              = ABAP_TRUE
      NO_HTML_HEADER              = ABAP_TRUE
      NO_TOOLBAR                  = SPACE
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      ILLEGAL_NODE_SELECTION_MODE = 5
      FAILED                      = 6
      ILLEGAL_COLUMN_NAME         = 7.

  CREATE OBJECT GR_TREE_DATA
    EXPORTING
      PARENT                      = GR_DATA_CONTAINER
      NODE_SELECTION_MODE         = 0
      ITEM_SELECTION              = ABAP_TRUE
      NO_HTML_HEADER              = ABAP_FALSE
      NO_TOOLBAR                  = SPACE
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      ILLEGAL_NODE_SELECTION_MODE = 5
      FAILED                      = 6
      ILLEGAL_COLUMN_NAME         = 7.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REGIST_ALV_EVENT_0100
*&---------------------------------------------------------------------*
FORM REGIST_ALV_EVENT_0100.

  DATA: LT_EVENTS TYPE CNTL_SIMPLE_EVENTS,
        LS_EVENT  TYPE CNTL_SIMPLE_EVENT.

* REGISTER EVENT
  CALL METHOD GR_TREE1->GET_REGISTERED_EVENTS
    IMPORTING
      EVENTS = LT_EVENTS.

  LS_EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_NODE_DOUBLE_CLICK.
  APPEND LS_EVENT TO LT_EVENTS.

  LS_EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_ITEM_DOUBLE_CLICK.
  APPEND LS_EVENT TO LT_EVENTS.

  LS_EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_HEADER_CLICK.
  APPEND LS_EVENT TO LT_EVENTS.

  CALL METHOD GR_TREE1->SET_REGISTERED_EVENTS
    EXPORTING
      EVENTS                    = LT_EVENTS
    EXCEPTIONS
      CNTL_ERROR                = 1
      CNTL_SYSTEM_ERROR         = 2
      ILLEGAL_EVENT_COMBINATION = 3.

  IF SY-SUBRC <> 0.
    MESSAGE X000 WITH 'ERROR'.
  ENDIF.

*-- GR_EVENT_RECEIVER
  IF GR_TREE_EVENT_RECEIVER IS INITIAL.
    CREATE OBJECT GR_TREE_EVENT_RECEIVER.
  ENDIF.

* Handler Event
  SET HANDLER:
    GR_TREE_EVENT_RECEIVER->HANDLE_NODE_DOUBLE_CLICK
        FOR GR_TREE1,
    GR_TREE_EVENT_RECEIVER->HANDLE_ITEM_DOUBLE_CLICK
        FOR GR_TREE1,
    GR_TREE_EVENT_RECEIVER->HANDLE_HEADER_CLICK
        FOR GR_TREE1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_NODE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM EVENT_NODE_DOUBLE_CLICK
                  USING P_NODE_KEY TYPE LVC_NKEY
                        P_SENDER TYPE REF TO CL_GUI_ALV_TREE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_NODE_ITEM_DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM EVENT_NODE_ITEM_DOUBLE_CLICK  USING P_NODE_KEY TYPE LVC_NKEY
                                         P_FIELDNAME TYPE LVC_FNAME
                                         SENDER.

  RANGES: LR_BUDAT  FOR COBK-BUDAT,
          LR_KSTAR  FOR CSKB-KSTAR,
          LR_POSID  FOR PRPS_R-POSID.

  DATA LV_KOAGR TYPE KAGRU.

  DATA LS_POSID TYPE CURTO_PSPNR_RANGE.

  CASE SENDER.

    WHEN GR_TREE1.

      CLEAR GS_SELECT.

      CLEAR: R_POSID, R_POSID[].

      READ TABLE GT_PRPS_DISPLAY INTO GS_SELECT INDEX P_NODE_KEY.

      IF SY-SUBRC <> 0.
        EXIT.
      ENDIF.

      IF GS_SELECT-PSPID IS NOT INITIAL AND      "Project
         GS_SELECT-POSID IS INITIAL.

        LOOP AT GT_PRPS WHERE PSPID = GS_SELECT-PSPID.

          MOVE: GT_PRPS-OBJNR TO LS_POSID-LOW,
                'I'           TO LS_POSID-SIGN,
                'EQ'          TO LS_POSID-OPTION.

          APPEND LS_POSID TO R_POSID.

        ENDLOOP.

      ELSEIF GS_SELECT-PSPID IS NOT INITIAL AND  "WBS
              GS_SELECT-POSID IS NOT INITIAL.

        LOOP AT GT_PRPS WHERE PSPID = GS_SELECT-PSPID
                          AND POSID = GS_SELECT-POSID.

          MOVE: GT_PRPS-OBJNR TO LS_POSID-LOW,
                'I'           TO LS_POSID-SIGN,
                'EQ'          TO LS_POSID-OPTION.

          APPEND LS_POSID TO R_POSID.

        ENDLOOP.

      ELSE.       "ALL



      ENDIF.

      CLEAR: GT_OUTTAB_DISPLAY[].

      PERFORM SET_TREE_DATA TABLES R_POSID.
      PERFORM FREE_DATA_TREE.
      PERFORM DISPLAY_ALV_TREE_0100_01.
      PERFORM REGIST_ALV_EVENT_0100_01.    "TREE EVENT

      MESSAGE S005 .

    WHEN GR_TREE_DATA.

      CASE P_FIELDNAME.

        WHEN 'F1HSL'.

          SELECT DISTINCT
                 'I' AS SIGN, 'EQ' AS OPTION,
                 POSID AS LOW
           INTO CORRESPONDING FIELDS OF TABLE @LR_POSID
            FROM PRPS
           WHERE OBJNR IN @R_POSID.

          READ TABLE GT_OUTTAB_DISPLAY INTO DATA(LS_DISPLAY)
                 INDEX P_NODE_KEY.

          IF LS_DISPLAY-SETID IS NOT INITIAL AND
             LS_DISPLAY-KSTAR IS NOT INITIAL.

            CONCATENATE 'IEQ' LS_DISPLAY-KSTAR INTO LR_KSTAR.
            APPEND LR_KSTAR.

          ELSEIF LS_DISPLAY-SETID IS NOT INITIAL AND
                  LS_DISPLAY-KSTAR IS INITIAL.

            MOVE LS_DISPLAY-SETID TO LV_KOAGR.

          ELSE.

            READ TABLE GT_ZCOT0150 WITH KEY ZCODE = LS_DISPLAY-ZCODE.

            IF SY-SUBRC = 0.

              PERFORM SET_KSTAR_RANGES TABLES LR_KSTAR
                                       USING GT_ZCOT0150-FKAGRU
                                             GT_ZCOT0150-TKAGRU.

            ENDIF.

          ENDIF.

          MOVE: 'I'  TO LR_BUDAT-SIGN,
                'BT' TO LR_BUDAT-OPTION.

          CONCATENATE PA_GJAHR PA_SPERL+1(2) '01' INTO LR_BUDAT-LOW.
          CONCATENATE PA_GJAHR PA_EPERL+1(2) '01' INTO LR_BUDAT-HIGH.

          CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
            EXPORTING
              DAY_IN            = LR_BUDAT-HIGH
            IMPORTING
              LAST_DAY_OF_MONTH = LR_BUDAT-HIGH
            EXCEPTIONS
              DAY_IN_NO_DATE    = 1
              OTHERS            = 2.

          APPEND LR_BUDAT.

          SUBMIT RKPEP003  USING SELECTION-SCREEN '1000'
               WITH CN_PSPNR    IN LR_POSID
               WITH R_KSTAR     IN LR_KSTAR
               WITH KOAGR       = LV_KOAGR
               WITH R_BUDAT     IN LR_BUDAT
               AND RETURN.
      ENDCASE.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_NODE_HANDLE_HEADER_CLICK
*&---------------------------------------------------------------------*
FORM EVENT_NODE_HANDLE_HEADER_CLICK  USING P_FIELDNAME TYPE LVC_FNAME
                                           SENDER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_HIERARCHY_HEADER_0100
*&---------------------------------------------------------------------*
FORM BUILD_HIERARCHY_HEADER_0100 .

  GS_HIERARCHY_HEADER-HEADING = TEXT-H01.
  GS_HIERARCHY_HEADER-WIDTH = 30.
  GS_HIERARCHY_HEADER-WIDTH_PIX = ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV_TREE_0100
*&---------------------------------------------------------------------*
FORM DISPLAY_ALV_TREE_0100 .

  CALL METHOD GR_TREE1->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_HIERARCHY_HEADER  = GS_HIERARCHY_HEADER
      IT_TOOLBAR_EXCLUDING = GT_EXCLUDE[]
    CHANGING
      IT_OUTTAB            = GT_PRPS_DISPLAY[]
      IT_FIELDCATALOG      = GT_FIELDCAT[].

  PERFORM BUILD_HIERARCHY.

* calculate totals
  CALL METHOD GR_TREE1->UPDATE_CALCULATIONS.

* this method must be called to send the data to the frontend
  CALL METHOD GR_TREE1->FRONTEND_UPDATE.

  CALL METHOD CL_GUI_CFW=>FLUSH
    EXCEPTIONS
      CNTL_SYSTEM_ERROR = 1
      CNTL_ERROR        = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_FIELDCAT_0100
*&---------------------------------------------------------------------*
FORM APPEND_FIELDCAT_0100 .

  PERFORM GET_FIELDCATLOG_DATA.
  PERFORM MODIFY_FIELDCATLOG_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
FORM GET_FIELDCATLOG_DATA .

  DATA: LT_FIELDCAT TYPE KKBLO_T_FIELDCAT.

  CALL FUNCTION 'K_KKB_FIELDCAT_MERGE'
    EXPORTING
      I_CALLBACK_PROGRAM     = SY-REPID
      I_STRUCNAME            = 'ZCOS0180' "ABAP DIC. 정의된 STRUCTURE
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

    " Error Fieldcatalog merge!!
    MESSAGE E020(ZMSTDA).

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
FORM MODIFY_FIELDCATLOG_DATA .

  DATA:  LV_TEXT(50).

  LOOP AT GT_FIELDCAT INTO GS_FIELDCAT.

    CLEAR: LV_TEXT.

    CASE GS_FIELDCAT-FIELDNAME.

      WHEN 'DESCR'.
        LV_TEXT = TEXT-C01.

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

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_NODEKEY
*&---------------------------------------------------------------------*
FORM ADD_NODEKEY  USING   PV_RELAT_KEY TYPE LVC_NKEY
                           PR_TREE       TYPE REF TO CL_GUI_ALV_TREE
                           PS_OUTTAB     TYPE ZCOS0180
                           P_GUBUN
                  CHANGING PV_NODE_KEY  TYPE LVC_NKEY.

  DATA: LV_NODE_TEXT TYPE LVC_VALUE.

* set item-layout
  DATA: LT_ITEM_LAYOUT TYPE LVC_T_LAYI,
        LS_ITEM_LAYOUT TYPE LVC_S_LAYI.

  DATA LS_DISPLAY TYPE ZCOS0180.
  MOVE PS_OUTTAB TO LS_DISPLAY.

  DATA LS_NODE_LAYOUT TYPE LVC_S_LAYN.

  DATA: LV_PSPID TYPE PS_PSPID,
        LV_POSID TYPE PS_POSID.

  CASE P_GUBUN.

    WHEN 'ALL'.
      CLEAR LS_DISPLAY.
      LV_NODE_TEXT = TEXT-T06.

    WHEN 'PSPID'.

      CLEAR: LS_DISPLAY-POSID,
             LS_DISPLAY-OBJNR,
             LS_DISPLAY-PSPNR,
             LS_DISPLAY-POSID,
             LS_DISPLAY-POST1.

      LS_NODE_LAYOUT-N_IMAGE   = '@EC@'.
      LS_NODE_LAYOUT-EXP_IMAGE = '@EC@'.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
        EXPORTING
          INPUT  = PS_OUTTAB-PSPID
        IMPORTING
          OUTPUT = LV_PSPID.

*      CONCATENATE LV_PSPID '(' PS_OUTTAB-PSPTX ')'
*            INTO LV_NODE_TEXT SEPARATED BY SPACE.

      LV_NODE_TEXT = LV_PSPID.

    WHEN 'POSID'.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
        EXPORTING
          INPUT  = PS_OUTTAB-POSID
        IMPORTING
          OUTPUT = LV_POSID.

      LS_NODE_LAYOUT-ISFOLDER  = ' '.

      LS_NODE_LAYOUT-N_IMAGE   = '@ED@'.
      LS_NODE_LAYOUT-EXP_IMAGE = '@ED@'.
      LS_ITEM_LAYOUT-FIELDNAME = PR_TREE->C_HIERARCHY_COLUMN_NAME.
      LS_ITEM_LAYOUT-STYLE =
       CL_GUI_COLUMN_TREE=>STYLE_EMPHASIZED.

      APPEND LS_ITEM_LAYOUT TO LT_ITEM_LAYOUT.

      LV_NODE_TEXT = LV_POSID.

  ENDCASE.

  CALL METHOD PR_TREE->ADD_NODE
    EXPORTING
      I_RELAT_NODE_KEY = PV_RELAT_KEY
      I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
      I_NODE_TEXT      = LV_NODE_TEXT
      IS_OUTTAB_LINE   = LS_DISPLAY
      IS_NODE_LAYOUT   = LS_NODE_LAYOUT
      IT_ITEM_LAYOUT   = LT_ITEM_LAYOUT
    IMPORTING
      E_NEW_NODE_KEY   = PV_NODE_KEY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_HIERARCHY
*&---------------------------------------------------------------------*
FORM BUILD_HIERARCHY .

  DATA: LV_KEY1 TYPE LVC_NKEY,
        LV_KEY2 TYPE LVC_NKEY,
        LV_KEY3 TYPE LVC_NKEY,
        LV_ROOT TYPE LVC_NKEY.

  DATA LS_PRPS TYPE ZCOS0180.

  PERFORM ADD_NODEKEY     USING    ''
                                   GR_TREE1
                                   LS_PRPS
                                   'ALL'
                          CHANGING LV_ROOT.

  LOOP AT GT_PRPS.

    MOVE GT_PRPS TO LS_PRPS.

    AT NEW PSPID.

      MOVE LS_PRPS-PSPTX TO LS_PRPS-DESCR.

      PERFORM ADD_NODEKEY     USING    LV_ROOT
                                       GR_TREE1
                                       LS_PRPS
                                       'PSPID'
                              CHANGING LV_KEY2.
    ENDAT.

    MOVE LS_PRPS-POST1 TO LS_PRPS-DESCR.

    PERFORM ADD_NODEKEY     USING    LV_KEY2
                                     GR_TREE1
                                     LS_PRPS
                                     'POSID'
                            CHANGING LV_KEY3.

    MODIFY GT_PRPS FROM LS_PRPS.

  ENDLOOP.

  CALL METHOD GR_TREE1->EXPAND_NODE
    EXPORTING
      I_NODE_KEY = LV_ROOT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_FIELDCAT_0100_01
*&---------------------------------------------------------------------*
FORM APPEND_FIELDCAT_0100_01 .

  PERFORM GET_FIELDCATLOG_DATA2.
  PERFORM MODIFY_FIELDCATLOG_DATA2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_FIELDCATLOG_DATA2
*&---------------------------------------------------------------------*
FORM GET_FIELDCATLOG_DATA2 .

  DATA: LT_FIELDCAT TYPE KKBLO_T_FIELDCAT.

  CALL FUNCTION 'K_KKB_FIELDCAT_MERGE'
    EXPORTING
      I_CALLBACK_PROGRAM     = SY-REPID
      I_STRUCNAME            = 'ZCOS0260' "ABAP DIC. 정의된 STRUCTURE
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
        ET_FIELDCAT_LVC   = GT_FIELDCAT2[]
      EXCEPTIONS
        IT_DATA_MISSING   = 1.
  ELSE.

    " Error Fieldcatalog merge!!
    MESSAGE E020(ZMSTDA).

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_FIELDCATLOG_DATA2
*&---------------------------------------------------------------------*
FORM MODIFY_FIELDCATLOG_DATA2 .

  DATA:  LV_TEXT(50).

  LOOP AT GT_FIELDCAT2 INTO GS_FIELDCAT.

    CLEAR: LV_TEXT.

    CASE GS_FIELDCAT-FIELDNAME.

      WHEN 'DESCR'.
        LV_TEXT = TEXT-C01.
        GS_FIELDCAT-OUTPUTLEN = 30.

      WHEN 'HSLSUM'.
        LV_TEXT = TEXT-C02.
        GS_FIELDCAT-DO_SUM = ABAP_TRUE.

        GS_FIELDCAT-OUTPUTLEN = 20.

      WHEN 'ZCODE' OR 'GUBUN' OR 'SETTEXT' OR
            'SETID' OR 'SHORTNAME' OR 'KSTAR' OR
            'KTEXT' OR 'SUBNAME' OR 'HLEVEL' OR 'TYPE' OR
            'WAERS'.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.

    ENDCASE.

    IF GS_FIELDCAT-FIELDNAME CP 'HSL*' AND
       GS_FIELDCAT-FIELDNAME <> 'HSLSUM'.

      CONCATENATE GS_FIELDCAT-FIELDNAME+3(2) TEXT-T05
            INTO LV_TEXT SEPARATED BY SPACE.

      GS_FIELDCAT-DO_SUM = ABAP_TRUE.

      READ TABLE GT_MONTH
        WITH KEY V1 = GS_FIELDCAT-FIELDNAME+3(2).

      IF SY-SUBRC <> 0 .
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
      ENDIF.

      GS_FIELDCAT-OUTPUTLEN = 20.

    ENDIF.

    IF LV_TEXT IS NOT INITIAL.
      GS_FIELDCAT-COLTEXT   = LV_TEXT.
      GS_FIELDCAT-SCRTEXT_L = LV_TEXT.
      GS_FIELDCAT-SCRTEXT_M = LV_TEXT.
      GS_FIELDCAT-SCRTEXT_S = LV_TEXT.
    ENDIF.

    MODIFY GT_FIELDCAT2 FROM GS_FIELDCAT.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_KSTAR_RANGES
*&---------------------------------------------------------------------*
FORM SET_KSTAR_RANGES TABLES PR_KSTAR STRUCTURE R_KSTAR
                             USING P_FKAGRU P_TKAGRU.

  RANGES R_SETNAME FOR SETNODE-SETNAME.

  IF P_TKAGRU IS NOT INITIAL.

    MOVE: 'I'      TO R_SETNAME-SIGN,
          'BT'     TO R_SETNAME-OPTION,
          P_FKAGRU TO R_SETNAME-LOW,
          P_TKAGRU TO R_SETNAME-HIGH.

    APPEND R_SETNAME.

  ELSE.

    MOVE: 'I'      TO R_SETNAME-SIGN,
          'EQ'     TO R_SETNAME-OPTION,
          P_FKAGRU TO R_SETNAME-LOW.

    APPEND R_SETNAME.

  ENDIF.

  SELECT * FROM SETHEADER
    INTO TABLE @DATA(LT_SETHEADER)
   WHERE SETCLASS = '0102'
     AND SUBCLASS = @PA_KOKRS
     AND SETNAME  IN @R_SETNAME.

  LOOP AT LT_SETHEADER INTO DATA(LS_SETHEADER).

    PERFORM HIERARCHY_READ USING LS_SETHEADER-SETNAME.

    LOOP  AT GT_VALUES.

      IF GT_VALUES-VFROM = GT_VALUES-VTO.

        MOVE: GT_VALUES-VFROM   TO PR_KSTAR-LOW,
              'I'               TO PR_KSTAR-SIGN,
              'EQ'              TO PR_KSTAR-OPTION.
      ELSE.

        MOVE: GT_VALUES-VFROM   TO PR_KSTAR-LOW,
              GT_VALUES-VTO     TO PR_KSTAR-HIGH,
              'I'               TO PR_KSTAR-SIGN,
              'BT'              TO PR_KSTAR-OPTION.
      ENDIF.

      COLLECT PR_KSTAR.
      CLEAR   PR_KSTAR.

    ENDLOOP.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_COMMENT
*&---------------------------------------------------------------------*
FORM BUILD_COMMENT USING
      PT_LIST_COMMENTARY TYPE SLIS_T_LISTHEADER
      P_LOGO             TYPE SDYDO_VALUE.

  DATA: LS_LINE TYPE SLIS_LISTHEADER.


  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = TEXT-T02.

  DATA(LV_GJAHR) = PA_GJAHR && TEXT-T04.
  DATA(LV_SMON)  = PA_SPERL+1(2) && TEXT-T05.
  DATA(LV_EMON)  = PA_EPERL+1(2) && TEXT-T05.

  CONCATENATE LV_GJAHR LV_SMON '~' LV_EMON
         INTO LS_LINE-INFO SEPARATED BY SPACE.

  APPEND LS_LINE TO PT_LIST_COMMENTARY.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = TEXT-T07.

  DATA(LV_VTXT) = '(' && PA_VTXT && ')'.

  CONCATENATE PA_VERSN LV_VTXT
         INTO LS_LINE-INFO SEPARATED BY SPACE.

  APPEND LS_LINE TO PT_LIST_COMMENTARY.

  LS_LINE-TYP  = 'A'.
  LS_LINE-INFO = TEXT-T03.
  APPEND LS_LINE TO PT_LIST_COMMENTARY.

*  P_LOGO = 'ENJOYSAP_LOGO'.
  P_LOGO = 'TSKLOGO'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_HIERARCHY_HEADER_0100_01
*&---------------------------------------------------------------------*
FORM BUILD_HIERARCHY_HEADER_0100_01 .

  GS_HIERARCHY_HEADER2-HEADING = TEXT-H02.
  GS_HIERARCHY_HEADER2-WIDTH = 30.
  GS_HIERARCHY_HEADER2-WIDTH_PIX = ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV_TREE_0100_01
*&---------------------------------------------------------------------*
FORM DISPLAY_ALV_TREE_0100_01 .

* create info-table for html-header
  DATA: LT_LIST_COMMENTARY TYPE SLIS_T_LISTHEADER,
        L_LOGO             TYPE SDYDO_VALUE.

  PERFORM BUILD_COMMENT USING
                 LT_LIST_COMMENTARY
                 L_LOGO.

  CALL METHOD GR_TREE_DATA->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_HIERARCHY_HEADER  = GS_HIERARCHY_HEADER2
      IT_TOOLBAR_EXCLUDING = GT_EXCLUDE[]
      IT_LIST_COMMENTARY   = LT_LIST_COMMENTARY
      I_LOGO               = L_LOGO
    CHANGING
      IT_OUTTAB            = GT_OUTTAB_DISPLAY[]
      IT_FIELDCATALOG      = GT_FIELDCAT2[].

  PERFORM BUILD_HIERARCHY2.

* calculate totals
  CALL METHOD GR_TREE_DATA->UPDATE_CALCULATIONS.

* this method must be called to send the data to the frontend
  CALL METHOD GR_TREE_DATA->FRONTEND_UPDATE.

  CALL METHOD CL_GUI_CFW=>FLUSH
    EXCEPTIONS
      CNTL_SYSTEM_ERROR = 1
      CNTL_ERROR        = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_HIERARCHY2
*&---------------------------------------------------------------------*
FORM BUILD_HIERARCHY2 .

  DATA: LV_KEY1 TYPE LVC_NKEY,
        LV_KEY2 TYPE LVC_NKEY,
        LV_KEY3 TYPE LVC_NKEY,
        LV_KEY4 TYPE LVC_NKEY,
        LV_KEY5 TYPE LVC_NKEY.

  DATA LS_OUTTAB TYPE ZCOS0110.

  DATA: BEGIN OF LT_KEY OCCURS 0,
          LV_KEY TYPE LVC_NKEY,
        END OF LT_KEY.

  CLEAR: GT_TOPNODES, GT_TOPNODES[].

  DATA LT_OUTTAB LIKE TABLE OF GS_OUTTAB WITH HEADER LINE.

  DATA LV_SETID TYPE SETID.

  LOOP AT GT_OUTTAB INTO GS_OUTTAB.

    CASE GS_OUTTAB-TYPE.

      WHEN 'H'.

        PERFORM ADD_NODEKEY2     USING   LV_KEY1
                                         GR_TREE_DATA
                                         GS_OUTTAB
                                         'ZCODE'
                                CHANGING LV_KEY2.

        MOVE LV_KEY2 TO GS_TOPNODES-NODEKEY.
        APPEND GS_TOPNODES TO GT_TOPNODES.
        CLEAR GS_TOPNODES.

      WHEN 'R'.

        MOVE GS_OUTTAB-SHORTNAME TO GS_OUTTAB-DESCR.

        PERFORM ADD_NODEKEY2     USING   LV_KEY2
                                         GR_TREE_DATA
                                         GS_OUTTAB
                                         'SETID'
                                CHANGING LV_KEY3.

    ENDCASE.

    MODIFY GT_OUTTAB FROM GS_OUTTAB.

  ENDLOOP.

  SORT GT_TOPNODES DESCENDING.

  LOOP AT GT_TOPNODES INTO GS_TOPNODES.
    CALL METHOD GR_TREE_DATA->EXPAND_NODE
      EXPORTING
        I_NODE_KEY = GS_TOPNODES-NODEKEY.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TREE_DATA
*&---------------------------------------------------------------------*
FORM SET_TREE_DATA TABLES PT_OBJNR STRUCTURE FTR_S_OBJNR.

  DATA LV_FIELDNAME  TYPE FIELDNAME.
  DATA LV_FIELDNAME2 TYPE FIELDNAME.

  DATA: RESULT_TAB TYPE MATCH_RESULT_TAB.

  DATA LV_OPERAND TYPE STRING.

  DATA LS_OUTTAB TYPE ZCOS0260.

  CLEAR GT_OUTTAB[].
  CLEAR GS_OUTTAB.

  FIELD-SYMBOLS: <FS_HSL>   TYPE ANY,
                 <FS_SUM>   TYPE ANY,
                 <FS_ALL>   TYPE ANY,
                 <FS_WKG>   TYPE ANY,
                 <FS_VALUE> TYPE ANY.

  FIELD-SYMBOLS: <FS>  TYPE ANY.
  FIELD-SYMBOLS: <FS1> TYPE ANY.

  DATA: LV_RES    TYPE I,
        LV_OPE    TYPE C,
        LV_OFF    TYPE I,
        LV_LENGTH TYPE I.

  DATA LV_INDEX TYPE SYINDEX.
  DATA LV_LINES TYPE SYTFILL.

  DATA LV_ZCODE TYPE ZCODE.

  RANGES R_SETNAME FOR SETNODE-SETNAME.

  LOOP AT GT_ZCOT0150.

    MOVE: GT_ZCOT0150-ZCODE TO GS_OUTTAB-ZCODE,
          GT_ZCOT0150-DESCR TO GS_OUTTAB-SETTEXT,
          GT_ZCOT0150-GUBUN TO GS_OUTTAB-GUBUN,
          GV_WAERS          TO GS_OUTTAB-WAERS,
          'H'               TO GS_OUTTAB-TYPE.

    COLLECT GS_OUTTAB INTO GT_OUTTAB.

    CASE GT_ZCOT0150-GUBUN.

      WHEN '2'.

        CLEAR: R_SETNAME, R_SETNAME[].

        IF GT_ZCOT0150-TKAGRU IS NOT INITIAL.

          MOVE: 'I'                TO R_SETNAME-SIGN,
                'BT'               TO R_SETNAME-OPTION,
                GT_ZCOT0150-FKAGRU TO R_SETNAME-LOW,
                GT_ZCOT0150-TKAGRU TO R_SETNAME-HIGH.

          APPEND R_SETNAME.

        ELSE.

          MOVE: 'I'      TO R_SETNAME-SIGN,
                'EQ'     TO R_SETNAME-OPTION,
               GT_ZCOT0150-FKAGRU TO R_SETNAME-LOW.

          APPEND R_SETNAME.

        ENDIF.

        SELECT * FROM SETHEADER
          INTO TABLE @DATA(LT_SETHEADER)
         WHERE SETCLASS = '0102'
           AND SUBCLASS = @PA_KOKRS
           AND SETNAME  IN @R_SETNAME.

        IF SY-SUBRC = 0 .

          LOOP AT LT_SETHEADER INTO DATA(LS_SETHEADER).

            PERFORM HIERARCHY_READ USING LS_SETHEADER-SETNAME.

            LOOP AT GT_NODES.

              MOVE: GT_NODES-SHORTNAME TO GS_OUTTAB-SETID,
                    GT_NODES-DESCRIPT  TO GS_OUTTAB-SHORTNAME,
                    GT_NODES-TYPE      TO GS_OUTTAB-TYPE,
                    GT_NODES-HLEVEL    TO GS_OUTTAB-HLEVEL.

*-- 상위노드 GET
              CASE GT_NODES-HLEVEL.

                WHEN 0.
                  MOVE: 'R' TO GS_OUTTAB-TYPE.
                  MOVE: GS_OUTTAB-SETID TO GS_OUTTAB-SUBNAME.

                  CLEAR: GS_OUTTAB-KSTAR, GS_OUTTAB-KTEXT,
                         GS_OUTTAB-DESCR.

                  COLLECT GS_OUTTAB INTO GT_OUTTAB.

                  READ TABLE GT_VALUES WITH KEY SETID = GT_NODES-SETID.  "계정 X
                  IF SY-SUBRC <> 0.
                    CONTINUE.
                  ENDIF.

                WHEN OTHERS.

                  READ TABLE GT_SETNODE
                     WITH KEY SUBSETNAME = GS_OUTTAB-SETID.

                  IF SY-SUBRC = 0.
                    MOVE: GT_SETNODE-SETNAME TO GS_OUTTAB-SUBNAME.
                  ELSE.
                    MOVE: GS_OUTTAB-SETID TO GS_OUTTAB-SUBNAME.
                  ENDIF.

                  READ TABLE GT_VALUES WITH KEY SETID = GT_NODES-SETID.  "계정 X

                  IF SY-SUBRC <> 0.

                    CLEAR: GS_OUTTAB-KSTAR,  GS_OUTTAB-KTEXT,
                           GS_OUTTAB-HSL01,  GS_OUTTAB-HSL02,
                           GS_OUTTAB-HSL03,  GS_OUTTAB-HSL04,
                           GS_OUTTAB-HSL05,  GS_OUTTAB-HSL06,
                           GS_OUTTAB-HSL07,  GS_OUTTAB-HSL08,
                           GS_OUTTAB-HSL09,  GS_OUTTAB-HSL10,
                           GS_OUTTAB-HSL11,  GS_OUTTAB-HSL12,
                           GS_OUTTAB-HSLSUM, GS_OUTTAB-DESCR.

                    COLLECT GS_OUTTAB INTO GT_OUTTAB.
                    CONTINUE.

                  ENDIF.

              ENDCASE.

              MOVE GT_NODES-TYPE TO GS_OUTTAB-TYPE.

*-- 그룹에 등록되어 있는 계정만큼

              LOOP AT GT_VALUES WHERE SETID = GT_NODES-SETID.

                LOOP AT GT_CSKA WHERE KSTAR >= GT_VALUES-VFROM
                                   AND KSTAR <= GT_VALUES-VTO.

                  MOVE: GT_CSKA-KSTAR TO GS_OUTTAB-KSTAR,
                        GT_CSKA-KTEXT TO GS_OUTTAB-KTEXT,
                        GT_CSKA-KTEXT TO GS_OUTTAB-DESCR.

                  COLLECT GS_OUTTAB INTO GT_OUTTAB.

                  LOOP AT GT_DATA ASSIGNING FIELD-SYMBOL(<LS_DATA>)
                                   WHERE KSTAR = GS_OUTTAB-KSTAR
                                     AND OBJNR IN PT_OBJNR.

                    LOOP AT GT_MONTH.

                      LV_FIELDNAME = '<LS_DATA>-WKG0' && GT_MONTH-V1.
                      LV_FIELDNAME2 = 'HSL' && GT_MONTH-V1.

                      ASSIGN (LV_FIELDNAME) TO <FS_WKG>.

                      ASSIGN COMPONENT LV_FIELDNAME2
                       OF STRUCTURE GS_OUTTAB TO <FS_HSL>.

                      MOVE <FS_WKG> TO <FS_HSL>.

                    ENDLOOP.

                    IF GS_OUTTAB-KSTAR CP '04*' OR  "매출 계정은 음수로 표시
                        GS_OUTTAB-KSTAR CP '0701*' OR
                        GS_OUTTAB-KSTAR CP '0703*' OR
                        GS_OUTTAB-KSTAR CP '0705*'.

                      GS_OUTTAB-HSL01 =  GS_OUTTAB-HSL01 * '-1'.
                      GS_OUTTAB-HSL02 =  GS_OUTTAB-HSL02 * '-1'.
                      GS_OUTTAB-HSL03 =  GS_OUTTAB-HSL03 * '-1'.
                      GS_OUTTAB-HSL04 =  GS_OUTTAB-HSL04 * '-1'.
                      GS_OUTTAB-HSL05 =  GS_OUTTAB-HSL05 * '-1'.
                      GS_OUTTAB-HSL06 =  GS_OUTTAB-HSL06 * '-1'.
                      GS_OUTTAB-HSL07 =  GS_OUTTAB-HSL07 * '-1'.
                      GS_OUTTAB-HSL08 =  GS_OUTTAB-HSL08 * '-1'.
                      GS_OUTTAB-HSL09 =  GS_OUTTAB-HSL09 * '-1'.
                      GS_OUTTAB-HSL10 =  GS_OUTTAB-HSL10 * '-1'.
                      GS_OUTTAB-HSL11 =  GS_OUTTAB-HSL11 * '-1'.
                      GS_OUTTAB-HSL12 =  GS_OUTTAB-HSL12 * '-1'.
                    ENDIF.

                    GS_OUTTAB-HSLSUM = GS_OUTTAB-HSL01 + GS_OUTTAB-HSL02 +
                                       GS_OUTTAB-HSL03 + GS_OUTTAB-HSL04 +
                                       GS_OUTTAB-HSL05 + GS_OUTTAB-HSL06 +
                                       GS_OUTTAB-HSL07 + GS_OUTTAB-HSL08 +
                                       GS_OUTTAB-HSL09 + GS_OUTTAB-HSL10 +
                                       GS_OUTTAB-HSL11 + GS_OUTTAB-HSL12.

                    COLLECT GS_OUTTAB INTO GT_OUTTAB.

                    MOVE GS_OUTTAB TO LS_OUTTAB.
                    MOVE 'H'       TO LS_OUTTAB-TYPE.

                    CLEAR: LS_OUTTAB-SETID,   LS_OUTTAB-SHORTNAME,
                           LS_OUTTAB-KSTAR,   LS_OUTTAB-KTEXT,
                           LS_OUTTAB-SUBNAME, LS_OUTTAB-HLEVEL,
                           LS_OUTTAB-DESCR.

                    COLLECT LS_OUTTAB INTO GT_OUTTAB.

                    CLEAR: GS_OUTTAB-HSL01,  GS_OUTTAB-HSL02,
                           GS_OUTTAB-HSL03,  GS_OUTTAB-HSL04,
                           GS_OUTTAB-HSL05,  GS_OUTTAB-HSL06,
                           GS_OUTTAB-HSL07,  GS_OUTTAB-HSL08,
                           GS_OUTTAB-HSL09,  GS_OUTTAB-HSL10,
                           GS_OUTTAB-HSL11,  GS_OUTTAB-HSL12,
                           GS_OUTTAB-HSLSUM.

                  ENDLOOP.

                ENDLOOP.

              ENDLOOP.

            ENDLOOP.

          ENDLOOP.

        ENDIF.

    ENDCASE.

    CLEAR GS_OUTTAB.

  ENDLOOP.

  LOOP AT GT_OUTTAB INTO GS_OUTTAB WHERE GUBUN = '1'.  "수식 계산

    CLEAR: LV_INDEX, LV_LINES.

    READ TABLE GT_ZCOT0150 WITH KEY ZCODE = GS_OUTTAB-ZCODE.

    IF SY-SUBRC = 0.

      REFRESH RESULT_TAB.

      FIND ALL OCCURRENCES OF REGEX '[\+\-]'
           IN GT_ZCOT0150-CALCU
           RESPECTING CASE
           RESULTS RESULT_TAB.

      IF SY-SUBRC = 0.

        LV_LINES = LINES( RESULT_TAB ).

        ADD 1 TO LV_LINES.

        DO LV_LINES TIMES.

          ADD 1 TO LV_INDEX.

          READ TABLE RESULT_TAB INTO DATA(LS_RESULT) INDEX LV_INDEX.

          IF LV_INDEX = 1.

            MOVE: GT_ZCOT0150-CALCU(4)   TO LV_ZCODE,
                  GT_ZCOT0150-CALCU+4(1) TO LV_OPE.

            LV_OFF = LS_RESULT-OFFSET + LS_RESULT-LENGTH.

            READ TABLE GT_OUTTAB INTO DATA(LS_OUTTAB1)
                                     WITH KEY ZCODE = LV_ZCODE
                                              SETID = SPACE.
            IF SY-SUBRC = 0.

              MOVE: LS_OUTTAB1-HSL01 TO GS_OUTTAB-HSL01,
                    LS_OUTTAB1-HSL02 TO GS_OUTTAB-HSL02,
                    LS_OUTTAB1-HSL03 TO GS_OUTTAB-HSL03,
                    LS_OUTTAB1-HSL04 TO GS_OUTTAB-HSL04,
                    LS_OUTTAB1-HSL05 TO GS_OUTTAB-HSL05,
                    LS_OUTTAB1-HSL06 TO GS_OUTTAB-HSL06,
                    LS_OUTTAB1-HSL07 TO GS_OUTTAB-HSL07,
                    LS_OUTTAB1-HSL08 TO GS_OUTTAB-HSL08,
                    LS_OUTTAB1-HSL09 TO GS_OUTTAB-HSL09,
                    LS_OUTTAB1-HSL10 TO GS_OUTTAB-HSL10,
                    LS_OUTTAB1-HSL11 TO GS_OUTTAB-HSL11,
                    LS_OUTTAB1-HSL12 TO GS_OUTTAB-HSL12.

            ENDIF.

          ELSE.

            MOVE GT_ZCOT0150-CALCU+LV_OFF(4) TO LV_ZCODE.

            LV_OFF = LS_RESULT-OFFSET + LS_RESULT-LENGTH.

            READ TABLE GT_OUTTAB INTO DATA(LS_OUTTAB2)
                                     WITH KEY ZCODE = LV_ZCODE
                                              SETID = SPACE.

            IF SY-SUBRC = 0.

              CASE LV_OPE.

                WHEN '+'.

                  GS_OUTTAB-HSL01 = GS_OUTTAB-HSL01 + LS_OUTTAB2-HSL01.
                  GS_OUTTAB-HSL02 = GS_OUTTAB-HSL02 + LS_OUTTAB2-HSL02.
                  GS_OUTTAB-HSL03 = GS_OUTTAB-HSL03 + LS_OUTTAB2-HSL03.
                  GS_OUTTAB-HSL04 = GS_OUTTAB-HSL04 + LS_OUTTAB2-HSL04.
                  GS_OUTTAB-HSL05 = GS_OUTTAB-HSL05 + LS_OUTTAB2-HSL05.
                  GS_OUTTAB-HSL06 = GS_OUTTAB-HSL06 + LS_OUTTAB2-HSL06.
                  GS_OUTTAB-HSL07 = GS_OUTTAB-HSL07 + LS_OUTTAB2-HSL07.
                  GS_OUTTAB-HSL08 = GS_OUTTAB-HSL08 + LS_OUTTAB2-HSL08.
                  GS_OUTTAB-HSL09 = GS_OUTTAB-HSL09 + LS_OUTTAB2-HSL09.
                  GS_OUTTAB-HSL10 = GS_OUTTAB-HSL10 + LS_OUTTAB2-HSL10.
                  GS_OUTTAB-HSL11 = GS_OUTTAB-HSL11 + LS_OUTTAB2-HSL11.
                  GS_OUTTAB-HSL12 = GS_OUTTAB-HSL12 + LS_OUTTAB2-HSL12.

                WHEN '-'.

                  GS_OUTTAB-HSL01 = GS_OUTTAB-HSL01 - LS_OUTTAB2-HSL01.
                  GS_OUTTAB-HSL02 = GS_OUTTAB-HSL02 - LS_OUTTAB2-HSL02.
                  GS_OUTTAB-HSL03 = GS_OUTTAB-HSL03 - LS_OUTTAB2-HSL03.
                  GS_OUTTAB-HSL04 = GS_OUTTAB-HSL04 - LS_OUTTAB2-HSL04.
                  GS_OUTTAB-HSL05 = GS_OUTTAB-HSL05 - LS_OUTTAB2-HSL05.
                  GS_OUTTAB-HSL06 = GS_OUTTAB-HSL06 - LS_OUTTAB2-HSL06.
                  GS_OUTTAB-HSL07 = GS_OUTTAB-HSL07 - LS_OUTTAB2-HSL07.
                  GS_OUTTAB-HSL08 = GS_OUTTAB-HSL08 - LS_OUTTAB2-HSL08.
                  GS_OUTTAB-HSL09 = GS_OUTTAB-HSL09 - LS_OUTTAB2-HSL09.
                  GS_OUTTAB-HSL10 = GS_OUTTAB-HSL10 - LS_OUTTAB2-HSL10.
                  GS_OUTTAB-HSL11 = GS_OUTTAB-HSL11 - LS_OUTTAB2-HSL11.
                  GS_OUTTAB-HSL12 = GS_OUTTAB-HSL12 - LS_OUTTAB2-HSL12.

              ENDCASE.

            ENDIF.

            GS_OUTTAB-HSLSUM = GS_OUTTAB-HSL01 + GS_OUTTAB-HSL02 +
                               GS_OUTTAB-HSL03 + GS_OUTTAB-HSL04 +
                               GS_OUTTAB-HSL05 + GS_OUTTAB-HSL06 +
                               GS_OUTTAB-HSL07 + GS_OUTTAB-HSL08 +
                               GS_OUTTAB-HSL09 + GS_OUTTAB-HSL10 +
                               GS_OUTTAB-HSL11 + GS_OUTTAB-HSL12.

            MOVE GT_ZCOT0150-CALCU+LS_RESULT-OFFSET(1) TO LV_OPE.

          ENDIF.

        ENDDO.

      ENDIF.

    ENDIF.

    MODIFY GT_OUTTAB FROM GS_OUTTAB.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form HIERARCHY_READ
*&---------------------------------------------------------------------*
FORM HIERARCHY_READ  USING   P_SETID TYPE SETNAMENEW.

  CLEAR: GT_NODES,  GT_NODES[],
         GT_VALUES, GT_VALUES[],
         GT_INFO,   GT_INFO[].

  DATA: LV_SETID     LIKE SETHIER-SETID,
        LV_OVERWRITE LIKE SY-DATAR.

  CALL FUNCTION 'G_SET_ENCRYPT_SETID'
    EXPORTING
      SETCLASS  = '0102'
      SHORTNAME = P_SETID
    IMPORTING
      SETID     = LV_SETID.

  IF SY-SUBRC = 0.

    CALL FUNCTION 'K_HIERARCHY_TABLES_READ'
      EXPORTING
        E_CLASS                     = '0102'
        E_SETID                     = LV_SETID
        E_KOKRS                     = PA_KOKRS
      TABLES
        T_NODES                     = GT_NODES
        T_VALUES                    = GT_VALUES
      CHANGING
        C_INFO                      = GT_INFO
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
*& Form ADD_NODEKEY
*&---------------------------------------------------------------------*
FORM ADD_NODEKEY2  USING   PV_RELAT_KEY TYPE LVC_NKEY
                           PR_TREE       TYPE REF TO CL_GUI_ALV_TREE
                           PS_OUTTAB     TYPE ZCOS0260
                           P_GUBUN
                  CHANGING PV_NODE_KEY  TYPE LVC_NKEY.

  DATA: LV_NODE_TEXT TYPE LVC_VALUE.

  DATA: LT_ITEM_LAYOUT TYPE LVC_T_LAYI,
        LS_ITEM_LAYOUT TYPE LVC_S_LAYI.

  DATA LS_DISPLAY TYPE ZCOS0260.
  MOVE PS_OUTTAB TO LS_DISPLAY.

  DATA LV_KEY TYPE LVC_NKEY.

  DATA LS_NODE_LAYOUT TYPE LVC_S_LAYN.

  CASE P_GUBUN.

    WHEN 'ZCODE'.
      CLEAR: LS_DISPLAY-SETID, LS_DISPLAY-SHORTNAME,
             LS_DISPLAY-KSTAR, LS_DISPLAY-KTEXT.

      LV_NODE_TEXT = LS_DISPLAY-SETTEXT.

      LS_NODE_LAYOUT-ISFOLDER  = 'X'.
      CALL METHOD PR_TREE->ADD_NODE
        EXPORTING
          I_RELAT_NODE_KEY = PV_RELAT_KEY
          I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
          I_NODE_TEXT      = LV_NODE_TEXT
          IS_OUTTAB_LINE   = LS_DISPLAY
          IS_NODE_LAYOUT   = LS_NODE_LAYOUT
          IT_ITEM_LAYOUT   = LT_ITEM_LAYOUT
        IMPORTING
          E_NEW_NODE_KEY   = PV_NODE_KEY.

    WHEN 'SETID'.

      CLEAR: LS_DISPLAY-KSTAR, LS_DISPLAY-KTEXT.

      LV_NODE_TEXT = LS_DISPLAY-SETID.
      LS_NODE_LAYOUT-ISFOLDER  = 'X'.

      CALL METHOD PR_TREE->ADD_NODE
        EXPORTING
          I_RELAT_NODE_KEY = PV_RELAT_KEY
          I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
          I_NODE_TEXT      = LV_NODE_TEXT
          IS_OUTTAB_LINE   = LS_DISPLAY
          IS_NODE_LAYOUT   = LS_NODE_LAYOUT
          IT_ITEM_LAYOUT   = LT_ITEM_LAYOUT
        IMPORTING
          E_NEW_NODE_KEY   = PV_NODE_KEY.

*      PERFORM ADD_NODEKEY3     USING   PV_NODE_KEY
*                                       GR_TREE_DATA
*                                       LS_DISPLAY-ZCODE
*                                       LS_DISPLAY-SETID
*                              CHANGING LV_KEY.

      PERFORM ADD_LINE USING LS_DISPLAY-SETID
                             LS_DISPLAY-ZCODE
                             PV_NODE_KEY.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_LINE
*&---------------------------------------------------------------------*
FORM ADD_LINE  USING   P_SETID
                       P_ZCODE
                       P_KEY TYPE LVC_NKEY.

  DATA LS_NODE_LAYOUT TYPE LVC_S_LAYN.
  DATA LV_NODE_TEXT  TYPE LVC_VALUE.
  DATA LV_KEY        TYPE LVC_NKEY.

  DATA LS_OUTTAB TYPE ZCOS0260.

  DATA LV_SETID TYPE SETID.

  LOOP AT GT_OUTTAB INTO DATA(LS_OUT)
                         WHERE ZCODE   = P_ZCODE
                           AND SUBNAME = P_SETID
                           AND  TYPE <> 'H' AND TYPE <> 'R'.

    MOVE LS_OUT TO LS_OUTTAB.

    IF LS_OUTTAB-SETID = LS_OUTTAB-SUBNAME.

      LV_NODE_TEXT = LS_OUTTAB-KSTAR.
      MOVE LS_OUTTAB-KTEXT TO LS_OUTTAB-DESCR.
      MODIFY GT_OUTTAB FROM LS_OUTTAB.

    ELSEIF LS_OUTTAB-SETID <> LS_OUTTAB-SUBNAME.

      IF LV_SETID = LS_OUTTAB-SETID.
        CONTINUE.
      ENDIF.

      LV_SETID = LS_OUTTAB-SETID.

      LV_NODE_TEXT = LS_OUTTAB-SETID.
      MOVE LS_OUTTAB-SHORTNAME TO LS_OUTTAB-DESCR.
      MODIFY GT_OUTTAB FROM LS_OUTTAB.

      CLEAR: LS_OUTTAB-KSTAR, LS_OUTTAB-KTEXT.

      LS_NODE_LAYOUT-ISFOLDER  = 'X'.

    ENDIF.

    CALL METHOD GR_TREE_DATA->ADD_NODE
      EXPORTING
        I_RELAT_NODE_KEY = P_KEY
        I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
        I_NODE_TEXT      = LV_NODE_TEXT
        IS_OUTTAB_LINE   = LS_OUTTAB
        IS_NODE_LAYOUT   = LS_NODE_LAYOUT
      IMPORTING
        E_NEW_NODE_KEY   = LV_KEY.

    IF LS_OUTTAB-SETID <> LS_OUTTAB-SUBNAME.

      PERFORM SET_KSTAR USING  LS_OUTTAB-SETID
                               LS_OUTTAB-SUBNAME
                               P_ZCODE
                               LV_KEY.

      READ TABLE GT_SETNODE WITH KEY SETNAME = LS_OUTTAB-SETID
        TRANSPORTING NO FIELDS.

      IF SY-SUBRC = 0 .
        PERFORM ADD_LINE USING LS_OUTTAB-SETID
                               LS_OUTTAB-ZCODE
                               LV_KEY.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_KSTAR
*&---------------------------------------------------------------------*
FORM SET_KSTAR  USING   P_SETID
                        P_SUBNAME
                        P_ZCODE
                        P_KEY TYPE LVC_NKEY.

  DATA LV_NODE_TEXT  TYPE LVC_VALUE.
  DATA LV_KEY        TYPE LVC_NKEY.

  LOOP AT GT_OUTTAB INTO DATA(LS_TEMP)
                      WHERE ZCODE = P_ZCODE
                        AND SETID = P_SETID
                        AND SUBNAME <> P_SETID
                        AND KSTAR IS NOT INITIAL
                        AND TYPE <> 'H' AND TYPE <> 'R'.

    LV_NODE_TEXT = LS_TEMP-KSTAR.
    MOVE LS_TEMP-KTEXT TO LS_TEMP-DESCR.
    MODIFY GT_OUTTAB FROM LS_TEMP.

    CALL METHOD GR_TREE_DATA->ADD_NODE
      EXPORTING
        I_RELAT_NODE_KEY = P_KEY
        I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
        I_NODE_TEXT      = LV_NODE_TEXT
        IS_OUTTAB_LINE   = LS_TEMP
      IMPORTING
        E_NEW_NODE_KEY   = LV_KEY.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXCEL_DOWNLOAD
*&---------------------------------------------------------------------*
FORM EXCEL_DOWNLOAD .

  DATA: LI_DATA TYPE REF TO DATA.

  DATA: LV_XML          TYPE XSTRING,
        LV_GUI_TYPE     TYPE I VALUE 1,
        LV_DISPLAY_MODE TYPE I,
        LV_VERSION      TYPE STRING,
        LV_FLAVOUR      TYPE STRING.

  DATA T_SORT TYPE LVC_T_SORT.

  DATA T_FIELDCAT TYPE LVC_T_FCAT.

  DATA: LS_XML_CHOICE TYPE IF_SALV_BS_XML=>S_TYPE_XML_CHOICE.

  DATA: LT_XML_CHOICE TYPE IF_SALV_BS_XML=>T_TYPE_XML_CHOICE.

  DATA: LI_RESULT_DATA TYPE REF TO CL_SALV_EX_RESULT_DATA_TABLE,
        LI_CONTROLLER  TYPE REF TO CL_SALV_EXPORT_C8R.


  DATA: LV_DEFAULT_EXTENSION TYPE STRING,
        LV_INITIAL_DIRECTORY TYPE STRING,
        LV_DEFAULT_FILE_NAME TYPE STRING,
        LV_MASK              TYPE CHAR255,
        LV_MASK1             TYPE STRING,
        LV_APPLICATION       TYPE STRING.


  FIELD-SYMBOLS: <LT_DATA> TYPE STANDARD TABLE.

  CREATE DATA LI_DATA LIKE GT_OUTTAB.

  ASSIGN LI_DATA->* TO <LT_DATA>.

  <LT_DATA> = GT_OUTTAB[].

  CLEAR: T_FIELDCAT[].

  T_FIELDCAT[] =  GT_FIELDCAT2[].

  LOOP AT T_FIELDCAT INTO GS_FIELDCAT.

    CASE GS_FIELDCAT-FIELDNAME.

      WHEN 'HLEVEL' OR 'TYPE'.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.
      WHEN OTHERS.
        GS_FIELDCAT-NO_OUT = ABAP_FALSE.
        MODIFY T_FIELDCAT FROM GS_FIELDCAT.
    ENDCASE.

  ENDLOOP.

  CALL METHOD CL_SALV_EXPORT_XML_DIALOG=>EXECUTE
    EXPORTING
      GUI_TYPE     = LV_GUI_TYPE
      DISPLAY_MODE = LV_DISPLAY_MODE
    RECEIVING
      VALUE        = LT_XML_CHOICE.

  READ TABLE LT_XML_CHOICE INTO LS_XML_CHOICE INDEX 1.

  IF SY-SUBRC NE 0.
    MESSAGE 'FORMAT Not Choice!'
    TYPE 'S' RAISING FORMAT_NOT_CHOICE.
  ENDIF.

  CREATE OBJECT LI_CONTROLLER
    EXPORTING
      T_CHOICE = LT_XML_CHOICE.

  CASE CL_SALV_BS_A_XML_BASE=>GET_VERSION( ).
    WHEN IF_SALV_BS_XML=>VERSION_25.
      LV_VERSION = IF_SALV_BS_XML=>VERSION_25.
    WHEN IF_SALV_BS_XML=>VERSION_26.
      LV_VERSION = IF_SALV_BS_XML=>VERSION_26.
  ENDCASE.

  LV_FLAVOUR = IF_SALV_BS_C_TT=>C_TT_XML_FLAVOUR_EXPORT.

  CALL METHOD CL_SALV_EX_UTIL=>FACTORY_RESULT_DATA_TABLE
    EXPORTING
      R_DATA              = LI_DATA
*     S_LAYOUT            = IS_LAYOUT
      T_FIELDCATALOG      = T_FIELDCAT[]
      T_SORT              = T_SORT[]
    RECEIVING
      R_RESULT_DATA_TABLE = LI_RESULT_DATA.

  CALL METHOD CL_SALV_BS_TT_UTIL=>IF_SALV_BS_TT_UTIL~TRANSFORM
    EXPORTING
      XML_TYPE      = LS_XML_CHOICE-XML_TYPE
      XML_VERSION   = LV_VERSION
      R_RESULT_DATA = LI_RESULT_DATA
      XML_FLAVOUR   = LV_FLAVOUR
      GUI_TYPE      = IF_SALV_BS_XML=>C_GUI_TYPE_GUI
    IMPORTING
      XML           = LV_XML.


  CALL METHOD CL_ALV_BDS=>CREATE_MASK_FOR_FILEFILTER
    EXPORTING
      I_FRONTEND          = LS_XML_CHOICE-FRONTEND
    IMPORTING
      E_DEFAULT_EXTENSION = LV_DEFAULT_EXTENSION
    CHANGING
      C_MASK              = LV_MASK.

  LV_MASK1 = LV_MASK.

  IF LS_XML_CHOICE-FRONTEND EQ CL_ALV_BDS=>MC_MHTML_FRONTEND.
    LV_DEFAULT_EXTENSION = CL_ALV_BDS=>MC_EXCEL_EXTENSION.
  ENDIF.

  CONCATENATE 'export.' LV_DEFAULT_EXTENSION
        INTO LV_DEFAULT_FILE_NAME.

  DATA: LV_LENGTH      TYPE I,
        LV_FILENAME    TYPE STRING,
        LV_APPL_PARA   TYPE STRING,
        LV_XML_STREAM  TYPE ETXML_XLINE_TABTYPE,
        LV_TITLE       TYPE STRING,
        LV_LOC_FN      TYPE STRING,
        LV_LOC_DIR     TYPE STRING,
        LV_USER_ACTION TYPE I.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
    EXPORTING
      WINDOW_TITLE         = LV_TITLE
      DEFAULT_EXTENSION    = LV_DEFAULT_EXTENSION
      DEFAULT_FILE_NAME    = LV_DEFAULT_FILE_NAME
      FILE_FILTER          = LV_MASK1
      INITIAL_DIRECTORY    = LV_INITIAL_DIRECTORY
    CHANGING
      FILENAME             = LV_LOC_FN
      PATH                 = LV_LOC_DIR
      FULLPATH             = LV_LOC_DIR
      USER_ACTION          = LV_USER_ACTION
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.

  IF SY-SUBRC <> 0.
    MESSAGE E162(ALVHT).
    EXIT.
  ENDIF.

  IF LV_USER_ACTION = CL_GUI_FRONTEND_SERVICES=>ACTION_CANCEL .
    MESSAGE S161(ALVHT).
    EXIT.
  ENDIF.

  CONCATENATE LV_LOC_DIR LV_LOC_FN INTO LV_FILENAME.

  IF NOT LV_FILENAME IS INITIAL.
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        BUFFER        = LV_XML
      IMPORTING
        OUTPUT_LENGTH = LV_LENGTH
      TABLES
        BINARY_TAB    = LV_XML_STREAM.

    CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_DOWNLOAD
      EXPORTING
        BIN_FILESIZE = LV_LENGTH
        FILETYPE     = 'BIN'
        FILENAME     = LV_FILENAME
      CHANGING
        DATA_TAB     = LV_XML_STREAM
      EXCEPTIONS
        OTHERS       = 1.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REGIST_ALV_EVENT_0100_01
*&---------------------------------------------------------------------*
FORM REGIST_ALV_EVENT_0100_01 .

  DATA: LT_EVENTS TYPE CNTL_SIMPLE_EVENTS,
        LS_EVENT  TYPE CNTL_SIMPLE_EVENT.

* REGISTER EVENT
  CALL METHOD GR_TREE_DATA->GET_REGISTERED_EVENTS
    IMPORTING
      EVENTS = LT_EVENTS.

  LS_EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_NODE_DOUBLE_CLICK.
  APPEND LS_EVENT TO LT_EVENTS.

  LS_EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_ITEM_DOUBLE_CLICK.
  APPEND LS_EVENT TO LT_EVENTS.

  LS_EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_HEADER_CLICK.
  APPEND LS_EVENT TO LT_EVENTS.

  CALL METHOD GR_TREE_DATA->SET_REGISTERED_EVENTS
    EXPORTING
      EVENTS                    = LT_EVENTS
    EXCEPTIONS
      CNTL_ERROR                = 1
      CNTL_SYSTEM_ERROR         = 2
      ILLEGAL_EVENT_COMBINATION = 3.

  IF SY-SUBRC <> 0.
    MESSAGE X000 WITH 'ERROR'.
  ENDIF.

*-- GR_EVENT_RECEIVER
  IF GR_TREE_EVENT_RECEIVER IS INITIAL.
    CREATE OBJECT GR_TREE_EVENT_RECEIVER.
  ENDIF.

* Handler Event
  SET HANDLER:
    GR_TREE_EVENT_RECEIVER->HANDLE_NODE_DOUBLE_CLICK
        FOR GR_TREE_DATA,
    GR_TREE_EVENT_RECEIVER->HANDLE_ITEM_DOUBLE_CLICK
        FOR GR_TREE_DATA,
    GR_TREE_EVENT_RECEIVER->HANDLE_HEADER_CLICK
        FOR GR_TREE_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FREE_DATA_TREE
*&---------------------------------------------------------------------*
FORM FREE_DATA_TREE .

  CALL METHOD GR_TREE_DATA->FREE.
  CLEAR GR_TREE_DATA.

  CREATE OBJECT GR_TREE_DATA
    EXPORTING
      PARENT                      = GR_DATA_CONTAINER
      NODE_SELECTION_MODE         = 0
      ITEM_SELECTION              = ABAP_TRUE
      NO_HTML_HEADER              = ABAP_FALSE
      NO_TOOLBAR                  = SPACE
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      ILLEGAL_NODE_SELECTION_MODE = 5
      FAILED                      = 6
      ILLEGAL_COLUMN_NAME         = 7.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_NODEKEY3
*&---------------------------------------------------------------------*
FORM ADD_NODEKEY3  USING   PV_RELAT_KEY TYPE LVC_NKEY
                           PR_TREE       TYPE REF TO CL_GUI_ALV_TREE
                           P_CODE
                           P_ROOT
                  CHANGING PV_NODE_KEY  TYPE LVC_NKEY.

  DATA: LV_NODE_TEXT TYPE LVC_VALUE.

  DATA: LT_ITEM_LAYOUT TYPE LVC_T_LAYI,
        LS_ITEM_LAYOUT TYPE LVC_S_LAYI.

  DATA LV_KEY  TYPE LVC_NKEY.
  DATA LV_KEY2 TYPE LVC_NKEY.
  DATA LS_OUTTAB TYPE ZCOS0260.

  DATA LS_VALUE TYPE ZCOS0260.

  DATA LS_NODE_LAYOUT TYPE LVC_S_LAYN.

  LOOP AT  GT_OUTTAB INTO LS_OUTTAB
                 WHERE ZCODE       = P_CODE
                   AND SUBNAME = P_ROOT.

    MOVE LS_OUTTAB TO LS_VALUE.

    CASE LS_OUTTAB-TYPE.

      WHEN 'B'.
        LV_NODE_TEXT =  LS_OUTTAB-KSTAR.

      WHEN 'S'.
        LV_NODE_TEXT =  LS_OUTTAB-SHORTNAME.
        CLEAR: LS_VALUE-KSTAR, LS_VALUE-KTEXT.

    ENDCASE.

    CALL METHOD PR_TREE->ADD_NODE
      EXPORTING
        I_RELAT_NODE_KEY = PV_RELAT_KEY
        I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
        I_NODE_TEXT      = LV_NODE_TEXT
        IS_OUTTAB_LINE   = LS_VALUE
        IS_NODE_LAYOUT   = LS_NODE_LAYOUT
        IT_ITEM_LAYOUT   = LT_ITEM_LAYOUT
      IMPORTING
        E_NEW_NODE_KEY   = PV_NODE_KEY.

  ENDLOOP.

  PERFORM ADD_NODEKEY3     USING   PV_NODE_KEY
                                   GR_TREE_DATA
                                   LS_OUTTAB-ZCODE
                                   LS_OUTTAB-SETID
                          CHANGING LV_KEY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM AUTHORITY_CHECK .

  DATA LV_TYPE TYPE C.
  DATA LV_MESSAGE TYPE BAPI_MSG.

  DATA: LT_0070  LIKE TABLE OF ZCAS0070,
        LS_0070  LIKE ZCAS0070,
        LV_CLASS TYPE ZCAT0031-CD_CLASS,
        LV_CODE  TYPE ZCAT0031-CD_CODE.

  CLEAR: R_PRCTR1,  R_PRCTR1[],
         R_BUKRS,   R_BUKRS[],
         R_PSPID,   R_PSPID[].

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

  IF SO_BUKRS[] IS INITIAL AND
     SO_PRCTR[] IS INITIAL AND
     SO_PSPID[] IS INITIAL AND
     ( SO_POSID[] IS INITIAL AND PA_PDGR IS INITIAL ).

    MESSAGE S000 WITH TEXT-E12 DISPLAY LIKE 'E'.
    STOP.

  ENDIF.

  CALL FUNCTION 'ZCA_PRG_AUTH_CHECK'
    EXPORTING
      I_MODULE     = 'CO'
      I_POSIDGR_CO = PA_PDGR
    IMPORTING
      E_TYPE       = LV_TYPE
      E_MESSAGE    = LV_MESSAGE
    TABLES
      IT_PRCTR_CO  = SO_PRCTR[]
      IT_BUKRS_CO  = SO_BUKRS[]
      IT_PSPID_CO  = SO_PSPID[]
      IT_POSID_CO  = SO_POSID[].

  IF LV_TYPE = 'E'.
    MESSAGE S000 WITH LV_MESSAGE DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  SELECT * FROM ZCOT0320
    INTO TABLE @DATA(LT_ZCOT0320)
    WHERE BNAME = @SY-UNAME.

  LOOP AT LT_ZCOT0320 INTO DATA(LS_ZCOT0320).

    IF LS_ZCOT0320-PRCTR1 IS NOT INITIAL.

      MOVE: LS_ZCOT0320-PRCTR1 TO R_PRCTR1-LOW,
            'I'                TO R_PRCTR1-SIGN,
            'EQ'               TO R_PRCTR1-OPTION.

      COLLECT R_PRCTR1.
      CLEAR   R_PRCTR1.

    ENDIF.

    IF LS_ZCOT0320-BUKRS IS NOT INITIAL.

      MOVE: LS_ZCOT0320-BUKRS TO R_BUKRS-LOW,
            'I'                TO R_BUKRS-SIGN,
            'EQ'               TO R_BUKRS-OPTION.

      COLLECT R_BUKRS.
      CLEAR   R_BUKRS.

    ENDIF.

    IF LS_ZCOT0320-PSPID IS NOT INITIAL.

      MOVE: LS_ZCOT0320-PSPID  TO R_PSPID-LOW,
            'I'                TO R_PSPID-SIGN,
            'EQ'               TO R_PSPID-OPTION.

      COLLECT R_PSPID.
      CLEAR   R_PSPID.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCRFIELDS_FUNCTXT
*&---------------------------------------------------------------------*
FORM SCRFIELDS_FUNCTXT .

  GS_FUNTXT-ICON_ID   = ICON_INFORMATION.
  GS_FUNTXT-QUICKINFO = 'Program Help'.

  SSCRFIELDS-FUNCTXT_01 = GS_FUNTXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCR_USER_COMMAND
*&---------------------------------------------------------------------*
FORM SCR_USER_COMMAND .

  CASE SSCRFIELDS-UCOMM.
    WHEN 'FC01'.
      PERFORM CALL_POPUP_HELP(ZCAR9000) USING SY-REPID
                                              SY-DYNNR
                                              SY-LANGU ''.

    WHEN OTHERS.

  ENDCASE.

ENDFORM.
