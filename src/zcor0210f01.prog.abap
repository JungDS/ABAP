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

*  SELECT SINGLE VTEXT INTO @PA_VTXT
*    FROM TKVS AS A
*    LEFT JOIN TKVST AS B
*      ON A~VERSI = B~VERSI
*     AND B~SPRAS = @SY-LANGU
*   WHERE A~VERSI = 'P0'.

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

  PERFORM SCRFIELDS_FUNCTXT.

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

*  SELECT SINGLE VTEXT INTO @PA_VTXT
*    FROM TKVS AS A
*    LEFT JOIN TKVST AS B
*      ON A~VERSI = B~VERSI
*     AND B~SPRAS = @SY-LANGU
*   WHERE A~VERSI = @PA_VERSN.

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

  SELECT A~VERSN, A~WRTTP, A~GJAHR, P~PRCTR, P~PBUKR AS BUKRS,
         A~OBJNR, A~KSTAR, B~KTEXT,
         SUM( A~WKG001 ) AS WKG001, SUM( A~WKG002 ) AS WKG002,
         SUM( A~WKG003 ) AS WKG003, SUM( A~WKG004 ) AS WKG004,
         SUM( A~WKG005 ) AS WKG005, SUM( A~WKG006 ) AS WKG006,
         SUM( A~WKG007 ) AS WKG007, SUM( A~WKG008 ) AS WKG008,
         SUM( A~WKG009 ) AS WKG009, SUM( A~WKG010 ) AS WKG010,
         SUM( A~WKG011 ) AS WKG011, SUM( A~WKG012 ) AS WKG012
    FROM COSP AS A
   INNER JOIN PRPS AS P
      ON A~OBJNR = P~OBJNR
    LEFT JOIN CSKU AS B
      ON A~KSTAR = B~KSTAR
     AND B~KTOPL = @GV_KTOPL
     AND B~SPRAS = @SY-LANGU
   WHERE A~LEDNR = '00'
     AND ( ( A~VERSN = '000'      AND A~WRTTP = '04' ) OR
           ( A~VERSN = @PA_VERSN  AND A~WRTTP = '01' ) OR
           ( A~VERSN = '000'      AND A~WRTTP = '01'  AND
             A~KSTAR = '0984000010' ) )
     AND A~GJAHR = @PA_GJAHR
     AND A~KSTAR IN @R_KSTAR
     AND A~OBJNR IN @R_OBJNR
     AND P~PBUKR IN @R_BUKRS
     AND P~PRCTR IN @SO_PRCTR
   GROUP BY A~VERSN, A~WRTTP, A~GJAHR, P~PRCTR, P~PBUKR,
            A~OBJNR, A~KSTAR, B~KTEXT
   UNION ALL
  SELECT A~VERSN, A~WRTTP, A~GJAHR, P~PRCTR,
         P~PBUKR AS BUKRS, A~OBJNR, A~KSTAR,  B~KTEXT,
         SUM( A~WKG001 ) AS WKG001, SUM( A~WKG002 ) AS WKG002,
         SUM( A~WKG003 ) AS WKG003, SUM( A~WKG004 ) AS WKG004,
         SUM( A~WKG005 ) AS WKG005, SUM( A~WKG006 ) AS WKG006,
         SUM( A~WKG007 ) AS WKG007, SUM( A~WKG008 ) AS WKG008,
         SUM( A~WKG009 ) AS WKG009, SUM( A~WKG010 ) AS WKG010,
         SUM( A~WKG011 ) AS WKG011, SUM( A~WKG012 ) AS WKG012
    FROM COSS AS A
   INNER JOIN PRPS AS P
      ON A~OBJNR = P~OBJNR
    LEFT JOIN CSKU AS B
      ON A~KSTAR = B~KSTAR
     AND B~KTOPL = @GV_KTOPL
     AND B~SPRAS = @SY-LANGU
   WHERE A~LEDNR = '00'
     AND ( ( A~VERSN = '000'      AND A~WRTTP = '04' ) OR
           ( A~VERSN = @PA_VERSN  AND A~WRTTP = '01' ) OR
           ( A~VERSN = '000'      AND A~WRTTP = '01'  AND
             A~KSTAR = '0984000010' ) )
     AND A~GJAHR = @PA_GJAHR
     AND A~KSTAR IN @R_KSTAR
     AND A~OBJNR IN @R_OBJNR
     AND P~PBUKR IN @R_BUKRS
     AND P~PRCTR IN @SO_PRCTR
   GROUP BY A~VERSN, A~WRTTP, A~GJAHR, P~PRCTR, P~PBUKR,
            A~OBJNR, A~KSTAR, B~KTEXT

    UNION ALL
  SELECT A~RVERS  AS VERSN, A~WRTTP, A~RYEAR AS GJAHR,
         P~PRCTR, P~PBUKR AS BUKRS,
         A~ROBJNR AS OBJNR, A~RKSTAR AS KSTAR, B~KTEXT,
        SUM( A~HSL01 ) AS WKG001, SUM( A~HSL02 ) AS WKG002,
        SUM( A~HSL03 ) AS WKG003, SUM( A~HSL04 ) AS WKG004,
        SUM( A~HSL05 ) AS WKG005, SUM( A~HSL06 ) AS WKG006,
        SUM( A~HSL07 ) AS WKG007, SUM( A~HSL08 ) AS WKG008,
        SUM( A~HSL09 ) AS WKG009, SUM( A~HSL10 ) AS WKG010,
        SUM( A~HSL11 ) AS WKG011, SUM( A~HSL12 ) AS WKG012
    FROM ZCOT1180 AS A
   INNER JOIN PRPS AS P
      ON A~ROBJNR = P~OBJNR
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
     AND P~PBUKR  IN @R_BUKRS
     AND P~PRCTR  IN @SO_PRCTR
   GROUP BY A~RVERS, A~WRTTP, A~RYEAR, P~PRCTR, P~PBUKR,
            A~ROBJNR, A~RKSTAR, B~KTEXT

   UNION ALL
  SELECT A~RVERS AS VERSN, A~WRTTP, A~RYEAR AS GJAHR, P~PRCTR,
         A~BUKRS, A~OBJNR,  A~SAKNR AS KSTAR, B~TXT20 AS KTEXT,
        SUM( A~HSL01 ) AS WKG001, SUM( A~HSL02 ) AS WKG002,
        SUM( A~HSL03 ) AS WKG003, SUM( A~HSL04 ) AS WKG004,
        SUM( A~HSL05 ) AS WKG005, SUM( A~HSL06 ) AS WKG006,
        SUM( A~HSL07 ) AS WKG007, SUM( A~HSL08 ) AS WKG008,
        SUM( A~HSL09 ) AS WKG009, SUM( A~HSL10 ) AS WKG010,
        SUM( A~HSL11 ) AS WKG011, SUM( A~HSL12 ) AS WKG012
    FROM ZFIT0621 AS A        "관계사간 거래 계획
   INNER JOIN PRPS AS P
      ON A~OBJNR = P~OBJNR
    LEFT JOIN SKAT AS B
      ON A~SAKNR  = B~SAKNR
     AND B~KTOPL  = @GV_KTOPL
     AND B~SPRAS  = @SY-LANGU
   WHERE A~RVERS  = @PA_VERSN
     AND A~WRTTP  = '01'
     AND A~KOKRS  = @PA_KOKRS
     AND A~RYEAR  = @PA_GJAHR
     AND A~SAKNR  IN @R_KSTAR
     AND A~OBJNR  IN @R_OBJNR
     AND A~BUKRS  IN @R_BUKRS
     AND P~PRCTR  IN @SO_PRCTR
   GROUP BY A~RVERS, A~WRTTP, A~RYEAR, P~PRCTR, A~BUKRS,
            A~OBJNR, A~SAKNR, B~TXT20

    INTO TABLE @GT_DATA.

*-- 관계사간 거래 ADD
  SELECT A~SPMON, A~SAKNR, A~TXT20, B~PRCTR,
         B~PBUKR AS BUKRS, B~OBJNR,
         SUM( A~DMBTR ) AS DMBTR
    INTO TABLE @DATA(LT_ZFIT0620)
    FROM ZFIT0620 AS A
    LEFT JOIN PRPS AS B
      ON A~POSID = B~POSID
   WHERE A~POSID IN @R_POSID2
     AND A~SPMON IN @R_SPMON
   GROUP BY A~SPMON, A~SAKNR, A~TXT20,
            B~PRCTR, B~PBUKR, B~OBJNR.

  LOOP AT LT_ZFIT0620 INTO DATA(LS_ZFIT0620).

    MOVE: '000'              TO GT_DATA-VERSN,
          '04'               TO GT_DATA-WRTTP,
          PA_GJAHR           TO GT_DATA-GJAHR,
          LS_ZFIT0620-OBJNR  TO GT_DATA-OBJNR,
          LS_ZFIT0620-SAKNR  TO GT_DATA-KSTAR,
          LS_ZFIT0620-TXT20  TO GT_DATA-KTEXT,
          LS_ZFIT0620-BUKRS  TO GT_DATA-BUKRS,
          LS_ZFIT0620-PRCTR  TO GT_DATA-PRCTR.

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

    CLEAR: LT_DATA[].

    LOOP AT GT_DATA WHERE OBJNR = GT_PRPS-OBJNR.
      MOVE GT_DATA TO LT_DATA.
      CLEAR: LT_DATA-VERSN, LT_DATA-WRTTP,
             LT_DATA-GJAHR, LT_DATA-KSTAR,
             LT_DATA-KTEXT, LT_DATA-BUKRS,
             LT_DATA-PRCTR.
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

  PERFORM MAKE_TABLE.
  PERFORM SET_TREE_DATA.

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

  CLEAR: R_OBJNR,   R_OBJNR[],
         R_KSTAR,   R_KSTAR[],
         R_BUKRS,   R_BUKRS[],
         R_SPMON,   R_SPMON[],
         R_POSID2,  R_POSID2[].

  CLEAR: GT_BASE, GT_BASE[].

  RANGES R_POSID FOR PRPS-POSID.

  PERFORM AUTHORITY_CHECK.

  SELECT PRCTR, BUKRS
    FROM ZCOT1150
    INTO TABLE @GT_BASE
   WHERE PRCTR IN @SO_PRCTR
     AND PRCTR IN @R_PRCTR1
     AND BUKRS IN @R_BUKRS2
   GROUP BY PRCTR, BUKRS
   ORDER BY PRCTR, BUKRS.

  IF SY-SUBRC <> 0.
    MESSAGE S000 WITH TEXT-E01 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  LOOP AT GT_BASE.

    MOVE: 'I'           TO R_BUKRS-SIGN,
          'EQ'          TO R_BUKRS-OPTION,
          GT_BASE-BUKRS TO R_BUKRS-LOW.

    COLLECT R_BUKRS.
    CLEAR   R_BUKRS.

  ENDLOOP.

*-- 손익센터 누락 건
  CLEAR GT_BASE.
  MOVE: 'PZZZ' TO GT_BASE-PRCTR.

  APPEND GT_BASE.

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

  SELECT A~*
    FROM PRPS AS A
   INNER JOIN PROJ AS B
      ON A~PSPHI = B~PSPNR
    INTO TABLE @GT_PRPS
   WHERE A~OBJNR IN @R_OBJNR
     AND A~PBUKR IN @R_BUKRS
     AND A~POSID IN @R_POSID
     AND B~PSPID IN @SO_PSPID
     AND A~PRCTR IN @SO_PRCTR
     AND A~LOEVM = @SPACE
     AND B~LOEVM = @SPACE
     AND A~PKOKR = @PA_KOKRS
     AND A~PBUKR IN @R_BUKRS2
     AND A~PRCTR IN @R_PRCTR1
     AND B~PSPID IN @R_PSPID.

  IF SY-SUBRC <> 0 .
    MESSAGE S004 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

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
*& Form CREATE_INSTANCE_0100
*&---------------------------------------------------------------------*
FORM CREATE_INSTANCE_0100 .

  CREATE OBJECT GR_TREE_DATA
    EXPORTING
      PARENT                      = CL_GUI_CONTAINER=>SCREEN0
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

  RANGES LR_BUKRS FOR T001-BUKRS.

  DATA:
    L_RSDS_SELOPT_T TYPE RSDS_SELOPT_T WITH HEADER LINE,
    L_RSDS_FRANGE_T TYPE RSDS_FRANGE_T WITH HEADER LINE,
    L_RSDS_RANGE    TYPE RSDS_RANGE,
    L_RSDS_TRANGE   TYPE RSDS_TRANGE,
    L_RSDS_TEXPR    TYPE RSDS_TEXPR,
    L_DATE_INIT     LIKE SY-DATLO.

  DATA LV_KOAGR TYPE KAGRU.

  DATA LS_POSID TYPE CURTO_PSPNR_RANGE.

  DATA LV_COND TYPE STRING.

  CASE SENDER.

    WHEN GR_TREE_DATA.

      IF P_FIELDNAME CP 'S_*'.

        IF P_FIELDNAME+6 IS INITIAL.

          CONCATENATE 'PRCTR' '=' 'P_FIELDNAME+2(4)'
                 INTO LV_COND
                 SEPARATED BY SPACE.

        ELSE.

          CONCATENATE 'PRCTR' '=' 'P_FIELDNAME+2(4)' 'AND'
                      'PBUKR' '=' 'P_FIELDNAME+6(4)'
                 INTO LV_COND
                 SEPARATED BY SPACE.

        ENDIF.

        LOOP AT GT_PRPS WHERE (LV_COND).
          MOVE: 'I'           TO LR_POSID-SIGN,
                'EQ'          TO LR_POSID-OPTION,
                GT_PRPS-POSID TO LR_POSID-LOW.

          COLLECT LR_POSID.
        ENDLOOP.

        READ TABLE <GT_TABLE_DISPLAY>
          ASSIGNING FIELD-SYMBOL(<LS_DISPLAY>) INDEX P_NODE_KEY.

        ASSIGN COMPONENT 'SETID' OF STRUCTURE <LS_DISPLAY>
           TO FIELD-SYMBOL(<LV_SETID>).

        ASSIGN COMPONENT 'KSTAR' OF STRUCTURE <LS_DISPLAY>
           TO FIELD-SYMBOL(<LV_KSTAR>).

        IF <LV_SETID> IS NOT INITIAL AND
           <LV_KSTAR> IS NOT INITIAL.

          CONCATENATE 'IEQ' <LV_KSTAR> INTO LR_KSTAR.
          APPEND LR_KSTAR.

        ELSEIF <LV_SETID> IS NOT INITIAL AND
                <LV_KSTAR> IS INITIAL.

          MOVE <LV_SETID> TO LV_KOAGR.

        ELSE.

          ASSIGN COMPONENT 'ZCODE' OF STRUCTURE <LS_DISPLAY>
             TO FIELD-SYMBOL(<LV_ZCODE>).

          READ TABLE GT_ZCOT0150 WITH KEY ZCODE = <LV_ZCODE>.

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

*        LOOP AT GT_BASE WHERE PRCTR = P_FIELDNAME+2(4).
*
*          MOVE: GT_BASE-BUKRS TO L_RSDS_SELOPT_T-LOW,
*                'I'           TO L_RSDS_SELOPT_T-SIGN,
*                'EQ'          TO L_RSDS_SELOPT_T-OPTION.
*
*          APPEND L_RSDS_SELOPT_T.
*
*        ENDLOOP.
*
*        MOVE:  'PBUKR'          TO L_RSDS_FRANGE_T-FIELDNAME,
*              L_RSDS_SELOPT_T[] TO L_RSDS_FRANGE_T-SELOPT_T[].
*
*        APPEND L_RSDS_FRANGE_T.
*
*        MOVE: 'PRPS_R'          TO L_RSDS_RANGE-TABLENAME,
*              L_RSDS_FRANGE_T[] TO L_RSDS_RANGE-FRANGE_T[].
*
*        APPEND L_RSDS_RANGE TO L_RSDS_TRANGE.
*
*        CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_EX'
*          EXPORTING
*            FIELD_RANGES = L_RSDS_TRANGE
*          IMPORTING
*            EXPRESSIONS  = L_RSDS_TEXPR.

*        SUBMIT RKPEP003  USING SELECTION-SCREEN '1000'
*             WITH FREE SELECTIONS L_RSDS_TEXPR
*             WITH CN_PSPNR    IN LR_POSID
*             WITH R_KSTAR     IN LR_KSTAR
*             WITH KOAGR       = LV_KOAGR
*             WITH R_BUDAT     IN LR_BUDAT
*             AND RETURN.

        SUBMIT RKPEP003  USING SELECTION-SCREEN '1000'
             WITH CN_PSPNR    IN LR_POSID
             WITH R_KSTAR     IN LR_KSTAR
             WITH KOAGR       = LV_KOAGR
             WITH R_BUDAT     IN LR_BUDAT
             AND RETURN.

      ENDIF.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_NODE_HANDLE_HEADER_CLICK
*&---------------------------------------------------------------------*
FORM EVENT_NODE_HANDLE_HEADER_CLICK  USING P_FIELDNAME TYPE LVC_FNAME
                                           SENDER.

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
*& Form APPEND_FIELDCAT_0100_01
*&---------------------------------------------------------------------*
FORM APPEND_FIELDCAT_0100_01 .

  PERFORM MODIFY_FIELDCATLOG_DATA2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_FIELDCATLOG_DATA2
*&---------------------------------------------------------------------*
FORM MODIFY_FIELDCATLOG_DATA2 .

  DATA:  LV_TEXT(50).

  LOOP AT GT_FIELDCAT INTO GS_FIELDCAT.

    CLEAR: LV_TEXT.

    IF GS_FIELDCAT-FIELDNAME CP 'P_*' OR
       GS_FIELDCAT-FIELDNAME CP 'S_*'.

      GS_FIELDCAT-DO_SUM = ABAP_TRUE.

    ELSEIF GS_FIELDCAT-FIELDNAME = 'DESCR'.
      GS_FIELDCAT-KEY = 'X'.

    ELSE.
      GS_FIELDCAT-NO_OUT = ABAP_TRUE.
    ENDIF.

    GS_FIELDCAT-OUTPUTLEN = 40.

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
  LS_LINE-KEY  = TEXT-T10.

  DATA(LV_TEXT) = '(' && PA_VTXT && ')'.
  CONCATENATE PA_VERSN LV_TEXT
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
      IT_OUTTAB            = <GT_TABLE_DISPLAY>
      IT_FIELDCATALOG      = GT_FIELDCAT[].

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

  FIELD-SYMBOLS: <FS_TYPE>   TYPE ANY,
                 <FS_VALUE>  TYPE ANY,
                 <FS_VALUE2> TYPE ANY.

  DATA: BEGIN OF LT_KEY OCCURS 0,
          LV_KEY TYPE LVC_NKEY,
        END OF LT_KEY.

  CLEAR: GT_TOPNODES, GT_TOPNODES[].

  DATA LV_SETID TYPE SETID.

  LOOP AT <GT_TABLE> ASSIGNING <GS_LINE>.

    ASSIGN COMPONENT 'TYPE'
        OF STRUCTURE <GS_LINE> TO <FS_TYPE>.

    CASE <FS_TYPE>.

      WHEN 'H'.

        PERFORM ADD_NODEKEY2     USING   LV_KEY1
                                         GR_TREE_DATA
                                         <GS_LINE>
                                         'ZCODE'
                                CHANGING LV_KEY2.

        MOVE LV_KEY2 TO GS_TOPNODES-NODEKEY.
        APPEND GS_TOPNODES TO GT_TOPNODES.
        CLEAR GS_TOPNODES.

      WHEN 'R'.

        ASSIGN COMPONENT 'SHORTNAME'
            OF STRUCTURE <GS_LINE> TO <FS_VALUE>.

        ASSIGN COMPONENT 'DESCR'
            OF STRUCTURE <GS_LINE> TO <FS_VALUE2>.

        <FS_VALUE2> = <FS_VALUE>.

        PERFORM ADD_NODEKEY2     USING   LV_KEY2
                                         GR_TREE_DATA
                                         <GS_LINE>
                                         'SETID'
                                CHANGING LV_KEY3.

    ENDCASE.

    MODIFY <GT_TABLE> FROM <GS_LINE>.

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
FORM SET_TREE_DATA .

  DATA LV_MONTH TYPE N LENGTH 3.
  DATA LV_FIELDNAME TYPE FIELDNAME.
  DATA LV_BFIELD    TYPE FIELDNAME.

  DATA: RESULT_TAB TYPE MATCH_RESULT_TAB.

  DATA LV_OPERAND TYPE STRING.

  FIELD-SYMBOLS: <FS_HSL>     TYPE ANY,
                 <FS_SUM>     TYPE ANY,
                 <FS_PLAN>    TYPE ANY,
                 <FS_WKG>     TYPE ANY,
                 <FS_VALUE>   TYPE ANY,
                 <FS_VALUE2>  TYPE ANY,
                 <FS_HSL_PR>  TYPE ANY,
                 <FS_PLAN_PR> TYPE ANY.

  FIELD-SYMBOLS: <FS>  TYPE ANY.
  FIELD-SYMBOLS: <FS1> TYPE ANY.

  DATA: LV_RES    TYPE I,
        LV_OPE    TYPE C,
        LV_OFF    TYPE I,
        LV_LENGTH TYPE I.

  DATA LV_INDEX TYPE SYINDEX.
  DATA LV_LINES TYPE SYTFILL.

  DATA LV_ZCODE TYPE ZCODE.

  DATA LV_PRCTR_BUKRS TYPE CHAR8.

  DATA: LR_DATA  TYPE REF TO DATA.
  CREATE DATA LR_DATA LIKE LINE OF <GT_TABLE>.
  ASSIGN  LR_DATA->* TO FIELD-SYMBOL(<LS_TEMP>).

  RANGES R_SETNAME FOR SETNODE-SETNAME.

  LOOP AT GT_ZCOT0150.

    ASSIGN COMPONENT 'ZCODE' OF STRUCTURE <GS_LINE>
      TO <FS_VALUE>.

    <FS_VALUE> = GT_ZCOT0150-ZCODE.

    ASSIGN COMPONENT 'SETTEXT' OF STRUCTURE <GS_LINE>
      TO <FS_VALUE>.

    <FS_VALUE> = GT_ZCOT0150-DESCR.

    ASSIGN COMPONENT 'GUBUN' OF STRUCTURE <GS_LINE>
      TO <FS_VALUE>.

    <FS_VALUE> = GT_ZCOT0150-GUBUN.

    ASSIGN COMPONENT 'WAERS' OF STRUCTURE <GS_LINE>
      TO <FS_VALUE>.

    <FS_VALUE> = GV_WAERS.

    ASSIGN COMPONENT 'TYPE' OF STRUCTURE <GS_LINE>
      TO <FS_VALUE>.

    <FS_VALUE> = 'H'.

    COLLECT <GS_LINE> INTO <GT_TABLE>.

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

          MOVE: 'I'               TO R_SETNAME-SIGN,
                'EQ'              TO R_SETNAME-OPTION,
               GT_ZCOT0150-FKAGRU TO R_SETNAME-LOW.

          APPEND R_SETNAME.

        ENDIF.

        SELECT * FROM SETHEADER
          INTO TABLE @DATA(LT_SETHEADER)
         WHERE SETCLASS = '0102'
           AND SUBCLASS = @PA_KOKRS
           AND SETNAME  IN @R_SETNAME
          ORDER BY SETCLASS, SUBCLASS, SETNAME.

        IF SY-SUBRC = 0 .

          LOOP AT LT_SETHEADER INTO DATA(LS_SETHEADER).

            PERFORM HIERARCHY_READ USING LS_SETHEADER-SETNAME.

            LOOP AT GT_NODES.

              ASSIGN COMPONENT 'SETID' OF STRUCTURE <GS_LINE>
                TO <FS_VALUE>.

              <FS_VALUE> = GT_NODES-SHORTNAME.

              ASSIGN COMPONENT 'SHORTNAME' OF STRUCTURE <GS_LINE>
                TO <FS_VALUE>.

              <FS_VALUE> = GT_NODES-DESCRIPT.

              ASSIGN COMPONENT 'TYPE' OF STRUCTURE <GS_LINE>
                TO <FS_VALUE>.

              <FS_VALUE> = GT_NODES-TYPE.

              ASSIGN COMPONENT 'HLEVEL' OF STRUCTURE <GS_LINE>
                TO <FS_VALUE>.

              <FS_VALUE> = GT_NODES-HLEVEL.


*-- 상위노드 GET
              CASE GT_NODES-HLEVEL.

                WHEN 0.

                  ASSIGN COMPONENT 'TYPE' OF STRUCTURE <GS_LINE>
                    TO <FS_VALUE>.

                  <FS_VALUE> = 'R'.

                  ASSIGN COMPONENT 'SUBNAME' OF STRUCTURE <GS_LINE>
                    TO <FS_VALUE>.

                  ASSIGN COMPONENT 'SETID' OF STRUCTURE <GS_LINE>
                   TO <FS_VALUE2>.

                  <FS_VALUE> = <FS_VALUE2>.

                  PERFORM CLEAR_FIELD USING 'KSTAR'
                                            <GS_LINE>.

                  PERFORM CLEAR_FIELD USING 'KTEXT'
                                            <GS_LINE>.

                  PERFORM CLEAR_FIELD USING 'DESCR'
                                            <GS_LINE>.

                  COLLECT <GS_LINE> INTO <GT_TABLE>.

                  READ TABLE GT_VALUES
                       WITH KEY SETID = GT_NODES-SETID.  "계정 X
                  IF SY-SUBRC <> 0.
                    CONTINUE.
                  ENDIF.

                WHEN OTHERS.

                  READ TABLE GT_SETNODE
                     WITH KEY SUBSETNAME = GT_NODES-SHORTNAME.

                  IF SY-SUBRC = 0.

                    ASSIGN COMPONENT 'SUBNAME'
                      OF STRUCTURE <GS_LINE> TO <FS_VALUE>.

                    <FS_VALUE> = GT_SETNODE-SETNAME.

                  ELSE.

                    ASSIGN COMPONENT 'SUBNAME'
                     OF STRUCTURE <GS_LINE> TO <FS_VALUE>.

                    <FS_VALUE> = GT_NODES-SHORTNAME.

                  ENDIF.

                  READ TABLE GT_VALUES
                      WITH KEY SETID = GT_NODES-SETID.  "계정 X

                  IF SY-SUBRC <> 0.

                    PERFORM CLEAR_FIELD USING 'KSTAR'
                                              <GS_LINE>.

                    PERFORM CLEAR_FIELD USING 'KTEXT'
                                              <GS_LINE>.


                    PERFORM CLEAR_FIELD USING 'DESCR'
                                              <GS_LINE>.

                    LOOP AT GT_BASE.

                      AT NEW PRCTR.

                        LV_FIELDNAME = 'P_' && GT_BASE-PRCTR.
                        PERFORM CLEAR_FIELD USING LV_FIELDNAME
                                                  <GS_LINE>.

                        LV_FIELDNAME = 'S_' && GT_BASE-PRCTR.
                        PERFORM CLEAR_FIELD USING LV_FIELDNAME
                                                  <GS_LINE>.

                      ENDAT.

                      LV_FIELDNAME = 'P_' && GT_BASE-PRCTR
                                          && GT_BASE-BUKRS.
                      PERFORM CLEAR_FIELD USING LV_FIELDNAME
                                                <GS_LINE>.

                      LV_FIELDNAME = 'S_' && GT_BASE-PRCTR
                                          && GT_BASE-BUKRS.
                      PERFORM CLEAR_FIELD USING LV_FIELDNAME
                                                <GS_LINE>.

                    ENDLOOP.

                    COLLECT <GS_LINE> INTO <GT_TABLE>.
                    CONTINUE.

                  ENDIF.

              ENDCASE.

              ASSIGN COMPONENT 'TYPE' OF STRUCTURE <GS_LINE>
                TO <FS_VALUE>.

              <FS_VALUE> = GT_NODES-TYPE.

*-- 그룹에 등록되어 있는 계정만큼

              LOOP AT GT_VALUES WHERE SETID = GT_NODES-SETID.

                LOOP AT GT_CSKA WHERE KSTAR >= GT_VALUES-VFROM
                                  AND KSTAR <= GT_VALUES-VTO.

                  ASSIGN COMPONENT 'KSTAR' OF STRUCTURE <GS_LINE>
                    TO <FS_VALUE>.

                  <FS_VALUE> = GT_CSKA-KSTAR.

                  ASSIGN COMPONENT 'KTEXT' OF STRUCTURE <GS_LINE>
                    TO <FS_VALUE>.

                  <FS_VALUE> = GT_CSKA-KTEXT.

                  ASSIGN COMPONENT 'DESCR' OF STRUCTURE <GS_LINE>
                    TO <FS_VALUE>.

                  <FS_VALUE> = GT_CSKA-KTEXT.

                  COLLECT <GS_LINE> INTO <GT_TABLE>.

                  LOOP AT GT_DATA ASSIGNING FIELD-SYMBOL(<LS_DATA>)
                                   WHERE KSTAR = GT_CSKA-KSTAR.

                    READ TABLE GT_BASE
                     WITH KEY PRCTR = <LS_DATA>-PRCTR
                              BUKRS = <LS_DATA>-BUKRS
                      TRANSPORTING  NO FIELDS.

                    IF SY-SUBRC = 0 .
                      LV_PRCTR_BUKRS = <LS_DATA>-PRCTR &&
                                      <LS_DATA>-BUKRS.
                    ELSE.
                      <LS_DATA>-PRCTR = 'PZZZ'.
                      LV_PRCTR_BUKRS = 'PZZZ'.
*                      CONTINUE.            "CBO 맵핑이 없으면 SUM 제외
                    ENDIF.

                    CLEAR LV_MONTH.

                    DO 12 TIMES.

                      ADD 1 TO LV_MONTH.

                      IF LV_MONTH >= PA_SPERL AND
                         LV_MONTH <= PA_EPERL.

                        LV_FIELDNAME = '<LS_DATA>-WKG' && LV_MONTH.

                        ASSIGN (LV_FIELDNAME) TO <FS_WKG>.

                        CASE <LS_DATA>-WRTTP.

                          WHEN '04'.   "실적

                            LV_BFIELD = 'S_' && LV_PRCTR_BUKRS.

                            ASSIGN COMPONENT LV_BFIELD
                             OF STRUCTURE <GS_LINE> TO <FS_HSL>.

                            <FS_HSL> = <FS_HSL> + <FS_WKG>.

                            LV_BFIELD = 'S_' && <LS_DATA>-PRCTR.

                            ASSIGN COMPONENT LV_BFIELD
                             OF STRUCTURE <GS_LINE> TO <FS_HSL_PR>.

                            <FS_HSL_PR> = <FS_HSL_PR> + <FS_WKG>.

                          WHEN '01'.    "계획

                            IF <LS_DATA>-VERSN = '000' AND "수주계획은 실적으로 표기
                               <LS_DATA>-KSTAR  = '0984000010'.

                              LV_BFIELD = 'S_' && LV_PRCTR_BUKRS.

                              ASSIGN COMPONENT LV_BFIELD
                               OF STRUCTURE <GS_LINE> TO <FS_HSL>.

                              <FS_HSL> = <FS_HSL> + <FS_WKG>.

                              LV_BFIELD = 'S_' && <LS_DATA>-PRCTR.

                              ASSIGN COMPONENT LV_BFIELD
                               OF STRUCTURE <GS_LINE> TO <FS_HSL_PR>.

                              <FS_HSL_PR> = <FS_HSL_PR> + <FS_WKG>.

                            ELSE.

                              LV_BFIELD = 'P_' && LV_PRCTR_BUKRS.

                              ASSIGN COMPONENT LV_BFIELD
                               OF STRUCTURE <GS_LINE> TO <FS_PLAN>.

                              <FS_PLAN> = <FS_PLAN> + <FS_WKG>.

                              LV_BFIELD = 'P_' && <LS_DATA>-PRCTR.

                              ASSIGN COMPONENT LV_BFIELD
                               OF STRUCTURE <GS_LINE> TO <FS_PLAN_PR>.

                              <FS_PLAN_PR> = <FS_PLAN_PR> + <FS_WKG>.

                            ENDIF.

                        ENDCASE.

                      ENDIF.

                    ENDDO.

                    IF GT_CSKA-KSTAR CP '04*' OR
                        GT_CSKA-KSTAR CP '0701*' OR
                        GT_CSKA-KSTAR CP '0703*' OR
                        GT_CSKA-KSTAR CP '0705*'.  "매출 계정은 음수로 표시

                      IF <FS_PLAN> IS  ASSIGNED.
                        <FS_PLAN> = <FS_PLAN> * '-1'.
                      ENDIF.

                      IF <FS_HSL> IS  ASSIGNED.
                        <FS_HSL> = <FS_HSL> * '-1'.
                      ENDIF.

                      IF <FS_HSL_PR> IS  ASSIGNED.
                        <FS_HSL_PR> = <FS_HSL_PR> * '-1'.
                      ENDIF.

                      IF <FS_PLAN_PR> IS  ASSIGNED.
                        <FS_PLAN_PR> = <FS_PLAN_PR> * '-1'.
                      ENDIF.

                    ENDIF.

                    COLLECT <GS_LINE> INTO <GT_TABLE>.

                    DO.

                      ASSIGN COMPONENT SY-INDEX OF STRUCTURE <GS_LINE>
                              TO FIELD-SYMBOL(<LS_VALUE>).
                      IF SY-SUBRC <> 0.
                        EXIT.
                      ENDIF.

                      ASSIGN COMPONENT SY-INDEX OF STRUCTURE <LS_TEMP>
                              TO FIELD-SYMBOL(<LS_VALUE2>).
                      IF SY-SUBRC <> 0.
                        EXIT.
                      ENDIF.

                      <LS_VALUE2> = <LS_VALUE>.

                    ENDDO.

                    ASSIGN COMPONENT 'TYPE'
                        OF STRUCTURE <LS_TEMP> TO <FS_VALUE>.

                    <FS_VALUE> = 'H'.

                    PERFORM CLEAR_FIELD USING 'SETID'
                                              <LS_TEMP>.

                    PERFORM CLEAR_FIELD USING 'SHORTNAME'
                                              <LS_TEMP>.

                    PERFORM CLEAR_FIELD USING 'KSTAR'
                                              <LS_TEMP>.

                    PERFORM CLEAR_FIELD USING 'KTEXT'
                                              <LS_TEMP>.

                    PERFORM CLEAR_FIELD USING 'SUBNAME'
                                              <LS_TEMP>.

                    PERFORM CLEAR_FIELD USING 'HLEVEL'
                                              <LS_TEMP>.

                    PERFORM CLEAR_FIELD USING 'DESCR'
                                              <LS_TEMP>.

                    COLLECT <LS_TEMP> INTO <GT_TABLE>.

                    LOOP AT GT_BASE.

                      AT NEW PRCTR.

                        LV_FIELDNAME = 'P_' && GT_BASE-PRCTR.
                        PERFORM CLEAR_FIELD USING LV_FIELDNAME
                                                  <GS_LINE>.

                        LV_FIELDNAME = 'S_' && GT_BASE-PRCTR.
                        PERFORM CLEAR_FIELD USING LV_FIELDNAME
                                                  <GS_LINE>.

                      ENDAT.

                      LV_FIELDNAME = 'P_' && GT_BASE-PRCTR
                                          && GT_BASE-BUKRS.
                      PERFORM CLEAR_FIELD USING LV_FIELDNAME
                                                <GS_LINE>.

                      LV_FIELDNAME = 'S_' && GT_BASE-PRCTR
                                          && GT_BASE-BUKRS.
                      PERFORM CLEAR_FIELD USING LV_FIELDNAME
                                                <GS_LINE>.

                    ENDLOOP.

                  ENDLOOP.

                ENDLOOP.

              ENDLOOP.

            ENDLOOP.

          ENDLOOP.

        ENDIF.

    ENDCASE.

    CLEAR <GS_LINE>.

  ENDLOOP.

  LOOP AT <GT_TABLE> ASSIGNING <GS_LINE>.

    CLEAR: LV_INDEX, LV_LINES.

    ASSIGN COMPONENT 'GUBUN'
           OF STRUCTURE <GS_LINE> TO <FS_VALUE>.
    CHECK <FS_VALUE> = '1'.

    ASSIGN COMPONENT 'ZCODE'
           OF STRUCTURE <GS_LINE> TO <FS_VALUE>.

    READ TABLE GT_ZCOT0150 WITH KEY ZCODE = <FS_VALUE>.

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

          READ TABLE RESULT_TAB
               INTO DATA(LS_RESULT) INDEX LV_INDEX.

          IF LV_INDEX = 1.

            MOVE: GT_ZCOT0150-CALCU(4)   TO LV_ZCODE,
                  GT_ZCOT0150-CALCU+4(1) TO LV_OPE.

            LV_OFF = LS_RESULT-OFFSET + LS_RESULT-LENGTH.

            READ TABLE <GT_TABLE>
                  ASSIGNING FIELD-SYMBOL(<LS_LINE_TEMP>)
                  WITH KEY ('ZCODE') = LV_ZCODE
                           ('SETID') = SPACE.

            IF SY-SUBRC = 0.

              LOOP AT GT_BASE.

                AT NEW PRCTR.

                  LV_FIELDNAME = 'P_' && GT_BASE-PRCTR.

                  ASSIGN COMPONENT LV_FIELDNAME
                         OF STRUCTURE <GS_LINE>
                      TO <FS_VALUE>.

                  ASSIGN COMPONENT LV_FIELDNAME
                         OF STRUCTURE <LS_LINE_TEMP>
                      TO <FS_VALUE2>.

                  <FS_VALUE> = <FS_VALUE2>.

                  LV_FIELDNAME = 'S_' && GT_BASE-PRCTR.

                  ASSIGN COMPONENT LV_FIELDNAME
                      OF STRUCTURE <GS_LINE>
                      TO <FS_VALUE>.

                  ASSIGN COMPONENT LV_FIELDNAME
                        OF STRUCTURE <LS_LINE_TEMP>
                      TO <FS_VALUE2>.

                  <FS_VALUE> = <FS_VALUE2>.

                ENDAT.

                LV_FIELDNAME = 'P_' && GT_BASE-PRCTR &&
                                       GT_BASE-BUKRS.

                ASSIGN COMPONENT LV_FIELDNAME
                    OF STRUCTURE <GS_LINE>
                    TO <FS_VALUE>.

                ASSIGN COMPONENT LV_FIELDNAME
                    OF STRUCTURE <LS_LINE_TEMP>
                    TO <FS_VALUE2>.

                <FS_VALUE> = <FS_VALUE2>.

                LV_FIELDNAME = 'S_' && GT_BASE-PRCTR &&
                                       GT_BASE-BUKRS.

                ASSIGN COMPONENT LV_FIELDNAME
                    OF STRUCTURE <GS_LINE>
                    TO <FS_VALUE>.

                ASSIGN COMPONENT LV_FIELDNAME
                    OF STRUCTURE <LS_LINE_TEMP>
                    TO <FS_VALUE2>.

                <FS_VALUE> = <FS_VALUE2>.

              ENDLOOP.

            ENDIF.

          ELSE.

            MOVE GT_ZCOT0150-CALCU+LV_OFF(4) TO LV_ZCODE.

            LV_OFF = LS_RESULT-OFFSET + LS_RESULT-LENGTH.

            READ TABLE <GT_TABLE>
                  ASSIGNING FIELD-SYMBOL(<LS_LINE_DATA>)
                  WITH KEY ('ZCODE') = LV_ZCODE
                           ('SETID') = SPACE.

            IF SY-SUBRC = 0.

              LOOP AT GT_BASE.

                AT NEW PRCTR.

                  LV_FIELDNAME = 'P_' && GT_BASE-PRCTR.

                  ASSIGN COMPONENT LV_FIELDNAME
                      OF STRUCTURE <GS_LINE> TO <FS_VALUE>.

                  ASSIGN COMPONENT LV_FIELDNAME
                     OF STRUCTURE <LS_LINE_DATA> TO <FS_VALUE2>.

                  CASE LV_OPE.

                    WHEN '+'.
                      <FS_VALUE> = <FS_VALUE> + <FS_VALUE2>.

                    WHEN '-'.
                      <FS_VALUE> = <FS_VALUE> - <FS_VALUE2>.

                  ENDCASE.

                  LV_FIELDNAME = 'S_' && GT_BASE-PRCTR.

                  ASSIGN COMPONENT LV_FIELDNAME
                      OF STRUCTURE <GS_LINE> TO <FS_VALUE>.

                  ASSIGN COMPONENT LV_FIELDNAME
                     OF STRUCTURE  <LS_LINE_DATA> TO <FS_VALUE2>.

                  CASE LV_OPE.

                    WHEN '+'.
                      <FS_VALUE> = <FS_VALUE> + <FS_VALUE2>.

                    WHEN '-'.
                      <FS_VALUE> = <FS_VALUE> - <FS_VALUE2>.

                  ENDCASE.

                ENDAT.

                LV_FIELDNAME = 'P_' && GT_BASE-PRCTR && GT_BASE-BUKRS.

                ASSIGN COMPONENT LV_FIELDNAME
                    OF STRUCTURE <GS_LINE> TO <FS_VALUE>.

                ASSIGN COMPONENT LV_FIELDNAME
                   OF STRUCTURE <LS_LINE_DATA> TO <FS_VALUE2>.

                CASE LV_OPE.

                  WHEN '+'.
                    <FS_VALUE> = <FS_VALUE> + <FS_VALUE2>.

                  WHEN '-'.
                    <FS_VALUE> = <FS_VALUE> - <FS_VALUE2>.

                ENDCASE.

                LV_FIELDNAME = 'S_' && GT_BASE-PRCTR && GT_BASE-BUKRS.

                ASSIGN COMPONENT LV_FIELDNAME
                    OF STRUCTURE <GS_LINE> TO <FS_VALUE>.

                ASSIGN COMPONENT LV_FIELDNAME
                   OF STRUCTURE  <LS_LINE_DATA> TO <FS_VALUE2>.

                CASE LV_OPE.

                  WHEN '+'.
                    <FS_VALUE> = <FS_VALUE> + <FS_VALUE2>.

                  WHEN '-'.
                    <FS_VALUE> = <FS_VALUE> - <FS_VALUE2>.

                ENDCASE.

              ENDLOOP.

            ENDIF.

            MOVE GT_ZCOT0150-CALCU+LS_RESULT-OFFSET(1) TO LV_OPE.

          ENDIF.

        ENDDO.

      ENDIF.

    ENDIF.

    MODIFY <GT_TABLE> FROM <GS_LINE>.

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
                           PS_DATA
                           P_GUBUN
                  CHANGING PV_NODE_KEY  TYPE LVC_NKEY.

  DATA: LV_NODE_TEXT TYPE LVC_VALUE.

  DATA: LT_ITEM_LAYOUT TYPE LVC_T_LAYI,
        LS_ITEM_LAYOUT TYPE LVC_S_LAYI.

  DATA LV_KEY TYPE LVC_NKEY.

  DATA LS_NODE_LAYOUT TYPE LVC_S_LAYN.

  DATA: LR_DATA  TYPE REF TO DATA.
  CREATE DATA LR_DATA LIKE LINE OF <GT_TABLE>.
  ASSIGN  LR_DATA->* TO FIELD-SYMBOL(<LS_TEMP>).

  DO.

    ASSIGN COMPONENT SY-INDEX OF STRUCTURE PS_DATA
            TO FIELD-SYMBOL(<LS_VALUE>).
    IF SY-SUBRC <> 0.
      EXIT.
    ENDIF.

    ASSIGN COMPONENT SY-INDEX OF STRUCTURE <LS_TEMP>
            TO FIELD-SYMBOL(<LS_VALUE2>).
    IF SY-SUBRC <> 0.
      EXIT.
    ENDIF.

    <LS_VALUE2> = <LS_VALUE>.

  ENDDO.

  CASE P_GUBUN.

    WHEN 'ZCODE'.

      PERFORM CLEAR_FIELD USING 'SETID'
                                <LS_TEMP>.

      PERFORM CLEAR_FIELD USING 'SHORTNAME'
                                <LS_TEMP>.

      PERFORM CLEAR_FIELD USING 'KSTAR'
                                <LS_TEMP>.

      PERFORM CLEAR_FIELD USING 'KTEXT'
                                <LS_TEMP>.

      ASSIGN COMPONENT 'SETTEXT' OF STRUCTURE <LS_TEMP>
              TO FIELD-SYMBOL(<LV_SETTEXT>).

      LV_NODE_TEXT = <LV_SETTEXT>.

      LS_NODE_LAYOUT-ISFOLDER  = 'X'.
      CALL METHOD PR_TREE->ADD_NODE
        EXPORTING
          I_RELAT_NODE_KEY = PV_RELAT_KEY
          I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
          I_NODE_TEXT      = LV_NODE_TEXT
          IS_OUTTAB_LINE   = <LS_TEMP>
          IS_NODE_LAYOUT   = LS_NODE_LAYOUT
          IT_ITEM_LAYOUT   = LT_ITEM_LAYOUT
        IMPORTING
          E_NEW_NODE_KEY   = PV_NODE_KEY.

    WHEN 'SETID'.

      PERFORM CLEAR_FIELD USING 'KSTAR'
                                <LS_TEMP>.

      PERFORM CLEAR_FIELD USING 'KTEXT'
                                <LS_TEMP>.

      ASSIGN COMPONENT 'SETID' OF STRUCTURE <LS_TEMP>
              TO FIELD-SYMBOL(<LV_SETID>).

      LV_NODE_TEXT = <LV_SETID>.

      LS_NODE_LAYOUT-ISFOLDER  = 'X'.

      CALL METHOD PR_TREE->ADD_NODE
        EXPORTING
          I_RELAT_NODE_KEY = PV_RELAT_KEY
          I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
          I_NODE_TEXT      = LV_NODE_TEXT
          IS_OUTTAB_LINE   = <LS_TEMP>
          IS_NODE_LAYOUT   = LS_NODE_LAYOUT
          IT_ITEM_LAYOUT   = LT_ITEM_LAYOUT
        IMPORTING
          E_NEW_NODE_KEY   = PV_NODE_KEY.

      ASSIGN COMPONENT 'ZCODE' OF STRUCTURE <LS_TEMP>
              TO FIELD-SYMBOL(<LV_ZCODE>).

      ASSIGN COMPONENT 'SETID' OF STRUCTURE <LS_TEMP>
              TO FIELD-SYMBOL(<LV_SETID2>).

      PERFORM ADD_LINE USING <LV_SETID2>
                             <LV_ZCODE>
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

  DATA LV_INDEX TYPE SYTABIX.

* set item-layout
  DATA: LT_ITEM_LAYOUT TYPE LVC_T_LAYI,
        LS_ITEM_LAYOUT TYPE LVC_S_LAYI.

  FIELD-SYMBOLS: <LS_DATA>    TYPE ANY,
                 <FS_VALUE>   TYPE ANY,
                 <FS_VALUE2>  TYPE ANY,
                 <FS_SETID>   TYPE ANY,
                 <FS_SUBNAME> TYPE ANY,
                 <FS_ZCODE>   TYPE ANY.

  DATA LV_SETID TYPE SETID.

  DATA: LR_DATA  TYPE REF TO DATA.
  CREATE DATA LR_DATA LIKE LINE OF <GT_TABLE>.
  ASSIGN  LR_DATA->* TO FIELD-SYMBOL(<LS_TEMP>).

  LOOP AT <GT_TABLE> ASSIGNING FIELD-SYMBOL(<LS_LINE>).

    LV_INDEX = SY-TABIX.

    ASSIGN COMPONENT 'ZCODE' OF STRUCTURE <LS_LINE>
        TO <FS_ZCODE>.

    CHECK <FS_ZCODE> = P_ZCODE.

    ASSIGN COMPONENT 'SUBNAME' OF STRUCTURE <LS_LINE>
        TO <FS_SUBNAME>.

    CHECK <FS_SUBNAME> = P_SETID.

    ASSIGN COMPONENT 'TYPE' OF STRUCTURE <LS_LINE>
        TO <FS_VALUE>.

    CHECK <FS_VALUE> <> 'H' AND
          <FS_VALUE> <> 'R'.

    DO.

      ASSIGN COMPONENT SY-INDEX OF STRUCTURE <LS_LINE>
              TO FIELD-SYMBOL(<LS_VALUE>).
      IF SY-SUBRC <> 0.
        EXIT.
      ENDIF.

      ASSIGN COMPONENT SY-INDEX OF STRUCTURE <LS_TEMP>
              TO FIELD-SYMBOL(<LS_VALUE2>).
      IF SY-SUBRC <> 0.
        EXIT.
      ENDIF.

      <LS_VALUE2> = <LS_VALUE>.

    ENDDO.

    ASSIGN COMPONENT 'SETID' OF STRUCTURE <LS_TEMP>
        TO <FS_SETID>.

    ASSIGN COMPONENT 'SUBNAME' OF STRUCTURE <LS_TEMP>
        TO <FS_SUBNAME>.

    IF <FS_SETID> = <FS_SUBNAME>.

      ASSIGN COMPONENT 'KSTAR' OF STRUCTURE <LS_TEMP>
          TO <FS_VALUE>.

      LV_NODE_TEXT = <FS_VALUE>.

      ASSIGN COMPONENT 'KTEXT' OF STRUCTURE <LS_TEMP>
          TO <FS_VALUE>.

      ASSIGN COMPONENT 'DESCR' OF STRUCTURE <LS_TEMP>
          TO <FS_VALUE2>.

      <FS_VALUE2> = <FS_VALUE>.

      MODIFY <GT_TABLE> FROM <LS_TEMP> INDEX LV_INDEX.

    ELSEIF <FS_SETID> <> <FS_SUBNAME>.

      IF LV_SETID = <FS_SETID>.
        CONTINUE.
      ENDIF.

      LV_SETID = <FS_SETID>.

      LV_NODE_TEXT = <FS_SETID>.

      ASSIGN COMPONENT 'SHORTNAME' OF STRUCTURE <LS_TEMP>
          TO <FS_VALUE>.

      ASSIGN COMPONENT 'DESCR' OF STRUCTURE <LS_TEMP>
          TO <FS_VALUE2>.

      <FS_VALUE2> = <FS_VALUE>.

      MODIFY <GT_TABLE> FROM <LS_TEMP> INDEX LV_INDEX.

      PERFORM CLEAR_FIELD USING 'KSTAR'
                                 <LS_TEMP>.

      PERFORM CLEAR_FIELD USING 'KTEXT'
                                 <LS_TEMP>.

      LS_NODE_LAYOUT-ISFOLDER  = 'X'.

    ENDIF.

    CALL METHOD GR_TREE_DATA->ADD_NODE
      EXPORTING
        I_RELAT_NODE_KEY = P_KEY
        I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
        I_NODE_TEXT      = LV_NODE_TEXT
        IS_OUTTAB_LINE   = <LS_TEMP>
        IS_NODE_LAYOUT   = LS_NODE_LAYOUT
        IT_ITEM_LAYOUT   = LT_ITEM_LAYOUT
      IMPORTING
        E_NEW_NODE_KEY   = LV_KEY.

    IF  <FS_SETID> <> <FS_SUBNAME>.

      PERFORM SET_KSTAR USING  <FS_SETID>
                               <FS_SUBNAME>
                               P_ZCODE
                               LV_KEY.

      READ TABLE GT_SETNODE WITH KEY SETNAME = <FS_SETID>
        TRANSPORTING NO FIELDS.

      IF SY-SUBRC = 0 .
        PERFORM ADD_LINE USING <FS_SETID>
                               <FS_ZCODE>
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

  FIELD-SYMBOLS: <LS_DATA>    TYPE ANY,
                 <FS_VALUE>   TYPE ANY,
                 <FS_VALUE2>  TYPE ANY,
                 <FS_SETID>   TYPE ANY,
                 <FS_SUBNAME> TYPE ANY,
                 <FS_KSTAR>   TYPE ANY,
                 <FS_TYPE>    TYPE ANY,
                 <FS_ZCODE>   TYPE ANY.

  DATA LV_NODE_TEXT  TYPE LVC_VALUE.
  DATA LV_KEY        TYPE LVC_NKEY.
  DATA LV_INDEX      TYPE SYTABIX.

* set item-layout
  DATA: LT_ITEM_LAYOUT TYPE LVC_T_LAYI,
        LS_ITEM_LAYOUT TYPE LVC_S_LAYI.

  DATA IS_NODE_LAYOUT TYPE  LVC_S_LAYN.

  LOOP AT <GT_TABLE> ASSIGNING FIELD-SYMBOL(<LS_LINE>).

    LV_INDEX = SY-TABIX.

    ASSIGN COMPONENT 'ZCODE' OF STRUCTURE <LS_LINE>
      TO <FS_ZCODE>.

    CHECK <FS_ZCODE> = P_ZCODE.

    ASSIGN COMPONENT 'SETID' OF STRUCTURE <LS_LINE>
      TO <FS_SETID>.

    CHECK <FS_SETID> = P_SETID.

    ASSIGN COMPONENT 'SUBNAME' OF STRUCTURE <LS_LINE>
      TO <FS_SUBNAME>.

    CHECK <FS_SUBNAME> <> P_SETID.

    ASSIGN COMPONENT 'KSTAR' OF STRUCTURE <LS_LINE>
      TO <FS_KSTAR>.

    CHECK <FS_KSTAR> IS NOT INITIAL.

    ASSIGN COMPONENT 'TYPE' OF STRUCTURE <LS_LINE>
      TO <FS_TYPE>.

    CHECK <FS_TYPE> <> 'H' AND
          <FS_TYPE> <> 'R'.

    LV_NODE_TEXT = <FS_KSTAR>.

    ASSIGN COMPONENT 'KTEXT' OF STRUCTURE <LS_LINE>
    TO <FS_VALUE>.

    ASSIGN COMPONENT 'DESCR' OF STRUCTURE <LS_LINE>
        TO <FS_VALUE2>.

    <FS_VALUE2> = <FS_VALUE>.

    MODIFY <GT_TABLE> FROM <LS_LINE> INDEX LV_INDEX.

    IS_NODE_LAYOUT-STYLE = CL_GUI_COLUMN_TREE=>STYLE_EMPHASIZED_C.
    IS_NODE_LAYOUT-STYLE = CL_GUI_COLUMN_TREE=>STYLE_INACTIVE.

*    LS_ITEM_LAYOUT-CLASS = CL_GUI_COLUMN_TREE=>STYLE_INACTIVE.
*    LS_ITEM_LAYOUT-STYLE = CL_GUI_COLUMN_TREE=>STYLE_EMPHASIZED_C.
*    LS_ITEM_LAYOUT-FIELDNAME = GR_TREE_DATA->C_HIERARCHY_COLUMN_NAME.
*    APPEND LS_ITEM_LAYOUT TO LT_ITEM_LAYOUT.

    CALL METHOD GR_TREE_DATA->ADD_NODE
      EXPORTING
        I_RELAT_NODE_KEY = P_KEY
        I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
        I_NODE_TEXT      = LV_NODE_TEXT
        IS_NODE_LAYOUT   = IS_NODE_LAYOUT
        IS_OUTTAB_LINE   = <LS_LINE>
        IT_ITEM_LAYOUT   = LT_ITEM_LAYOUT
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

  CREATE DATA LI_DATA LIKE <GT_TABLE>.
  ASSIGN LI_DATA->* TO <LT_DATA>.

  <LT_DATA> = <GT_TABLE>.

  CLEAR: T_FIELDCAT[].

  T_FIELDCAT[] =  GT_FIELDCAT[].

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
*& Form MAKE_TABLE
*&---------------------------------------------------------------------*
FORM MAKE_TABLE .

  DATA LV_FIELDNAME TYPE FIELDNAME.
  DATA LV_TITLE     TYPE LVC_TXTCOL.

  CLEAR GT_FIELDCAT[].

  IF <GT_TABLE> IS ASSIGNED.
    UNASSIGN <GT_TABLE>.
    UNASSIGN <GS_LINE>.
    UNASSIGN <GT_TABLE_DISPLAY>.
    CLEAR GR_DATA.
  ENDIF.

  PERFORM FILL_FIELD_CATEGORY USING :

        'S' 'FIELDNAME'   'ZCODE',
        ' ' 'OUTPUTLEN'   '4',
        ' ' 'REF_TABLE'   'ZCOS0110',
        ' ' 'REF_FIELD'   'ZCODE',
        'E' 'COLTEXT'     TEXT-C01,

        'S' 'FIELDNAME'   'GUBUN',
        ' ' 'OUTPUTLEN'   '1',
        ' ' 'REF_TABLE'   'ZCOS0110',
        ' ' 'REF_FIELD'   'GUBUN',
        'E' 'COLTEXT'     TEXT-C02,

        'S' 'FIELDNAME'   'SETTEXT',
        ' ' 'OUTPUTLEN'   '40',
        ' ' 'REF_TABLE'   'ZCOS0110',
        ' ' 'REF_FIELD'   'SETTEXT',
        'E' 'COLTEXT'     TEXT-C03,

        'S' 'FIELDNAME'   'SETID',
        ' ' 'OUTPUTLEN'   '34',
        ' ' 'REF_TABLE'   'ZCOS0110',
        ' ' 'REF_FIELD'   'SETID',
        'E' 'COLTEXT'     TEXT-C04,

        'S' 'FIELDNAME'   'SHORTNAME',
        ' ' 'OUTPUTLEN'   '24',
        ' ' 'REF_TABLE'   'ZCOS0110',
        ' ' 'REF_FIELD'   'SHORTNAME',
        'E' 'COLTEXT'     TEXT-C05,

        'S' 'FIELDNAME'   'KSTAR',
        ' ' 'OUTPUTLEN'   '10',
        ' ' 'REF_TABLE'   'ZCOS0110',
        ' ' 'REF_FIELD'   'KSTAR',
        'E' 'COLTEXT'     TEXT-C06,

        'S' 'FIELDNAME'   'KTEXT',
        ' ' 'OUTPUTLEN'   '20',
        ' ' 'REF_TABLE'   'ZCOS0110',
        ' ' 'REF_FIELD'   'KTEXT',
        'E' 'COLTEXT'     TEXT-C07,

        'S' 'FIELDNAME'   'DESCR',
        ' ' 'OUTPUTLEN'   '200',
        ' ' 'REF_TABLE'   'ZCOS0110',
        ' ' 'REF_FIELD'   'DESCR',
        'E' 'COLTEXT'     TEXT-C08,

        'S' 'FIELDNAME'   'WAERS',
        ' ' 'OUTPUTLEN'   '5',
        ' ' 'REF_TABLE'   'ZCOS0110',
        ' ' 'REF_FIELD'   'WAERS',
        'E' 'COLTEXT'     TEXT-C09,

        'S' 'FIELDNAME'   'SUBNAME',
        ' ' 'OUTPUTLEN'   '34',
        ' ' 'REF_TABLE'   'ZCOS0110',
        ' ' 'REF_FIELD'   'SUBNAME',
        'E' 'COLTEXT'     TEXT-C10,

        'S' 'FIELDNAME'   'HLEVEL',
        ' ' 'OUTPUTLEN'   '6',
        ' ' 'REF_TABLE'   'ZCOS0110',
        ' ' 'REF_FIELD'   'HLEVEL',
        'E' 'COLTEXT'     TEXT-C11,

        'S' 'FIELDNAME'   'TYPE',
        ' ' 'OUTPUTLEN'   '1',
        ' ' 'REF_TABLE'   'ZCOS0110',
        ' ' 'REF_FIELD'   'TYPE',
        'E' 'COLTEXT'     TEXT-C12.

  LOOP AT GT_BASE.

    AT NEW PRCTR.

      CASE GT_BASE-PRCTR.

        WHEN 'PZZZ'.

          LV_FIELDNAME = 'P_' && GT_BASE-PRCTR.
          LV_TITLE     = TEXT-T11 && '(' && TEXT-T07 && ')'.

          PERFORM FILL_FIELD_CATEGORY USING :
                'S' 'FIELDNAME'   LV_FIELDNAME,
                ''  'OUTPUTLEN'    '20',
                ' ' 'CFIELDNAME'  'WAERS',
                ' ' 'REF_TABLE'   'ZCOS0110',
                ' ' 'REF_FIELD'   'F1HSL',
                'E' 'COLTEXT'     LV_TITLE.

          LV_FIELDNAME = 'S_' && GT_BASE-PRCTR.
          LV_TITLE     = TEXT-T11 && '(' && TEXT-T08 && ')'.

          PERFORM FILL_FIELD_CATEGORY USING :
                'S' 'FIELDNAME'   LV_FIELDNAME,
                ''  'OUTPUTLEN'    '20',
                ' ' 'CFIELDNAME'  'WAERS',
                ' ' 'REF_TABLE'   'ZCOS0110',
                ' ' 'REF_FIELD'   'F1HSL',
                'E' 'COLTEXT'     LV_TITLE.

        WHEN OTHERS.

          SELECT SINGLE KTEXT FROM CEPCT
            INTO @DATA(LV_KTEXT)
           WHERE PRCTR = @GT_BASE-PRCTR
             AND KOKRS = @PA_KOKRS
             AND DATBI = '99991231'.

          LV_FIELDNAME = 'P_' && GT_BASE-PRCTR.
          LV_TITLE     = GT_BASE-PRCTR && '_' &&
                         LV_KTEXT && '(' && TEXT-T07 && ')'.

          PERFORM FILL_FIELD_CATEGORY USING :
                'S' 'FIELDNAME'   LV_FIELDNAME,
                ''  'OUTPUTLEN'    '20',
                ' ' 'CFIELDNAME'  'WAERS',
                ' ' 'REF_TABLE'   'ZCOS0110',
                ' ' 'REF_FIELD'   'F1HSL',
                'E' 'COLTEXT'     LV_TITLE.

          LV_FIELDNAME = 'S_' && GT_BASE-PRCTR.
          LV_TITLE     =  GT_BASE-PRCTR && '_' &&
                          LV_KTEXT && '(' && TEXT-T08 && ')'.

          PERFORM FILL_FIELD_CATEGORY USING :
                'S' 'FIELDNAME'   LV_FIELDNAME,
                ''  'OUTPUTLEN'    '20',
                ' ' 'CFIELDNAME'  'WAERS',
                ' ' 'REF_TABLE'   'ZCOS0110',
                ' ' 'REF_FIELD'   'F1HSL',
                'E' 'COLTEXT'     LV_TITLE.

      ENDCASE.

    ENDAT.

    SELECT SINGLE BUTXT INTO @DATA(LV_BUTXT)
      FROM T001
     WHERE BUKRS = @GT_BASE-BUKRS.

    IF SY-SUBRC = 0.

      LV_FIELDNAME = 'P_' && GT_BASE-PRCTR && GT_BASE-BUKRS.
      LV_TITLE     = GT_BASE-BUKRS && '_' &&
                     LV_BUTXT && '(' && TEXT-T07 && ')'.

      PERFORM FILL_FIELD_CATEGORY USING :
            'S' 'FIELDNAME'   LV_FIELDNAME,
            ''  'OUTPUTLEN'    '20',
            ' ' 'CFIELDNAME'  'WAERS',
            ' ' 'REF_TABLE'   'ZCOS0110',
            ' ' 'REF_FIELD'   'F1HSL',
            'E' 'COLTEXT'     LV_TITLE.

      LV_FIELDNAME = 'S_' && GT_BASE-PRCTR && GT_BASE-BUKRS.
      LV_TITLE     = GT_BASE-BUKRS && '_' &&
                    LV_BUTXT && '(' && TEXT-T08 && ')'.

      PERFORM FILL_FIELD_CATEGORY USING :
            'S' 'FIELDNAME'   LV_FIELDNAME,
            ''  'OUTPUTLEN'    '20',
            ' ' 'CFIELDNAME'  'WAERS',
            ' ' 'REF_TABLE'   'ZCOS0110',
            ' ' 'REF_FIELD'   'F1HSL',
            'E' 'COLTEXT'     LV_TITLE.

    ENDIF.

  ENDLOOP.

  CALL METHOD CL_ALV_TABLE_CREATE=>CREATE_DYNAMIC_TABLE
    EXPORTING
      IT_FIELDCATALOG = GT_FIELDCAT
    IMPORTING
      EP_TABLE        = GR_DATA.

  ASSIGN  GR_DATA->* TO <GT_TABLE>.

  CREATE DATA GR_DATA LIKE LINE OF <GT_TABLE>.
  ASSIGN  GR_DATA->* TO <GS_LINE>.

  CALL METHOD CL_ALV_TABLE_CREATE=>CREATE_DYNAMIC_TABLE
    EXPORTING
      IT_FIELDCATALOG = GT_FIELDCAT
    IMPORTING
      EP_TABLE        = GR_DATA2.

  ASSIGN  GR_DATA2->* TO <GT_TABLE_DISPLAY>.

  CREATE DATA GR_DATA2 LIKE LINE OF <GT_TABLE_DISPLAY>.
  ASSIGN  GR_DATA2->* TO <GS_LINE_TEMP>.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_FIELD_CATEGORY
*&---------------------------------------------------------------------*
FORM FILL_FIELD_CATEGORY  USING PV_GUB PV_FNAME PV_CON.

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

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CLEAR_FIELD
*&---------------------------------------------------------------------*
FORM CLEAR_FIELD  USING    VALUE(P_FIELD)
                           PS_LINE.

  FIELD-SYMBOLS <FS_CLEAR> TYPE ANY.

  ASSIGN COMPONENT P_FIELD OF STRUCTURE PS_LINE TO <FS_CLEAR>.
  CLEAR <FS_CLEAR>.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCRFIELDS_FUNCTXT
*&---------------------------------------------------------------------*
FORM SCRFIELDS_FUNCTXT .

  GS_FUNTXT-ICON_ID   = ICON_INFORMATION.
  GS_FUNTXT-QUICKINFO = 'Program Help'.

  SSCRFIELDS-FUNCTXT_01 = GS_FUNTXT.

  GS_FUNTXT-ICON_ID   = ICON_MAPPED_RELATION.
  GS_FUNTXT-QUICKINFO = TEXT-Z01.
  GS_FUNTXT-ICON_TEXT = TEXT-Z01.

  SSCRFIELDS-FUNCTXT_02 = GS_FUNTXT.

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

    WHEN 'FC02'.
      CALL TRANSACTION 'ZCOV1150' AND SKIP FIRST SCREEN.
    WHEN OTHERS.

  ENDCASE.

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
         R_BUKRS2,  R_BUKRS2[],
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

  IF SO_PRCTR[] IS INITIAL AND
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
