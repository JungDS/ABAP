*&---------------------------------------------------------------------*
*& Include          ZCOR0400F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIAL_SET
*&---------------------------------------------------------------------*
FORM INITIAL_SET .

  SELECT SINGLE BEZEI, WAERS INTO (@PA_KTXT,@GV_WAERS)
    FROM TKA01
   WHERE KOKRS = '1000'.

  SELECT SINGLE VTEXT INTO @PA_VTXT
    FROM TKVS AS A
    LEFT JOIN TKVST AS B
      ON A~VERSI = B~VERSI
     AND B~SPRAS = @SY-LANGU
   WHERE A~VERSI = 'B1'.

  "__ 20191223 BSGSM_FCM ADD default cac
  SET PARAMETER ID 'CAC' FIELD PA_KOKRS.

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

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SELSCREEN
*&---------------------------------------------------------------------*
FORM SET_SELSCREEN .

  LOOP AT SCREEN.

    IF SCREEN-GROUP1 = 'MG1'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

  CASE ABAP_TRUE.

    WHEN PA_RAD1.

      LOOP AT SCREEN.

        IF SCREEN-GROUP1 = 'WBS'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.

      ENDLOOP.

    WHEN PA_RAD2.

      LOOP AT SCREEN.

        IF SCREEN-GROUP1 = 'KOS'.
          SCREEN-ACTIVE = 0.
          MODIFY SCREEN.
        ENDIF.

      ENDLOOP.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_CONTROLLING_AREA
*&---------------------------------------------------------------------*
FORM CHECK_CONTROLLING_AREA .

*  SELECT SINGLE BEZEI INTO @PA_KTXT
*    FROM TKA01
*   WHERE KOKRS = @PA_KOKRS.
*
*  IF SY-SUBRC <> 0.
*    SET CURSOR FIELD 'PA_KOKRS'.
*    MESSAGE E027  WITH TEXT-001.
*  ENDIF.
*
*  SELECT SINGLE VTEXT INTO @PA_VTXT
*    FROM TKVS AS A
*    LEFT JOIN TKVST AS B
*      ON A~VERSI = B~VERSI
*     AND B~SPRAS = @SY-LANGU
*   WHERE A~VERSI = @PA_VERSN.
*
*  IF SY-SUBRC <> 0.
*    SET CURSOR FIELD 'PA_VERSN'.
*    MESSAGE E027  WITH TEXT-002.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form INTIAL_VALUE_CHECK
*&---------------------------------------------------------------------*
FORM INTIAL_VALUE_CHECK .

  CASE ABAP_TRUE.

    WHEN PA_RAD1.

      IF PA_KOSTL IS INITIAL.
        SET CURSOR FIELD 'PA_KOSTL'.
        MESSAGE S026  WITH TEXT-C01 DISPLAY LIKE 'E'.
        STOP.
      ENDIF.

      SELECT SINGLE B~KTEXT INTO @GV_KTEXT
      FROM CSKS AS A
      LEFT JOIN CSKT AS B
        ON A~KOKRS = B~KOKRS
       AND A~KOSTL = B~KOSTL
       AND A~DATBI = B~DATBI
       AND B~SPRAS = @SY-LANGU
     WHERE A~KOKRS = @PA_KOKRS
       AND A~KOSTL = @PA_KOSTL.

      IF SY-SUBRC <> 0.
        SET CURSOR FIELD 'PA_KOSTL'.
        MESSAGE S027  WITH TEXT-C01 DISPLAY LIKE 'E'.
        STOP.
      ENDIF.

      PERFORM AUTHORITY_CHECK USING 'KOSTL'.

    WHEN PA_RAD2.

      IF PA_POSID IS INITIAL.
        SET CURSOR FIELD 'PA_POSID'.
        MESSAGE S026  WITH TEXT-C03 DISPLAY LIKE 'E'.
        STOP.
      ENDIF.

      SELECT SINGLE POST1 INTO @GV_POST1
       FROM PRPS
       WHERE POSID = @PA_POSID.

      IF SY-SUBRC <> 0.
        SET CURSOR FIELD 'PA_POSID'.
        MESSAGE S027  WITH TEXT-C03 DISPLAY LIKE 'E'.
        STOP.
      ENDIF.

      PERFORM AUTHORITY_CHECK USING 'WBS'.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
FORM SELECTED_DATA_RTN .

  DATA LV_MONTH     TYPE N LENGTH 2.
  DATA LV_FIELDNAME TYPE FIELDNAME.
  DATA LV_FYEAR     TYPE N LENGTH 7.

  RANGES R_KSTAR FOR CSKA-KSTAR.

  FIELD-SYMBOLS: <FS1>      TYPE ANY,
                 <FS_VALUE> TYPE ANY.

  CLEAR GV_OBJNR.

  RANGES R_OBJNR FOR ZCOT0040-ROBJNR.

  CLEAR: GT_OUTTAB, GT_OUTTAB[].

  CASE ABAP_TRUE.

    WHEN PA_RAD1.

      R_OBJNR-LOW    = 'KS' && PA_KOKRS && PA_KOSTL.
      R_OBJNR-SIGN   = 'I'.
      R_OBJNR-OPTION = 'EQ'.
      APPEND R_OBJNR.

      R_KSTAR-LOW    = '06*'.
      R_KSTAR-SIGN   = 'I'.
      R_KSTAR-OPTION = 'CP'.
      APPEND R_KSTAR.

    WHEN PA_RAD2.

      SELECT SINGLE OBJNR INTO @R_OBJNR-LOW
        FROM PRPS
       WHERE POSID = @PA_POSID.
      R_OBJNR-SIGN   = 'I'.
      R_OBJNR-OPTION = 'EQ'.
      APPEND R_OBJNR.

      R_KSTAR-LOW    = '06*'.
      R_KSTAR-SIGN   = 'I'.
      R_KSTAR-OPTION = 'CP'.
      APPEND R_KSTAR.

      R_KSTAR-LOW    = '05*'.
      R_KSTAR-SIGN   = 'I'.
      R_KSTAR-OPTION = 'CP'.
      APPEND R_KSTAR.

  ENDCASE.

  GV_OBJNR = R_OBJNR-LOW.

  SELECT RKSTAR,
         SUM( HSL01 ) AS HSL01, SUM( HSL02 ) AS HSL02,
         SUM( HSL03 ) AS HSL03, SUM( HSL04 ) AS HSL04,
         SUM( HSL05 ) AS HSL05, SUM( HSL06 ) AS HSL06,
         SUM( HSL07 ) AS HSL07, SUM( HSL08 ) AS HSL08,
         SUM( HSL09 ) AS HSL09, SUM( HSL10 ) AS HSL10,
         SUM( HSL11 ) AS HSL11, SUM( HSL12 ) AS HSL12
    INTO TABLE @DATA(LT_ZCOT0040)
    FROM ZCOT0040
   WHERE RLDNR  = '00'
     AND RRCTY  = '1'
     AND RVERS  = @PA_VERSN
     AND RYEAR  = @PA_GJAHR
     AND ROBJNR IN @R_OBJNR
   GROUP BY RKSTAR
   ORDER BY RKSTAR.

  SELECT A~KSTAR, B~KTEXT
    INTO TABLE @DATA(LT_CSKA)
    FROM CSKA AS A
    LEFT JOIN CSKU AS B
      ON A~KTOPL = B~KTOPL
     AND A~KSTAR = B~KSTAR
     AND B~SPRAS = @SY-LANGU
   WHERE A~KTOPL = @GC_KTOPL
     AND A~KSTAR IN @R_KSTAR
   ORDER BY A~KSTAR.

  LOOP AT LT_CSKA INTO DATA(LS_CSKA).

    READ TABLE LT_ZCOT0040 INTO DATA(LS_ZCOT0040)
                   WITH KEY RKSTAR = LS_CSKA-KSTAR
                   BINARY SEARCH.

    IF SY-SUBRC = 0.

      MOVE-CORRESPONDING LS_ZCOT0040 TO GT_OUTTAB.
      MOVE: GV_WAERS                 TO GT_OUTTAB-RTCUR,
            LS_CSKA-KTEXT            TO GT_OUTTAB-KTEXT.

      GT_OUTTAB-SUM = GT_OUTTAB-HSL01 + GT_OUTTAB-HSL02 +
                      GT_OUTTAB-HSL03 + GT_OUTTAB-HSL04 +
                      GT_OUTTAB-HSL05 + GT_OUTTAB-HSL06 +
                      GT_OUTTAB-HSL07 + GT_OUTTAB-HSL08 +
                      GT_OUTTAB-HSL09 + GT_OUTTAB-HSL10 +
                      GT_OUTTAB-HSL11 + GT_OUTTAB-HSL12.

    ELSE.

      MOVE: GV_WAERS                 TO GT_OUTTAB-RTCUR,
            '1'                      TO GT_OUTTAB-SORTKEY,
            LS_CSKA-KSTAR            TO GT_OUTTAB-RKSTAR,
            LS_CSKA-KTEXT            TO GT_OUTTAB-KTEXT.

    ENDIF.

    MOVE '@77@' TO GT_OUTTAB-ICON.

    APPEND GT_OUTTAB.
    CLEAR  GT_OUTTAB.

  ENDLOOP.

  SORT GT_OUTTAB BY SORTKEY RKSTAR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECKED_SAVED_DATA
*&---------------------------------------------------------------------*
FORM CHECKED_SAVED_DATA .

  CLEAR GV_EXIT.

  DATA LV_MONTH TYPE N LENGTH 2.
  DATA LV_FIELDNAME TYPE FIELDNAME.

  CALL METHOD GR_GRID1->CHECK_CHANGED_DATA.

  LOOP AT GT_OUTTAB.

    CLEAR LV_MONTH.
    CLEAR GT_OUTTAB-CHANGED.

    DO 12 TIMES.

      ADD 1 TO LV_MONTH.
      LV_FIELDNAME = 'SH' && LV_MONTH.

      ASSIGN COMPONENT LV_FIELDNAME OF STRUCTURE GT_OUTTAB
        TO FIELD-SYMBOL(<FS1>).

      IF <FS1> IS NOT INITIAL.
        GT_OUTTAB-CHANGED = ABAP_TRUE.
      ENDIF.

    ENDDO.

    MODIFY GT_OUTTAB.

  ENDLOOP.

  READ TABLE GT_OUTTAB WITH KEY CHANGED = ABAP_TRUE
             TRANSPORTING NO FIELDS.

  IF SY-SUBRC <> 0.
    MESSAGE S000 WITH TEXT-E01 DISPLAY LIKE 'E'.
    GV_EXIT = ABAP_TRUE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
FORM POPUP_TO_CONFIRM  USING PV_TITLE
                            PV_QUEST.

  "-- call popup
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR       = PV_TITLE                "TEXT-PT1
      TEXT_QUESTION  = PV_QUEST                "TEXT-QT1
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

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_DATA_RTN
*&---------------------------------------------------------------------*
FORM SAVE_DATA_RTN .

  DATA: LV_MESSAGE TYPE STRING.

  FIELD-SYMBOLS: <FS1> TYPE ANY,
                 <FS2> TYPE ANY.

  DATA LV_VALID TYPE C.

  DATA LS_HEADERINFO  LIKE  BAPIPLNHDR.
  DATA LS_RETURN TYPE BAPIRET2.

  DATA LV_FNAME TYPE FIELDNAME.
  DATA LV_MONTH TYPE N LENGTH 2.

  DATA : LT_INDEXSTRUCTURE LIKE BAPIACPSTRU OCCURS 0 WITH HEADER LINE,
         LT_COOBJECT       LIKE BAPIPCPOBJ  OCCURS 0 WITH HEADER LINE,
         LT_PERVALUE       LIKE BAPIPCPVAL  OCCURS 0 WITH HEADER LINE.

  DATA LT_ZCOT0040 TYPE TABLE OF ZCOT0040 WITH HEADER LINE.

  DATA LV_BELNR TYPE BELNR_D.
  DATA LV_SEQ   TYPE ZERSEQ.

  LOOP AT GT_OUTTAB WHERE CHANGED = ABAP_TRUE.

    CLEAR: GT_RETURN, GT_RETURN[].

    CLEAR: LT_ZCOT0040, LT_ZCOT0040[].
    CLEAR  LV_BELNR.
    CLEAR  LV_VALID.
    CLEAR  LV_SEQ.

    CLEAR: LS_HEADERINFO, LT_INDEXSTRUCTURE,
                          LT_INDEXSTRUCTURE[],
                          LT_COOBJECT,
                          LT_COOBJECT[],
                          LT_PERVALUE,
                          LT_PERVALUE[].

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        NR_RANGE_NR             = '01'
        OBJECT                  = 'ZCO_DOCNR'
        TOYEAR                  = PA_GJAHR
      IMPORTING
        NUMBER                  = LV_BELNR
      EXCEPTIONS
        INTERVAL_NOT_FOUND      = 1
        NUMBER_RANGE_NOT_INTERN = 2
        OBJECT_NOT_FOUND        = 3
        QUANTITY_IS_0           = 4
        QUANTITY_IS_NOT_1       = 5
        INTERVAL_OVERFLOW       = 6
        BUFFER_OVERFLOW         = 7
        OTHERS                  = 8.

    IF SY-SUBRC <> 0.
      MESSAGE S001 WITH TEXT-E03 INTO GT_OUTTAB-MESSAGE.
      MOVE '@0A@' TO GT_OUTTAB-ICON.
      MODIFY GT_OUTTAB.
      CONTINUE.
    ENDIF.

    _CONVERSION_IN LV_BELNR.

*-- Header Data
    LS_HEADERINFO-CO_AREA     = PA_KOKRS.     "관리 회계영역
    LS_HEADERINFO-FISC_YEAR   = PA_GJAHR.     "회계연도
    LS_HEADERINFO-PERIOD_FROM = '001'.        "기간 시작
    LS_HEADERINFO-PERIOD_TO   = '012'.        "기간 종료
    LS_HEADERINFO-VERSION     = PA_VERSN.     "버전

*-- 전표 헤더 텍스트
    LS_HEADERINFO-PLAN_CURRTYPE = 'C'. "통화

*-- CO-계획: 액티비티투입 & 주요지표 계획 BAPIs
    LT_INDEXSTRUCTURE-OBJECT_INDEX = 1.
    LT_INDEXSTRUCTURE-VALUE_INDEX  = 1.
    APPEND LT_INDEXSTRUCTURE.

*-- CO 계획: 1차 원가 BAPI에 대한 오브젝트
    LT_COOBJECT-OBJECT_INDEX  = 1.

    CASE ABAP_TRUE.
      WHEN PA_RAD1.
        LT_COOBJECT-COSTCENTER    = PA_KOSTL.
      WHEN PA_RAD2.
        LT_COOBJECT-WBS_ELEMENT   = PA_POSID.
    ENDCASE.

    APPEND LT_COOBJECT.

*-- CO 계획: 1차 원가 BAPI에 대한 값
    LT_PERVALUE-VALUE_INDEX  = 1.
    LT_PERVALUE-COST_ELEM    = GT_OUTTAB-RKSTAR.   "원가요소
    LT_PERVALUE-TRANS_CURR   = GV_WAERS.

    CLEAR LV_MONTH.

    DO 12 TIMES.

      ADD 1 TO LV_MONTH.

      LV_FNAME = 'SH' && LV_MONTH.

      ASSIGN COMPONENT LV_FNAME OF STRUCTURE GT_OUTTAB
         TO <FS1>.

      IF <FS1> IS INITIAL.
        CONTINUE.
      ELSE.
        LV_VALID = ABAP_TRUE.
      ENDIF.

      LV_FNAME = 'LT_PERVALUE-FIX_VAL_PER' && LV_MONTH.

      ASSIGN (LV_FNAME) TO <FS2>.

      IF GV_WAERS = 'KRW'.
        <FS2> = <FS1>  * 100.
      ELSE.
        <FS2> = <FS1>.
      ENDIF.

*-- CBO 저장 데이터
      MOVE: '00'             TO LT_ZCOT0040-RLDNR,
            '1'              TO LT_ZCOT0040-RRCTY,
            PA_VERSN         TO LT_ZCOT0040-RVERS,
            PA_GJAHR         TO LT_ZCOT0040-RYEAR,
            GV_OBJNR         TO LT_ZCOT0040-ROBJNR,
            LV_BELNR         TO LT_ZCOT0040-RDOCNR,
            'BT01'           TO LT_ZCOT0040-BUDTYPE_9,
            PA_KOKRS         TO LT_ZCOT0040-RKOKRS,
            SY-UZEIT         TO LT_ZCOT0040-CPUTM,
            SY-DATUM         TO LT_ZCOT0040-CPUDT,
            SY-UNAME         TO LT_ZCOT0040-USNAM,
            GV_WAERS         TO LT_ZCOT0040-RTCUR,
            GT_OUTTAB-RKSTAR TO LT_ZCOT0040-RKSTAR.

      LV_FNAME = 'HSL' && LV_MONTH.

      ASSIGN COMPONENT LV_FNAME OF STRUCTURE LT_ZCOT0040
         TO <FS2>.

      MOVE <FS1> TO <FS2>.

      IF <FS1> > 0.

        MOVE: 'SUPL' TO LT_ZCOT0040-PROCESS_9.   "증액

      ELSEIF <FS1> < 0.

        MOVE: 'RENT' TO LT_ZCOT0040-PROCESS_9.   "감액

      ENDIF.

      COLLECT LT_ZCOT0040.
      CLEAR   LT_ZCOT0040.

    ENDDO.

    APPEND LT_PERVALUE.

    IF LV_VALID IS INITIAL.

      MOVE '@09@'   TO GT_OUTTAB-ICON.
      MOVE TEXT-I01 TO GT_OUTTAB-MESSAGE.
      MODIFY GT_OUTTAB.
      CONTINUE.

    ENDIF.

    LOOP AT LT_ZCOT0040.
      ADD 1 TO LV_SEQ.
      MOVE LV_SEQ TO LT_ZCOT0040-RSEQ.
      MODIFY LT_ZCOT0040.
    ENDLOOP.

    CALL FUNCTION 'BAPI_COSTACTPLN_POSTPRIMCOST'
      EXPORTING
        HEADERINFO     = LS_HEADERINFO
        DELTA          = ABAP_TRUE
      TABLES
        INDEXSTRUCTURE = LT_INDEXSTRUCTURE
        COOBJECT       = LT_COOBJECT
        PERVALUE       = LT_PERVALUE
        RETURN         = GT_RETURN.

    READ TABLE GT_RETURN WITH KEY TYPE = 'E'.

    IF SY-SUBRC EQ 0 .

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      PERFORM BUILD_MESSAGE USING    GT_RETURN
                            CHANGING GT_OUTTAB-MESSAGE.
      MOVE '@0A@' TO GT_OUTTAB-ICON.

    ELSE.

      TRY .

          INSERT  ZCOT0040 FROM TABLE LT_ZCOT0040.
          COMMIT WORK.

          MOVE '@08@' TO GT_OUTTAB-ICON.

          GT_OUTTAB-SUM = GT_OUTTAB-SUM + GT_OUTTAB-SUMSH.
          CLEAR GT_OUTTAB-SUMSH.
          CLEAR LV_MONTH.

          DO 12 TIMES.

            ADD 1 TO LV_MONTH.

            LV_FNAME = 'SH' && LV_MONTH.

            ASSIGN COMPONENT LV_FNAME OF STRUCTURE GT_OUTTAB
               TO <FS1>.

            LV_FNAME = 'HSL' && LV_MONTH.

            ASSIGN COMPONENT LV_FNAME OF STRUCTURE GT_OUTTAB
               TO <FS2>.

            <FS2> = <FS2> + <FS1>.

            CLEAR <FS1>.

          ENDDO.

        CATCH CX_SY_SQL_ERROR INTO DATA(LR_ERROR).

          ROLLBACK WORK.

          LV_MESSAGE = LR_ERROR->GET_TEXT( ).
          MESSAGE S001 WITH LV_MESSAGE INTO GT_OUTTAB-MESSAGE.
          MOVE '@0A@' TO GT_OUTTAB-ICON.

      ENDTRY.

    ENDIF.

    MODIFY GT_OUTTAB.

  ENDLOOP.

  MESSAGE S000 WITH TEXT-S01.

  PERFORM REFRESH_GRID_0100.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM AUTHORITY_CHECK USING  VALUE(P_VALUE).

  DATA LV_TYPE TYPE C.
  DATA LV_MESSAGE TYPE BAPI_MSG.

  DATA: LT_0070  LIKE TABLE OF ZCAS0070,
        LS_0070  LIKE ZCAS0070,
        LV_CLASS TYPE ZCAT0031-CD_CLASS,
        LV_CODE  TYPE ZCAT0031-CD_CODE.

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

**  CHECK GV_MODE = 'S'. 처리자 강현수 21.04.08

  CASE P_VALUE.

    WHEN 'KOSTL'.

      CALL FUNCTION 'ZCA_PRG_AUTH_CHECK'
        EXPORTING
          I_MODULE   = 'CO'
          I_KOSTL_CO = PA_KOSTL
        IMPORTING
          E_TYPE     = LV_TYPE
          E_MESSAGE  = LV_MESSAGE.

      CHECK LV_TYPE = 'E'.
      MESSAGE S000 WITH LV_MESSAGE DISPLAY LIKE 'E'.
      STOP.

    WHEN 'WBS'.

      CALL FUNCTION 'ZCA_PRG_AUTH_CHECK'
        EXPORTING
          I_MODULE   = 'CO'
          I_POSID_CO = PA_POSID
        IMPORTING
          E_TYPE     = LV_TYPE
          E_MESSAGE  = LV_MESSAGE.

      CHECK LV_TYPE = 'E'.
      MESSAGE S000 WITH LV_MESSAGE DISPLAY LIKE 'E'.
      STOP.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_INSTANCE_0100
*&---------------------------------------------------------------------*
FORM CREATE_INSTANCE_0100 .

  CREATE OBJECT GR_SPLITTER1
    EXPORTING
      ROWS    = 2
      COLUMNS = 1
      PARENT  = CL_GUI_SPLITTER_CONTAINER=>SCREEN0.

  GR_PARENT_HTML = GR_SPLITTER1->GET_CONTAINER(
      ROW       = 1
      COLUMN    = 1 ).

  GR_DATA_CONTAINER = GR_SPLITTER1->GET_CONTAINER(
      ROW       = 2
      COLUMN    = 1 ).

  CALL METHOD GR_SPLITTER1->SET_ROW_HEIGHT
    EXPORTING
      ID     = 1
      HEIGHT = 8.

  CALL METHOD GR_SPLITTER1->SET_ROW_HEIGHT
    EXPORTING
      ID     = 2
      HEIGHT = 50.

  CREATE OBJECT GR_GRID1
    EXPORTING
      I_PARENT = GR_DATA_CONTAINER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form INIT_LAYOUT_0100
*&---------------------------------------------------------------------*
FORM INIT_LAYOUT_0100 .

  CLEAR GS_LAYOUT.

  GS_LAYOUT-ZEBRA      = ABAP_TRUE.
  GS_LAYOUT-SEL_MODE   = SPACE.     "B:단일,C:복수,D:셀,A:행/열
  GS_LAYOUT-BOX_FNAME  = SPACE.
  GS_LAYOUT-NO_ROWMARK = SPACE.
  GS_LAYOUT-TOTALS_BEF = ABAP_TRUE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_GRID_EXCLUDE_0100
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

*
  _SET_EX:
*   CL_GUI_ALV_GRID=>MC_FC_FIND,
*
*    "-- begin 기능버튼활성화시 제외
*    CL_GUI_ALV_GRID=>MC_FC_SORT_ASC,
*    CL_GUI_ALV_GRID=>MC_FC_SORT_DSC,
*    CL_GUI_ALV_GRID=>MC_MB_SUBTOT,
*    CL_GUI_ALV_GRID=>MC_MB_SUM,
*    "-- end
*
    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
    CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW,
*
*    "-- begin 기능버튼활성화
    CL_GUI_ALV_GRID=>MC_FC_LOC_COPY,
    CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,
    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE,
    CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
*    "-- end
*
**    CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO,
**    CL_GUI_ALV_GRID=>MC_FC_CHECK,
**
***   CL_GUI_ALV_GRID=>MC_FC_DETAIL,
***   CL_GUI_ALV_GRID=>MC_FC_FILTER,
**    CL_GUI_ALV_GRID=>MC_FC_GRAPH,
**    CL_GUI_ALV_GRID=>MC_FC_HTML,
**    CL_GUI_ALV_GRID=>MC_FC_INFO,
**    CL_GUI_ALV_GRID=>MC_FC_REFRESH,
**
***   CL_GUI_ALV_GRID=>MC_FC_VIEWS,
***   CL_GUI_ALV_GRID=>MC_FC_LOAD_VARIANT,
***   CL_GUI_ALV_GRID=>MC_FC_PRINT,
***   CL_GUI_ALV_GRID=>MC_MB_VARIANT,
***   CL_GUI_ALV_GRID=>MC_MB_EXPORT,
**
**    CL_GUI_ALV_GRID=>MC_FC_VIEW_CRYSTAL,
**    CL_GUI_ALV_GRID=>MC_FC_VIEW_EXCEL,
**    CL_GUI_ALV_GRID=>MC_FC_VIEW_GRID,
**    CL_GUI_ALV_GRID=>MC_FC_VIEW_LOTUS,
**    CL_GUI_ALV_GRID=>MC_FC_EXPCRDATA,
**    CL_GUI_ALV_GRID=>MC_FC_EXPCRDESIG,
**    CL_GUI_ALV_GRID=>MC_FC_EXPCRTEMPL,
**    CL_GUI_ALV_GRID=>MC_FC_CALL_ABC,
**    CL_GUI_ALV_GRID=>MC_FC_CALL_CRBATCH.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_FIELDCAT_0100
*&---------------------------------------------------------------------*
FORM APPEND_FIELDCAT_0100 .

  PERFORM GET_FIELDCATLOG_DATA.
  PERFORM MODIFY_FIELDCATLOG_DATA.

ENDFORM.
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
*& Form GET_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
FORM GET_FIELDCATLOG_DATA .

  DATA: LT_FIELDCAT TYPE KKBLO_T_FIELDCAT.

  CALL FUNCTION 'K_KKB_FIELDCAT_MERGE'
    EXPORTING
      I_CALLBACK_PROGRAM     = SY-REPID
      I_STRUCNAME            = 'ZCOS0380'
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

    MESSAGE E014.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MODIFY_FIELDCATLOG_DATA
*&---------------------------------------------------------------------*
FORM MODIFY_FIELDCATLOG_DATA .

  DATA:  LV_TEXT(50).

  LOOP AT GT_FIELDCAT INTO GS_FIELDCAT.

    CLEAR: LV_TEXT.

    CASE GS_FIELDCAT-FIELDNAME.

      WHEN 'SORTKEY' OR 'CHANGED'.
        GS_FIELDCAT-NO_OUT = ABAP_TRUE.

      WHEN 'ICON'.
        GS_FIELDCAT-ICON = ABAP_TRUE.
        LV_TEXT = TEXT-C10.
        GS_FIELDCAT-KEY       = ABAP_TRUE.

      WHEN 'RKSTAR'.
        LV_TEXT = TEXT-C04.
        GS_FIELDCAT-OUTPUTLEN = '10'.
        GS_FIELDCAT-KEY       = ABAP_TRUE.

      WHEN 'KTEXT'.
        LV_TEXT = TEXT-C05.
        GS_FIELDCAT-OUTPUTLEN = '25'.
        GS_FIELDCAT-KEY       = ABAP_TRUE.

      WHEN 'SUM'.
        LV_TEXT = TEXT-C06.
        GS_FIELDCAT-OUTPUTLEN = '15'.
        GS_FIELDCAT-DO_SUM    = ABAP_TRUE.
        GS_FIELDCAT-NO_ZERO   = ABAP_TRUE.

      WHEN 'SUMSH'.
        LV_TEXT = TEXT-C07.
        GS_FIELDCAT-OUTPUTLEN = '15'.
        GS_FIELDCAT-DO_SUM    = ABAP_TRUE.
        GS_FIELDCAT-NO_ZERO   = ABAP_TRUE.

      WHEN 'MESSAGE'.
        LV_TEXT = TEXT-C11.
        GS_FIELDCAT-OUTPUTLEN = '100'.

      WHEN OTHERS.

    ENDCASE.

    IF GS_FIELDCAT-FIELDNAME CP 'HSL*'.

      LV_TEXT = GS_FIELDCAT-FIELDNAME+3(2) && TEXT-C08.
      GS_FIELDCAT-OUTPUTLEN = '15'.
      GS_FIELDCAT-DO_SUM    = ABAP_TRUE.
      GS_FIELDCAT-EMPHASIZE  = 'C500'.
      GS_FIELDCAT-NO_ZERO   = ABAP_TRUE.

    ELSEIF GS_FIELDCAT-FIELDNAME CP 'SH*'.

      LV_TEXT = GS_FIELDCAT-FIELDNAME+2(2) && TEXT-C09.
      GS_FIELDCAT-EDIT = ABAP_TRUE.
      GS_FIELDCAT-OUTPUTLEN = '15'.
      GS_FIELDCAT-DO_SUM    = ABAP_TRUE.
      GS_FIELDCAT-NO_ZERO   = ABAP_TRUE.

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
*& Form MAKE_TOP_OF_PAGE_DATA_0100
*&---------------------------------------------------------------------*
FORM MAKE_TOP_OF_PAGE_DATA_0100 .

  DATA: LT_TEXTS TYPE SDYDO_TEXT_TABLE,
        LV_TEXT  TYPE SDYDO_TEXT_ELEMENT.

  DATA LV_KOSTL TYPE KOSTL.
  DATA LV_POSID TYPE PS_POSID.

  CONCATENATE TEXT-001 ':' PA_KOKRS
        INTO LV_TEXT SEPARATED BY SPACE.

  CALL METHOD GR_TOP_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
      SAP_EMPHASIS = CL_DD_AREA=>KEY
      STYLE_CLASS  = SPACE.

  CALL METHOD GR_TOP_DOCUMENT->NEW_LINE.

  CONCATENATE TEXT-C02 ':' PA_GJAHR
        INTO LV_TEXT SEPARATED BY SPACE.

  CALL METHOD GR_TOP_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
      SAP_EMPHASIS = CL_DD_AREA=>KEY
      STYLE_CLASS  = SPACE.

  CALL METHOD GR_TOP_DOCUMENT->NEW_LINE.

  CONCATENATE TEXT-002 ':' PA_VERSN  PA_VTXT
        INTO LV_TEXT SEPARATED BY SPACE.

  CALL METHOD GR_TOP_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
      SAP_EMPHASIS = CL_DD_AREA=>KEY
      STYLE_CLASS  = SPACE.

  CASE 'X'.

    WHEN PA_RAD1.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = PA_KOSTL
        IMPORTING
          OUTPUT = LV_KOSTL.

      CONCATENATE TEXT-C01 ':' LV_KOSTL  GV_KTEXT
            INTO LV_TEXT SEPARATED BY SPACE.

    WHEN PA_RAD2.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
        EXPORTING
          INPUT  = PA_POSID
        IMPORTING
          OUTPUT = LV_POSID.

      CONCATENATE TEXT-C03 ':' LV_POSID  GV_POST1
            INTO LV_TEXT SEPARATED BY SPACE.

  ENDCASE.

  CALL METHOD GR_TOP_DOCUMENT->NEW_LINE.

  CALL METHOD GR_TOP_DOCUMENT->ADD_TEXT
    EXPORTING
      TEXT         = LV_TEXT
      SAP_COLOR    = CL_DD_DOCUMENT=>LIST_HEADING_INT
      SAP_EMPHASIS = CL_DD_AREA=>KEY
      STYLE_CLASS  = SPACE.

  CALL METHOD GR_TOP_DOCUMENT->NEW_LINE
    EXPORTING
      REPEAT = 1.

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
*& Form DISPLAY_ALV_GRID_0100
*&---------------------------------------------------------------------*
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
      IT_OUTTAB                     = GT_OUTTAB[]
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3.

  IF SY-SUBRC NE 0.
    MESSAGE E000(0K) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  DATA(LV_LINES) = LINES( GT_OUTTAB ).
  MESSAGE S039 WITH LV_LINES.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_GRID_0100
*&---------------------------------------------------------------------*
FORM REFRESH_GRID_0100 .

  GS_STABLE-ROW = ABAP_TRUE. "Row
  GS_STABLE-COL = ABAP_TRUE. "column

  CALL METHOD GR_GRID1->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = GS_STABLE
      I_SOFT_REFRESH = SPACE.

  CALL METHOD CL_GUI_CFW=>FLUSH.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_TOOLBAR
*&---------------------------------------------------------------------*
FORM EVENT_TOOLBAR  USING    P_E_OBJECT
                             P_E_INTERACTIVE
                             P_SENDER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_USER_COMMAND
*&---------------------------------------------------------------------*
FORM EVENT_USER_COMMAND  USING    P_E_UCOMM
                                  P_SENDER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_DATA_CHANGED
*&---------------------------------------------------------------------*
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
  DATA: LS_MOD_CELLS TYPE LVC_S_MODI,
        LS_INS_CELLS TYPE LVC_S_MOCE,
        LS_DEL_CELLS TYPE LVC_S_MOCE.

  DATA LV_MONTH TYPE N LENGTH 2.
  DATA LV_FIELDNAME TYPE FIELDNAME.

  DATA LV_TABIX TYPE SY-TABIX.
  DATA LV_VALUE TYPE HSLXX9_CS.
  DATA LV_SUM   TYPE HSLXX9_CS.

  DEFINE _MODIFY_CELL.
    CALL METHOD pr_data_changed->modify_cell
      EXPORTING
        i_fieldname = &1
        i_row_id    = &2
        i_value     = &3.

  END-OF-DEFINITION.

  DEFINE _GET_CELL_VALUE.
    CALL METHOD pr_data_changed->get_cell_value
      EXPORTING
        i_fieldname = &1
        i_row_id    = &2
      IMPORTING
        e_value     = &3.
  END-OF-DEFINITION.

  DEFINE _ADD_PROTOCOL.
    CALL METHOD pr_data_changed->add_protocol_entry
      EXPORTING
        i_fieldname = &1
        i_row_id    = &2
        i_msgid     = 'ZCO01'
        i_msgty     = &3
        i_msgno     = &4
        i_msgv1     = &5
        i_msgv2     = &6
        i_msgv3     = &7
        i_msgv4     = &8.
  END-OF-DEFINITION.

  DEFINE _MODIFY_STYLE.
    CALL METHOD pr_data_changed->modify_style
      EXPORTING
        i_fieldname = &1
        i_row_id    = &2
        i_style     = &3.
  END-OF-DEFINITION.
*--- End of Example

  CASE PR_SENDER.

    WHEN GR_GRID1.

      LOOP AT PR_DATA_CHANGED->MT_MOD_CELLS INTO LS_MOD_CELLS.

        IF LS_MOD_CELLS-FIELDNAME CP 'SH*'.

          CLEAR LV_MONTH.

          DO 12 TIMES.

            ADD 1 TO LV_MONTH.

            LV_FIELDNAME = 'SH' && LV_MONTH.

            _GET_CELL_VALUE LV_FIELDNAME LS_MOD_CELLS-ROW_ID
                                         LV_VALUE.

            LV_SUM = LV_SUM + LV_VALUE.

          ENDDO.

          _MODIFY_CELL 'SUMSH'   LS_MOD_CELLS-ROW_ID LV_SUM.

        ENDIF.

      ENDLOOP.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
FORM EVENT_DATA_CHANGED_FINISHED  USING    P_E_MODIFIED
                                           P_ET_GOOD_CELLS
                                           P_SENDER.
  IF P_E_MODIFIED IS NOT INITIAL.
    PERFORM REFRESH_GRID_0100.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM EVENT_HOTSPOT_CLICK  USING    P_E_ROW_ID
                                   P_E_COLUMN_ID
                                   P_ES_ROW_NO
                                   P_SENDER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM EVENT_DOUBLE_CLICK  USING    P_E_ROW
                                  P_E_COLUMN
                                  P_ES_ROW_NO
                                  P_SENDER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_HELP_ON_F4
*&---------------------------------------------------------------------*
FORM EVENT_HELP_ON_F4  USING    P_E_FIELDNAME
                                P_E_FIELDVALUE
                                P_ES_ROW_NO
                                P_ER_EVENT_DATA
                                P_ET_BAD_CELLS
                                P_E_DISPLAY
                                P_SENDER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM EVENT_TOP_OF_PAGE  USING    P_E_DYNDOC_ID
                                 P_TABLE_INDEX
                                 P_SENDER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVENT_END_OF_LIST
*&---------------------------------------------------------------------*
FORM EVENT_END_OF_LIST  USING    P_E_DYNDOC_ID
                                 P_SENDER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REGIST_ALV_EVENT_0100
*&---------------------------------------------------------------------*
FORM REGIST_ALV_EVENT_0100 USING PR_GRID TYPE REF TO CL_GUI_ALV_GRID.

* REGISTER EVENT
  CALL METHOD PR_GRID->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CALL METHOD PR_GRID->SET_READY_FOR_INPUT
    EXPORTING
      I_READY_FOR_INPUT = 1.

*-- GR_EVENT_RECEIVER
  IF GR_EVENT_RECEIVER IS INITIAL.
    CREATE OBJECT GR_EVENT_RECEIVER.
  ENDIF.

* Handler Event
  SET HANDLER:
    GR_EVENT_RECEIVER->HANDLE_TOOLBAR       FOR ALL INSTANCES,
    GR_EVENT_RECEIVER->HANDLE_DATA_CHANGED  FOR ALL INSTANCES,
    GR_EVENT_RECEIVER->HANDLE_DATA_CHANGED_FINISHED
      FOR ALL INSTANCES,
    GR_EVENT_RECEIVER->HANDLE_USER_COMMAND  FOR ALL INSTANCES,
    GR_EVENT_RECEIVER->HANDLE_HOTSPOT_CLICK FOR ALL INSTANCES,
    GR_EVENT_RECEIVER->HANDLE_DOUBLE_CLICK  FOR ALL INSTANCES.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_MESSAGE
*&---------------------------------------------------------------------*
FORM BUILD_MESSAGE USING    PS_MESSAGE STRUCTURE BAPIRET2
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
*& Form ENQUE_LOCK
*&---------------------------------------------------------------------*
FORM ENQUE_LOCK .

  DATA LS_RSTABLE TYPE RSTABLE.

  LS_RSTABLE-TABNAME = 'ZCOT0040'.
  LS_RSTABLE-VARKEY  = SY-MANDT && PA_VERSN && PA_GJAHR.

  CALL FUNCTION 'ENQUEUE_E_TABLE'
    EXPORTING
      MODE_RSTABLE   = 'E'
      TABNAME        = LS_RSTABLE-TABNAME
      VARKEY         = LS_RSTABLE-VARKEY
    EXCEPTIONS
      FOREIGN_LOCK   = 1
      SYSTEM_FAILURE = 2
      OTHERS         = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
        DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DEQUE_LOCK
*&---------------------------------------------------------------------*
FORM DEQUE_LOCK .

  DATA LS_RSTABLE TYPE RSTABLE.

  LS_RSTABLE-TABNAME = 'ZCOT0040'.
  LS_RSTABLE-VARKEY  = SY-MANDT && PA_VERSN && PA_GJAHR.

  CALL FUNCTION 'DEQUEUE_E_TABLE'
    EXPORTING
      MODE_RSTABLE   = 'E'
      TABNAME        = LS_RSTABLE-TABNAME
      VARKEY         = LS_RSTABLE-VARKEY
    EXCEPTIONS
      FOREIGN_LOCK   = 1
      SYSTEM_FAILURE = 2
      OTHERS         = 3.

ENDFORM.
