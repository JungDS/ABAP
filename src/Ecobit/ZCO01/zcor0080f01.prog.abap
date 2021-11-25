*&---------------------------------------------------------------------*
*& Include          ZCOR0080F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
FORM SELECTED_DATA_RTN .

  PERFORM SELECTED_MAIN_DATA.

ENDFORM.                    " SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
*&      Form  SELECTED_MAIN_DATA
*&---------------------------------------------------------------------*
FORM SELECTED_MAIN_DATA.

  DATA LV_MONTH     TYPE N LENGTH 2.
  DATA LV_FIELDNAME TYPE FIELDNAME.
  DATA LV_FYEAR     TYPE N LENGTH 7.

  FIELD-SYMBOLS: <FS1>      TYPE ANY,
                 <FS_VALUE> TYPE ANY.

  CLEAR GV_OBJNR.

  RANGES R_OBJNR FOR ZCOT0040-ROBJNR.

  CLEAR: GV_FSUM, GV_ASUM, GV_ISUM, GV_TSUM, GV_WAERS,
         GV_PSUM.

  CLEAR: GT_OUTTAB, GT_OUTTAB[].

  SELECT SINGLE WAERS INTO @GV_WAERS
    FROM TKA01
   WHERE KOKRS = @PA_KOKRS.

  MOVE: PA_KOKRS TO ZCOS0040-KOKRS,
        PA_KTXT  TO ZCOS0040-BEZEI,
        PA_GJAHR TO ZCOS0040-GJAHR,
        PA_VERSN TO ZCOS0040-VERSN,
        PA_VTXT  TO ZCOS0040-VTEXT,
        PA_KSTAR TO ZCOS0040-KSTAR,
        PA_KOSTL TO ZCOS0040-KOSTL,
        PA_POSID TO ZCOS0040-POSID.

  CASE ABAP_TRUE.

    WHEN PA_RAD1.

      R_OBJNR-LOW    = 'KS' && PA_KOKRS && PA_KOSTL.
      R_OBJNR-SIGN   = 'I'.
      R_OBJNR-OPTION = 'EQ'.
      APPEND R_OBJNR.

    WHEN PA_RAD2.

      SELECT SINGLE OBJNR INTO @R_OBJNR-LOW
        FROM PRPS
       WHERE POSID = @PA_POSID.
      R_OBJNR-SIGN   = 'I'.
      R_OBJNR-OPTION = 'EQ'.
      APPEND R_OBJNR.

  ENDCASE.

  GV_OBJNR = R_OBJNR-LOW.

*-- 반영전
  SELECT RLDNR, RRCTY, RVERS, RYEAR,
         ROBJNR, RKSTAR,
         SUM( HSL01 ) AS HSL01, SUM( HSL02 ) AS HSL02,
         SUM( HSL03 ) AS HSL03, SUM( HSL04 ) AS HSL04,
         SUM( HSL05 ) AS HSL05, SUM( HSL06 ) AS HSL06,
         SUM( HSL07 ) AS HSL07, SUM( HSL08 ) AS HSL08,
         SUM( HSL09 ) AS HSL09, SUM( HSL10 ) AS HSL10,
         SUM( HSL11 ) AS HSL11, SUM( HSL12 ) AS HSL12
    INTO TABLE @DATA(LT_ZCOT0040)
    FROM ZCOT0040
   WHERE RLDNR = '00'
     AND RRCTY = '1'
     AND RVERS = @PA_VERSN
     AND RYEAR = @PA_GJAHR
     AND RKSTAR = @PA_KSTAR
     AND ROBJNR IN @R_OBJNR
   GROUP BY RLDNR, RRCTY, RVERS, RYEAR,
         ROBJNR, RKSTAR.

*-- 실적
  SELECT GJAHR, RACCT, OBJNR, FISCYEARPER,
         SUM( HSL ) AS HSL
    INTO TABLE @DATA(LT_ACDOCA)
    FROM ACDOCA
   WHERE RLDNR = '0L'
     AND GJAHR = @PA_GJAHR
     AND KOKRS = @PA_KOKRS
     AND RACCT = @PA_KSTAR
     AND OBJNR IN @R_OBJNR
     AND BLART IN ('DD', 'SS')
   GROUP BY GJAHR, RACCT, OBJNR ,FISCYEARPER.

  SELECT  OBJNR, KSTAR,
         SUM( WKG001 ) AS WKG001, SUM( WKG002 ) AS WKG002,
         SUM( WKG003 ) AS WKG003, SUM( WKG004 ) AS WKG004,
         SUM( WKG005 ) AS WKG005, SUM( WKG006 ) AS WKG006,
         SUM( WKG007 ) AS WKG007, SUM( WKG008 ) AS WKG008,
         SUM( WKG009 ) AS WKG009, SUM( WKG010 ) AS WKG010,
         SUM( WKG011 ) AS WKG011, SUM( WKG012 ) AS WKG012
    INTO TABLE @DATA(LT_COSP)
    FROM COSP
   WHERE LEDNR = '00'
     AND VERSN = '000'
     AND WRTTP IN ('04', '60', '21', '22')
     AND GJAHR = @PA_GJAHR
     AND KSTAR = @PA_KSTAR
     AND OBJNR IN @R_OBJNR
     AND VRGNG <> 'SDOR'
   GROUP BY OBJNR, KSTAR.

  SELECT  OBJNR, KSTAR,
         SUM( WKG001 ) AS WKG001, SUM( WKG002 ) AS WKG002,
         SUM( WKG003 ) AS WKG003, SUM( WKG004 ) AS WKG004,
         SUM( WKG005 ) AS WKG005, SUM( WKG006 ) AS WKG006,
         SUM( WKG007 ) AS WKG007, SUM( WKG008 ) AS WKG008,
         SUM( WKG009 ) AS WKG009, SUM( WKG010 ) AS WKG010,
         SUM( WKG011 ) AS WKG011, SUM( WKG012 ) AS WKG012
    INTO TABLE @DATA(LT_COSP_RKU)
    FROM COSP
   WHERE LEDNR = '00'
     AND VERSN = '000'
     AND WRTTP IN ('04', '60', '21', '22')
     AND GJAHR = @PA_GJAHR
     AND KSTAR = @PA_KSTAR
     AND OBJNR IN @R_OBJNR
     AND VRGNG = 'RKU1'
   GROUP BY OBJNR, KSTAR.

*--
  READ TABLE LT_ZCOT0040 ASSIGNING FIELD-SYMBOL(<LS_ZCOT0040>)
        INDEX 1.
  READ TABLE LT_COSP     ASSIGNING FIELD-SYMBOL(<LS_COSP>)
        INDEX 1.
  READ TABLE LT_COSP_RKU  ASSIGNING FIELD-SYMBOL(<LS_COSP_RKU>)
        INDEX 1.

  DO 12 TIMES.

    ADD 1 TO LV_MONTH.

*-- 계획
    LV_FIELDNAME = 'HSL' && LV_MONTH.

    IF <LS_ZCOT0040> IS ASSIGNED.
      ASSIGN COMPONENT LV_FIELDNAME OF STRUCTURE <LS_ZCOT0040>
         TO <FS1>.
      MOVE <FS1> TO GT_OUTTAB-FHSTL.
    ENDIF.

    UNASSIGN <FS1>.

*-- 실적
    LV_FIELDNAME = 'WKG0' && LV_MONTH.
    LV_FYEAR     = PA_GJAHR && '0' && LV_MONTH.

    IF <LS_COSP> IS ASSIGNED.
      ASSIGN COMPONENT LV_FIELDNAME OF STRUCTURE <LS_COSP>
         TO <FS1>.
      GT_OUTTAB-AHSTL = <FS1>.
    ENDIF.

    IF <LS_COSP_RKU> IS ASSIGNED.
      ASSIGN COMPONENT LV_FIELDNAME OF STRUCTURE <LS_COSP_RKU>
         TO <FS1>.
      GT_OUTTAB-AHSTL = GT_OUTTAB-AHSTL - <FS1>.
    ENDIF.

    READ TABLE LT_ACDOCA ASSIGNING FIELD-SYMBOL(<LS_ACDOCA>)
                WITH KEY FISCYEARPER = LV_FYEAR.

    IF SY-SUBRC = 0 .
      GT_OUTTAB-AHSTL = GT_OUTTAB-AHSTL - <LS_ACDOCA>-HSL.
    ENDIF.

    GT_OUTTAB-MONTH = LV_MONTH && TEXT-M01.
    GT_OUTTAB-RTCUR = GV_WAERS.
    GT_OUTTAB-THSTL = GT_OUTTAB-FHSTL.
    GT_OUTTAB-PHSTL = GT_OUTTAB-FHSTL - GT_OUTTAB-AHSTL.

    APPEND GT_OUTTAB.

    GV_FSUM = GV_FSUM + GT_OUTTAB-FHSTL.
    GV_ASUM = GV_ASUM + GT_OUTTAB-AHSTL.
    GV_TSUM = GV_TSUM + GT_OUTTAB-THSTL.
    GV_PSUM = GV_PSUM + GT_OUTTAB-PHSTL.

    CLEAR  GT_OUTTAB.

  ENDDO.

*-- 기예산 신청 번호 GET
  PERFORM CHECKED_SUBMIT_DATA.

ENDFORM.                    " SELECTED_MAIN_DATA
*&---------------------------------------------------------------------*
*&      Form  CHECKED_SAVED_DATA
*&---------------------------------------------------------------------*
FORM CHECKED_SAVED_DATA .

  DATA LV_SUM TYPE HSLXX9_CS.

  CLEAR GV_EXIT.

  LOOP AT GT_OUTTAB WHERE IHSTL <> 0.

    CLEAR LV_SUM.

    LV_SUM = GT_OUTTAB-PHSTL + GT_OUTTAB-IHSTL.

    IF LV_SUM < 0.
      GV_EXIT = ABAP_TRUE.
      EXIT.
    ENDIF.

  ENDLOOP.

  IF GV_EXIT = ABAP_TRUE.
    MESSAGE S000 WITH TEXT-E02 DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.                    " CHECKED_SAVED_DATA
*&---------------------------------------------------------------------*
*&      Form  POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
FORM POPUP_TO_CONFIRM USING PV_TITLE
                            PV_QUEST.

  "-- call popup
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR       = PV_TITLE                "TEXT-PT1
*     DIAGNOSE_OBJECT             = ' '
      TEXT_QUESTION  = PV_QUEST                "TEXT-QT1
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
*&      Form  SAVE_DATA_RTN
*&---------------------------------------------------------------------*
FORM SAVE_DATA_RTN .

  DATA: LV_MESSAGE TYPE STRING.

  FIELD-SYMBOLS: <FS1> TYPE ANY.

  DATA : LS_HEADERINFO  LIKE  BAPIPLNHDR.
  DATA LS_RETURN TYPE BAPIRET2.

  DATA LV_FNAME TYPE FIELDNAME.

  DATA : LT_INDEXSTRUCTURE LIKE BAPIACPSTRU OCCURS 0 WITH HEADER LINE,
         LT_COOBJECT       LIKE BAPIPCPOBJ  OCCURS 0 WITH HEADER LINE,
         LT_PERVALUE       LIKE BAPIPCPVAL  OCCURS 0 WITH HEADER LINE.

  DATA LT_ZCOT0040 TYPE TABLE OF ZCOT0040 WITH HEADER LINE.


  DATA LV_BELNR TYPE BELNR_D.
  DATA LV_SEQ   TYPE ZERSEQ.

  CLEAR: GT_RETURN, GT_RETURN[].

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
    MESSAGE S001 WITH TEXT-E03 DISPLAY LIKE 'E'.
    EXIT.
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
  LT_PERVALUE-COST_ELEM    = ZCOS0040-KSTAR.   "원가요소
  LT_PERVALUE-TRANS_CURR   = GV_WAERS.

  LOOP AT GT_OUTTAB WHERE IHSTL <> 0.

    LV_FNAME = 'LT_PERVALUE-FIX_VAL_PER' && GT_OUTTAB-MONTH(2).

    ASSIGN (LV_FNAME) TO <FS1>.

    IF GV_WAERS = 'KRW'.
      <FS1> = GT_OUTTAB-IHSTL  * 100.
    ELSE.
      <FS1> = GT_OUTTAB-IHSTL.
    ENDIF.

*-- CBO 저장 데이터
    MOVE: '00'           TO LT_ZCOT0040-RLDNR,
          '1'            TO LT_ZCOT0040-RRCTY,
          PA_VERSN       TO LT_ZCOT0040-RVERS,
          PA_GJAHR       TO LT_ZCOT0040-RYEAR,
          GV_OBJNR       TO LT_ZCOT0040-ROBJNR,
          LV_BELNR       TO LT_ZCOT0040-RDOCNR,
          'BT01'         TO LT_ZCOT0040-BUDTYPE_9,
          PA_KOKRS       TO LT_ZCOT0040-RKOKRS,
          SY-UZEIT       TO LT_ZCOT0040-CPUTM,
          SY-DATUM       TO LT_ZCOT0040-CPUDT,
          SY-UNAME       TO LT_ZCOT0040-USNAM,
          GV_WAERS       TO LT_ZCOT0040-RTCUR,
          ZCOS0040-KSTAR TO LT_ZCOT0040-RKSTAR.

    LV_FNAME = 'HSL' && GT_OUTTAB-MONTH(2).

    ASSIGN COMPONENT LV_FNAME OF STRUCTURE LT_ZCOT0040
       TO <FS1>.

    MOVE GT_OUTTAB-IHSTL TO <FS1>.

    IF <FS1> > 0.

      MOVE: 'SUPL' TO LT_ZCOT0040-PROCESS_9.   "증액

    ELSEIF <FS1> < 0.

      MOVE: 'RENT' TO LT_ZCOT0040-PROCESS_9.   "감액

    ENDIF.

    COLLECT LT_ZCOT0040.
    CLEAR   LT_ZCOT0040.

  ENDLOOP.

  IF SY-SUBRC <> 0.
    MESSAGE S000 WITH TEXT-E04 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  APPEND LT_PERVALUE.

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
                          CHANGING LV_MESSAGE.

    MESSAGE S000 WITH LV_MESSAGE DISPLAY LIKE 'E'.

  ELSE.


    TRY .

        INSERT  ZCOT0040 FROM TABLE LT_ZCOT0040.

        COMMIT WORK.

        MESSAGE S007.

        CLEAR: GV_ISUM, GV_FSUM.
        GV_FSUM = GV_TSUM.

        LOOP AT GT_OUTTAB.

          MOVE GT_OUTTAB-THSTL TO GT_OUTTAB-FHSTL.

          CLEAR GT_OUTTAB-IHSTL.

          MODIFY GT_OUTTAB.

        ENDLOOP.

      CATCH CX_SY_SQL_ERROR INTO DATA(LR_ERROR).

        ROLLBACK WORK.

        LV_MESSAGE = LR_ERROR->GET_TEXT( ).
        MESSAGE S001 WITH LV_MESSAGE DISPLAY LIKE 'E'.

    ENDTRY.

  ENDIF.

ENDFORM.                    " SAVE_DATA_RTN
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
*& Form INITIAL_SET
*&---------------------------------------------------------------------*
FORM INITIAL_SET .

  SELECT SINGLE BEZEI INTO @PA_KTXT
    FROM TKA01
   WHERE KOKRS = '1000'.

  SELECT SINGLE VTEXT INTO @PA_VTXT
    FROM TKVS AS A
    LEFT JOIN TKVST AS B
      ON A~VERSI = B~VERSI
     AND B~SPRAS = @SY-LANGU
   WHERE A~VERSI = 'B1'.

  CASE SY-TCODE.
    WHEN 'ZCOR0081'.
      GV_MODE = 'S'.
      SY-TITLE = TEXT-T04.
    WHEN OTHERS.
      GV_MODE = 'E'.
      SY-TITLE = TEXT-T03.
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
*& Form INTIAL_VALUE_CHECK
*&---------------------------------------------------------------------*
FORM INTIAL_VALUE_CHECK .

  CLEAR ZCOS0040.

  CASE ABAP_TRUE.

    WHEN PA_RAD1.

      IF PA_KOSTL IS INITIAL.
        SET CURSOR FIELD 'PA_KOSTL'.
        MESSAGE S026  WITH TEXT-C01 DISPLAY LIKE 'E'.
        STOP.
      ENDIF.

      SELECT SINGLE B~KTEXT INTO @ZCOS0040-KOSTLTX
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

      SELECT SINGLE POST1 INTO @ZCOS0040-POST1
       FROM PRPS
       WHERE POSID = @PA_POSID.

      IF SY-SUBRC <> 0.
        SET CURSOR FIELD 'PA_POSID'.
        MESSAGE S027  WITH TEXT-C03 DISPLAY LIKE 'E'.
        STOP.
      ENDIF.

      PERFORM AUTHORITY_CHECK USING 'WBS'.

  ENDCASE.

  IF PA_KSTAR IS INITIAL.
    SET CURSOR FIELD 'PA_KSTAR'.
    MESSAGE S026  WITH TEXT-C02 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  SELECT SINGLE B~KTEXT INTO @ZCOS0040-KTEXT
  FROM CSKA AS A
  LEFT JOIN CSKU AS B
    ON A~KTOPL = B~KTOPL
   AND A~KSTAR = B~KSTAR
   AND B~SPRAS = @SY-LANGU
 WHERE A~KTOPL = @GC_KTOPL
   AND A~KSTAR = @PA_KSTAR.

  IF SY-SUBRC <> 0.
    SET CURSOR FIELD 'PA_KSTAR'.
    MESSAGE S027  WITH TEXT-C02 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

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
*& Form GET_NEXT_NO
*&---------------------------------------------------------------------*
FORM GET_NEXT_NO    USING P_NO     TYPE NRNR
                           P_OBJ   TYPE NROBJ
                           P_CONV
                  CHANGING P_NUMBER
                           P_RETURN STRUCTURE BAPIRET2.

  DATA L_DOMLEN LIKE TNRO-DOMLEN.

  DATA LS_DD01V LIKE DD01V.

  DATA L_FUNCTION_NAME TYPE RS38L_FNAM.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR             = P_NO
      OBJECT                  = P_OBJ
    IMPORTING
      NUMBER                  = P_NUMBER
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
    P_RETURN-TYPE = 'E'.
    P_RETURN-MESSAGE  = TEXT-E01.
    EXIT.
  ENDIF.

  CHECK P_CONV = 'X'.

  SELECT SINGLE DOMLEN FROM TNRO INTO L_DOMLEN
   WHERE OBJECT = P_OBJ.

  CHECK SY-SUBRC = 0 AND LS_DD01V-CONVEXIT IS NOT INITIAL.

  CALL FUNCTION 'DDIF_DOMA_GET'
    EXPORTING
      NAME          = P_OBJ
      STATE         = 'A'
      LANGU         = SY-LANGU
    IMPORTING
      DD01V_WA      = LS_DD01V
    EXCEPTIONS
      ILLEGAL_INPUT = 1
      OTHERS        = 2.

  CONCATENATE 'CONVERSION_EXIT_' LS_DD01V-CONVEXIT
              '_INPUT'
         INTO L_FUNCTION_NAME.

  CALL FUNCTION L_FUNCTION_NAME
    EXPORTING
      INPUT  = P_NUMBER
    IMPORTING
      OUTPUT = P_NUMBER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUBMIT_DATA_RTN
*&---------------------------------------------------------------------*
FORM SUBMIT_DATA_RTN .

  DATA LV_FNAME TYPE FIELDNAME.
  DATA: LS_ZCOT1190 TYPE ZCOT1190,
        LT_ZCOT1200 TYPE TABLE OF ZCOT1200 WITH HEADER LINE.

  LOOP AT GT_OUTTAB WHERE IHSTL <> 0.

*-- CBO 저장 데이터
    MOVE: '00'           TO LT_ZCOT1200-RLDNR,
          '1'            TO LT_ZCOT1200-RRCTY,
          PA_VERSN       TO LT_ZCOT1200-RVERS,
          PA_GJAHR       TO LT_ZCOT1200-RYEAR,
          GV_OBJNR       TO LT_ZCOT1200-ROBJNR,
          'BT01'         TO LT_ZCOT1200-BUDTYPE_9,
          PA_KOKRS       TO LT_ZCOT1200-RKOKRS,
          GV_WAERS       TO LT_ZCOT1200-RTCUR,
          ZCOS0040-KSTAR TO LT_ZCOT1200-RKSTAR.

    LV_FNAME = 'HSL' && GT_OUTTAB-MONTH(2).

    ASSIGN COMPONENT LV_FNAME OF STRUCTURE LT_ZCOT1200
       TO FIELD-SYMBOL(<FS1>).

    MOVE GT_OUTTAB-IHSTL TO <FS1>.

    IF <FS1> > 0.

      MOVE: 'SUPL' TO LT_ZCOT1200-PROCESS_9,
            '001'  TO LT_ZCOT1200-RSEQ.        "증액

    ELSEIF <FS1> < 0.

      MOVE: 'RENT' TO LT_ZCOT1200-PROCESS_9,
            '002'  TO LT_ZCOT1200-RSEQ.        "감액

    ENDIF.

    COLLECT LT_ZCOT1200.
    CLEAR   LT_ZCOT1200.

  ENDLOOP.

  IF SY-SUBRC <> 0.
    MESSAGE S000 WITH TEXT-E04 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'ZCO_GW_BUDGET_GWKEY_CREATE'
    EXPORTING
      I_GWTYP       = 'C'
      I_SPMON       = SY-DATUM(6)
    IMPORTING
      E_GWKEY       = LS_ZCOT1190-GWKEY
    EXCEPTIONS
      INVALID_GWTYP = 1
      OTHERS        = 2.

  IF SY-SUBRC <> 0.
    MESSAGE S000 WITH TEXT-E05 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  MOVE: 'C'            TO LS_ZCOT1190-GWTYP,
        'I0'           TO LS_ZCOT1190-GWSTS,
        SY-DATUM(6)    TO LS_ZCOT1190-SPMON,
        ZCOS0040-TITLE TO LS_ZCOT1190-TITLE,
        SY-UZEIT       TO LS_ZCOT1190-ERZET,
        SY-DATUM       TO LS_ZCOT1190-ERDAT,
        SY-UNAME       TO LS_ZCOT1190-ERNAM,
        SY-UZEIT       TO LS_ZCOT1190-AEZET,
        SY-DATUM       TO LS_ZCOT1190-AEDAT,
        SY-UNAME       TO LS_ZCOT1190-AENAM.

  MOVE: LS_ZCOT1190-GWKEY TO LT_ZCOT1200-GWKEY,
        GV_FSUM           TO LT_ZCOT1200-FAMOUNT.

  MODIFY LT_ZCOT1200 TRANSPORTING GWKEY FAMOUNT
                      WHERE GWKEY IS INITIAL.

  TRY .

      INSERT  ZCOT1190 FROM LS_ZCOT1190.
      INSERT  ZCOT1200 FROM TABLE LT_ZCOT1200.

      COMMIT WORK.

      MESSAGE S053 WITH LS_ZCOT1190-GWKEY.

      CLEAR: GV_ISUM, GV_FSUM.
      GV_FSUM = GV_TSUM.

      LOOP AT GT_OUTTAB.

        MOVE GT_OUTTAB-THSTL TO GT_OUTTAB-FHSTL.

        CLEAR GT_OUTTAB-IHSTL.

        MODIFY GT_OUTTAB.

      ENDLOOP.

      ZCOS0040-GWKEY = LS_ZCOT1190-GWKEY.

      SELECT SINGLE URL INTO @GV_URL
        FROM ZCOT1210
       WHERE SYSID = @SY-SYSID
         AND GWTYP = 'C'.

      GV_URL = GV_URL && ZCOS0040-GWKEY.

      CALL FUNCTION 'CALL_BROWSER'
        EXPORTING
          URL                    = GV_URL
        EXCEPTIONS
          FRONTEND_NOT_SUPPORTED = 1
          FRONTEND_ERROR         = 2
          PROG_NOT_FOUND         = 3
          NO_BATCH               = 4
          UNSPECIFIED_ERROR      = 5
          OTHERS                 = 6.

    CATCH CX_SY_SQL_ERROR INTO DATA(LR_ERROR).

      ROLLBACK WORK.

      DATA(LV_MESSAGE) = LR_ERROR->GET_TEXT( ).
      MESSAGE S001 WITH LV_MESSAGE DISPLAY LIKE 'E'.

  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECKED_SUBMIT_DATA
*&---------------------------------------------------------------------*
FORM CHECKED_SUBMIT_DATA .

  CHECK GV_MODE = 'S'.

  SELECT SINGLE A~GWKEY, A~TITLE
    INTO (@ZCOS0040-GWKEY,@ZCOS0040-TITLE)
    FROM ZCOT1190 AS A
    INNER JOIN ZCOT1200 AS B
      ON A~GWKEY = B~GWKEY
   WHERE A~GWSTS IN ('', 'S0', 'A0' )
     AND A~GWTYP = 'C'
     AND B~ROBJNR = @GV_OBJNR
     AND B~RKSTAR = @ZCOS0040-KSTAR.

  IF SY-SUBRC = 0.
    MESSAGE S054 WITH ZCOS0040-GWKEY DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM AUTHORITY_CHECK  USING  VALUE(P_VALUE).

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

  CHECK GV_MODE = 'S'.

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
*& Form SCRFIELDS_FUNCTXT
*&---------------------------------------------------------------------*
FORM SCRFIELDS_FUNCTXT .

  GS_FUNTXT-ICON_ID   = ICON_INFORMATION.
  GS_FUNTXT-QUICKINFO = 'Program Help'.

  SSCRFIELDS-FUNCTXT_01 = GS_FUNTXT.

ENDFORM.
