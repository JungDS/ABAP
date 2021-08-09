*&---------------------------------------------------------------------*
*& Include          ZCOR0360F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form CHECK_CONTROLLING_AREA
*&---------------------------------------------------------------------*
FORM CHECK_CONTROLLING_AREA .

  SELECT SINGLE VTEXT INTO @PA_VTXT
    FROM TKVS AS A
    LEFT JOIN TKVST AS B
      ON A~VERSI = B~VERSI
     AND B~SPRAS = @SY-LANGU
   WHERE A~VERSI = @PA_VERSN.

  IF SY-SUBRC <> 0.
    SET CURSOR FIELD 'PA_VERSN'.
    MESSAGE E027  WITH TEXT-002.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTED_DATA_RTN
*&---------------------------------------------------------------------*
FORM SELECTED_DATA_RTN .

  CLEAR: GT_DATA, GT_DATA[].

  SELECT VERSN,
         CASE WHEN VERSN = '000' AND WRTTP = '01' AND KSTAR = '0984000010' THEN '04'
              ELSE WRTTP END AS WRTTP,
         GJAHR,
         KSTAR,
         SUM( WKG001 ) AS WKG001, SUM( WKG002 ) AS WKG002,
         SUM( WKG003 ) AS WKG003, SUM( WKG004 ) AS WKG004,
         SUM( WKG005 ) AS WKG005, SUM( WKG006 ) AS WKG006,
         SUM( WKG007 ) AS WKG007, SUM( WKG008 ) AS WKG008,
         SUM( WKG009 ) AS WKG009, SUM( WKG010 ) AS WKG010,
         SUM( WKG011 ) AS WKG011, SUM( WKG012 ) AS WKG012
    FROM COSP
   WHERE LEDNR = '00'
     AND ( ( VERSN = '000'      AND WRTTP = '04' ) OR
           ( VERSN = @PA_VERSN  AND WRTTP = '01' ) OR
           ( VERSN = '000'      AND WRTTP = '01'  AND KSTAR = '0984000010' ) )
     AND GJAHR = @PA_GJAHR
     AND KSTAR IN @R_KSTAR
     AND OBJNR IN @R_OBJNR
   GROUP BY VERSN, WRTTP, GJAHR, KSTAR
   UNION ALL
  SELECT VERSN,
         CASE WHEN VERSN = '000' AND WRTTP = '01' AND KSTAR = '0984000010' THEN '04'
              ELSE WRTTP END AS WRTTP,
         GJAHR,
         KSTAR,
         SUM( WKG001 ) AS WKG001, SUM( WKG002 ) AS WKG002,
         SUM( WKG003 ) AS WKG003, SUM( WKG004 ) AS WKG004,
         SUM( WKG005 ) AS WKG005, SUM( WKG006 ) AS WKG006,
         SUM( WKG007 ) AS WKG007, SUM( WKG008 ) AS WKG008,
         SUM( WKG009 ) AS WKG009, SUM( WKG010 ) AS WKG010,
         SUM( WKG011 ) AS WKG011, SUM( WKG012 ) AS WKG012
    FROM COSS
   WHERE LEDNR = '00'
     AND ( ( VERSN = '000'      AND WRTTP = '04' ) OR
           ( VERSN = @PA_VERSN  AND WRTTP = '01' ) OR
           ( VERSN = '000'      AND WRTTP = '01' AND KSTAR = '0984000010' ) )
     AND GJAHR = @PA_GJAHR
     AND KSTAR IN @R_KSTAR
     AND OBJNR IN @R_OBJNR
   GROUP BY VERSN, WRTTP, GJAHR, KSTAR
    UNION ALL
  SELECT RVERS  AS VERSN, WRTTP, RYEAR AS GJAHR,
         RKSTAR AS KSTAR,
        SUM( HSL01 ) AS WKG001, SUM( HSL02 ) AS WKG002,
        SUM( HSL03 ) AS WKG003, SUM( HSL04 ) AS WKG004,
        SUM( HSL05 ) AS WKG005, SUM( HSL06 ) AS WKG006,
        SUM( HSL07 ) AS WKG007, SUM( HSL08 ) AS WKG008,
        SUM( HSL09 ) AS WKG009, SUM( HSL10 ) AS WKG010,
        SUM( HSL11 ) AS WKG011, SUM( HSL12 ) AS WKG012
    FROM ZCOT1180
   WHERE RVERS  = '000'
     AND WRTTP  = '04'
     AND RKOKRS = @PA_KOKRS
     AND RYEAR  = @PA_GJAHR
     AND RKSTAR IN @R_KSTAR
     AND ROBJNR IN @R_OBJNR
   GROUP BY RVERS, WRTTP, RYEAR, RKSTAR

   UNION ALL
  SELECT RVERS , WRTTP, RYEAR AS GJAHR,
         SAKNR AS KSTAR,
        SUM( HSL01 ) AS WKG001, SUM( HSL02 ) AS WKG002,
        SUM( HSL03 ) AS WKG003, SUM( HSL04 ) AS WKG004,
        SUM( HSL05 ) AS WKG005, SUM( HSL06 ) AS WKG006,
        SUM( HSL07 ) AS WKG007, SUM( HSL08 ) AS WKG008,
        SUM( HSL09 ) AS WKG009, SUM( HSL10 ) AS WKG010,
        SUM( HSL11 ) AS WKG011, SUM( HSL12 ) AS WKG012
    FROM ZFIT0621       "관계사간 계획
   WHERE RVERS  = @PA_VERSN
     AND WRTTP  = '01'
     AND KOKRS  = @PA_KOKRS
     AND RYEAR  = @PA_GJAHR
     AND SAKNR  IN @R_KSTAR
     AND OBJNR  IN @R_OBJNR
   GROUP BY RVERS, WRTTP, RYEAR, SAKNR
    INTO TABLE @GT_DATA.

*-- 관계사간 거래 ADD
  SELECT SPMON, SAKNR,
         SUM( DMBTR ) AS DMBTR
    INTO TABLE @DATA(LT_ZFIT0620)
    FROM ZFIT0620
   WHERE SAKNR IN @R_KSTAR
     AND SPMON IN @R_SPMON
   GROUP BY SPMON, SAKNR.

  LOOP AT LT_ZFIT0620 INTO DATA(LS_ZFIT0620).

    MOVE: '000'              TO GT_DATA-VERSN,
          '04'               TO GT_DATA-WRTTP,
          PA_GJAHR           TO GT_DATA-GJAHR,
          LS_ZFIT0620-SAKNR  TO GT_DATA-KSTAR.

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

ENDFORM.
*&---------------------------------------------------------------------*
*& Form INITIAL_SET
*&---------------------------------------------------------------------*
FORM INITIAL_SET .

  SELECT SINGLE BEZEI, WAERS INTO (@PA_KTXT, @GV_WAERS)
    FROM TKA01
   WHERE KOKRS = '1000'.

  SELECT SINGLE VTEXT INTO @PA_VTXT
    FROM TKVS AS A
    LEFT JOIN TKVST AS B
      ON A~VERSI = B~VERSI
     AND B~SPRAS = @SY-LANGU
   WHERE A~VERSI = 'P0'.

  MOVE SY-DATUM+4(2) TO PA_PERBL.

  "__ 20191223 BSGSM_FCM ADD default cac
  SET PARAMETER ID 'CAC' FIELD PA_KOKRS.

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
*& Form SET_KSTAR
*&---------------------------------------------------------------------*
FORM SET_KSTAR .

  CLEAR: R_KSTAR, R_KSTAR[],
         R_SPMON, R_SPMON[],
         R_OBJNR, R_OBJNR[].

*-- 수주
  MOVE: '0984000010' TO R_KSTAR-LOW,
        'I'          TO R_KSTAR-SIGN,
        'EQ'         TO R_KSTAR-OPTION.
  APPEND R_KSTAR.

*-- 매출
  MOVE: '0401101001' TO R_KSTAR-LOW,
        '0499999999' TO R_KSTAR-HIGH,
        'I'          TO R_KSTAR-SIGN,
        'BT'         TO R_KSTAR-OPTION.
  APPEND R_KSTAR.

*-- 매출원가 (500101001)
  MOVE: '0500101001' TO R_KSTAR-LOW,
        '0599999999' TO R_KSTAR-HIGH,
        'I'          TO R_KSTAR-SIGN,
        'BT'         TO R_KSTAR-OPTION.
  APPEND R_KSTAR.

*-- 판매관리비
  MOVE: '0601101001' TO R_KSTAR-LOW,
        '0699999999' TO R_KSTAR-HIGH,
        'I'          TO R_KSTAR-SIGN,
        'BT'         TO R_KSTAR-OPTION.
  APPEND R_KSTAR.

*-- 영업외손익
  MOVE: '0701101001' TO R_KSTAR-LOW,
        '0799999999' TO R_KSTAR-HIGH,
        'I'          TO R_KSTAR-SIGN,
        'BT'         TO R_KSTAR-OPTION.
  APPEND R_KSTAR.

*-- 내부거래
  MOVE: '0984000100' TO R_KSTAR-LOW,
        '0984000999' TO R_KSTAR-HIGH,
        'I'          TO R_KSTAR-SIGN,
        'BT'         TO R_KSTAR-OPTION.
  APPEND R_KSTAR.

  MOVE: 'I'   TO R_SPMON-SIGN,
        'EQ'  TO R_SPMON-OPTION.

  R_SPMON-LOW  = PA_GJAHR && PA_PERBL+1(2).
  APPEND R_SPMON.

  R_OBJNR-LOW    = 'KS' && PA_KOKRS && '*'.
  R_OBJNR-SIGN   = 'I'.
  R_OBJNR-OPTION = 'CP'.
  APPEND R_OBJNR.

  R_OBJNR-LOW    = 'PR' && '*'.
  R_OBJNR-SIGN   = 'I'.
  R_OBJNR-OPTION = 'CP'.
  APPEND R_OBJNR.

  IF PA_CHECK = ABAP_TRUE.  "연결자료 포함

*-- 매출
    MOVE: '0984000100' TO R_KSTAR-LOW,
          '0984000109' TO R_KSTAR-HIGH,
          'I'          TO R_KSTAR-SIGN,
          'BT'         TO R_KSTAR-OPTION.
    APPEND R_KSTAR.

*-- 매출원가
    MOVE: '0984000110' TO R_KSTAR-LOW,
          '0984000119' TO R_KSTAR-HIGH,
          'I'          TO R_KSTAR-SIGN,
          'BT'         TO R_KSTAR-OPTION.
    APPEND R_KSTAR.

*-- 판매관리비
    MOVE: '0984000120' TO R_KSTAR-LOW,
          '0984000129' TO R_KSTAR-HIGH,
          'I'          TO R_KSTAR-SIGN,
          'BT'         TO R_KSTAR-OPTION.
    APPEND R_KSTAR.

*-- 영업외손익
    MOVE: '0984000130' TO R_KSTAR-LOW,
          '0984000139' TO R_KSTAR-HIGH,
          'I'          TO R_KSTAR-SIGN,
          'BT'         TO R_KSTAR-OPTION.
    APPEND R_KSTAR.

*-- 영업외비용
    MOVE: '0984000140' TO R_KSTAR-LOW,
          '0984000149' TO R_KSTAR-HIGH,
          'I'          TO R_KSTAR-SIGN,
          'BT'         TO R_KSTAR-OPTION.
    APPEND R_KSTAR.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form TOP_WRITE
*&---------------------------------------------------------------------*
FORM TOP_WRITE .

  DATA LV_VALUE TYPE CHAR100.

  CASE 'X'.

    WHEN PA_RAD1.
      CONCATENATE '(단위:' TEXT-006 INTO LV_VALUE
           SEPARATED BY SPACE.

    WHEN PA_RAD2.
      CONCATENATE '(단위:' TEXT-005 INTO LV_VALUE
           SEPARATED BY SPACE.
  ENDCASE.

  CONCATENATE LV_VALUE ')' INTO LV_VALUE.

  WRITE: /1(170) TEXT-T02 CENTERED NO-GAP.
  ULINE AT /74(24).

  WRITE:/2 '당기: '       CENTERED NO-GAP,
         PA_GJAHR         CENTERED NO-GAP,
         '년'             CENTERED NO-GAP,
         PA_PERBL+1(2)    CENTERED NO-GAP,
         '월'             CENTERED NO-GAP,
         133 LV_VALUE     CENTERED NO-GAP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DATA_WRITE
*&---------------------------------------------------------------------*
FORM DATA_WRITE .

  DATA LV_PERCENT TYPE P DECIMALS 1.
  DATA LV_VALUE TYPE CHAR100.

  PERFORM WRITE_TITLE.

  LOOP AT GT_OUTTAB.

    FORMAT RESET.

    IF GT_OUTTAB-GUBUN  = TEXT-C04 OR
       GT_OUTTAB-GUBUN  = TEXT-C05 OR
       GT_OUTTAB-GUBUN  = TEXT-C08 OR
       GT_OUTTAB-GUBUN  = TEXT-C09 OR
       GT_OUTTAB-GUBUN  = TEXT-C11 OR
       GT_OUTTAB-GUBUN  = TEXT-C12 OR
       GT_OUTTAB-GUBUN  = TEXT-C14.

      FORMAT COLOR COL_NORMAL INTENSIFIED.

    ELSE.


    ENDIF.

    WRITE: / SY-VLINE NO-GAP.

    CASE GT_OUTTAB-ETC.

      WHEN 'P'.

        WRITE: (26)   GT_OUTTAB-GUBUN LEFT-JUSTIFIED NO-GAP,
                      SY-VLINE NO-GAP.

        PERFORM SET_PERCENT_WRITE USING GT_OUTTAB-P1.


        PERFORM SET_PERCENT_WRITE USING GT_OUTTAB-M1.
        PERFORM SET_PERCENT_WRITE USING GT_OUTTAB-M2.

        WRITE: (12)   ' ' NO-GAP ,
                      SY-VLINE NO-GAP.

        PERFORM SET_PERCENT_WRITE USING GT_OUTTAB-C1.
        PERFORM SET_PERCENT_WRITE USING GT_OUTTAB-C2.

        WRITE: (12)   ' ' NO-GAP,
                      SY-VLINE NO-GAP,
               (12)   ' ' NO-GAP,
                      SY-VLINE NO-GAP.

      WHEN OTHERS.

        WRITE: (26)   GT_OUTTAB-GUBUN LEFT-JUSTIFIED NO-GAP,
                                      SY-VLINE NO-GAP.

        PERFORM MOVING_MINUS USING    GT_OUTTAB-P1
                                      GT_OUTTAB-WAERS
                             CHANGING LV_VALUE.

        WRITE: (19)   LV_VALUE RIGHT-JUSTIFIED,
                                     SY-VLINE NO-GAP.

        PERFORM MOVING_MINUS USING    GT_OUTTAB-M1
                                      GT_OUTTAB-WAERS
                             CHANGING LV_VALUE.

        WRITE: (19)  LV_VALUE RIGHT-JUSTIFIED,
                                     SY-VLINE NO-GAP.

        PERFORM MOVING_MINUS USING    GT_OUTTAB-M2
                                      GT_OUTTAB-WAERS
                             CHANGING LV_VALUE.

        WRITE: (19) LV_VALUE RIGHT-JUSTIFIED ,
                                     SY-VLINE NO-GAP.

        IF GT_OUTTAB-M1 IS NOT INITIAL.
          GT_OUTTAB-M3 = ( 1 - ( GT_OUTTAB-M1 - GT_OUTTAB-M2 ) /
                            ABS( GT_OUTTAB-M1 ) ) * '100'.

        ENDIF.

        PERFORM MOVING_MINUS2 USING   GT_OUTTAB-M3
                             CHANGING LV_VALUE.

        IF GT_OUTTAB-M3 IS INITIAL.
          WRITE: (11)   LV_VALUE RIGHT-JUSTIFIED NO-ZERO,
                                     SY-VLINE NO-GAP.
        ELSE.
          WRITE: (10)   LV_VALUE RIGHT-JUSTIFIED NO-GAP,
                 (1)    '%' ,  SY-VLINE NO-GAP.
        ENDIF.

        PERFORM MOVING_MINUS USING    GT_OUTTAB-C1
                                      GT_OUTTAB-WAERS
                             CHANGING LV_VALUE.

        WRITE:  (19)   LV_VALUE RIGHT-JUSTIFIED ,
                                            SY-VLINE NO-GAP.

        PERFORM MOVING_MINUS USING    GT_OUTTAB-C2
                                      GT_OUTTAB-WAERS
                             CHANGING LV_VALUE.

        WRITE: (19)  LV_VALUE RIGHT-JUSTIFIED ,
                                            SY-VLINE NO-GAP.

        IF GT_OUTTAB-C1 IS NOT INITIAL.
          GT_OUTTAB-C3 = ( 1 - ( GT_OUTTAB-C1 - GT_OUTTAB-C2 ) /
                            ABS( GT_OUTTAB-C1 ) ) * '100'.
        ENDIF.

        PERFORM MOVING_MINUS2 USING   GT_OUTTAB-C3
                             CHANGING LV_VALUE.

        IF GT_OUTTAB-C3 IS INITIAL.
          WRITE: (11)   LV_VALUE RIGHT-JUSTIFIED NO-ZERO,
                                 SY-VLINE NO-GAP.
        ELSE.
          WRITE: (10)   LV_VALUE      RIGHT-JUSTIFIED NO-GAP,
                 (1)    '%' ,  SY-VLINE NO-GAP.
        ENDIF.


        IF GT_OUTTAB-P1 IS NOT INITIAL.
          GT_OUTTAB-D1 = ( 1 - ( GT_OUTTAB-P1 - GT_OUTTAB-C2 ) /
                            ABS( GT_OUTTAB-P1 ) ) * '100'.

        ENDIF.

        PERFORM MOVING_MINUS2 USING   GT_OUTTAB-D1
                             CHANGING LV_VALUE.

        IF GT_OUTTAB-D1 IS INITIAL.
          WRITE: (11)   LV_VALUE RIGHT-JUSTIFIED NO-ZERO,
                                     SY-VLINE NO-GAP.
        ELSE.
          WRITE: (10)   LV_VALUE RIGHT-JUSTIFIED NO-GAP,
                 (1)    '%' ,  SY-VLINE NO-GAP.
        ENDIF.

    ENDCASE.

    ULINE AT (172).

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form WRITE_TITLE
*&---------------------------------------------------------------------*
FORM WRITE_TITLE .

  DATA LV_VTEXT TYPE CHAR30.

  LV_VTEXT = '(' && PA_VTXT && ')'.

  ULINE AT (172).

  FORMAT COLOR COL_KEY INTENSIFIED.

  WRITE: / SY-VLINE NO-GAP.
  WRITE: (26)   ' '     CENTERED NO-GAP, SY-VLINE NO-GAP,
         (20)   ' '     CENTERED NO-GAP, SY-VLINE NO-GAP,
         (54)   '당 월' CENTERED NO-GAP, SY-VLINE NO-GAP,
         (54)   '누 계' CENTERED NO-GAP, SY-VLINE NO-GAP,
         (12)    '  '   CENTERED NO-GAP, SY-VLINE NO-GAP.

  WRITE: / SY-VLINE       NO-GAP.
  WRITE: (26)    '구 분'   CENTERED NO-GAP, SY-VLINE NO-GAP,
         (20)    '년 계획' CENTERED NO-GAP, SY-VLINE NO-GAP,
         160(12) '진척율'  CENTERED NO-GAP, SY-VLINE NO-GAP.

  WRITE  50(110)  SY-ULINE.

  WRITE: / SY-VLINE NO-GAP.

  WRITE: (26) ' '       CENTERED NO-GAP, SY-VLINE  NO-GAP,
         (20) LV_VTEXT  CENTERED NO-GAP, SY-VLINE  NO-GAP,
         (20) '계 획'   CENTERED NO-GAP, SY-VLINE  NO-GAP,
         (20) '실 적'   CENTERED NO-GAP, SY-VLINE  NO-GAP,
         (12) '달성율'  CENTERED NO-GAP, SY-VLINE  NO-GAP,
         (20) '계 획'   CENTERED NO-GAP, SY-VLINE  NO-GAP,
         (20) '실 적'   CENTERED NO-GAP, SY-VLINE  NO-GAP,
         (12) '달성율'  CENTERED NO-GAP, SY-VLINE  NO-GAP,
         (12) ' '       CENTERED NO-GAP, SY-VLINE  NO-GAP.

  ULINE AT (172).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DATA_MAKE
*&---------------------------------------------------------------------*
FORM DATA_MAKE .

  CLEAR: GT_OUTTAB, GT_OUTTAB[],
         GS_OUTTAB.

  PERFORM INITIAL_DATA_SET.

*-- 1.수주
  PERFORM SET_DATA USING TEXT-C01
                         '0984000010'   ''.


*-- 2.매출
  PERFORM SET_DATA USING TEXT-C02
                         '0401101001'
                         '0499999999'.

* 20191125 MSKIM 주석처리, 중복처리하고있었음.
*  IF PA_CHECK = ABAP_TRUE.
*    PERFORM SET_DATA USING TEXT-C02
*                           '0984000100'
*                           '0984000109'.
*  ENDIF.

*-- 3.매출원가
  PERFORM SET_DATA USING TEXT-C03
                         '0500101001'
                         '0599999999'.

* 20191125 MSKIM 주석처리, 중복처리하고있었음.
*  IF PA_CHECK = ABAP_TRUE.
*    PERFORM SET_DATA USING TEXT-C03
*                           '0984000110'
*                           '0984000119'.
*  ENDIF.

*-- 4.매출이익
  PERFORM SET_CALC USING TEXT-C04
                         TEXT-C02
                         TEXT-C03
                         '-'.

*-- 4.1 매출이익율
  PERFORM SET_PERCEN USING TEXT-C05
                           TEXT-C04.

*-- 5. 판매관리비
  PERFORM SET_DATA USING TEXT-C06
                         '0601101001'
                         '0699999999'.

* 20191125 MSKIM 주석처리, 중복처리하고있었음.
*  IF PA_CHECK = ABAP_TRUE.
*    PERFORM SET_DATA USING TEXT-C06
*                           '0984000120'
*                           '0984000129'.
*  ENDIF.

*-- 5.1 판매관리비율
  PERFORM SET_PERCEN USING TEXT-C07
                           TEXT-C05.

*-- 6. 영업이익
  PERFORM SET_CALC USING TEXT-C08
                         TEXT-C04
                         TEXT-C06
                         '-'.

*-- 6.1 영업이익율
  PERFORM SET_PERCEN USING TEXT-C09
                           TEXT-C08.

*-- 7.1 영업외수익
  PERFORM SET_DATA USING TEXT-C10
                         '0701101001'
                         '0701999999'.

  PERFORM SET_DATA USING TEXT-C10
                         '0703101001'
                         '0703999999'.

  PERFORM SET_DATA USING TEXT-C10
                         '0705101001'
                         '0705999999'.

  IF PA_CHECK = ABAP_TRUE.
    PERFORM SET_DATA USING TEXT-C10
                           '0984000130'
                           '0984000139'.
  ENDIF.

*-- 7. 영업외비용
  PERFORM SET_DATA USING TEXT-C15
                         '0702101001'
                         '0702999999'.

  PERFORM SET_DATA USING TEXT-C15
                         '0704101001'
                         '0704999999'.

  PERFORM SET_DATA USING TEXT-C15
                         '0706101001'
                         '0706999999'.

  IF PA_CHECK = ABAP_TRUE.
    PERFORM SET_DATA USING TEXT-C15
                           '0984000140'
                           '0984000149'.
  ENDIF.

*-- 8. 경상이익
  PERFORM SET_CALC2 USING TEXT-C11.

*-- 8.1 경상이익율
  PERFORM SET_PERCEN USING TEXT-C12
                           TEXT-C11.


*-- 9.내부거래
*[CO] ZCOR0360_보고레포트1_억원 중복적용 로직 제외 BY MS.KIM 2019.11.15
  IF PA_CHECK IS NOT INITIAL.
    PERFORM SET_DATA USING TEXT-C13
                           '0984000100'
                           '0984000999'.

*-- 10.경상이익(내부거래)
    PERFORM SET_CALC USING TEXT-C14
                           TEXT-C11
                           TEXT-C13
                           '-'.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_DATA
*&---------------------------------------------------------------------*
FORM SET_DATA  USING P_GUBUN
                     P_KSTAR_F
                     P_KSTAR_T.

  DATA LV_MONTH TYPE N LENGTH 3.
  DATA LV_FIELDNAME TYPE FIELDNAME.

  DATA LV_INT TYPE P DECIMALS 1.

  RANGES: LR_KSTAR FOR CSKA-KSTAR.

  IF P_KSTAR_T IS NOT INITIAL.

    MOVE: 'I'        TO LR_KSTAR-SIGN,
          'BT'       TO LR_KSTAR-OPTION,
          P_KSTAR_F  TO LR_KSTAR-LOW,
          P_KSTAR_T  TO LR_KSTAR-HIGH.

    APPEND LR_KSTAR.

  ELSE.

    MOVE: 'I'        TO LR_KSTAR-SIGN,
          'EQ'       TO LR_KSTAR-OPTION,
          P_KSTAR_F  TO LR_KSTAR-LOW,
          P_KSTAR_T  TO LR_KSTAR-HIGH.

    APPEND LR_KSTAR.

  ENDIF.

  LOOP AT GT_DATA WHERE KSTAR IN LR_KSTAR.

    MOVE: P_GUBUN  TO GT_OUTTAB-GUBUN,
          GV_WAERS TO GT_OUTTAB-WAERS.

    CASE GT_DATA-WRTTP.

      WHEN '01'.

*-- 년계획
        CLEAR LV_MONTH.

        DO 12 TIMES.
          ADD 1 TO LV_MONTH.
          LV_FIELDNAME = 'WKG' && LV_MONTH.
          ASSIGN COMPONENT LV_FIELDNAME OF STRUCTURE GT_DATA
              TO FIELD-SYMBOL(<FS_WKG>).
          GT_OUTTAB-P1 = GT_OUTTAB-P1 + <FS_WKG>.
        ENDDO.

*-- 당월계획
        LV_FIELDNAME = 'WKG' && PA_PERBL.
        ASSIGN COMPONENT LV_FIELDNAME OF STRUCTURE GT_DATA
            TO <FS_WKG>.
        GT_OUTTAB-M1 = GT_OUTTAB-M1 + <FS_WKG>.

*-- 누계계획
        CLEAR LV_MONTH.

        DO .
          ADD 1 TO LV_MONTH.

          IF LV_MONTH > PA_PERBL.
            EXIT.
          ENDIF.

          LV_FIELDNAME = 'WKG' && LV_MONTH.
          ASSIGN COMPONENT LV_FIELDNAME OF STRUCTURE GT_DATA
              TO <FS_WKG>.
          GT_OUTTAB-C1 = GT_OUTTAB-C1 + <FS_WKG>.

        ENDDO.

      WHEN '04'.

*-- 당월실적
        LV_FIELDNAME = 'WKG' && PA_PERBL.
        ASSIGN COMPONENT LV_FIELDNAME OF STRUCTURE GT_DATA
            TO <FS_WKG>.
        GT_OUTTAB-M2 = GT_OUTTAB-M2 + <FS_WKG>.

*-- 누계실적
        CLEAR LV_MONTH.

        DO .
          ADD 1 TO LV_MONTH.

          IF LV_MONTH > PA_PERBL.
            EXIT.
          ENDIF.

          LV_FIELDNAME = 'WKG' && LV_MONTH.
          ASSIGN COMPONENT LV_FIELDNAME OF STRUCTURE GT_DATA
              TO <FS_WKG>.
          GT_OUTTAB-C2 = GT_OUTTAB-C2 + <FS_WKG>.

        ENDDO.

    ENDCASE.

    IF    GT_DATA-KSTAR CP '04*' OR    "매출 계정은 음수로 표시
          GT_DATA-KSTAR CP '0701*' OR
          GT_DATA-KSTAR CP '0703*' OR
          GT_DATA-KSTAR CP '0705*'.

      GT_OUTTAB-P1 = GT_OUTTAB-P1 * '-1'.
      GT_OUTTAB-C1 = GT_OUTTAB-C1 * '-1'.
      GT_OUTTAB-C2 = GT_OUTTAB-C2 * '-1'.
      GT_OUTTAB-M1 = GT_OUTTAB-M1 * '-1'.
      GT_OUTTAB-M2 = GT_OUTTAB-M2 * '-1'.

    ENDIF.

    COLLECT GT_OUTTAB.
    CLEAR   GT_OUTTAB.

  ENDLOOP.

  CHECK PA_RAD2 = ABAP_TRUE.

  LOOP AT GT_OUTTAB WHERE GUBUN = P_GUBUN.

    LV_INT = GT_OUTTAB-P1 * '100' / '100000000'.
    GT_OUTTAB-P1 = LV_INT.

    LV_INT = GT_OUTTAB-C1 * '100' / '100000000'.
    GT_OUTTAB-C1 = LV_INT.

    LV_INT = GT_OUTTAB-C2 * '100' / '100000000'.
    GT_OUTTAB-C2 = LV_INT.

    LV_INT = GT_OUTTAB-M1 * '100' / '100000000'.
    GT_OUTTAB-M1 = LV_INT.

    LV_INT = GT_OUTTAB-M2 * '100' / '100000000'.
    GT_OUTTAB-M2 = LV_INT.

    MODIFY GT_OUTTAB.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form INITIAL_DATA_SET
*&---------------------------------------------------------------------*
FORM INITIAL_DATA_SET .

  MOVE GV_WAERS TO GT_OUTTAB-WAERS.

  MOVE TEXT-C01 TO GT_OUTTAB-GUBUN.
  APPEND GT_OUTTAB.
  CLEAR  GT_OUTTAB.

  MOVE GV_WAERS TO GT_OUTTAB-WAERS.
  MOVE TEXT-C02 TO GT_OUTTAB-GUBUN.
  APPEND GT_OUTTAB.
  CLEAR  GT_OUTTAB.

  MOVE GV_WAERS TO GT_OUTTAB-WAERS.
  MOVE TEXT-C03 TO GT_OUTTAB-GUBUN.
  APPEND GT_OUTTAB.
  CLEAR  GT_OUTTAB.

  MOVE GV_WAERS TO GT_OUTTAB-WAERS.
  MOVE TEXT-C04 TO GT_OUTTAB-GUBUN.
  APPEND GT_OUTTAB.
  CLEAR  GT_OUTTAB.

  MOVE GV_WAERS   TO GT_OUTTAB-WAERS.
  MOVE: TEXT-C05  TO GT_OUTTAB-GUBUN,
          'P'     TO GT_OUTTAB-ETC.
  APPEND GT_OUTTAB.
  CLEAR  GT_OUTTAB.

  MOVE GV_WAERS TO GT_OUTTAB-WAERS.
  MOVE TEXT-C06 TO GT_OUTTAB-GUBUN.
  APPEND GT_OUTTAB.
  CLEAR  GT_OUTTAB.

  MOVE GV_WAERS    TO GT_OUTTAB-WAERS.
  MOVE: TEXT-C07   TO GT_OUTTAB-GUBUN,
          'P'      TO GT_OUTTAB-ETC.
  APPEND GT_OUTTAB.
  CLEAR  GT_OUTTAB.

  MOVE GV_WAERS TO GT_OUTTAB-WAERS.
  MOVE TEXT-C08 TO GT_OUTTAB-GUBUN.
  APPEND GT_OUTTAB.

  MOVE GV_WAERS   TO GT_OUTTAB-WAERS.
  MOVE: TEXT-C09  TO GT_OUTTAB-GUBUN,
          'P'     TO GT_OUTTAB-ETC.
  APPEND GT_OUTTAB.
  CLEAR  GT_OUTTAB.

  MOVE GV_WAERS TO GT_OUTTAB-WAERS.
  MOVE TEXT-C10 TO GT_OUTTAB-GUBUN.
  APPEND GT_OUTTAB.
  CLEAR  GT_OUTTAB.

  MOVE GV_WAERS TO GT_OUTTAB-WAERS.
  MOVE TEXT-C15 TO GT_OUTTAB-GUBUN.
  APPEND GT_OUTTAB.
  CLEAR  GT_OUTTAB.

  MOVE GV_WAERS TO GT_OUTTAB-WAERS.
  MOVE TEXT-C11 TO GT_OUTTAB-GUBUN.
  APPEND GT_OUTTAB.
  CLEAR  GT_OUTTAB.

  MOVE GV_WAERS    TO GT_OUTTAB-WAERS.
  MOVE: TEXT-C12   TO GT_OUTTAB-GUBUN,
          'P'      TO GT_OUTTAB-ETC.
  APPEND GT_OUTTAB.
  CLEAR  GT_OUTTAB.

* 20191125 MSKIM 주석처리, 중복처리하고있었음.
  IF PA_CHECK IS NOT INITIAL.

    MOVE GV_WAERS TO GT_OUTTAB-WAERS.
    MOVE TEXT-C13 TO GT_OUTTAB-GUBUN.
    APPEND GT_OUTTAB.
    CLEAR  GT_OUTTAB.

    MOVE GV_WAERS TO GT_OUTTAB-WAERS.
    MOVE TEXT-C14 TO GT_OUTTAB-GUBUN.
    APPEND GT_OUTTAB.
    CLEAR  GT_OUTTAB.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_CALC
*&---------------------------------------------------------------------*
FORM SET_CALC  USING P_GUBUN
                     P1 P2 P_CALC.

  CLEAR GS_OUTTAB.

  READ TABLE GT_OUTTAB INTO DATA(LS_OUTTAB1)
             WITH KEY GUBUN = P1.

  READ TABLE GT_OUTTAB INTO DATA(LS_OUTTAB2)
             WITH KEY GUBUN = P2.

  MOVE: P_GUBUN  TO GS_OUTTAB-GUBUN,
        GV_WAERS TO GS_OUTTAB-WAERS.

  CASE P_CALC.

    WHEN '-'.
      GS_OUTTAB-P1 = LS_OUTTAB1-P1 - LS_OUTTAB2-P1.
      GS_OUTTAB-C1 = LS_OUTTAB1-C1 - LS_OUTTAB2-C1.
      GS_OUTTAB-C2 = LS_OUTTAB1-C2 - LS_OUTTAB2-C2.
      GS_OUTTAB-M1 = LS_OUTTAB1-M1 - LS_OUTTAB2-M1.
      GS_OUTTAB-M2 = LS_OUTTAB1-M2 - LS_OUTTAB2-M2.

    WHEN '+'.
      GS_OUTTAB-P1 = LS_OUTTAB1-P1 + LS_OUTTAB2-P1.
      GS_OUTTAB-C1 = LS_OUTTAB1-C1 + LS_OUTTAB2-C1.
      GS_OUTTAB-C2 = LS_OUTTAB1-C2 + LS_OUTTAB2-C2.
      GS_OUTTAB-M1 = LS_OUTTAB1-M1 + LS_OUTTAB2-M1.
      GS_OUTTAB-M2 = LS_OUTTAB1-M2 + LS_OUTTAB2-M2.

  ENDCASE.

  COLLECT GS_OUTTAB INTO GT_OUTTAB.
  CLEAR   GS_OUTTAB.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_PERCENT
*&---------------------------------------------------------------------*
FORM SET_PERCEN  USING P_GUBUN
                        P_VALUE.

  CLEAR  GS_OUTTAB.

  READ TABLE GT_OUTTAB INTO DATA(LS_OUTTAB2)
           WITH KEY GUBUN = TEXT-C02.

  READ TABLE GT_OUTTAB INTO DATA(LS_OUTTAB4)
             WITH KEY GUBUN = P_VALUE.

  MOVE: P_GUBUN  TO GS_OUTTAB-GUBUN,
        GV_WAERS TO GS_OUTTAB-WAERS,
        'P'      TO GS_OUTTAB-ETC.

  PERFORM CALC_PERCENT USING    LS_OUTTAB4-P1
                                LS_OUTTAB2-P1
                       CHANGING GS_OUTTAB-P1.

  PERFORM CALC_PERCENT USING    LS_OUTTAB4-C1
                                LS_OUTTAB2-C1
                       CHANGING GS_OUTTAB-C1.

  PERFORM CALC_PERCENT USING    LS_OUTTAB4-C2
                                LS_OUTTAB2-C2
                       CHANGING GS_OUTTAB-C2.


  PERFORM CALC_PERCENT USING    LS_OUTTAB4-M1
                                LS_OUTTAB2-M1
                       CHANGING GS_OUTTAB-M1.

  PERFORM CALC_PERCENT USING    LS_OUTTAB4-M2
                                LS_OUTTAB2-M2
                       CHANGING GS_OUTTAB-M2.

  PERFORM CALC_PERCENT USING    LS_OUTTAB4-P1
                                LS_OUTTAB2-P1
                       CHANGING GS_OUTTAB-P1.

  COLLECT GS_OUTTAB INTO GT_OUTTAB.
  CLEAR   GS_OUTTAB.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALC_PERCENT
*&---------------------------------------------------------------------*
FORM CALC_PERCENT  USING    P1
                            P2
                   CHANGING P_VALUE.

  DATA LV_PERCENT TYPE P DECIMALS 1.

  CHECK P2 IS NOT INITIAL.

  LV_PERCENT = ( P1 / P2 ) * '100'.

  P_VALUE = LV_PERCENT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_PERCENT_WRITE
*&---------------------------------------------------------------------*
FORM SET_PERCENT_WRITE  USING P_VALUE.

  DATA LV_PERCENT TYPE P DECIMALS 1.
  DATA LV_VALUE TYPE CHAR100.

  MOVE P_VALUE TO LV_PERCENT.

  WRITE LV_PERCENT TO LV_VALUE LEFT-JUSTIFIED NO-ZERO.

  CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
    CHANGING
      VALUE = LV_VALUE.

  IF LV_PERCENT IS NOT INITIAL.
    WRITE: (18)   LV_VALUE RIGHT-JUSTIFIED NO-GAP,
           (1)    '%' , SY-VLINE NO-GAP.

  ELSE.
    WRITE: (19)   ' ' RIGHT-JUSTIFIED ,
                      SY-VLINE NO-GAP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MOVING_MINUS
*&---------------------------------------------------------------------*
FORM MOVING_MINUS  USING    P1
                            P_WAERS
                   CHANGING PV_VALUE TYPE CHAR100.

  CLEAR PV_VALUE.

  IF PA_RAD2 = ABAP_TRUE.
    WRITE P1 TO PV_VALUE LEFT-JUSTIFIED DECIMALS 1
                         NO-ZERO.

  ELSE.
    WRITE P1 TO PV_VALUE LEFT-JUSTIFIED
                        CURRENCY P_WAERS NO-ZERO.
  ENDIF.

  CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
    CHANGING
      VALUE = PV_VALUE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MOVING_MINUS2
*&---------------------------------------------------------------------*
FORM MOVING_MINUS2  USING    P1
                    CHANGING PV_VALUE TYPE CHAR100.

  CLEAR PV_VALUE.
  WRITE P1 TO PV_VALUE LEFT-JUSTIFIED NO-ZERO.

  CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
    CHANGING
      VALUE = PV_VALUE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_CALC2
*&---------------------------------------------------------------------*
FORM SET_CALC2  USING P_GUBUN.

  CLEAR GS_OUTTAB.

  READ TABLE GT_OUTTAB INTO DATA(LS_OUTTAB1)
             WITH KEY GUBUN = TEXT-C08.

  READ TABLE GT_OUTTAB INTO DATA(LS_OUTTAB2)
             WITH KEY GUBUN = TEXT-C10.

  READ TABLE GT_OUTTAB INTO DATA(LS_OUTTAB3)
           WITH KEY GUBUN = TEXT-C15.

  MOVE: P_GUBUN  TO GS_OUTTAB-GUBUN,
        GV_WAERS TO GS_OUTTAB-WAERS.

  GS_OUTTAB-P1 = LS_OUTTAB1-P1 + LS_OUTTAB2-P1 - LS_OUTTAB3-P1.
  GS_OUTTAB-C1 = LS_OUTTAB1-C1 + LS_OUTTAB2-C1 - LS_OUTTAB3-C1.
  GS_OUTTAB-C2 = LS_OUTTAB1-C2 + LS_OUTTAB2-C2 - LS_OUTTAB3-C2.
  GS_OUTTAB-M1 = LS_OUTTAB1-M1 + LS_OUTTAB2-M1 - LS_OUTTAB3-M1.
  GS_OUTTAB-M2 = LS_OUTTAB1-M2 + LS_OUTTAB2-M2 - LS_OUTTAB3-M2.

  COLLECT GS_OUTTAB INTO GT_OUTTAB.
  CLEAR   GS_OUTTAB.

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
