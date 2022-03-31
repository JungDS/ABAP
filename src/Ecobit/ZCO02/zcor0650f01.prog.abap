*&---------------------------------------------------------------------*
*& Include          ZCOR0650F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION .

  GV_REPID = SY-REPID.


  " 기본값 : 관리회계영역
  P_KOKRS = ZCL_CO_COMMON=>GET_DEFAULT_KOKRS( ).
  IF P_KOKRS IS INITIAL.
    P_KOKRS = '1000'.
  ENDIF.
  ZCL_CO_COMMON=>SET_KOKRS( P_KOKRS ).

  P_BUKRS = ZCL_CO_COMMON=>GET_DEFAULT_BUKRS( ).


  " 기본값 : 회계연도
  DATA LV_DATUM TYPE SY-DATUM.
  LV_DATUM = SY-DATUM.
  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      DATE      = LV_DATUM
      DAYS      = '50'
      MONTHS    = '00'
      SIGNUM    = '-'
      YEARS     = '00'
    IMPORTING
      CALC_DATE = LV_DATUM.
  P_GJAHR = LV_DATUM+0(4).
  P_VERSI = SPACE.


  " Selection Screen 텍스트
  TEXT_S01 = '실행조건'(S02).
  TEXT_S02 = '계획/실제선택'(S03).
  TEXT_S03 = '요약/상세선택'(S04).
  TEXT_S04 = '계획'(S05).
  TEXT_S05 = '계획버전'(S06).
  SY-TITLE = '[CO] 회사별 손익추이 레포트(사별권한)'(T01).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCRFIELDS_FUNCTXT
*&---------------------------------------------------------------------*
FORM SCRFIELDS_FUNCTXT .

  SSCRFIELDS = VALUE #(
    FUNCTXT_01 = VALUE SMP_DYNTXT( ICON_ID   = ICON_INFORMATION
                                   QUICKINFO = TEXT-S01 )
  ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_SCR
*&---------------------------------------------------------------------*
FORM MODIFY_SCR .

  IF P_PLN IS INITIAL.
    LOOP AT SCREEN.
      IF SCREEN-NAME EQ 'P_KOKRS'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.

      IF SCREEN-NAME EQ 'P_VERSI'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

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
*&---------------------------------------------------------------------*
*& Form CHECK_INPUT_DATA
*&---------------------------------------------------------------------*
FORM CHECK_INPUT_DATA .

  IF P_PLN IS INITIAL.
    CLEAR P_VERSI.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_BLOCK_BL1
*&---------------------------------------------------------------------*
FORM CHECK_BLOCK_BL1 .

  CHECK SSCRFIELDS-UCOMM EQ 'ONLI'.
  CHECK P_PLN EQ GC_X.


  IF P_VERSI IS INITIAL.
    " & 필드는 필수입니다.
    MESSAGE E026 WITH '계획버젼'(S06).
  ENDIF.


  SELECT COUNT(*)
    FROM TKA09
   WHERE KOKRS    EQ @P_KOKRS
     AND VERSN    EQ @P_VERSI
     AND PLANNING EQ @GC_X.

  IF SY-SUBRC NE 0.
    " & 필드값이 유효하지 않습니다.
    MESSAGE E027 WITH '계획버젼'(S06).
  ENDIF.



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


  LV_CLASS = 'CASUSR'.
  LV_CODE  = SY-UNAME.

  "__ SUPER USER ID 체크
  PERFORM CALL_F4_VALUES(ZCAR9000) TABLES LT_0070
                                    USING LV_CLASS
                                          LV_CODE
                                          LS_0070.
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


  " 회사코드 점검
  CALL FUNCTION 'ZCA_PRG_AUTH_CHECK'
    EXPORTING
      I_MODULE    = 'CO'
      I_BUKRS_CO  = P_BUKRS
    IMPORTING
      E_TYPE      = LV_TYPE
      E_MESSAGE   = LV_MESSAGE.

  IF LV_TYPE = 'E'.
    MESSAGE LV_MESSAGE TYPE GC_S DISPLAY LIKE GC_E.
    STOP.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXECUTE_REPORT
*&---------------------------------------------------------------------*
FORM EXECUTE_REPORT .

  DATA LV_ACTPLN.

  IF P_ACT EQ ABAP_ON.
    LV_ACTPLN = '0'.
  ELSE.
    LV_ACTPLN = '1'.
  ENDIF.

  IF P_VERSI IS INITIAL.
    P_VERSI = '000'.
  ENDIF.

  _CONVERSION_IN:  P_VERSI.

  SELECT REPID, RNAME
    FROM TKEB1
   WHERE APPLCLASS EQ 'KE'
     AND SUBCLASS  EQ '01'
     AND TABNAME   EQ 'CE11000'
     INTO TABLE @DATA(LT_TKEB1).

  SORT LT_TKEB1 BY REPID.

  CASE ABAP_ON.
    WHEN P_DTL.
      READ TABLE LT_TKEB1 INTO DATA(LS_TKEB1)
                          WITH KEY REPID = 'PA-11-D'
                                   BINARY SEARCH.
      CHECK SY-SUBRC EQ 0.
    WHEN P_SUM.
      READ TABLE LT_TKEB1 INTO LS_TKEB1
                          WITH KEY REPID = 'PA-11-S'
                                   BINARY SEARCH.
      CHECK SY-SUBRC EQ 0.
  ENDCASE.

  DATA: LT_BDC_DATA TYPE TABLE OF BDCDATA,
        LS_OPTION   TYPE CTU_PARAMS,
        LT_MESSAGE  TYPE TABLE OF BDCMSGCOLL.




  SET PARAMETER ID 'BUK' FIELD P_BUKRS.

*  EXPORT CALLED FROM 'X' TO MEMORY ID 'TEMP_ONLTYP2'.
*  SUBMIT (LS_TKEB1-RNAME) VIA SELECTION-SCREEN
*                          WITH PAR_01 = P_BUKRS
*                          WITH PAR_02 = P_GJAHR
*                          WITH PAR_03 = LV_ACTPLN
*                          WITH PAR_04 = P_VERSI
*  AND RETURN.

    LT_BDC_DATA = VALUE #(
      (  PROGRAM = LS_TKEB1-RNAME
          DYNPRO = '1000'
        DYNBEGIN = 'X' )

      ( FNAM = 'BDC_OKCODE'
        FVAL = '=ONLI' )

*      ( FNAM = 'PAR_01' " 회사 코드
*        FVAL = P_BUKRS )

      ( FNAM = 'PAR_02' " 회계연도
        FVAL = P_GJAHR )

      ( FNAM = 'PAR_03' " 계획/실제 지시자
        FVAL = LV_ACTPLN )

      ( FNAM = 'PAR_04' " 버전
        FVAL = P_VERSI )

      (  PROGRAM = LS_TKEB1-RNAME
          DYNPRO = '1000'
        DYNBEGIN = 'X' )

      ( FNAM = 'BDC_OKCODE'
        FVAL = '=ENDE' )

        ).

    LS_OPTION = VALUE #( DISMODE  = 'E'
                         UPDMODE  = 'A'
                         RACOMMIT = 'X' ).

  TRY.

    CASE ABAP_ON.
      WHEN P_DTL.
        CALL TRANSACTION 'ZCOR0651' WITHOUT AUTHORITY-CHECK
                                    USING LT_BDC_DATA
                                    OPTIONS FROM LS_OPTION
                                    MESSAGES INTO LT_MESSAGE.
      WHEN P_SUM.
        CALL TRANSACTION 'ZCOR0652' WITHOUT AUTHORITY-CHECK
                                    USING LT_BDC_DATA
                                    OPTIONS FROM LS_OPTION
                                    MESSAGES INTO LT_MESSAGE.
    ENDCASE.

  CATCH CX_ROOT INTO DATA(LX_ROOT).
    MESSAGE LX_ROOT->GET_TEXT( ) TYPE GC_S DISPLAY LIKE GC_E.

  ENDTRY.

ENDFORM.
