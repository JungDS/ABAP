*&--------------------------------------------------------------------&*
*& PROGRAM ID  : ZCOR0340                                             &*
*& Title       : [CO] 코스트센터: 실제/계획/차이                      &*
*& Created By  : BSGABAP4                                             &*
*& Created On  : 2019.09.23                                           &*
*& Description : [CO] 코스트센터: 실제/계획/차이                      &*
*----------------------------------------------------------------------*
* MODIFICATION LOG
*----------------------------------------------------------------------*
* Tag  Date.       Author.     Description.
*----------------------------------------------------------------------*
* N    2019.09.23  BSGABAP4    INITIAL RELEASE
* U    2020.0.05  BSGSM_FCM    KSTAR 검색 조건 추가
*----------------------------------------------------------------------*
REPORT ZCOR0340 MESSAGE-ID ZCO01.

TABLES: CSKS, CSKA, SSCRFIELDS.

DATA: " GT_VALUES LIKE GRPVALUES  OCCURS 0 WITH HEADER LINE,
  GT_NODES LIKE GRPOBJECTS OCCURS 0 WITH HEADER LINE,
  GT_INFO  LIKE GRPHINFO   OCCURS 0 WITH HEADER LINE.


CONSTANTS GC_KTOPL TYPE KTOPL VALUE '1000'.

DATA : GS_FUNTXT TYPE SMP_DYNTXT.

DATA LV_PROGNAME TYPE PROGNAME.
DATA GT_VALUES TYPE TABLE OF GRPVALUES WITH HEADER LINE.

RANGES: R_PRCTR2 FOR ZCOT0320-PRCTR2,
        R_BUKRS2 FOR ZCOT0320-BUKRS2,
        R_KOSTL  FOR CSKS-KOSTL,
        R_KSTAR  FOR CSKA-KSTAR.  "  GL, 원가요소 BSGSM_FCM 20200105

SELECTION-SCREEN FUNCTION KEY 1.

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-T01.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(12) TEXT-001 FOR FIELD PA_KOKRS.
SELECTION-SCREEN POSITION 33.
PARAMETERS: PA_KOKRS TYPE KOKRS OBLIGATORY
                                MATCHCODE OBJECT CSH_TKA01
                                DEFAULT '1000'.
SELECTION-SCREEN COMMENT 45(20) PA_KTXT MODIF ID MG1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(12) TEXT-002 FOR FIELD PA_VERSN.
SELECTION-SCREEN POSITION 33.
PARAMETERS  PA_VERSN TYPE VERSN OBLIGATORY MATCHCODE
                                 OBJECT ZH_TKA09
                                 DEFAULT 'P0'.
SELECTION-SCREEN COMMENT 45(20) PA_VTXT.
SELECTION-SCREEN END OF LINE.

PARAMETERS  PA_GJAHR TYPE GJAHR DEFAULT SY-DATUM(4) OBLIGATORY.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(12) TEXT-003 FOR FIELD PA_SPERL.
SELECTION-SCREEN POSITION 33.
PARAMETERS : PA_SPERL TYPE PERBL DEFAULT '001' OBLIGATORY. "BSGSM_FCM 20210617

SELECTION-SCREEN COMMENT 45(12) TEXT-004 FOR FIELD PA_EPERL.
SELECTION-SCREEN POSITION 65.
PARAMETERS : PA_EPERL TYPE PERBL DEFAULT '012' OBLIGATORY.  "BSGSM_FCM 20210617
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN: END OF BLOCK B1.

SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-T05.
SELECT-OPTIONS: SO_KOSTL FOR CSKS-KOSTL MODIF ID KOS
                                        MATCHCODE OBJECT KOST.

PARAMETERS PA_KSGRU TYPE KSGRU.

SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS: SO_KSTAR FOR CSKA-KSTAR  MATCHCODE
                                  OBJECT REEXCOCOSTELEM.

PARAMETERS PA_KAGRU TYPE KAGRU.

SELECTION-SCREEN: END OF BLOCK B2.

*---------------------------------------------------------------------*
INITIALIZATION.
*---------------------------------------------------------------------*
  SELECT SINGLE BEZEI INTO PA_KTXT
    FROM TKA01
   WHERE KOKRS = '1000'.

  SELECT SINGLE VTEXT INTO @PA_VTXT
    FROM TKVS AS A
    LEFT JOIN TKVST AS B
      ON A~VERSI = B~VERSI
     AND B~SPRAS = @SY-LANGU
*   WHERE A~VERSI = 000'.
   WHERE A~VERSI = 'P0'.  " <<<   P0 으로 변경 REQ BY 강현수

  GS_FUNTXT-ICON_ID   = ICON_INFORMATION.
  GS_FUNTXT-QUICKINFO = 'Program Help'.

  SSCRFIELDS-FUNCTXT_01 = GS_FUNTXT.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR PA_KAGRU.
*---------------------------------------------------------------------*
  PERFORM F4_KAGRU CHANGING PA_KAGRU.   "원가요소 그룹

*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR PA_KSGRU.
*---------------------------------------------------------------------*
  PERFORM F4_KSGRU CHANGING PA_KSGRU.   "코스트센터 그룹

*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON BLOCK B1.
*---------------------------------------------------------------------*
  PERFORM CHECK_B1.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON BLOCK B2.
*---------------------------------------------------------------------*
  PERFORM CHECK_B2.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*---------------------------------------------------------------------*
  PERFORM SCR_USER_COMMAND.

*---------------------------------------------------------------------*
START-OF-SELECTION.
*---------------------------------------------------------------------*

  PERFORM AUTHORITY_CHECK.
  PERFORM RANGES_SET.

  SELECT SINGLE UNI_IDC25 FROM T803VP
    INTO @DATA(LV_IDC25)
    WHERE RGJNR     = '1SIP'
      AND VERSN     = '00000001'
      AND PROGCLASS = 'RW_EXECUTION'.

  LV_PROGNAME = 'GP' && LV_IDC25 && SY-MANDT.

  SET PARAMETER ID 'ZPROG' FIELD SY-CPROG.

  SUBMIT (LV_PROGNAME)
    WITH $1KOKRE   = PA_KOKRS
    WITH $1GJAHR   = PA_GJAHR
    WITH $1PERIV   = PA_SPERL
    WITH $1PERIB   = PA_EPERL
    WITH $1VERP    = PA_VERSN
    WITH _1KOSET IN R_KOSTL
    WITH $1KOSET   = SPACE

**<<<<<<<<<    bsgsm_fcm 20201005..  req by 강현수k
     WITH _1KSTAR IN R_KSTAR
    WITH $1KSTAR   = SPACE

**>>>>>>>>>  bsgsm_fcm 202010005 ..
    AND RETURN.

*&---------------------------------------------------------------------*
*& Form F4_KAGRU
*&---------------------------------------------------------------------*
FORM F4_KAGRU  CHANGING P_KAGRU.

  DATA LV_KAGRU TYPE KAGRU.

  CALL FUNCTION 'K_GROUP_SELECT'
    EXPORTING
      CLASS         = '0102'
      FIELD_NAME    = 'KSTAR'
      KOKRS         = PA_KOKRS
      KTOPL         = GC_KTOPL
    IMPORTING
      SET_NAME      = LV_KAGRU
    EXCEPTIONS
      NO_SET_PICKED = 1
      OTHERS        = 2.

  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.

  IF LV_KAGRU IS NOT INITIAL.
    P_KAGRU  = LV_KAGRU.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F4_KSGRU
*&---------------------------------------------------------------------*
FORM F4_KSGRU  CHANGING P_KSGRU.

  DATA: HELP_SETNR     LIKE RGSMH-SETNR,
        HELP_SEARCHFLD LIKE RGSMH-SEARCHFLD,
        HELP_SET       LIKE RGSBS-SETNR,
        HELP_SETCLASS  LIKE RGSMH-CLASS.

  MOVE PA_KOKRS TO HELP_SEARCHFLD.

  CALL FUNCTION 'K_GROUP_SELECT'
    EXPORTING
      CLASS           = '0101'
      FIELD_NAME      = 'KOSTL'
      SEARCHFLD       = HELP_SEARCHFLD
      SEARCHFLD_INPUT = ' '
      SET             = HELP_SET
    IMPORTING
      SET_NAME        = HELP_SETNR
    EXCEPTIONS
      NO_SET_PICKED   = 1.

  IF SY-SUBRC = 0.
    P_KSGRU = HELP_SETNR.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_B1
*&---------------------------------------------------------------------*
FORM CHECK_B1 .

  SELECT SINGLE BEZEI INTO @PA_KTXT
    FROM TKA01
   WHERE KOKRS = @PA_KOKRS.

  IF SY-SUBRC <> 0.
    SET CURSOR FIELD 'PA_KOKRS'.
    MESSAGE E023  WITH TEXT-001.
  ENDIF.

  SELECT SINGLE VTEXT INTO @PA_VTXT
    FROM TKVS AS A
    LEFT JOIN TKVST AS B
      ON A~VERSI = B~VERSI
     AND B~SPRAS = @SY-LANGU
   WHERE A~VERSI = @PA_VERSN.
*   WHERE A~VERSI = 'B1'.,


  IF PA_SPERL > PA_EPERL AND
      PA_EPERL IS NOT INITIAL.
    SET CURSOR FIELD 'PA_SPERL'.
    MESSAGE E018  WITH TEXT-E01.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_B2
*&---------------------------------------------------------------------*
FORM CHECK_B2 .

  DATA LS_RETURN TYPE BAPIRET2.

  IF SO_KOSTL[] IS NOT INITIAL AND
     PA_KSGRU IS NOT INITIAL.
    SET CURSOR FIELD 'SO_KOSTL-LOW'.
    MESSAGE E000  WITH TEXT-E02.
  ENDIF.

  IF SO_KOSTL[] IS NOT INITIAL.

    SELECT SINGLE * FROM CSKS
      INTO @DATA(LS_CSKS)
     WHERE KOKRS = @PA_KOKRS
       AND KOSTL IN @SO_KOSTL
       AND DATBI >= @SY-DATUM
       AND DATAB <= @SY-DATUM.

    IF SY-SUBRC <> 0.
      SET CURSOR FIELD 'SO_KOSTL-LOW'.
      MESSAGE E027  WITH TEXT-E05.
    ENDIF.

  ENDIF.

  IF PA_KSGRU IS NOT INITIAL.

    PERFORM GET_CHECK_GROUP USING '0101'
                              PA_KSGRU
                        CHANGING LS_RETURN.

    IF LS_RETURN-TYPE = 'E'.
      SET CURSOR FIELD 'PA_KSGRU'.
      MESSAGE E027  WITH TEXT-E06.
    ENDIF.

  ENDIF.

  IF SO_KSTAR[] IS NOT INITIAL AND
     PA_KAGRU IS NOT INITIAL.

    SET CURSOR FIELD 'SO_KSTAR-LOW'.
    MESSAGE E026  WITH TEXT-E03.

  ENDIF.

  IF SO_KSTAR[] IS NOT INITIAL.

    SELECT SINGLE * FROM CSKA
      INTO @DATA(LS_CSKA)
     WHERE KTOPL = @GC_KTOPL
       AND KSTAR IN @SO_KSTAR.

    IF SY-SUBRC <> 0.
      SET CURSOR FIELD 'SO_KSTAR-LOW'.
      MESSAGE E027  WITH TEXT-E09.
    ENDIF.

  ENDIF.

  IF PA_KAGRU IS NOT INITIAL.

    PERFORM GET_CHECK_GROUP USING '0102'
                                  PA_KAGRU
                            CHANGING LS_RETURN.


    IF LS_RETURN-TYPE = 'E'.
      SET CURSOR FIELD 'PA_KAGRU'.
      MESSAGE E027  WITH TEXT-E10.
    ENDIF.

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

  CLEAR: R_BUKRS2, R_BUKRS2[].

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

  IF SO_KOSTL[] IS INITIAL AND PA_KSGRU IS INITIAL.
    MESSAGE S000 WITH TEXT-E13 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  CALL FUNCTION 'ZCA_PRG_AUTH_CHECK'
    EXPORTING
      I_MODULE    = 'CO'
      I_KSTGR_CO  = PA_KSGRU
    IMPORTING
      E_TYPE      = LV_TYPE
      E_MESSAGE   = LV_MESSAGE
    TABLES
      IT_KOSTL_CO = SO_KOSTL[].

  IF LV_TYPE = 'E'.
    MESSAGE S000 WITH LV_MESSAGE DISPLAY LIKE 'E'.
    STOP.
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
*& Form RANGES_SET
*&---------------------------------------------------------------------*
FORM RANGES_SET .

  CLEAR: R_KOSTL,  R_KOSTL[],
         R_PRCTR2, R_PRCTR2[],
         R_BUKRS2, R_BUKRS2[].

  CLEAR : R_KSTAR, R_KSTAR[].  " add bsgsm_fcm..


  SELECT * FROM ZCOT0320
    INTO TABLE @DATA(LT_ZCOT0320)
    WHERE BNAME = @SY-UNAME.


  LOOP AT LT_ZCOT0320 INTO DATA(LS_ZCOT0320).

    IF LS_ZCOT0320-PRCTR2 IS NOT INITIAL.

      MOVE: LS_ZCOT0320-PRCTR2 TO R_PRCTR2-LOW,
            'I'                TO R_PRCTR2-SIGN,
            'EQ'               TO R_PRCTR2-OPTION.

      COLLECT R_PRCTR2.
      CLEAR   R_PRCTR2.

    ENDIF.


    IF LS_ZCOT0320-BUKRS2 IS NOT INITIAL.

      MOVE: LS_ZCOT0320-BUKRS2 TO R_BUKRS2-LOW,
            'I'                TO R_BUKRS2-SIGN,
            'EQ'               TO R_BUKRS2-OPTION.

      COLLECT R_BUKRS2.
      CLEAR   R_BUKRS2.

    ENDIF.

  ENDLOOP.

  IF SO_KOSTL[] IS NOT INITIAL.

    R_KOSTL[] = SO_KOSTL[].

  ENDIF.

  IF PA_KSGRU IS NOT INITIAL.

    PERFORM READ_HIERARCHY_TABLES TABLES GT_VALUES
                                  USING '0101'
                                        PA_KSGRU.  "코스트센터 그룹

    LOOP AT GT_VALUES.

      MOVE: 'I'             TO R_KOSTL-SIGN,
            'BT'            TO R_KOSTL-OPTION,
            GT_VALUES-VFROM TO R_KOSTL-LOW,
            GT_VALUES-VTO   TO R_KOSTL-HIGH.

      COLLECT R_KOSTL.
      CLEAR   R_KOSTL.

    ENDLOOP.

  ENDIF.



  SELECT KOSTL FROM CSKS
    INTO TABLE @DATA(LT_CSKS)
   WHERE KOSTL IN @R_KOSTL
     AND KOKRS = @PA_KOKRS
     AND BUKRS IN @R_BUKRS2
     AND PRCTR IN @R_PRCTR2
     AND DATBI >= @SY-DATUM
     AND DATAB <= @SY-DATUM.

  LOOP AT LT_CSKS INTO DATA(LS_CSKS).

    MOVE: 'I'  TO R_KOSTL-SIGN,
          'EQ' TO R_KOSTL-OPTION.

    R_KOSTL-LOW  = LS_CSKS-KOSTL.

    COLLECT R_KOSTL.
    CLEAR   R_KOSTL.

  ENDLOOP.


**>>>>>>>>>>>>>>>>>  ADD BSGSM_FCM 20201005
  IF SO_KSTAR[] IS NOT INITIAL.

    R_KSTAR[] = SO_KSTAR[].

  ENDIF.


  IF PA_KAGRU IS NOT INITIAL.

    PERFORM SET_KSTAR_RANGES TABLES R_KSTAR
                              USING PA_KAGRU.

  ENDIF.


**<<<<<<<<<<<<< END BY BSGSM-FCM 20200105


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_CHECK_GROUP
*&---------------------------------------------------------------------*
FORM GET_CHECK_GROUP  USING    VALUE(P_SETCLASS)
                                        P_GROUP
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

INCLUDE ZCOR0340F01.
