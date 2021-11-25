*&--------------------------------------------------------------------&*
*& PROGRAM ID  : ZCOR0350                                             &*
*& Title       : [CO] 예산신청 데이터 정리 ( 배치전용 )               &*
*& Created By  : BSGABAP4                                             &*
*& Created On  : 2019.09.26                                           &*
*& Description : [CO] 예산신청 데이터 정리 ( 배치전용 )               &*
*----------------------------------------------------------------------*
* MODIFICATION LOG
*----------------------------------------------------------------------*
* Tag  Date.       Author.     Description.
*----------------------------------------------------------------------*
* N    2019.09.26  BSGABAP4    INITIAL RELEASE
*----------------------------------------------------------------------*
REPORT ZCOR0350 MESSAGE-ID ZCO01.

TABLES: ZCOT1190, SSCRFIELDS.

DATA GS_FUNTXT TYPE SMP_DYNTXT.
DATA LV_ANSWER.
DATA LV_MESSAGE TYPE CHAR100.

SELECTION-SCREEN FUNCTION KEY 1.
SELECT-OPTIONS: SO_ERDAT FOR ZCOT1190-ERDAT,
                SO_ERNAM FOR ZCOT1190-ERNAM,
                SO_GWKEY FOR ZCOT1190-GWKEY.

*----------------------------------------------------------------------*
INITIALIZATION.
*----------------------------------------------------------------------*
  SO_ERDAT-HIGH = SY-DATUM - 7.
  SO_ERDAT-LOW  = SY-DATUM - 14.
  SO_ERDAT-SIGN = 'I'.
  SO_ERDAT-OPTION = 'BT'.
  APPEND SO_ERDAT.

  GS_FUNTXT-ICON_ID   = ICON_INFORMATION.
  GS_FUNTXT-QUICKINFO = 'Program Help'.

  SSCRFIELDS-FUNCTXT_01 = GS_FUNTXT.

*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
*---------------------------------------------------------------------*
  PERFORM SCR_USER_COMMAND. "양식 Download

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  IF SY-BATCH = ABAP_FALSE.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        TITLEBAR       = '확인'
        TEXT_QUESTION  = TEXT-I01
      IMPORTING
        ANSWER         = LV_ANSWER
      EXCEPTIONS
        TEXT_NOT_FOUND = 1
        OTHERS         = 2.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      EXIT.
    ENDIF.

    CHECK LV_ANSWER = 1.

  ENDIF.

  SELECT * FROM ZCOT1190
    INTO TABLE @DATA(LT_ZCOT1190)
   WHERE ERDAT  IN @SO_ERDAT
     AND ERNAM  IN @SO_ERNAM
     AND GWKEY  IN @SO_GWKEY
     AND GWSTS = 'I0'.

  IF SY-SUBRC <> 0.
    MESSAGE S004  DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  ULINE /1(157).

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.

  WRITE:/ SY-VLINE , (20)  '예산신청번호'  CENTERED,
          SY-VLINE , (12)  '생성자'    CENTERED,
          SY-VLINE , (12)  '생성일'    CENTERED,
          SY-VLINE , (100) 'Message'   CENTERED,
          SY-VLINE.

  ULINE /1(157).

  LOOP AT LT_ZCOT1190 INTO DATA(LS_ZCOT1190).

    FORMAT RESET.

    UPDATE ZCOT1190 SET GWSTS = 'Z9'
                        AEDAT = SY-DATUM
                        AENAM = SY-UNAME
                        AEZET = SY-UZEIT
                  WHERE GWKEY = LS_ZCOT1190-GWKEY
                    AND GWSTS = 'I0'.

    IF SY-SUBRC = 0.

      COMMIT WORK .
      LV_MESSAGE = '삭제 성공하였습니다'.

      FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.

    ELSE.
      ROLLBACK WORK.
      LV_MESSAGE = '삭제 실패하였습니다'.

      FORMAT COLOR COL_NEGATIVE  INTENSIFIED OFF.

    ENDIF.

    WRITE:/ SY-VLINE , (20)  LS_ZCOT1190-GWKEY  CENTERED,
            SY-VLINE , (12)  LS_ZCOT1190-ERNAM  CENTERED,
            SY-VLINE , (12)  LS_ZCOT1190-ERDAT  CENTERED,
            SY-VLINE , (100) LV_MESSAGE  CENTERED,
            SY-VLINE.

  ENDLOOP.

  ULINE /1(157).

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
