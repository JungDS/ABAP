*&---------------------------------------------------------------------*
*& Include          ZCOR0460I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CLEAR: SAVE_OK, GV_EXIT, GV_ANSWER, GV_ERROR.

*-- ALV CHECKED CHANGE DATA
  CALL METHOD GR_GRID1->CHECK_CHANGED_DATA( ).
  CALL METHOD GR_GRID1->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROWS[].
  .

  SAVE_OK = OK_CODE.
  CLEAR OK_CODE.

  CASE SAVE_OK.

    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.

      "__ 도움말
    WHEN '&HELP'.

      PERFORM CALL_POPUP_HELP(ZCAR9000) USING SY-REPID SY-DYNNR SY-LANGU ''.

    WHEN '&POST'.":회계기표

      PERFORM CREATE_DATA_RTN.

      PERFORM REFRESH_GRID_0100.

    WHEN '&CANC'.":취소


      CLEAR : GS_ROWS , GS_DISPLAY .

      "-- call popup
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TITLEBAR              = '확인'
*         DIAGNOSE_OBJECT       = ' '
          TEXT_QUESTION         = '선택한 전표를  일괄 역분개 합니다.'
*         TEXT_BUTTON_1         = 'Ja'(001)
*         ICON_BUTTON_1         = ' '
*         TEXT_BUTTON_2         = 'Nein'(002)
*         ICON_BUTTON_2         = ' '
*         DEFAULT_BUTTON        = '1'
          DISPLAY_CANCEL_BUTTON = ' '
*         USERDEFINED_F1_HELP   = ' '
*         START_COLUMN          = 25
*         START_ROW             = 6
*         POPUP_TYPE            =
*         IV_QUICKINFO_BUTTON_1 = ' '
*         IV_QUICKINFO_BUTTON_2 = ' '
        IMPORTING
          ANSWER                = GV_ANSWER
*   TABLES
*         PARAMETER             =
        EXCEPTIONS
          TEXT_NOT_FOUND        = 1
          OTHERS                = 2.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      IF GV_ANSWER = '1'.

        LOOP AT GT_ROWS INTO GS_ROWS.

          READ TABLE GT_DISPLAY ASSIGNING <FS_DISP> INDEX GS_ROWS-INDEX.

          CHECK <FS_DISP>-BELNR3 IS NOT INITIAL.
          CLEAR GV_ANSWER.

          CLEAR : GS_INFO .

          IF <FS_DISP>-BELNR3 IS NOT INITIAL.

            GS_INFO-BELNR = <FS_DISP>-BELNR3.
            GS_INFO-GJAHR = <FS_DISP>-SPMON+0(4).
            PERFORM CANCEL_DOC USING 'RR'.

          ENDIF.

          CLEAR : GS_INFO , GV_EXIT.

        ENDLOOP.

      ELSE.

        MESSAGE S000 WITH TEXT-M55.


      ENDIF.


      PERFORM REFRESH_GRID_0100.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
*-- OK_CODE ACTIONS.
  CASE OK_CODE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
