*&---------------------------------------------------------------------*
*& Report ZCOR0040_MIG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcor0040config  MESSAGE-ID zco01..
INCLUDE zcor0040config_t01.
INCLUDE zcor0040cofing_scr.
INCLUDE zcor0040config_f01.

INITIALIZATION.
  PERFORM initial_set.
  PERFORM scrfields_functxt.

AT SELECTION-SCREEN.
*---------------------------------------------------------------------*
  PERFORM scr_user_command.


AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.

START-OF-SELECTION.

  CLEAR gv_flag.
  PERFORM role_check USING  gv_flag.

  IF gv_flag EQ abap_true.

    CASE 'X'.
      WHEN pa_back.

        SELECT SINGLE *
           FROM zcot0010b
          INTO @DATA(ls_001b).

        IF sy-subrc EQ 0.
          MESSAGE s000(zco01) WITH '이미 자료 존재, 복사 불필요..' DISPLAY LIKE 'E'.
        ELSE.
          PERFORM back_zcot0010b.
        ENDIF.

      WHEN pa_rest.

        PERFORM get_cot0010b_to_0010.

      WHEN pa_crea.

        PERFORM create_bukrs_data.

      WHEN pa_dele.

        IF pa_gjahr IS NOT INITIAL AND    pa_gjahr >= '2021'.

          PERFORM delete_bukrs_data.

        ELSE.
          IF  pa_gjahr < 2021.
            MESSAGE s000 WITH '2021 회계년도 이전은 삭제 불가입니다. ' DISPLAY LIKE 'E'.
          ELSEIF pa_gjahr IS INITIAL..
            MESSAGE s000 WITH '회계년도는 필수 엔트리 입니다. ' DISPLAY LIKE 'E'.
          ENDIF.

        ENDIF.

      WHEN pa_1100.

        PERFORM 1100_copy$$paste_bukrs_data.


    ENDCASE.

  ELSE.

    MESSAGE s000 WITH 'IT팀 CO모듈  PI 전용입니다..' DISPLAY LIKE 'E'.

  ENDIF.
