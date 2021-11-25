*&---------------------------------------------------------------------*
*& Include          ZCOR0420NSCR
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_2000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_2000 OUTPUT.
  SET PF-STATUS '2000'.
  SET TITLEBAR '2000'.

ENDMODULE.                 " STATUS_2000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SCREEN_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SCREEN_DATA OUTPUT.

  CHECK G_GRID IS INITIAL.

  CREATE OBJECT G_GRID
    EXPORTING
      I_PARENT = CL_GUI_CUSTOM_CONTAINER=>SCREEN0.

* ALV FIELDS 속성
  PERFORM MAKE_FIELD_CATEGORY.

* TOOLBAR 제어
  PERFORM EXCLUDE_FUNCTIONS USING 'GT_EXCLUDE'.

* 화면에 Display
  PERFORM GRID_DISPLAY_PART.

  PERFORM EVENT_HANDLER_REGISTER.
ENDMODULE.                 " SCREEN_DATA  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_2000 INPUT.
  CASE SY-UCOMM.
    WHEN 'DELE'.
      PERFORM CHECK_DATA.
      CHECK GV_ERROR IS INITIAL.
      PERFORM SELECT_DATA.
      PERFORM REFRESH_DATA.
    WHEN 'BACK'.
      PERFORM CONTAINER_FREE.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_3000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_3000 OUTPUT.
 SET PF-STATUS '3000'.
 SET TITLEBAR  '3000'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_3000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_3000 INPUT.
  CASE SY-UCOMM.
    WHEN 'ENTR'.
      PERFORM ZCOR0430_GO.
      LEAVE TO SCREEN 0.
    WHEN 'CANC'.
      GV_ERROR = 'X'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
