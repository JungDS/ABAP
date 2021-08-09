*&---------------------------------------------------------------------*
*& Include          ZCOR0480I01
*&---------------------------------------------------------------------*



*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.

  LEAVE TO SCREEN 0.

ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
*
  DATA : L_INDEX1  LIKE SY-TABIX,
         LT_INDEX1 LIKE TABIX OCCURS 0 WITH HEADER LINE,
         LT_INDEX2 LIKE TABIX OCCURS 0 WITH HEADER LINE,
         L_ERR.

  CLEAR : L_ERR.
  CASE SY-UCOMM.

    WHEN 'DELE'.

      PERFORM REFRESH_DISPLAY USING G_GRID1.
      PERFORM REFRESH_DISPLAY USING G_GRID2.



  ENDCASE.
*
*
ENDMODULE.                 " USER_COMMAND_0100  INPUT
