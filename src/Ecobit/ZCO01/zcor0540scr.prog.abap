*&---------------------------------------------------------------------*
*& Include          ZCOR0540SCR
*&---------------------------------------------------------------------*

SELECTION-SCREEN FUNCTION KEY 1.

SELECTION-SCREEN BEGIN OF BLOCK B01 WITH FRAME TITLE TEXT_S01.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (30) TEXT_S02 FOR FIELD P_R01.
PARAMETERS P_R01  RADIOBUTTON GROUP RG1.

SELECTION-SCREEN COMMENT 40(60) TEXT_S04.
SELECTION-SCREEN END OF LINE.


SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (30) TEXT_S03 FOR FIELD P_R02.
PARAMETERS P_R02  RADIOBUTTON GROUP RG1.

SELECTION-SCREEN COMMENT 40(60) TEXT_S05.
SELECTION-SCREEN END OF LINE.



SELECTION-SCREEN END OF BLOCK B01.
