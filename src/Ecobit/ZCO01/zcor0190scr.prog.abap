*&---------------------------------------------------------------------*
*& Include          ZCOR0190SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-T01.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(12) TEXT-001 FOR FIELD PA_KOKRS.
SELECTION-SCREEN POSITION 33.
PARAMETERS: PA_KOKRS TYPE KOKRS MEMORY ID CAC OBLIGATORY
                                MATCHCODE OBJECT CSH_TKA01
                                MODIF ID MG1 DEFAULT '1000'.
SELECTION-SCREEN COMMENT 45(20) PA_KTXT MODIF ID MG1.
SELECTION-SCREEN END OF LINE.

PARAMETERS: PA_BUKRS TYPE BUKRS MEMORY ID BUK
                                MATCHCODE OBJECT ZSH_BUKRS,
            PA_GJAHR TYPE GJAHR OBLIGATORY DEFAULT SY-DATUM(4),
            PA_VERSN TYPE VERSN OBLIGATORY DEFAULT '0'
                                MATCHCODE OBJECT ZH_TKA09.

SELECTION-SCREEN: END OF BLOCK BL1.

"__ help
SELECTION-SCREEN FUNCTION KEY 1.

*--------------------------------------------------------------------*
* [ESG_CO] DEV_ESG 기존PGM 고도화 #7, 2021.12.09 10:02:56, MDP_06
*--------------------------------------------------------------------*
SELECTION-SCREEN FUNCTION KEY 2.
SELECTION-SCREEN FUNCTION KEY 3.
