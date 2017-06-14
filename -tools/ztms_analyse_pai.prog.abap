*&---------------------------------------------------------------------*
*&  Include           ZACNTRANSP_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.
  save_ok = ok_code.
  CLEAR ok_code.
  CASE save_ok.
    WHEN 'EXIT'.
      PERFORM exit_screen.
    WHEN OTHERS.
*     do nothing
  ENDCASE.

ENDMODULE.                 " PAI_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE sy-ucomm.
    WHEN 'OK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0300 INPUT.
  save_ok = ok_code.
  CLEAR ok_code.
  CASE save_ok.
    WHEN 'EXIT'.
      CALL METHOD g_container_300->free.
      FREE: g_container_300, g_grid300.
      PERFORM exit_screen.
    WHEN OTHERS.
*     do nothing
  ENDCASE.
ENDMODULE.                 " PAI_0300  INPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0400 INPUT.
  DATA: lv_rep TYPE flag.

  save_ok = ok_code.
  CLEAR ok_code.
  CASE save_ok.
    WHEN 'EXIT'.
      CASE gv_textmode.
        WHEN gc_textmode_edit OR gc_textmode_new.
          CALL METHOD text_editor->get_textstream
*        EXPORTING
*          only_when_modified     = cl_gui_textedit=>true
             IMPORTING
               text                   = gv_text
*         IS_MODIFIED            =
             EXCEPTIONS
               error_cntl_call_method = 1
               not_supported_by_gui   = 2
               OTHERS                 = 3.

          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

          CALL METHOD cl_gui_cfw=>flush
            EXCEPTIONS
              cntl_system_error = 1
              cntl_error        = 2
              OTHERS            = 3.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar       = 'Enregistrer commentaire'
              text_question  = 'Enregistrer commentaire ?'
            IMPORTING
              answer         = lv_rep
            EXCEPTIONS
              text_not_found = 1
              OTHERS         = 2.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.

          CASE lv_rep.
            WHEN 1.
            WHEN 2.
              CLEAR gv_text.
            WHEN OTHERS.
          ENDCASE.
        WHEN gc_textmode_show.
        WHEN OTHERS.
      ENDCASE.
      CALL METHOD editor_container->free.
      FREE: editor_container, text_editor.
      PERFORM exit_screen.
    WHEN OTHERS.
*     do nothing
  ENDCASE.

ENDMODULE.                 " PAI_0400  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_0001 INPUT.

  CASE sy-ucomm.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN '&GET_VARI'.

    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " EXIT_0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  CASE sy-ucomm.
  WHEN '&SAV_VARI'.
  WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0001  INPUT
