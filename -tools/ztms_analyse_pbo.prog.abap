*&---------------------------------------------------------------------*
*&  Include           ZACNTRANSP_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.
  SET PF-STATUS 'MAIN0100'.
  SET TITLEBAR 'MAIN0100'.

  IF g_custom_container IS INITIAL.
    PERFORM create_and_init_alv CHANGING gt_alv[].
  ENDIF.

ENDMODULE.                 " PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PBO_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0300 OUTPUT.
  SET PF-STATUS 'MAIN0300'.
  SET TITLEBAR 'MAIN0300'.

  IF g_container_300 IS INITIAL.
    PERFORM init_alv_0300 CHANGING gt_comments[].
  ENDIF.

ENDMODULE.                 " PBO_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PBO_0400  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0400 OUTPUT.
  DATA: lv_text TYPE string.

  CONCATENATE sy-datum+6(2) '/'
              sy-datum+4(2) '/'
              sy-datum+0(4) ' - '
              sy-uzeit+0(2) ':'
              sy-uzeit+2(2) ':'
              sy-uzeit+4(2)
              INTO lv_text.

  CASE gv_textmode.
    WHEN gc_textmode_edit OR gc_textmode_show.
      lv_text = gv_text.
    WHEN gc_textmode_new.
    WHEN OTHERS.
  ENDCASE.

  IF editor_container IS INITIAL.
    CREATE OBJECT editor_container
      EXPORTING
        container_name              = 'TEXTEDITOR'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    CREATE OBJECT text_editor
      EXPORTING
        parent                     = editor_container
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_position          = gv_editor_length
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true.

    CLEAR gv_text.
    CALL METHOD text_editor->set_textstream
      EXPORTING
        text = lv_text.

    CASE gv_textmode.
      WHEN gc_textmode_edit.
      WHEN gc_textmode_show.
        CALL METHOD text_editor->set_readonly_mode.
      WHEN gc_textmode_new.
      WHEN OTHERS.
    ENDCASE.

  ENDIF.
ENDMODULE.                 " PBO_0400  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
  SET PF-STATUS 'MAIN0001'.
  SET TITLEBAR 'MAIN0001'.

ENDMODULE.                 " STATUS_0001  OUTPUT
