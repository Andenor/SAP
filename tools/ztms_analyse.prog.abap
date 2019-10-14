REPORT  ztms_analyse.

INCLUDE ztms_analyse_cls.
INCLUDE ztms_analyse_top.
INCLUDE ztms_analyse_alv100.
INCLUDE ztms_analyse_alv300.
INCLUDE ztms_analyse_scr.
INCLUDE ztms_analyse_frm.
INCLUDE ztms_analyse_pbo.
INCLUDE ztms_analyse_pai.

START-OF-SELECTION.
  PERFORM init.

END-OF-SELECTION.

  PERFORM alv.
