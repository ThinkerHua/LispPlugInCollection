;;;ȫ�ֱ�����fhΪ���ָ߶�, scΪ��ע����, preΪ����ֵ����, excΪ����X Y������
(princ "�������ZB")
(princ "  ���ܣ������ע")
(princ "  ���ߣ�Thinker(ThinkerHua@hotmail.com)")
(defun c:zb (/ pa pb pc ta tb X Y tbox_x tbox_y sl DimZin_Old mouse engl_t1 engl_t2 engl_pl err_msg fh_old sc_old pre_old) 
    ;;;pa pb pcΪ����(�����)����, ta tbΪ���ֻ���
    ;;;X YΪ����, tbox_x tbox_yΪ�ж����ֶ��󳤶�����ʱ����洢��
    ;;;slΪ���ֶ��󳤶�(�༴����ˮƽ�γ�)
    ;;;DimZin_OldΪ����������ʱ�洢��
    ;;;mouseΪ��̬Ԥ��ʱgrread����ֵ����ȡ�����ã�
    ;;;engl_t1 engl_t2 engl_plΪ���֡����ߵ����ϴ��У���̬Ԥ��ʱ���ϸ��£�
    ;;;fh_old sc_old pre_oldΪ��Ӧȫ�ֱ�������ʱ�洢��

    ;;;��������
    (defun *Error* (msg) 
        (if (= msg "quit / exit abort") 
            (if (/= err_msg nil) (princ err_msg)) ;�����˳�ʱ������Զ��������Ϣ
            (progn (princ (strcat "\nError��" msg))  ;����������ʱ�����������Ϣ
                   (setq *Error* nil)
            )
        )
    )

    ;;;�������ֲ���������ĩ�㺯��
    (defun poi_calc (sl) 
        (if (>= (car pb) (car pa)) 
            ;����λ���ڱ�ע���Ҳ�
            (setq ta (list (car pb) (+ (cadr pb) (/ fh 2)) 0)
                  tb (list (car pb) (- (cadr pb) (/ fh 2)) 0)
                  pc (list (+ (car pb) sl) (cadr pb) 0)
            )
            ;����λ���ڱ�ע�����
            (setq ta (list (- (car pb) sl) (+ (cadr pb) (/ fh 2)) 0)
                  tb (list (car ta) (- (cadr pb) (/ fh 2)) 0)
                  pc (list (car ta) (cadr pb) 0)
            )
        )
    )

    ;;;������س�ʼ������
    (if (= fh nil) 
        (setq fh 2.5)
    )
    (if (= sc nil) 
        (setq sc 0.001)
    )
    (if (= pre nil) 
        (setq pre 3)
    )
    (if (= exc nil) 
        (setq exc "Y")
    )
    (progn 
        (princ "\n��ǰ����ֵ  �ָ�=")
        (princ fh)
        (princ "  ����=")
        (princ sc)
        (princ "  ����=")
        (princ pre)
        (princ "  ����X Y����=")
        (princ exc)
    )

    ;;;ָ�����ע�ĵ�������ָߡ����������ȡ�����X Y����
    (setq pa nil)
    (while (= pa nil) 
        (initget "F S P E")
        (setq pa (getpoint 
                     "\nָ��Ҫ��ע������� �� [�ָ�(F)/����(S)/����(P)/����X Y����(E)]:"
                 )
        )
        (cond 
            ((= pa "F")
             (setq pa     nil
                   fh_old fh
             )
             (setq fh (getreal (strcat "\nָ���ָ�<" (rtos fh 2) ">��")))
             (if (= fh nil) (setq fh fh_old))
             (princ (strcat "��ǰ�ָ�=" (rtos fh 2)))
            )
            ((= pa "S")
             (setq pa     nil
                   sc_old sc
             )
             (setq sc (getreal (strcat "\nָ������<" (rtos sc 2 3) ">��")))
             (if (= sc nil) (setq sc sc_old))
             (princ (strcat "��ǰ����=" (rtos sc 2 3)))
            )
            ((= pa "P")
             (setq pa      nil
                   pre_old pre
             )
             (setq pre (getint (strcat "\nָ������<" (rtos pre 2 0) ">��")))
             (if (= pre nil) (setq pre pre_old))
             (princ (strcat "��ǰ����=" (rtos pre 2 0)))
            )
            ((= pa "E")
             (setq pa nil)
             (initget 1 "Y N")
             (setq exc (getkword (strcat "\n�Ƿ񽻻�X Y����[��(Y)/��(N)]<" exc ">��")))
             (princ 
                 (strcat "��ǰ��������=" 
                         (cond 
                             ((= exc "Y") "��")
                             ((= exc "N") "��")
                         )
                 )
             )
            )
            ((= pa nil) (progn (setq err_msg "ȡ��") (quit)))
            (T)
        )
    )

    ;;;��ȡ����ֵ
    (setq DimZin_Old (getvar "DimZin"))
    (setvar "DimZin" 1) ;С��ĩβ0������
    (if (= exc "N") 
        (setq Y (rtos (* (cadr pa) sc) 2 pre)
              X (rtos (* (car pa) sc) 2 pre)
        )
        (setq X (rtos (* (cadr pa) sc) 2 pre)
              Y (rtos (* (car pa) sc) 2 pre)
        )
    )
    (setvar "DimZin" DimZin_Old) ;�ָ���������
    (setq X      (strcat "X=" X)
          Y      (strcat "Y=" Y)
          tbox_x (textbox 
                     (list 
                         '(0 . "text")
                         (cons 40 fh)
                         (cons 1 X)
                         (cons 7 (getvar "textstyle"))
                     )
                 )
          tbox_y (textbox 
                     (list 
                         '(0 . "text")
                         (cons 40 fh)
                         (cons 1 Y)
                         (cons 7 (getvar "textstyle"))
                     )
                 )
    )
    ;;;���������ַ������ȼ�������ˮƽ�γ���
    (if (>= (car (cadr tbox_x)) (car (cadr tbox_y))) 
        (setq sl (car (cadr tbox_x)))
        (setq sl (car (cadr tbox_y)))
    )

    ;;;��ʱָ�����ֲ���㲢�������������ʼֵ���ڳ��������в��ϸ���
    (setq pb pa)
    (poi_calc sl)
    (princ "\nָ�����ֲ���㣺")

    ;;;��ʱ�������
    (prin_mtxt ta x fh 7)
    (setq engl_t1 (entget (entlast)))
    (prin_mtxt tb y fh 1)
    (setq engl_t2 (entget (entlast)))

    ;;;��ʱ������
    (prin_pl pa pb pc)
    (setq engl_pl (entget (entlast)))

    ;;;ʵʱԤ��
    (while (= (car (setq mouse (grread t 13 0))) 5) 
        (setq pb (cadr mouse))
        (poi_calc sl)
        (progn 
            (setq engl_t1 (subst (cons 10 ta) (assoc 10 engl_t1) engl_t1))
            (entmod engl_t1)
            (setq engl_t2 (subst (cons 10 tb) (assoc 10 engl_t2) engl_t2))
            (entmod engl_t2)
            (setq engl_pl (update_point_list_of_LWPOLYLINE 
                              engl_pl
                              (list (cons '10 pa) (cons '10 pb) (cons '10 pc))
                          )
            )
            (entmod engl_pl)
        )
    )

    (prin1)
)

        ;;;�������ֺ���
(defun prin_mtxt (point txt fh position) 
    (entmake 
        (list 
            '(0 . "MText")
            '(100 . "AcDbEntity")
            '(100 . "AcDbMText")
            (list 10 (car point) (cadr point))
            (cons 40 fh)
            (cons 71 position)
            (cons 1 txt)
            (cons 7 (getvar "textstyle"))
        )
    )
)

        ;;;�������ߺ���
(defun prin_pl (p1 p2 p3) 
    (entmake 
        (list '(0 . "LWPOLYLINE") 
              '(100 . "AcDbEntity")
              '(100 . "AcDbPolyline")
              (cons 90 3)
              (cons 10 p1)
              (cons 10 p2)
              (cons 10 p3)
        )
    )
)

        ;;;����LWPOLYLINE�����б�
(defun update_point_list_of_LWPOLYLINE (pline point_list / new_pline new_point sub_list) 
    ;������贫���point_list�Ƿ��ϱ�׼���б�
    ;��������point_list�����ϱ�׼������ɴ��������������
    ;�б�Ҫ�Ļ���Ӧ�����Ƚ�point_list��ϴ���޳�����׼����
    (setq new_pline nil)
    (foreach sub_list pline 
        (if (and (= 10 (car sub_list)) (/= point_list nil)) 
            (setq new_point  (car point_list)
                  point_list (cdr point_list)
                  new_pline  (append new_pline (list new_point))
            )
            (setq new_pline (append new_pline (list sub_list)))
        )
    )
)