;;;全局变量：fh为文字高度, sc为标注比例, pre为坐标值精度, exc为交换X Y坐标标记
(princ "\n启动命令：ZB")
(princ "  功能：坐标标注")
(princ "  作者：Thinker(ThinkerHua@hotmail.com)")
(defun c:zb (/ pa pb pc ta tb X Y tbox_x tbox_y sl DimZin_Old mouse engl_t1 engl_t2 engl_pl err_msg fh_old sc_old pre_old) 
    ;;;pa pb pc为引线(多段线)顶点, ta tb为文字基点
    ;;;X Y为坐标, tbox_x tbox_y为判断文字对象长度用临时坐标存储器
    ;;;sl为文字对象长度(亦即引线水平段长)
    ;;;DimZin_Old为环境变量临时存储器
    ;;;mouse为动态预览时grread返回值（获取坐标用）
    ;;;engl_t1 engl_t2 engl_pl为文字、引线的资料串行（动态预览时不断更新）
    ;;;fh_old sc_old pre_old为对应全局变量的临时存储器

    ;;;错误处理函数
    (defun *Error* (msg) 
        (if (= msg "quit / exit abort") 
            (if (/= err_msg nil) (princ err_msg)) ;主动退出时，输出自定义错误信息
            (progn (princ (strcat "\nError：" msg))  ;其他错误发生时，输出错误信息
                   (setq *Error* nil)
            )
        )
    )

    ;;;计算文字插入点和引线末点函数
    (defun poi_calc (sl) 
        (if (>= (car pb) (car pa)) 
            ;插入位置在标注点右侧
            (setq ta (list (car pb) (+ (cadr pb) (/ fh 2)) 0)
                  tb (list (car pb) (- (cadr pb) (/ fh 2)) 0)
                  pc (list (+ (car pb) sl) (cadr pb) 0)
            )
            ;插入位置在标注点左侧
            (setq ta (list (- (car pb) sl) (+ (cadr pb) (/ fh 2)) 0)
                  tb (list (car ta) (- (cadr pb) (/ fh 2)) 0)
                  pc (list (car ta) (cadr pb) 0)
            )
        )
    )

    ;;;程序加载初始化设置
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
        (princ "\n当前参数值  字高=")
        (princ fh)
        (princ "  比例=")
        (princ sc)
        (princ "  精度=")
        (princ pre)
        (princ "  交换X Y坐标=")
        (princ exc)
    )

    ;;;指定需标注的点或设置字高、比例、精度、交换X Y坐标
    (setq pa nil)
    (while (= pa nil) 
        (initget "F S P E")
        (setq pa (getpoint 
                     "\n指定要标注的坐标点 或 [字高(F)/比例(S)/精度(P)/交换X Y坐标(E)]:"
                 )
        )
        (cond 
            ((= pa "F")
             (setq pa     nil
                   fh_old fh
             )
             (setq fh (getreal (strcat "\n指定字高<" (rtos fh 2) ">：")))
             (if (= fh nil) (setq fh fh_old))
             (princ (strcat "当前字高=" (rtos fh 2)))
            )
            ((= pa "S")
             (setq pa     nil
                   sc_old sc
             )
             (setq sc (getreal (strcat "\n指定比例<" (rtos sc 2 3) ">：")))
             (if (= sc nil) (setq sc sc_old))
             (princ (strcat "当前比例=" (rtos sc 2 3)))
            )
            ((= pa "P")
             (setq pa      nil
                   pre_old pre
             )
             (setq pre (getint (strcat "\n指定精度<" (rtos pre 2 0) ">：")))
             (if (= pre nil) (setq pre pre_old))
             (princ (strcat "当前精度=" (rtos pre 2 0)))
            )
            ((= pa "E")
             (setq pa nil)
             (initget 1 "Y N")
             (setq exc (getkword (strcat "\n是否交换X Y坐标[是(Y)/否(N)]<" exc ">：")))
             (princ 
                 (strcat "当前交换坐标=" 
                         (cond 
                             ((= exc "Y") "是")
                             ((= exc "N") "否")
                         )
                 )
             )
            )
            ((= pa nil) (progn (setq err_msg "取消") (quit)))
            (T)
        )
    )

    ;;;获取坐标值
    (setq DimZin_Old (getvar "DimZin"))
    (setvar "DimZin" 1) ;小数末尾0不消除
    (if (= exc "N") 
        (setq Y (rtos (* (cadr pa) sc) 2 pre)
              X (rtos (* (car pa) sc) 2 pre)
        )
        (setq X (rtos (* (cadr pa) sc) 2 pre)
              Y (rtos (* (car pa) sc) 2 pre)
        )
    )
    (setvar "DimZin" DimZin_Old) ;恢复环境变量
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
    ;;;根据坐标字符串长度计算引线水平段长度
    (if (>= (car (cadr tbox_x)) (car (cadr tbox_y))) 
        (setq sl (car (cadr tbox_x)))
        (setq sl (car (cadr tbox_y)))
    )

    ;;;临时指定文字插入点并计算各个插入点初始值，在程序运行中不断更新
    (setq pb pa)
    (poi_calc sl)
    (princ "\n指定文字插入点：")

    ;;;临时输出文字
    (prin_mtxt ta x fh 7)
    (setq engl_t1 (entget (entlast)))
    (prin_mtxt tb y fh 1)
    (setq engl_t2 (entget (entlast)))

    ;;;临时画引线
    (prin_pl pa pb pc)
    (setq engl_pl (entget (entlast)))

    ;;;实时预览
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

        ;;;绘制文字函数
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

        ;;;绘制引线函数
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

        ;;;更新LWPOLYLINE顶点列表
(defun update_point_list_of_LWPOLYLINE (pline point_list / new_pline new_point sub_list) 
    ;这里假设传入的point_list是符合标准的列表
    ;如果传入的point_list不符合标准，将造成错误，甚至程序崩溃
    ;有必要的话，应当事先将point_list清洗，剔除不标准数据
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
