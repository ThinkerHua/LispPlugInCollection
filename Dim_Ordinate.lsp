;;;2016.01.15更新
;;;pa pb pc为引线(多段线)顶点,ta tb为文字基点(均为全局变量)
;;;fh为文字高度,sc为标注比例,pre为坐标值精度,exc为交换X Y坐标标记(均为全局变量)
;;;X Y为坐标,tbox_x tbox_y为判断文字对象长度用临时坐标存储器
;;;sl为文字对象长度(亦即引线水平段长)
;;;cyc为循环标记,DimZin_Old为环境变量临时存储器
;;;mouse为动态预览时grread返回值（获取坐标用）
;;;engl_t1,engl_t2,engl_pl为文字、引线的资料串行（动态预览时不断更新）
(defun c:zb (/        X         Y  tbox_x   tbox_y    sl
       cyc      DimZin_Old  mouse   engl_t1  engl_t2
       engl_pl
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
    (princ "******作者：Thinker(ThinkerHua@hotmail.com)******")
    (princ "\n当前参数值   字高：")
    (princ fh)
    (princ "   比例：")
    (princ sc)
    (princ "   精度：")
    (princ pre)
    (princ "   交换X Y坐标：")
    (princ exc)
  )
  (setq DimZin_Old (getvar "DimZin"))
  (setvar "DimZin" 1)

;;;指定需标注的点或设置字高、比例、精度、交换X Y坐标
  (while (and (= cyc nil) (/= (type pa) 'str))
    (initget "F S P E")
    (setq
      pa
       (getpoint
   "\n指定要标注的坐标点或[字高(F)/比例(S)/精度(P)/交换X Y坐标(E)]:"
       )
    )
    (cond
      ((= pa "F")
       (progn (setq fh (getreal "\n指定字高："))
        (setq pa nil)
       )
      )
      ((= pa "S")
       (progn (setq sc (getreal "\n指定比例："))
        (setq pa nil)
       )
      )
      ((= pa "P")
       (progn (setq pre (getint "\n指定精度："))
        (setq pa nil)
       )
      )
      ((= pa "E")
       (progn (initget 1 "Y N")
        (setq exc (getkword "\n是否交换X Y坐标[是(Y)/否(N)]<Y>:"))
        (setq pa nil)
       )
      )
      ((= pa nil)
      )
      (T (setq cyc T))
    )
  )
  (setq cyc nil)

;;;获取坐标值
  (if (= exc "N")
    (progn (setq Y (rtos (* (cadr pa) sc) 2 pre))
     (setq X (rtos (* (car pa) sc) 2 pre))
    )
    (progn (setq X (rtos (* (cadr pa) sc) 2 pre))
     (setq Y (rtos (* (car pa) sc) 2 pre))
    )
  )
  (setq X (strcat "X=" X))
  (setq Y (strcat "Y=" Y))
  (setvar "DimZin" DimZin_Old)

;;;根据坐标字符串长度计算引线水平段长度
;;;****************************************************************
;;;2015.12.31 本段代码在极端情况下不能实现引线与文字对齐
;;;例如X=11111.111,Y=88888.888   ,原因是各字符不等宽
;;;一般情况均适用，且理论上执行速度比更新后的代码快
;;;  (setq tbox (textbox (list
;;;                        '(0 . "text")
;;;         (cons 40 fh)
;;;         (cons 1
;;;           (if (>= (strlen X) (strlen Y))
;;;                            X
;;;                            Y
;;;           )
;;;                        )
;;;                      )
;;;             )
;;;  )
;;;  (setq sl (- (car (cadr tbox)) (car (car tbox))))
;;;****************************************************************
;;;以下为更新后的代码，任何情况都能实现引线与文字对齐
  (setq  tbox_x (textbox  (list
        '(0 . "text")
        (cons 40 fh)
        (cons 1 X)
        (cons 7 (getvar "textstyle"))
      )
         )
  )
  (setq  tbox_y (textbox  (list
        '(0 . "text")
        (cons 40 fh)
        (cons 1 Y)
        (cons 7 (getvar "textstyle"))
      )
         )
  )
  (if (>= (car (cadr tbox_x)) (car (cadr tbox_y)))
    (setq sl (car (cadr tbox_x)))
    (setq sl (car (cadr tbox_y)))
  )

;;;临时指定文字插入点并计算各个插入点初始值，在程序运行中不断更新
  (setq pb pa)
  (poi_calc sl)
  (princ "\n指定文字插入点：")

;;;输出文字
  (prin_mtxt ta x fh 7)
  (setq engl_t1 (entget (entlast)))
  (prin_mtxt tb y fh 1)
  (setq engl_t2 (entget (entlast)))

;;;画引线
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
          ;画引线：先删除，再重绘
;;;****************************************************************
;;;××××××不知如何更新多段线多个顶点值，暂如此处理××××××
;;;****************************************************************
      (entdel (cdr (car engl_pl)))
      (prin_pl pa pb pc)
      (setq engl_pl (entget (entlast)))
    )
  )

  (prin1)
)

;;;计算文字插入点和引线末点函数
(defun poi_calc  (sl)
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

;;;绘制文字函数
(defun prin_mtxt (point txt fh position)
  (entmake (list
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
  (entmake (list '(0 . "LWPOLYLINE")
     '(100 . "AcDbEntity")
     '(100 . "AcDbPolyline")
     (cons 90 3)
     (cons 10 p1)
     (cons 10 p2)
     (cons 10 p3)
     )
  )
)