;;;虽然理论上可以为任意的关键字，
;;;但是为了保持与CAD的选择的习惯一致，
;;;在这个函数中我特意摒避了CAD内置于SSGET中的关键字~
;;;主要思路给出来了，要别的类似效果的自己改下就成！
;;;同时也请大家严格尊重用户习惯~不要乱用系统内置关键字。
(vl-load-com)
(setq *acad* (vlax-get-acad-object))
(setq *doc* (vla-get-ActiveDocument *acad*))
;;;带过滤器的entsel
(defun Fsxm-entsel (msg filter)
	(setq enp (entsel msg))
	(if	(or (= (type enp) 'str) (and enp (ssget (cadr enp) filter)))
		enp
	)
)
;;;用分隔符解释字符串成表
(defun Fsxm-Split (string strkey / po strlst xlen)
	(setq xlen (1+ (strlen strkey)))
	(while (setq po (vl-string-search strkey string))
		(setq strlst (cons (substr string 1 po) strlst))
		(setq string (substr string (+ po xlen)))
	)
	(reverse (cons string strlst))
)
;;;点化字串
(defun Pt2Str (pt)
	(strcat	(rtos (car pt) 2 20)
			","
			(rtos (cadr pt) 2 20)
			","
			(rtos (caddr pt) 2 20)
			"\n"
	)
)
;;;带关键字的ssget
(defun Fsxm-ssget (Msg Kwd Fil / Kwd0 pt var)
	(cond ((cadr (ssgetfirst)))
		  (t
		   (setq Kwd0 "W L C BOX ALL F WP CP G A R M P U AU SI")
		   (initget (strcat Kwd0 " " kwd))
		   (cond ((and (listp (setq var (fsxm-entsel Msg Fil)))
					   (/= 52 (getvar "errno"))
				  )
				  (vla-sendcommand *doc* (Pt2Str (cadr (grread t))))
				  (ssget Fil)
				 )
				 ((member var (fsxm-split Kwd0 " "))
				  (vla-sendcommand *doc* (strcat var "\n"))
				  (ssget Fil)
				 )
				 (t var)
		   )
		  )
	)
)