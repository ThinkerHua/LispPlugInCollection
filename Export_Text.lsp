;;;全局变量_ET_file_name_, _ET_mode_
(vl-load-com)
(princ "\n启动命令：ET")
(princ " 文本导出到文本文档(*.txt)")
(princ " BY Thinker(ThinkerHua@hotmail.com)")
(princ)
(defun c:ET	(/				 default_file_name
			 information	 err_msg		 file
			 selction		 selction_name	 selction_type
			 selction_data	 txt			 option
			)
	(defun *Error* (msg)
		(if	(or	(= msg "Function cancelled")
				(= msg "quit / exit abort")
			)
			(princ err_msg)
			(progn
				(princ (strcat "\n出错信息：" msg))
				(setq *Error* nil)
			)
		)
	)
	(setq default_file_name
			 (strcat (getenv "userprofile")
					 "\\Documents\\exported.txt"
			 )
	)
	(if	(= _ET_file_name_ nil)
		(setq _ET_file_name_ default_file_name)
	)
	(if	(= _ET_mode_ nil)
		(setq _ET_mode_ "A")
	)
	(setq information
			 (strcat
				 "当前参数：  导出文件："
				 _ET_file_name_
				 "  写入模式："
				 (cond
					 ((= _ET_mode_ "A") "追加")
					 ((= _ET_mode_ "W") "覆盖")
				 )
			 )
	)
	(princ information)
	(setq selction
			 (ssget_with_keyword
				 "F M"
				 "\n选取文字或 [导出文件(F)/写入模式(M)] <F>："
				 '(0 . "TEXT,MTEXT")
			 )
	)
	(if	(= selction nil)
		(progn
			(setq err_msg "\n选择集为空!")
			(quit)
		)
	)
;;;	(setq loop T)
;;;	(while (= loop T)
;;;		(initget "F M S")
;;;		(setq option
;;;				 (getkword "\n导出文件(F)/写入模式(M)/选取文字(S)<S>：")
;;;		)
;;;		(cond
;;;			((= option "F")
;;;			 (progn
;;;				 (setq
;;;					 _ET_file_name_
;;;						(getfiled "指定导出文件"
;;;								  _ET_file_name_
;;;								  "txt"
;;;								  1
;;;						)
;;;				 )
;;;				 (cond
;;;					 ((= _ET_file_name_ nil)
;;;					  (princ "\n未指定导出文件。")
;;;					 )
;;;					 (T
;;;					  (princ (strcat "\n当前导出文件：" _ET_file_name_))
;;;					 )
;;;				 )
;;;			 )
;;;			)
;;;			((= option "M")
;;;			 (progn
;;;				 (initget 1 "A W")
;;;				 (setq _ET_mode_
;;;						  (getkword	(strcat
;;;										"\n写入模式：[追加(A)/覆盖(W)]<"
;;;										_ET_mode_
;;;										">："
;;;									)
;;;						  )
;;;				 )
;;;				 (princ (strcat "\n当前写入模式：" _ET_mode_))
;;;			 )
;;;			)
;;;			((= option "S")
;;;			 (progn
;;;				 (setq selction (ssget '((0 . "TEXT,MTEXT"))))
;;;				 (setq loop nil)
;;;			 )
;;;			)
;;;		)
;;;	)
	(setq file (open _ET_file_name_ _ET_mode_))
	(if	(= file nil)
		(progn
			(setq err_msg "\n文件打开失败!")
			(quit)
		)
	)
	(setq i 0)
	(repeat	(sslength selction)
		(setq selction_name (ssname selction i))
		(setq selction_data (entget selction_name))
		(setq selction_type (cdr (assoc 0 selction_data)))
		(if	(or	(= selction_type "TEXT")
				(= selction_type "MTEXT")
			)
			(progn
				(setq txt (cdr (assoc 1 selction_data)))
				(princ txt file)
				(princ "\n" file)
			)
		)
		(setq i (1+ i))
	)
	(close file)
	(princ (strcat "\n完成，导出到文件: " _ET_file_name_))
)

(defun ssget_with_keyword (key_list msg arg / sel)
	(princ msg)
;;;	(setvar 'nomutt 1)
	(if	(= arg nil)
		(setq sel (vl-catch-all-apply 'ssget))
		(setq sel (vl-catch-all-apply 'ssget (list (list arg))))
	)
;;;	(setvar 'nomutt 0)
	(if	(not (vl-catch-all-error-p sel))
		sel
	)
)
