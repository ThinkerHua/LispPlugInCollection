(defun c:ET	(/				default_file_name
			 file_name		mode		   information
			 file			selction	   selction_name
			 selction_type	selction_data  txt
			 option
			)
	(setq default_file_name
			 (strcat (getenv "userprofile")
					 "\\Documents\\exported.txt"
			 )
	)
	(if	(= file_name nil)
		(setq file_name default_file_name)
	)
	(if	(= mode nil)
		(setq mode "A")
	)
	(setq information
			 (strcat
				 "******作者：Thinker(ThinkerHua@hotmail.com)******"
				 "\n导出文件："
				 file_name
				 "\n写入模式："
				 (cond
					 ((= mode "A") "追加")
					 ((= mode "W") "覆盖")
				 )
			 )
	)
	(princ information)
	(setq loop T)
	(while (= loop T)
		(initget "F M S")
		(setq option
				 (getkword "\n导出文件(F)/写入模式(M)/选取文字(S)<S>：")
		)
		(cond
			((= option "F")
			 (progn
				 (setq
					 file_name (getfiled "指定导出文件"
										 file_name
										 "txt"
										 1
							   )
				 )
				 (cond
					 ((= file_name nil)
					  (princ "\n未指定导出文件。")
					 )
					 (T (princ (strcat "\n当前导出文件：" file_name)))
				 )
			 )
			)
			((= option "M")
			 (progn
				 (initget 1 "A W")
				 (setq mode
						  (getkword	(strcat
										"\n写入模式：[追加(A)/覆盖(W)]<"
										mode
										">："
									)
						  )
				 )
				 (princ (strcat "\n当前写入模式：" mode))
			 )
			)
			((= option "S")
			 (progn
				 (setq selction (ssget '((0 . "TEXT,MTEXT"))))
				 (setq loop nil)
			 )
			)
		)
	)
	(setq file (open file_name mode))
	(if	(= file nil)
		(princ "\n文件打开失败!")
		(progn
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
			(princ (strcat "\n完成，导出到文件: " file_name))
		)
	)
	(prin1)
)