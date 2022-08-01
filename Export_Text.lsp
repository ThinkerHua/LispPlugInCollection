;;;全局变量_ET_file_name_, _ET_mode_
(princ "\n启动命令：ET")
(princ "  功能：将选择的文字导出到文本文档(*.txt)")
(princ "  作者：Thinker(ThinkerHua@hotmail.com)")
(princ)
(defun c:ET (/ default_file_name information err_msg file selction selction_name selction_type 
             selction_data txt option mode_old
            ) 
    (defun *Error* (msg) 
        (if (= msg "quit / exit abort") 
            (if (/= err_msg nil) (princ err_msg)) ;主动退出时，输出自定义错误信息
            (progn (princ (strcat "\nError:" msg))  ;其他错误发生时，输出错误信息
                   (setq *Error* nil)
            )
        )
    )
    (setq default_file_name (strcat (getenv "userprofile") 
                                    "\\Documents\\exported.txt"
                            )
    )
    (if (= _ET_file_name_ nil) 
        (setq _ET_file_name_ default_file_name)
    )
    (if (= _ET_mode_ nil) 
        (setq _ET_mode_ "A")
    )
    (setq information (strcat "当前参数：  导出文件=" 
                              _ET_file_name_
                              "  写入模式="
                              (cond 
                                  ((= _ET_mode_ "A") "追加")
                                  ((= _ET_mode_ "W") "覆盖")
                              )
                      )
    )
    (princ information)
    (setq selction (ssget "I" '((0 . "TEXT,MTEXT")))) ;先选文字再执行
    (if (= selction nil) 
        (setq loop T)
        (setq loop nil)
    )
    (while (= loop T)  ;先执行再选文字
        (initget 0 "S F M")
        (setq option (getkword 
                         "\n选择操作：[选择文字(S)/导出文件(F)/写入模式(M)/]<S>:"
                     )
        )
        (cond 
            ((or (= option nil) (= option "S"))
             (setq loop nil)
             (setq selction (ssget '((0 . "TEXT,MTEXT"))))
            )
            ((= option "F")
             (setq _ET_file_name_ (getfiled "指定导出文件" _ET_file_name_ "txt" 1))
             (if (= _ET_file_name_ nil) 
                 (progn 
                     (setq _ET_file_name_ default_file_name)
                     (princ (strcat "\n未指定导出文件，设置为默认文件：" _ET_file_name_))
                 )
                 (princ (strcat "\n指定导出文件：" _ET_file_name_))
             )
            )
            ((= option "M")
             (initget 0 "A W")
             (setq mode_old  _ET_mode_
                   _ET_mode_ (getkword (strcat "\n写入模式：[追加(A)/覆盖(W)]<" _ET_mode_ ">："))
             )
             (if (= _ET_mode_ nil) (setq _ET_mode_ mode_old))
             (princ 
                 (strcat "\n当前写入模式：" 
                         (cond 
                             ((= _ET_mode_ "A") "追加")
                             ((= _ET_mode_ "W") "覆盖")
                         )
                 )
             )
            )
            (T)
        )
    )
    (if (= selction nil) 
        (progn (setq err_msg "\n选择集为空!") (quit))
    )
    (setq file (open _ET_file_name_ _ET_mode_))
    (if (= file nil) 
        (progn (setq err_msg "\n文件打开失败!") (quit))
    )
    (setq i 0)
    (repeat (sslength selction) 
        (setq selction_name (ssname selction i))
        (setq selction_data (entget selction_name))
        (setq selction_type (cdr (assoc 0 selction_data)))
        (if (or (= selction_type "TEXT") (= selction_type "MTEXT")) 
            (progn (setq txt (cdr (assoc 1 selction_data))) 
                   (princ txt file)
                   (princ "\n" file)
            )
        )
        (setq i (1+ i))
    )
    (close file)
    (princ (strcat "\n完成，导出到文件: " _ET_file_name_))
    (prin1)
)
