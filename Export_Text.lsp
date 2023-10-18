;;;ȫ�ֱ���_ET_file_name_, _ET_mode_
(princ "\n�������ET")
(princ "  ���ܣ���ѡ������ֵ������ı��ĵ�(*.txt)")
(princ "  ���ߣ�Thinker(ThinkerHua@hotmail.com)")
(princ)
(defun c:ET (/ default_file_name information err_msg file selction selction_name selction_type 
             selction_data txt option mode_old
            ) 
    (defun *Error* (msg) 
        (if (= msg "quit / exit abort") 
            (if (/= err_msg nil) (princ err_msg)) ;�����˳�ʱ������Զ��������Ϣ
            (progn (princ (strcat "\nError:" msg))  ;����������ʱ�����������Ϣ
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
    (setq information (strcat "��ǰ������  �����ļ�=" 
                              _ET_file_name_
                              "  д��ģʽ="
                              (cond 
                                  ((= _ET_mode_ "A") "׷��")
                                  ((= _ET_mode_ "W") "����")
                              )
                      )
    )
    (princ information)
    (setq selction (ssget "I" '((0 . "TEXT,MTEXT")))) ;��ѡ������ִ��
    (if (= selction nil) 
        (setq loop T)
        (setq loop nil)
    )
    (while (= loop T)  ;��ִ����ѡ����
        (initget 0 "S F M")
        (setq option (getkword 
                         "\nѡ�������[ѡ������(S)/�����ļ�(F)/д��ģʽ(M)/]<S>:"
                     )
        )
        (cond 
            ((or (= option nil) (= option "S"))
             (setq loop nil)
             (setq selction (ssget '((0 . "TEXT,MTEXT"))))
            )
            ((= option "F")
             (setq _ET_file_name_ (getfiled "ָ�������ļ�" _ET_file_name_ "txt" 1))
             (if (= _ET_file_name_ nil) 
                 (progn 
                     (setq _ET_file_name_ default_file_name)
                     (princ (strcat "\nδָ�������ļ�������ΪĬ���ļ���" _ET_file_name_))
                 )
                 (princ (strcat "\nָ�������ļ���" _ET_file_name_))
             )
            )
            ((= option "M")
             (initget 0 "A W")
             (setq mode_old  _ET_mode_
                   _ET_mode_ (getkword (strcat "\nд��ģʽ��[׷��(A)/����(W)]<" _ET_mode_ ">��"))
             )
             (if (= _ET_mode_ nil) (setq _ET_mode_ mode_old))
             (princ 
                 (strcat "\n��ǰд��ģʽ��" 
                         (cond 
                             ((= _ET_mode_ "A") "׷��")
                             ((= _ET_mode_ "W") "����")
                         )
                 )
             )
            )
            (T)
        )
    )
    (if (= selction nil) 
        (progn (setq err_msg "\nѡ��Ϊ��!") (quit))
    )
    (setq file (open _ET_file_name_ _ET_mode_))
    (if (= file nil) 
        (progn (setq err_msg "\n�ļ���ʧ��!") (quit))
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
    (princ (strcat "\n��ɣ��������ļ�: " _ET_file_name_))
    (prin1)
)
