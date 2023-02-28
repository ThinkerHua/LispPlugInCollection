;https://cadtips.cadalyst.com/tables/export-table-lines-and-text-excel
;by Juan Villarreal

;---------------------------------------------------------------------------------------------------------------------------------
;-------------------------------------- GATHERING TABLE INFORMATION ------------------------------------
;---------------------------------------------------------------------------------------------------------------------------------
(defun tableinfo (ss / n entlist) 
    (setq n 0)
    (repeat (sslength ss) 
        (setq entlist (entget (ssname ss n)))
        (cond 
            ((member (cdr (assoc 0 entlist)) '("LINE" "POLYLINE"))
             (getlinepts entlist)
             (setq linelist (cons (ssname ss n) linelist))
            )
            ((member (cdr (assoc 0 entlist)) '("TEXT" "MTEXT"))
             (setq textlist (cons (ssname ss n) textlist))
            )
            ((member (cdr (assoc 0 entlist)) '("INSERT"))
             (setq blocklist (cons (ssname ss n) blocklist))
            )
        )
        (setq n (1+ n))
    )
)
;-------------------------- Cell Count/Height/Width Determination ----------------------
;;Gathers x and y positions of lines and polylines in separate lists
;;This is used to determine height/width & # of rows/columns
;;Line info must be gathered first in order to determine
;;cell position of any other gathered information
;---------------------------------------------------------------------------------------
(defun getlinepts (alist / x xpt ypt) 
    (foreach x alist 
        (if (member (car x) '(10 11)) 
            (progn 
                (if (not (vl-position (setq xpt (atof (rtos (car (trans (cdr x) 0 1)) 2 2))) lpxlist)) 
                    (setq lpxlist (cons xpt lpxlist))
                )
                (if (not (vl-position (setq ypt (atof (rtos (cadr (trans (cdr x) 0 1)) 2 2))) lpylist)) 
                    (setq lpylist (cons ypt lpylist))
                )
            )
            ;研读注记 by Thinker at 2022/08/04：此处可优化避免重复求值，提高效率
        )
    )
);defun
;---------------------------- Text Info and Cell Position -----------------------------------------------------
;;Determine cell position by insertionpoint of text objects
;;(Using text center is probably more reliable)
;;Create list of indexed lists containing "Order", "Position", "Content", "Height", "Rotation", "StyleName" and "TrueColor"
;;to be used to fill acad table after creation
;;If row and column is already in list, replace with combined string
;--------------------------------------------------------------------------------------------------------------
(defun gettxtinfo (alist / x vlaobj pos rpos cpos expos) 
    (setq vlaobj (vlax-ename->vla-object txt) ;研读注记 by Thinker at 2022/08/04：txt哪来的？全局变量？
          pos    (trans (midp vlaobj) 0 1) ;Midpoint
           ;研读注记 by Thinker at 2022/08/04：求文本中心点？
          rpos   (1- (vl-position (cadr pos) (vl-sort (cons (cadr pos) lpylist) '>))) ;Row Position
           ;研读注记 by Thinker at 2022/08/04：有必要将文本中心点组进列表？
           ;CAD Y轴从下往上，EXCEL Y轴（行号）从上往下，所以降序排列
          cpos   (1- (vl-position (car pos) (vl-sort (cons (car pos) lpxlist) '<))) ;Column Position
    )
    (if (setq expos (vl-position (list rpos cpos) (mapcar '(lambda (x) (cdr (assoc "Position" x))) tinfo)))  ;if cell is taken
        (setq tinfo (replace 
                        tinfo
                        expos
                        (replace 
                            (nth expos tinfo)
                            2
                            (cons "Content" 
                                  (if (> (cadr pos) (cdr (assoc "Order" (nth expos tinfo))))  ;in order according to y position
                                      (strcat (vla-fieldcode vlaobj) " " (cdr (assoc "Content" (nth expos tinfo))))
                                      (strcat (cdr (assoc "Content" (nth expos tinfo))) " " (vla-fieldcode vlaobj))
                                  )
                            )
                        )
                    )
        )
        (setq tinfo (cons 
                        (list 
                            (Cons "Order" (cadr pos))
                            (Cons "Position" (list rpos cpos)) ;Position
                            (Cons "Content" (vla-fieldcode vlaobj)) ;Content
                            (Cons "Height" (vla-get-height vlaobj))
                            (Cons "Rotation" (vla-get-rotation vlaobj))
                            (Cons "StyleName" (vla-get-StyleName vlaobj))
                            (Cons "TrueColor" 
                                  (if (= (vla-get-colorindex (vla-get-truecolor vlaobj)) 256) 
                                      (vla-get-truecolor 
                                          (vla-item 
                                              (vla-get-layers ActDoc)
                                              (vla-get-layer vlaobj)
                                          )
                                      )
                                      (vla-get-truecolor vlaobj)
                                  )
                            )
                        )
                        tinfo
                    )
        )
    )
    ;(vla-delete vlaobj)
);defun
;--------------------------- Block Info and Cell Position -------------------------------------------------------
;;Gather block information
;;determine cell position according to insertion point
;;Create an indexed list of lists containing "Position" (row, column), "ObjID",
;;"Attributes" (attribute id, attributetextstring) and "Scale" 
;----------------------------------------------------------------------------------------------------------------
(defun getblockinfo (obj / pos rpos cpos bname objid bobj attid) 
    (if (= (type obj) 'ename) 
        (setq obj (vlax-ename->vla-object obj))
    )
    (setq pos   (trans (midp obj) 0 1)
          rpos  (1- (vl-position (cadr pos) (vl-sort (cons (cadr pos) lpylist) '>))) ;Row Position
          cpos  (1- (vl-position (car pos) (vl-sort (cons (car pos) lpxlist) '<))) ;Column Position
          bname (vla-get-name obj) ;Block Name
          bobj  (vla-item (vla-get-blocks ActDoc) bname)
    ) ;Block Vla Object
    (vlax-for i bobj  ; Foreach item in block
        (if (eq (vla-get-objectname i) "AcDbAttributeDefinition")  ;If item is an attribute
            (setq attid (append attid (list (vla-get-objectid i)))) ;List Attribute Id
        )
    )
    (setq objid (vla-get-objectid bobj)) ;Block Object Id
    (setq binfo (cons 
                    (list 
                        (Cons "Name" bname)
                        (Cons "Position" (list rpos cpos))
                        (Cons "ObjID" objid)
                        (if (= (vla-get-hasattributes obj) :vlax-true) 
                            (Cons "Attributes" 
                                  (reverse 
                                      (mapcar 
                                          '(lambda (x y) (cons y (vla-get-textstring x)))
                                          (vlax-safearray->list (variant-value (vla-getattributes obj)))
                                          attid
                                      )
                                  )
                            )
                        )
                        (Cons "Scale" (vla-get-xscalefactor obj))
                    )
                    binfo
                )
    )
)
;------------------------------------------------------------------------------------------------------------------------
;-------------------------------------------- REPLACE by Charles Alan Butler---------------------------------------------
;;Cab's replace function used in this routine to avoid overwriting cells and to update cell merge lists
;------------------------------------------------------------------------------------------------------------------------
(defun replace (lst i itm) 
    (setq i (1+ i))
    (mapcar 
        '(lambda (x) 
             (if (zerop (setq i (1- i))) itm x)
         )
        lst
    )
)

;-------------------------Midpoint-----------------
(defun midp (obj / ObjLl ObjUr) 
    (vla-GetBoundingBox obj 'ObjLl 'ObjUr)
    (mapcar 
        '(lambda (a b) (/ (+ a b) 2.0))
        (safearray-value ObjLl)
        (safearray-value ObjUr)
    )
)

;-------------------------Q&D Number Accumulation---------------------------
;Used in this routine for polar distances to determine which cells to merge.
;;Recursive function possible. Ask Gile (recursion master) if desired.
(defun acnumlist (nlist / acnlist) 
    (repeat (length nlist) 
        (setq acnlist (cons (apply '+ nlist) acnlist)
              nlist   (reverse (cdr (reverse nlist)))
        )
    )
    acnlist
)
;--------------------------------------------------------------------------
;; ?Remove_nth ? (Lee Mac)          ;;
;; ~ Removes the nth item in a list.  ;;

(defun Remove_nth (i lst / j) 
    (setq j -1)
    (vl-remove-if 
        (function 
            (lambda (x) 
                (eq i (setq j (1+ j)))
            )
        )
        lst
    )
)


 ;;; private function (fixo)
(defun setcelltext (cells row column value) 
    (vl-catch-all-apply 
        'vlax-put-property
        (list cells 
              'Item
              row
              column
              (vlax-make-variant 
                  (vl-princ-to-string value)
                  8
              )
        )
    )
)
(defun setgridlines (xlapp range)  ;(fixo)
    ;; select the range:
    (vlax-invoke-method range 'Select)
    ;; get excel application selection property:
    (setq range (vlax-get-property xlapp 'Selection))
    ;; get selection borders
    (setq borders (vlax-get-property range 'Borders))
    ;; iterate through all edges of the selection
    (setq cnt 0)
    (vlax-for a borders 
        (setq cnt (1+ cnt))
        (vl-catch-all-apply 
            (function 
                (lambda () 
                    (progn 
                        (if (< cnt 5) 
                            (progn 
                                (vlax-put-property 
                                    a
                                    'LineStyle
                                    (vlax-make-variant 1 3)
                                ) ; single line style
                                (vlax-put-property 
                                    a
                                    'Weight
                                    (vlax-make-variant 2 3)
                                ) ;  lines
                                (vlax-put-property 
                                    a
                                    'ColorIndex
                                    (vlax-make-variant 1 5)
                                )
                            ) ; color black

                            ;; turn off the diagonal lines:
                            (vlax-put-property a 'LineStyle (vlax-make-variant -4142 3))
                        )
                    )
                )
            )
        )
    )
    (princ)
)

(defun conexcelcolumn (/ a b list1)  ;(Q. J. Chen)
    (setq a 65)
    (setq list1 nil)
    (repeat 26 
        (setq list1 (append 
                        list1
                        (list (chr a))
                    )
        )
        (setq a (1+ a))
    )
    (setq a 65)
    (repeat 26 
        (setq b 65)
        (repeat 26 
            (setq list1 (append 
                            list1
                            (list (strcat (chr a) (chr b)))
                        )
            )
            (setq b (1+ b))
        )
        (setq a (1+ a))
    )

    list1
)

  
 ;;; private function
 ;;;  apply props
(defun Orient (xlrange) 
    (mapcar 
        '(lambda (prop value) 
             (vl-catch-all-apply 
                 'vlax-put-property
                 (list xlrange 
                       prop
                       value
                 )
             )
         )

        (list 'HorizontalAlignment 'VerticalAlignment 'Orientation)

        (list -4143 -4108 (cvunit (cdr (assoc "Rotation" x)) "radian" "degree"))
    )
)

;---------------------------------------------------------------------------------------------------------------------
;------------------------------------------- CONVERT OLD TABLE ROUTINE -----------------------------------------------
;---------------------------------------------------------------------------------------------------------------------
(defun c:TE (/ ActDoc *error* orerror otcontents textlist colwidths i mlist p0 hmergelist2 vmergelist2 *Space* lpxlist lpylist 
             tinfo cwidths check tstyle spos newstring tstylelst blocklist rowheights selsets tstylelst2 tstylelst3 kword 
             linelist binfo rheights hmergelist vmergelist ssitem tblobj mb colorlst colorlst2 th tr ts tc newstyle RowTypes a 
             acapp acsp address adoc atable borders cnt col data_list fname font prop release row selrange sset txt_list urange 
             value xlapp xlbook xlbooks xlcells xlrange xlsheet xlsheets
            ) 

    (vl-load-com)
    (setq oerror *error*)
    (defun *error* (msg) 
        (princ (strcat "\n<" msg ">\n"))
        (mapcar '(lambda (x) (and x (not (vlax-object-released-p x)) (vlax-release-object x))) (list ssitem))
        (setq *error* oerror)
        (setvar 'nomutt 0)
        (vla-EndUndoMark ActDoc)
        (princ)
    ) ;defun *error*
    (setq ActDoc  (vla-get-activedocument (vlax-get-acad-object))
          *Space* (vlax-get-property ActDoc (nth (vla-get-ActiveSpace ActDoc) '("PaperSpace" "ModelSpace")))
    )

    (vla-EndUndoMark ActDoc)
    (vla-StartUndoMark ActDoc)

    (setq otcontents (ssget))
    (command "._zoom" "object" otcontents "")
    (princ "\nSorting Line Info...")
    (tableinfo otcontents)
    (setq lpxlist (vl-sort lpxlist '<)
          lpylist (vl-sort lpylist '>)
    )
    (princ "\nSorting Text Info...")
    (mapcar '(lambda (txt) (gettxtinfo (entget txt)) (redraw txt 2)) textlist) ;;using redraw function To avoid interference
    (princ "\nSorting Block Info...")
    (mapcar '(lambda (blk) (getblockinfo blk)) blocklist)
    (setq colwidths  (mapcar '(lambda (x) (- (nth (1+ (vl-position x lpxlist)) lpxlist) x)) 
                             (reverse (cdr (reverse lpxlist)))
                     )
          rowheights (mapcar '(lambda (x) (- x (nth (1+ (vl-position x lpylist)) lpylist))) 
                             (reverse (cdr (reverse lpylist)))
                     )
    )
    (setq p0 (vlax-3d-point (trans (list (car lpxlist) (car lpylist) 0.0) 1 0))) ;;<---Table Placement (Currently using Top Left corner)
    (progn 
        (princ "\nSearching for merged cells...")
        (princ)
        (setvar 'nomutt 1)
        ;-----------------------------------Method to determine which cells to merge--------------------------------------------
        ;Method fails if missed selection is not possible at zoom level.
        ;To determine which cells to merge, a selection at point is used.
        ;For each row, a selection is attempted at each vertical line at row's center.
        ;If no selection is made, the point is at the center or left of horizontally merged cells.
        ;For each column, a selection is attempted at each horizontal line at column's center.
        ;If no selection is made, the point is at the center or upper region of vertically merged cells.
        ;Continuation of merging is determined by a 'consecutive miss'.
        ;When a 'consecutive miss' is made, max column/row item is replaced by the next column/row.
        ;-----------------------------------------------------------------------------------------------------------------------
        (setq selsets (vla-get-selectionsets ActDoc))
        (vl-catch-all-error-p (vl-catch-all-apply 'vla-add (list selsets "InxCheckSet")))
        (setq ssitem   (vla-item selsets "InxCheckSet")
              cwidths  (acnumlist colwidths)
              rheights (acnumlist rowheights)
        ) ;;col widths & row heights accumulated for polar use
        (mapcar 
            '(lambda (pt rh) 
                 (mapcar 
                     '(lambda (x) 
                          (vl-catch-all-error-p (vl-catch-all-apply 'vla-clear (list ssitem)))
                          (vla-selectatpoint ssitem (vlax-3d-point (polar (list (car lpxlist) (+ pt (/ rh 2)) 0.0) 0 x)))
                          (if (zerop (vla-get-count ssitem)) 
                              (if check 
                                  (setq hmergelist (replace hmergelist 0 (replace mlist 3 (1+ (vl-position x cwidths)))))
                                  (setq hmergelist (cons 
                                                       (setq mlist (list 
                                                                       (1- (vl-position pt lpylist))
                                                                       (vl-position x cwidths)
                                                                       (1- (vl-position pt lpylist))
                                                                       (1+ (vl-position x cwidths))
                                                                   )
                                                       )
                                                       hmergelist
                                                   )
                                        check      T
                                  )
                              ) ;if
                              (setq check nil
                                    mlist nil
                              )
                          )
                      ) ;lambda
                     cwidths
                 ) ;mapcar
             ) ;lambda
            (member (nth 1 lpylist) lpylist)
            rowheights
        ) ;mapcar

        (mapcar 
            '(lambda (pt cw) 
                 (mapcar 
                     '(lambda (x) 
                          (vl-catch-all-error-p (vl-catch-all-apply 'vla-clear (list ssitem)))
                          (vla-selectatpoint ssitem 
                                             (vlax-3d-point (polar (list (+ pt (/ cw 2)) (car lpylist) 0.0) (* pi 1.5) x))
                          )
                          (if (zerop (vla-get-count ssitem)) 
                              (if check 
                                  (setq vmergelist (replace vmergelist 0 (replace mlist 2 (1+ (vl-position x rheights)))))
                                  (setq vmergelist (cons 
                                                       (setq mlist (list 
                                                                       (vl-position x rheights)
                                                                       (vl-position pt lpxlist)
                                                                       (1+ (vl-position x rheights))
                                                                       (vl-position pt lpxlist)
                                                                   )
                                                       )
                                                       vmergelist
                                                   )
                                        check      T
                                  )
                              ) ;if
                              (setq check nil
                                    mlist nil
                              )
                          )
                      ) ;lambda
                     rheights
                 ) ;mapcar
             ) ;lambda
            lpxlist
            colwidths
        ) ;mapcar

        (setvar 'nomutt 0)
    ) ;progn
    (setq hmergelist2 (mapcar '(lambda (b) (list (car b) (cadr b))) hmergelist))
    (setq vmergelist2 (mapcar '(lambda (b) (list (car b) (cadr b))) vmergelist))

    (mapcar 
        '(lambda (a / expos) 
             (if (setq expos (vl-position (list (car a) (cadr a)) vmergelist2)) 
                 (setq dmergelist (cons (list (car a) (cadr a) (caddr (nth expos vmergelist)) (cadddr a)) dmergelist))
             )
         )
        hmergelist
    )

    (setq xlapp    (vlax-get-or-create-object "Excel.Application") ;(fixo)
          xlbooks  (vlax-get-property xlapp 'Workbooks)
          xlbook   (vlax-invoke-method xlbooks 'Add)
          xlsheets (vlax-get-property xlbook 'Sheets)
          xlsheet  (vlax-get-property xlsheets 'Item 1)
          xlcells  (vlax-get-property xlsheet 'Cells)
    )
    (vla-put-visible xlapp :vlax-true)
    (vlax-invoke-method xlsheet "Activate")
    (setq ecol (conexcelcolumn))
    ;place text
    (mapcar 
        '(lambda (x / r c xlrange) 
             (setq r (1+ (cadr (assoc "Position" x)))
                   c (1+ (caddr (assoc "Position" x)))
             )
             (setcelltext xlcells r c (cdr (assoc "Content" x)))
             (setq xlRange (vlax-get-property xlsheet "Range" (strcat (nth (1- c) ecol) (itoa r))))
             (vlax-invoke-method xlRange "Select")
             (setq xlRange (vlax-get-property xlapp "Selection"))
             (Orient xlrange)
         )
        tinfo
    )
    ;place block info
    (mapcar 
        '(lambda (x / r c bstring) 
             (setq r (1+ (cadr (assoc "Position" x)))
                   c (1+ (caddr (assoc "Position" x)))
             )
             (setq bstring "")
             (if (cdr (assoc "Attributes" x)) 
                 (progn 
                     (mapcar 
                         '(lambda (y) 
                              (setq bstring (strcat ":" (cdr y) bstring))
                          )
                         (cdr (assoc "Attributes" x))
                     )
                     (setcelltext xlcells r c (strcat "Block:" (cdr (assoc "Name" x)) bstring))
                 )
             ) ;if
         )
        binfo
    )
    ;merge cells



    (princ "\nProcessing Merge Info")
    ;-------------------------------------------------------------------------------------------------------------------------
    (defun convertlist (mrglist / newmrglist) 
        (foreach x mrglist 
            (setq newmrglist (append newmrglist 
                                     (list 
                                         (strcat (nth (cadr x) ecol) 
                                                 (itoa (1+ (car x)))
                                                 ":"
                                                 (nth (cadddr x) ecol)
                                                 (itoa (1+ (caddr x)))
                                         )
                                     )
                             )
            )
        )
    )

    (defun applylist (mrglist / xlRange) 
        (foreach x mrglist 
            (setq xlRange (vlax-get-property xlsheet "Range" x))
            (vlax-invoke-method xlRange "Select")
            (setq xlRange (vlax-get-property xlapp "Selection"))
            (vlax-put-property xlRange "MergeCells" :vlax-true)
        )
    )

    (setq hmergelist2 (convertlist hmergelist)
          vmergelist2 (convertlist vmergelist)
    )
    (applylist hmergelist2)
    (applylist vmergelist2)
    (vlax-invoke-method 
        (vlax-get-property xlsheet 'Columns)
        'AutoFit
    )
    ;;;  align all columns to center
    (vlax-put-property 
        (setq urange (vlax-get-property xlsheet 'UsedRange))
        'HorizontalAlignment
        -4108
    )
    ;;;  draw grid lines
    (setgridlines xlapp urange)

    (mapcar 
        '(lambda (x)  ;(fixo)
             (vl-catch-all-apply 
                 '(lambda () 
                      (vlax-release-object x)
                  )
             )
         )
        (list xlcells xlsheet xlsheets xlbook xlbooks xlapp)
    )
    (setq xlapp nil)
    (gc)
    (gc)
    (gc)
    (mapcar '(lambda (x) (and x (not (vlax-object-released-p x)) (vlax-release-object x))) (list ssitem))
    (mapcar '(lambda (txt) (redraw txt 1)) textlist) ;;using redraw function again
    (setq *error* oerror)
    (vla-EndUndoMark ActDoc)
    (princ)
    (princ)
);defun

