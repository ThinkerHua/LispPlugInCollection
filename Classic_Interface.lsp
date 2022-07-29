;关闭命令行回显
(setvar "CMDECHO" 0)
;开始选项卡
(setvar "startmode" 1)
;启动时不新建空白图形
(setvar "startup" 3)
;关闭RIBBON界面
(command "ribbonclose")
;显示菜单栏
(setvar "MENUBAR" 1)
;打开工具栏
(command "toolbar" "标准" "Show")
(command "toolbar" "样式" "Show")
(command "toolbar" "工作空间" "Show")
(command "toolbar" "图层" "Show")
(command "toolbar" "特性" "Show")
(command "toolbar" "绘图" "Show")
(command "toolbar" "修改" "Show")
(command "toolbar" "绘图次序" "Show")
(command "workspace" "sa" "AutoCAD经典")
;关闭导航浮动工具栏
(setvar "NAVBARDISPLAY" 0)
;恢复命令行回显
(setvar "CMDECHO" 1)
(princ)