pro MakeExeShield_NM_CT
  thisPath=FILE_DIRNAME(ROUTINE_FILEPATH('Shield_NM_CT'))+'\'
  exePath='I:\Felles\Medisinsk service\FysikkDiagnostikk\Utstyr_Software\Software\IDL_programmer\Exe\'
  MAKE_RT, 'Shield_NM_CT', exePath, /OVERWRITE, SAVEFILE=thisPath+'Shield_NM_CT.sav', /VM, /WIN32
end

;----------------------
;
;
;Shield_NM_CT.ini (change show to false)
;[DIALOG]
;Show=FALSE
;BackColor=&H6B1F29
;Caption=IDL Virtual Machine Application
;Picture=.\splash.bmp
;DefaultAction=.\IDL71\bin\bin.x86\idlrt.exe -vm=Shield_NM_CT.sav
;
;[BUTTON1]
;Show=True
;Caption=TestObjGraphics
;Action=.\IDL71\bin\bin.x86\idlrt.exe -vm=Shield_NM_CT.sav
;
;[BUTTON2]
;Show=True
;Caption=Exit
;Action=Exit
;
;[BUTTON3]
;Show=False
;Caption=
;Action=
;
;[BUTTON4]
;Show=False
;Caption=
;Action=
;-----------------------------
;autorun.inf
;[autorun]
;open = Shield_NM_CT.exe
;icon= Shield_NM_CT.ico


