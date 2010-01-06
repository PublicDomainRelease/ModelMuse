# Microsoft Developer Studio Project File - Name="MF2005_Importer" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=MF2005_Importer - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "MF2005_Importer.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "MF2005_Importer.mak" CFG="MF2005_Importer - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "MF2005_Importer - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "MF2005_Importer - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "MF2005_Importer - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /browser /compile_only /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /FR /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /stack:0x4000000 /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "MF2005_Importer - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /browser /check:bounds /compile_only /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /FR /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib DFOR.LIB /nologo /stack:0x4000000 /subsystem:console /incremental:no /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "MF2005_Importer - Win32 Release"
# Name "MF2005_Importer - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\de47.f
DEP_F90_DE47_=\
	".\Debug\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gmg7.f
DEP_F90_GMG7_=\
	".\Debug\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2bas7.f
DEP_F90_GWF2B=\
	".\openspec.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2bcf7.f
DEP_F90_GWF2BC=\
	".\Debug\GLOBAL.mod"\
	".\Debug\GWFBASMODULE.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2chd7.f
DEP_F90_GWF2C=\
	".\Debug\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2drn7.f
DEP_F90_GWF2D=\
	".\Debug\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2drt7.f
DEP_F90_GWF2DR=\
	".\Debug\GLOBAL.mod"\
	".\Debug\PARAMMODULE.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2ets7.f
DEP_F90_GWF2E=\
	".\Debug\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2evt7.f
DEP_F90_GWF2EV=\
	".\Debug\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2fhb7.f
DEP_F90_GWF2F=\
	".\Debug\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2gag7.f
DEP_F90_GWF2G=\
	".\Debug\GLOBAL.mod"\
	".\Debug\GWFLAKMODULE.mod"\
	".\Debug\GWFSFRMODULE.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2ghb7.f
DEP_F90_GWF2GH=\
	".\Debug\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2hfb7.f
DEP_F90_GWF2H=\
	".\Debug\GLOBAL.mod"\
	".\Debug\PARAMMODULE.mod"\
	".\openspec.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2huf7.f
DEP_F90_GWF2HU=\
	".\Debug\GLOBAL.mod"\
	".\Debug\GWFBASMODULE.mod"\
	".\Debug\PARAMMODULE.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2hydmod7.f
DEP_F90_GWF2HY=\
	".\Debug\GLOBAL.mod"\
	".\Debug\GWFIBSMODULE.MOD"\
	".\Debug\GWFSFRMODULE.MOD"\
	".\Debug\GWFSTRMODULE.MOD"\
	".\Debug\GWFSUBMODULE.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2ibs7.f
DEP_F90_GWF2I=\
	".\Debug\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2lak7.f
DEP_F90_GWF2L=\
	".\Debug\GLOBAL.mod"\
	".\Debug\GWFSFRMODULE.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2lpf7.f
DEP_F90_GWF2LP=\
	".\Debug\GLOBAL.mod"\
	".\Debug\GWFBASMODULE.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2mnw27.f
DEP_F90_GWF2M=\
	".\Debug\DE4MODULE.mod"\
	".\Debug\GLOBAL.mod"\
	".\Debug\GWFBASMODULE.mod"\
	".\Debug\GWFBCFMODULE.mod"\
	".\Debug\GWFHUFMODULE.mod"\
	".\Debug\GWFLPFMODULE.mod"\
	".\Debug\PCGMODULE.mod"\
	".\Debug\SIPMODULE.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2mnw2i7.f
DEP_F90_GWF2MN=\
	".\Debug\GLOBAL.mod"\
	".\Debug\GWFBASMODULE.mod"\
	
NODEP_F90_GWF2MN=\
	".\Debug\GWFMNW2MODULE.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2mnw7.f
DEP_F90_GWF2MNW=\
	".\Debug\DE4MODULE.mod"\
	".\Debug\GLOBAL.mod"\
	".\Debug\GMGMODULE.mod"\
	".\Debug\GWFBASMODULE.mod"\
	".\Debug\PCGMODULE.mod"\
	".\Debug\SIPMODULE.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2rch7.f
DEP_F90_GWF2R=\
	".\Debug\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2res7.f
DEP_F90_GWF2RE=\
	".\Debug\GLOBAL.mod"\
	".\Debug\GWFBASMODULE.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2riv7.f
DEP_F90_GWF2RI=\
	".\Debug\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2sfr7.f
DEP_F90_GWF2S=\
	".\Debug\GLOBAL.mod"\
	".\Debug\GWFBCFMODULE.mod"\
	".\Debug\GWFHUFMODULE.mod"\
	".\Debug\GWFLPFMODULE.mod"\
	".\Debug\GWFSFRMODULE.MOD"\
	".\Debug\PARAMMODULE.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2str7.f
DEP_F90_GWF2ST=\
	".\Debug\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2sub7.f
DEP_F90_GWF2SU=\
	".\Debug\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2swt7.f
DEP_F90_GWF2SW=\
	".\Debug\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2uzf1.f
DEP_F90_GWF2U=\
	".\Debug\GLOBAL.mod"\
	".\Debug\GWFBASMODULE.mod"\
	".\Debug\GWFBCFMODULE.mod"\
	".\Debug\GWFHUFMODULE.mod"\
	".\Debug\GWFLPFMODULE.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwf2wel7.f
DEP_F90_GWF2W=\
	".\Debug\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\gwfsfrmodule.f
# End Source File
# Begin Source File

SOURCE=.\hufutl7.f
DEP_F90_HUFUT=\
	".\Debug\GWFHUFMODULE.mod"\
	".\Debug\PARAMMODULE.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\lmt7.f
DEP_F90_LMT7_=\
	".\Debug\GLOBAL.mod"\
	".\openspec.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\mf2005.f
DEP_F90_MF200=\
	".\Debug\DE4MODULE.mod"\
	".\Debug\GLOBAL.mod"\
	".\Debug\GMGMODULE.mod"\
	".\Debug\GWFBASMODULE.mod"\
	".\Debug\GWFEVTMODULE.mod"\
	".\Debug\GWFHUFMODULE.mod"\
	".\Debug\GWFLAKMODULE.mod"\
	".\Debug\GWFRCHMODULE.mod"\
	".\Debug\GWFUZFMODULE.mod"\
	".\Debug\PCGMODULE.mod"\
	".\Debug\SIPMODULE.mod"\
	".\openspec.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\obs2bas7.f
DEP_F90_OBS2B=\
	".\Debug\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\obs2chd7.f
DEP_F90_OBS2C=\
	".\Debug\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\obs2drn7.f
DEP_F90_OBS2D=\
	".\Debug\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\obs2ghb7.f
DEP_F90_OBS2G=\
	".\Debug\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\obs2riv7.f
DEP_F90_OBS2R=\
	".\Debug\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\obs2str7.f
DEP_F90_OBS2S=\
	".\Debug\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\parutl7.f
DEP_F90_PARUT=\
	".\Debug\PARAMMODULE.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\pcg7.f
DEP_F90_PCG7_=\
	".\Debug\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\sip7.f
DEP_F90_SIP7_=\
	".\Debug\GLOBAL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\utl7.f
DEP_F90_UTL7_=\
	".\openspec.inc"\
	
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
