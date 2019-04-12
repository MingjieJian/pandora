      subroutine LILTA
     $(XLM,CALLER)
C
C     Rudolf Loeser, 1978 Apr 09
C---- Prints heading for detailed CSF calculation dump.
C     !DASH
      save
C     !DASH
      real*8 XLM
      integer IQFIN, IQGDS, IQINC, IQREF, LUEO, jummy
      character CALLER*(*), FIN*3, GDS*3, INC*3, REF*3
C     !COM
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ( 51),IQINC)
      equivalence (IQQ( 50),IQREF)
      equivalence (IQQ( 49),IQFIN)
      equivalence (IQQ( 60),IQGDS)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, ONOFF, HI, BYE
C     !EJECT
C
      call HI ('LILTA')
C     !BEG
      call MESHED (CALLER, 2)
C
      call ONOFF  (IQINC, jummy, INC)
      call ONOFF  (IQREF, jummy, REF)
      call ONOFF  (IQFIN, jummy, FIN)
      call ONOFF  (IQGDS, jummy, GDS)
C
      write (LUEO,100) XLM,INC,REF,FIN,GDS
  100 format(' ','C. S. F.  dump!'//
     $       ' ','Continuum Source Function at',1PE20.10,' Angstroms'/
     $       ' ','INCIDNT = ',A,', REFLECT = ',A,', FINITE = ',A,
     $           ', GDS = ',A)
C     !END
      call BYE ('LILTA')
C
      return
      end
