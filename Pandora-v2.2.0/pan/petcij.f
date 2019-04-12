      subroutine PETCIJ
     $(NO,N,NL,CIJ,FCE,KIJ)
C
C     Rudolf Loeser, 1978 Nov 14
C---- Prints, for SETCIJ.
C     !DASH
      save
C     !DASH
      real*8 CIJ, FCE
      integer IQCIJ, KIJ, N, NL, NO
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
      equivalence (IQQ(278),IQCIJ)
C     !DASH
      external PRIAM, STENO, LINER, FENSTER, HI, BYE
C
C               CIJ(N,NL,NL), KIJ(NL,NL), FCE(N,NT)
      dimension CIJ(*),       KIJ(*),     FCE(*)
C
      call HI ('PETCIJ')
C     !BEG
      if((NO.gt.0).and.(IQCIJ.gt.0)) then
        call PRIAM   (NO, 'CIJ', 3)
C
        write (NO,100)
  100   format(' ','Collisional Transition Rates')
        call STENO   (NO, N, NL, CIJ)
C
        call FENSTER (N, NL, KIJ, FCE, NO)
      end if
C     !END
      call BYE ('PETCIJ')
C
      return
      end
