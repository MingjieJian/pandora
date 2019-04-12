      subroutine SETRIJ
     $(NO,PIJ,CIJ)
C
C     Rudolf Loeser, 1985 Feb 08
C---- Supervises printing of RIJ=CIJ+PIJ.
C     !DASH
      save
C     !DASH
      real*8 CIJ, PIJ
      integer IQRIJ, N, NL, NO
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
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
      equivalence (IQQ(189),IQRIJ)
C     !DASH
      external PRIAM, DIG, HI, BYE
C
C               PIJ(N,NL,NL), CIJ(N,NL,NL)
      dimension PIJ(*),       CIJ(*)
C
      call HI ('SETRIJ')
C     !BEG
      if((NO.gt.0).and.(IQRIJ.gt.0)) then
        call PRIAM (NO,'RIJ',3)
C
        call DIG   (NO,N,NL,CIJ,PIJ)
      end if
C     !END
      call BYE ('SETRIJ')
C
      return
      end
