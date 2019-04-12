      subroutine PUMA
     $(WS,TMU,N,M,YNT,YY,MUX,KODE,GO)
C
C     Rudolf Loeser, 2000 Jul 13
C---- Initializes the calculation of intensity integral, and
C     deals with basic errors, for SIMBA.
C     The meaning of the various values of KODE is explained in SIMBA.
C     !DASH
      save
C     !DASH
      real*8 TMU, TWO, WS, YNT, YY, ZERO
      integer IQFIN, KODE, M, MUX, N
      logical GO
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 3),TWO   )
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
      equivalence (IQQ( 49),IQFIN)
C     !DASH
      external ZERO1, HI, BYE
C
C               WS(N), TMU(N)
      dimension WS(*), TMU(*)
C     !EJECT
C
      call HI ('PUMA')
C     !BEG
      call ZERO1 (WS,N)
      KODE = 0
      YNT  = ZERO
      MUX  = 0
      YY   = ZERO
C
      if(M.le.3) then
        KODE = 1
      else
        if((IQFIN.le.0).and.(TMU(N).lt.TWO)) then
          KODE = 2
        end if
      end if
C
      GO = KODE.ne.1
C     !END
      call BYE ('PUMA')
C
      return
      end
