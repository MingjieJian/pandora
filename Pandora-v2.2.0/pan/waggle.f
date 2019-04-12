      subroutine WAGGLE
     $(J,ML,N,LIMP,BD)
C
C     Rudolf Loeser, 1978 Sep 21
C---- Computes default BD-J, J .gt. NL.
C     !DASH
      save
C     !DASH
      real*8 BD, F1, F2, F3, R, ZERO
      integer I, IQPBS, J, LIMP, ML, N, NL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
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
      equivalence (IQQ(158),IQPBS)
C     !DASH
      external  DIVIDE, MOVE1, HI, BYE
      intrinsic max
C
C               BD(N,LIMP)
      dimension BD(N,*)
C     !EJECT
C
      call HI ('WAGGLE')
C     !BEG
      NL = max(ML,1)
      if(IQPBS.le.0) then
        F1 = J-NL
        F2 = LIMP-NL
        F3 = LIMP-J
        do 100 I = 1,N
          if(BD(I,J).lt.ZERO) then
            R = F3*BD(I,NL)+F1
            call DIVIDE (R,F2,BD(I,J))
          end if
  100   continue
      else
        call MOVE1      (BD(1,NL),N,BD(1,J))
      end if
C     !END
      call BYE ('WAGGLE')
C
      return
      end
