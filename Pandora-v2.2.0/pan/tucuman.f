      subroutine TUCUMAN
     $(N,NL,XNUK,XNU,P,TE,XNE,PF,SA,GM)
C
C     Rudolf Loeser, 2004 Dec 14
C---- Gets SA and GM (Hydrogen), for KALT.
C     !DASH
      save
C     !DASH
      real*8 GM, P, PART, PF, SA, TE, XNE, XNU, XNUK
      integer I, IONST, IQUVP, J, N, NL
      character QELSM*8
C     !COM  or  !DASH
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
      equivalence (IQQ(165),IQUVP)
C     !DASH
      external MANDOLIN, SAHARA, RYE, HI, BYE
C
C               GM(N,NL), TE(N), XNE(N), SA(N), PF(N), XNU(NL), P(NL)
      dimension GM(N,*),  TE(*), XNE(*), SA(*), PF(*), XNU(*),  P(*)
C
      data QELSM, IONST, PART /'H       ', 1, 0.D0/
C
      call HI ('TUCUMAN')
C     !BEG
      call MANDOLIN (N, TE, XNE, PF, QELSM, IONST, PART, IQUVP)
      do 100 I = 1,N
        call SAHARA (XNUK, TE(I), PF(I), SA(I))
  100 continue
      do 101 J = 1,NL
        call RYE    (N, P(J), XNU(J), TE, GM(1,J))
  101 continue
C     !END
      call BYE ('TUCUMAN')
C
      return
      end
