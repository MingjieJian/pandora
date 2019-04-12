      subroutine DICEROS
     $(N,NL,AMASS,TE,XNU,EQN,WC)
C
C     Rudolf Loeser, 1991 May 07
C---- Computes EQN and WC for OPHIR.
C     !DASH
      save
C     !DASH
      real*8 AMASS, EQN, FAC, ONE, RMASS, RNU, RT, TE, TERM, WC, XNU,
     $       XNUK
      integer I, J, N, NL
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ(  9),XNUK )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external  DIVIDE, HI, BYE
      intrinsic abs
C
C               TE(N), EQN(NL), WC(N,NL), XNU(NSL)
      dimension TE(*), EQN(*),  WC(N,*),  XNU(*)
C
      data FAC /2.349D-4/
C
      call HI ('DICEROS')
C     !BEG
      do 100 J = 1,NL
        call DIVIDE ((XNUK-XNU(1)), abs(XNUK-XNU(J)), RNU)
        EQN(J) = sqrt(RNU)
  100 continue
C
      RMASS = sqrt(AMASS/(AMASS+ONE))
C
      do 102 I = 1,N
        RT   = sqrt(TE(I))
        TERM = (FAC/RMASS)*RT
C
        do 101 J = 1,NL
          WC(I,J)= TERM*EQN(J)
  101   continue
  102 continue
C     !END
      call BYE ('DICEROS')
C
      return
      end
