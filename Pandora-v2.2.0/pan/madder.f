      subroutine MADDER
     $(LIMP,N,TE,XNE,XNP,HNI)
C
C     Rudolf Loeser, 1981 May 29
C---- Computes population of level LIMP of Hydrogen.
C     !DASH
      save
C     !DASH
      real*8 CON23, CON5, E, ELL2, FIVE, HNI, R, RAT, RT, RYDBRG, S, TE,
     $       XNE, XNP, ZERO
      integer I, LIMP, LSMP, N
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 4),RYDBRG)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 6),FIVE  )
C     !DASH
C     !EJECT
      external RIGEL, DIVIDE, HI, BYE
C
C               TE(N), XNE(N), XNP(N), HNI(N,LIMP)
      dimension TE(*), XNE(*), XNP(*), HNI(N,*)
C
      data LSMP /0/
C
      call HI ('MADDER')
C     !BEG
      if(LSMP.ne.LIMP) then
C----   Initialization of R and S
        LSMP = LIMP
C
        ELL2 = LIMP**2
        call RIGEL    (23, CON23)
        R = CON23*ELL2
C
        call RIGEL    ( 5, CON5)
        S = CON5/(ELL2*RYDBRG)
      end if
C
C---- Compute
      do 100 I = 1,N
        if(TE(I).gt.FIVE) then
          RT = sqrt(TE(I))
          E  = exp(S/TE(I))
          call DIVIDE (E, (RT**3), RAT)
          HNI(I,LIMP) = R*XNE(I)*XNP(I)*RAT
C
        else
          HNI(I,LIMP) = ZERO
        end if
  100 continue
C     !END
      call BYE ('MADDER')
C
      return
      end
