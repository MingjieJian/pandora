      subroutine XIAN
     $(WVL,DNU,CDW,A,N)
C
C     Rudolf Loeser, 2004 Apr 23
C---- Converts wavelengths (Angstroms) to XI for a particular line.
C     !DASH
      save
C     !DASH
      real*8 A, CDW, CON16, DNU, OCDW, ONE, TERM, WVL
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external RIGEL, DIVIDE, HI, BYE
C
      dimension A(*)
C
      call HI ('XIAN')
C     !BEG
      call RIGEL  (16, CON16)
      call DIVIDE (CON16, (CDW*DNU), TERM)
      call DIVIDE (ONE, CDW, OCDW)
C
      do 100 I = 1,N
        A(I) = A(I)*OCDW-TERM
  100 continue
C     !END
      call BYE ('XIAN')
C
      return
      end
