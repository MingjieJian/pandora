      subroutine OPIS
     $(ARR,QAR,KNT)
C
C     Rudolf Loeser, 1991 Jun 28
C---- Readies LSFP for printing, for LIZARD.
C     !DASH
      save
C     !DASH
      real*8 ARR, ONE, THREE
      integer I, KNT
      character QAR*10
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 4),THREE )
C     !DASH
      external GRINDLE, HI, BYE
C
C               ARR(8), QAR(16)
      dimension ARR(*), QAR(*)
C
      call HI ('OPIS')
C     !BEG
      do 100 I = 1,KNT
        if(ARR(I).eq.THREE) then
          ARR(I) = ONE
        end if
  100 continue
C
      call GRINDLE (ARR,QAR,KNT)
C     !END
      call BYE ('OPIS')
C
      return
      end
