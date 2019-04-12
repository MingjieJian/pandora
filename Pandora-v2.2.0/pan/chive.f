      subroutine CHIVE
     $(IMAGE,RADI,NRAD,IS,SINT,J)
C
C     Rudolf Loeser, 1982 Apr 12
C---- Enters plot points, for AMASIA.
C     !DASH
      save
C     !DASH
      real*8 RADI, SINT, X, Y, ZERO
      integer I, IS, J, LINC, NRAD
      character IMAGE*(*)
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
C     !DASH
      external BEIGE, HI, BYE
C
C               RADI(NRAD), SINT(NRAD)
      dimension RADI(*),    SINT(*)
C
      call HI ('CHIVE')
C     !BEG
      LINC = 1
      do 100 I = IS,NRAD
        if(SINT(I).gt.ZERO) then
          X = log10(RADI(I))
          Y = log10(SINT(I))
          call BEIGE  (IMAGE, X, Y, ALPHS(J), LINC)
        end if
  100 continue
C     !END
      call BYE ('CHIVE')
C
      return
      end
