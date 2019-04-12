      subroutine LUISA
     $(NW,WAVE,FLUX,IMAGE)
C
C     Rudolf Loeser, 1982 Apr 28
C---- Enters results into plot image, for ACME.
C     !DASH
      save
C     !DASH
      real*8 FLUX, WAVE, X, Y, ZERO
      integer I, LINC, NW
      character IMAGE*(*), STAR*1
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
      equivalence (SYMBS(45),STAR  )
C     !DASH
      external LINK, HI, BYE
C
C               WAVE(NW), FLUX(NW)
      dimension WAVE(*),  FLUX(*)
C
      call HI ('LUISA')
C     !BEG
      LINC = 1
      do 100 I = 1,NW
        if((FLUX(I).le.ZERO).or.(WAVE(I).le.ZERO)) then
          LINC = 1
        else
          X = log10(WAVE(I))
          Y = log10(FLUX(I))
          call LINK (IMAGE, X, Y, STAR, LINC)
        end if
  100 continue
C     !END
      call BYE ('LUISA')
C
      return
      end
