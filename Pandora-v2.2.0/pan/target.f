      subroutine TARGET
     $(L,XLM,TERM)
C
C     Rudolf Loeser, 1981 Jun 01
C---- Computes an intermediate term for Hydrogen bound-free opacity.
C     (This is version 2 of TARGET.)
C     !DASH
      save
C     !DASH
      real*8 TERM, XLM, ZERO
      integer KODE, L
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external DOLT, GUNNAR, HI, BYE
C
      call HI ('TARGET')
C     !BEG
      call DOLT     (L,XLM,KODE)
      if(KODE.eq.1) then
        call GUNNAR (L,XLM,TERM)
      else
        TERM = ZERO
      end if
C     !END
      call BYE ('TARGET')
C
      return
      end
