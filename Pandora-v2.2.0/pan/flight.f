      subroutine FLIGHT
     $(DL,RES,K,IMAGE,PLOT)
C
C     Rudolf Loeser, 1980 Aug 05
C---- Drives BEIGE, to enter points into a plot of line profiles.
C     (This is version 2 of FLIGHT.)
C     !DASH
      save
C     !DASH
      real*8 DL, F, RES, ZERO
      integer I, K, LINC
      character IMAGE*(*), PLOT*1
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external BEIGE, HI, BYE
C
C               DL(KM), RES(KM)
      dimension DL(*),  RES(*)
C
      call HI ('FLIGHT')
C     !BEG
      LINC = 1
      do 100 I = 1,K
        if(RES(I).gt.ZERO) then
          F = log10(RES(I))
          call BEIGE (IMAGE, DL(I), F, PLOT, LINC)
        end if
  100 continue
C     !END
      call BYE ('FLIGHT')
C
      return
      end
