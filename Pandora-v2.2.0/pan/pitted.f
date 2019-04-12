      subroutine PITTED
     $(N,Z,TE,JS,CTA,CTS,NO)
C
C     Rudolf Loeser, 2003 Mar 21
C---- Add shock temperature variation to TE.
C     !DASH
      save
C     !DASH
      real*8 ARG, CTA, CTS, DTE, TE, TRM, TSAV, Z, ZERO
      integer I, JS, N, NO
      logical PRINT
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external ABJECT, LINER, SHIM, HI, BYE
C
C               Z(N), TE(N)
      dimension Z(*), TE(*)
C
      call HI ('PITTED')
C     !BEG
      PRINT = NO.gt.0
      if(PRINT) then
        call ABJECT (NO)
        write (NO,100) JS,CTA,CTS
  100   format(' ','Added temperature variation due to shock wave.'//
     $         ' ','JS =',I5,5X,'CTA =',1PE14.6,5X,'CTS =',E14.6//
     $         ' ',4X,'i',15X,'Z',8X,'input TE',13X,'DTE',8X,'final TE')
        call LINER  (1, NO)
      end if
C
      do 102 I = 1,N
        if(I.le.JS) then
          DTE = ZERO
        else
          ARG = (Z(JS)-Z(I))/CTS
          TRM = exp(ARG)
          DTE = CTA*TRM
        end if
        TSAV  = TE(I)
        TE(I) = TE(I)+DTE
C
        if(PRINT) then
          write (NO,101) I,Z(I),TSAV,DTE,TE(I)
  101     format(' ',I5,1P4E16.8)
          call SHIM (I, 5, NO)
        end if
  102 continue
C     !END
      call BYE ('PITTED')
C
      return
      end
