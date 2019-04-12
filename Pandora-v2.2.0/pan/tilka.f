      subroutine TILKA
     $(X,N,XL,XR,OK)
C
C     Rudolf Loeser, 2005 Dec 15
C---- Gets axis limits for line flux background plots.
C     (This is version 2 of TILKA.)
C     !DASH
      save
C     !DASH
      real*8 X, XL, XR, ZERO
      integer N
      logical OK
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external HI, BYE
C
C               X(N)
      dimension X(1)
C
      call HI ('TILKA')
C     !BEG
      OK = (X(1).gt.ZERO).and.(X(N).gt.ZERO)
C
      if(OK) then
        XL = log10(X(1))
        XR = log10(X(N))
      end if
C     !END
      call BYE ('TILKA')
C
      return
      end
