      subroutine COSMOS
     $(KODE,M,TE,XX,XL,XR)
C
C     Rudolf Loeser, 1998 Mar 16
C---- Sets up X-coordinate, for FENCE.
C     (This is version 2 of COSMOS.)
C     !DASH
      save
C     !DASH
      real*8 TE, TLL, TLR, XL, XR, XX, ZERO
      integer I, KODE, M
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external LOGO, HI, BYE
C
C               TE(N), XX(N)
      dimension TE(*), XX(*)
C
      data TLL,TLR /3.8D0, 5.D0/
C
      call HI ('COSMOS')
C     !BEG
      if(KODE.eq.1) then
        call LOGO (TE, M, 0, ZERO, XX)
        XL = TLL
        XR = TLR
C
      else
C
        do 100 I = 1,M
          XX(I) = I
  100   continue
        XL = XX(1)
        XR = XX(M)
      end if
C     !END
      call BYE ('COSMOS')
C
      return
      end
