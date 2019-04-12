      subroutine HEMLOCK
     $(W,N,I,FIN,REF,DT,SE2,SE3,SE4,SES2,SES3,SES4,SMLLTAU)
C
C     Rudolf Loeser, 1971 Jul 08 (revised 2000 Jan 25)
C---- Computes the I'th row of the W-matrix, for the RT weight
C     matrix calculation.
C     DT is a table of distances from the diagonal, and SE2, SE3, SE4,
C     SES2, SES3 and SES4 are save arrays associated with DT.
C     !DASH
      save
C     !DASH
      real*8 DT, SE2, SE3, SE4, SES2, SES3, SES4, W
      integer I, JL, JR, N
      logical FIN, REF, SMLLTAU
C     !DASH
      external HAZEL, BIRCH, QUINCE, FIR, YEW, HI, BYE
C
C               DT(2*N), SE2(2*N), SE3(2*N), SES2(2*N), SES3(2*N),
      dimension DT(*),   SE2(*),   SE3(*),   SES2(*),   SES3(*),
C
C               W(N,N), SE4(2*N), SES4(2*N)
     $          W(N,*), SE4(*),   SES4(*)
C     !EJECT
C
      call HI ('HEMLOCK')
C     !BEG
C---- Set up basic W matrix, which is for a FINITE slab (except for
C     special treatment of the bottom-right diagonal element for the
C     REFLECTIVE case)
C
C     Do diagonal terms, and set up starting indices for stepping to
C     the left (JL) and the right (JR) (special case for REFLECTIVE)
      call HAZEL    (W,N,I,JL,JR,DT,SE2,SE3,SE4,SES2,SES3,SES4,
     $               REF,SMLLTAU)
C
      if(JL.gt.1) then
C----   Go to the left
        call BIRCH  (W,N,I,JL,DT,SE2,SE3,SES2,SES3)
      end if
C
      if(JR.lt.N) then
C----   Go to the right
        call QUINCE (W,N,I,JR,DT,SE2,SE3,SES2,SES3)
      end if
C
C
      if(.not.FIN) then
C----   Modify the last two columns of the matrix, for a SEMI-INFINITE
C       atmosphere
        call FIR    (W,N,I,DT,SE2,SE3)
      end if
C
      if(REF) then
C----   Modify the matrix further for a REFLECTIVE (i.e. symmetric)
C       slab, using the extension of the DT table
        call YEW    (W,N,I,DT,SE2,SE3,SES2,SES3)
      end if
C     !END
      call BYE ('HEMLOCK')
C
      return
      end
