      subroutine KNAWEL
     $(N,XN1O,XNKO,XN1N,ZION,XN1F,XNKF,RS,RSP)
C
C     Rudolf Loeser, 1988 Aug 03
C---- Computes final sets of N1 and NP, for CARAMBA.
C     !DASH
      save
C     !DASH
      real*8 OMW, ONE, R, RS, RSP, W, XN1, XN1F, XN1N, XN1O, XNK, XNKF,
     $       XNKO, ZION
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
      external  DIVIDE, HI, BYE
      intrinsic max
C
C               XN1O(N), XNKO(N), XN1N(N), XN1F(N), XNKF(N), ZION(N),
      dimension XN1O(*), XNKO(*), XN1N(*), XN1F(*), XNKF(*), ZION(*),
C
C               RS(N), RSP(N)
     $          RS(*), RSP(*)
C
      call HI ('KNAWEL')
C     !BEG
      do 100 I = 1,N
        W = ZION(I)
        OMW = ONE-W
C
        call DIVIDE ((XN1O(I)+XNKO(I)),(XN1N(I)+XNKO(I)),RS(I))
C
        XN1 = RS(I)*XN1N(I)
        XN1F(I) = XN1*W+XN1O(I)*OMW
C
        R = XN1F(I)/XN1O(I)
        if(R.ge.ONE) then
          RSP(I) = max(RS(I),(ONE/R))
        else
          RSP(I) = max(RS(I),R)
        end if
C
        XNK = RSP(I)*XNKO(I)
        XNKF(I) = XNK*W+XNKO(I)*OMW
  100 continue
C     !END
      call BYE ('KNAWEL')
C
      return
      end
