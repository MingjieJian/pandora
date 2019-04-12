      subroutine YUCATAN
     $(Z,TE,XLT,COND,N)
C
C     Rudolf Loeser, 1981 Feb 03
C---- Computes conductive flux gradient, for DAFFY.
C     !DASH
      save
C     !DASH
      real*8 A, B, COND, F, RAT, TE, XLT, Z, ZERO
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external DIVIDE, HI, BYE
C
C               Z(N), TE(N), XLT(N), COND(N)
      dimension Z(*), TE(*), XLT(*), COND(*)
C
      data F /-1.D-10/
C
      call HI ('YUCATAN')
C     !BEG
      COND(1) = ZERO
      COND(N) = ZERO
      call DIVIDE ((TE(2)-TE(1)),(Z(2)-Z(1)),RAT)
      B = (XLT(2)+XLT(1))*RAT
      do 100 I = 2,(N-1)
        A = B
        call DIVIDE ((TE(I+1)-TE(I)),(Z(I+1)-Z(I)),RAT)
        B = (XLT(I+1)+XLT(I))*RAT
        call DIVIDE ((B-A),(Z(I+1)-Z(I-1)),RAT)
        COND(I) = F*RAT
  100 continue
C     !END
      call BYE ('YUCATAN')
C
      return
      end
