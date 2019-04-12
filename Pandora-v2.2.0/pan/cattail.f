      subroutine CATTAIL
     $(INDX,XLM,N,NOPAC,XNE,HN,TE,CONT)
C
C     Rudolf Loeser, 1988 Oct 27
C---- Computes a set of H- free-free opacity values.
C     (This is version 4 of CATTAIL.)
C     !DASH
      save
C     !DASH
      real*8 A, B, C, CONT, D, HN, ONE, TE, VL2, XLM, XNE, ZERO
      integer I, INDX, N, NOPAC
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  HI, BYE
      intrinsic max
C
C               XNE(N), HN(N,Limp), TE(N), CONT(Nopac,N)
      dimension XNE(*), HN(N,*),    TE(*), CONT(NOPAC,*)
C
      data A,B,C,D /1.D-38, 2.18403D5, 9.407D2, 5.9415D2/
C
      call HI ('CATTAIL')
C     !BEG
      do 100 I = 1,N
        VL2 = (XLM/B)*(ONE+(XLM/C)*max((ONE-D/TE(I)),ZERO))
        CONT(INDX,I) = XNE(I)*HN(I,1)*A*VL2
  100 continue
C     !END
      call BYE ('CATTAIL')
C
      return
      end
