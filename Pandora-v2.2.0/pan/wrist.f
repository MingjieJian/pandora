      subroutine WRIST
     $(N,S,CNEX,XS,IQINC,XJNU)
C
C     Rudolf Loeser, 1978 Apr 09
C---- Computes XJNU (= mean intensity), for SASKIA.
C     !DASH
      save
C     !DASH
      real*8 CNEX, ONE, S, XJNU, XS
      integer IQINC, J, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  MOVE1, ARRINC, HI, BYE
C
C               S(N), CNEX(N), XS(N,N), XJNU(N)
      dimension S(*), CNEX(*), XS(N,*), XJNU(*)
C
C
      call HI ('WRIST')
C     !BEG
      call MOVE1 (S,N,XJNU)
C
      do 100 J = 1,N
        call ARRINC (XS(1,J),S(J),XJNU,N)
  100 continue
C
      if(IQINC.gt.0) then
        call ARRINC (CNEX,ONE,XJNU,N)
      end if
C     !END
      call BYE ('WRIST')
C
      return
      end
