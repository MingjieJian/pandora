      subroutine BEEBALM
     $(N,XM,RR,XJNU)
C
C     Rudolf Loeser, 1981 Jul 22
C---- Computes XJNU directly, for TREFOIL.
C     !DASH
      save
C     !DASH
      real*8 RR, XJNU, XM
      integer J, N
C     !DASH
      external ZERO1, ARRINC, HI, BYE
C
C               XM(N,N), RR(N), XJNU(N)
      dimension XM(N,*), RR(*), XJNU(*)
C
      call HI ('BEEBALM')
C     !BEG
      call ZERO1    (XJNU,N)
      do 100 J = 1,N
        call ARRINC (XM(1,J),RR(J),XJNU,N)
  100 continue
C     !END
      call BYE ('BEEBALM')
C
      return
      end
