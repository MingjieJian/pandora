      subroutine ZIRA
     $(AIJ,M,NL,SUM)
C
C     Rudolf Loeser, 1990 Oct 05
C---- Computes sum of A-values, such that
C     SUM = sum [ AIJ(M,n) ]  for 1 .le. n .le. (M-1).
C     !DASH
      save
C     !DASH
      real*8 AIJ, SUM, ZERO
      integer M, N, NL
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
C               AIJ(NL,NL)
      dimension AIJ(NL,*)
C
      call HI ('ZIRA')
C     !BEG
      SUM = ZERO
C
      if(M.gt.1) then
        do 100 N = 1,(M-1)
          SUM = SUM+AIJ(M,N)
  100   continue
      end if
C     !END
      call BYE ('ZIRA')
C
      return
      end
