      subroutine LIBYA
     $(SI,N,DI,MRR,XI,NRAD)
C
C     Rudolf Loeser, 1992 Sep 23
C---- Sets up a column in the consolidated intensity array.
C     (This is version 2 of LIBYA.)
C     !DASH
      save
C     !DASH
      real*8 DI, SI, XI
      integer I, J, MRR, N, NRAD
C     !DASH
      external HI, BYE
C
C               SI(N), DI(MRR), XI(NRAD)
      dimension SI(*), DI(*),   XI(*)
C
      call HI ('LIBYA')
C     !BEG
      J = NRAD+1
      do 100 I = 2,N
        J = J-1
        XI(J) = SI(I)
  100 continue
C
      if(MRR.gt.1) then
        do 101 I = 1,(MRR-1)
          XI(I) = DI(I)
  101   continue
      end if
C     !END
      call BYE ('LIBYA')
C
      return
      end
