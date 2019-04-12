      subroutine ORLOP
     $(M,YA,CHKA,MN1,KZANX,Y,CHK)
C
C     Rudolf Loeser, 2004 Jan 28
C---- Selects values corresponding to regular Z-scale, for  Special-N1.
C     !DASH
      save
C     !DASH
      real*8 CHK, CHKA, Y, YA
      integer I, K, KZANX, M, MN1
C     !DASH
      external MOVE1, HI, BYE
C
C               J = N+MXTAP
C
C               YA(J), CHKA(J), Y(N), CHK(N), KZANX(N)
      dimension YA(*), CHKA(*), Y(*), CHK(*), KZANX(*)
C
      call HI ('ORLOP')
C     !BEG
      if(M.eq.MN1) then
        call MOVE1 (YA,   MN1, Y  )
        call MOVE1 (CHKA, MN1, CHK)
      else
        do 100 I = 1,MN1
          K      = KZANX(I)
          Y(I)   = YA(K)
          CHK(I) = CHKA(K)
  100   continue
      end if
C     !END
      call BYE ('ORLOP')
C
      return
      end
