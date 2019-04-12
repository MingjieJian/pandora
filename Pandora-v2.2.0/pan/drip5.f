      subroutine DRIP5
     $(LU,N,NL,KOLEV,S,B,RK,RL,KASE,IETA,LNLIM,EXLYM,TGLYM,YL)
C
C     Rudolf Loeser, 1989 Apr 18
C---- Prints abbreviated Lyman results, for HAWSER.
C     !DASH
      save
C     !DASH
      real*8 B, EXLYM, RK, RL, S, TGLYM, YL
      integer IETA, KASE, KOLEV, LASE, LNLIM, LU, N, NL
      character CASE*1, KO*2
C     !DASH
      external  LINER, VECOUT, HI, BYE
      intrinsic min, max
C
C               S(N), B(N), RK(N,NL), RL(N,NL)
      dimension S(*), B(*), RK(N,*),  RL(N,*)
C
      dimension CASE(4)
C
      data CASE /'A', 'B', 'C', '?'/
C
      call HI ('DRIP5')
C     !BEG
      write (KO,100) KOLEV
  100 format(I2)
      LASE = min(max(KASE,1),4)
C
      call LINER    (2,LU)
      write (LU,101) CASE(LASE),IETA,LNLIM,EXLYM,TGLYM,YL
  101 format(' ','Abbreviated LYMAN printout',84X,'(Option ALYMPRNT)'//
     $       ' ','Case ',A1,5X,'eta =',I4,5X,'LNLIM =',I4,5X,'EXLYM =',
     $            1PE12.4,5X,'TGLYM =',E12.4,5X,'YL =',E12.4)
C
      call VECOUT (LU,S          ,N,'S'       )
      call VECOUT (LU,B          ,N,('B'//KO) )
      call VECOUT (LU,RK(1,KOLEV),N,('RK'//KO))
      call VECOUT (LU,RL(1,KOLEV),N,('RL'//KO))
C     !END
      call BYE ('DRIP5')
C
      return
      end
