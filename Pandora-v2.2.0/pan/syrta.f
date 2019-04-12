      subroutine SYRTA
     $(NO,N,IU,IL,YBAR,RHO,S,AW,CHI,SA,ASTAR,RBDQ)
C
C     Rudolf Loeser, 2003 Mar 18
C---- Prints b-ratios from Chi, for CROCUS.
C     !DASH
      save
C     !DASH
      real*8 ASTAR, AW, BDIT, CHI, RBDQ, RHO, S, SA, YBAR
      integer I, IL, IU, N, NO
C     !DASH
      external BRAT, LINER, SHIM, HI, BYE
C
C               YBAR(N), RHO(N), S(N), CHI(N), ASTAR(N), SA(N), AW(N),
      dimension YBAR(*), RHO(*), S(*), CHI(*), ASTAR(*), SA(*), AW(*),
C
C               RBDQ(N,NL)
     $          RBDQ(*)
C
      call HI ('SYRTA')
C     !BEG
      if(NO.gt.0) then
        call LINER  (2, NO)
        write (NO,100) IU,IL
  100   format(' ','RBDQ: b-ratios from CHI for transition (',I2,
     $             ',',I2,').'//
     $         ' ',15X,'JBAR',12X,'RHO',14X,'S',13X,'AW',13X,'SA',
     $             12X,'CHI',13X,'A*',11X,'RBDQ')
        call LINER  (1, NO)
        do 102 I = 1,N
          call BRAT (I, IU, IL, RBDQ, BDIT)
          write (NO,101) I,YBAR(I),RHO(I),S(I),AW(I),SA(I),CHI(I),
     $                     ASTAR(I),BDIT
  101     format(' ',I5,1P8E15.8)
          call SHIM (I, 5, NO)
  102   continue
      end if
C     !END
      call BYE ('SYRTA')
C
      return
      end
