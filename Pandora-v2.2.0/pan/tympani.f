      subroutine TYMPANI
     $(NO,N,NLH,KITER,LIMIT,EIDIF,RZM,ZMER,AEL,ZHEL,HND,XNC,XNE,SUM,
     $ BDI,HNP,SA)
C     Rudolf Loeser, 1975 Jul 29
C     Revised RL/SGK Apr  9 2014 
C---- Prints for SHERBET.
C     !DASH
      save
C     !DASH
      real*8 AEL, BDI, EIDIF, HND, HNP, RZM, SA, SUM, XNC, XNE, ZHEL,
     $       ZMER
      integer KITER, LIMIT, N, NLH, NO
C     !DASH
      external  DASHER, MILLET, LINER, HI, BYE
C
C               RZM(N), ZMER(N), AEL(N), HND(N), XNE(N), SUM(N), XNC(N),
      dimension RZM(*), ZMER(*), AEL(*), HND(*), XNE(*), SUM(*), XNC(*),
C
C               BDI(N,NL), HNP(N), SA(N), ZHEL(N)
     $          BDI(N,*),  HNP(*), SA(*), ZHEL(*)
C
      call HI ('TYMPANI')
C     !BEG
      if(NO.gt.0) then
        call LINER  (2, NO)
        call DASHER (NO)
        call LINER  (2, NO)
C
        write (NO,100) KITER,LIMIT,EIDIF
  100   format(' ','Electron Density, calculated from the ',
     $             'quadratic expression.'//
     $         ' ',I2,' Iterations  (Limit=',I3,').',5X,
     $             'Relative convergence criterion =',F10.5)
C
        call MILLET (NO, N, NLH, AEL, ZMER, RZM, HND, ZHEL, XNC, XNE,
     $               SA, HNP, SUM, BDI)
      end if
C     !END
      call BYE ('TYMPANI')
C
      return
      end
