      subroutine DRUM
     $(NO,KOUNT,LIMIT,EIDIF,N,RZM,ZMER,AEL,ZHEL,HND,XNC,XNE)
C
C     Rudolf Loeser, 1975 Jul 24
C     Revised RL/SGK Apr  9 2014 
C---- Prints results for HELGA.
C     !DASH
      save
C     !DASH
      real*8 AEL, EIDIF, HND, RZM, XNC, XNE, ZHEL, ZMER, dummy
      integer KOUNT, LIMIT, N, NO
C     !DASH
      external  PRIAM, MILLET, LINER, HI, BYE
C
C               RZM(N), ZMER(N), AEL(N), HND(N), XNE(N), XNC(N), ZHEL(N)
      dimension RZM(*), ZMER(*), AEL(*), HND(*), XNE(*), XNC(*), ZHEL(*)
C
      call HI ('DRUM')
C     !BEG
      if(NO.gt.0) then
        call PRIAM  (NO,'NE',2)
C
        call LINER  (1,NO)
        write (NO,100) KOUNT,LIMIT,EIDIF
  100   format(' ','Electron Density, calculated from ',
     $             'the direct expression.'//
     $         ' ',I2,' Iterations  (limit=',I3,').',5X,
     $             'Relative convergence criterion =',F10.5)
C
        call MILLET (NO,N,0,AEL,ZMER,RZM,HND,ZHEL,XNC,XNE,dummy,dummy,
     $               dummy,dummy)
      end if
C     !END
      call BYE ('DRUM')
C
      return
      end
