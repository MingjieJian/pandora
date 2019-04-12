      subroutine EULALIA
     $(LU,GMASIN,GMASS,GMASSE,ZOLD,ZNEW,Z,WZM,N,ZED,FZLIM,DZMSS,ZQ)
C
C     Rudolf Loeser, 1978 Sep 07
C---- Prints details concerning new Z scale.
C     !DASH
      save
C     !DASH
      real*8 DZMSS, FZLIM, GMASIN, GMASS, GMASSE, WZM, Z, ZED, ZNEW,
     $       ZOLD, ZQ
      integer I, LU, N
C     !DASH
      external PRIAM, LINER, HI, BYE
C
C               GMASIN(N), GMASS(N), ZOLD(N), ZNEW(N), Z(N), GMASSE(N),
      dimension GMASIN(*), GMASS(*), ZOLD(*), ZNEW(*), Z(*), GMASSE(*),
C
C               ZED(N)
     $          ZED(*)
C
      call HI ('EULALIA')
C     !BEG
      if(LU.gt.0) then
C
        call PRIAM (LU, 'Z SCALE', 7)
        call LINER (2, LU)
        write (LU,100) FZLIM,DZMSS,ZQ,WZM
  100   format(' ','Calculation of new Z scale from input mass.'///
     $         ' ','FZLIM',F8.3,'  Factor limiting the allowed change ',
     $             'between "Old Z" and "New Z"'/
     $         ' ','DZMSS',F8.3,'  The constant Q =',1PE9.2,0P,
     $             ' was subtracted from the "Old Z" and "New Z" ',
     $             'tables, '/
     $         ' ',15X,'then added to get "Edited Z", to avoid ',
     $             'Z values closer to zero'/
     $         ' ',15X,'than DZMSS times the largest absolute ',
     $             'value of "Old Z".'/
     $         ' ','  WZM',F8.3,'  Weighted Z = WZM*(Edited Z) ',
     $             '+ (1-WZM)*(Old Z)'///
     $         ' ',14X,'Input',9X,'Current',10X,'Edited',13X,'Old',
     $             13X,'New',10X,'Edited',8X,'Weighted'/
     $         ' ',3X,3(12X,'Mass'),4(15X,'Z'))
        call LINER (1, LU)
C
        write (LU,101) (I,GMASIN(I),GMASS(I),GMASSE(I),ZOLD(I),ZNEW(I),
     $                  ZED(I),Z(I),I=1,N)
  101   format(5(' ',I3,1P7E16.7/))
C
      end if
C     !END
      call BYE ('EULALIA')
C
      return
      end
