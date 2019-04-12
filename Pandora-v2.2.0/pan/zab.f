      subroutine ZAB
     $(LU,INDEX,KNT,NAB,BANDL,BANDU,N,WAVCO,ARRCO,COMP,NCP)
C
C     Rudolf Loeser, 1983 Jul 05
C---- Plots, for AMBRO.
C     !DASH
      save
C     !DASH
      real*8 ARRCO, BANDL, BANDU, COMP, WAVCO, YH, YL
      integer IB, INDEX, KNT, LU, N, NAB, NCP
      logical OK
C     !DASH
      external NANDI, MIDAS, HI, BYE
C
C               ARRCO(NCP,N), BANDL(NAB), BANDU(NAB), WAVCO(NCP),
      dimension ARRCO(*),     BANDL(*),   BANDU(*),   WAVCO(*),
C
C               COMP(NCP), INDEX(KNT)
     $          COMP(*),   INDEX(*)
C
      call HI ('ZAB')
C     !BEG
      if(LU.gt.0) then
C----   Find ordinate limits
        call NANDI     (ARRCO,(NCP*N),YL,YH,OK)
C
        if(OK) then
C----     Make graphs for all wavelength bands
          do 100 IB = 1,NAB
            call MIDAS (LU,IB,NAB,BANDL,BANDU,YL,YH,WAVCO,ARRCO,NCP,N,
     $                  INDEX,KNT,COMP)
  100     continue
        end if
      end if
C     !END
      call BYE ('ZAB')
C
      return
      end
