      subroutine QLOG1
     $(D,F)
C
C     Rudolf Loeser, 1981 Jul 27
C---- Computes F = log(1+D), D .gt. -1.
C     !DASH
      save
C     !DASH
      real*8 CRIT, D, DS, F, FS, ONE, SUM, ZERO
C     !DASH
      external LSUM, ABORT
C
      data CRIT /.1D0/
      data DS,FS /0.D0, 0.D0/
      data ZERO,ONE /0.D0, 1.D0/
C
C     !BEG
      if(D.le.-ONE) then
        write (*,100) D
  100   format(' ','QLOG1:  D =',1PE24.16,', cannot compute log(1+D).')
        call ABORT
      end if
      if(D.ne.DS) then
        DS = D
        if(DS.ge.CRIT) then
          FS = log(ONE+DS)
        else
          call LSUM (DS,SUM)
          FS = DS*(ONE-DS*SUM)
        end if
      end if
      F = FS
C     !END
C
      return
      end
