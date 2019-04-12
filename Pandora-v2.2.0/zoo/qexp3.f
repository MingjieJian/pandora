      subroutine QEXP3
     $(D,E,KODE,F)
C     Rudolf Loeser, 1981 Jul 26
C---- Computes F = (1 - exp(-D)) / D - exp(-D).
C     If the input parameter KODE=1, then E already contains
C     the input value exp(-D); if KODE=0, then E contains
C     no input, and the value of exp(-D) will be put there
C     by this routine; if KODE=2, then E contains no input,
C     and nothing will be put there by this routine.
C     !DASH
      save
C     !DASH
      real*8 BIGD, CRIT, D, DS, E, ES, F, FS, ONE, SUM, ZERO
      integer KODE
C     !DASH
      external  GETEE, QSUM
      intrinsic abs
C
      data CRIT,BIGD /.1D0, 50.D0/
      data DS,FS,ES /1000.D0, .001D0, 0.D0/
      data ZERO,ONE  /0.D0, 1.D0/
C
C     !BEG
      if(D.ne.DS) then
        DS = D
        if(DS.eq.ZERO) then
          ES = ONE
          FS = ZERO
        else
          ES = -ONE
          if(DS.ge.BIGD) then
            FS = ONE/DS
          else
            call GETEE (DS,ES,E,KODE)
            if(abs(DS).ge.CRIT) then
              FS = (ONE-ES)/DS-ES
            else
              call QSUM (DS,SUM)
              FS = ES*DS*SUM
            end if
          end if
        end if
      end if
      F = FS
      if(KODE.eq.0) then
        if(ES.eq.-ONE) then
          call GETEE (DS,ES,E,KODE)
        end if
        E = ES
      end if
C     !END
C
      return
      end
