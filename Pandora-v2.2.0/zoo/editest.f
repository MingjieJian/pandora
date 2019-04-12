      subroutine EDITEST
     $(F,CRIT,KODE,BAD)
C     Rudolf Loeser, 1990 Jun 19
C---- Checks to see whether F is "bad", for EDIT1.
C     !DASH
      save
C     !DASH
      real*8 CRIT, F
      integer KODE
      logical BAD
C     !DASH
      external IS_BAD
C
C     !BEG
      call IS_BAD (F,BAD)
      if(.not.BAD) then
        if(KODE.eq.1) then
          BAD = F.lt.CRIT
        else if(KODE.eq.2) then
          BAD = F.le.CRIT
        else if(KODE.eq.3) then
          BAD = F.eq.CRIT
        else if(KODE.eq.4) then
          BAD = F.ge.CRIT
        else
          BAD = F.gt.CRIT
        end if
      end if
C     !END
C
      return
      end
