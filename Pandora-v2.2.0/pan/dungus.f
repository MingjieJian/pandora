      subroutine DUNGUS
     $(WVLEN,OFF,KOUNT,WVNUM)
C
C     Rudolf Loeser, 1991 Jul 12
C---- Sets up wavenumber.
C     !DASH
      save
C     !DASH
      real*8 OFF, WVLEN, WVNUM
      integer I, KOUNT
C     !DASH
      external WANDA, HI, BYE
C
C               WVLEN(KOUNT), WVNUM(KOUNT)
      dimension WVLEN(*),     WVNUM(*)
C
      call HI ('DUNGUS')
C     !BEG
      do 100 I = 1,KOUNT
        call WANDA ((WVLEN(I)+OFF),WVNUM(I))
  100 continue
C     !END
      call BYE ('DUNGUS')
C
      return
      end
