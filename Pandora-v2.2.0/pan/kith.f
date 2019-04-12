      subroutine KITH
     $(N,WAVE)
C
C     Rudolf Loeser, 2002 Sep 25
C---- Computes the wavelength of the N/1 H Lyman line (N>1).
C     !DASH
      save
C     !DASH
      real*8 WAVE, XNU, dummy1, dummy2
      integer N
C     !DASH
      external HYDATA, ANGIE, HI, BYE
C
      call HI ('KITH')
C     !BEG
      call HYDATA (N,XNU,dummy1,dummy2)
      call ANGIE  (XNU,WAVE)
C     !END
      call BYE ('KITH')
C
      return
      end
