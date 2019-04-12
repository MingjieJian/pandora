      subroutine AVAHI
     $(I,WAVE,WEIGHT,N,H,HINT)
C
C     Rudolf Loeser, 1987 Mar 22
C---- Dumps for BRABANT.
C     !DASH
      save
C     !DASH
      real*8 H, HINT, WAVE, WEIGHT
      integer I, LUEO, N
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, VECOUT, HI, BYE
C
C               H(N), HINT(N)
      dimension H(*), HINT(*)
C
      call HI ('AVAHI')
C     !BEG
      call LINER  (2, LUEO)
      write (LUEO,100) I,WAVE,WEIGHT
  100 format(' ','>>>>>     Contribution from ',I3,'. wavelength ',
     $           '(',1PE20.12,')',10X,'Integration weight =',E14.7)
C
      call VECOUT (LUEO, H,    N, 'Monochromatic flux'     )
      call VECOUT (LUEO, HINT, N, 'Accumulated flux so far')
C     !END
      call BYE ('AVAHI')
C
      return
      end
