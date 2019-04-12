      subroutine SLIVER
     $(XNU,IU,IL,CORE)
C
C     Rudolf Loeser, 1983 Dec 05
C---- Sets up line core wavelength.
C     !DASH
      save
C     !DASH
      real*8 CORE, XNU
      integer IL, IU
C     !DASH
      external ANGIE, HI, BYE
C
C               XNU(NSL)
      dimension XNU(*)
C
      call HI ('SLIVER')
C     !BEG
      call ANGIE ((XNU(IU)-XNU(IL)),CORE)
C     !END
      call BYE ('SLIVER')
C
      return
      end
