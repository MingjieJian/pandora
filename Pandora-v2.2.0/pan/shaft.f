      subroutine SHAFT
     $(XDT,NDT,XJNU,PLANK,H,N,SA)
C
C     Rudolf Loeser, 1973 Oct 24
C---- Makes graphs, for QUIVER.
C     !DASH
      save
C     !DASH
      real*8 H, PLANK, SA, XDT, XJNU
      integer MO, N, NDT
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external ABJECT, HORN, HI, BYE
C
C               XDT(NDT), XJNU(N,NDT), PLANK(N,NDT), H(N,NDT), SA(NDT,3)
      dimension XDT(*),   XJNU(*),     PLANK(*),     H(*),     SA(*)
C
      call HI ('SHAFT')
C     !BEG
      if(MO.gt.0) then
C
        call ABJECT (MO)
        write (MO,100)
  100   format(' ','Graph of log(Dust Jnu) vs. frequency.')
        call HORN   (XJNU, N, XDT, SA, NDT, MO)
C
        call ABJECT (MO)
        write (MO,101)
  101   format(' ','Graph of log(Dust Planck function) vs. frequency.')
        call HORN   (PLANK, N, XDT, SA, NDT, MO)
C
        call ABJECT (MO)
        write (MO,102)
  102   format(' ','Graph of log(Monochromatic Flux) vs. frequency.')
        call HORN   (H, N, XDT, SA, NDT, MO)
C
      end if
C     !END
      call BYE ('SHAFT')
C
      return
      end
