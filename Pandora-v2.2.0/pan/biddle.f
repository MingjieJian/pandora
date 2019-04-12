      subroutine BIDDLE
     $(NDT,XDT,TW)
C
C     Rudolf Loeser, 2002 Jan 08
C---- Sets up a dump, for SINEW.
C     !DASH
      save
C     !DASH
      real*8 TW, XDT
      integer LUEO, NDT
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
C               XDT(NDT), TW(NDT)
      dimension XDT(*),   TW(*)
C
      call HI ('BIDDLE')
C     !BEG
      call VECOUT (LUEO, XDT, NDT, 'XDT - frequency units'   )
      call VECOUT (LUEO, TW,  NDT, 'TW - integration weights')
      call LINER  (2, LUEO)
C     !END
      call BYE ('BIDDLE')
C
      return
      end
