      subroutine POUT
     $(X,W,IW,RECZ)
C
C     Rudolf Loeser, 1994 May 17
C---- Recomputes HND-dependent quantities, and
C     Z-dependent quantities (if needed).
C     (This is version 3 of POUT.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW
      logical RECZ
C     !DASH
      external NUBA, DAUCUS, GANDER, FLASK, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
      call HI ('POUT')
C     !BEG
      if(RECZ) then
C----   Quantities depending on Z
C
C----   Temperature gradients
        call FLASK  (X, W, IW)
C----   Geometrical dilution
        call GANDER (X)
C----   Spherical terms
        call NUBA   (X, W)
C
      end if
C---- Quantities depending on HND and Z
C
C---- Fluid velocities
      call DAUCUS   (X, W)
C     !END
      call BYE ('POUT')
C
      return
      end
