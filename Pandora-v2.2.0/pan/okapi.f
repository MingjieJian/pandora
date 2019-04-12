      subroutine OKAPI
     $(N,H,CALLER)
C
C     Rudolf Loeser,1985 Jan 24
C---- Dumps, for CHULYM.
C     !DASH
      save
C     !DASH
      real*8 H
      integer LUEO, N
      character CALLER*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external VECOUT, MASHED, HI, BYE
C
C               H(N)
      dimension H(*)
C
      call HI ('OKAPI')
C     !BEG
      call VECOUT (LUEO, H, N, 'Updated H')
      call MASHED (CALLER)
C     !END
      call BYE ('OKAPI')
C
      return
      end
