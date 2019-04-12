      subroutine KULU
     $(ZT,ZI,Z1,Z1Z,Z2,Z2Z,Z3,Z3Z,JQ,JX,ZMIN,ZMAX)
C
C     Rudolf Loeser, 1990 Apr 23
C---- Gets plot limits for KAILUR.
C     !DASH
      save
C     !DASH
      real*8 Z1, Z2, Z3, ZI, ZMAX, ZMIN, ZT
      integer IMAX1, IMAXT, IMIN1, IMINT, JQ, JX
      logical Z1Z, Z2Z, Z3Z
C     !DASH
      external  GUARD, HI, BYE
      intrinsic min, max
C
C               ZT(N), ZI(N), Z1(N), Z2(N), Z3(N)
      dimension ZT(*), ZI(*), Z1(*), Z2(*), Z3(*)
C
      call HI ('KULU')
C     !BEG
      call GUARD   (JQ,JX,ZT,IMINT,IMAXT)
      call GUARD   (JQ,JX,ZI,IMIN1,IMAX1)
      ZMIN = min(ZT(IMINT),ZI(IMIN1))
      ZMAX = max(ZT(IMAXT),ZI(IMAX1))
C
      if(.not.Z1Z) then
        call GUARD (JQ,JX,Z1,IMIN1,IMAX1)
        ZMIN = min(ZMIN,Z1(IMIN1))
        ZMAX = max(ZMAX,Z1(IMAX1))
      end if
C
      if(.not.Z2Z) then
        call GUARD (JQ,JX,Z2,IMIN1,IMAX1)
        ZMIN = min(ZMIN,Z2(IMIN1))
        ZMAX = max(ZMAX,Z2(IMAX1))
      end if
C
      if(.not.Z3Z) then
        call GUARD (JQ,JX,Z3,IMIN1,IMAX1)
        ZMIN = min(ZMIN,Z3(IMIN1))
        ZMAX = max(ZMAX,Z3(IMAX1))
      end if
C     !END
      call BYE ('KULU')
C
      return
      end
