      subroutine UPSILON
     $(Z,N,GDZ,GDT,WN,TITLE)
C
C     Rudolf Loeser, 1989 Jul 19
C---- Applies "geometrical dilution corrections" to a weight matrix.
C     (This is an approximate way of treating spherical geometry.)
C     (This is version 2 of UPSILON.)
C     !DASH
      save
C     !DASH
      real*8 GDT, GDZ, WN, Z, ZERO
      integer KEQZ, KGDT, N, NGDZ
      character TITLE*(*)
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(45),NGDZ )
      equivalence (LEST(46),KGDT )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external ARISOD, GOOSE, MOVE1, ARRMUL, HI, BYE
C
C               Z(N), WN(N,N), GDZ(N), GDT(N,N)
      dimension Z(*), WN(*),   GDZ(*), GDT(*)
C
      call HI ('UPSILON')
C     !BEG
C---- Determine whether current Z-values are the same of the set of
C     Z-values last used to recompute GDT
      call ARISOD  (Z, N, GDZ, NGDZ, ZERO, KEQZ)
C
      if(KEQZ.eq.0) then
C----   Sets of Z-values are not the same - GDT must be recomputed
C       (and save current Z in GDZ)
        NGDZ = N
        call MOVE1 (Z, N, GDZ)
        call GOOSE (GDZ, N, GDT, KGDT, TITLE)
      end if
C
      if(KGDT.eq.1) then
C----   Since the terms of GDT do not all =1, multiply
        call ARRMUL (WN, GDT, WN, (N**2))
      end if
C     !END
      call BYE ('UPSILON')
C
      return
      end
