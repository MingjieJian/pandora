      subroutine KAILUR
     $(LU,KODE,LAB,N,Z,JQ,JX,ZT,ZI,Z1,Z2,Z3)
C
C     Rudolf Loeser, 1990 Apr 20
C---- Plots derivative terms of diffusion calculations.
C     !DASH
      save
C     !DASH
      real*8 Z, Z1, Z2, Z3, ZI, ZMAX, ZMIN, ZT
      integer JQ, JX, KODE, LU, N
      logical OK, Z1Z, Z2Z, Z3Z
      character LAB*(*)
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
C
C---- IMAGE       as of 1997 Aug 21
      integer     IMALEN
      parameter   (IMALEN=65535)
      character   IMAGE*(IMALEN)
      common      /IMAGE/ IMAGE
C     Character string to hold plot images constructed by the
C     K-type line printer plotting routines;
C     but used also as a general scratch character array.
C     .
C     !DASH
      external NAUGHTD, KULU, BORLEY, NORI, MAIHAN, HI, BYE
C
C               Z(N), ZT(N), ZI(N), Z1(N), Z2(N), Z3(N)
      dimension Z(*), ZT(*), ZI(*), Z1(*), Z2(*), Z3(*)
C     !EJECT
C
      call HI ('KAILUR')
C     !BEG
      call NAUGHTD  (Z1,1,N,Z1Z)
      call NAUGHTD  (Z2,1,N,Z2Z)
      call NAUGHTD  (Z3,1,N,Z3Z)
C---- Get function limits
      call KULU     (ZT,ZI,Z1,Z1Z,Z2,Z2Z,Z3,Z3Z,JQ,JX,ZMIN,ZMAX)
C---- Initialize plot image
      call BORLEY   (IMAGE,ZMIN,ZMAX,Z,JQ,JX,KODE,OK)
      if(OK) then
C----   Enter data
        call NORI   (IMAGE,JQ,JX,Z,ZT,ALPHS(20),KODE)
        call NORI   (IMAGE,JQ,JX,Z,ZI,ALPHS( 9),KODE)
        if(.not.Z1Z) then
          call NORI (IMAGE,JQ,JX,Z,Z1,NUMBS( 2),KODE)
        end if
        if(.not.Z2Z) then
          call NORI (IMAGE,JQ,JX,Z,Z2,NUMBS( 3),KODE)
        end if
        if(.not.Z3Z) then
          call NORI (IMAGE,JQ,JX,Z,Z3,NUMBS( 4),KODE)
        end if
C----   Print plot
        call MAIHAN (LU,IMAGE,Z1Z,Z2Z,Z3Z,LAB)
      end if
C     !END
      call BYE ('KAILUR')
C
      return
      end
