      subroutine NEST
     $(X,IX,N,NL,NO,TE,XNE,XNC,XNU,FCJ,CIJAD,CHIJ,CIJ)
C
C     Rudolf Loeser, 2004 Apr 02
C---- Final adjustments to CIJ.
C     !DASH
      save
C     !DASH
      real*8 CHIJ, CIJ, CIJAD, FCJ, TE, X, XNC, XNE, XNU
      integer IX, KCHIJ, KFELE, N, NL, NNL2, NO
      logical KILROY
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
      equivalence (LEST(70),KFELE)
      equivalence (LEST(71),KCHIJ)
C     !DASH
      external CORVUS, ARRADD, TRABZON, HI, BYE
C
      dimension X(*), IX(*)
C
C               CIJ(N,NL**2), CIJAD(N,NL**2), CHIJ(N,NL**2), FCJ(N,NL),
      dimension CIJ(*),       CIJAD(*),       CHIJ(*),       FCJ(*),
C
C               XNU(NSL), XNC(N), XNE(N), TE(N)
     $          XNU(*),   XNC(*), XNE(*), TE(*)
C
      call HI ('NEST')
C     !BEG
      NNL2 = N*NL*NL
C
      if(KFELE.gt.0) then
C----   Fast electrons
        KILROY = .true.
        call CORVUS  (N, NL, NO, KILROY, FCJ, CIJ)
      end if
C
C---- "Additional" input amount
      call ARRADD    (CIJ, CIJAD, CIJ, NNL2)
C
      if(KCHIJ.gt.0) then
C----   Analyze excitations
        call TRABZON (X, IX, TE, XNE, XNC, XNU, CIJ, CHIJ, NO)
C----   Collisions with Hydrogen
        call ARRADD  (CIJ, CHIJ,  CIJ, NNL2)
      end if
C     !END
      call BYE ('NEST')
C
      return
      end
