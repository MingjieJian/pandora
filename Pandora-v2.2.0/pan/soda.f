      subroutine SODA
     $(X,W,IW,DEE,DELTA,VAMB,VBMB,VCMB,VDMB,VEMB,N,Z,TE,ZT,XNE,HND,
     $ H2N,HEND,RHEAB,VM,HK,H1,HEK,HE1,HE2K,HE21,BETA,ZI,ZION,XION,
     $ KVLG,DUMP)
C
C     Rudolf Loeser, 1989 Sep 19
C---- Computes DEE, DELTA, and velocities, for CARAMBA.
C     (This is version 2 of SODA.)
C     !DASH
      save
C     !DASH
      real*8 BETA, DEE, DELTA, H1, H2N, HE1, HE21, HE2K, HEK, HEND, HK,
     $       HND, RHEAB, TE, VAMB, VBMB, VCMB, VDMB, VEMB, VM, W, X,
     $       XION, XNE, Z, ZI, ZION, ZT
      integer IN, IS, IW, IZ1, IZ2, IZ3, IZXG, KVLG, MOX, N
      logical DUMP
C     !DASH
      external NATRON, LIMPET, ERATO, CAPE, WGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               DEE(4,5,N), DELTA(7,N), Z(N), RHEAB(N), XNE(N), HND(N),
      dimension DEE(4,5,*), DELTA(7,*), Z(*), RHEAB(*), XNE(*), HND(*),
C
C               HK(N), HEND(N), HEK(N), HE2K(N), ZT(N), ZI(N), XION(N),
     $          HK(*), HEND(*), HEK(*), HE2K(*), ZT(*), ZI(*), XION(*),
C
C               HE21(N), BETA(N), VAMB(N), HE1(N), TE(N), H1(N), VM(N),
     $          HE21(*), BETA(*), VAMB(*), HE1(*), TE(*), H1(*), VM(*),
C
C               VEMB(N), ZION(N), H2N(N), VCMB(N), VDMB(N), VBMB(N)
     $          VEMB(*), ZION(*), H2N(*), VCMB(*), VDMB(*), VBMB(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IZ1   ),(IN( 2),IZ2   ),(IN( 3),IZ3   ),(IN( 4),IZXG  )
C     !EJECT
C
      call HI ('SODA')
C     !BEG
C     (Get, and allocate, W allotment)
      call NATRON (IN, IS, MOX, 'SODA')
C
C---- Compute Hydrogen ionization terms
      call LIMPET (N, Z, HK, H1, ZI, ZION, XION, W, IW)
C---- Compute velocities and other terms
      call ERATO  (X, W, IW, N, Z, HK, H1, HEK, HE1, HE2K, HE21, BETA,
     $             TE, ZT, XNE, HND, H2N, HEND, RHEAB, VM, VAMB, VBMB,
     $             VCMB, VDMB, VEMB, XION, ZION, ZI, W(IZ1), W(IZ2),
     $             W(IZ3), W(IZXG), DEE, KVLG, DUMP)
C---- Compute DELTA's
      call CAPE   (N, DELTA, DEE, ZI, W(IZ1), W(IZ2), W(IZ3), ZT,
     $             W(IZXG), HE1, BETA, HE2K)
C
C     (Give back W allotment)
      call WGIVE  (W, 'SODA')
C     !END
      call BYE ('SODA')
C
      return
      end
