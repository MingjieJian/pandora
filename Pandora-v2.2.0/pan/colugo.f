      subroutine COLUGO
     $(X,W,IW,KAMB,KVLG,LUAP,DMPA,NL,XNK,XND,GHI,GHL,N,Z,TE,ZT,XNE,
     $ HND,H2N,HEND,RHEAB,HK,H1,HEK,HE1,HE2K,HE21,BETA,SHE,ZI,ZION,
     $ XION,Z1,Z2,Z3,ZXG,VAMB,VBMB,VCMB,VDMB,VE,VP,VH,V1,V2,V3,VM,
     $ ALPHA)
C
C     Rudolf Loeser, 1989 Sep 15
C---- Computes ambipolar diffusion terms, and various intermediates,
C     for diffusion calculations.
C     (This is version 2 of COLUGO.)
C     !DASH
      save
C     !DASH
      real*8 ALPHA, BETA, GHI, GHL, H1, H2N, HE1, HE21, HE2K, HEK, HEND,
     $       HK, HND, RHEAB, SHE, TE, V1, V2, V3, VAMB, VBMB, VCMB,
     $       VDMB, VE, VH, VM, VP, W, X, XION, XND, XNE, XNK, Z, Z1, Z2,
     $       Z3, ZI, ZION, ZT, ZXG, dummy
      integer IADD, IN, IS, ISIG, ISION, ISLVL, IVK, IVL, IW, IWS, JN,
     $        KAMB, KVLG, LUAP, MOX, MUX, N, NL
      logical DMPA
      character LAB*9
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external FAITH, MUKLUK, AGOUTA, CLOGO, GOLLIAS, TOLUGO, MOLUGO,
     $         HALT, IGIVE, WGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               ZXG(N), XND(N,NL), GHI(N), GHL(N,NL), ALPHA(NL), TE(N),
      dimension ZXG(*), XND(N,*),  GHI(*), GHL(N,*),  ALPHA(*),  TE(*),
C
C               XNE(N), HND(N), H2N(N), HEND(N), V3(N), HEK(N), HE1(N),
     $          XNE(*), HND(*), H2N(*), HEND(*), V3(*), HEK(*), HE1(*),
C
C               VE(N), BETA(N), VM(N), Z2(N), Z3(N), RHEAB(N), XION(N),
     $          VE(*), BETA(*), VM(*), Z2(*), Z3(*), RHEAB(*), XION(*),
C
C               SHE(N), ZT(N), ZI(N), Z1(N), HK(N), Z(N), H1(N), VP(N),
     $          SHE(*), ZT(*), ZI(*), Z1(*), HK(*), Z(*), H1(*), VP(*),
C
C               VH(N), V1(N), V2(N), ZION(N), XNK(N), HE2K(N), HE21(N),
     $          VH(*), V1(*), V2(*), ZION(*), XNK(*), HE2K(*), HE21(*),
C
C               VAMB(N), VBMB(N), VCMB(N), VDMB(N)
     $          VAMB(*), VBMB(*), VCMB(*), VDMB(*)
C
      dimension IN(5)
      equivalence
     $(IN( 1),ISION ),(IN( 2),IADD  ),(IN( 3),ISLVL ),(IN( 4),IVL   ),
     $(IN( 5),IVK   )
C
      dimension JN(1)
      equivalence
     $(IN( 1),ISIG  )
C
      dimension LAB(3)
C
      data LAB /'Hydrogen ', 'Helium-I ', 'Helium-II'/
C     !EJECT
C
      call HI ('COLUGO')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call TOLUGO    (IN, IS,  MOX, 'COLUGO')
      call MOLUGO    (JN, IWS, MUX, 'COLUGO')
C
C---- Compute pressures, ionization terms, and velocities
      call FAITH     (X, W, IW, LUAP, DMPA, KVLG, N, Z, TE, ZT, XNE,
     $                HND, H2N, HEND, RHEAB, VM, HK, H1, HEK, HE1,
     $                HE2K, HE21, BETA, ZI, ZION, XION, Z1, Z2, Z3,
     $                ZXG, VAMB, VBMB, VCMB, VDMB, VE, VP, VH, V1, V2,
     $                V3)
C
C---- Compute ambipolar diffusion terms
      if(KAMB.eq.1) then
C----   Hydrogen
        call CLOGO   (N, NL, ALPHA, VH, VP, W(IVL), W(IVK))
        call MUKLUK  (N, 1, NL, XNK, W(IVK), GHI, W(ISION), XND,
     $                W(IVL), GHL, W(ISLVL), dummy, 0, Z, W, IW)
      else if(KAMB.eq.2) then
C----   Helium-I
        call CLOGO   (N, NL, ALPHA, V1, V2, W(IVL), W(IVK))
        call MUKLUK  (N, 1, NL, XNK, W(IVK), GHI, W(ISION), XND,
     $                W(IVL), GHL, W(ISLVL), dummy, 0, Z, W, IW)
      else if(KAMB.eq.3) then
C----   Helium-II
        call CLOGO   (N, NL, ALPHA, V2, V3, W(IVL), W(IVK))
        call GOLLIAS (SHE, V1, N, NL, W(IADD))
        call MUKLUK  (N, 1, NL, XNK, W(IVK), GHI, W(ISION), XND,
     $                W(IVL), GHL, W(ISLVL), W(IADD), 1, Z, W, IW)
      else
        write (MSSLIN(1),100) KAMB
  100   format('KAMB =',I12,', which is neither 1, 2, nor 3.')
        call HALT    ('COLUGO', 1)
      end if
C
      if(DMPA) then
        call AGOUTA  ('COLUGO', KAMB, N, NL, XND, XNK, ALPHA, Z, GHL,
     $                W(ISLVL), GHI, W(ISION), SHE, W(IADD), W(IVL),
     $                W(IVK), LAB(KAMB), IW(ISIG))
      end if
C
C     (Give back W & IW allotments)
      call WGIVE     (W,  'COLUGO')
      call IGIVE     (IW, 'COLUGO')
C     !END
      call BYE ('COLUGO')
C
      return
      end
