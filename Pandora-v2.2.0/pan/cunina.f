      subroutine CUNINA
     $(X,W,IW,KVLG,LUVP,DMPV,NL,XNK,XND,GXI,GXL,N,Z,VM,GX1M,DZZ,VBMB,
     $ HND,HEND,BETA,HE1,HE2K,ZETA)
C
C     Rudolf Loeser, 1989 Sep 18
C---- Computes mass motion terms for diffusion calculations.
C     (This is version 2 of CUNINA.)
C     !DASH
      save
C     !DASH
      real*8 BETA, DZZ, GX1M, GXI, GXL, HE1, HE2K, HEND, HND, VBMB, VM,
     $       W, X, XND, XNK, Z, ZETA, dummy
      integer IN, IS, ISIG, ISION, ISLVL, IVK, IVL, IW, IWS, JN, KVLG,
     $        LUVP, MOX, MUX, N, NL
      logical DMPV
      character LAB*9
C     !DASH
      external ANIUC, RUNINA, MUKLUK, AGALMA, MUNINA, PAPPUS, WGIVE,
     $         IGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               XNK(N), XND(N,NL), GXI(N), GXL(N,NL), GX1M(N), BETA(N),
      dimension XNK(*), XND(N,*),  GXI(*), GXL(N,*),  GX1M(*), BETA(*),
C
C               VBMB(N), DZZ(N), ZETA(N), HE2K(N), HE1(N), VM(N), Z(N),
     $          VBMB(*), DZZ(*), ZETA(*), HE2K(*), HE1(*), VM(*), Z(*),
C
C               HND(N), HEND(N)
     $          HND(*), HEND(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IVK   ),(IN( 2),IVL   ),(IN( 3),ISION ),(IN( 4),ISLVL )
C
      dimension JN(1)
      equivalence
     $(JN( 1),ISIG  )
C
      dimension LAB(3)
C
      data LAB /'Hydrogen ', 'Helium-I ', 'Helium-II'/
C     !EJECT
C
      call HI ('CUNINA')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call RUNINA   (IN, IS,  MOX, 'CUNINA')
      call MUNINA   (JN, IWS, MUX, 'CUNINA')
C
C---- Set up velocities
      call ANIUC    (N, NL, VM, W(IVL), W(IVK))
C
C---- Compute mass motion term for Level 1
      call PAPPUS   (DMPV, 'CUNINA', KVLG, N, Z, VBMB, HND, HEND, XNK,
     $               XND, BETA, HE1, HE2K, ZETA, DZZ, GX1M, GXL, W, IW,
     $               LAB(KVLG))
C
C---- Compute mass motion terms for upper levels, and GXI
      call MUKLUK   (N, 2, NL, XNK, W(IVK), GXI, W(ISION), XND, W(IVL),
     $               GXL, W(ISLVL), dummy, 0, Z, W, IW)
C
      if(DMPV) then
        call AGALMA ('CUNINA', N, NL, XND, XNK, VM, Z, W(ISLVL), GXL,
     $               W(ISION), GXI, LAB(KVLG), IW(ISIG))
      end if
C
C     (Give back W & IW allotments)
      call WGIVE    (W,  'CUNINA')
      call IGIVE    (IW, 'CUNINA')
C     !END
      call BYE ('CUNINA')
C
      return
      end
