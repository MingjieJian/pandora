      subroutine DEAD
     $(X,W,IW,N,TAUKIN,REFLM,PREF,PO,XCBLREF,P5,XCBL5,TAU5,ZP,Z,ZO,F,
     $ ISWA,IMG,FO,NO)
C     Rudolf Loeser, 1994 May 17
C---- Computes PREF, and new Z-from-TAUKIN.
C     (This is version 3 of DEAD.)
C     !DASH
      save
C     !DASH
      real*8 F, FO, P5, PO, PREF, REFLM, TAU5, TAUKIN, W, WLM, X, XCBL5,
     $       XCBLREF, Z, ZO, ZP, ZZERO
      integer IMG, INDEX, ISWA, IW, LTYPE, LU, N, NO
      logical DUMP, GOOD, KILROY
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external RIPPLE, MOVE1, PIETRO, LARS, PLINTH, CONSUB, PRIAM, HIT,
     $         GOON, CAKE, HALT, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               XCBL5(Miklen), XCBLREF(Miklen), ISWA(Nopac), TAUKIN(N),
      dimension XCBL5(*),      XCBLREF(*),      ISWA(*),     TAUKIN(*),
C
C               PO(N), P5(N), ZP(N), ZO(N), FO(N), TAU5(N), F(N), Z(N),
     $          PO(*), P5(*), ZP(*), ZO(*), FO(N), TAU5(N), F(N), Z(N),
C
C               PREF(N), IMG(N)
     $          PREF(*), IMG(*)
C
      data WLM,LTYPE,DUMP /5.D3, 14, .false./
C
      call HI ('DEAD')
C     !BEG
      call MOVE1  (Z   , N, ZO)
      call MOVE1  (PREF, N, PO)
      LU = NO
C
C---- Compute PREF, total continuum opacity at REFLM Angstroms
      KILROY = .true.
      call RIPPLE (X, W, IW, REFLM, LTYPE, XCBLREF, KILROY, PREF, DUMP)
C---- Check PREF
C     >>>>> (resets LU = LUEO if GOOD = .false.)
      call PIETRO (N, PREF, IMG, FO, LU, GOOD)
C---- Compute ZP (raw new Z) from PREF and TAUKIN
      call CAKE   (N, TAUKIN, PREF, F, ZP, W)
C
C---- Compute P5, total continuum opacity at 5000 Angstroms
      KILROY = .true.
      call RIPPLE (X, W, IW, WLM,   LTYPE, XCBL5,   KILROY, P5,   DUMP)
C---- Compute TAU5, continuum optical depth at 5000 Angstroms
      call LARS   (X, W, N, P5, TAU5, IMG)
C---- Get ZZERO, the value of ZP where TAU5 = 1 (i.e. star's surface)
C     >>>>> (resets LU = LUEO if GOOD = .false.)
      call GOON   (TAU5, ZP, ZZERO, N, INDEX, LU, GOOD)
C---- Get final new Z
      call MOVE1  (ZP, N, Z)
      call CONSUB (ZZERO, Z, N)
C     !EJECT
C---- Print
      if(GOOD) then
        call PRIAM  (LU, 'Z SCALE', 7)
      end if
      call HIT      (LU, N, REFLM, INDEX, ZZERO, PO, PREF, P5, TAU5,
     $               TAUKIN, ZP, ZO, Z)
      call PLINTH   (REFLM, XCBLREF, ISWA, LU)
      if(WLM.ne.REFLM) then
        call PLINTH (WLM,   XCBL5,   ISWA, LU)
      end if
C
      if(.not.GOOD) then
        write (MSSLIN(1),100)
  100   format(' ','PREF had to be edited, and is not useable.')
        call HALT   ('DEAD', 1)
      end if
C     !END
      call BYE ('DEAD')
C
      return
      end
