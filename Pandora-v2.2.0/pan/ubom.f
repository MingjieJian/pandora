      subroutine UBOM
     $(KAMB,KVLG,XNK,XND,XNKL,XNDL,IHEDF,XPBL,N,NL,HND,H1,HK,HEND,
     $ RHEAB,HE1,HEK,BETA,HE21,HE2K,SHE,SHE2,PNF,VEC,KODE)
C
C     Rudolf Loeser, 1989 Sep 11
C---- Sets up populations data for ambipolar diffusion calculation.
C     (See below for a note on the notation.)
C     !DASH
      save
C     !DASH
      real*8 BETA, H1, HE1, HE21, HE2K, HEK, HEND, HK, HND, PNF, RHEAB,
     $       SHE, SHE2, VEC, XND, XNDL, XNK, XNKL, XPBL, dummy
      integer IHEDF, KAMB, KION, KODE, KVLG, LUEO, N, NL
      character MSS*100
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external  MOVE1, POPUTIL, BRYMBO, BUTTER, STABEET, ZERO1, ARRMUL,
     $          ARRADD, MESHED, MASHED, HI, BYE
      intrinsic max
C
C               XNK(N), XND(N,NL), XPBL(Lenpbl), HK(N), PNF(N), HEK(N),
      dimension XNK(*), XND(N,*),  XPBL(*),      HK(*), PNF(*), HEK(*),
C
C               HE1(N), HE2K(N), HE21(N), HEND(N), BETA(N), XNDL(N,NL),
     $          HE1(*), HE2K(*), HE21(*), HEND(*), BETA(*), XNDL(*),
C
C               HND(N), SHE(N), VEC(N), RHEAB(N), XNKL(N), SHE2(N),
     $          HND(*), SHE(*), VEC(*), RHEAB(*), XNKL(*), SHE2(*),
C
C               H1(N)
     $          H1(*)
C
      dimension MSS(3)
C     !EJECT
C
      call HI ('UBOM')
C     !BEG
      KION = max(KAMB,KVLG)
      if(KION.gt.0) then
C----   Hydrogen
        if(KION.eq.1) then
          call MOVE1   (XNK,      N, HK)
          call MOVE1   (XND(1,1), N, H1)
          MSS(1) = 'H1 and HK from N1 and NK'
        else
          call POPUTIL (XPBL, 1, 1, H1, 1, HK, 0, dummy, 0, dummy)
          MSS(1) = 'H1 and HK from data file'
        end if
C----   Helium-I
        if(KION.eq.2) then
          call MOVE1   (XNK,      N, HEK)
          call MOVE1   (XND(1,1), N, HE1)
          call POPUTIL (XPBL, 4, 0, dummy, 0, dummy, 0, dummy, 1, VEC)
          call ARRADD  (HE1, VEC, SHE, N)
          MSS(2) = 'HE1 and HEK from N1 and NK, SHE from data file'
        else
          call POPUTIL (XPBL, 4, 1, HE1, 1, HEK, 1, SHE, 0, dummy)
          call ARRMUL  (HEK, PNF, HEK, N)
          call ARRMUL  (HE1, PNF, HE1, N)
          call ARRMUL  (SHE, PNF, SHE, N)
          MSS(2) = 'HE1, HEK, and SHE from data file and PNF'
        end if
C----   Helium-II
        if(KION.eq.3) then
          call MOVE1   (XNK,      N, HE2K)
          call MOVE1   (XND(1,1), N, HE21)
          call POPUTIL (XPBL, 5, 0, dummy, 0, dummy, 0, dummy, 1, VEC)
          call ARRADD  (HE21, VEC, SHE2, N)
          MSS(3) = 'HE21 and HE2K from N1 and NK, SHE2 from data file'
        else
          call POPUTIL (XPBL, 5, 1, HE21, 1, HE2K, 1, SHE2, 0, dummy)
          call ARRMUL  (HE2K, PNF, HE2K, N)
          call ARRMUL  (HE21, PNF, HE21, N)
          call ARRMUL  (SHE2, PNF, SHE2, N)
          MSS(3) = 'HE21, HE2K, and SHE2 from data file and PNF'
        end if
      end if
C     !EJECT
C---- Notation:
C     Helium I       = alpha                          = HE1
C     Helium II      = beta [ average of (HEK,SHE2) ] = BETA
C     Helium III     = gamma                          = HE2K
C
C     total Helium                                    = HEND
C     total Helium-I (neutral Helium)                 = SHE
C     total Helium-II                                 = SHE2
C
C
C
      call STABEET  (N, HEK, SHE2, BETA)
C
      call BRYMBO   (N, RHEAB, HEND)
      call ARRMUL   (HEND, HND, HEND, N)
C
      if(KODE.gt.0) then
        call MESHED ('UBOM', 3)
        write (LUEO,100) MSS
  100   format(' ',A/
     $         ' ',A/
     $         ' ',A)
        write (LUEO,101)
  101   format(' ','Initial values of BETA and HEND were computed.')
        call MASHED ('UBOM')
      end if
C
      if(IHEDF.eq.1) then
C----   Special provision --- no Helium!
        call ZERO1  (HEK,  N)
        call ZERO1  (HE1,  N)
        call ZERO1  (HE2K, N)
        call ZERO1  (HE21, N)
        call ZERO1  (BETA, N)
        call ZERO1  (HEND, N)
        call ZERO1  (SHE,  N)
        call ZERO1  (SHE2, N)
      end if
C
C---- Set up "local" copies of XNK and XND
      call MOVE1    (XNK, N,      XNKL)
      call MOVE1    (XND, (N*NL), XNDL)
C     !END
      call BYE ('UBOM')
C
      return
      end
