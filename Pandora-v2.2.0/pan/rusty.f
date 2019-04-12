      subroutine RUSTY
     $(XLM,CORE,OXYGEN2,N,XNE,TE,V,O2N,O2BD,H1,VEX,EMU,CABS)
C
C     Rudolf Loeser, 2007 Jan 22
C---- Computes a set of O-II background line opacity values.
C     !DASH
      save
C     !DASH
      real*8 CABS, CORE, EMU, H1, O2BD, O2N, TE, V, VEX, XLM, XNE
      integer I, IL, IU, L, LX2DS, N
      logical DMPI, DUMP, KILROY, OXYGEN2, YES
C     !COM
C---- WURGO       as of 2007 Jan 25
      parameter   (MX2L=1, LX2L=3)
      integer     MX2L, LX2L, IUX2, ILX2, LDLX2
      real*8      X2MAS, X2SKE, X2WVL, X2WLO, X2WHI, X2NUU, X2NUL, X2AUL
      real*8      X2PU,  X2PL,  X2DDL, X2CDL, X2CRD, X2CVW, X2CSK
      dimension   X2WVL(MX2L), X2WLO(MX2L), X2WHI(MX2L), X2NUU(MX2L),
     $            X2NUL(MX2L), X2PU(MX2L),  X2PL(MX2L),  X2AUL(MX2L),
     $            IUX2(MX2L),  ILX2(MX2L),  LDLX2(MX2L)
      dimension   X2DDL(LX2L,MX2L), X2CDL(LX2L,MX2L),
     $            X2CRD(LX2L,MX2L), X2CVW(LX2L,MX2L), X2CSK(LX2L,MX2L)
      common      /WURGO0/ X2MAS,X2SKE
      common      /WURGO1/ X2WVL,X2WLO,X2WHI
      common      /WURGO2/ X2NUU,X2NUL,X2PU,X2PL
      common      /WURGO3/ X2AUL,X2DDL,X2CDL,X2CRD,X2CVW,X2CSK
      common      /WURGO4/ IUX2,ILX2,LDLX2
C     Data for Oxygen-II lines in the background.
C     .
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(130),LX2DS)
C     !DASH
C     !EJECT
      external FUSTY, GUSTY, MALTA, YALTA, MINNA, DUSTY, HI, BYE
C
C               VEX(N), H1(N), O2N(N,Limp), V(N), XNE(N), CABS(N,Nlin),
      dimension VEX(*), H1(*), O2N(N,*),    V(*), XNE(*), CABS(N,*),
C
C               TE(N), O2BD(N,Limp)
     $          TE(*), O2BD(N,*)
C
      call HI ('RUSTY')
C     !BEG
      KILROY = .true.
C
      do 101 L = 1,MX2L
        call FUSTY     (XLM, L, CORE, OXYGEN2, YES, DUMP)
        if(YES) then
C
          call MALTA   (XLM, DUMP, 'RUSTY')
          if(DUMP) then
            call DUSTY (XLM, L, KILROY)
          end if
C
          IU = IUX2(L)
          IL = ILX2(L)
          do 100 I = 1,N
            call MINNA (DUMP, I, LX2DS, DMPI)
            call GUSTY (L, XLM, EMU, XNE(I), TE(I), V(I), H1(I),
     $                  VEX(I), O2N(I,1), O2N(I,IL), O2BD(I,IU),
     $                  O2BD(I,IL), I, DMPI, CABS(I,L))
  100     continue
          call YALTA   (DUMP, 'RUSTY')
        end if
  101 continue
C     !END
      call BYE ('RUSTY')
C
      return
      end
