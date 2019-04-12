      subroutine CRUSTY
     $(XLM,CORE,OXYGEN2,N,TE,O2BD,DMPF,CEMI)
C
C     Rudolf Loeser, 2007 Jan 22
C---- Computes a set of O-II background line source function values.
C     !DASH
      save
C     !DASH
      real*8 CEMI, CORE, O2BD, TE, XLM
      integer I, IL, IU, L, LX2DS, N
      logical DMPF, DMPI, DUMP, OXYGEN2, YES
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
      external FUSTY, BUSTY, MALTA, YALTA, MINNA, HI, BYE
C
C               CEMI(N,Nlin), O2BD(N,Limd), TE(N)
      dimension CEMI(N,*),    O2BD(N,*),    TE(*)
C
      call HI ('CRUSTY')
C     !BEG
      DMPF = .false.
C
      do 101 L = 1,MX2L
        call FUSTY     (XLM, L, CORE, OXYGEN2, YES, DUMP)
        if(YES) then
          if(DUMP) then
            DMPF = .true.
          end if
C
          call MALTA   (XLM, DUMP, 'CRUSTY')
C
          IU = IUX2(L)
          IL = ILX2(L)
          do 100 I = 1,N
            call MINNA (DUMP, I, LX2DS, DMPI)
            call BUSTY (L, TE(I), O2BD(I,IU), O2BD(I,IL), I, DMPI,
     $                  CEMI(I,L))
  100     continue
          call YALTA   (DUMP, 'CRUSTY')
        end if
  101 continue
C     !END
      call BYE ('CRUSTY')
C
      return
      end
