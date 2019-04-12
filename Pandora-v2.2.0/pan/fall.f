      subroutine FALL
     $(XLM,CORE,OXYGEN3,N,TE,O3BD,DMPF,CEMI)
C
C     Rudolf Loeser, 2007 Jan 22
C---- Computes a set of O-III background line source function values.
C     !DASH
      save
C     !DASH
      real*8 CEMI, CORE, O3BD, TE, XLM
      integer I, IL, IU, L, LX3DS, N
      logical DMPF, DMPI, DUMP, OXYGEN3, YES
C     !COM
C---- WARGO       as of 2007 Jan 18
      parameter   (MX3L=2, LX3L=6)
      integer     MX3L, LX3L, IUX3, ILX3, LDLX3
      real*8      X3MAS, X3SKE, X3WVL, X3WLO, X3WHI, X3NUU, X3NUL, X3AUL
      real*8      X3PU,  X3PL,  X3DDL, X3CDL, X3CRD, X3CVW, X3CSK
      dimension   X3WVL(MX3L), X3WLO(MX3L), X3WHI(MX3L), X3NUU(MX3L),
     $            X3NUL(MX3L), X3PU(MX3L),  X3PL(MX3L),  X3AUL(MX3L),
     $            IUX3(MX3L),  ILX3(MX3L),  LDLX3(MX3L)
      dimension   X3DDL(LX3L,MX3L), X3CDL(LX3L,MX3L),
     $            X3CRD(LX3L,MX3L), X3CVW(LX3L,MX3L), X3CSK(LX3L,MX3L)
      common      /WARGO0/ X3MAS,X3SKE
      common      /WARGO1/ X3WVL,X3WLO,X3WHI
      common      /WARGO2/ X3NUU,X3NUL,X3PU,X3PL
      common      /WARGO3/ X3AUL,X3DDL,X3CDL,X3CRD,X3CVW,X3CSK
      common      /WARGO4/ IUX3,ILX3,LDLX3
C     Data for Oxygen-III lines in the background.
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
      equivalence (KZQ(131),LX3DS)
C     !DASH
C     !EJECT
      external SAIL, GALL, MALTA, YALTA, MINNA, HI, BYE
C
C               CEMI(N,Nlin), O3BD(N,Limd), TE(N)
      dimension CEMI(N,*),    O3BD(N,*),    TE(*)
C
      call HI ('FALL')
C     !BEG
      DMPF = .false.
C
      do 101 L = 1,MX3L
        call SAIL      (XLM, L, CORE, OXYGEN3, YES, DUMP)
        if(YES) then
          if(DUMP) then
            DMPF = .true.
          end if
C
          call MALTA   (XLM, DUMP, 'FALL')
C
          IU = IUX3(L)
          IL = ILX3(L)
          do 100 I = 1,N
            call MINNA (DUMP, I, LX3DS, DMPI)
            call GALL  (L, TE(I), O3BD(I,IU), O3BD(I,IL), I, DMPI,
     $                  CEMI(I,L))
  100     continue
          call YALTA   (DUMP, 'FALL')
        end if
  101 continue
C     !END
      call BYE ('FALL')
C
      return
      end
