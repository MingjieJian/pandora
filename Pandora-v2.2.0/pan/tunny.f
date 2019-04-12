      subroutine TUNNY
     $(XLM,CORE,OXYGEN1,N,TE,OBD,DMPF,CEMI)
C
C     Rudolf Loeser, 2004 Apr 19
C---- Computes a set of O-I background line source function values.
C     !DASH
      save
C     !DASH
      real*8 CEMI, CORE, OBD, TE, XLM
      integer I, IL, IU, L, LOXDS, N
      logical DMPF, DMPI, DUMP, OXYGEN1, YES
C     !COM
C---- FURGO       as of 2004 Jun 11
      parameter   (MOXL=11)
      integer     MOXL, IUOX, ILOX
      real*8      OXMAS, OXSKE, OXWVL, OXWLO, OXWHI, OXNUU, OXNUL
      real*8      OXPU,  OXPL,  OXAUL, OXCRD, OXCVW, OXCSK
      dimension   OXWVL(MOXL), OXWLO(MOXL), OXWHI(MOXL), OXNUU(MOXL),
     $            OXNUL(MOXL), OXPU(MOXL),  OXPL(MOXL),  OXAUL(MOXL),
     $            OXCRD(MOXL), OXCVW(MOXL), OXCSK(MOXL),
     $            IUOX(MOXL),  ILOX(MOXL)
      common      /FURGO0/ OXMAS,OXSKE
      common      /FURGO1/ OXWVL,OXWLO,OXWHI
      common      /FURGO2/ OXNUU,OXNUL,OXPU,OXPL
      common      /FURGO3/ OXAUL,OXCRD,OXCVW,OXCSK
      common      /FURGO4/ IUOX,ILOX
C     Data for Oxygen-I lines in the background.
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
      equivalence (KZQ(209),LOXDS)
C     !DASH
      external KREIDE, TOXIN, MALTA, YALTA, MINNA, HI, BYE
C
C               CEMI(N,Nlin), OBD(N,Limd), TE(N)
      dimension CEMI(N,*),    OBD(N,*),    TE(*)
C     !EJECT
C
      call HI ('TUNNY')
C     !BEG
      DMPF = .false.
C
      do 101 L = 1,MOXL
        call KREIDE    (XLM, L, CORE, OXYGEN1, YES, DUMP)
        if(YES) then
          if(DUMP) then
            DMPF = .true.
          end if
C
          call MALTA   (XLM, DUMP, 'TUNNY')
C
          IU = IUOX(L)
          IL = ILOX(L)
          do 100 I = 1,N
            call MINNA (DUMP, I, LOXDS, DMPI)
            call TOXIN (L, TE(I), OBD(I,IU), OBD(I,IL), I, DMPI,
     $                  CEMI(I,L))
  100     continue
          call YALTA   (DUMP, 'TUNNY')
        end if
  101 continue
C     !END
      call BYE ('TUNNY')
C
      return
      end
