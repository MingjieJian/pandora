      subroutine BUNNY
     $(XLM,CORE,OXYGEN1,N,XNE,TE,V,ON,OBD,H1,VEX,EMU,CABS)
C
C     Rudolf Loeser, 2004 Apr 16
C---- Computes a set of O-I background line opacity values.
C     !DASH
      save
C     !DASH
      real*8 CABS, CORE, EMU, H1, OBD, ON, TE, V, VEX, XLM, XNE
      integer I, IL, IU, L, LOXDS, N
      logical DMPI, DUMP, KILROY, OXYGEN1, YES
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
      external KREIDE, VIXEN, MALTA, YALTA, MINNA, FUNNY, HI, BYE
C
C               TE(N), H1(N), ON(N,Limp), VEX(N), XNE(N), OBD(N,Limp),
      dimension TE(*), H1(*), ON(N,*),    VEX(*), XNE(*), OBD(N,*),
C
C               V(N), CABS(N,Nlin)
     $          V(*), CABS(N,*)
C     !EJECT
C
      call HI ('BUNNY')
C     !BEG
      KILROY = .true.
C
      do 101 L = 1,MOXL
        call KREIDE    (XLM, L, CORE, OXYGEN1, YES, DUMP)
        if(YES) then
C
          call MALTA   (XLM, DUMP, 'BUNNY')
          if(DUMP) then
            call FUNNY (XLM, L, KILROY)
          end if
C
          IU = IUOX(L)
          IL = ILOX(L)
          do 100 I = 1,N
            call MINNA (DUMP, I, LOXDS, DMPI)
            call VIXEN (L, XLM, EMU, XNE(I), TE(I), V(I), H1(I),
     $                  VEX(I), ON(I,1), ON(I,IL), OBD(I,IU),
     $                  OBD(I,IL), I, DMPI, CABS(I,L))
  100     continue
          call YALTA   (DUMP, 'BUNNY')
        end if
  101 continue
C     !END
      call BYE ('BUNNY')
C
      return
      end
