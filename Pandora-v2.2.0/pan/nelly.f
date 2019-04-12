      subroutine NELLY
     $(XLM,CORE,HELIUM1,N,XNE,TE,V,HE1N,HE1BD,H1,VEX,EMU,CABS)
C
C     Rudolf Loeser, 2005 Jun 27
C---- Computes a set of He-I background line opacity values.
C     !DASH
      save
C     !DASH
      real*8 CABS, CORE, EMU, H1, HE1BD, HE1N, TE, V, VEX, XLM, XNE
      integer I, IL, IU, L, LEEDS, N
      logical DMPI, DUMP, HELIUM1, KILROY, YES
C     !COM
C---- FIRGO       as of 2005 Jul 07
      parameter   (MHEE=4)
      integer     MHEE, IUHEE, ILHEE
      real*8      HEEMAS, HEEWVL, HEEWLO, HEEWHI, HEENUU, HEENUL
      real*8      HEEAUL, HEEPU,  HEEPL,  HEECRD, HEECVW, HEECSK
      real*8      HEESKE
      dimension   HEEWVL(MHEE), HEEWLO(MHEE), HEEWHI(MHEE),
     $            HEENUU(MHEE), HEENUL(MHEE), HEEPU(MHEE),
     $            HEEPL(MHEE),  HEEAUL(MHEE), HEECRD(MHEE),
     $            HEECVW(MHEE), HEECSK(MHEE),
     $            IUHEE(MHEE),  ILHEE(MHEE)
      common      /FIRGO0/ HEEMAS,HEESKE
      common      /FIRGO1/ HEEWVL,HEEWLO,HEEWHI
      common      /FIRGO2/ HEENUU,HEENUL,HEEPU,HEEPL
      common      /FIRGO3/ HEEAUL,HEECRD,HEECVW,HEECSK
      common      /FIRGO4/ IUHEE,ILHEE
C     Data for Helium lines in the background.
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
      equivalence (KZQ(215),LEEDS)
C     !DASH
C     !EJECT
      external TICKLE, DEKLA, MALTA, YALTA, MINNA, HUSSAR, HI, BYE
C
C               TE(N), H1(N), HE1N(N,Limp), V(N), XNE(N), CABS(N,Nlin),
      dimension TE(*), H1(*), HE1N(N,*),    V(*), XNE(*), CABS(N,*),
C
C               VEX(N), HE1BD(N,Limp)
     $          VEX(*), HE1BD(N,*)
C
      call HI ('NELLY')
C     !BEG
      KILROY = .true.
C
      do 101 L = 1,MHEE
        call TICKLE     (XLM, L, CORE, HELIUM1, YES, DUMP)
        if(YES) then
C
          call MALTA    (XLM, DUMP, 'NELLY')
          if(DUMP) then
            call HUSSAR (XLM, L, KILROY)
          end if
C
          IU = IUHEE(L)
          IL = ILHEE(L)
          do 100 I = 1,N
            call MINNA  (DUMP, I, LEEDS, DMPI)
            call DEKLA  (L, XLM, EMU, XNE(I), TE(I), V(I), H1(I),
     $                   VEX(I), HE1N(I,1), HE1N(I,IL), HE1BD(I,IU),
     $                   HE1BD(I,IL), I, DMPI, CABS(I,L))
  100     continue
          call YALTA    (DUMP, 'NELLY')
        end if
  101 continue
C     !END
      call BYE ('NELLY')
C
      return
      end
