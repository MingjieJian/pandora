      subroutine KELLY
     $(XLM,CORE,HELIUM1,N,TE,HE1BD,DMPF,CEMI)
C
C     Rudolf Loeser, 2005 Jun 24
C---- Computes a set of He-I background line source function values.
C     !DASH
      save
C     !DASH
      real*8 CEMI, CORE, HE1BD, TE, XLM
      integer I, IL, IU, L, LEEDS, N
      logical DMPF, DMPI, DUMP, HELIUM1, YES
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
      external TICKLE, REKLA, MALTA, YALTA, MINNA, HI, BYE
C
C               CEMI(N,Nlin), HE1BD(N,Limd), TE(N)
      dimension CEMI(N,*),    HE1BD(N,*),    TE(*)
C     !EJECT
C
      call HI ('KELLY')
C     !BEG
      DMPF = .false.
C
      do 101 L = 1,MHEE
        call TICKLE    (XLM, L, CORE, HELIUM1, YES, DUMP)
        if(YES) then
          if(DUMP) then
            DMPF = .true.
          end if
C
          call MALTA   (XLM, DUMP, 'KELLY')
C
          IU = IUHEE(L)
          IL = ILHEE(L)
          do 100 I = 1,N
            call MINNA (DUMP, I, LEEDS, DMPI)
            call REKLA (L, TE(I), HE1BD(I,IU), HE1BD(I,IL), I, DMPI,
     $                  CEMI(I,L))
  100     continue
          call YALTA   (DUMP, 'KELLY')
        end if
  101 continue
C     !END
      call BYE ('KELLY')
C
      return
      end
