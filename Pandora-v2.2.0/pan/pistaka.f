      subroutine PISTAKA
     $(N,M,RS,CKI,PKL,PLK,PL1,P1L,BDI,CQI,EP1,EP2,DUMP)
C
C     Rudolf Loeser, 1981 Dec 11
C---- Prints details, for PIE.
C     !DASH
      save
C     !DASH
      real*8 BDI, CKI, CQI, EP1, EP2, P1L, PKL, PL1, PLK, RS
      integer I, LDINT, LUEO, M, N
      logical DMPI, DUMP
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 48),LDINT)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, MINNA, HI, BYE
C
C               CKI(N,NL), PKL(N), PLK(N), PL1(N), P1L(N), BDI(N,NL),
      dimension CKI(N,*),  PKL(*), PLK(*), PL1(*), P1L(*), BDI(N,*),
C
C               CQI(N), EP1(N), EP2(N), RS(N)
     $          CQI(*), EP1(*), EP2(*), RS(*)
C     !EJECT
C
      call HI ('PISTAKA')
C     !BEG
      if(DUMP) then
        call LINER   (3, LUEO)
        write (LUEO,100)
  100   format(' ',3X,'I',10X,'RS',9X,'CKM',9X,'PKL',9X,'PLK',9X,
     $            'PL1',9X,'P1L',9X,'BDM',11X,'Q',9X,'EP1',9X,'EP2')
        call LINER   (1, LUEO)
C
        do 102 I = 1,N
          call MINNA (DUMP, I, LDINT, DMPI)
          if(DMPI) then
            write (LUEO,101) I,RS(I),CKI(I,M),PKL(I),PLK(I),PL1(I),
     $                       P1L(I),BDI(I,M),CQI(I),EP1(I),EP2(I)
  101       format(' ',I4,1P10E12.4)
          end if
  102   continue
      end if
C     !END
      call BYE ('PISTAKA')
C
      return
      end
