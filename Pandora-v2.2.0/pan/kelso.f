      subroutine KELSO
     $(N,NL,M,EP1,EP2,RS,PKL,SPKL,GVL,PLK1,DUMP)
C
C     Rudolf Loeser, 1990 Apr 12
C---- Prints details, for GOLOT.
C     !DASH
      save
C     !DASH
      real*8 EP1, EP2, GVL, PKL, PLK1, RS, SPKL
      integer I, LDINT, LUEO, M, N, NL
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
C               EP1(N), EP2(N), GVL(N,NL), PKL(N), SPKL(N), PLK1(N),
      dimension EP1(*), EP2(*), GVL(N,*),  PKL(*), SPKL(*), PLK1(*),
C
C               RS(N)
     $          RS(*)
C     !EJECT
C
      call HI ('KELSO')
C     !BEG
      if(DUMP) then
        call LINER   (3, LUEO)
        write (LUEO,100) M
  100   format(' ',4X,'I',13X,'RS',12X,'PKL',11X,'PLK1',11X,'SPKL',6X,
     $             'GNVL(',I2,')',12X,'EP1',12X,'EP2')
        call LINER   (1, LUEO)
C
        do 102 I = 1,N
          call MINNA (DUMP, I, LDINT, DMPI)
          if(DMPI) then
            write (LUEO,101) I,RS(I),PKL(I),PLK1(I),SPKL(I),GVL(I,M),
     $                       EP1(I),EP2(I)
  101       format(' ',I5,1P7E15.7)
          end if
  102   continue
      end if
C     !END
      call BYE ('KELSO')
C
      return
      end
