      subroutine UBANGI
     $(WT,NWT)
C
C     Rudolf Loeser, 1984 Feb 07
C---- Makes edited wavelengths table, for rates calculations
C     continuum integrations.
C     (This is version 3 of UBANGI.)
C     !DASH
      save
C     !DASH
      real*8 WT
      integer I, IPEX, LUEO, NWT
      logical GOOD
C     !COM
C---- TABLET      as of 2007 Feb 21
      integer     KONLIM,KONWAL
      parameter   (KONLIM=20000)
      real*8      CONTIT,CONWAV
      integer     NUMKON,NUMTRU,NMKUSE,KONADR,KONTYP,KONLIC,KONNSH
      dimension   CONTIT(KONLIM),CONWAV(KONLIM),KONADR(KONLIM),
     $            KONTYP(KONLIM),KONLIC(KONLIM),KONNSH(KONLIM)
      common      /TABLET0/ KONWAL,NUMKON,NUMTRU,NMKUSE
      common      /TABLET1/ CONWAV
      common      /TABLET2/ CONTIT
      common      /TABLET3/ KONADR
      common      /TABLET4/ KONTYP
      common      /TABLET5/ KONLIC
      common      /TABLET6/ KONNSH
C
C     Index, and other data, for Continuum Data Blocks.
C
C     KONWAL - (= KONLIM)
C     NUMKON - total number of Blocks
C     NUMTRU - number of line-specific Blocks ( .le. NUMKON)
C     NMKUSE - number of Blocks to be used for SIAM scratch storage
C
C     CONTIT - Block name (also called "Header Code" or XLTIT,SLTIT)
C     CONWAV - wavelength (Angstroms)
C     KONADR - file address of Block
C     KONTYP - Block code (labelled-common "kwack" via subroutine BEECH)
C     KONNSH - number of supplementary headers (shared blocks only)
C     KONLIC - line transition descriptor, = 100*iu+il (if needed)
C     .
C     !EJECT
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 18),IPEX )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external NELUMBO, MESHED, LINER, MASHED, HI, BYE
C
C               WT(Numkon)
      dimension WT(*)
C
      call HI ('UBANGI')
C     !BEG
      NWT = 0
C
      do 100 I = 1,NUMKON
        call NELUMBO (KONTYP(I), GOOD)
        if(GOOD) then
          NWT = NWT+1
          WT(NWT) = CONWAV(I)
        end if
  100 continue
C
      if((IPEX.lt.0).or.(IPEX.eq.12)) then
        call MESHED  ('UBANGI', 2)
        write (LUEO,101)
  101   format(' ','Edited rates integrations wavelengths')
        call LINER   (1, LUEO)
        write (LUEO,102) (I,WT(I),I=1,NWT)
  102   format(' ',1P,I7,E24.16,I7,E24.16,I7,E24.16,I7,E24.16)
        call MASHED  ('UBANGI')
      end if
C     !END
      call BYE ('UBANGI')
C
      return
      end
