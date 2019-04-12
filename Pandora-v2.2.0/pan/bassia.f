      subroutine BASSIA
     $(XCBL,N,NF,FREQ,WAVE,VEC,YNT)
C
C     Rudolf Loeser, 1988 Apr 25
C---- Gets data from Continuum Data blocks, for WADDY.
C     (This is version 2 of BASSIA.)
C     !DASH
      save
C     !DASH
      real*8 FREQ, VEC, WAVE, XCBL, YNT
      integer J, KKB, KKCO, KKJNU, N, NF
      logical USE
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
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(15),KKB   )
      equivalence (KKK(19),KKCO  )
      equivalence (KKK(13),KKJNU )
C     !DASH
      external NIKETAS, ANGIE, LEYTE, LALLER, HI, BYE
C
C               WAVE(Numkon), FREQ(Numkon), YNT(N,Numkon), XCBL(Numkon),
      dimension WAVE(*),      FREQ(*),      YNT(N,*),      XCBL(*),
C
C               VEC(N)
     $          VEC(*)
C
      call HI ('BASSIA')
C     !BEG
      NF = 0
      do 100 J = 1,NUMKON
        call NIKETAS  (KONTYP(J), USE)
        if(USE) then
          NF = NF+1
          WAVE(NF) = CONWAV(J)
          call ANGIE  (WAVE(NF), FREQ(NF))
          call LEYTE  (XCBL, MIKLEN, KONADR(J))
          call LALLER (N, XCBL(KKCO), XCBL(KKJNU), XCBL(KKB), VEC,
     $                 YNT(1,NF))
        end if
  100 continue
C     !END
      call BYE ('BASSIA')
C
      return
      end
