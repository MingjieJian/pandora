      subroutine REVAL
     $(XCBL,XLMB,NB,N, TAU,CSF,B)
C
C     Rudolf Loeser, 1996 Feb 28
C---- Retrieves selected continuum data for all wavelengths of a batch.
C     !DASH
      save
C     !DASH
      real*8 B, CSF, TAU, XCBL, XLMB
      integer J, KKB, KKSCON, KKTAUK, N, NB, jummy
C     !COM
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(12),KKTAUK)
      equivalence (KKK(14),KKSCON)
      equivalence (KKK(15),KKB   )
C
C---- ISOLA       as of 1997 Nov 19
      real*8      WAVEDEL
      common      /ISOLA/ WAVEDEL
C     Two Continuum Wavelength values are "equal" if their
C     relative difference is less than WAVEDEL.
C     .
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
C     !DASH
C     !EJECT
      external SAMAR, MOVE1, HI, BYE
C
C               XCBL(Miklen), XLMB(NB), TAU(N,NB), CSF(N,NB), B(N,NB)
      dimension XCBL(*),      XLMB(*),  TAU(N,*),  CSF(N,*),  B(N,*)
C
      call HI ('REVAL')
C     !BEG
      do 100 J = 1,NB
        call SAMAR (XLMB(J),XCBL,MIKLEN, CONWAV,WAVEDEL,KONADR,NUMKON,
     $              jummy)
        call MOVE1 (XCBL(KKTAUK),N,TAU(1,J))
        call MOVE1 (XCBL(KKSCON),N,CSF(1,J))
        call MOVE1 (XCBL(KKB   ),N,B  (1,J))
  100 continue
C     !END
      call BYE ('REVAL')
C
      return
      end
