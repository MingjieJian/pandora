      subroutine REPION
     $(W,IW,XCBL,SWAVE,SLTIT,NSH)
C
C     Rudolf Loeser, 2002 Aug 20
C---- Updates supplementary headers in shared blocks.
C     !DASH
      save
C     !DASH
      real*8 PWAVE, SLTIT, SWAVE, W, XCBL, ZERO
      integer I, ICOMP, IW, K, KKKTIT, MSH, NSH
      logical BLOCK
C     !COM
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(40),KKKTIT)
C
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
C---- ISOLA       as of 1997 Nov 19
      real*8      WAVEDEL
      common      /ISOLA/ WAVEDEL
C     Two Continuum Wavelength values are "equal" if their
C     relative difference is less than WAVEDEL.
C     .
C     !EJECT
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external PIONER, PERION, PRIONE, BOHOL, LOOKUD, LEYTE, COMPD,
     $         HI, BYE
C
      dimension W(*), IW(*)
C
C               XCBL(Miklen), SWAVE(Konwal), SLTIT(Konwal)
      dimension XCBL(*),      SWAVE(*),      SLTIT(*)
C
      call HI ('REPION')
C     !BEG
      if(NSH.gt.0) then
C
C----   Sort temporary tables
        call PERION      (W, IW, SWAVE, SLTIT, NSH)
C
        BLOCK = .false.
        PWAVE = ZERO
C----   Loop over all supplementary header codes
        do 100 I = 1,NSH
          call COMPD     (SWAVE(I), PWAVE, WAVEDEL, ICOMP)
          if(ICOMP.ne.0) then
            if(BLOCK) then
C----         Finish processing of previous block
              KONNSH(K) = MSH
              call BOHOL (XCBL, MIKLEN, KONADR(K))
              BLOCK = .false.
            end if
C----       Set current value of shared wavelength . . .
            PWAVE = SWAVE(I)
C----       . . . find index of corresponding existing block . . .
            call PRIONE  (PWAVE, K)
C----       . . . read it . . .
            call LEYTE   (XCBL, MIKLEN, KONADR(K))
            BLOCK = .true.
C----       . . . and initialize shared blocks count
            MSH = 0
          end if
C----     Enter supplementary header into current block
          call PIONER    (XCBL(KKKTIT), MSH, SLTIT(I), PWAVE)
  100   continue
C----   Finnish processing of final block
        KONNSH(K) = MSH
        call BOHOL       (XCBL, MIKLEN, KONADR(K))
C
      end if
C     !END
      call BYE ('REPION')
C
      return
      end
