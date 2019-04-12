      subroutine PICKLE
     $(XCBL,XLM,XLTIT,ITYPE,LIC)
C
C     Rudolf Loeser, 1995 Apr 06
C---- Appends another Continuum Block to the "scratch file", and
C     updates the index.
C     (This is version 3 of PICKLE.)
C     !DASH
      save
C     !DASH
      real*8 XCBL, XLM, XLTIT
      integer IADRS, ITYPE, LIC
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
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external HALT, CEBU, HARTAM, HI, BYE
C
C               XCBL(Miklen)
      dimension XCBL(*)
C
      call HI ('PICKLE')
C     !BEG
      if(NUMKON.ge.KONWAL) then
        write (MSSLIN(1),100) NUMKON,KONWAL
  100   format('NUMKON =',I12,', KONWAL =',I12,'; Continuum Blocks ',
     $         'index is full.')
        call HALT   ('PICKLE', 1)
      else
C
C----   Save initial form of block, and get its address
        call CEBU   (XCBL, MIKLEN, IADRS)
C
C----   Increase block count, for tables in labelled COMMON
        NUMKON = NUMKON+1
C
C----   Save some block data in those tables
        CONWAV(NUMKON) = XLM
        CONTIT(NUMKON) = XLTIT
        KONADR(NUMKON) = IADRS
        KONLIC(NUMKON) = LIC
        call HARTAM (ITYPE, KONTYP(NUMKON))
C
      end if
C     !END
      call BYE ('PICKLE')
C
      return
      end
