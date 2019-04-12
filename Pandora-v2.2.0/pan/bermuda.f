      subroutine BERMUDA
     $(XCBL,BACKGR,LINCON,NW,WAVES,XLTIT,XMULT,LTYPE,TAUK,SCON,OPAC,
     $ CNXP,FD,WVNUM,WTAB,LINK)
C
C     Rudolf Loeser, 1992 Apr 29
C---- Retrieves data from Continuum Data Blocks for
C     Emergent Continuum calculations.
C     (This is version 3 of BERMUDA.)
C     !DASH
      save
C     !DASH
      real*8 CNXP, FD, OPAC, SCON, TAUK, WAVES, WTAB, WVNUM, XCBL,
     $       XLTIT, XMULT, ZERO
      integer J, KJCNXP, KJFD, KJOPAC, KJSCON, KJTAUK, KKLAMD, KKLTIT,
     $        KKMULT, KTRU, LINK, LTYPE, N, NW, jummy
      logical BACKGR, DOIT, LINCON, USE
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(54),KKLAMD)
      equivalence (KKK( 1),KKLTIT)
      equivalence (KKK( 2),KKMULT)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !EJECT
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
      external BOJCA, KYOTO, CONRAD, LEYTE, TOVE, SNUGUD, HALT, HI, BYE
C
C               SCON(N,Numkon), OPAC(N,Numkon), CNXP(N,Numkon),
      dimension SCON(N,*),      OPAC(N,*),      CNXP(N,*),
C
C               XMULT(Numkon), LTYPE(Numkon), TAUK(N,Numkon),
     $          XMULT(*),      LTYPE(*),      TAUK(N,*),
C
C               WAVES(Numkon), XLTIT(Numkon), WVNUM(Numkon),
     $          WAVES(*),      XLTIT(*),      WVNUM(*),
C
C               FD(N,Numkon), XCBL(Miklen), WTAB(Numkon)
     $          FD(N,*),      XCBL(*),      WTAB(*)
C
      call HI ('BERMUDA')
C     !BEG
      NW = 0
C
      call KYOTO        (LINK, KTRU, DOIT)
      if(DOIT) then
        call CONRAD     (KTRU, KJTAUK, KJSCON, KJOPAC, KJCNXP, KJFD,
     $                   jummy)
C
        J  = 0
  100   continue
          J = J+1
          if(J.le.NUMKON) then
            call BOJCA  (KONLIC(J), KONTYP(J), BACKGR, LINCON, USE)
            if(.not.USE) then
              goto 100
            end if
C
            call LEYTE  (XCBL, MIKLEN, KONADR(J))
            NW = NW+1
            if(NW.gt.NMKUSE) then
              write (MSSLIN(1),101) NMKUSE,J,KONLIC(J)
  101         format('NMKUSE =',I12,' exceeded; J =',I12,', KONLIC =',
     $                I12)
              call HALT ('BERMUDA', 1)
            end if
            LTYPE(NW) = KONTYP(J)
            call TOVE   (N, XCBL(KKLTIT), XCBL(KKLAMD), XCBL(KKMULT),
     $                   XCBL(KJTAUK), XCBL(KJSCON), XCBL(KJOPAC),
     $                   XCBL(KJCNXP), XCBL(KJFD), XLTIT(NW),
     $                   WAVES(NW), XMULT(NW), TAUK(1,NW), SCON(1,NW),
     $                   OPAC(1,NW), CNXP(1,NW), FD(1,NW))
            go to 100
          end if
C
        call SNUGUD     (NW, WAVES, ZERO, WVNUM, WTAB, 0)
      end if
C     !END
      call BYE ('BERMUDA')
C
      return
      end
