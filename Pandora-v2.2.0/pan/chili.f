      subroutine CHILI
     $(QNAME,KSW)
C
C     Rudolf Loeser, 1988 Jun 03
C---- Reads absorber suppressors.
C     (This is version 2 of CHILI.)
C     !DASH
      save
C     !DASH
      integer I, J, KERR, KIND, KSW, LUEO
      logical KILROY
      character QNAME*8
C     !COM
C---- OPACITY     as of 2007 Jan 12
C     Paraphernalia for background absorption/emission contributors.
C
C     (Must recompile BARE, BRACE, FORAGER & SHARI when changing NABS!)
      parameter   (NABS=45)
C
      integer     NABS,NOPAC,KOPAC,TOPAC,LOPAC
      character   CNAME*24,SYMID*1,SHNAM*6
      dimension   KOPAC(NABS),LOPAC(NABS),SYMID(NABS)
      dimension   CNAME(NABS),SHNAM(NABS)
C
      common      /OPAC1/ NOPAC
      common      /OPAC2/ KOPAC
      common      /OPAC3/ LOPAC
      common      /OPAC4/ CNAME
      common      /OPAC5/ SYMID
      common      /OPAC6/ SHNAM
C
C     NOPAC = number of contributors
C     KOPAC = contributor status switch: 0 = don't use, 1 = use
C     CNAME = name (description) of contributor
C             NOTE: This sequence of names establishes the indices or
C                   ordinals by which the contributors are also known.
C     SHNAM = "short" name of contributor
C     LOPAC = "printout order" list of contributor ordinals
C     SYMID = scratch space for symbolic identifiers
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external ZEROI, CILENTO, MUSHED, ABORT, HI, BYE
C
C               KSW(Nopac)
      dimension KSW(*)
C
      call HI ('CHILI')
C     !BEG
      call ZEROI      (KSW, 1, NOPAC)
      call CILENTO    (KSW,    NOPAC, QNAME)
      KILROY = .true.
      KERR   = 0
C
      do 102 I = 1,NOPAC
C
        KIND = KSW(I)
        if(KIND.le.0) then
          goto 103
        end if
        if(KIND.le.NOPAC) then
          KOPAC(KIND) = 0
C
        else
          call MUSHED ('CHILI', 1, KILROY)
          write (LUEO,100) NOPAC
  100     format(' ','Error reading absorber indices.     NOPAC =',I5/
     $           ' ','Indices =')
          write (LUEO,101) (KSW(J),J=1,NOPAC)
  101     format(' ',24I5)
          KERR = KERR+1
        end if
C
  102 continue
      if(KERR.gt.0) then
        call ABORT
      end if
C
  103 continue
C     !END
      call BYE ('CHILI')
C
      return
      end
