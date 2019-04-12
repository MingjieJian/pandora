      subroutine ROOSTER
     $(IS,IE,C,KONFORM,NO,ISW)
C
C     Rudolf Loeser, 1973 Feb 06
C---- Prints the main part of a contributions array.
C     !DASH
      save
C     !DASH
      real*8 C, PART
      integer I, IE, IS, ISW, KNT, KONFORM, L, NO
      logical KROW
      character LABEL*27
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
C     !DASH
C     !EJECT
      external MOVED, NAUGHTD, PING, HI, BYE
C
C               C(Nopac,N), ISW(Nopac)
      dimension C(NOPAC,*), ISW(*)
C
      dimension PART(10)
C
      call HI ('ROOSTER')
C     !BEG
      KNT = (IE-IS)+1
C
      do 101 L = 1,NOPAC
        I = LOPAC(L)
        call MOVED   (C(I,IS), NOPAC, KNT, PART, 1, KNT)
        call NAUGHTD (PART, 1, KNT, KROW)
C
        if(.not.KROW) then
          write (LABEL,100) CNAME(I)(3:24),SYMID(I),ISW(I)
  100     format(A22,' ',A1,I2,' ')
          call PING  (NO, KONFORM, LABEL, PART, KNT)
        end if
C
  101 continue
C     !END
      call BYE ('ROOSTER')
C
      return
      end
