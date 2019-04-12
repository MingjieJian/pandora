      subroutine BARE
     $(LUA,LUE,CA,CE,Z,TAUK,ZL,PLTID)
C
C     Rudolf Loeser, 1988 Jun 08
C---- Sets up plot identifiers.
C     (This is version 2 of BARE.)
C     !DASH
      save
C     !DASH
      real*8 A, CA, CE, FM, TAUK, W, Z, ZERO, ZL
      integer I, J, JBEG, JEND, L, LPNT, LUA, LUE, N
      logical USEA, USEE
      character BLANK*1, EQUAL*1, MINUS*1, PLTID*1, PLUS*1, STAR*1,
     $          qummy*10
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
C     !EJECT
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(45),STAR  )
      equivalence (SYMBS(39),PLUS  )
      equivalence (SYMBS(40),MINUS )
      equivalence (SYMBS(44),EQUAL )
C     !DASH
C     !EJECT
      external  ZED, SORT, SETC, HI, BYE
      intrinsic min, max
C
C               CA(Nopac,N), CE(Nopac,N), Z(N), TAUK(N), ZL(N), PLTID(4)
      dimension CA(NOPAC,*), CE(NOPAC,*), Z(*), TAUK(*), ZL(*), PLTID(*)
C
      dimension FM(NABS), LPNT(NABS)
C
      call HI ('BARE')
C     !BEG
      PLTID(1) = STAR
      PLTID(2) = PLUS
      PLTID(3) = MINUS
      PLTID(4) = EQUAL
      call SETC   (SYMID, 1, NOPAC, BLANK)
C
      USEA = LUA.gt.0
      USEE = LUE.gt.0
      if(USEA.or.USEE) then
        call ZED  (Z, N, TAUK, N, 0, JBEG, JEND, ZL, qummy, 'BARE')
C
        do 101 I = 1,NOPAC
          W = ZERO
          do 100 J = JBEG,JEND
            A = ZERO
            if(USEA) then
              A = A+CA(I,J)
            end if
            if(USEE) then
              A = A+CE(I,J)
            end if
            W = max(W,A)
  100     continue
          FM(I) = -W
  101   continue
C
        call SORT (FM, NOPAC, LPNT, 'Plot-ID keys')
C
        do 102 I = 1,min(NOPAC,26)
          L = LPNT(I)
          SYMID(L) = ALPHS(I)
  102   continue
      end if
C     !END
      call BYE ('BARE')
C
      return
      end
