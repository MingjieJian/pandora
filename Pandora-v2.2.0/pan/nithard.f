      subroutine NITHARD
     $(IMAGE,WL,NUMKON,SIG,INDX,IPER,KOUNT)
C
C     Rudolf Loeser, 1982 May 11
C---- Enters points into graph, for TONY.
C     !DASH
      save
C     !DASH
      real*8 SIG, TWO, WL, Y, YM, YML
      integer I, INDX, IPER, J, KOUNT, KOUNTA, KOUNTB, LINC, M, NUMKON
      character BLANK*1, IMAGE*(*)
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
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 3),TWO   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
C     !EJECT
      external KKOUNT, LINK, HI, BYE
C
C               WL(Numkon), IPER(Nopac,Numkon), INDX(Numkon)
      dimension WL(*),      IPER(NOPAC,*),      INDX(*)
C
      call HI ('NITHARD')
C     !BEG
      call KKOUNT           (IMAGE, KOUNTA)
C
      do 101 I = 1,NOPAC
        if(SYMID(I).ne.BLANK) then
          LINC = 1
C
          do 100 J = 1,NUMKON
            if(INDX(J).ne.0) then
              M = IPER(I,J)
C
              if(WL(J).ne.SIG) then
                if((M.ge.1).and.(M.le.100)) then
                  YM  = M
                  YML = log10(YM)
                  Y   = YML-TWO
                  call LINK (IMAGE, WL(J), Y, SYMID(I), LINC)
                else
                  LINC = 1
                end if
              end if
C
            end if
  100     continue
C
        end if
  101 continue
C
      call KKOUNT           (IMAGE, KOUNTB)
      KOUNT = KOUNTB-KOUNTA
C     !END
      call BYE ('NITHARD')
C
      return
      end
