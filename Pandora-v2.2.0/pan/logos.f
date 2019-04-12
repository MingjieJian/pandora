      subroutine LOGOS
     $(N,INDX,CO,VEC,YNT)
C
C     Rudolf Loeser, 1994 Jan 25
C---- Computes an integrand for cooling rates calculation.
C     !DASH
      save
C     !DASH
      real*8 CO, VEC, YNT
      integer INDX, J, N
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
      external HI, BYE
C
C               CO(Nopac,N), VEC(N), YNT(N)
      dimension CO(NOPAC,*), VEC(*), YNT(*)
C
      call HI ('LOGOS')
C     !BEG
      do 100 J = 1,N
        YNT(J) = VEC(J)*CO(INDX,J)
  100 continue
C     !END
      call BYE ('LOGOS')
C
      return
      end
