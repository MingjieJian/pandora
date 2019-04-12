      subroutine NARREDY
     $(LUMR,GMASS,TE,V,VR,VM,PGS,XNE,BDHM,BDI)
C
C     Rudolf Loeser, 1999 Aug 05
C---- Produces output for Kurucz's spectrum calculation program.
C     !DASH
      save
C     !DASH
      real*8 BDHM, BDI, GMASS, PGS, TE, V, VM, VR, XNE
      integer LUMR, N, NL, jummy
      logical HYDR
      character H1*2, NAME*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C     !DASH
      external MONIKA, PALSY, SPALY, HI, BYE
C
C               BDI(N,NL), VR(N), BDHM(N), PGS(N), XNE(N), TE(N), V(N),
      dimension BDI(N,*),  VR(*), BDHM(*), PGS(*), XNE(*), TE(*), V(*),
C
C               GMASS(N), VM(N)
     $          GMASS(*), VM(*)
C
      data H1 /'H1'/
C
      call HI ('NARREDY')
C     !BEG
      call MONIKA  (2, NAME, jummy)
      HYDR = NAME(:2).eq.H1
      if(HYDR) then
        call PALSY (LUMR, N, GMASS, TE, V, VR, VM, PGS, XNE)
      end if
      call SPALY   (LUMR, N, NL, NAME(:6), HYDR, BDHM, BDI)
C     !END
      call BYE ('NARREDY')
C
      return
      end
