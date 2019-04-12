      subroutine CREEP
     $(LU,N,ZABS,ZSCA,ZSCR,ZBNM,ZBDN,ZAXA,ZAYA)
C
C     Rudolf Loeser, 2005 Mar 04
C---- Prints PRD terms for Continuum Calculations.
C     (This is version 2 of CREEP.)
C     !DASH
      save
C     !DASH
      real*8 ZABS, ZAXA, ZAYA, ZBDN, ZBNM, ZSCA, ZSCR
      integer I, IPRDD, LU, N
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 98),IPRDD)
C     !DASH
      external LINER, HI, BYE
C
C               ZABS(N), ZSCA(N), ZSCR(N), ZBNM(N), ZBDN(N), ZAXA(N),
      dimension ZABS(*), ZSCA(*), ZSCR(*), ZBNM(*), ZBDN(*), ZAXA(*),
C
C               ZAYA(N)
     $          ZAYA(*)
C
      call HI ('CREEP')
C     !BEG
      if(LU.gt.0) then
        call LINER (1, LU)
        write (LU,100)
  100   format(' ',14X,'ZABS',9X,'ZSCA',9X,'ZSCR',9X,'ZBNM',9X,'ZBDN',
     $             9X,'ZAXA',9X,'ZAYA')
        call LINER (1, LU)
        do 102 I = 1,N,IPRDD
          write (LU,101) I,ZABS(I),ZSCA(I),ZSCR(I),ZBNM(I),ZBDN(I),
     $                     ZAXA(I),ZAYA(I)
  101     format(' ',I5,1P7E13.5)
  102   continue
      end if
C     !END
      call BYE ('CREEP')
C
      return
      end
