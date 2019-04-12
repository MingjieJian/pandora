      subroutine TREVES
     $(N,XKL,ZAXA,ZAYA,BHSNMS,XA,YA)
C
C     Rudolf Loeser, 1982 Feb 04
C---- Prints debug data for angle-dependent Continuum Calculations.
C     !DASH
      save
C     !DASH
      real*8 BHSNMS, XA, XKL, YA, ZAXA, ZAYA
      integer LUEO, N
      character LINE*127
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LABFIL, VECOUT, ARROUT, HI, BYE
C
C               ZAXA(N), ZAYA(N), BHSNMS(N), XA(N,N), YA(N,N), XKL(N,N)
      dimension ZAXA(*), ZAYA(*), BHSNMS(*), XA(*),   YA(*),   XKL(*)
C
      call HI ('TREVES')
C     !BEG
      call LABFIL ('KL', LINE)
      call ARROUT (LUEO, XKL, N, N, LINE)
C
      call LABFIL ('ZAXA', LINE)
      call VECOUT (LUEO, ZAXA, N, LINE)
C
      call LABFIL ('ZAYA', LINE)
      call VECOUT (LUEO, ZAYA, N, LINE)
C
      call LABFIL ('BHSNUM*', LINE)
      call VECOUT (LUEO, BHSNMS, N, LINE)
C
      call LABFIL ('XA', LINE)
      call ARROUT (LUEO, XA, N, N, LINE)
C
      call LABFIL ('YA', LINE)
      call ARROUT (LUEO, YA, N, N, LINE)
C     !END
      call BYE ('TREVES')
C
      return
      end
