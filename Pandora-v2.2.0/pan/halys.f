      subroutine HALYS
     $(NO,KODE,N,TE,COOL,HEAT,TOTAL)
C
C     Rudolf Loeser, 1982 Apr 21
C---- Plots cooling and heating rates.
C     KODE .eq. 1: Rates; .eq. 2: Integrated Rates.
C     !DASH
      save
C     !DASH
      real*8 COOL, HEAT, TE, TOTAL, XL, XR, YBIG, YL, YM, YSML, YU
      integer KODE, N, NLIM, NO
      character INTEG*16, RATES*5
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
C
C---- IMAGE       as of 1997 Aug 21
      integer     IMALEN
      parameter   (IMALEN=65535)
      character   IMAGE*(IMALEN)
      common      /IMAGE/ IMAGE
C     Character string to hold plot images constructed by the
C     K-type line printer plotting routines;
C     but used also as a general scratch character array.
C     .
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external ABJECT, LINER, HALT, KEEL, ZEEL, SEED, REEL, HI, BYE
C
C               COOL(N), HEAT(N), TOTAL(N), TE(N)
      dimension COOL(*), HEAT(*), TOTAL(*), TE(*)
C
      data RATES,INTEG /'Rates', 'Integrated Rates'/
C
      call HI ('HALYS')
C     !BEG
      if((KODE.lt.1).or.(KODE.gt.2)) then
        write (MSSLIN(1),100) KODE
  100   format('KODE =',I12,', which is not 1 or 2.')
        call HALT ('HALYS', 1)
      end if
C
C---- Compute graph limits
      call KEEL   (TE, N, NLIM, COOL, HEAT, TOTAL, YSML, YBIG,
     $             YL, YM, YU)
      XL = 1
      XR = NLIM
C---- Initialize graph
      call ZEEL   (IMAGE, XL, XR, YL, YU, NLIM)
C---- Enter points
      call SEED   (IMAGE, TOTAL, NLIM, YSML, YBIG, YM, YU, ALPHS(4))
      call SEED   (IMAGE, HEAT , NLIM, YSML, YBIG, YM, YU, ALPHS(8))
      call SEED   (IMAGE, COOL , NLIM, YSML, YBIG, YM, YU, ALPHS(3))
C---- Write heading
      call ABJECT (NO)
      if(KODE.eq.1) then
        write (NO,101) RATES
  101   format(' ','Graph of log10(',A,') vs. Z-index; C:Cooling, ',
     $             'H:Heating, D:Difference (Cooling-Heating).')
      else
        write (NO,101) INTEG
      end if
      call LINER  (1, NO)
C---- Write graph
      call REEL   (NO, IMAGE, NLIM, YSML, YBIG)
C     !END
      call BYE ('HALYS')
C
      return
      end
