      subroutine ALINDA
     $(N,LG,YA,WN,CNXP,CI,RR,DUMP)
C
C     Rudolf Loeser, 1982 Feb 02
C---- Computes RR, for angle-dependent Continuum Calculations.
C     !DASH
      save
C     !DASH
      real*8 CI, CNXP, RR, WN, YA
      integer LG, LUEO, MM, N
      logical DUMP
      character LINE*127
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external  MOVE1, GARTH, LABFIL, VECOUT, HI, BYE
C
C               YA(N,N,LG), WN(N,N,LG), CI(N,LG), RR(N), CNXP(N)
      dimension YA(N,N,*),  WN(N,N,*),  CI(N,*),  RR(*), CNXP(*)
C
      call HI ('ALINDA')
C     !BEG
C---- Initialize
      call MOVE1    (CNXP, N, RR)
C
C---- Accumulate angle sums
      do 100 MM = 1,LG
        call GARTH  (CI(1,MM), N, YA(1,1,MM), WN(1,1,MM), RR)
  100 continue
C
      if(DUMP) then
        call LABFIL ('CNXP', LINE)
        call VECOUT (LUEO, CNXP, N, LINE)
        call LABFIL ('RR'  , LINE)
        call VECOUT (LUEO, RR  , N, LINE)
      end if
C     !END
      call BYE ('ALINDA')
C
      return
      end
