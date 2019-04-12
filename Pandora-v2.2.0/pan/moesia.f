      subroutine MOESIA
     $(N,NSHL,CSHL,YASHL,WNSHL,MRR,CDSK,YADSK,WNDSK,CNXP,RR,DUMP)
C
C     Rudolf Loeser, 1982 Feb 09
C---- Computes RR, for angle-dependent Continuum Calculations.
C     !DASH
      save
C     !DASH
      real*8 CDSK, CNXP, CSHL, RR, WNDSK, WNSHL, YADSK, YASHL
      integer I, LUEO, MM, MRR, N, NSHL
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
      external MOVE1, GARTH, TATAR, LABFIL, VECOUT, HI, BYE
C
C               CSHL(N,NSHL), YASHL(N,N,NSHL), WNSHL(N,N,NSHL), CNXP(N),
      dimension CSHL(N,*),    YASHL(N,N,*),    WNSHL(N,N,*),    CNXP(*),
C
C               CDSK(N,MRR ), YADSK(N,N,MRR ), WNDSK(N,N,MRR ), RR(N)
     $          CDSK(N,*),    YADSK(N,N,*),    WNDSK(N,N,*),    RR(*)
C
      call HI ('MOESIA')
C     !BEG
C---- Initialize
      call MOVE1    (CNXP, N, RR)
C
C---- Accumulate ray sums
C     Shell rays
      I = 0
      do 100 MM = 1,NSHL
        call TATAR  (I)
        call GARTH  (CSHL(1,MM), I, YASHL(1,1,MM), WNSHL(1,1,MM), RR)
  100 continue
C
C     Disk rays
      do 101 MM = 1,MRR
        call GARTH  (CDSK(1,MM), N, YADSK(1,1,MM), WNDSK(1,1,MM), RR)
  101 continue
C
      if(DUMP) then
        call LABFIL ('CNXP', LINE)
        call VECOUT (LUEO, CNXP, N, LINE)
        call LABFIL ('RR'  , LINE)
        call VECOUT (LUEO, RR  , N, LINE)
      end if
C     !END
      call BYE ('MOESIA')
C
      return
      end
