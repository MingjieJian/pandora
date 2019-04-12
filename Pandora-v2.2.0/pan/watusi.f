      subroutine WATUSI
     $(IU,IL,KLIN,XNU,P,AIJ,OLL,DW,XND,STIM,GTN,FXI)
C
C     Rudolf Loeser, 1983 Dec 12
C---- Computes GTN for transition (IU,IL), which is either
C     a radiative transition (KLIN=1), or
C     a passive   transition (KLIN=2).
C     Also computes FXI, whether or not it is needed.
C     (This is version 5 of WATUSI.)
C     !DASH
      save
C     !DASH
      real*8 AIJ, CON, DNU, DW, EIGHTH, F15, F7, FAC, FXI, GTN, OLL, P,
     $       PUL, STIM, TERM, XND, XNU, ZERO
      integer I, IL, IU, IUL, KLIN, N, NL
      logical RAD
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(18),EIGHTH)
C     !DASH
      external  RIGEL, INTRANS, DIVIDE, HI, BYE
      intrinsic abs
C
C               OLL(NT), P(NSL), DW(N), XND(N,NL), STIM(N), AIJ(NL,NL),
      dimension OLL(*),  P(*),   DW(*), XND(N,*),  STIM(*), AIJ(NL,*),
C
C               XNU(NSL), GTN(N), FXI(N)
     $          XNU(*),   GTN(*), FXI(*)
C
      data F7,F15 /1.D7, 1.D15/
C     !EJECT
C
      call HI ('WATUSI')
C     !BEG
      RAD = KLIN.eq.1
C
C---- Compute FAC
      call RIGEL      (41,CON)
      DNU = XNU(IU)-XNU(IL)
      FAC = (abs(AIJ(IU,IL))/F7)/(DNU**4)*CON
C
C---- Compute PUL
      call DIVIDE     (P(IU),P(IL),PUL)
      call INTRANS    (IU,IL,'WATUSI',IUL)
      PUL = PUL*OLL(IUL)
C
C---- Loop over depths
      do 100 I = 1,N
C
        if(RAD) then
          TERM = PUL*(XND(I,IL)/F15)*STIM(I)
        else
          TERM = PUL*(XND(I,IL)/F15)-(XND(I,IU)/F15)
        end if
C
        call DIVIDE   ((FAC*TERM),DW(I),GTN(I))
        FXI(I) = F7*FAC*TERM*DNU
  100 continue
C     !END
      call BYE ('WATUSI')
C
      return
      end
