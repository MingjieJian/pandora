      subroutine FLIBBLE
     $(X,IX,W,IW,N,NL,NSL,NT,KRJ,GMI,XND,BDIJ,RHOIJ,KIJ,RHOO,RHNW,WEIT,
     $ XJBAR,KFSV,WUSED,IFUDGE,XR,XM,XMS,Z,IMG,FO,CIJ,PIJ,LEGEND)
C
C     Rudolf Loeser, 1981 Feb 13
C---- Computes a basic set of ratios of Departure Coefficients, of the
C     form (J/1), where J is a level index such that 1 .lt. J .le. NL.
C
C---- When KRJ=1, the calculation uses RHOIJ (and RHOO) and XJBAR;
C     when KRJ=2, the calculation uses only XJBAR;
C     when KRJ=3. the calculation uses only RHOIJ (and RHOO).
C---- When KRJ=1 or 3, it may be necessary to adjust RHOIJ
C     to insure that BDIJ .gt. 0. For this we use
C     RHO(new) = RHO(previous)*(1-Weight)+Weight*RHOO,
C     (where RHOO is RHO(old)), and
C     where Weight gradually approaches 1. Upon return, WUSED
C     will be set to the Weight that was actually used.
C---- (When KRJ=2, only Weight=0. is allowed.)
C---- (When KRJ=1 or 3, then nonzero values of Weight are allowed
C     when IFUDGE=1, but not when IFUDGE=0.
C     When fudging is not allowed then, if negative values were
C     eliminated by editing, WUSED is returned = -1.)
C     (This is version 3 of FLIBBLE.)
C     !DASH
      save
C     !DASH
      real*8 BDIJ, CIJ, FO, GMI, ONE, PIJ, RACK, RHNW, RHOIJ, RHOO, W,
     $       WEIT, WUSED, X, XJBAR, XM, XMS, XND, XR, Z, ZERO
      integer IFUDGE, IMG, ITAU, IW, IX, KFSV, KIJ, KRJ, LGT, MSL, N,
     $        NL, NSL, NT
      logical BDED, FUJ, KILROY, LILROY, NOK
      character LEGEND*33
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external RACE, FIDGET, ALARIC, MINGO, MASHED, PLUSD, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               GMI(N,NSL), Z(NL,NL), PIJ(N,NL,NL), RHOO(N,NT), IMG(N),
      dimension GMI(*),     Z(*),     PIJ(*),       RHOO(*),    IMG(*),
C
C               RHOIJ(N,NT), XM(NL-1,NL-1), XMS(NL-1,NL-1), KIJ(NL,NL),
     $          RHOIJ(*),    XM(*),         XMS(*),         KIJ(*),
C
C               CIJ(N,NL,NL), XR(NL-1), XJBAR(N,NT), BDIJ(N,NL), FO(N),
     $          CIJ(*),       XR(*),    XJBAR(*),    BDIJ(*),    FO(*),
C
C               XND(N,NL), WEIT(N,NT), RHNW(N,NL)
     $          XND(N,*),  WEIT(*),    RHNW(*)
C
      data MSL /1/
C
      call HI ('FLIBBLE')
C     !BEG
      call PLUSD        (XND(1,MSL), 1, N, LGT)
      NOK    = LGT.eq.N
      KILROY = .true.
      LILROY = .true.
      WUSED  = ZERO
      FUJ    = IFUDGE.gt.0
C     !EJECT
  100 continue
C
C---- Loop over all depths
      do 101 ITAU = 1,N
C----   Compute basic B-ratios, BDIJ, at this depth, for all levels,
C       relative to the ground level, (J/1) (? printouts to LUEO)
        call RACE       (X, IX, W, IW, ITAU, N, NL, NSL, MSL, KRJ,
     $                   PIJ, CIJ, GMI, XND, NOK, BDIJ, RHOIJ, KIJ,
     $                   XJBAR, KFSV, WUSED, XM, XR, Z, XMS, LEGEND,
     $                   RACK, FUJ, LILROY)
        if(RACK.le.ZERO) then
C----     A value of BDIJ .le. 0 was generated;
C         if fudging is allowed, then fudge and try again
          if(FUJ) then
            call FIDGET (KILROY, N, NT, RHOIJ, RHOO, RHNW, WEIT, WUSED)
            goto 100
C
          end if
        end if
  101 continue
C---- Write final fudging data, for iterative summary
      call MINGO        (KFSV, 0, 0, WUSED, ZERO)
C
C---- (? printout trailer [for LINEN via RACE, above])
      if(.not.LILROY) then
        call MASHED     ('LINEN')
      end if
C
C---- Make sure that no values of BDIJ .le. 0 survive
      if(.not.FUJ) then
        BDED = .false.
        call ALARIC     (BDIJ, IMG, FO, N, NL, IFUDGE, LEGEND, BDED)
        if(BDED) then
          WUSED = -ONE
        end if
      end if
C     !END
      call BYE ('FLIBBLE')
C
      return
      end
