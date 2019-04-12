      subroutine RP
     $(DT,J,TERM,SE2,SE3,SE4,SES2,SES3,SES4,SMLLTAU)
C
C     Rudolf Loeser, 1971 Jul 07
C---- Computes the term RP, for the RT Weight Matrix.
C     !DASH
      save
C     !DASH
      real*8 DM, DP, DT, E2P, E3M, E3P, E4M, E4P, HALF, HM, HNDRDTH,
     $       RAT, S, SE2, SE3, SE4, SES2, SES3, SES4, TERM, TWO, TWTHRD,
     $       XDEN, XNUM, ZERO
      integer J, JM, JP
      logical SMLLDEL, SMLLTAU
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT(17),TWTHRD)
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external HICKORY, WALNUT, DIVIDE, HI, BYE
C
C               DT(2*N), SE2(2*N), SE3(2*N), SES2(2*N), SES3(2*N),
      dimension DT(*),   SE2(*),   SE3(*),   SES2(*),   SES3(*),
C
C               SES4(N), SE4(N)
     $          SES4(*), SE4(*)
C
      data HNDRDTH /1.D-2/
C     !EJECT
C
      call HI ('RP')
C     !BEG
      JM = J-1
      JP = J+1
      DM = DT(JM)
      DP = DT(JP)
      HM = HALF*DM
C
      SMLLDEL = (DP.lt.HNDRDTH).and.(DM.lt.HNDRDTH)
      if(SMLLDEL) then
        call HICKORY   (2,JP,DT,SES2,E2P)
        call HICKORY   (3,JP,DT,SES3,E3P)
      else
        call WALNUT    (2,JP,DT,SE2,E2P)
        call WALNUT    (3,JP,DT,SE3,E3P)
      end if
C
      if(SMLLTAU) then
C
        if(SMLLDEL) then
          S = ZERO
        else
          S = HALF
        end if
        XNUM = S-E3P-DP*E2P
        XDEN = TWO*DP
        call DIVIDE    (XNUM,XDEN,TERM)
C
      else
C
        if(SMLLDEL) then
          call HICKORY (4,JP,DT,SES4,E4P)
          call HICKORY (3,JM,DT,SES3,E3M)
          call HICKORY (4,JM,DT,SES4,E4M)
          S = ZERO
        else
          call WALNUT  (4,JP,DT,SE4,E4P)
          call WALNUT  (3,JM,DT,SE3,E3M)
          call WALNUT  (4,JM,DT,SE4,E4M)
          S = TWTHRD
        end if
        XNUM = S-E4P-E4M-(DP+HM)*E3P-HM*E3M
        XDEN = DP*(DP+DM)
        call DIVIDE    (XNUM,XDEN,RAT)
        TERM = RAT-HALF*E2P
C
      end if
C     !END
      call BYE ('RP')
C
      return
      end
