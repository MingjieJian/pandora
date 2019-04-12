      subroutine CP
     $(DT,J,TERM,SE3,SE4,SES3,SES4,SMLLTAU)
C
C     Rudolf Loeser, 1971 Jul 07
C---- Computes the term CP, for the RT Weight Matrix.
C     !DASH
      save
C     !DASH
      real*8 DM, DP, DT, E3M, E3P, E4M, E4P, EDM, EDP, HALF, HNDRDTH,
     $       ONE, P, RAT, SE3, SE4, SES3, SES4, T, TERM, TWTHRD, XDEN,
     $       XNUM, ZERO
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
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HICKORY, WALNUT, DIVIDE, HI, BYE
C
C               DT(2*N), SE3(2*N), SE4(N), SES3(2*N), SES4(N)
      dimension DT(*),   SE3(*),   SE4(*), SES3(*),   SES4(*)
C
      data HNDRDTH /1.D-2/
C     !EJECT
C
      call HI ('CP')
C     !BEG
      JM = J-1
      JP = J+1
      DM = DT(JM)
      DP = DT(JP)
C
      SMLLDEL = (DM.lt.HNDRDTH).and.(DP.lt.HNDRDTH)
      if(SMLLDEL) then
        call HICKORY   (3,JM,DT,SES3,E3M)
        call HICKORY   (3,JP,DT,SES3,E3P)
      else
        call WALNUT    (3,JM,DT,SE3,E3M)
        call WALNUT    (3,JP,DT,SE3,E3P)
      end if
C
      if(SMLLTAU) then
C
        if(SMLLDEL) then
          P = -ONE
          T =  ZERO
        else
          P =  ZERO
          T =  HALF
        end if
        call DIVIDE    ((E3M-T),DM,EDM)
        call DIVIDE    ((E3P-T),DP,EDP)
        TERM = P+HALF*(EDM+EDP)
C
      else
C
        if(SMLLDEL) then
          call HICKORY (4,JM,DT,SES4,E4M)
          call HICKORY (4,JP,DT,SES4,E4P)
          P = -ONE
          T =  ZERO
        else
          P =  ZERO
          T = -TWTHRD
          call WALNUT  (4,JM,DT,SE4,E4M)
          call WALNUT  (4,JP,DT,SE4,E4P)
        end if
        XNUM = (T+E4M+E4P)+HALF*(DM+DP)*(E3M+E3P)
        XDEN = DM*DP
        call DIVIDE    (XNUM,XDEN,RAT)
        TERM = P+RAT
C
      end if
C     !END
      call BYE ('CP')
C
      return
      end
