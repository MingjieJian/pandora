      subroutine BEST
     $(TI,TJ,Y,GIJ)
C
C     Rudolf Loeser, 1971 Sep 27
C---- Computes terms for G(I,J), for interior columns.
C     !DASH
      save
C     !DASH
      real*8 CWARTR, D, E2TI, E3TI, E4D, E4TI, E5D, E5TI, EA4, EA5, GIJ,
     $       HALF, HNDRD, HNDRDTH, OMY, ONE, OPY, P, R, S, THIRD, TI,
     $       TJ, TWO, TWTHRD, Y, Z, ZERO, dummy
      integer KOD
      logical TIG, TIJ, TJG
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(14),CWARTR)
      equivalence (DLIT(13),THIRD )
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT(17),TWTHRD)
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external  ASSUR, EXPINT, MARDUK, HALT, HI, BYE
      intrinsic abs
C
      data HNDRD, HNDRDTH /1.D2, 1.D-2/
C
      call HI ('BEST')
C     !BEG
      TJG = TJ.ge.HNDRDTH
      TIG = TI.gt.HNDRDTH
      TIJ = TI.gt.(HNDRD*TJ)
C
      if(TI.ge.TJ) then
        P =  ONE
        Z =  ZERO
      else
        P = -ONE
        Z =  ONE
      end if
C
      OMY = ONE-Y
      OPY = ONE+Y
C     !EJECT
      if((.not.TJG).and.TIJ) then
        call ASSUR    (TI,TJ,4,EA4)
        call ASSUR    (TI,TJ,5,EA5)
        call EXPINT   (2,TI,E2TI,dummy)
C
        GIJ = -(Y*EA5/TJ+HALF*OMY*EA4)/TJ-CWARTR*TJ*(ONE-THIRD*Y)*E2TI
C
      else
        D = abs(TI-TJ)
        if(TJG.or.((.not.TJG).and.(.not.TIJ).and.TIG)) then
          KOD = 1
          call EXPINT (3,TI,E3TI,dummy)
          call EXPINT (4,TI,E4TI,dummy)
          call EXPINT (5,TI,E5TI,dummy)
          call EXPINT (4,D ,E4D ,dummy)
          call EXPINT (5,D ,E5D ,dummy)
        else if((.not.TJG).and.(.not.TIJ).and.(.not.TIG)) then
          KOD = 2
          call MARDUK (TI,3,E3TI)
          call MARDUK (TI,4,E4TI)
          call MARDUK (TI,5,E5TI)
          call MARDUK (D ,4,E4D )
          call MARDUK (D ,5,E5D )
        else
          write (MSSLIN(1),100) TJG,TIG,TIJ
  100     format('TJG =',L8,', TIG =',L8,', TIJ =',L8,': this does ',
     $           'not make sense.')
          call HALT   ('BEST',1)
        end if
C
        R = TI/TJ
        S = (Y*(E5TI-E5D)/TJ+HALF*(OPY*E4TI-P*OMY*E4D))/TJ+HALF*E3TI
C
        if(KOD.eq.1) then
C
          GIJ = S+THIRD*Z*(TWO*Y*R-OPY)/TJ
C
        else
C
          GIJ = S-CWARTR*P*TJ*(ONE-THIRD*Y)
     $           -HALF*Z*TI*(TWO-R*(OPY-TWTHRD*Y*R))
        end if
C
      end if
C     !END
      call BYE ('BEST')
C
      return
      end
