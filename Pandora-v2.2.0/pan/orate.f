      subroutine ORATE
     $(N,CVA,VA,TA,CVB,VB,TB,CHGSGN,DONE)
C
C     Rudolf Loeser, 1990 May 07
C---- Forces VB=VA, provided VA exists.
C     !DASH
      save
C     !DASH
      real*8 CVA, CVB, VA, VB
      integer LUEO, N
      logical CHGSGN, DONE, ZVA, ZVB
      character TA*(*), TB*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external NAUGHTD, MOVE1, NEGATE, MESHED, VECOUT, MASHED, HI, BYE
C
C               VA(N), VB(N)
      dimension VA(*), VB(*)
C
      call HI ('ORATE')
C     !BEG
      DONE = .false.
C
      call NAUGHTD    (VA, 1, N, ZVA)
C
      if(.not.ZVA) then
        call NAUGHTD  (VB, 1, N, ZVB)
        call MOVE1    (VA, N, VB)
        CVB = CVA
        if(CHGSGN) then
          call NEGATE (VB, N)
          CVB = -CVB
        end if
C
        DONE = .true.
C
        if(.not.ZVB) then
C----     Print message
          call MESHED ('ORATE', 3)
          write (LUEO,100) TB,TA
  100     format(' ','"',A,'" has been set equal to "',A,'"')
          call VECOUT (LUEO, VB, N, TB)
          call MASHED ('ORATE')
        end if
      end if
C     !END
      call BYE ('ORATE')
C
      return
      end
