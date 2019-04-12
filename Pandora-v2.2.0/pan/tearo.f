      subroutine TEARO
     $(N,VM,VB,TB,CHGSGN,DONE)
C
C     Rudolf Loeser, 1998 Mar 23
C---- Forces VB=VM, provided VM exists.
C     !DASH
      save
C     !DASH
      real*8 VB, VM
      integer LUEO, N
      logical CHGSGN, DONE, ZVB, ZVM
      character TB*(*)
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
C               VM(N), VB(N)
      dimension VM(*), VB(*)
C
      call HI ('TEARO')
C     !BEG
      DONE = .false.
C
      call NAUGHTD    (VM, 1, N, ZVM)
C
      if(.not.ZVM) then
        call NAUGHTD  (VB, 1, N, ZVB)
        call MOVE1    (VM, N, VB)
        if(CHGSGN) then
          call NEGATE (VB, N)
        end if
C
        DONE = .true.
C
        if(.not.ZVB) then
          call MESHED ('TEARO', 3)
          write (LUEO,100) TB
  100     format(' ','"',A,'" has been set equal to "VM"')
          call VECOUT (LUEO, VB, N, TB)
          call MASHED ('TEARO')
        end if
      end if
C     !END
      call BYE ('TEARO')
C
      return
      end
