      subroutine KOILS
     $(VM,MN1,KINOUT)
C
C     Rudolf Loeser, 1998 Jul 16
C---- Determines what the physical conditions are. Upon return,
C     KINOUT = 1 means: inflow; = 2 means: outflow.
C     !DASH
      save
C     !DASH
      real*8 V1, VM, VN, ZERO
      integer KINOUT, LUEO, MN1
      logical INFLOW, OUTFLW
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external MESHED, VECOUT, ABORT, HI, BYE
C
C               VM(N)
      dimension VM(*)
C     !EJECT
C
      call HI ('KOILS')
C     !BEG
      KINOUT = 0
      INFLOW = .false.
      OUTFLW = .false.
C
      V1 = VM(1)
      VN = VM(MN1)
      if((V1.ge.ZERO).or.(VN.ne.ZERO)) then
        if((V1.ge.ZERO).and.(VN.ge.ZERO)) then
          INFLOW = .true.
          KINOUT = 1
        else if((V1.le.ZERO).and.(VN.le.ZERO)) then
          OUTFLW = .true.
          KINOUT = 2
        end if
      end if
C
      if(INFLOW.and.OUTFLW) then
        call MESHED ('KOILS',1)
        write (LUEO,100) MN1,INFLOW,OUTFLW
  100   format(' ','MN1 =',I8,'; INFLOW =',L6,' and OUTFLW =',L6,
     $             ': how can they both be true?')
        call VECOUT (LUEO,VM,MN1,'VM')
        call ABORT
      end if
C     !END
      call BYE ('KOILS')
C
      return
      end
