      subroutine WAITER
     $(I,J,IU,IL,KODE, WARR,WVAL)
C
C     Rudolf Loeser, 1999 Aug 30
C---- Stores (KODE=1) or gets (KODE=2) a value from the
C     W-array (weights used for the SE calculation).
C     !DASH
      save
C     !DASH
      real*8 WARR, WVAL, ZERO
      integer I, IL, IU, J, KODE, LUEO, MUL, NL, NT
      logical OK
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 5),NT )
      equivalence (JZQ( 2),NL )
C
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
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external WAXESS, LINER, ABORT, HALT, MESHED, HI, BYE
C
C               WARR(MUL,NT)
      dimension WARR(*)
C
      call HI ('WAITER')
C     !BEG
      MUL = NL*(NL-1)/2
C
      if(KODE.eq.1) then
        call WAXESS   (I, J, IU, IL, KODE, WARR, MUL, NT, WVAL, OK)
        if(.not.OK) then
          call MESHED ('WAITER', 1)
          write (LUEO,100) I,J,IU,IL,KODE, NT,NL,MUL
  100     format(' ','Error in WAITER:'/
     $           ' ','I =',I10,', J =',I10,', IU =',I10,', IL =',I10,
     $               'KODE =',I10/
     $           ' ','NT =',I5,',NL =',I5,', MUL =',I7)
          call ABORT
        end if
C
      else if(KODE.eq.2) then
        if(I.eq.J) then
          OK = .false.
        else if(I.gt.J) then
          call WAXESS (I, J, IU, IL, KODE, WARR, MUL, NT, WVAL, OK)
        else
          call WAXESS (J, I, IU, IL, KODE, WARR, MUL, NT, WVAL, OK)
        end if
        if(.not.OK) then
          WVAL = ZERO
        end if
C
      else
        write (MSSLIN(1),101) KODE
  101   format('KODE =',I12,', which is not 1 (store) or 2 (get).')
        call HALT     ('WAITER', 1)
      end if
C     !END
      call BYE ('WAITER')
C
      return
      end
