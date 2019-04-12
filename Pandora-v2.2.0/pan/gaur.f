      subroutine GAUR
     $(LRQ,NPQ,NL)
C
C     Rudolf Loeser, 1992 Mar 26
C---- Checks input values of principal quantum numbers.
C     (This is version 2 of GAUR.)
C     !DASH
      save
C     !DASH
      integer J, LRQ, LUEO, NL, NPQ
      logical KILROY, STOP
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external MUSHED, ABORT, HI, BYE
C
C               LRQ(NL), NPQ(NL)
      dimension LRQ(*),  NPQ(*)
C
      call HI ('GAUR')
C     !BEG
      KILROY = .true.
      STOP   = .false.
  100 format(' ','Error in principal quantum numbers for level ',I4,5X,
     $           'n =',I4,5X,'l =',I4)
C
      do 103 J = 1,NL
        if(LRQ(J).eq.(-3)) then
          if(NPQ(J).le.3) then
            call MUSHED ('GAUR', 1, KILROY)
            write (LUEO,100) J,NPQ(J),LRQ(J)
            write (LUEO,101)
  101       format(' ','l may not = -3 for n = 1, 2, 3')
            STOP = .true.
          end if
C
        else if((LRQ(J).lt.0).and.(LRQ(J).ne.(-1))) then
          call MUSHED   ('GAUR', 1, KILROY)
          write (LUEO,100) J,NPQ(J),LRQ(J)
          write (LUEO,102)
  102     format(' ','The only allowed negative values of l are -1 ',
     $               'and -3')
          STOP = .true.
        end if
  103 continue
C
      if(STOP) then
        call ABORT
      end if
C     !END
      call BYE ('GAUR')
C
      return
      end
