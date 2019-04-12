      subroutine PATROL
     $(X,NAME1,INDX1,NAME2,INDX2,NAME3,INDX3,ELABD)
C
C     Rudolf Loeser, 1981 May 19
C---- Gets Ion data for population ion data initialization.
C     (This is version 4 of PATROL.)
C     !DASH
      save
C     !DASH
      real*8 AB, AB1, AB2, AB3, ELABD, X, ZERO, dummy
      integer INDX1, INDX2, INDX3
      logical GO, STOP
      character BLANK*1, NAME1*(*), NAME2*(*), NAME3*(*)
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
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
C     !EJECT
      external  FRANK, SAGA, LUNA, HALT, HI, BYE
      intrinsic max
C
      dimension X(*)
C
C               ELABD(N)
      dimension ELABD(*)
C
      data STOP,GO /.true., .false./
C
      call HI ('PATROL')
C     !BEG
      AB1 = ZERO
      AB2 = ZERO
      AB3 = ZERO
C
      if(NAME1(:1).ne.BLANK) then
        call FRANK  (NAME1, 0, AB1, dummy, dummy, dummy, INDX1)
        if((INDX1.eq.0).or.(AB1.eq.ZERO)) then
          call SAGA (NAME1, 'PATROL1', GO)
        end if
      end if
C
      if(NAME2(:1).ne.BLANK) then
        call FRANK  (NAME2, 0, AB2, dummy, dummy, dummy, INDX2)
        if((INDX2.eq.0).or.(AB2.eq.ZERO)) then
          call SAGA (NAME2, 'PATROL2', STOP)
        end if
      end if
C
      if(NAME3(:1).ne.BLANK) then
        call FRANK  (NAME3, 0, AB3, dummy, dummy, dummy, INDX3)
        if((INDX3.eq.0).or.(AB3.eq.ZERO)) then
          call SAGA (NAME3, 'PATROL3', STOP)
        end if
      end if
C
      AB = max(AB1,AB2,AB3)
      if((AB.le.ZERO).or.(AB.ne.AB1)) then
        write (MSSLIN(1),100) NAME1,AB1,NAME2,AB2,NAME3,AB3
  100   format(3(A5,' AB =',1PE12.4,4X),'; huh?')
        call HALT   ('PATROL4', 1)
      else
C
        call LUNA   (X, NAME1, AB, ELABD)
      end if
C     !END
      call BYE ('PATROL')
C
      return
      end
