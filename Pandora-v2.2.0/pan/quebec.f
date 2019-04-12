      subroutine QUEBEC
     $(T,N,TABNAM,CALLER,INDEX)
C
C     Rudolf Loeser, 1989 Aug 31
C---- Given the table T of length N, sorted in ascending order.
C     Returns with INDEX set such that T(INDEX)=0.
C     (This is version 2 of QUEBEC.)
C     !DASH
      save
C     !DASH
      real*8 T, ZERO
      integer INDEX, LOOK, LUEO, N, NOTE
      character CALLER*(*), TABNAM*(*)
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LOOKSD, MESHED, VECOUT, ABORT, HI, BYE
C
C               T(N)
      dimension T(*)
C
      call HI ('QUEBEC')
C     !BEG
      INDEX = 1
      call LOOKSD   (T, N, ZERO, ZERO, INDEX, NOTE, LOOK)
      if(LOOK.eq.2) then
        INDEX = N
      end if
C
      if(T(INDEX).ne.ZERO) then
        call MESHED ('QUEBEC', 1)
        write (LUEO,100) CALLER,TABNAM,N,INDEX,LOOK,NOTE
  100   format(' ','Table look-up called from ',A,': there is no 0 ',
     $             'in ',A//
     $         ' ','N =',I10,10X,'INDEX =',I10,10X,'LOOK =',I2,10X,
     $             'NOTE =',I2)
        call VECOUT (LUEO, T, N, TABNAM)
        call ABORT
      end if
C     !END
      call BYE ('QUEBEC')
C
      return
      end
