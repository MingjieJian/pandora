      subroutine NATTER
     $(KODE,TITLE,Y,TAU,N,CALLER)
C
C     Rudolf Loeser, 1989 Dec 06
C---- General error advisory, from WN matrix calculation.
C     !DASH
      save
C     !DASH
      real*8 TAU, Y
      integer KODE, LUEO, N
      character CALLER*(*), TITLE*(*)
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external JUNO, VECOUT, HI, BYE
C
C               TAU(N)
      dimension TAU(*)
C
      call HI ('NATTER')
C     !BEG
      if(KODE.le.0) then
        call JUNO
        write (LUEO,100) CALLER,TITLE,Y
  100   format(' ','"Lambda-1" operator apparently was not computed ',
     $             'properly.'10X,'Called from: ',A/
     $         ' ',A//
     $         ' ',5X,'Y =',1PE12.4)
C
        call VECOUT (LUEO, TAU, N, 'TAU')
      end if
C     !END
      call BYE ('NATTER')
C
      return
      end
