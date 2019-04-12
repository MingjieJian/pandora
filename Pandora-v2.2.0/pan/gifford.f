      subroutine GIFFORD
     $(IU,IL,N,DCON,CCON,DMAX)
C
C     Rudolf Loeser, 1992 Apr 23
C---- Prints for TABOR.
C     !DASH
      save
C     !DASH
      real*8 CCON, DCON, DMAX
      integer I, IL, IU, LUEO, N
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, HI, BYE
C
C               DCON(N), CCON(N)
      dimension DCON(*), CCON(*)
C
      call HI ('GIFFORD')
C     !BEG
      call LINER (2, LUEO)
      write (LUEO,100) IU,IL,N,DMAX
  100 format(' ','Truncated tables for transition (',I2,'/',I2,'),',
     $           ' N =',I6//
     $       ' ','Truncation criterion',1PE16.8//
     $       ' ',9X,'i',21X,'DWN',21X,'CDL')
      call LINER (1, LUEO)
C
      write (LUEO,101) (I,DCON(I),CCON(I),I=1,N)
  101 format(5(' ',I10,1P2E24.16/))
C     !END
      call BYE ('GIFFORD')
C
      return
      end
