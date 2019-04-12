      subroutine GERARD
     $(IU,IL,MP,DCON,CCON)
C
C     Rudolf Loeser, 1992 Apr 07
C---- Prints for TABOR.
C     !DASH
      save
C     !DASH
      real*8 CCON, DCON
      integer I, IL, IU, LUEO, MP
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
C               DCON(MP), CCON(MP)
      dimension DCON(*),  CCON(*)
C
      call HI ('GERARD')
C     !BEG
      call LINER (2, LUEO)
      write (LUEO,100) IU,IL,MP
  100 format(' ','Consolidated tables for transition (',I2,'/',I2,
     $           '), M-prime =',I6//
     $       ' ',9X,'i',21X,'DWN',21X,'CDL')
      call LINER (1, LUEO)
C
      write (LUEO,101) (I,DCON(I),CCON(I),I=1,MP)
  101 format(5(' ',I10,1P2E24.16/))
C     !END
      call BYE ('GERARD')
C
      return
      end
