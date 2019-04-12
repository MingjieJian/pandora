      subroutine SIBERIA
     $(L,CQS,R1L,SMT,RL1,SCT)
C
C     Rudolf Loeser, 1987 Nov 06
C---- Prints, for EUROPE.
C     !DASH
      save
C     !DASH
      real*8 CQS, R1L, RL1, SCT, SMT
      integer L, LUEO
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
      call HI ('SIBERIA')
C     !BEG
      if(L.eq.2) then
        call LINER (1, LUEO)
      end if
C
      write (LUEO,100) L,CQS,R1L,SMT,RL1,SCT
  100 format(' ','L=',I2,2X,'QS=',1PE12.4,2X,'R1L=',E12.4,2X,'SMT=',
     $           E12.4,2X,'RL1=',E12.4,2X,'SCT=',E12.4)
C     !END
      call BYE ('SIBERIA')
C
      return
      end
