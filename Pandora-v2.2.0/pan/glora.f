      subroutine GLORA
     $(KILROY,I,TE,J,HN,BD,F,TINC,T,ORES,OREM)
C
C     Rudolf Loeser, 2003 Jul 11
C---- Dump for H-bf absorption.
C     !DASH
      save
C     !DASH
      real*8 BD, F, HN, OREM, ORES, T, TE, TINC
      integer I, J, LUEO
      logical KILROY
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
      call HI ('GLORA')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        call LINER (1, LUEO)
        write (LUEO,100)
  100   format(' ',3X,'i',13X,'TE',2X,'j',13X,'HN',13X,'BD',14X,'F',
     $             11X,'TINC',14X,'T',11X,'ORES',11X,'OREM')
        write (LUEO,101) I,TE,J,HN,BD,F,TINC,T,ORES,OREM
  101   format(' ',I4,1PE15.7,I3,7E15.7)
      else
        write (LUEO,102) J,HN,BD,F,TINC,T,ORES,OREM
  102   format(' ',19X,I3,1P7E15.7)
      end if
C     !END
      call BYE ('GLORA')
C
      return
      end
