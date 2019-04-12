      subroutine BELL
     $(N,XJNUO,XJNU)
C
C     Rudolf Loeser, 1981 Jul 22
C---- Dumps, for CRANE.
C     !DASH
      save
C     !DASH
      real*8 XJNU, XJNUO
      integer I, LUEO, N
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
C               XJNUO(N), XJNU(N)
      dimension XJNUO(*), XJNU(*)
C
      call HI ('BELL')
C     !BEG
      call LINER (1,LUEO)
      write (LUEO,100) (I,XJNUO(I),XJNU(I),I=1,N)
  100 format(' ',20X,'JNUO',17X,'JNU'//
     $     5(' ',I4,1P2E20.10/))
C     !END
      call BYE ('BELL')
C
      return
      end
